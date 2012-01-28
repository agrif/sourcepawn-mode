;;; sourcepawn-mode.el --- SourcePawn major mode for emacs

;; Copyright (c) 2010, Aaron Griffith
;; This file is licensed under the GNU GPL -- see below.

;; Author: Aaron Griffith <aargri@gmail.com>
;; Homepage: http://gammalevel.com/teamfortress2/sourcepawn-mode

;;; Commentary:

;; SourcePawn is a scripting language for SourceMod, which can be
;; found at <http://www.sourcemod.net/>,
;;
;; More (and nicer) documentation for sourcepawn-mode may be found at
;; <http://gamma-level.com/teamfortress2/sourcepawn-mode>.
;;
;; Suggestions, improvements, and bug reports are welcome. Please
;; contact me at the email address above!

;;; Installation:

;; NOTE: the file `sourcepawn-mode.el.in` is used to GENERATE the file
;; `sourcepawn-mode.el`, which is what you should install. DO NOT USE
;; the .in version: it will not work. Instead, see the README for how
;; to generate the real file, or get a pregenerated file from my
;; website, linked above.
;;
;; Installation instructions:
;;
;; 1. Put this file somewhere in your emacs load path OR add the
;;    following to your .emacs file (modifying the path
;;    appropriately):
;;
;;    (add-to-list 'load-path "/home/agrif/emacsinclude")
;;
;; 2. Add the following to your .emacs file to load this file
;;    automatically when needed, and to make this autoload *.sp files:
;;
;;    (autoload 'sourcepawn-mode "sourcepawn-mode" nil t)
;;    (add-to-list 'auto-mode-alist '(".sp\\'" . sourcepawn-mode))
;;
;; 3. (Optional) Customize SourcePawn mode with your own hooks.  Below
;;    is a sample which automatically untabifies when you save:
;;
;;    (defun my-sourcepawn-mode-hook ()
;;      (add-hook 'local-write-file-hooks 'auto-untabify-on-save))
;;
;;    (add-hook 'sourcepawn-mode-hook 'my-sourcepawn-mode-hook)
;;

;;; License:

;; This file is released under the GNU GPL.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; STILL TODO - customize, compile? menus? tags?

;; define the mode hook list
(defvar sourcepawn-mode-hook
  nil
  "A list for storing sourcepawn-mode hooks.")

;; breaking down the variable name match regexp into parts
(defvar sourcepawn-mode-font-lock-regexp-variable-names
  "\\(?:\\(?:\\sw\\)+:\\)?\\(\\(?:\\sw\\)+\\)\\(?:[ \t]*\\[[^,;\n]*\\]\\)?\\(?:[ \t]*=[ \t]*\\(?:\\sw\\|\\s_\\|[ \t]\\)+\\)?"
  "A regexp that matches the list part of SourcePawn variable declarations, e.g. 'String:test[256] = \"test\"'. The variable name must be in group 1.")

(defvar sourcepawn-mode-font-lock-regexp-variable-names-prefix
  "\\(?:new\\|decl\\)[ \t]+"
  "A regexp that matches the part of a variable declaration before the variable names list. Must have no numbered groups.")

(defvar sourcepawn-mode-font-lock-regexp-variable-names-seperator
  "[ \t]*,[ \t]*"
  "A regexp that matches the seperator between a variable declaration list element. Must have no numbered groups.")

;; helper to tell us when our last match succeeded
(defvar sourcepawn-mode-font-lock-flag-inside-variable-declaration
  nil
  "A flag that, when t, means sourcepawn-mode-font-lock-matcher-variable-names is inside a variable declaration.")

;; the function to match variable names
(defun sourcepawn-mode-font-lock-matcher-variable-names (limit)
  "A font lock matcher function for SourcePawn variable declarations."
  (let ((start-regexp (concat sourcepawn-mode-font-lock-regexp-variable-names-prefix sourcepawn-mode-font-lock-regexp-variable-names))
		(list-regexp (concat sourcepawn-mode-font-lock-regexp-variable-names-seperator sourcepawn-mode-font-lock-regexp-variable-names)))
	(if (not (and sourcepawn-mode-font-lock-flag-inside-variable-declaration (looking-at list-regexp)))
		(setq sourcepawn-mode-font-lock-flag-inside-variable-declaration (re-search-forward start-regexp limit t))
	  (setq sourcepawn-mode-font-lock-flag-inside-variable-declaration (re-search-forward list-regexp limit t)))))

;; set up the syntax table
(defvar sourcepawn-mode-syntax-table
  (let ((st (make-syntax-table)))
	;; make _ a words character, so tokens == words, and word movement commands make sense
	(modify-syntax-entry ?_ "w" st)
	
	;; syntax classes used in C and C++ style comments
	(modify-syntax-entry ?/ "_ 124b" st)
	(modify-syntax-entry ?* "_ 23" st)
	(modify-syntax-entry (string-to-char "\n") "> b")
	st)
  "Syntax table for sourcepawn-mode.")

;; tells us when we are directly after a non-braced if/while/for statement
;; returns nil if not, or the indentation of the if/while/for if it is
(defun sourcepawn-mode-single-line-block-p ()
  "Tells us if the current line is a single-expression block, and returns the indentation of the start of that block."
  (save-excursion
	(beginning-of-line)
	;; can't be a special block if we start with a brace!
	(if (or (bobp) (looking-at "[ \t]*{"))
		nil
	  (forward-line -1)
	  (beginning-of-line)
	  (let ((limit-point (point)))
		(setq limit-point (save-excursion
							;; put point at the end of the line, or the start of a comment
							(if (re-search-forward "\\(//\\|/\\*\\)" (line-end-position) t)
								(match-beginning 1)
							  (line-end-position))))
		(if (and 
			 (re-search-forward "[ \t]*\\<\\(?:if\\|while\\|for\\)\\>[ \t]*([^\n]*)[ \t]*" limit-point t)
			 (equal (point) limit-point))
			(current-indentation)
		  nil)))))

;; a replacement for indent-line-to with sane point management
(defun sourcepawn-mode-indent-line-to (column)
  "Like indent-line-to but with sane point management."
  (if (string-match "^[ \\t]*$" (buffer-substring (point-at-bol) (point)))
	  (indent-line-to column)
	(save-excursion (indent-line-to column))))

;; our indentation function
(defun sourcepawn-mode-indent-line ()
  "Indent the current line as SourcePawn code."
  (interactive)
  ;; set ret to 'noindent to signal indentation cannot be done
  ;; endbrace-count stores how many "}" we see right at the beginning of the line
  (let (ret (endbrace-count 0))
	(sourcepawn-mode-indent-line-to
	 ;; make sure we don't indent to a negative
	 (max 0
		  (save-excursion
			(beginning-of-line)
			(if (bobp)
				0 ;; first line
			  ;; not first line, what should we indent to?
			  (let ((special-indent (sourcepawn-mode-single-line-block-p)))
				(if (not (null special-indent))
					(+ default-tab-width special-indent) ;; indent once, we're special
				  ;; we're not special :(
				  ;; check our relative matching-parens ()[]{} depth in the last line
				  ;; and indent in or out that much relative to last line's indentation
				  ;; count how many "}" there are on the line we will indent
				  ;; DECREMENT because we want these to act like the end of the last line
				  (save-excursion
					(while (and (looking-at "[ \t]*}") (re-search-forward "[ \t]*}" (line-end-position) t))
					  (setq endbrace-count (- endbrace-count 1)))
					;; first find last non-blank line, non-special line
					(forward-line -1)
					(while (and (not (bobp)) (or (looking-at "[ \t]*$") (sourcepawn-mode-single-line-block-p)))
					  (forward-line -1))
					;; count how many "}" there are at the beginning of the line (which is currently the last line)
					;; INCREMENT because these are working against what parse-partial-sexp finds
					(while (and (looking-at "[ \t]*}") (re-search-forward "[ \t]*}" (line-end-position) t))
					  (setq endbrace-count (+ endbrace-count 1)))
					;; add in the indentation for this S-EXP level
					(+ (current-indentation)
					   (* default-tab-width
						  (+ (car (parse-partial-sexp (line-beginning-position) (line-end-position)))
							 endbrace-count))))))))))
	ret))

;; Symbol lists auto-generated from SourcePawn includes. As such, there may be errors.
;; DO NOT CHANGE the parts after this comment, before the end -!- comment
;; IT WILL JUST CHANGE BACK
;; change the appropriate sp-reserved-keywords files in the source package
;; -!- start generated keywords

(defvar sourcepawn-mode-font-lock-regexp-keywords
  "\\<\\(assert\\|b\\(?:egin\\|reak\\)\\|c\\(?:ase\\|ellsof\\|hars\\|on\\(?:st\\|tinue\\)\\)\\|d\\(?:e\\(?:cl\\|fault\\)\\|o\\)\\|e\\(?:lse\\|num\\|xit\\)\\|f\\(?:or\\(?:ward\\)?\\|unc\\(?:enum\\|tag\\)\\)\\|if\\|n\\(?:ative\\|ew\\)\\|operator\\|public\\|return\\|s\\(?:izeof\\|t\\(?:atic\\|ock\\|ruct\\)\\|witch\\)\\|tagof\\|while\\)\\>"
  "An optimized regexp of SourcePawn keywords.")

(defvar sourcepawn-mode-font-lock-regexp-constants
  "\\<\\(c\\(?:ell\\(?:bits\\|m\\(?:ax\\|in\\)\\)\\|har\\(?:bits\\|m\\(?:ax\\|in\\)\\)\\)\\|debug\\|false\\|true\\|ucharmax\\)\\>"
  "An optimized regexp of SourcePawn constants.")

(defvar sourcepawn-mode-font-lock-regexp-types
  "\\<\\(F\\(?:ixed\\|loat\\)\\|String\\|bool\\)\\>"
  "An optimized regexp of SourcePawn types.")

(defvar sourcepawn-mode-font-lock-regexp-preprocessor
  "\\<\\(assert\\|ctrlchar\\|d\\(?:efined?\\|ynamic\\)\\|e\\(?:lse\\(?:if\\)?\\|mit\\|nd\\(?:i\\(?:f\\|nput\\)\\|script\\)\\|rror\\)\\|i\\(?:f\\|nclude\\)\\|library\\|p\\(?:ack\\|ragma\\)\\|rational\\|semicolon\\|t\\(?:\\(?:absiz\\|ryinclud\\)e\\)\\|un\\(?:def\\|used\\)\\)\\>"
  "An optimized regexp of SourcePawn preprocessor.")

(defvar sourcepawn-mode-font-lock-regexp-generated-constants
  "\\<\\(A\\(?:DM\\(?:FLAG_\\(?:BAN\\|C\\(?:H\\(?:A\\(?:NGEMAP\\|T\\)\\|EATS\\)\\|ON\\(?:FIG\\|VARS\\)\\|USTOM[1-6]\\)\\|GENERIC\\|KICK\\|PASSWORD\\|R\\(?:CON\\|ESERVATION\\|OOT\\)\\|SLAY\\|UNBAN\\|VOTE\\)\\|INMENU_\\(?:\\(?:PLAYER\\|SERVER\\|VOTING\\)COMMANDS\\)\\)\\|LL_VISIBLE_CONTENTS\\|PLRes_\\(?:Failure\\|S\\(?:ilentFailure\\|uccess\\)\\)\\|UT\\(?:HMETHOD_\\(?:IP\\|NAME\\|STEAM\\)\\|OLOAD_EXTENSIONS\\)\\|ccess_\\(?:Effective\\|Real\\)\\|dmin\\(?:Cache_\\(?:\\(?:Admin\\|Group\\|Override\\)s\\)\\|Flags_TOTAL\\|_\\(?:Ban\\|C\\(?:h\\(?:a\\(?:ngemap\\|t\\)\\|eats\\)\\|on\\(?:fig\\|vars\\)\\|ustom[1-6]\\)\\|Generic\\|Kick\\|Password\\|R\\(?:CON\\|eservation\\|oot\\)\\|Slay\\|Unban\\|Vote\\)\\)\\)\\|BANFLAG_\\(?:AUT\\(?:HID\\|O\\)\\|IP\\|NOKICK\\)\\|C\\(?:O\\(?:MMAND_\\(?:FILTER_\\(?:ALIVE\\|CONNECTED\\|DEAD\\|NO_\\(?:BOTS\\|IMMUNITY\\|MULTI\\)\\)\\|TARGET_\\(?:AMBIGUOUS\\|EMPTY_FILTER\\|IMMUNE\\|NO\\(?:NE\\|T_\\(?:ALIVE\\|DEAD\\|HUMAN\\|IN_GAME\\)\\)\\)\\)\\|NTENTS_\\(?:A\\(?:REAPORTAL\\|UX\\)\\|CURRENT_\\(?:0\\|180\\|270\\|90\\|DOWN\\|UP\\)\\|DE\\(?:BRIS\\|TAIL\\)\\|EMPTY\\|GRATE\\|HITBOX\\|IGNORE_NODRAW_OPAQUE\\|LADDER\\|M\\(?:IST\\|O\\(?:NSTER\\(?:CLIP\\)?\\|VEABLE\\)\\)\\|O\\(?:PAQUE\\|RIGIN\\)\\|PLAYERCLIP\\|S\\(?:LIME\\|OLID\\)\\|T\\(?:E\\(?:AM[12]\\|STFOGVOLUME\\)\\|RANSLUCENT\\)\\|UNUSED[56]\\|W\\(?:ATER\\|INDOW\\)\\)\\)\\|S_\\(?:SLOT_\\(?:C4\\|GRENADE\\|\\(?:PRIM\\|SECOND\\)ARY\\)\\|TEAM_\\(?:CT\\|NONE\\|SPECTATOR\\|T\\)\\)\\|o\\(?:mmand_\\(?:Allow\\|Deny\\)\\|nVar\\(?:Bound_\\(?:\\(?:Low\\|Upp\\)er\\)\\|Query_\\(?:Not\\(?:\\(?:Foun\\|Vali\\)d\\)\\|Okay\\|Protected\\)\\)\\|okie\\(?:Access_P\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)\\|Menu\\(?:Action_\\(?:\\(?:Display\\|Select\\)Option\\)\\|_\\(?:OnOff\\(?:_Int\\)?\\|YesNo\\(?:_Int\\)?\\)\\)\\)\\)\\)\\|D\\(?:B\\(?:Bind_\\(?:Float\\|Int\\|String\\)\\|Prio_\\(?:High\\|Low\\|Normal\\)\\|Val_\\(?:Data\\|Error\\|Null\\|TypeMismatch\\)\\)\\|ialogType_\\(?:AskConnect\\|Entry\\|M\\(?:enu\\|sg\\)\\|Text\\)\\)\\|E\\(?:T_\\(?:Event\\|Hook\\|\\(?:Ignor\\|Singl\\)e\\)\\|ventHookMode_P\\(?:ost\\(?:NoCopy\\)?\\|re\\)\\)\\|F\\(?:BEAM_\\(?:END\\(?:ENTITY\\|VISIBLE\\)\\|F\\(?:ADE\\(?:IN\\|OUT\\)\\|OREVER\\)\\|HALOBEAM\\|ISACTIVE\\|NOTILE\\|ONLYNOISEONCE\\|S\\(?:HADE\\(?:IN\\|OUT\\)\\|INENOISE\\|OLID\\|TART\\(?:ENTITY\\|VISIBLE\\)\\)\\|USE_HITBOXES\\)\\|CVAR_\\(?:ARCHIVE\\(?:_XBOX\\)?\\|C\\(?:HEAT\\|LIENTDLL\\)\\|D\\(?:ATACACHE\\|EMO\\|ONTRECORD\\)\\|FILESYSTEM\\|GAMEDLL\\|INPUTSYSTEM\\|LAUNCHER\\|MATERIAL_SYSTEM\\|N\\(?:E\\(?:TWORKSYSTEM\\|VER_AS_STRING\\)\\|O\\(?:NE\\|T\\(?:IFY\\|_CONNECTED\\)\\)\\)\\|P\\(?:LUGIN\\|R\\(?:INTABLEONLY\\|OTECTED\\)\\)\\|REPLICATED\\|S\\(?:OUNDSYSTEM\\|PONLY\\|TUDIORENDER\\)\\|TOOLSYSTEM\\|U\\(?:N\\(?:\\(?:LOGG\\|REGISTER\\)ED\\)\\|SERINFO\\)\\|VPHYSICS\\)\\|EATURECAP_COMMANDLISTENER\\|L\\(?:OAT_PI\\|_\\(?:A\\(?:IMTARGET\\|TCONTROLS\\)\\|BASEVELOCITY\\|C\\(?:LIENT\\|ONVEYOR\\)\\|D\\(?:ISSOLVING\\|ONTTOUCH\\|UCKING\\)\\|EDICT_\\(?:ALWAYS\\|CHANGED\\|D\\(?:IRTY_PVS_INFORMATION\\|ONTSEND\\)\\|F\\(?:REE\\|ULL\\(?:CHECK\\)?\\)\\|P\\(?:\\(?:ENDING_DORMANT_\\|VS\\)CHECK\\)\\)\\|F\\(?:AKECLIENT\\|LY\\|ROZEN\\|ULL_EDICT_CHANGED\\)\\|G\\(?:ODMODE\\|R\\(?:APHED\\|ENADE\\)\\)\\|IN\\(?:RAIN\\|WATER\\)\\|KILLME\\|N\\(?:OTARGET\\|PC\\)\\|O\\(?:BJECT\\|N\\(?:FIRE\\|GROUND\\|TRAIN\\)\\)\\|PARTIALGROUND\\|S\\(?:T\\(?:ATICPROP\\|EPMOVEMENT\\)\\|WIM\\)\\|TRANSRAGDOLL\\|UNBLOCKABLE_BY_PLAYER\\|W\\(?:ATERJUMP\\|ORLDBRUSH\\)\\)\\)\\|PERM_\\(?:G_\\(?:EXEC\\|READ\\|WRITE\\)\\|O_\\(?:EXEC\\|READ\\|WRITE\\)\\|U_\\(?:EXEC\\|READ\\|WRITE\\)\\)\\|eature\\(?:Status_\\(?:Available\\|Un\\(?:available\\|known\\)\\)\\|Type_\\(?:Capability\\|Native\\)\\)\\|ileT\\(?:ime_\\(?:Created\\|Last\\(?:Access\\|Change\\)\\)\\|ype_\\(?:Directory\\|File\\|Unknown\\)\\)\\|loat\\)\\|I\\(?:N\\(?:VALID_\\(?:ADMIN_ID\\|ENT_REFERENCE\\|F\\(?:CVAR_FLAGS\\|UNCTION\\)\\|GROUP_ID\\|HANDLE\\|MESSAGE_ID\\|STRING_\\(?:INDEX\\|TABLE\\)\\|TOPMENUOBJECT\\)\\|_\\(?:A\\(?:LT[12]\\|TTACK2?\\)\\|B\\(?:ACK\\|ULLRUSH\\)\\|CANCEL\\|DUCK\\|FORWARD\\|GRENADE[12]\\|JUMP\\|LEFT\\|MOVE\\(?:\\(?:LEF\\|RIGH\\)T\\)\\|R\\(?:ELOAD\\|IGHT\\|UN\\)\\|S\\(?:CORE\\|PEED\\)\\|USE\\|W\\(?:ALK\\|EAPON[12]\\)\\|ZOOM\\)\\)\\|TEMDRAW_\\(?:CONTROL\\|D\\(?:EFAULT\\|ISABLED\\)\\|IGNORE\\|NOTEXT\\|RAWLINE\\|SPACER\\)\\|dentity_\\(?:Core\\|\\(?:Extensio\\|Plugi\\)n\\)\\|mmunity_\\(?:Default\\|Global\\)\\)\\|KvData_\\(?:Color\\|Float\\|Int\\|N\\(?:UMTYPES\\|one\\)\\|Ptr\\|String\\|UInt64\\|WString\\)\\|L\\(?:A\\(?:NG_SERVER\\|ST_VISIBLE_CONTENTS\\)\\|isten_\\(?:Default\\|No\\|Yes\\)\\)\\|M\\(?:A\\(?:PLIST_FLAG_\\(?:CLEARARRAY\\|MAPSFOLDER\\|NO_DEFAULT\\)\\|SK_\\(?:ALL\\|NPC\\(?:SOLID\\(?:_BRUSHONLY\\)?\\|WORLDSTATIC\\)\\|OPAQUE\\(?:_AND_NPCS\\)?\\|PLAYERSOLID\\(?:_BRUSHONLY\\)?\\|S\\(?:HOT\\(?:_\\(?:\\(?:HUL\\|PORTA\\)L\\)\\)?\\|OLID\\(?:_BRUSHONLY\\)?\\|PLITAREAPORTAL\\)\\|VISIBLE\\(?:_AND_NPCS\\)?\\|WATER\\)\\|X\\(?:PLAYERS\\|_\\(?:LIGHTSTYLES\\|\\(?:NAME\\|TARGET\\)_LENGTH\\)\\)\\)\\|ENU\\(?:FLAG_\\(?:BUTTON_EXIT\\(?:BACK\\)?\\|NO_SOUND\\)\\|_\\(?:ACTIONS_\\(?:ALL\\|DEFAULT\\)\\|NO_PAGINATION\\|TIME_FOREVER\\)\\)\\|O\\(?:TDPANEL_TYPE_\\(?:FILE\\|INDEX\\|TEXT\\|URL\\)\\|VETYPE_\\(?:CUSTOM\\|FLY\\(?:GRAVITY\\)?\\|ISOMETRIC\\|LADDER\\|NO\\(?:CLIP\\|NE\\)\\|OBSERVER\\|PUSH\\|STEP\\|VPHYSICS\\|WALK\\)\\)\\|apChange_\\(?:Instant\\|\\(?:Map\\|Round\\)End\\)\\|enu\\(?:Action_\\(?:Cancel\\|D\\(?:isplay\\(?:Item\\)?\\|rawItem\\)\\|End\\|S\\(?:\\(?:elec\\|tar\\)t\\)\\|Vote\\(?:Cancel\\|End\\|Start\\)\\)\\|S\\(?:ource_\\(?:External\\|No\\(?:ne\\|rmal\\)\\|RawPanel\\)\\|tyle_\\(?:Default\\|Radio\\|Valve\\)\\)\\)\\)\\|N\\(?:etFlow_\\(?:Both\\|\\(?:Incom\\|Outgo\\)ing\\)\\|ominate_\\(?:A\\(?:dded\\|lreadyInVote\\)\\|InvalidMap\\|Replaced\\|VoteFull\\)\\)\\|Override_Command\\(?:Group\\)?\\|P\\(?:CRE_\\(?:CASELESS\\|DOTALL\\|EXTENDED\\|MULTILINE\\|NO_UTF8_CHECK\\|U\\(?:NGREEDY\\|TF8\\)\\)\\|LA\\(?:TFORM_MAX_PATH\\|YER_FLAG_BITS\\)\\|a\\(?:ram_\\(?:A\\(?:\\(?:n\\|rra\\)y\\)\\|Cell\\(?:ByRef\\)?\\|Float\\(?:ByRef\\)?\\|String\\|VarArgs\\)\\|th_SM\\)\\|l\\(?:Info_\\(?:Author\\|Description\\|Name\\|URL\\|Version\\)\\|ugin_\\(?:BadLoad\\|C\\(?:hanged\\|ontinue\\|reated\\)\\|Error\\|Failed\\|Handled\\|Loaded\\|Paused\\|Running\\|Stop\\|Uncompiled\\)\\)\\|rop\\(?:Field_\\(?:Entity\\|Float\\|Integer\\|String\\(?:_T\\)?\\|Unsupported\\|Vector\\)\\|_\\(?:Data\\|Send\\)\\)\\)\\|QUERYCOOKIE_FAILED\\|R\\(?:E\\(?:GEX_ERROR_\\(?:BAD\\(?:COUNT\\|MAGIC\\|NEWLINE\\|OPTION\\|PARTIAL\\|UTF8\\(?:_OFFSET\\)?\\)\\|CALLOUT\\|DFA_\\(?:RECURSE\\|U\\(?:COND\\|ITEM\\|MLIMIT\\)\\|WSSIZE\\)\\|INTERNAL\\|MATCHLIMIT\\|N\\(?:O\\(?:M\\(?:ATCH\\|EMORY\\)\\|NE\\|SUBSTRING\\)\\|ULL\\(?:WSLIMIT\\)?\\)\\|PARTIAL\\|RECURSIONLIMIT\\|UNKNOWN_OPCODE\\)\\|NDER\\(?:FX_\\(?:CLAMP_MIN_SCALE\\|DISTORT\\|E\\(?:NV_\\(?:RAIN\\|SNOW\\)\\|XPLODE\\)\\|F\\(?:ADE_\\(?:FAST\\|SLOW\\)\\|LICKER_\\(?:FAST\\|SLOW\\)\\)\\|GLOWSHELL\\|HOLOGRAM\\|MAX\\|NO\\(?:NE\\|_DISSIPATION\\)\\|PULSE_\\(?:FAST\\(?:_WIDER?\\)?\\|SLOW\\(?:_WIDE\\)?\\)\\|RAGDOLL\\|S\\(?:OLID_\\(?:FAST\\|SLOW\\)\\|POTLIGHT\\|TROBE_\\(?:FAST\\(?:ER\\)?\\|SLOW\\)\\)\\)\\|_\\(?:ENVIRONMENTAL\\|GLOW\\|NO\\(?:NE\\|RMAL\\)\\|TRANS\\(?:A\\(?:DD\\(?:FRAMEBLEND\\)?\\|LPHA\\(?:ADD\\)?\\)\\|COLOR\\|TEXTURE\\)\\|WORLDGLOW\\)\\)\\|QUIRE_\\(?:EXTENSIONS\\|PLUGIN\\)\\)\\|ayType_\\(?:EndPoint\\|Infinite\\)\\)\\|S\\(?:DK\\(?:C\\(?:all_\\(?:Entity\\(?:List\\)?\\|GameRules\\|Player\\|Static\\)\\|onf_\\(?:Signature\\|Virtual\\)\\)\\|Library_\\(?:Engine\\|Server\\)\\|Pass_\\(?:By\\(?:Ref\\|Value\\)\\|P\\(?:lain\\|ointer\\)\\)\\|Type_\\(?:Bool\\|CBase\\(?:Entity\\|Player\\)\\|Edict\\|Float\\|PlainOldData\\|QAngle\\|String\\|Vector\\)\\)\\|EEK_\\(?:CUR\\|END\\|SET\\)\\|M\\(?:C\\(?:Error_\\(?:Custom\\|Invalid\\(?:Property1\\|Section[1-5]\\|Tokens\\)\\|Okay\\|Stream\\(?:Error\\|Open\\)\\|TokenOverflow\\)\\|Parse_\\(?:Continue\\|Halt\\(?:Fail\\)?\\)\\)\\|_\\(?:PARAM_\\(?:COPYBACK\\|STRING_\\(?:BINARY\\|COPY\\|UTF8\\)\\)\\|REPLY_TO_C\\(?:HAT\\|ONSOLE\\)\\)\\)\\|ND\\(?:ATTN_\\(?:IDLE\\|NO\\(?:NE\\|RMAL\\)\\|RICOCHET\\|STATIC\\)\\|PITCH_\\(?:HIGH\\|LOW\\|NORMAL\\)\\|VOL_NORMAL\\)\\|OU\\(?:ND_FROM_\\(?:LOCAL_PLAYER\\|PLAYER\\|WORLD\\)\\|RCE\\(?:MOD_\\(?:PLUGINAPI_VERSION\\|V\\(?:ERSION\\|_\\(?:M\\(?:\\(?:AJ\\|IN\\)OR\\)\\|RELEASE\\)\\)\\)\\|_SDK_\\(?:DARKMESSIAH\\|EPISODE\\(?:2VALVE\\|[12]\\)\\|LEFT4DEAD2?\\|ORIGINAL\\|UNKNOWN\\)\\)\\)\\|P_\\(?:ERROR_\\(?:A\\(?:BORTED\\|RRAY_\\(?:BOUNDS\\|TOO_BIG\\)\\)\\|D\\(?:ECOMPRESSOR\\|IVIDE_BY_ZERO\\)\\|FILE_FORMAT\\|HEAP\\(?:L\\(?:EAK\\|OW\\)\\|MIN\\)\\|IN\\(?:DEX\\|STRUCTION_PARAM\\|VALID_\\(?:ADDRESS\\|INSTRUCTION\\|NATIVE\\)\\)\\|MEMACCESS\\|N\\(?:ATIVE\\|O\\(?:NE\\|T\\(?:DEBUGGING\\|_\\(?:FOUND\\|RUNNABLE\\)\\)\\)\\)\\|PARAM\\(?:S_MAX\\)?\\|STACK\\(?:L\\(?:EAK\\|OW\\)\\|MIN\\)\\|TRACKER_BOUNDS\\)\\|PARAMFLAG_BYREF\\)\\|ort_\\(?:Ascending\\|Descending\\|Float\\|Integer\\|Random\\|String\\)\\|tring\\)\\|T\\(?:E\\(?:MP_REQUIRE_EXTENSIONS\\|_EXPLFLAG_\\(?:DRAWALPHA\\|NO\\(?:ADDITIVE\\|DLIGHTS\\|FIREBALL\\(?:SMOKE\\)?\\|NE\\|PARTICLES\\|SOUND\\)\\|ROTATE\\)\\)\\|F\\(?:C\\(?:lass\\(?:Type\\|_\\(?:DemoMan\\|Engineer\\|Heavy\\|Medic\\|Pyro\\|S\\(?:cout\\|niper\\|oldier\\|py\\)\\|Unknown\\)\\)\\|ond\\(?:_\\(?:B\\(?:\\(?:onk\\|uff\\)ed\\)\\|C\\(?:harging\\|loaked\\|ritCola\\)\\|D\\(?:azed\\|e\\(?:adRingered\\|moBuff\\)\\|isguis\\(?:ed\\|ing\\)\\)\\|Healing\\|Jarated\\|Kritzkrieged\\|O\\(?:nFire\\|verhealed\\)\\|Slowed\\|T\\(?:aunting\\|eleport\\(?:edGlow\\|ing\\)\\)\\|U\\(?:bercharge\\(?:Fading\\|d\\)\\|nknown[12]\\)\\|Zoomed\\)\\)?\\)\\|Resource_\\(?:B\\(?:ackstabs\\|uildingsDestroyed\\)\\|Captures\\|D\\(?:\\(?:e\\(?:ath\\|fense\\)\\|omination\\)s\\)\\|Hea\\(?:\\(?:dsho\\|lPoin\\)ts\\)\\|Invulns\\|KillAssists\\|MaxHealth\\|P\\(?:ing\\|layerClass\\)\\|Re\\(?:supplyPoints\\|venge\\)\\|Score\\|T\\(?:eleports\\|otalScore\\)\\)\\|Team\\(?:_\\(?:Blue\\|Red\\|Spectator\\|Unassigned\\)\\)?\\|_\\(?:CONDFLAG_\\(?:B\\(?:\\(?:ONK\\|UFF\\)ED\\)\\|C\\(?:HARGING\\|LOAKED\\|RITCOLA\\)\\|D\\(?:AZED\\|E\\(?:ADRINGERED\\|MOBUFF\\)\\|ISGUIS\\(?:ED\\|ING\\)\\)\\|HEALING\\|JARATED\\|KRITZKRIEGED\\|NONE\\|O\\(?:NFIRE\\|VERHEALED\\)\\|SLOWED\\|T\\(?:AUNTING\\|ELEPORT\\(?:GLOW\\|ING\\)\\)\\|UBERCHARGE\\(?:D\\|FADE\\)\\|ZOOMED\\)\\|DEATHFLAG_\\(?:ASSISTER\\(?:DOMINATION\\|REVENGE\\)\\|DEADRINGER\\|FIRSTBLOOD\\|KILLER\\(?:DOMINATION\\|REVENGE\\)\\)\\|STUNFLAG\\(?:S_\\(?:BIGBONK\\|GHOSTSCARE\\|LOSERSTATE\\|\\(?:NORMA\\|SMAL\\)LBONK\\)\\|_\\(?:BONKSTUCK\\|CHEERSOUND\\|GHOSTEFFECT\\|LIMITMOVEMENT\\|NOSOUNDOREFFECT\\|\\(?:SLOWDOW\\|THIRDPERSO\\)N\\)\\)\\)\\)\\|IMER_\\(?:DATA_HNDL_CLOSE\\|FLAG_NO_MAPCHANGE\\|HNDL_CLOSE\\|REPEAT\\)\\|opMenu\\(?:Action_\\(?:D\\(?:isplay\\(?:Option\\|Title\\)\\|rawOption\\)\\|RemoveObject\\|SelectOption\\)\\|Object_\\(?:Category\\|Item\\)\\|Position_\\(?:Last\\(?:Category\\|Root\\)\\|Start\\)\\)\\)\\|USERMSG_\\(?:BLOCKHOOKS\\|INITMSG\\|RELIABLE\\)\\|V\\(?:DECODE_FLAG_\\(?:ALLOW\\(?:N\\(?:OTINGAME\\|ULL\\)\\|WORLD\\)\\|BYREF\\)\\|ENCODE_FLAG_COPYBACK\\|O\\(?:ICE_\\(?:LISTEN\\(?:ALL\\|TEAM\\)\\|MUTED\\|NORMAL\\|SPEAKALL\\|TEAM\\)\\|TE\\(?:FLAG_NO_REVOTES\\|INFO_\\(?:CLIENT_I\\(?:NDEX\\|TEM\\)\\|ITEM_\\(?:INDEX\\|VOTES\\)\\)\\)\\)\\)\\|attacker\\|bool\\|native\\|stunflags\\|target\\|weapon\\)\\>"
  "An optimized regexp of SourcePawn generated-constants.")

(defvar sourcepawn-mode-font-lock-regexp-generated-types
  "\\<\\(A\\(?:PLRes\\|ction\\|dm\\(?:AccessMode\\|in\\(?:CachePart\\|Flag\\|Id\\)\\)\\)\\|Co\\(?:nVar\\(?:Bounds\\|QueryResult\\)\\|okie\\(?:Access\\|Menu\\(?:Action\\)?\\)\\)\\|D\\(?:B\\(?:BindType\\|Priority\\|Result\\)\\|ialogType\\)\\|E\\(?:ventHookMode\\|x\\(?:ecType\\|tension\\)\\)\\|F\\(?:eature\\(?:Status\\|Type\\)\\|ileT\\(?:\\(?:imeMod\\|yp\\)e\\)\\|unction\\)\\|GroupId\\|Handle\\|I\\(?:dentity\\|mmunityType\\)\\|KvDataTypes\\|ListenOverride\\|M\\(?:apChange\\|enu\\(?:Action\\|S\\(?:\\(?:ourc\\|tyl\\)e\\)\\)\\|oveType\\)\\|N\\(?:etFlow\\|ominateResult\\)\\|Override\\(?:\\(?:Rul\\|Typ\\)e\\)\\|P\\(?:a\\(?:\\(?:ram\\|th\\)Type\\)\\|l\\(?:Vers\\|ugin\\(?:Info\\|Status\\)?\\)\\|rop\\(?:\\(?:Field\\)?Type\\)\\)\\|QueryCookie\\|R\\(?:ayType\\|e\\(?:gexError\\|nder\\(?:Fx\\|Mode\\)\\|plySource\\)\\)\\|S\\(?:DK\\(?:CallType\\|FuncConfSource\\|Library\\|PassMethod\\|Type\\)\\|MC\\(?:Error\\|Result\\)\\|haredPlugin\\|ort\\(?:Order\\|Type\\)\\)\\|T\\(?:F\\(?:C\\(?:lassType\\|ond\\)\\|ResourceType\\|Team\\)\\|opMenu\\(?:Action\\|Object\\(?:Type\\)?\\|Position\\)\\)\\|UserMsg\\)\\>"
  "An optimized regexp of SourcePawn generated-types.")

(defvar sourcepawn-mode-font-lock-regexp-generated-forwards
  "\\<\\(AskPluginLoad2?\\|On\\(?:A\\(?:dminMenu\\(?:Created\\|Ready\\)\\|\\(?:llPluginsLoad\\|utoConfigsBuffer\\)ed\\)\\|Ban\\(?:Client\\|Identity\\)\\|C\\(?:lient\\(?:Authorized\\|Co\\(?:mmand\\|nnect\\(?:ed\\)?\\|okiesCached\\)\\|Disconnect\\(?:_Post\\)?\\|Flood\\(?:Check\\|Result\\)\\|P\\(?:ostAdmin\\(?:Check\\|Filter\\)\\|reAdminCheck\\|utInServer\\)\\|SettingsChanged\\)\\|onfigsExecuted\\)\\|GameFrame\\|L\\(?:ibrary\\(?:\\(?:Add\\|Remov\\)ed\\)\\|ogAction\\)\\|Map\\(?:End\\|Start\\|TimeLeftChanged\\)\\|NominationRemoved\\|Pl\\(?:ayerRunCmd\\|ugin\\(?:End\\|PauseChange\\|Start\\)\\)\\|Re\\(?:buildAdminCache\\|moveBan\\)\\|ServerCfg\\)\\|TF2_CalcIsAttackCritical\\|operator%\\)\\>"
  "An optimized regexp of SourcePawn generated-forwards.")

(defvar sourcepawn-mode-font-lock-regexp-generated-natives-stocks
  "\\<\\(A\\(?:TTN_TO_SNDLEVEL\\|c\\(?:ceptEntityInput\\|tivateEntity\\)\\|d\\(?:d\\(?:A\\(?:dmGroupCmdOverride\\|mbientSoundHook\\)\\|Command\\(?:Listener\\|Override\\)\\|FileToDownloadsTable\\|GameLogHook\\|MenuItem\\|NormalSoundHook\\|ServerTag\\|T\\(?:argetsToMenu2?\\|empEntHook\\|o\\(?:Forward\\|StringTable\\|TopMenu\\)\\)\\|\\(?:UserFlag\\|Vector\\)s\\)\\|minInheritGroup\\)\\|r\\(?:c\\(?:Cosine\\|Sine\\|Tangent2?\\)\\|eClientCookiesCached\\)\\|utoExecConfig\\)\\|B\\(?:an\\(?:Client\\|Identity\\)\\|f\\(?:GetNumBytesLeft\\|Read\\(?:Angles?\\|B\\(?:ool\\|yte\\)\\|C\\(?:har\\|oord\\)\\|Entity\\|Float\\|Num\\|S\\(?:hort\\|tring\\)\\|Vec\\(?:Coord\\|Normal\\)\\|Word\\)\\|Write\\(?:Angles?\\|B\\(?:ool\\|yte\\)\\|C\\(?:har\\|oord\\)\\|Entity\\|Float\\|Num\\|S\\(?:hort\\|tring\\)\\|Vec\\(?:Coord\\|Normal\\)\\|Word\\)\\)\\|i\\(?:ndAdminIdentity\\|tToFlag\\)\\|reakString\\|uildPath\\|yteCountToCells\\)\\|C\\(?:S_\\(?:RespawnPlayer\\|SwitchTeam\\)\\|a\\(?:ll_\\(?:Cancel\\|Finish\\|Push\\(?:Array\\(?:Ex\\)?\\|Cell\\(?:Ref\\)?\\|Float\\(?:Ref\\)?\\|String\\(?:Ex\\)?\\)\\|StartF\\(?:orward\\|unction\\)\\)\\|n\\(?:AdminTarget\\|MapChooserStartVote\\|PanelDrawFlags\\|TestFeatures\\|UserTarget\\|cel\\(?:C\\(?:lientMenu\\|reatedEvent\\)\\|Menu\\|Vote\\)\\)\\)\\|h\\(?:a\\(?:nge\\(?:ClientTeam\\|EdictState\\)\\|rTo\\(?:\\(?:Low\\|Upp\\)er\\)\\)\\|eck\\(?:CommandAccess\\|VoteDelay\\)\\)\\|l\\(?:ear\\(?:Array\\|SyncHud\\|Trie\\)\\|ientCommand\\|o\\(?:ne\\(?:Array\\|Handle\\)\\|seHandle\\)\\)\\|o\\(?:mpileRegex\\|sine\\)\\|reate\\(?:A\\(?:dm\\(?:Group\\|in\\)\\|rray\\|uthMethod\\)\\|ConVar\\|D\\(?:ata\\(?:Pack\\|Timer\\)\\|i\\(?:alog\\|rectory\\)\\)\\|E\\(?:dict\\|ntityByName\\|vent\\)\\|F\\(?:akeClient\\|orward\\)\\|GlobalForward\\|HudSynchronizer\\|KeyValues\\|Menu\\(?:Ex\\)?\\|Native\\|P\\(?:anel\\(?:FromMenu\\)?\\|rofiler\\)\\|Stack\\|T\\(?:imer\\|opMenu\\|rie\\)\\)\\)\\|D\\(?:e\\(?:gToRad\\|leteFile\\)\\|i\\(?:rExists\\|sp\\(?:atch\\(?:KeyValue\\(?:Float\\|Vector\\)?\\|Spawn\\)\\|lay\\(?:AskConnectBox\\|Menu\\(?:AtItem\\)?\\|TopMenu\\)\\)\\)\\|rawPanel\\(?:Item\\|Text\\)\\|umpAdminCache\\)\\|E\\(?:mit\\(?:AmbientSound\\|S\\(?:entence\\|ound\\(?:To\\(?:All\\|Client\\)\\)?\\)\\)\\|n\\(?:d\\(?:Message\\|OfMapVoteEnabled\\|PrepSDKCall\\)\\|t\\(?:IndexToEntRef\\|RefToEntIndex\\)\\)\\|quipPlayerWeapon\\|x\\(?:p\\(?:lodeString\\|onential\\)\\|t\\(?:endMapTimeLimit\\|inguishEntity\\)\\)\\)\\|F\\(?:a\\(?:deClientVolume\\|keClientCommand\\(?:Ex\\)?\\)\\|i\\(?:le\\(?:Exists\\|Position\\|S\\(?:eek\\|ize\\)\\|ToKeyValues\\)\\|nd\\(?:Adm\\(?:Group\\|inByIdentity\\)\\|C\\(?:harInString\\|lientCookie\\|onVar\\)\\|DataMapOffs\\|EntityByClassname\\|F\\(?:irstConCommand\\|lagBy\\(?:Char\\|Name\\)\\)\\|NextConCommand\\|PluginBy\\(?:File\\|Number\\)\\|S\\(?:endProp\\(?:Info\\|Offs\\)\\|tring\\(?:In\\(?:Array\\|dex\\)\\|Table\\)\\)\\|T\\(?:arget\\|eamByName\\|opMenuCategory\\)\\|ValueInArray\\)\\|reEvent\\)\\|l\\(?:ag\\(?:ArrayToBits\\|Bit\\(?:ArrayToBits\\|sTo\\(?:\\(?:Bit\\)?Array\\)\\)\\|ToBit\\)\\|oat\\(?:A\\(?:bs\\|dd\\)\\|Compare\\|Div\\|Fraction\\|Mul\\|Sub\\|ToString\\)\\|ushFile\\)\\|or\\(?:ce\\(?:ChangeLevel\\|PlayerSuicide\\)\\|mat\\(?:ActivitySource\\|Ex\\|NativeString\\|Time\\|UserLogText\\)?\\)\\)\\|G\\(?:ameConfGet\\(?:KeyValue\\|Offset\\)\\|e\\(?:oipCo\\(?:de[23]\\|untry\\)\\|t\\(?:A\\(?:dm\\(?:Group\\(?:AddFlags?\\|CmdOverride\\|Immun\\(?:e\\(?:Count\\|From\\)\\|ity\\(?:Level\\)?\\)\\)\\|in\\(?:Flags?\\|Group\\(?:Count\\)?\\|ImmunityLevel\\|Password\\|TopMenu\\|Username\\)\\)\\|ngleVectors\\|rray\\(?:Array\\|Cell\\|S\\(?:ize\\|tring\\)\\)\\)\\|C\\(?:harBytes\\|lient\\(?:A\\(?:bs\\(?:Angles\\|Origin\\)\\|imTarget\\|rmor\\|uthString\\|vg\\(?:Choke\\|Data\\|L\\(?:atency\\|oss\\)\\|Packets\\)\\)\\|Buttons\\|Co\\(?:okie\\(?:Time\\)?\\|unt\\)\\|D\\(?:ataRate\\|eaths\\)\\|Eye\\(?:Angles\\|Position\\)\\|Fr\\(?:ags\\|omSerial\\)\\|Health\\|I\\(?:P\\|nfo\\)\\|L\\(?:a\\(?:nguage\\|tency\\)\\|istening\\(?:Flags\\)?\\)\\|M\\(?:axs\\|enu\\|ins\\|odel\\)\\|Name\\|OfUserId\\|Serial\\|T\\(?:eam\\|ime\\)\\|UserId\\|Weapon\\)\\|md\\(?:Arg\\(?:String\\|s\\)?\\|ReplySource\\)\\|o\\(?:mmand\\(?:Flags\\|Iterator\\|Override\\)\\|nVar\\(?:Bo\\(?:ol\\|unds\\)\\|Fl\\(?:ags\\|oat\\)\\|Int\\|Name\\|String\\)\\|okie\\(?:Access\\|Iterator\\)\\)\\|urrentMap\\)\\|E\\(?:dict\\(?:Classname\\|Flags\\)\\|n\\(?:gineTime\\|t\\(?:Data\\(?:Array\\|Ent2?\\|Float\\|String\\|Vector\\)?\\|Prop\\(?:Ent\\|Float\\|String\\|Vector\\)?\\|SendPropOffs\\|ity\\(?:Count\\|Flags\\|Gravity\\|MoveType\\|NetClass\\|Render\\(?:Fx\\|Mode\\)\\)\\)\\)\\|vent\\(?:Bool\\|Float\\|Int\\|Name\\|String\\)\\|x\\(?:cludeMapList\\|tensionFileStatus\\)\\)\\|F\\(?:eatureStatus\\|ileTime\\|orwardFunctionCount\\|unctionByName\\)\\|Game\\(?:Description\\|\\(?:FolderNa\\|Ti\\)me\\)\\|L\\(?:anguage\\(?:Count\\|Info\\)\\|istenOverride\\)\\|M\\(?:a\\(?:p\\(?:History\\(?:Size\\)?\\|TimeL\\(?:\\(?:ef\\|imi\\)t\\)\\)\\|x\\(?:\\(?:Client\\|Entitie\\|PageItem\\)s\\)\\)\\|enu\\(?:ExitB\\(?:\\(?:ackB\\)?utton\\)\\|Item\\(?:Count\\)?\\|OptionFlags\\|Pagination\\|S\\(?:electionPosition\\|tyle\\(?:Handle\\)?\\)\\|Title\\|VoteInfo\\)\\|yHandle\\)\\|N\\(?:ative\\(?:Array\\|Cell\\(?:Ref\\)?\\|String\\(?:Length\\)?\\)\\|extMap\\|umStringTables\\)\\|P\\(?:a\\(?:ckPosition\\|nel\\(?:CurrentKey\\|Style\\|TextRemaining\\)\\)\\|l\\(?:ayer\\(?:DecalFile\\|WeaponSlot\\)\\|ugin\\(?:Filename\\|I\\(?:nfo\\|terator\\)\\|Status\\)\\)\\|rofilerTime\\)\\|R\\(?:andom\\(?:\\(?:Floa\\|In\\)t\\)\\|egexSubString\\)\\|S\\(?:erver\\(?:Language\\|NetStats\\)\\|oundDuration\\|tringTable\\(?:Data\\(?:Length\\)?\\|MaxStrings\\|N\\(?:ame\\|umStrings\\)\\)\\|ysTickCount\\)\\|T\\(?:eam\\(?:C\\(?:\\(?:lientC\\)?ount\\)\\|\\(?:Nam\\|Scor\\)e\\)\\|i\\(?:ck\\(?:Interval\\|edTime\\)\\|me\\)\\|opMenu\\(?:InfoString\\|ObjName\\)\\|rie\\(?:Array\\|S\\(?:ize\\|tring\\)\\|Value\\)\\)\\|U\\(?:Random\\(?:\\(?:Floa\\|In\\)t\\)\\|ser\\(?:Admin\\|FlagBits\\|Message\\(?:Id\\|Name\\)\\)\\)\\|Vector\\(?:Angles\\|CrossProduct\\|D\\(?:istance\\|otProduct\\)\\|Length\\|Vectors\\)\\)\\)\\|ivePlayerItem\\|uessSDKVersion\\)\\|H\\(?:asEndOfMapVoteFinished\\|ook\\(?:ConVarChange\\|E\\(?:ntityOutput\\|vent\\(?:Ex\\)?\\)\\|SingleEntityOutput\\|UserMessage\\)\\)\\|I\\(?:gniteEntity\\|mplodeStrings\\|n\\(?:itiateMapChooserVote\\|sert\\(?:MenuItem\\|ServerCommand\\)\\|t\\(?:ToString\\|ernalShowMenu\\)\\)\\|s\\(?:C\\(?:ha\\(?:r\\(?:Alpha\\|Lower\\|MB\\|Numeric\\|Space\\|Upper\\)\\|tTrigger\\)\\|lient\\(?:Authorized\\|Connected\\|In\\(?:Game\\|KickQueue\\|VotePool\\)\\|Muted\\|Observer\\|TimingOut\\)\\)\\|De\\(?:calPrecached\\|dicatedServer\\)\\|En\\(?:\\(?:dOfFi\\|tNetworkab\\)le\\)\\|FakeClient\\|GenericPrecached\\|M\\(?:\\(?:apVali\\|odelPrecache\\)d\\)\\|NewVoteAllowed\\|P\\(?:ackReadable\\|l\\(?:ayer\\(?:\\(?:Aliv\\|InGam\\)e\\)\\|uginDebugging\\)\\)\\|S\\(?:erverProcessing\\|oundPrecached\\|tackEmpty\\)\\|V\\(?:alid\\(?:ConVarChar\\|E\\(?:dict\\|ntity\\)\\|Handle\\)\\|oteInProgress\\)\\)\\)\\|K\\(?:eyValuesToFile\\|i\\(?:ckClient\\(?:Ex\\)?\\|llTimer\\)\\|v\\(?:CopySubkeys\\|Delete\\(?:Key\\|This\\)\\|FindKeyById\\|G\\(?:et\\(?:Color\\|DataType\\|Float\\|N\\(?:ameSymbol\\|um\\)\\|S\\(?:ection\\(?:Name\\|Symbol\\)\\|tring\\)\\|UInt64\\|Vector\\)\\|o\\(?:Back\\|to\\(?:\\(?:FirstSub\\|Next\\)Key\\)\\)\\)\\|JumpToKey\\(?:Symbol\\)?\\|NodesInStack\\|Rewind\\|S\\(?:avePosition\\|et\\(?:Color\\|EscapeSequences\\|Float\\|Num\\|S\\(?:ectionName\\|tring\\)\\|UInt64\\|Vector\\)\\)\\)\\)\\|L\\(?:ibraryExists\\|o\\(?:ad\\(?:GameConfigFile\\|Maps\\|T\\(?:opMenuConfig\\|ranslations\\)\\)\\|ckStringTables\\|g\\(?:Action\\|Error\\|Message\\(?:Ex\\)?\\|To\\(?:File\\(?:Ex\\)?\\|Game\\|OpenFile\\(?:Ex\\)?\\)\\|arithm\\)\\)\\)\\|M\\(?:a\\(?:ke\\(?:CompatEntRef\\|VectorFromPoints\\)\\|rkNativeAsOptional\\|tchRegex\\)\\|orePlugins\\)\\|N\\(?:egateVector\\|o\\(?:minateMap\\|rmalizeVector\\|tifyPostAdminCheck\\)\\)\\|Open\\(?:Directory\\|File\\)\\|P\\(?:o\\(?:pStack\\(?:Array\\|Cell\\|String\\)?\\|w\\)\\|r\\(?:e\\(?:cache\\(?:Decal\\|Generic\\|Model\\|S\\(?:entenceFile\\|ound\\)\\)\\|fetchSound\\|pSDKCall_\\(?:AddParameter\\|Set\\(?:FromConf\\|ReturnInfo\\|Signature\\|Virtual\\)\\)\\)\\|int\\(?:CenterText\\(?:All\\)?\\|HintText\\(?:ToAll\\)?\\|To\\(?:C\\(?:hat\\(?:All\\)?\\|onsole\\)\\|Server\\)\\)\\|ocessTargetString\\)\\|ush\\(?:Array\\(?:Array\\|Cell\\|String\\)\\|Stack\\(?:Array\\|Cell\\|String\\)\\)\\)\\|QueryClientConVar\\|R\\(?:adToDeg\\|e\\(?:ad\\(?:Co\\(?:\\(?:mmand\\|okie\\)Iterator\\)\\|DirEntry\\|F\\(?:ile\\(?:Cell\\|Line\\|String\\)?\\|lagString\\)\\|MapList\\|P\\(?:ack\\(?:Cell\\|Float\\|String\\)\\|lugin\\)\\|StringTable\\)\\|d\\(?:isplayAdminMenu\\|raw\\(?:ClientVoteMenu\\|MenuItem\\)\\)\\|g\\(?:AdminCmd\\|C\\(?:lientCookie\\|onsoleCmd\\)\\|PluginLibrary\\|ServerCmd\\|isterAuthIdentType\\)\\|move\\(?:A\\(?:dmin\\|ll\\(?:FromForward\\|MenuItems\\)\\|mbientSoundHook\\)\\|Ban\\|CommandListener\\|Dir\\|Edict\\|From\\(?:Array\\|Forward\\|T\\(?:opMenu\\|rie\\)\\)\\|GameLogHook\\|MenuItem\\|NormalSoundHook\\|PlayerItem\\|ServerTag\\|TempEntHook\\|UserFlags\\)\\|nameFile\\|pl\\(?:aceString\\(?:Ex\\)?\\|yTo\\(?:Command\\|TargetError\\)\\)\\|quireFeature\\|s\\(?:et\\(?:ConVar\\|Pack\\)\\|izeArray\\)\\)\\|ound\\(?:Float\\|To\\(?:Ceil\\|Floor\\|Nearest\\|Zero\\)\\)\\|unAdminCacheChecks\\)\\|S\\(?:DKCall\\|MC_\\(?:CreateParser\\|GetErrorString\\|ParseFile\\|Set\\(?:Parse\\(?:End\\|Start\\)\\|R\\(?:awLine\\|eaders\\)\\)\\)\\|QL\\(?:_\\(?:BindParam\\(?:Float\\|Int\\|String\\)\\|C\\(?:heckConfig\\|onnect\\(?:Custom\\|Ex\\)?\\)\\|DefConnect\\|E\\(?:scapeString\\|xecute\\)\\|F\\(?:astQuery\\|etch\\(?:Float\\|Int\\|MoreResults\\|Row\\|S\\(?:ize\\|tring\\)\\)\\|ieldN\\(?:ameToNum\\|umToName\\)\\)\\|Get\\(?:AffectedRows\\|Driver\\(?:\\(?:Iden\\|Produc\\)t\\)?\\|Error\\|FieldCount\\|InsertId\\|RowCount\\)\\|HasResultSet\\|Is\\(?:FieldNull\\|SameConnection\\)\\|LockDatabase\\|MoreRows\\|PrepareQuery\\|Qu\\(?:ery\\|oteString\\)\\|Re\\(?:adDriver\\|wind\\)\\|T\\(?:Connect\\|Query\\)\\|UnlockDatabase\\)\\|ite_UseDatabase\\)\\|caleVector\\|e\\(?:archForClients\\|nd\\(?:ConVarValue\\|PanelToClient\\)\\|rver\\(?:Command\\|Execute\\)\\|t\\(?:A\\(?:dm\\(?:Group\\(?:AddFlag\\|Immun\\(?:eFrom\\|ity\\(?:Level\\)?\\)\\)\\|in\\(?:Flag\\|ImmunityLevel\\|Password\\)\\)\\|rray\\(?:Array\\|Cell\\|String\\)\\)\\|C\\(?:lient\\(?:Cookie\\|Info\\|Listening\\(?:Flags\\)?\\|ViewEntity\\)\\|mdReplySource\\|o\\(?:mmandFlags\\|nVar\\(?:Bo\\(?:ol\\|unds\\)\\|Fl\\(?:ags\\|oat\\)\\|Int\\|String\\)\\|okie\\(?:MenuItem\\|PrefabMenu\\)\\)\\)\\|E\\(?:dictFlags\\|nt\\(?:Data\\(?:Array\\|Ent2?\\|Float\\|String\\|Vector\\)?\\|Prop\\(?:Ent\\|Float\\|String\\|Vector\\)?\\|ity\\(?:Gravity\\|Health\\|Mo\\(?:del\\|veType\\)\\|Render\\(?:Color\\|Fx\\|Mode\\)\\)\\)\\|vent\\(?:B\\(?:ool\\|roadcast\\)\\|Float\\|Int\\|String\\)\\)\\|Fa\\(?:ilState\\|keClientConVar\\)\\|GlobalTransTarget\\|HudTextParams\\(?:Ex\\)?\\|Li\\(?:\\(?:ghtStyl\\|stenOverrid\\)e\\)\\|M\\(?:apListCompatBind\\|enu\\(?:ExitB\\(?:\\(?:ackB\\)?utton\\)\\|OptionFlags\\|Pagination\\|Title\\)\\)\\|N\\(?:ative\\(?:Array\\|CellRef\\|String\\)\\|extMap\\)\\|Pa\\(?:ckPosition\\|nel\\(?:CurrentKey\\|Keys\\|Title\\)\\)\\|RandomSeed\\|StringTableData\\|T\\(?:eamScore\\|rie\\(?:Array\\|String\\|Value\\)\\)\\|U\\(?:RandomSeed\\(?:Simple\\)?\\|ser\\(?:Admin\\|FlagBits\\)\\)\\|V\\(?:ariant\\(?:Bool\\|Color\\|Entity\\|Float\\|Int\\|PosVector3D\\|String\\|Vector3D\\)\\|oteResultCallback\\)\\)\\)\\|h\\(?:iftArrayUp\\|ow\\(?:Activity\\(?:2\\|Ex\\)?\\|CookieMenu\\|HudText\\|MOTDPanel\\|SyncHudText\\|VGUIPanel\\)\\)\\|i\\(?:mpleRegexMatch\\|ne\\)\\|lapPlayer\\|ort\\(?:ADTArray\\(?:Custom\\)?\\|Custom\\(?:[12]D\\)\\|\\(?:Float\\|Integer\\|String\\)s\\)\\|plitString\\|quareRoot\\|t\\(?:art\\(?:Message\\(?:All\\|Ex\\|One\\)?\\|Pr\\(?:epSDKCall\\|ofiling\\)\\)\\|op\\(?:Profiling\\|Sound\\)\\|r\\(?:Break\\|C\\(?:at\\|o\\(?:mpare\\|ntains\\|py\\)\\)\\|Equal\\|i\\(?:ngTo\\(?:Float\\(?:Ex\\)?\\|Int\\(?:Ex\\)?\\)\\|pQuotes\\)\\)\\)\\|\\(?:ubtractVector\\|wapArrayItem\\)s\\)\\|T\\(?:E_\\(?:IsValidProp\\|Read\\(?:Float\\|Num\\|Vector\\)\\|S\\(?:e\\(?:nd\\(?:To\\(?:All\\|Client\\)\\)?\\|tup\\(?:ArmorRicochet\\|B\\(?:eam\\(?:Follow\\|Laser\\|Points\\|Ring\\(?:Point\\)?\\)\\|loodSprite\\)\\|Dust\\|E\\(?:nergySplash\\|xplosion\\)\\|GlowSprite\\|M\\(?:etalSparks\\|uzzleFlash\\)\\|S\\(?:moke\\|parks\\)\\)\\)\\|tart\\)\\|Write\\(?:Angles\\|EncodedEnt\\|Float\\(?:Array\\)?\\|Num\\|Vector\\)\\)\\|F2_\\(?:AddCondition\\|DisguisePlayer\\|Get\\(?:Class\\|Player\\(?:C\\(?:\\(?:las\\|onditionFlag\\)s\\)\\|ResourceData\\)\\|ResourceEntity\\)\\|IgnitePlayer\\|Re\\(?:generatePlayer\\|move\\(?:AllWeapons\\|Condition\\|PlayerDisguise\\|WeaponSlot\\)\\|spawnPlayer\\)\\|S\\(?:etPlayer\\(?:Class\\|PowerPlay\\|ResourceData\\)\\|tunPlayer\\)\\)\\|R_\\(?:DidHit\\|Get\\(?:En\\(?:dPosition\\|tityIndex\\)\\|Fraction\\|HitGroup\\|P\\(?:laneNormal\\|ointContents\\(?:Ent\\)?\\)\\)\\|PointOutsideWorld\\|Trace\\(?:Hull\\(?:Ex\\|Filter\\(?:Ex\\)?\\)?\\|Ray\\(?:Ex\\|Filter\\(?:Ex\\)?\\)?\\)\\)\\|angent\\|eleportEntity\\|hrow\\(?:\\(?:Native\\)?Error\\)\\|ri\\(?:ggerTimer\\|mString\\)\\)\\|Un\\(?:hook\\(?:ConVarChange\\|E\\(?:\\(?:ntityOutpu\\|ven\\)t\\)\\|SingleEntityOutput\\|UserMessage\\)\\|setCommandOverride\\)\\|V\\(?:Format\\|erifyCoreVersion\\|oteMenu\\(?:ToAll\\)?\\)\\|Write\\(?:File\\(?:Cell\\|Line\\|String\\)?\\|Pack\\(?:Cell\\|Float\\|String\\)\\)\\|float\\|str\\(?:c\\(?:mp\\|opy\\)\\|len\\|ncmp\\)\\)\\>"
  "An optimized regexp of SourcePawn generated-natives-stocks.")

;; -!- end generated keywords
;; OKAY, now you can edit again!

;; define a regexp for full, hash-prefixed preprocessor expressions
(defvar sourcepawn-mode-font-lock-regexp-preprocessor-full
  (concat "#" sourcepawn-mode-font-lock-regexp-preprocessor "\\(?:[ \t]+" sourcepawn-mode-font-lock-regexp-preprocessor "\\)*")
  "A preprocessor regexp that includes the first \"#\".")

;; set up the syntax highlighting defaults
(defvar sourcepawn-mode-font-lock-defaults
	  `(
		;; preprocessor statements
		(,sourcepawn-mode-font-lock-regexp-preprocessor-full 0 font-lock-preprocessor-face keep)
		;; string color for braced include statements
		("#[iI][nN][cC][lL][uU][dD][eE][ \t]+\\(<.*>\\)" 1 font-lock-string-face t)
		;; variable def color for define statements
		("#[dD][eE][fF][iI][nN][eE][ \t]+\\<\\(\\(?:\\sw\\)+\\)\\>" 1 font-lock-variable-name-face)
		
		(,sourcepawn-mode-font-lock-regexp-keywords 1 font-lock-keyword-face)

		(,sourcepawn-mode-font-lock-regexp-types 1 font-lock-type-face)
		(,sourcepawn-mode-font-lock-regexp-generated-types 1 font-lock-type-face)
		
		(,sourcepawn-mode-font-lock-regexp-constants 1 font-lock-constant-face)
		(,sourcepawn-mode-font-lock-regexp-generated-constants 1 font-lock-constant-face)
		
		(,sourcepawn-mode-font-lock-regexp-generated-natives-stocks 1 font-lock-builtin-face)
		(,sourcepawn-mode-font-lock-regexp-generated-forwards 1 font-lock-function-name-face)
		
		;; variable declarations
		(sourcepawn-mode-font-lock-matcher-variable-names 1 font-lock-variable-name-face)
	   )
	  "The default syntax highlighting rules for sourcepawn-mode.")

;; define our mode
(define-derived-mode sourcepawn-mode fundamental-mode
  "SourcePawn"
  "Major mode to edit SourcePawn source files."
  :syntax-table sourcepawn-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults)
	   '(sourcepawn-mode-font-lock-defaults nil))

  ;; indentation
  (set (make-local-variable 'indent-line-function) 'sourcepawn-mode-indent-line)
  
  ;; comments
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-start-skip) "/\\*+ *\\|//+ *")
  (set (make-local-variable 'comment-end) ""))

;; register it to auto-load on *.sp files
;; Or, leave it up to the user (as I have)
;(add-to-list 'auto-mode-alist '(".sp\\'" . sourcepawn-mode))

;; tell emacs we provide sourcepawn-mode
(provide 'sourcepawn-mode)

;;; sourcepawn-mode.el ends here
