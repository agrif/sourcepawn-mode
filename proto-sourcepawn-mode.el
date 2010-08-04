;; emacs mode for markdown-formatted text

;; symbol lists copied from
;; http://svn.alliedmods.net/viewvc.cgi/trunk/editor/textpad/sourcepawn.syn?root=sourcemod&view=co

;; DO NOT CHANGE the parts after this comment, before the end -!- comment
;; IT WILL JUST CHANGE BACK
;; change the appropriate sp-reserved-keywords files in the source package
;; -!- start generated keywords
;; -!- end generated keywords
;; OKAY, now you can edit again!

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

;; define a regexp for full, hash-prefixed preprocessor expressions
(defvar sourcepawn-mode-font-lock-regexp-preprocessor-full
  (concat "#" sourcepawn-mode-font-lock-regexp-preprocessor "\\(?:[ \t]+" sourcepawn-mode-font-lock-regexp-preprocessor "\\)*")
  "A preprocessor regexp that includes the first \"#\".")

;; set up the syntax highlighting defaults
(defvar sourcepawn-mode-font-lock-defaults
	  ;; C++ style comments
	  `(("\\(//.*\\)$" 1 font-lock-comment-face t)
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
		(sourcepawn-mode-font-lock-matcher-variable-names 1 font-lock-variable-name-face))
	  "The default syntax highlighting rules for sourcepawn-mode.")

;; set up the syntax table
(defvar sourcepawn-mode-syntax-table
  (let ((st (make-syntax-table)))
	;; make _ a words character, so tokens == words, and word movement commands make sense
	(modify-syntax-entry ?_ "w" st)
	
	;; magick from an old font-lock mailing list for C-style comments
	(modify-syntax-entry ?/ ". 14" st)
	(modify-syntax-entry ?* ". 23" st)
	st)
  "Syntax table for sourcepawn-mode.")

;; define our mode
(define-derived-mode sourcepawn-mode fundamental-mode
  "SourcePawn"
  "Major mode to edit SourcePawn source files."
  :syntax-table sourcepawn-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults)
	   '(sourcepawn-mode-font-lock-defaults nil))

  ;; comments
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-start-skip) "/\\*+ *\\|//+ *")
  (set (make-local-variable 'comment-end) ""))

;; register it to auto-load on *.sp files
(add-to-list 'auto-mode-alist '(".sp\\'" . sourcepawn-mode))

;; tests
;font-lock-defaults
;sourcepawn-mode-font-lock-defaults

;; tell emacs we provide sourcepawn-mode
(provide 'sourcepawn-mode)
