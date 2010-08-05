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
	(if (or (bobp) (looking-at-p "[ \t]*{"))
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

;; our indentation function
;; TODO: lines after braceless if (...) and proper point handling on indent
(defun sourcepawn-mode-indent-line ()
  "Indent the current line as SourcePawn code."
  (interactive)
  (beginning-of-line)
  ;; set ret to 'noindent to signal indentation cannot be done
  ;; endbrace-count stores how many "}" we see right at the beginning of the line
  (let (ret (endbrace-count 0))
	(indent-line-to
	 ;; make sure we don't indent to a negative
	 (max 0
		  (if (bobp)
			  0 ;; first line
			;; not first line, what should we indent to?
			(let ((special-indent (sourcepawn-mode-single-line-block-p)))
			  (if (not (null special-indent))
				  (+ default-tab-width special-indent) ;; indent once, we're special
				;; we're not special :(
				;; check our relative matching-parens ()[]{} depth in the last line
				;; and indent in or out that much relative to last line's indentation
				(save-excursion
				  ;; count how many "}" there are on the line we will indent
				  ;; DECREMENT because we want these to act like the end of the last line
				  (while (and (looking-at-p "[ \t]*}") (re-search-forward "[ \t]*}" (line-end-position) t))
					(setq endbrace-count (- endbrace-count 1)))
				  ;; first find last non-blank line, non-special line
				  (forward-line -1)
				  (while (and (not (bobp)) (or (looking-at-p "[ \t]*$") (sourcepawn-mode-single-line-block-p)))
					(forward-line -1))
				  ;; count how many "}" there are at the beginning of the line (which is currently the last line)
				  ;; INCREMENT because these are working against what parse-partial-sexp finds
				  (while (and (looking-at-p "[ \t]*}") (re-search-forward "[ \t]*}" (line-end-position) t))
					(setq endbrace-count (+ endbrace-count 1)))
				  ;; add in the indentation for this S-EXP level
				  (+ (current-indentation)
					 (* default-tab-width
						(+ (car (parse-partial-sexp (line-beginning-position) (line-end-position)))
						   endbrace-count)))))))))
	ret))

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
(add-to-list 'auto-mode-alist '(".sp\\'" . sourcepawn-mode))

;; tests
;font-lock-defaults
;sourcepawn-mode-font-lock-defaults

;; tell emacs we provide sourcepawn-mode
(provide 'sourcepawn-mode)
