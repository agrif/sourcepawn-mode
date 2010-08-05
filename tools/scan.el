;; scan sourcepawn include files for forwards, natives, builtins, types, and constants

(setq input-dir "sp-include")
(setq output-dir "keywords/generated")

;; associative list, who's values are lists of (first . list) regexp pairs
;; (or the identical (first &rest-rules)
;; the first is matched, then list must match immediately after any number of times
;; the advantage here is first-only rules are written (first)
;; in both matches, the first group is appended to the gathering list
(setq match-rules
	  '(("constants"
		 ("#define[ \t]+\\([^_]\\(?:\\sw\\|_\\)+\\)")
		 ("\\<enum\\>[ \t\n]+\\(?:\\sw\\|_\\)+[ \t\n]*{" . "[ \t\n]*\\(\\(?:\\sw\\|_\\)+\\)[^,}]*[,}]"))
		("types"
		 ("\\<struct\\>[ \t\n]+\\(\\(?:\\sw\\|_\\)+\\)")
		 ("\\<enum\\>[ \t\n]+\\(\\(?:\\sw\\|_\\)+\\)"))
		("forwards" ("\\<forward\\>[ \t\n]+\\(?:\\(?:\\sw\\|_\\)+:\\)?\\(\\(?:\\sw\\|_\\)+\\)[ \t\n]*("))
		("natives-stocks"
		 ("\\<native\\>[ \t\n]+\\(?:\\(?:\\sw\\|_\\)+:\\)?\\(\\(?:\\sw\\|_\\)+\\)[ \t\n]*(")
		 ("\\<stock\\>[ \t\n]+\\(?:\\(?:\\sw\\|_\\)+:\\)?\\(\\(?:\\sw\\|_\\)+\\)[ \t\n]*("))))

;; comment-matching regexp (to remove them, prevent false matches)
(setq c-style-comment "/\\*\\(?:.\\|\n\\)*?\\*/")
(setq cpp-style-comment "//.*?$")

;; don't pollute my source with *~ files
(setq backup-inhibited t)

(defun filter (condp list)
  "A filter function, such as in Python."
  (delq nil (mapcar (lambda (el) (and (funcall condp el) el)) list)))

(defun string-in-list (str list)
  "Returns t if the given string is in the given list."
  (eval `(or ,@(mapcar (lambda (other) (string-equal str other)) list))))

;; test
;(string-in-list "Test" '("not" "Test"))

(defun apply-rule-to-buffer (rule)
  "Apply a single rule pair to the buffer, and return a list of matches."
  (let ((ret) (limit (point-max)) (primary (car rule)) (secondary (cdr rule)))
	(while (re-search-forward primary limit t)
	  (let ((match (match-string 1)))
		(if (not (or (null match) (string-in-list match ret)))
			(setq ret (cons match ret)))
		(if (null secondary)
			nil
		  ;; we have a list rule as well
		  (while (and (looking-at secondary) (re-search-forward secondary limit t))
			(let ((submatch (match-string 1)))
			  (if (or (null submatch) (string-in-list submatch ret))
				  nil
				(setq ret (cons submatch ret))))))))
	(nreverse ret)))

(defun apply-rules-to-file (rules filename)
  "Apply an associative list of rules to the given file, and return an associative list of matches."
  (message (format "Scanning %s..." filename))
  (if (not (file-readable-p filename))
	  nil
	(let ((ret))
	  (with-temp-buffer
		(insert-file-contents filename)
		;; get rid of C-style comments to prevent false matches
		(goto-char (point-min))
		(while (re-search-forward c-style-comment (point-max) t)
		  (replace-match "\n"))
		;; get rid of C++-style comments to prevent false matches
		(goto-char (point-min))
		(while (re-search-forward cpp-style-comment (point-max) t)
		  (replace-match "\n"))
		(mapcar (lambda (rulelist)
				  (let ((matches `(,(car rulelist))) (rules (cdr rulelist)))
					;; add stuff to matches
					(dolist (rule rules matches)
					  (goto-char (point-min))
					  (nconc matches (apply-rule-to-buffer rule)))))
				rules)))))

(defun apply-rules-to-files (rules filenames)
  "Do apply-rules-to-file to MANY files, and return an assoc. list."
  (mapcar (lambda (filename) `(,(file-name-nondirectory filename) . ,(apply-rules-to-file rules filename))) filenames))

(let
	((matches
	  (apply-rules-to-files match-rules
							(filter 'file-regular-p
									(mapcar (lambda (file) (concat input-dir "/" file)) (directory-files input-dir))))))
  (dolist (rule match-rules)
	(let ((name (car rule)))
	  (with-temp-buffer
		(dolist (match matches)
		  (let ((file (car match)) (matches-for-name (cdr (assoc name (cdr match)))))
			(if (not (null matches-for-name))
				(insert (format "\n# %s\n" file)))
			(dolist (word matches-for-name)
			  (insert (concat word "\n")))))
		(write-file (concat output-dir "/generated-" name ".txt"))))))