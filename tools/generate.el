;; read in a list of files, and print out optimized regexps
;; that match any word from that file

(setq input-file "proto-sourcepawn-mode.el")
(setq output-file "sourcepawn-mode.el")
(setq start-section ";; -!- start generated keywords")
(setq end-section   ";; -!- end generated keywords")

(setq file-list '("keywords/keywords.txt" "keywords/constants.txt" "keywords/types.txt" "keywords/preprocessor.txt" "keywords/generated/generated-constants.txt" "keywords/generated/generated-types.txt" "keywords/generated/generated-forwards.txt" "keywords/generated/generated-natives-stocks.txt"))

;; don't pollute my source with *~ files
(setq backup-inhibited t)

(defun list-from-file (filename)
  "Read the contents of a file into a list, one item from each line."
  (if (file-readable-p filename)
	  (with-temp-buffer
		(let ((retlist))
		  (insert-file-contents filename)
		  (goto-char (point-min))
		  (while (not (eobp))
			(let ((line (buffer-substring (line-beginning-position) (line-end-position))))
			  (if (not (or
						(string-equal "" line)
						(string-equal ";" (substring line 0 1))
						(string-equal "#" (substring line 0 1))))
				  (setq retlist
						(cons line retlist))))
			(forward-line))
		  (nreverse retlist)))
	'nil))

;; test
;(list-from-file "keywords.txt")

(defun files-to-regexp (filenames)
  "Read the contents of a list of given files, and create optimized regexps."
  (let (ret)
	(dolist (filename filenames)
	  (setq ret (cons
				 `(,(file-name-sans-extension (file-name-nondirectory filename)) . ,(regexp-opt (list-from-file filename) 'words))
				 ret)))
	(nreverse ret)))

;; test
;(files-to-regexp file-list)

(defun files-to-lisp-code (filenames)
  "Read the contents of a list of given files, optimize some regexps, then return a string of lisp code."
  (eval `(concat
		  ,@(mapcar (lambda (regexp)
					  (let ((docstring (format "An optimized regexp of SourcePawn %s." (car regexp))))
						(format "\n(defvar sourcepawn-mode-font-lock-regexp-%s\n  %S\n  %S)\n" (car regexp) (cdr regexp) docstring)))
					(files-to-regexp file-list)))))

(defun replace-block-in-file (filename-in filename delim-begin delim-end contents)
  "Replace the block between lines delim-begin and delim-end in filename with contents."
  (with-temp-buffer
	(insert-file-contents filename-in)
	(goto-char (point-min))
	(while (not (or (eobp) (string-equal delim-begin (buffer-substring (line-beginning-position) (line-end-position)))))
	  (forward-line))
	(if (eobp)
		t
	  (forward-line)
	  (insert (concat contents "\n"))
	  (while (not (or (eobp) (string-equal delim-end (buffer-substring (line-beginning-position) (line-end-position)))))
		(delete-region (line-beginning-position) (save-excursion (forward-line) (line-beginning-position))))
	  (if (eobp)
		  t
		(write-file filename)))))

(if (replace-block-in-file input-file output-file start-section end-section (files-to-lisp-code file-list))
	(message "An error occurred, and the file was left unmodified for safety."))