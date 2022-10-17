(provide 'main-lib)
(require 'package)
(require 'cl-lib)

(defun bonk/load-envvars-file (file &optional noerror)
  (if (null (file-exists-p file))
	  (unless noerror
		(signal 'file-error (list "No envvar file exists" file)))
	(with-temp-buffer
	  (insert-file-contents file)
	  (when-let (env (read (current-buffer)))
		(let ((tz (getenv-internal "TZ")))
		  (setq-default
		   process-environment
		   (append env (default-value 'process-environment))
		   exec-path
		   (append (split-string (getenv "PATH") path-separator t)
				   (list exec-directory))
		   shell-file-name
		   (or (getenv "SHELL")
			   (default-value 'shell-file-name)))
		  (when-let (newtz (getenv-internal "TZ"))
			(unless (equal tz newtz)
			  (set-time-zone-rule newtz))))
		env))))

(defun bonk/infer-indent-style ()
  ;; Honestly, This is more of a wild guess since we could be using tabs and having it wrongly
  ;; configure on our ide
  (let ((space-count (how-many "^ "))
		(tab-count (how-many "^\t")))
	(if (> space-count tab-count )
		(setq indent-tabs-mode nil))
	(if (> tab-count space-count)
		(setq indent-tabs-mode t))))

(defun bonk/set-font (font-name)
	(interactive "sSet font name: ")
	(set-face-attribute 'default nil
						:font font-name
						:weight 'normal))

  (defun bonk/set-font-bar ()
	(interactive)
(let ((faces '(mode-line
			   mode-line-buffer-id
			   mode-line-emphasis
			   mode-line-highlight
			   mode-line-inactive)))
	 (mapc
	  (lambda (face) (set-face-attribute face nil :font "Hack-15"))
	  faces)))
(defun bonk/set-font-italic (font-name)
	  (interactive "sSet font name: ")
	  (set-face-attribute 'default nil
						  :font font-name
						  :weight 'normal
						  :slant 'italic))
	(defun bonk/set-font-bold (font-name)
	  (interactive "sSet font name: ")
	  (set-face-attribute 'default nil
						  :font font-name
						  :weight 'bold))
	(defun bonk/set-font-bold-italic (font-name)
	  (interactive "sSet font name: ")
	  (set-face-attribute 'default nil
						  :font font-name
						  :weight 'bold
						  :slant 'italic))

(defun bonk/set-latex-scale (scale-number)
  (interactive "nSelect the scale in which latex previews are going to be displayed: ")
  (setq org-format-latex-options (plist-put org-format-latex-options :scale scale-number))
  )

(defun file! ()
  (cond ((bound-and-true-p byte-compile-current-file))
		(load-file-name)
		((stringp (car-safe current-load-list))
		 (car current-load-list))
		(buffer-file-name)
		((error "Cannot get this file-path"))))

(defun dir! ()
  (when-let (path (file!))
	(directory-file-name (file-name-directory path))))

(defmacro eval-if! (cond then &rest body)
  (declare (indent 2))
  (if (eval cond)
	  then
	(macroexp-progn body)))

(defmacro eval-when! (cond &rest body)
  (declare (indent 1))
  (when (eval cond)
	(macroexp-progn body)))

;;; Closure factories
(defmacro fn! (arglist &rest body)
  (declare (indent defun) (doc-string 1) (pure t) (side-effect-free t))
  `(cl-function
	(lambda
	  ,(letf! (defun* allow-other-keys (args)
				(mapcar
				 (lambda (arg)
				   (cond ((nlistp (cdr-safe arg)) arg)
						 ((listp arg) (allow-other-keys arg))
						 (arg)))
				 (if (and (memq '&key args)
						  (not (memq '&allow-other-keys args)))
					 (if (memq '&aux args)
						 (let (newargs arg)
						   (while args
							 (setq arg (pop args))
							 (when (eq arg '&aux)
							   (push '&allow-other-keys newargs))
							 (push arg newargs))
						   (nreverse newargs))
					   (append args (list '&allow-other-keys)))
				   args)))
		 (allow-other-keys arglist))
	  ,@body)))

(defmacro cmd! (&rest body)
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))

(defalias 'kbd! #'general-simulate-key)
