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

(defun bonk/disable-eglot-logging-for-lag ()
"It must be triggered after eglot is running, not before"
(interactive)
	(fset #'jsonrpc--log-event #'ignore)
  )

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 70) '(100 . 100)))))
(global-set-key (kbd "C-c y") 'toggle-transparency)

(defun bonk-keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defun bonk-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun bonk-compile-file (f)
(setq f (flatten-list (list f)))
(message "Compiling file(s): %s" f)
(if (featurep 'native-compile)
	(dolist (source f)
	  (let ((cache (bonk-compile-locate-eln-file (file-name-base source))))
		(if (and (or (null cache)
					 (file-newer-than-file-p cache source))
				 (file-exists-p osurce))
			(native-compyle-async f)
		  (message "Skipping compilation of file %s" source))))
  (dolist (source f)
	(when (file-exists-p source)
	  (if (file-directory-p source)
		  (byte-recompile-directory source 0)
		(byte-recompile-file source nil 0))))))

;; A function to compile the buffer's file
(defun bonk-compile-buffer (&optional b)
  "Compiles (native or byte-code) the file of buffer B."
  (when (and b ;; Let's be sure it is not nil
             (not (bufferp b)))
    (cl-return nil))
  (let ((file (buffer-file-name b)))
    (when file
      (bonk-compile-file file))))

(defmacro file! ()
  (cond ((bound-and-true-p byte-compile-current-file))
		(load-file-name)
		((stringp (car-safe current-load-list))
		 (car current-load-list))
		(buffer-file-name)
		((error "Cannot get this file-path"))))

(defmacro dir! ()
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

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol (or list of them) referring to Emacs features (aka
packages). PACKAGE may use :or/:any and :and/:all operators. The precise format
is:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
  If :or/:any/:and/:all are omitted, :and/:all are implied.

This emulates `eval-after-load' with a few key differences:

1. No-ops for package that are disabled by the user (via `package!') or not
   installed yet.
2. Supports compound package statements (see :or/:any and :and/:all above).

Since the contents of these blocks will never by byte-compiled, avoid putting
things you want byte-compiled in them! Like function/macro definitions."
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (unless (memq package (bound-and-true-p bonk-disabled-packages))
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              `(with-eval-after-load ',package ,@body)))
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)) (car body))
               (setq body `((after! ,next ,@body)))))
            (`(after! (:and ,@package) ,@body))))))

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro defbackport! (type symbol &rest body)
  "Backport a function/macro/alias from later versions of Emacs."
  (declare (indent defun) (doc-string 4))
  (unless (fboundp (bonk-unquote symbol))
    `(,type ,symbol ,@body)))

;; Introduced in Emacs 28.1
(defbackport! defun ensure-list (object)
  "Return OBJECT as a list.
If OBJECT is already a list, return OBJECT itself.  If it's
not a list, return a one-element list containing OBJECT."
  (declare (pure t) (side-effect-free t))
  (if (listp object) object (list object)))

;; Introduced in Emacs 28.1
(defbackport! defun always (&rest _args)
  "Do nothing and return t.
This function accepts any number of ARGUMENTS, but ignores them.
Also see `ignore'."
  t)

;; Introduced in Emacs 28.1
(defbackport! defun file-name-concat (directory &rest components)
  "Append COMPONENTS to DIRECTORY and return the resulting string.

Elements in COMPONENTS must be a string or nil.
DIRECTORY or the non-final elements in COMPONENTS may or may not end
with a slash -- if they don't end with a slash, a slash will be
inserted before contatenating."
  (mapconcat
   #'identity
   (cl-loop for str in (cons directory components)
            if (and str (/= 0 (length str))
                    (if (string-suffix-p "/" str)
                        (substring str 0 -1)
                      str))
            collect it)
   "/"))

;; Introduced in Emacs 28.1
(defbackport! defmacro with-environment-variables (variables &rest body)
  "Set VARIABLES in the environment and execute BODY.
VARIABLES is a list of variable settings of the form (VAR VALUE),
where VAR is the name of the variable (a string) and VALUE
is its value (also a string).

The previous values will be be restored upon exit."
  (declare (indent 1) (debug (sexp body)))
  (unless (consp variables)
    (error "Invalid VARIABLES: %s" variables))
  `(let ((process-environment (copy-sequence process-environment)))
     ,@(cl-loop for var in variables
                collect `(setenv ,(car var) ,(cadr var)))
     ,@body))

;; Introduced in Emacs 28.1
(defbackport! defun file-name-with-extension (filename extension)
  "Return FILENAME modified to have the specified EXTENSION.
The extension (in a file name) is the part that begins with the last \".\".
This function removes any existing extension from FILENAME, and then
appends EXTENSION to it.

EXTENSION may include the leading dot; if it doesn't, this function
will provide it.

It is an error if FILENAME or EXTENSION is empty, or if FILENAME
is in the form of a directory name according to `directory-name-p'.

See also `file-name-sans-extension'."
  (let ((extn (string-trim-left extension "[.]")))
    (cond ((string-empty-p filename)
           (error "Empty filename"))
          ((string-empty-p extn)
           (error "Malformed extension: %s" extension))
          ((directory-name-p filename)
           (error "Filename is a directory: %s" filename))
          ((concat (file-name-sans-extension filename) "." extn)))))

;; Introduced in Emacs 29+
(defbackport! defmacro with-memoization (place &rest code)
  "Return the value of CODE and stash it in PLACE.
If PLACE's value is non-nil, then don't bother evaluating CODE
and return the value found in PLACE instead."
  (declare (indent 1) (debug (gv-place body)))
  (gv-letplace (getter setter) place
    `(or ,getter
         ,(macroexp-let2 nil val (macroexp-progn code)
            `(progn
               ,(funcall setter val)
               ,val)))))

;; Introduced in Emacs 29+ (emacs-mirror/emacs@f117b5df4dc6)
(defbackport! defalias 'bol #'line-beginning-position)
(defbackport! defalias 'eol #'line-end-position)

(defmacro letf! (bindings &rest body)
  "Temporarily rebind function, macros, and advice in BODY.

Intended as syntax sugar for `cl-letf', `cl-labels', `cl-macrolet', and
temporary advice.

BINDINGS is either:

  A list of, or a single, `defun', `defun*', `defmacro', or `defadvice' forms.
  A list of (PLACE VALUE) bindings as `cl-letf*' would accept.

TYPE is one of:

  `defun' (uses `cl-letf')
  `defun*' (uses `cl-labels'; allows recursive references),
  `defmacro' (uses `cl-macrolet')
  `defadvice' (uses `defadvice!' before BODY, then `undefadvice!' after)

NAME, ARGLIST, and BODY are the same as `defun', `defun*', `defmacro', and
`defadvice!', respectively.

\(fn ((TYPE NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defun* defmacro defadvice))
	(setq bindings (list bindings)))
  (dolist (binding (reverse bindings) body)
	(let ((type (car binding))
		  (rest (cdr binding)))
	  (setq
	   body (pcase type
			  (`defmacro `(cl-macrolet ((,@rest)) ,body))
			  (`defadvice `(progn (defadvice! ,@rest)
								  (unwind-protect ,body (undefadvice! ,@rest))))
			  ((or `defun `defun*)
			   `(cl-letf ((,(car rest) (symbol-function #',(car rest))))
				  (ignore ,(car rest))
				  ,(if (eq type 'defun*)
					   `(cl-labels ((,@rest)) ,body)
					 `(cl-letf (((symbol-function #',(car rest))
								 (lambda! ,(cadr rest) ,@(cddr rest))))
						,body))))
			  (_
			   (when (eq (car-safe type) 'function)
				 (setq type (list 'symbol-function type)))
			   (list 'cl-letf (list (cons type rest)) body)))))))

(defmacro quiet! (&rest forms)
  "Run FORMS without generating any output.

This silences calls to `message', `load', `write-region' and anything that
writes to `standard-output'. In interactive sessions this inhibits output to the
echo-area, but not to *Messages*."
  `(if init-file-debug
	   (progn ,@forms)
	 ,(if noninteractive
		  `(letf! ((standard-output (lambda (&rest _)))
				   (defun message (&rest _))
				   (defun load (file &optional noerror nomessage nosuffix must-suffix)
					 (funcall load file noerror t nosuffix must-suffix))
				   (defun write-region (start end filename &optional append visit lockname mustbenew)
					 (unless visit (setq visit 'no-message))
					 (funcall write-region start end filename append visit lockname mustbenew)))
			 ,@forms)
		`(let ((inhibit-message t)
			   (save-silently t))
		   (prog1 ,@forms (message ""))))))

(defmacro cmd! (&rest body)
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))

;;; Loading
(defmacro add-load-path! (&rest dirs)
  "Add DIRS to `load-path', relative to the current file.
The current file is the file from which `add-to-load-path!' is used."
  `(let ((default-directory (dir!))
         file-name-handler-alist)
     (dolist (dir (list ,@dirs))
       (cl-pushnew (expand-file-name dir) load-path :test #'string=))))

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol (or list of them) referring to Emacs features (aka
packages). PACKAGE may use :or/:any and :and/:all operators. The precise format
is:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
  If :or/:any/:and/:all are omitted, :and/:all are implied.

This emulates `eval-after-load' with a few key differences:

1. No-ops for package that are disabled by the user (via `package!') or not
   installed yet.
2. Supports compound package statements (see :or/:any and :and/:all above).

Since the contents of these blocks will never by byte-compiled, avoid putting
things you want byte-compiled in them! Like function/macro definitions."
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (unless (memq package (bound-and-true-p bonk-disabled-packages))
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              `(with-eval-after-load ',package ,@body)))
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)) (car body))
               (setq body `((after! ,next ,@body)))))
            (`(after! (:and ,@package) ,@body))))))

(defmacro load! (filename &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILENAME is either a file path string or a form that should evaluate to such a
string at run time. PATH is where to look for the file (a string representing a
directory path). If omitted, the lookup is relative to either `load-file-name',
`byte-compile-current-file' or `buffer-file-name' (checked in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  `(bonk-load
	(file-name-concat ,(or path `(dir!)) ,filename)
	,noerror))

(defmacro defer-until! (condition &rest body)
  "Run BODY when CONDITION is true (checks on `after-load-functions'). Meant to
serve as a predicated alternative to `after!'."
  (declare (indent defun) (debug t))
  `(if ,condition
	   (progn ,@body)
	 ,(let ((fn (intern (format "bonk--delay-form-%s-h" (sxhash (cons condition body))))))
		`(progn
		   (fset ',fn (lambda (&rest args)
						(when ,(or condition t)
						  (remove-hook 'after-load-functions #',fn)
						  (unintern ',fn nil)
						  (ignore args)
						  ,@body)))
		   (put ',fn 'permanent-local-hook t)
		   (add-hook 'after-load-functions #',fn)))))

(defmacro defer-feature! (feature &rest fns)
  "Pretend FEATURE hasn't been loaded yet, until FEATURE-hook or FNS run.

Some packages (like `elisp-mode' and `lisp-mode') are loaded immediately at
startup, which will prematurely trigger `after!' (and `with-eval-after-load')
blocks. To get around this we make Emacs believe FEATURE hasn't been loaded yet,
then wait until FEATURE-hook (or any of FNS, if FNS are provided) is triggered
to reverse this and trigger `after!' blocks at a more reasonable time."
  (let ((advice-fn (intern (format "bonk--defer-feature-%s-a" feature)))
		(fns (or fns (list feature))))
	`(progn
	   (delq! ',feature features)
	   (defadvice! ,advice-fn (&rest _)
		 :before ',fns
		 ;; Some plugins (like yasnippet) will invoke a fn early to parse
		 ;; code, which would prematurely trigger this. In those cases, well
		 ;; behaved plugins will use `delay-mode-hooks', which we can check for:
		 (unless delay-mode-hooks
		   ;; ...Otherwise, announce to the world this package has been loaded,
		   ;; so `after!' handlers can react.
		   (provide ',feature)
		   (dolist (fn ',fns)
			 (advice-remove fn #',advice-fn)))))))

;;; Hooks
(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append? (if (eq (car forms) :after) (pop forms)))
		(fn (gensym "bonk-transient-hook")))
	`(let ((sym ,hook-or-function))
	   (defun ,fn (&rest _)
		 ,(format "Transient hook for %S" (bonk-unquote hook-or-function))
		 ,@forms
		 (let ((sym ,hook-or-function))
		   (cond ((functionp sym) (advice-remove sym #',fn))
				 ((symbolp sym)   (remove-hook sym #',fn))))
		 (unintern ',fn nil))
	   (cond ((functionp sym)
			  (advice-add ,hook-or-function ,(if append? :after :before) #',fn))
			 ((symbolp sym)
			  (put ',fn 'permanent-local-hook t)
			  (add-hook sym #',fn ,append?))))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
	 unquoted list of modes, a quoted hook variable or a quoted list of hook
	 variables.
  2. Optional properties :local, :append, and/or :depth [N], which will make the
	 hook buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be a quoted function, a quoted list
	 thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
	 implicitly be wrapped in a lambda).

\(fn HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)"
  (declare (indent (lambda (indent-point state)
					 (goto-char indent-point)
					 (when (looking-at-p "\\s-*(")
					   (lisp-indent-defform state indent-point))))
		   (debug t))
  (let* ((hook-forms (bonk--resolve-hook-forms hooks))
		 (func-forms ())
		 (defn-forms ())
		 append-p local-p remove-p depth)
	(while (keywordp (car rest))
	  (pcase (pop rest)
		(:append (setq append-p t))
		(:depth  (setq depth (pop rest)))
		(:local  (setq local-p t))
		(:remove (setq remove-p t))))
	(while rest
	  (let* ((next (pop rest))
			 (first (car-safe next)))
		(push (cond ((memq first '(function nil))
					 next)
					((eq first 'quote)
					 (let ((quoted (cadr next)))
					   (if (atom quoted)
						   next
						 (when (cdr quoted)
						   (setq rest (cons (list first (cdr quoted)) rest)))
						 (list first (car quoted)))))
					((memq first '(defun cl-defun))
					 (push next defn-forms)
					 (list 'function (cadr next)))
					((prog1 `(lambda (&rest _) ,@(cons next rest))
					   (setq rest nil))))
			  func-forms)))
	`(progn
	   ,@defn-forms
	   (dolist (hook (nreverse ',hook-forms))
		 (dolist (func (list ,@func-forms))
		   ,(if remove-p
				`(remove-hook hook func ,local-p)
			  `(add-hook hook func ,(or depth append-p) ,local-p)))))))

(defmacro remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.

Takes the same arguments as `add-hook!'.

If N and M = 1, there's no benefit to using this macro over `remove-hook'.

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (bonk--setq-hook-fns hooks var-vals)
			collect `(defun ,fn (&rest _)
					   ,(format "%s = %s" var (pp-to-string val))
					   (setq-local ,var ,val))
			collect `(add-hook ',hook #',fn -90))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
			in (bonk--setq-hook-fns hooks vars 'singles)
			collect `(remove-hook ',hook #',fn))))

(defun set-docsets! (modes &rest docsets)
  "Registers a list of DOCSETS for MODES.

MODES can be one major mode, or a list thereof.

DOCSETS can be strings, each representing a dash docset, or a vector with the
structure [DOCSET FORM]. If FORM evaluates to nil, the DOCSET is omitted. If it
is non-nil, (format DOCSET FORM) is used as the docset.

The first element in DOCSETS can be :add or :remove, making it easy for users to
add to or remove default docsets from modes.

DOCSETS can also contain sublists, which will be flattened.

Example:

  (set-docsets! '(js2-mode rjsx-mode) \"JavaScript\"
    [\"React\" (eq major-mode 'rjsx-mode)]
    [\"TypeScript\" (bound-and-true-p tide-mode)])

Used by `+lookup/in-docsets' and `+lookup/documentation'."
  (declare (indent defun))
  (let ((action (if (keywordp (car docsets)) (pop docsets))))
    (dolist (mode (ensure-list modes))
      (let ((hook (intern (format "%s-hook" mode)))
            (fn (intern (format "+lookup-init--%s-%s" (or action "set") mode))))
        (if (null docsets)
            (remove-hook hook fn)
          (fset
           fn (lambda ()
                (make-local-variable 'dash-docs-docsets)
                (unless (memq action '(:add :remove))
                  (setq dash-docs-docset nil))
                (dolist (spec docsets)
                  (cl-destructuring-bind (docset . pred)
                      (cl-typecase spec
                        (string (cons spec nil))
                        (vector (cons (aref spec 0) (aref spec 1)))
                        (otherwise (signal 'wrong-type-arguments (list spec '(vector string)))))
                    (when (or (null pred)
                              (eval pred t))
                      (if (eq action :remove)
                          (setq dash-docs-docsets (delete docset dash-docs-docsets))
                        (cl-pushnew docset dash-docs-docsets)))))))
          (add-hook hook fn 'append))))))

(defalias 'kbd! #'general-simulate-key)
