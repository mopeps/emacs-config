(provide 'bonk-ide)

(setup (:pkg exec-path-from-shell :straight t)
  :ensure t
  (exec-path-from-shell-initialize))

(setup (:pkg evil-nerd-commenter :straight t)
  (:global "M-/" evilnc-comment-or-uncomment-lines))

(setup (:pkg origami :straight t)
  (:disabled)
  (:hook-into c-mode
			  emacs-lisp-mode
			  go-mode
			  yaml-mode))

(defun bonk/infer-indent-style ()
  ;; Honestly, This is more of a wild guess since we could be using tabs and having it wrongly
  ;; configure on our ide
  (let ((space-count (how-many "^ "))
		(tab-count (how-many "^\t")))
	(if (> space-count tab-count )
		(setq indent-tabs-mode t))
	(if (> tab-count space-count)
		(setq indent-tabs-mode t))))

(defun bonk/prog-mode-settings ()
  (setq-default tab-width 4) ; I want tabs to be four spaces wide
  (setq standard-indent 4) ; I want indent to be four spaces wide
  (show-paren-mode t)
  (display-line-numbers-mode)
  (setq whitespace-style '(face tab-mark trailing))
  (custom-set-faces
	'(whitespace-tab ((t (:foreground "#636363")))))
  (setq whitespace-display-mappings '((tab-mark 9 [9474 9] [92 9])))
  (setq-local show-trailing-whitespace t)
  (bonk/infer-indent-style)
  (whitespace-mode))

(add-hook 'prog-mode-hook 'bonk/prog-mode-settings)
;; Indentation levels for each lang
(defvaralias 'js2-basic-offset 'tabwidth)
(defvaralias 'js-indent-level 'tab-width)
(defvaralias 'js-indent-level 'tab-width)
(defvaralias 'rjsx-indent-level 'tab-width)
(defvaralias 'rjsx-mode-markup-indent-offset 'tab-width)
(defvaralias 'rjsx-mode-code-indent-offset 'tab-width)
(defvaralias 'web-mode-css-indent-offset 'tab-width)
(defvaralias 'js2-indent-level 'tab-width)

;; Increase for better lsp-mode performance; see
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(when (boundp 'read-process-output-max)
  ;; New in Emacs 27
  (setq read-process-output-max (* 2048 2048))
  (load-file "~/magit.el"))

(setup (:pkg company :straight t)
	   (:hook-into lsp-mode)
	   (:bind
		"<tab>" company-complete-selection
		"<tab>" company-indent-or-complete-common)
	   (setq company-lsp-cache-candidates 'auto)
	   (setq company-minimum-prefix-length 1)
	   (setq company-idle-delay 0.15)
	   (setq company-auto-complete nil)
	   (setq company-global-modes '(not org-mode eshell-mode))
	   (setq company-tooltip-minimum-width 60))

(global-company-mode t)
(setup (:pkg all-the-icons-completion :straight t))

;; (setup (:pkg company-box :straight t)
;; 	   (:hook-into company-mode)
;; 	   (require 'all-the-icons)
;; 	   (setq company-box-icons-alist 'company-box-icons-all-the-icons
;; 			 ;; These are the Doom Emacs defaults
;; 			 ))

(setup (:pkg company-inf-ruby :straight t)
(:hook-into company)
(add-to-list 'company-backends 'company-inf-ruby))

(defun bonk/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(setup (:pkg lsp-mode :straight t)
	   (:hook bonk/lsp-mode-setup )
	   (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
	   (:option lsp-enable-which-key-integration t)
	   (setq lsp-completion-enable t)
	   (:when-loaded
		(progn
		  (setq lsp-language-id-configuration '((java-mode . "java")
												(kotlin-mode . "kotlin")
												(python-mode . "python")
												(gfm-view-mode . "markdown")
												(css-mode . "css")
												(xml-mode . "xml")
												(ruby-mode . "ruby")
												(c-mode . "c")
												(dart-mode . "dart")
												(c++-mode . "cpp")
												(rustic-mode . "rust")
												(objc-mode . "objective-c")
												(web-mode . "html")
												(html-mode . "html")
												(sgml-mode . "html")
												(mhtml-mode . "html")
												(go-mode . "go")
												(haskell-mode . "haskell")
												(php-mode . "php")
												(json-mode . "json")
												(typescript-mode . "typescript")
												))

		  (setq lsp-diagnostics-provider :none)

		  (:option lsp-file-watch-threshold nil)
		  (:option lsp-solargraph-multi-root nil)
		  ;; enable / disable the hints as you prefer: (setq lsp-auto-guess-root t)
(setq lsp-log-io nil)
(setq lsp-restart 'auto-restart)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-eldoc-hook nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-semantic-tokens-enable nil)
(setq lsp-enable-folding nil)
(setq lsp-enable-imenu nil)
(setq lsp-enable-snippet nil)
(setq read-process-output-max (* 1024 1024)) ;; 1MB
(setq lsp-idle-delay 0.5)
		  )))

(setup (:pkg lsp-ui :straight t)
	   (:hook-into lsp-mode)
	   (:when-loaded
		(progn
		  (setq lsp-ui-sideline-enable t)
		  (setq lsp-ui-sideline-show-hover nil)
		  (setq lsp-ui-doc-enable t)
		  (setq lsp-ui-doc-position 'bottom))))

(setup (:pkg lsp-ivy :straight t)
  (:load-after lsp-mode))

;; 	(setup (:pkg eglot :straight t)
;; 		   :ensure t)
;; ;;  hooks
;; 	(defun bonk-ide--add-eglot-hooks (mode-list)
;; 	  "Iterates over MODE-LIST recursively to add eglot-ensure to
;; 	existing mode hooks.

;; 	The mode must be loaded, ie. found with `fboundp'. A mode which
;; 	is not loaded will not have a hook added, in which case add it
;; 	manually with something like this:

;; 	`(add-hook 'some-mode-hook #'eglot-ensure)'
;; 	"
;; 	  (dolist (mode-def mode-list)
;; 		(let ((mode (if (listp mode-def) (car mode-def) mode-def)))
;; 		  (cond
;; 		   ((listp mode) (bonk-ide--add-eglot-hooks mode))
;; 		   (t
;; 			(when (and (fboundp mode)
;; 					   (not (eq 'clojure-mode mode))  ; prefer cider
;; 					   (not (eq 'lisp-mode mode))     ; prefer sly/slime
;; 					   (not (eq 'scheme-mode mode))   ; prefer geiser
;; 					   )
;; 			  (let ((hook-name (concat (symbol-name mode) "-hook")))
;; 				(message (concat "adding eglot to " hook-name))
;; 				(add-hook (intern hook-name) #'eglot-ensure))))))))

;; ;; add eglot to existing programming modes when eglot is loaded.
;; (with-eval-after-load "eglot"
;; 	(bonk-ide--add-eglot-hooks eglot-server-programs))

;; 	;;; customization
;; 	;; Shutdown server when last managed buffer is killed
;; 	(customize-set-variable 'eglot-autoshutdown t)

(setup (:pkg tree-sitter :straight t)
  (:hook tree-sitter-hl-mode)
  (:hook-into typescript-mode))
(setup (:pkg tree-sitter-langs :straight t))

(setup (:pkg rainbow-mode :straight t)
(:hook-into prog-mode))

(setup (:pkg yasnippet :straight t)                  ; Snippets
  (yas-global-mode 1))

  (with-eval-after-load 'yasnippet
   (setq yas-snippt-dirs '(yasnippet-snippets-dir))
  (setq
   yas-verbosity 1                      ; No need to be so verbose
   yas-wrap-around-region t)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-reload-all))


(setup (:pkg yasnippet-snippets :straight t)         ; Collection of snippets
  (:load-after yasnippet))

(setup (:pkg flycheck :straight t)
  (:hook-into company-mode lsp-mode)
  (setq flycheck-check-syntax-automatically `(idle-change mode-enabled))
  (setq flycheck-idle-change-delay 4)
  (setq flycheck-disabled-checkers
		'(ruby ruby-reek
			   ruby-standard
			   ;; ruby-rubocop
			   ruby-rubylint
			   yaml-ruby)))

(add-hook 'python-mode-hook 'flycheck-mode)

  (with-eval-after-load 'company
	(add-hook 'python-mode-hook 'company-mode))

  (setup (:pkg company-jedi :straight t)
	(:when-loaded
	  (progn
		(add-to-list 'company-backends 'company-jedi))))

  (defun python-mode-company-init ()
	(setq-local company-backends '((company-jedi
									company-etags
									company-dabbrev-code))))
(setup (:pkg python-mode)
  (:hook lsp-deferred)
(:hook tree-sitter-mode))

  (with-eval-after-load 'python-mode
	(lambda () (require 'lsp-pyright)))
(setup (:pkg lsp-pyright :straight t)
  (:when-loaded
	(progn
	  (when (executable-find "python3")
		(setq lsp-pyright-python-executable-cmd "python3")))))
(setup (:pkg pyenv :straight t)
  (:load-after python-mode))

(setup ruby-mode
 (:file-match "\\.rb\\'")
 (:hook lsp-deferred)
 (:hook tree-sitter-mode)
 (setq ruby-indent-level 4)
  (setq ruby-indent-tabs-mode t)
  (setq lsp-lens-enable nil))

;; (setup (:pkg enh-ruby-mode :straight t)
;; 	(:hook-into ruby-mode)
;; 	(setq enh-ruby-indent-tabs-mode t))

(setup (:pkg robe-mode :straight t)
  (:hook-into ruby-mode))
(eval-after-load 'company
  '(push 'company-robe company-backends))

(setup (:pkg rspec-mode :straight t)
  (:hook-into ruby-mode))

(setup (:pkg go-mode :straight t)
  (:file-match "\\.go\\'")
  (:hook tree-sitter-mode)
  (:hook lsp-deferred)
  (add-hook 'go-mode-hook (lambda ()
							(setq tab-width 4)))
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (defun lsp-go-install-save-hooks ()
	(add-hook 'before-save-hook 'lsp-format-buffer t t)
	(add-hook 'before-save-hook 'lsp-organize-imports t t))
  (add-hook 'go-mode-hook 'lsp-go-install-save-hooks))

(setup (:pkg typescript-mode :straight t)
  (:file-match "\\.tsx?\\'")
  (:hook tree-sitter-hl-mode)
  (:hook lsp-deferred)
  (:hook tide-setup)
  (:hook tide-hl-identifier-mode)
  )

(setup (:pkg tide :straight t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (:load-after typescript-mode company-mode flycheck-mode))

(setup (:pkg js2-mode :straight t)
  (:file-match "\\.jsx?\\'")
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil))

(setup (:pkg rjsx-mode :straight t)
  (:file-match "\\.js\\' \\.tsx\\' \\.ts\\'")
  (:hook lsp-deferred)
  (setq indent-tabs-mode t)
  (setq js2-basic-offset 4))

;; yaml-mode doesn't derive from prog-mode, but we can at least enable
;; whitespace-mode and apply cleanup.
(setup (:pkg yaml-mode :straight t)
	   (:file-match "\\.ya?ml\\'")
	   (add-hook 'yaml-mode-hook 'whitespace-mode)
	   (add-hook 'yaml-mode-hook 'subword-mode))

(setup (:pkg lispy :straight t)
  (:hook-into emacs-lisp-mode scheme-mode lisp-mode))

(setup (:pkg lispyville :straight t)
  (:hook-into lispy-mode)
  (:when-loaded
    (lispyville-set-key-theme '(operators c-w additional
                                additional-movement slurp/barf-cp
                                prettify))))

(setup common-lisp-mode
  (:file-match "\\.lisp\\'")
  (:hook lsp-deferred))

(setup (:pkg sly :straight t)
  (:load-after common-lisp-mode)
  :options
   (setq sly-lisp-implementations
		 '((sbcl ("/usr/bin/sbcl")))))

(setup emacs-lisp-mode
  (:hook flycheck-mode))

(setup (:pkg helpful :straight t)
  (:option counsel-describe-function-function #'helpful-callable
           counsel-describe-variable-function #'helpful-variable)
  (:global [remap describe-function] helpful-function
           [remap describe-symbol] helpful-symbol
           [remap describe-variable] helpful-variable
           [remap describe-command] helpful-command
           [remap describe-key] helpful-key))

(bonk/leader-keys
  "e"   '(:ignore t :which-key "eval")
  "eb"  '(eval-buffer :which-key "eval buffer"))

(bonk/leader-keys
  :keymaps '(visual)
  "er" '(eval-region :which-key "eval region"))

;; TODO: This causes issues for some reason.
;; :bind (:map geiser-mode-map
;;        ("TAB" . completion-at-point))

(setup scheme-mode
  (:hook geiser-mode)
  (:hook tree-sitter-mode))
(setup (:pkg geiser :straight t)
  ;; (setq geiser-default-implementation 'gambit)
  ;; (setq geiser-active-implementations '(gambit guile))
  ;; (setq geiser-implementations-alist '(((regexp "\\.scm$") gambit)
  ;;                                      ((regexp "\\.sld") gambit)))
  ;; (setq geiser-repl-default-port 44555) ; For Gambit Scheme
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-repl-default-port 44555) ; For Gambit Scheme
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile))))

(setup (:pkg lsp-java :straight t)
  (:disabled)
  (:hook-into lsp-mode))
(setup java-mode
  (:hook tree-sitter-hl-mode)
  (:hook copilot-mode)
  (:hook lsp-deferred))

(setup (:pkg clojure-mode :straight t)
	  (:hook copilot-mode)
	  (:hook tree-sitter-hl-mode))
(setup (:pkg cider :straight t)
  (:when-loaded
	(progn
	  ;; eldoc in cider mode
	  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
	  (add-hook 'cider-mode-hook '(paredit-mode +1))
	  (with-eval-after-load 'evil
		(defun my-evil-cider-repl-insert ()
		  "Enter insert mode at the prompt, If we 're behind the prompt."
		  (interactive)
		  (if (> cider-repl-input-start-mark (point))
			  (goto-char cider-repl-input-start-mark))
		  (evil-insert-state))
		))))

(setup (:pkg kotlin-mode :straight t)
  (:hook tree-sitter-hl-mode)
  (:hook copilot-mode)
  (:hook lsp-deferred)
  )

(setup (:pkg gradle-mode :straight t))

(setup c-mode
	  (:hook tree-sitter-mode)
  (:hook copilot-mode)
	   (:hook lsp-deferred))

(setup c++-mode
	  (:hook tree-sitter-mode)
  (:hook copilot-mode)
	   (:hook lsp-deferred))

(setup (:pkg flycheck-clang-analyzer :straight t)
  (:hook-into flycheck)
  (:when-loaded
	(progn
	(flycheck-clang-analyzer-setup))))

(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode))

(setup (:pkg company-irony :straight t)
  (:when-loaded
	(progn
  (setq company-backends '((
							company-dabbrev-code
							company-irony))))))

(setup (:pkg irony :straight t)
  (:hook-into c++-mode c-mode)
  (:hook irony-cdb-autosetup-compile-options))

(setup (:pkg zig-mode :straight t)
  (:disabled)
  (:hook lsp-deferred))

(setup (:pkg rustic :straight t)
  (:hook lsp-deferred)
  (:hook copilot-mode)
  (:hook tree-sitter-mode)
  (:with-map rustic-mode-map
	(:bind  "M-j"  lsp-ui-imenu
			"M-?"  lsp-find-references
			"C-c C-c l"  flycheck-list-errors
			"C-c C-c a"  lsp-execute-code-action
			"C-c C-c r"  lsp-rename
			"C-c C-c q"  lsp-workspace-restart
			"C-c C-c Q"  lsp-workspace-shutdown))
  :config
  (setq rustic-rustfmt-config-alist '((edition . "2018")))
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer"))	;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))

(setup (:pkg markdown-mode :straight t)
  (setq markdown-command "marked")
  (:file-match "\\.md\\'")
  (:when-loaded
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face)))))

(setup web-mode
  (:file-match "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'")
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(setup (:pkg impatient-mode :straight t))
(setup (:pkg skewer-mode :straight t))

(setup (:pkg dart-mode :straight t)
	  (:hook tree-sitter-hl-mode)
	  (:hook copilot-mode)
	  )
(use-package lsp-dart
  :straight t
  :ensure t
  :hook (dart-mode . lsp))

(setup (:pkg rainbow-delimiters :straight t)
	 (:hook-into
	  org-mode
	  prog-mode))

(setup (:pkg smartparens :straight t)
  (:hook-into prog-mode))

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

(use-package copilot
	:straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
	:ensure t)
  ;; you can utilize :map :hook and :config to customize copilot
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(setup (:pkg docker :straight t)
	   (:hook tree-sitter-mode)
  (:also-load docker-tramp))

(setup (:pkg docker-tramp :straight t))

(setup (:pkg terraform-mode :straight t)
	   (:file-match "\\.tf\\'")
	   (:hook tree-sitter-mode)
	  (:hook lsp-deferred))

(setup (:pkg company-terraform :straight t))

(setup (:pkg terraform-doc :straight t))

(setup (:pkg csv :straight t))
(setup (:pkg pandoc :straight t))
(setup (:pkg org-preview-html :straight t))

; Set up ESS, i.e. Statistics in Emacs, R, Stata, etc.
  (setup (:pkg ess :straight t)
	  (:hook tree-sitter-mode)
	)
  (setup (:pkg ess-view :straight t))
  (setup (:pkg ess-view-data :straight t))
  (setup (:pkg ess-r-insert-obj :straight t))
(setup (:pkg ess-R-data-view :straight t))
(setup (:pkg ess-smart-underscore :straight t))

;; (setup (:pkg ipython-shell-send :straight t))

;; (setup (:pkg conda :straight t)
;;   :options
;;   (setq conda-anaconda-home (expand-file-name "~/Programs/miniconda3/"))
;;   (setq conda-env-home-directory (expand-file-name "~/Programs/miniconda3/"))
;;   (setq conda-env-subdirectory "envs"))

;; (unless (getenv "CONDA_DEFAULT_ENV")
;;   (conda-env-activate "base"))

(setup (:pkg vterm :straight t)
  (:bind
   "C-<tab>" vterm-send-tab)
	  (:when-loaded
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000)
  ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can
  ;; spawn another if want one.
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-timer-delay 0.01)))

(bonk/leader-keys
  "Vt" '(vterm-other-window :which-key "vterm in new window")
  "Vb" '(vterm :which-key "open new buffer for vterm"))

(defun read-file (file-path)
  (with-temp-buffer
	(insert-file-contents file-path)
	(buffer-string)))

(defun get-current-package-version ()
  (interactive)
  (let ((package-json-file (concat (eshell/pwd) "/package.json")))
	(when (file-exists-p package-json-file)
	  (let* ((package-json-contents (read-file package-json-file))
			 (package-json (ignore-errors (json-parse-string package-json-contents))))
		(when package-json
		  (ignore-errors (gethash "version" package-json)))))))
(defun map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun get-git-status-prompt ()
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
	(seq-uniq (seq-filter 'identity (mapcar 'map-line-to-status-char status-lines)))))

(defun get-prompt-path ()
  (let* ((current-path (eshell/pwd))
		 (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
		 (has-path (not (string-match "^fatal" git-output))))
	(if (not has-path)
		(abbreviate-file-name current-path)
	  (string-remove-prefix (file-name-directory git-output) current-path))))

;; This prompt function mostly replicates my custom zsh prompt setup
;; that is powered by github.com/denysdovhan/spaceship-prompt.
(defun eshell-prompt ()
  (let ((current-branch (magit-get-current-branch))
        (package-version (get-current-package-version)))
    (concat
     "\n"
     (propertize (system-name) 'face `(:foreground "#62aeed"))
     (propertize " at " 'face `(:foreground "black"))
     (propertize (get-prompt-path) 'face `(:foreground "#82cfd3"))
     (when current-branch
       (concat
        (propertize " • " 'face `(:foreground "black"))
        (propertize (concat " " current-branch) 'face `(:foreground "#c475f0"))))
     (when package-version
       (concat
        (propertize " @ " 'face `(:foreground "black"))
        (propertize package-version 'face `(:foreground "#e8a206"))))
     (propertize " • " 'face `(:foreground "black"))
     (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#5a5b7f"))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face `(:foreground "red2"))
       (propertize "\nλ" 'face `(:foreground "#aece4a")))
     (propertize " " 'face `(:inherit (default))))))

(defun bonks/configure-eshell ()
  ;; Make sure magit is loaded
  (require 'magit)

  (require 'evil-collection-eshell)
  (evil-collection-eshell-setup)

  (setup (:pkg xterm-color :straight t))

  (push 'eshell-tramp eshell-modules-list)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
			(lambda ()
			  (setq xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output.
  (add-hook 'eshell-pre-command-hook
			(lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
			(lambda () (setenv "TERM" "dumb")))

  ;; Use completion-at-point to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  ;; Initialize the shell history
  (eshell-hist-initialize)

  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setenv "PAGER" "cat")

  (setq eshell-prompt-function      'eshell-prompt
		eshell-prompt-regexp        "^λ "
		eshell-history-size         10000
		eshell-buffer-maximum-lines 10000
		eshell-hist-ignoredups t
		eshell-highlight-prompt t
		eshell-scroll-to-bottom-on-input t
		eshell-prefer-lisp-functions nil))

(use-package eshell
  :hook (eshell-first-time-mode . bonks/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
	(setq eshell-destroy-buffer-when-process-dies t)
	(setq eshell-visual-commands '("htop" "zsh" "vim" "nvim"))))

(setup (:pkg eshell-toggle :straight t)
  (:global "C-M-'" eshell-toggle)
  (:option eshell-toggle-size-fraction 3
           eshell-toggle-use-projectile-root t
           eshell-toggle-run-command nil))

;; Backup and Autosave Directories
  (setq temporary-file-directory "~/.tmp/emacs/")
  (setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))
  (setq backup-directory-alist            '((".*" . "~/.Trash")))

(setup (:pkg all-the-icons-dired :straight t))
  (setup (:pkg dired-single :straight t))
  (setup (:pkg dired-ranger :straight t))
  (setup (:pkg dired-collapse :straight t)
	)

  (setup dired
	(setq dired-listing-switches "-agho --group-directories-first"
		  dired-omit-files "^\\.[^.].*"
		  dired-omit-verbose nil
		  dired-hide-details-hide-symlink-targets nil
		  delete-by-moving-to-trash t)

	;;(autoload 'dired-omit-mode "dired-x")

	(add-hook 'dired-load-hook
			  (lambda ()
				(interactive)
				(dired-collapse)))

	(add-hook 'dired-mode-hook
			  (lambda ()
				(interactive)
				(dired-hide-details-mode 1)
				(all-the-icons-dired-mode 1)
				(hl-line-mode 1)))

	(evil-collection-define-key 'normal 'dired-mode-map
	  "h" 'dired-single-up-directory
	  "H" 'dired-omit-mode
	  "l" 'dired-single-buffer
	  "y" 'dired-ranger-copy
	  "X" 'dired-ranger-move
	  "p" 'dired-ranger-paste))

  (setup (:pkg dired-rainbow :straight t)
	(:load-after dired
	  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
	  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
	  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
	  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
	  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
	  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
	  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
	  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
	  (dired-rainbow-define log "#c17d11" ("log"))
	  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
	  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
	  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
	  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
	  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
	  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
	  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
	  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
	  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
	  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
	  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))

  (eval-when-compile (require 'cl))
  (defun bonk/dired-link (path)
	(lexical-let ((target path))
	  (lambda () (interactive) (message "Path: %s" target) (dired target))))
  (bonk/leader-keys
	"d"   '(:ignore t :which-key "dired")
	"dd"  '(dired :which-key "Here")
	"dh"  `(,(bonk/dired-link "~/") :which-key "Home")
	"dn"  `(,(bonk/dired-link "~/Notes") :which-key "Notes")
	"dw"  `(,(bonk/dired-link "~/working") :which-key "Working")
	"dg"  `(,(bonk/dired-link "~/github") :which-key "Github")
	"do"  `(,(bonk/dired-link "~/Downloads") :which-key "Downloads")
	"dp"  `(,(bonk/dired-link "~/Pictures") :which-key "Pictures")
	"dv"  `(,(bonk/dired-link "~/Videos") :which-key "Videos")
	"d."  `(,(bonk/dired-link "~/.config") :which-key "dotfiles-config")
	"dl"  `(,(bonk/dired-link "~/.local") :which-key "dotfiles-local")
	"de"  `(,(bonk/dired-link "~/.emacs-modularized") :which-key ".emacs.d"))

(setq dired-listing-switches "-al --group-directories-first")

(setup (:pkg image+ :straight t))
(setup (:pkg image-dired+ :straight t))
(setup (:pkg ranger :straight t)
  :options
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-on-disable t)
  (setq ranger-show-hidden nil)
  (setq helm-descbinds-window-style 'same-window)
  (setq ranger-footer-delay 0.2)
  (setq ranger-preview-delay 0.150)
  (setq ranger-preview-file nil)
  (setq ranger-show-literal nil)
  (setq ranger-width-preview 0.55)
  (setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
  (setq ranger-max-preview-size 5)
  (setq ranger-dont-show-binary t))

(bonk/leader-keys
  "r"  '(ranger :which-key "Ranger Current Dir"))

(setup (:pkg projectile :straight t)
  (:global "C-c p" projectile-command-map)
  (projectile-mode)
    (setq projectile-project-search-path '("~/." "~/github" "~/working"))
  (setq projectile-switch-project-action #'projectile-dired))

(setup (:pkg counsel-projectile :straight t)
  (counsel-projectile-mode))
