(provide 'bonk-ide)

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
		(setq indent-tabs-mode nil))
	(if (> tab-count space-count)
		(setq indent-tabs-mode t))))

(defun bonk/prog-mode-settings ()
  (setq-default tab-width 4) ; I want tabs to be four spaces wide
  (setq standard-indent 4) ; I want indent to be four spaces wide
  (show-paren-mode t)
  (display-line-numbers-mode)
  (setq whitespace-line-column nil
		whitespace-style
		'(face indentation tabs tab-mark spaces space-mark newline newline-mark
			   trailing)
		whitespace-display-mappings
		'((tab-mark ?\t [?› ?\t])
		  (newline-mark ?\n [?¬ ?\n])
		  (space-mark ?\  [?·] [?.])))
  ;; (setq whitespace-display-mappings '(
  ;; 									   (tab-mark 9 [9474 9] [92 9])
  ;; 									   (space-mark 32 [183] [46])))
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
  (:hook-into prog-mode)
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

(setup (:pkg company-inf-ruby :straight t)
(:hook-into company)
(add-to-list 'company-backends 'company-inf-ruby))

(defun bonk/lsp-mode-setup ()
	(setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
	(lsp-headerline-breadcrumb-mode))

(setup (:pkg lsp-mode :straight t)
  (:disabled)
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
												(c-mode . "c")
												(dart-mode . "dart")
												(c++-mode . "cpp")
												;; (rustic-mode . "rust")
												(objc-mode . "objective-c")
												(web-mode . "html")
												(html-mode . "html")
												(sgml-mode . "html")
												(mhtml-mode . "html")
												(go-mode . "go")
												(haskell-mode . "haskell")
												(php-mode . "php")
												(json-mode . "json")
												(tsx-ts-mode . "typescript")
												(rjsx-mode . "javascript")
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
  (:disabled)
	   (:hook-into lsp-mode)
	   (:when-loaded
		(progn
		  (setq lsp-ui-sideline-enable t)
		  (setq lsp-ui-sideline-show-hover nil)
		  (setq lsp-ui-doc-enable t)
		  (setq lsp-ui-doc-position 'bottom))))

(setup (:pkg lsp-ivy :straight t)
  (:disabled)
  (:load-after lsp-mode))

(setup (:pkg consult-eglot :straight t)
  (:when-loaded
   (progn
	 (map! map eglot-mode-map [remap xref-find-definitions] #'consult-eglot-symbols))
  ))
(setup (:pkg flycheck-eglot :straight t))
(setup (:pkg eglot :straight t)
  :hook (flycheck-eglot-mode)
  :ensure t
  :commands (eglot-ensure)
  :options
  (setq eglot-sync-connect 1)
  (setq eglot-connect-timeout 10)
  (setq eglot-autoshutdown t)
  (setq eglot-send-changes-idle-time 0.8)
  (setq eglot-auto-display-help-buffer nil)
  )
;;  hooks
(defun bonk-ide--add-eglot-hooks (mode-list)
  "Iterates over MODE-LIST recursively to add eglot-ensure to
	  existing mode hooks.

	  The mode must be loaded, ie. found with `fboundp'. A mode which
	  is not loaded will not have a hook added, in which case add it
	  manually with something like this:

	  `(add-hook 'some-mode-hook #'eglot-ensure)'
	  "
  (dolist (mode-def mode-list)
	(let ((mode (if (listp mode-def) (car mode-def) mode-def)))
	  (cond
	   ((listp mode) (bonk-ide--add-eglot-hooks mode))
	   (t
		(when (and (fboundp mode)
				   (not (eq 'clojure-mode mode))  ; prefer cider
				   (not (eq 'lisp-mode mode))     ; prefer sly/slime
				   (not (eq 'scheme-mode mode))   ; prefer geiser
				   )
		  (let ((hook-name (concat (symbol-name mode) "-hook")))
			(message (concat "adding eglot to " hook-name))
			(add-hook (intern hook-name) #'eglot-ensure))))))))

;; add eglot to existing programming modes when eglot is loaded.
(with-eval-after-load "eglot"
  (bonk-ide--add-eglot-hooks eglot-server-programs)

  (add-to-list 'eglot-server-programs
			   '((ruby-mode) "solargraph" "stdio")))
	  ;;; customization
;; Shutdown server when last managed buffer is killed
(customize-set-variable 'eglot-autoshutdown t)
(customize-set-variable 'eglot-send t)

(setup (:pkg tree-sitter :straight t)
  (:hook tree-sitter-hl-mode)
  (:hook-into tsx-ts-mode))
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
  (:hook-into company-mode )
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
(setup (:pkg python-mode :straight t)
  (:hook tree-sitter-mode)
  (:hook eglot-ensure)
  :config

(set-docsets! '(python-mode inferior-python-mode) "Python 3" "NumPy" "SciPy" "Pandas")
  (set-ligatures! 'python-mode
				  ;; Functional
				  :def "def"
				  :lambda "lambda"
				  ;; Types
				  :null "None"
				  :true "True" :false "False"
				  :int "int" :str "str"
				  :float "float"
				  :bool "bool"
				  :tuple "tuple"
				  ;; Flow
				  :not "not"
				  :in "in" :not-in "not in"
				  :and "and" :or "or"
				  :for "for"
				  :return "return" :yield "yield")
  (:when-loaded
	(progn
	  (setq indent-tabs-mode nil)
	  (setq python-indent-guess-indent-offset t)
	  )))

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
 (:hook eglot-ensure)
 (:hook tree-sitter-mode)
 (setq ruby-indent-level 4)
  (setq ruby-indent-tabs-mode t)
  )

(setup (:pkg robe-mode :straight t)
  (:hook-into ruby-mode))
(eval-after-load 'company
  '(push 'company-robe company-backends))

(setup (:pkg rspec-mode :straight t)
  (:hook-into ruby-mode))

(setup (:pkg go-mode :straight t)
  (:file-match "\\.go\\'")
  (:hook tree-sitter-mode)
  (:hook eglot-ensure)
  (add-hook 'go-mode-hook (lambda ()
							(setq tab-width 4)))
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (defun lsp-go-install-save-hooks ()
	(add-hook 'before-save-hook 'lsp-format-buffer t t)
	(add-hook 'before-save-hook 'lsp-organize-imports t t))
  (add-hook 'go-mode-hook 'lsp-go-install-save-hooks))

(setup (:pkg rjsx-mode :straight t)
  (:disabled)
	(:file-match "\\.js\\' \\.jsx?\\' \\.tsx\\'")
	(:hook tree-sitter-hl-mode)
	(setq indent-tabs-mode 0)
	(setq js2-basic-offset 2))

(setup (:pkg prettier-js))

(setup (:pkg tsx-ts-mode)
	(:hook eglot-ensure)
	(:hook tide-setup)
	(tree-sitter-require 'tsx)
	(add-to-list
	 'tree-sitter-major-mode-language-alist
	 '(tsx-ts-mode . tsx))
	(:hook tree-sitter-hl-mode)
	(:hook tide-hl-identifier-mode)
	)

(setup (:pkg typescript-mode :straight t)
  (:hook tsx-ts-mode) ;; Completely hacky, feels dirty
  )

  (setup (:pkg tide :straight t)
	(setq flycheck-check-syntax-automatically '(save mode-enabled))
	(:load-after tsx-ts-mode company-mode flycheck-mode))

  (setup (:pkg js2-mode :straight t)
	;; Use js2-mode for Node scripts
	(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

	;; Don't use built-in syntax checking
	(setq js2-mode-show-strict-warnings nil))

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
  (:hook eglot-ensure))

(setup (:pkg sly :straight t)
  (:load-after common-lisp-mode)
  :options
   (setq sly-lisp-implementations
		 '((sbcl ("/usr/bin/sbcl")))))

(setup emacs-lisp-mode
  (:hook flycheck-mode)
  (:hook eglot-ensure)
  )
(setup (:pkg json-rpc :straight t))
(setup (:pkg json-rpc-server :straight t)
  :ensure t)

(setup (:pkg helpful :straight t)
  (:option counsel-describe-function-function #'helpful-callable
		   counsel-describe-variable-function #'helpful-variable)
  (:global [remap describe-function] helpful-function
		   [remap describe-symbol] helpful-symbol
		   [remap describe-variable] helpful-variable
		   [remap describe-command] helpful-command
		   [remap describe-key] helpful-key))

(bonk/set-leader-keys
  "e"   '(:ignore t :which-key "eval")
  "eb"  '(eval-buffer :which-key "eval buffer"))

(bonk/set-leader-keys
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
  (:hook eglot-ensure))

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
  (:hook eglot-ensure)
  )

(setup (:pkg gradle-mode :straight t))

(setup c-mode
	  (:hook tree-sitter-mode)
  (:hook copilot-mode)
	   (:hook eglot-ensure))

(setup c++-mode
	  (:hook tree-sitter-mode)
  (:hook copilot-mode)
	   (:hook eglot-ensure))

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

(setup (:pkg verilog-mode :straight t))

(defvar vhdl-ext-eglot-default-server 've-rust-hdl)

(defconst vhdl-ext-lsp-available-servers
  '((ve-hdl-checker . ("hdl_checker" "--lsp"))
	(ve-rust-hdl    . "vhdl_ls")
	(ve-ghdl-ls     . "ghdl-ls")
	(ve-vhdl-tool   . ("vhdl-tool" "lsp")))
  "Vhdl-ext available LSP servers.")
(defconst vhdl-ext-lsp-server-ids
  (mapcar #'car vhdl-ext-lsp-available-servers))
(defconst my-vhdl-style
  '((vhdl-tab-always-indent        . t)
	(vhdl-comment-only-line-offset . 4)
	(vhdl-offsets-alist            . ((arglist-close    . vhdl-lineup-arglist)
									  (statement-cont   . 0)
									  (case-alternative . 4)
									  (block-open       . 0)))
	(vhdl-echo-syntactic-information-p . t)
	)
  "My VHDL Programming Style")

(defun vhdl-ext-eglot-set-server (server-id)
  "Configure VHDL for `eglot' for selected SERVER-ID.
  Override any previous configuration for `vhdl-mode' and `vhdl-ts-mode'."
  (interactive (list (intern (completing-read "Server-id: " vhdl-ext-lsp-server-ids nil t))))
  (let ((cmd (alist-get server-id vhdl-ext-lsp-available-servers)))
	(unless cmd
	  (error "%s not recognized as a supported server" server-id))
	(if (not (executable-find (if (listp cmd)
								  (car cmd)
								cmd)))
		(message "%s not in $PATH, skipping config..." server-id)
	  ;; Else configure available server
	  (dolist (mode '(vhdl-mode vhdl-ts-mode))
		(setq eglot-server-programs (assq-delete-all mode eglot-server-programs))
		(if (listp cmd)
			(push (append (list mode) cmd) eglot-server-programs)
		  (push (list mode cmd) eglot-server-programs)))
	  (message "Set eglot VHDL server: %s" server-id))))
(defun bonk-vhdl-mode-hook ()
  ;; offset customizations not in my-vhdl-style
  (vhdl-set-offset 'statement-case-intro '++)
  ;; other customizations
  (setq tab-width 4
		;; this will make sure spaces are used instead of tabs
		indent-tabs-mode nil)
  (setq line-numbers-mode t)
  ;; keybindings for VHDL are put in vhdl-mode-map

  (vhdl-ext-eglot-set-server vhdl-ext-eglot-default-server)
  (define-key vhdl-mode-map "\C-m" 'newline-and-indent)

  )


(setup (:pkg vhdl-tools :straight t)
  (:hook-into vhdl-mode)
  (:hook whitespace-mode)
  (:hook display-line-numbers-mode)
  (:hook bonk-vhdl-mode-hook)
  ;; (:hook eglot-ensure)
  )

(setup (:pkg zig-mode :straight t)
  (:hook eglot-ensure)
  :config
  (setq zig-format-on-save nil) ; rely on :editor format instead
  (add-hook 'zig-mode-local-vars-hook #'tree-sitter! 'append)
  (flycheck-define-checker zig
	"A zig syntax checker using zig's `ast-check` command."
	:command ("zig" "ast-check" (eval (buffer-file-name)))
	:error-patterns
	((error line-start (file-name) ":" line ":" column ": error: " (message) line-end))
	:modes zig-mode)
  (add-to-list 'flycheck-checkers 'zig)
  )

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))
  (setup (:pkg rustic :straight t)
	(:hook copilot-mode)
	(:hook tree-sitter-mode)
	(:with-map rustic-mode-map
	  (:bind "C-c C-c l"  flycheck-list-errors
			 ))
	:config
	(setq rustic-rustfmt-config-alist '((edition . "2018")))
	(setq rustic-lsp-client 'eglot)
	(setq rust-prettify-symbols-alist nil)
	(setq rustic-indent-method-chain t)
	(after! rustic-flycheck
			(add-to-list 'flycheck-checkers 'rustic-clippy))
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
(define-key copilot-completion-map (kbd "C-c j") 'copilot-next-completion)
(define-key copilot-completion-map (kbd "C-c k") 'copilot-previous-completion)

(setup (:pkg dap-mode :straight t)
  (:disabled)
  ;; Assuming that `dap-debug' will invoke all this
  (:when-loaded
	(setq dap-auto-configure-features '(sessions locals controls tooltip))))

(defvar +debugger--realgud-alist
  '((realgud:bashdb    :modes (sh-mode))
	(realgud:gdb)
	(realgud:gub       :modes (go-mode))
	(realgud:kshdb     :modes (sh-mode))
	(realgud:pdb       :modes (python-mode))
	(realgud:perldb    :modes (perl-mode perl6-mode))
	(realgud:rdebug    :modes (ruby-mode))
	(realgud:remake)
	(realgud:trepan    :modes (perl-mode perl6-mode))
	(realgud:trepan2   :modes (python-mode))
	(realgud:trepan3k  :modes (python-mode))
	(realgud:trepanjs  :modes (javascript-mode js2-mode js3-mode))
	(realgud:trepanpl  :modes (perl-mode perl6-mode raku-mode))
	(realgud:zshdb     :modes (sh-mode))))
;; TODO Setup realgud

(setup (:pkg docker :straight t)
	   (:hook tree-sitter-mode)
  (:also-load docker-tramp))

(setup (:pkg docker-tramp :straight t))

(setup (:pkg terraform-mode :straight t)
	   (:file-match "\\.tf\\'")
	   (:hook tree-sitter-mode)
	  (:hook eglot-ensure))

(setup (:pkg company-terraform :straight t))

(setup (:pkg terraform-doc :straight t))

(setup (:pkg csv :straight t))
(setup (:pkg pandoc :straight t))
(setup (:pkg org-preview-html :straight t))

; Set up ESS, i.e. Statistics in Emacs, R, Stata, etc.
  (setup (:pkg ess :straight t)
	:defer t
	  (:hook tree-sitter-mode)
	)
  (setup (:pkg ess-view :straight t)
	:defer t)
  (setup (:pkg ess-view-data :straight t)
	:defer t)
  (setup (:pkg ess-r-insert-obj :straight t)
	:defer t)
(setup (:pkg ess-R-data-view :straight t)
  :defer t)
(setup (:pkg ess-smart-underscore :straight t)
  :defer t)

(setup (:pkg ein :straight t)
  (:hook eglot-ensure)
  (:hook tree-sitter-mode)
  )
(setup (:pkg math-preview :straight t))
(setup (:pkg ipython-shell-send :straight t))

(setup (:pkg conda :straight t)
  :options
  (setq conda-anaconda-home (expand-file-name "~/anaconda3/"))
  (setq conda-env-home-directory (expand-file-name "~/anaconda3/"))
  (setq conda-env-subdirectory "envs"))

(unless (getenv "CONDA_DEFAULT_ENV")
  (conda-env-activate "base"))

(setup (:pkg ebuild-mode :straight t))
