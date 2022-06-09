(setq inhibit-startup-message t)
	  (setq warning-minimum-level :emergency)

		(scroll-bar-mode -1) ; Disable visible scrollbar
		(tool-bar-mode -1)   ; Disable the toolbar
		(tooltip-mode -1)    ; Disable tooltips
		(set-fringe-mode 10) ; Give some breathing room

		(menu-bar-mode -1)   ; isable the menu bar

;; In Emacs 27+, package initialization occurs before `user-init-file' is
  ;; loaded, but after `early-init-file'. Doom handles package initialization, so
  ;; we must prevent Emacs from doing it early!
  		;; Set up the visible bell
		(setq visible-bell t)
	  (column-number-mode)
	  (global-display-line-numbers-mode t)

	  ;; Disable line numbers for some modes
	  (dolist (mode '(org-mode-hook
					  term-mode-hook
					  shell-mode-hook
					  eshell-mode-hook))
		(add-hook mode (lambda () (display-line-numbers-mode 0))))

;; UTF-8 as default encoding

	  (set-language-environment "utf-8")
	  (prefer-coding-system 'utf-8)
	  (setq coding-system-for-read 'utf-8)
	  (setq coding-system-for-write 'utf-8)

(defun set-font-to-fira-code ()
  (set-face-attribute 'default nil
					  :font "fira code-12"))
(defun my-frame-init ()
  ;; eg.
  (set-face-attribute 'default nil
					  :font  "cascadia code-13")
  (set-face-attribute 'mode-line nil
					  :font "cascadia code-11"
					  :weight 'normal)
	  (load-theme 'base16-onedark t))

(if (daemonp)
	(add-hook 'after-make-frame-functions
			  (lambda (frame)
				(select-frame frame)
				(my-frame-init)))
  (my-frame-init))

	  ;; (C-q Tab) inserts a tab space
	  (add-hook 'ess-mode-hook (lambda () (local-set-key "\t" 'self-insert-command)))
	(load-file "./ligature.el")
	(use-package ligature
	  :load-path "."
	  :config
	  ;; Enable the "www" ligature in every possible major mode
	  (ligature-set-ligatures 't '("www"))
	  ;; Enable traditional ligature support in eww-mode, if the
	  ;; `variable-pitch' face supports it
	  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
	  ;; Enable all Cascadia Code ligatures in programming modes
	  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
										   ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
										   "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
										   "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
										   "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
										   "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
										   "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
										   "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
										   ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
										   "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
										   "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
										   "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
										   "\\\\" "://"))
	  ;; Enables ligature checks globally in all buffers. You can also do it
	  ;; per mode with `ligature-mode'.
	  (global-ligature-mode t))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))


(setq package-check-signature nil) 
;; probably not necessary


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(use-package use-package-ensure-system-package)

(setq straight-vc-git-default-clone-depth 1)
(setq straight-recipes-gnu-elpa-use-mirror t)
;; (setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-check-for-modifications nil)
(defvar bootstrap-version)
(let* ((straight-repo-dir
        (expand-file-name "straight/repos" user-emacs-directory))
       (bootstrap-file
        (concat straight-repo-dir "/straight.el/bootstrap.el"))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (shell-command
     (concat
      "mkdir -p " straight-repo-dir " && "
      "git -C " straight-repo-dir " clone "
      "https://github.com/raxod502/straight.el.git && "
      "git -C " straight-repo-dir " checkout 2d407bc")))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
;; This is a variable that has been renamed but straight still refers when
;; doing :sraight (:no-native-compile t)
(setq comp-deferred-compilation-black-list nil)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :config
  (general-create-definer bonk/leader-keys
	:keymaps '(normal insert visual emacs)
	:prefix "SPC"
	:global-prefix "C-SPC"))

  (bonk/leader-keys
	"t"  '(:ignore t :which-key "toggles")
	"tt" '(counsel-load-theme :which-key "choose theme")
  ;; Window navigation
	"H" '(windmove-left :which-key "move to left window")
	"L" '(windmove-right :which-key "move to right window")
	"J" '(windmove-down :which-key "move to below window")
	"K" '(windmove-up :which-key "move to above window")
  ;; Buffer options
	"DD" '(kill-this-buffer :which "kills the current buffer")
	"vcc" '(vterm-send-C-c :which "kills current vterm process")
	"nn" '(neotree-toggle :which "toggles neotree")
  ;; Origami options
	"oon" '(origami-open-node :which "opens current origami node")
	"ooc" '(origami-close-node :which "closes current origami node")
	"oO" '(origami-open-all-nodes :which "opens all origami node")
	"oC" '(origami-close-all-nodes :which "closes all origami node")
	"orO" '(origami-open-node-recursively :which "opens all origami node below recursively")
	"orC" '(origami-close-node-recursively :which "closes all origami node below recursively")
	)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package command-log-mode
  :commands command-log-mode)



(use-package base16-theme)
	(use-package doom-themes)
	(use-package ewal-spacemacs-themes)
	(use-package moe-theme)
	(use-package zenburn-theme)
	(use-package yoshi-theme)
	(use-package sublime-themes)
	(use-package gruvbox-theme)
	(use-package nord-theme)
	(use-package color-theme-sanityinc-tomorrow)
	(use-package cyberpunk-theme)

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (set-face-attribute 'which-key-local-map-description-face nil :font "Iosevka-12")
  (set-face-attribute 'which-key-key-face nil :font "Iosevka-12")
  (set-face-attribute 'which-key-separator-face nil :font "Iosevka-12")
  (set-face-attribute 'which-key-note-face nil :font "Iosevka-12")
  (set-face-attribute 'which-key-special-key-face nil :font "Iosevka-12")
  (set-face-attribute 'which-key-group-description-face nil :font "Iosevka-12")
  (set-face-attribute 'which-key-command-description-face nil :font "Iosevka-12")
  (setq which-key-idle-delay 1)
  (setq which-key-allow-imprecise-window-fit t))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(bonk/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package ido
  :config
  (ido-mode 'buffers) ;; only use this line to turn off ido for file names!
  (setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
							 "*Messages*" "Async Shell Command")))

(defun bonk/org-no-line-number ()
	(display-line-numbers-mode 0))

(use-package org
  :straight (:no-native-compile t)
  :demand t
  :load-path "~/.emacs.d/elpa/org-9.5/"
    :pin org
	:commands (org-capture org-agenda)
	:hook
	 (org-mode . bonk/org-no-line-number)
	:config
	(setq org-ellipsis " ▾")
	)

(defun bonk/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
(set-face-attribute (car face) nil :font "Sarasa Fixed Slab K"
			:weight 'regular
			:height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun bonk/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
		visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :after org
  :hook (org-mode . bonk/org-mode-visual-fill))

;; Automatically tangle our Emacs.org config file when we save it
(defun bonk/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/github/emacs-config/config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'bonk/org-babel-tangle-config))))

(use-package ob-rust)
(use-package ob-go)
(use-package ob-typescript)
(with-eval-after-load 'org
  (org-babel-do-load-languages
	'org-babel-load-languages
	'((emacs-lisp . t)
	  (python . t)
	  (typescript . t)
	  (go . t)
	  (rust . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package origami
  :hook (c-mode . origami-mode)
  :hook (emacs-lisp-mode . origami-mode)
  :hook (go-mode . origami-mode)
  :hook (yaml-mode . origami-mode))

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
(setq gc-cons-threshold 100000000)
(when (boundp 'read-process-output-max)
  ;; New in Emacs 27
  (setq read-process-output-max (* 2048 2048))
  (load-file "./magit.el"))

(defun bonk/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . bonk/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config

  (add-hook 'typescript-mode-hook 'lsp)
  (add-hook 'rjsx-mode-hook 'lsp)
  (add-hook 'php-mode 'lsp)
  (add-hook 'css-mode 'lsp)
  (add-hook 'web-mode 'lsp)
  (lsp-enable-which-key-integration t)
  (setq lsp-completion-enable t)

  (setq lsp-language-id-configuration '((java-mode . "java")
										(python-mode . "python")
										(gfm-view-mode . "markdown")
										(rust-mode . "rust")
										(css-mode . "css")
										(xml-mode . "xml")
										(c-mode . "c")
										(c++-mode . "cpp")
										(objc-mode . "objective-c")
										(web-mode . "html")
										(html-mode . "html")
										(sgml-mode . "html")
										(mhtml-mode . "html")
										(go-mode . "go")
										(haskell-mode . "haskell")
										(php-mode . "php")
										(json-mode . "json")
										(rjsx-mode . "javascript")
										(typescript-mode . "typescript")
										))

  (setq lsp-diagnostics-provider :none)

  :custom
  (lsp-file-watch-threshold nil)
  (lsp-solargraph-multi-root nil)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-peek-always-show t)
(lsp-ui-sideline-show-hover nil)
(lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)



(use-package yasnippet                  ; Snippets
  :after company
  :config
  (setq
   yas-verbosity 1                      ; No need to be so verbose
   yas-wrap-around-region t)

  (with-eval-after-load 'yasnippet
	(setq yas-snippt-dirs '(yasnippet-snippets-dir)))

  (yas-reload-all)
  (yas-global-mode 1))

(use-package yasnippet-snippets         ; Collection of snippets
  :after yasnippet)

(use-package flycheck
  :after company)
(setq flycheck-disabled-checkers '(ruby ruby-reek ruby-rubocop ruby-rubylint yaml-ruby))

(use-package nvm
  :defer t)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred))

(use-package inf-ruby
  :after lsp)

(use-package ruby-mode
  :mode "\\.rb\\'"
  :hook ((ruby-mode . lsp-deferred))
  :config
  (setq ruby-indent-tabs-mode t)
  (setq ruby-indent-level tab-width))

(use-package enh-ruby-mode
  :after ruby-mode
  :config
  (setq enh-ruby-indent-tabs-mode t))

(use-package robe
  :after ruby-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(use-package rspec-mode
  :after ruby-mode)
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(use-package elixir-mode
  :ensure t
  :init  
  (add-hook 'elixir-mode-hook
            (lambda ()
              (push '(">=" . ?\u2265) prettify-symbols-alist)
              (push '("<=" . ?\u2264) prettify-symbols-alist)
              (push '("!=" . ?\u2260) prettify-symbols-alist)
              (push '("==" . ?\u2A75) prettify-symbols-alist)
              (push '("=~" . ?\u2245) prettify-symbols-alist)
              (push '("<-" . ?\u2190) prettify-symbols-alist)
              (push '("->" . ?\u2192) prettify-symbols-alist)
              (push '("<-" . ?\u2190) prettify-symbols-alist)
              (push '("|>" . ?\u25B7) prettify-symbols-alist))))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred)
  :config
  (add-hook 'go-mode-hook (lambda ()
							(setq tab-width 4)))
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (defun lsp-go-install-save-hooks ()
	(add-hook 'before-save-hook 'lsp-format-buffer t t)
	(add-hook 'before-save-hook 'lsp-organize-imports t t))
  (add-hook 'go-mode-hook 'lsp-go-install-save-hooks)
  ;; configure gopls
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
	 ("gopls.staticcheck" t t)))
  ;; Start LSP Mode and YASnippet mode
  (add-hook 'go-mode-hook 'lsp-deferred)
  (add-hook 'go-mode-hook 'yas-minor-mode))

(use-package ess
  :hook ((R-mode . lsp-deferred))
  :config
  (require 'ess-r-mode))

(use-package v-mode
  :straight (v-mode
             :type git
             :host github
             :repo "damon-kwok/v-mode"
             :files ("tokens" "v-mode.el"))
  :config
  :bind-keymap
  ("M-z" . v-menu)
  ("<f6>" . v-menu)
  ("C-c C-f" . v-format-buffer)
  :mode ("\\(\\.v?v\\|\\.vsh\\)$'" . 'v-mode))

(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :ensure t
  :init
    (require 'company)
    (slime-setup '(slime-fancy slime-company)))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
			  ("M-j" . lsp-ui-imenu)
			  ("M-?" . lsp-find-references)
			  ("C-c C-c l" . flycheck-list-errors)
			  ("C-c C-c a" . lsp-execute-code-action)
			  ("C-c C-c r" . lsp-rename)
			  ("C-c C-c q" . lsp-workspace-restart)
			  ("C-c C-c Q" . lsp-workspace-shutdown)
			  ("C-c C-c s" . lsp-rust-analyzer-status)
			  ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
			  ("C-c C-c d" . dap-hydra)
			  ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
	(setq-local buffer-save-without-query t)));; Create / cleanup rust scratch projects quickly

(use-package rust-playground :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for Cargo.toml and other config files

(use-package toml-mode :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; setting up debugging support with dap-mode (dap-mode is mainly used
;; for debugging in emacs, not only rust)

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(when (executable-find "lldb-mi")
  (use-package dap-mode
	:ensure
	:config
	(dap-ui-mode)
	(dap-ui-controls-mode 1)

	(require 'dap-lldb)
	(require 'dap-gdb-lldb)
	;; installs .extension/vscode
	(dap-gdb-lldb-setup)
	(dap-register-debug-template
	 "Rust::LLDB Run Configuration"
	 (list :type "lldb"
		   :request "launch"
		   :name "LLDB::Run"
	   :gdbpath "rust-lldb"
		   ;; uncomment if lldb-mi is not in PATH
		   ;; :lldbmipath "path/to/lldb-mi"
		   ))))

(use-package cuda-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(use-package flycheck-clang-analyzer
  :ensure t
  :config
  (with-eval-after-load 'flycheck
	(require 'flycheck-clang-analyzer)
	(flycheck-clang-analyzer-setup)))

(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode))

(use-package company-c-headers
  :ensure t)

(use-package company-irony
  :ensure t
  :config
  (setq company-backends '((company-c-headers
							company-dabbrev-code
							company-irony))))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(add-hook 'python-mode-hook 'flycheck-mode)

(with-eval-after-load 'company
    (add-hook 'python-mode-hook 'company-mode))

(use-package company-jedi
  :ensure t
  :config
    (require 'company)
    (add-to-list 'company-backends 'company-jedi))

(defun python-mode-company-init ()
  (setq-local company-backends '((company-jedi
                                  company-etags
                                  company-dabbrev-code))))

(use-package company-jedi
  :ensure t
  :config
    (require 'company)
    (add-hook 'python-mode-hook 'python-mode-company-init))

(use-package web-mode
  :mode "\\.html$'" "\\.jsx$" "\\.tsx$"
  :init 
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  )

(use-package rjsx-mode
  :mode "\\.js\\'"
  :hook (rjsx-mode . lsp-deferred)
  :init
  (setq indent-tabs-mode t)
  (setq js2-basic-offset 4))

(defun setup-tide-mode()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

  (use-package tide
	:after (rjsx-mode company flycheck)
	:hook (rjsx-mode . setup-tide-mode))

(use-package prettier-js
  :after (rjsx-mode)
  :hook (rjsx-mode . setup-tide-mode))

;; yaml-mode doesn't derive from prog-mode, but we can at least enable
;; whitespace-mode and apply cleanup.
(use-package yaml-mode
  :after lsp-mode
  :config
  (add-hook 'yaml-mode-hook 'whitespace-mode)
  (add-hook 'yaml-mode-hook 'subword-mode))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	  ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2))

(global-company-mode t)
(use-package company-box
  :hook (company-mode . company-box-mode))
(eval-after-load 'company
  '(push 'company-robe company-backends))



(use-package company-inf-ruby
:after (company ruby-mode)
:config (add-to-list 'company-backends 'company-inf-ruby))

(use-package ac-js2
:after (company tide js2-mode web-mode)
:config (add-to-list 'company-backends 'ac-js2))

;; HTML company backend
       (use-package company-web
	 :after (company web-mode)
	 :config (add-to-list 'company-backends 'company-web))
;; WIP missing CSS backend

;; Add `company-elisp' backend for elisp.
(add-hook 'emacs-lisp-mode-hook
		  '(lambda ()
			 (require 'company-elisp)
			 (push 'company-elisp company-backends)))

(use-package projectile
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/github")
    (setq projectile-project-search-path '("~/github")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; (use-package magit
  ;; :custom
  ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
;; (use-package forge)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh") ;; You can change this to bash, fish, etc
  ;;(setq explicit-zsh-args '()) ;; Use it to set especific shell args
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :after term
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :hook (vterm-mode . hide-mode-line-mode) ; modeline serves no purpose in vterm
  :config
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000)
  ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can
  ;; spawn another if want one.
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-timer-delay 0.01))

(bonk/leader-keys
  "vt" '(vterm-other-window :which-key "vterm in new window")
  "vb" '(vterm :which-key "open new buffer for vterm"))

;; Backup and Autosave Directories
  (setq temporary-file-directory "~/.tmp/emacs/")
  (setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))
  (setq backup-directory-alist            '((".*" . "~/.Trash")))

(use-package dired
   :ensure nil
   :commands (dired dired-jump)
   :bind (("C-x C-j" . dired-jump))
   :custom ((setq insert-directory-program "gls" dired-use-ls-dired t)
			(setq dired-listing-switches "-al --group-directories-first"))
   :config
   (evil-collection-define-key 'normal 'dired-mode-map
	 "h" 'dired-single-up-directory
	 "l" 'dired-single-buffer))

 (use-package dired-single)

 (use-package all-the-icons-dired
   :hook (dired-mode . all-the-icons-dired-mode))

 (use-package dired-open
   :config
   ;; Doesn't work as expected!
   ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
   (setq dired-open-extensions '(("png" . "feh")
								 ("mkv" . "mpv"))))

 (use-package dired-hide-dotfiles
   :hook (dired-mode . dired-hide-dotfiles-mode)
   :config
   (evil-collection-define-key 'normal 'dired-mode-map
	 "H" 'dired-hide-dotfiles-mode))
(use-package dired-rainbow
   :defer 2
   :config
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
   (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package neotree
  :defer t
  :custom
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package org-roam
  :after org-mode
  :straight t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Notes/Roam/")
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-capture-templates
	'(("d" "default" plain
	   #'org-roam-capture--get-point
	   "%?"
	   :file-name "%<%Y%m%d%H%M%S>-${slug}"
	   :head "#+title: ${title}\n"
	   :unnarrowed t)
	  ("ll" "link note" plain
	   #'org-roam-capture--get-point
	   "* %^{Link}"
	   :file-name "Inbox"
	   :olp ("Links")
	   :unnarrowed t
	   :immediate-finish)
	  ("lt" "link task" entry
	   #'org-roam-capture--get-point
	   "* TODO %^{Link}"
	   :file-name "Inbox"
	   :olp ("Tasks")
	   :unnarrowed t
	   :immediate-finish)))
  (org-roam-dailies-directory "Journal/")
  (org-roam-dailies-capture-templates
	'(("d" "default" entry
	   #'org-roam-capture--get-point
	   "* %?"
	   :file-name "Journal/%<%Y-%m-%d>"
	   :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
	  ("t" "Task" entry
	   #'org-roam-capture--get-point
	   "* TODO %?\n  %U\n  %a\n  %i"
	   :file-name "Journal/%<%Y-%m-%d>"
	   :olp ("Tasks")
	   :empty-lines 1
	   :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
	  ("j" "journal" entry
	   #'org-roam-capture--get-point
	   "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
	   :file-name "Journal/%<%Y-%m-%d>"
	   :olp ("Log")
	   :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
	  ("l" "log entry" entry
	   #'org-roam-capture--get-point
	   "* %<%I:%M %p> - %?"
	   :file-name "Journal/%<%Y-%m-%d>"
	   :olp ("Log")
	   :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
	  ("m" "meeting" entry
	   #'org-roam-capture--get-point
	   "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
	   :file-name "Journal/%<%Y-%m-%d>"
	   :olp ("Log")
	   :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")))
  :bind (:map org-roam-mode-map
		  (("C-c n l"   . org-roam)
		   ("C-c n f"   . org-roam-find-file)
		   ("C-c n d"   . org-roam-dailies-find-date)
		   ("C-c n c"   . org-roam-dailies-capture-today)
		   ("C-c n C r" . org-roam-dailies-capture-tomorrow)
		   ("C-c n t"   . org-roam-dailies-find-today)
		   ("C-c n y"   . org-roam-dailies-find-yesterday)
		   ("C-c n r"   . org-roam-dailies-find-tomorrow)
		   ("C-c n g"   . org-roam-graph))
		 :map org-mode-map
		 (("C-c n i" . org-roam-insert))
		 (("C-c n I" . org-roam-insert-immediate))))

(use-package elcord
  :straight t
  :custom
  (elcord-display-buffer-details nil)
  :config
  (elcord-mode))

(with-eval-after-load 'org
	(require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src C++")))
