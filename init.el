(setq inhibit-startup-message t)
(setq warning-minimum-level :emergency)

  (scroll-bar-mode -1) ; Disable visible scrollbar
  (tool-bar-mode -1)   ; Disable the toolbar
  (tooltip-mode -1)    ; Disable tooltips
  (set-fringe-mode 10) ; Give some breathing room

  (menu-bar-mode -1)   ; isable the menu bar

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

(set-face-attribute 'default nil :font  "Sarasa Gothic CL-12")
(set-face-font 'variable-pitch "Iosevka-12")
(set-face-font 'fixed-pitch "Sarasa Gothic CL-12")

;;============================================================
;; toggle between variable pitch and fixed pitch font for 
;; the current buffer
(defun fixed-pitch-mode ()
  (buffer-face-mode -1))

(defun variable-pitch-mode ()
  (buffer-face-mode t))

(defun toggle-pitch (&optional arg)
  "Switch between the `fixed-pitch' face and the `variable-pitch' face"
  (interactive)
  (buffer-face-toggle 'variable-pitch))

;; enable buffer-face mode to provide buffer-local fonts
(buffer-face-mode)

;; Set the fonts to format correctly
(add-hook 'text-mode-hook 'fixed-pitch-mode)
(add-hook 'dired-mode-hook 'variable-pitch-mode)
(add-hook 'calendar-mode-hook 'variable-pitch-mode)
(add-hook 'org-agenda-mode-hook 'variable-pitch-mode)
(add-hook 'shell-mode-hook 'variable-pitch-mode)
(add-hook 'eshell-mode-hook 'variable-pitch-mode)
(add-hook 'neotree-mode-hook 'variable-pitch-mode)
(add-hook 'counsel-mode-hook 'variable-pitch-mode)
(add-hook 'command-log-mode-hook 'variable-pitch-mode)
(add-hook 'which-key-mode-hook 'variable-pitch-mode)
(add-hook 'ivy-mode-hook 'variable-pitch-mode)
(add-hook 'helpful-mode-hook 'variable-pitch-mode)
										;(Add-hook 'bs-mode-hook 'fixed-pitch-mode)
										;(add-hook 'w3m-mode-hook 'variable-pitch-mode)
										;(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'eww-mode-hook 'variable-pitch-mode)

;; (C-q Tab) inserts a tab space
(add-hook 'ess-mode-hook (lambda () (local-set-key "\t" 'self-insert-command)))

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

(use-package doom-themes
:init (load-theme 'doom-zenburn t))
(use-package ewal-spacemacs-themes)
(use-package moe-theme)
(use-package zenburn-theme)
(use-package yoshi-theme)
(use-package sublime-themes)
(use-package gruvbox-theme)
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

;; Use ruler in text-mode
;;    (add-hook 'text-mode-hook
   ;;       (function (lambda ()
	   ;;	   (setq ruler-mode-show-tab-stops t)
		   ;;   (ruler-mode 1))))

(use-package ido
  :config
  (ido-mode 'buffers) ;; only use this line to turn off ido for file names!
  (setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
							 "*Messages*" "Async Shell Command")))

(defun bonk/org-no-line-number ()
  (global-display-line-numbers-mode 0))

  (use-package org
	:pin org
	:commands (org-capture org-agenda)
	:hook
	(org-mode . bonk/org-mode-setup)
	 (org-mode . bonk/org-no-line-number)
	:config
	(setq org-ellipsis " ▾")
	(bonk/org-font-setup))

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

(with-eval-after-load 'org
  (org-babel-do-load-languages
	'org-babel-load-languages
	'((emacs-lisp . t)
	  (python . t)
	  (typescript . t)
	  (go . t)
	  (rust . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(setq-default indent-tabs-mode t)
(setq-default tab-width 4) ; I want tabs to be four spaces wide
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
  (setq read-process-output-max (* 2048 2048)))

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
  (lsp-solargraph-multi-root nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
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
  (yas-global-mode))

(use-package yasnippet-snippets         ; Collection of snippets
  :after yasnippet)

(use-package flycheck
  :after company)
(setq flycheck-disabled-checkers '(ruby ruby-reek ruby-rubocop ruby-rubylint yaml-ruby))

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
  :diminish projectile-mode
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

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

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

(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(with-eval-after-load 'org
	(require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(tramp-theme ssh tramp-term zenburn-theme yoshi-theme yasnippet-snippets yaml-mode which-key web-mode vterm visual-fill-column use-package-ensure-system-package tide sublime-themes srcery-theme rspec-mode robe rjsx-mode rainbow-delimiters prettier-js org-bullets ob-typescript ob-rust ob-go neotree moe-theme magit lsp-ui lsp-java lsp-ivy ivy-rich helpful gruvbox-theme go-mode general flymake-ruby exec-path-from-shell ewal-spacemacs-themes evil-collection eterm-256color enh-ruby-mode doom-themes doom-modeline dired-single dired-open dired-hide-dotfiles cyberpunk-theme counsel-projectile company-web company-inf-ruby company-ctags company-box command-log-mode color-theme-sanityinc-tomorrow cmake-project all-the-icons-dired add-node-modules-path ac-js2)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
