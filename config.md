
# Table of Contents

1.  [Basic UI config](#orgeb197cb)
    1.  [General conf](#org0b3c175)
    2.  [Font Configuration](#orgf149467)
2.  [Package And System setup](#orgfae81d0)
    1.  [Straight.el](#orgb1ed043)
3.  [Keybinding Configuration](#org6a0180c)
4.  [UI Configuration](#org0851310)
    1.  [Command Log Mode](#orgdb4d294)
    2.  [Color Theme](#orge7c1f90)
        1.  [Custom Color<sub>theme</sub>](#org6fa47e5)
        2.  [downloaded<sub>themes</sub>](#org9e029cb)
    3.  [Better Modeline](#org5ffc884)
    4.  [Which Key](#org543b2e8)
    5.  [Ivy and Counsel](#org7d9e56a)
    6.  [Helpful Help Commands](#org9081c32)
    7.  [Text Scaling](#org8732ca2)
    8.  [Ido (tool for buffer)](#orgadcb4f0)
5.  [Org Mode](#org339a917)
    1.  [Basic configuration](#orgbda9aef)
    2.  [Better Font Faces](#orgbae36e3)
    3.  [Nicer Heading Bullets](#orgc548417)
    4.  [Center Org Buffers](#org3de624a)
    5.  [Auto-tangle Configuration FIles](#orgc1849a3)
    6.  [Configure Babel Languages](#orgbf5c48a)
6.  [Development](#org5e4ad26)
    1.  [General configs](#org0258bb2)
        1.  [Prog-mode](#org581a7e3)
    2.  [Languages](#org420692d)
        1.  [IDE Features with lsp-mode](#orgee5fc7f)
        2.  [Yasnippets](#org838f159)
        3.  [Flycheck](#org9265340)
        4.  [TypeScript](#org232b1c6)
        5.  [Ruby](#org44587dc)
        6.  [Elixir](#orgee31440)
        7.  [Golang](#orgf591952)
        8.  [R](#org0bf8b04)
        9.  [Vlang](#org6b782f9)
        10. [Elisp](#orgef76faa)
        11. [Rust](#org9f9f755)
        12. [C/C++](#orgb7c978a)
        13. [Python](#org52b2e44)
        14. [Javascript](#org772cc1f)
        15. [Yaml](#org690220f)
    3.  [Company Mode](#org13c708d)
        1.  [Company Backends](#org12e5ba6)
    4.  [Projectile](#orgaca95f9)
    5.  [Magit](#org2b530ee)
    6.  [Rainbow Delimiters](#org2df86ab)
7.  [Terminals](#orgad997c7)
    1.  [Term-mode](#org6561392)
        1.  [Useful key bindings:](#orgc7d5b9f)
        2.  [Term-mode 256color](#orgeff0abe)
    2.  [Vterm](#org4a3fd3e)
        1.  [Keybindings for opening vterm in other windows with `SPACE v +options`](#org65881a7)
    3.  [Shell-mode](#orgceb91df)
    4.  [Eshell](#orgd13e013)
8.  [File Management](#org5563da2)
    1.  [Backup-files](#org3174a2a)
        1.  [Configuration](#orgab1d429)
    2.  [Dired](#org700ec80)
        1.  [Key Bindings](#org4eaa188)
        2.  [Configuration](#org07c2c5f)
    3.  [NeoTree](#org021d3fb)
        1.  [KeyBindings](#orge817412)
        2.  [Configuration](#org6444441)
9.  [Org-Roam](#orgeaf6f74)
10. [Chats](#org240f5cb)
    1.  [Discord](#org368a4ba)
11. [Structure Templates](#org2ffa2a0)
12. [Applications](#org4e666a7)
    1.  [example config 'Some App'](#org8071158)



<a id="orgeb197cb"></a>

# Basic UI config


<a id="org0b3c175"></a>

## General conf

    
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


<a id="orgf149467"></a>

## Font Configuration

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
    	  (load-theme 'base16-nord t))
    
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


<a id="orgfae81d0"></a>

# Package And System setup

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


<a id="orgb1ed043"></a>

## Straight.el

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


<a id="org6a0180c"></a>

# Keybinding Configuration

This configuration uses [evil-mode](https://evil.readthedocs.io/en/latest/index.html) for a Vi-like modal editing experience.
[general.el](https://github.com/noctuid/general.el) is used for easy keybinding configuration that integrates well with which-key.
[evil-collection](https://github.com/emacs-evil/evil-collection) is used to automatically configure various Emacs modes with Vi-like keybindings
for evil-mode.

    
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


<a id="org0851310"></a>

# UI Configuration


<a id="orgdb4d294"></a>

## Command Log Mode

[command-log-mode](https://github.com/lewang/command-log-mode) is useful for displaying a panel showing each key binding you use in a panel on the right side of the frame.  Great for live streams and screencasts!

    
    (use-package command-log-mode
      :commands command-log-mode)


<a id="orge7c1f90"></a>

## Color Theme


<a id="org6fa47e5"></a>

### Custom Color<sub>theme</sub>


<a id="org9e029cb"></a>

### downloaded<sub>themes</sub>

[doom-themes](https://github.com/hlissner/emacs-doom-themes) is a great set of themes with a lot of variety and support for many different Emacs modes.  Taking a look at the [screenshots](https://github.com/hlissner/emacs-doom-themes/tree/screenshots) might help you decide which one you like best.  You can also run `M-x counsel-load-theme` to choose between them easily.

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


<a id="org5ffc884"></a>

## Better Modeline

[doom-modeline](https://github.com/seagle0128/doom-modeline) is a very attractive and rich (yet still minimal) mode line configuration for Emacs.  The default configuration is quite good but you can check out the [configuration options](https://github.com/seagle0128/doom-modeline#customize) for more things you can enable or disable.

**NOTE:** The first time you load your configuration on a new machine, you'll need to run \`M-x all-the-icons-install-fonts\` so that mode line icons display correctly.

    
    (use-package all-the-icons)
    
    (use-package doom-modeline
      :init (doom-modeline-mode 1)
      :custom ((doom-modeline-height 15)))


<a id="org543b2e8"></a>

## Which Key

[which-key](https://github.com/justbur/emacs-which-key) is a useful UI panel that appears when you start pressing any key binding in Emacs to offer you all possible completions for the prefix.  For example, if you press `C-c` (hold control and press the letter `c`), a panel will appear at the bottom of the frame displaying all of the bindings under that prefix and which command they run.  This is very useful for learning the possible key bindings in the mode of your current buffer.

    
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


<a id="org7d9e56a"></a>

## Ivy and Counsel

[Ivy](https://oremacs.com/swiper/) is an excellent completion framework for Emacs.  It provides a minimal yet powerful selection menu that appears when you open files, switch buffers, and for many other tasks in Emacs.  Counsel is a customized set of commands to replace \`find-file\` with \`counsel-find-file\`, etc which provide useful commands for each of the default completion commands.

[ivy-rich](https://github.com/Yevgnen/ivy-rich) adds extra columns to a few of the Counsel commands to provide more information about each item.

    
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


<a id="org9081c32"></a>

## Helpful Help Commands

[Helpful](https://github.com/Wilfred/helpful) adds a lot of very helpful (get it?) information to Emacs' `describe-` command buffers.  For example, if you use `describe-function`, you will not only get the documentation about the function, you will also see the source code of the function and where it gets used in other places in the Emacs configuration.  It is very useful for figuring out how things work in Emacs.

    
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


<a id="org8732ca2"></a>

## Text Scaling

This is an example of using [Hydra](https://github.com/abo-abo/hydra) to design a transient key binding for quickly adjusting the scale of the text on screen.  We define a hydra that is bound to `C-s t s` and, once activated, `j` and `k` increase and decrease the text scale.  You can press any other key (or `f` specifically) to exit the transient key map.

    
    (use-package hydra
      :defer t)
    
    (defhydra hydra-text-scale (:timeout 4)
      "scale text"
      ("j" text-scale-increase "in")
      ("k" text-scale-decrease "out")
      ("f" nil "finished" :exit t))
    
    (bonk/leader-keys
      "ts" '(hydra-text-scale/body :which-key "scale text"))


<a id="orgadcb4f0"></a>

## Ido (tool for buffer)

    (use-package ido
      :config
      (ido-mode 'buffers) ;; only use this line to turn off ido for file names!
      (setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
    							 "*Messages*" "Async Shell Command")))


<a id="org339a917"></a>

# Org Mode


<a id="orgbda9aef"></a>

## Basic configuration

    
      (defun bonk/org-no-line-number ()
    	(global-display-line-numbers-mode 0))
    
    (use-package org
      :straight (:no-native-compile t)
      :demand t
      :load-path "~/.emacs.d/elpa/org-9.5/"
    	:pin org
    	:commands (org-capture org-agenda)
    	:hook
    	(org-mode . bonk/org-mode-setup)
    	 (org-mode . bonk/org-no-line-number)
    	:config
    	(setq org-ellipsis " ▾")
    	(bonk/org-font-setup))


<a id="orgbae36e3"></a>

## Better Font Faces

    
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


<a id="orgc548417"></a>

## Nicer Heading Bullets

    (use-package org-bullets
      :after org
      :hook (org-mode . org-bullets-mode)
      :custom
      (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


<a id="org3de624a"></a>

## Center Org Buffers

    (defun bonk/org-mode-visual-fill ()
      (setq visual-fill-column-width 100
    		visual-fill-column-center-text t)
      (visual-fill-column-mode 1))
    
    (use-package visual-fill-column
      :after org
      :hook (org-mode . bonk/org-mode-visual-fill))


<a id="orgc1849a3"></a>

## Auto-tangle Configuration FIles

    ;; Automatically tangle our Emacs.org config file when we save it
    (defun bonk/org-babel-tangle-config ()
      (when (string-equal (buffer-file-name)
    					  (expand-file-name "~/github/emacs-config/config.org"))
    	;; Dynamic scoping to the rescue
    	(let ((org-confirm-babel-evaluate nil))
    	  (org-babel-tangle)))
      (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'bonk/org-babel-tangle-config))))


<a id="orgbf5c48a"></a>

## Configure Babel Languages

To execute or export code in `org-mode` code blocks, you'll need to set up `org-babel-load-languages` for each language you'd like to use.  [This page](https://orgmode.org/worg/org-contrib/babel/languages.html) documents all of the languages that you can use with `org-babel`.

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


<a id="org5e4ad26"></a>

# Development


<a id="org0258bb2"></a>

## General configs

Here is the config for wether using tabs, how to indent, how many spaces, etc.


<a id="org581a7e3"></a>

### Prog-mode

1.  Commenting Lines

        (use-package evil-nerd-commenter
          :bind ("M-/" . evilnc-comment-or-uncomment-lines))

2.  Folding with Origami

        (use-package origami
          :hook (c-mode . origami-mode)
          :hook (emacs-lisp-mode . origami-mode)
          :hook (go-mode . origami-mode)
          :hook (yaml-mode . origami-mode))

3.  Infer Indent Style

        
        (defun bonk/infer-indent-style ()
          ;; Honestly, This is more of a wild guess since we could be using tabs and having it wrongly
          ;; configure on our ide
          (let ((space-count (how-many "^ "))
        		(tab-count (how-many "^\t")))
        	(if (> space-count tab-count )
        		(setq indent-tabs-mode nil))
        	(if (> tab-count space-count)
        		(setq indent-tabs-mode t))))

4.  Configuration

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
          (setq read-process-output-max (* 2048 2048)))


<a id="org420692d"></a>

## Languages


<a id="orgee5fc7f"></a>

### IDE Features with lsp-mode

1.  lsp-mode

    We use the excellent [lsp-mode](https://emacs-lsp.github.io/lsp-mode/) to enable IDE-like functionality for many different programming languages via "language servers" that speak the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/).  Before trying to set up `lsp-mode` for a particular language, check out the [documentation for your language](https://emacs-lsp.github.io/lsp-mode/page/languages/) so that you can learn which language servers are available and how to install them.
    
    The `lsp-keymap-prefix` setting enables you to define a prefix for where `lsp-mode`'s default keybindings will be added.  I **highly recommend** using the prefix to find out what you can do with `lsp-mode` in a buffer.
    
    The `which-key` integration adds helpful descriptions of the various keys so you should be able to learn a lot just by pressing `C-c l` in a `lsp-mode` buffer and trying different things that you find there.
    
        
        
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

2.  lsp-ui

    [lsp-ui](https://emacs-lsp.github.io/lsp-ui/) is a set of UI enhancements built on top of `lsp-mode` which make Emacs feel even more
    like an IDE.  Check out the screenshots on the `lsp-ui` homepage (linked at the beginning
    of this paragraph) to see examples of what it can do.
    
        
        (use-package lsp-ui
          :hook (lsp-mode . lsp-ui-mode)
          :custom
          (lsp-ui-peek-always-show t)
        (lsp-ui-sideline-show-hover nil)
        (lsp-ui-doc-enable t)
          (lsp-ui-doc-position 'bottom))

3.  lsp-treemacs

    [lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs) provides nice tree views for different aspects of your code like symbols in a file, references of a symbol, or diagnostic messages (errors and warnings) that are found in your code.
    
    Try these commands with `M-x`:
    
    -   `lsp-treemacs-symbols` - Show a tree view of the symbols in the current file
    -   `lsp-treemacs-references` - Show a tree view for the references of the symbol under the cursor
    -   `lsp-treemacs-error-list` - Show a tree view for the diagnostic messages in the project
    
    This package is built on the [treemacs](https://github.com/Alexander-Miller/treemacs) package which might be of some interest to you if you like to have a file browser at the left side of your screen in your editor.
    
        
        (use-package lsp-treemacs
          :after lsp)

4.  lsp-ivy

    [lsp-ivy](https://github.com/emacs-lsp/lsp-ivy) integrates Ivy with `lsp-mode` to make it easy to search for things by name in your code.  When you run these commands, a prompt will appear in the minibuffer allowing you to type part of the name of a symbol in your code.  Results will be populated in the minibuffer so that you can find what you're looking for and jump to that location in the code upon selecting the result.
    
    Try these commands with `M-x`:
    
    -   `lsp-ivy-workspace-symbol` - Search for a symbol name in the current project workspace
    -   `lsp-ivy-global-workspace-symbol` - Search for a symbol name in all active project workspaces
    
        
        (use-package lsp-ivy
          :after lsp)

5.  lsp-clients


<a id="org838f159"></a>

### Yasnippets

Yasnippet automatically inserts code templates when I write a word and press the tab key.
It predefines most of the common templates, including the dreadful `if err !\=nil { ....`

    
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


<a id="org9265340"></a>

### Flycheck

Flycheck is one of the two main packages for code checks in the background. The
other one is Flymake. I use Flycheck because it allows me to define a custom “advanced”
checker.

    
    (use-package flycheck
      :after company)
    (setq flycheck-disabled-checkers '(ruby ruby-reek ruby-rubocop ruby-rubylint yaml-ruby))


<a id="org232b1c6"></a>

### TypeScript

This is a basic configuration for the TypeScript language so that `.ts` files activate `typescript-mode` when opened.  We're also adding a hook to `typescript-mode-hook` to call `lsp-deferred` so that we activate `lsp-mode` to get LSP features every time we edit TypeScript code.

    (use-package nvm
      :defer t)
    
    (use-package typescript-mode
      :mode "\\.ts\\'"
      :hook (typescript-mode . lsp-deferred))

**Important note!**  For `lsp-mode` to work with TypeScript (and JavaScript) you will need to install a language server on your machine.  If you have Node.js installed, the easiest way to do that is by running the following command:

    
    npm install -g typescript-language-server typescript

This will install the [typescript-language-server](https://github.com/theia-ide/typescript-language-server) and the TypeScript compiler package.


<a id="org44587dc"></a>

### Ruby

1.  inf-ruby

        (use-package inf-ruby
          :after lsp)

2.  ruby-mode

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

3.  robe-mode

        (use-package robe
          :after ruby-mode)
        (add-hook 'ruby-mode-hook 'robe-mode)
        (eval-after-load 'company
          '(push 'company-robe company-backends))

4.  Flymake

        (require 'flymake-ruby)
        (add-hook 'ruby-mode-hook 'flymake-ruby-load)

5.  rspec-mode

        (use-package rspec-mode
          :after ruby-mode)
        (eval-after-load 'rspec-mode
          '(rspec-install-snippets))


<a id="orgee31440"></a>

### Elixir

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


<a id="orgf591952"></a>

### Golang

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


<a id="org0bf8b04"></a>

### R

1.  ESS

        (use-package ess
          :hook ((R-mode . lsp-deferred))
          :config
          (require 'ess-r-mode))


<a id="org6b782f9"></a>

### Vlang

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


<a id="orgef76faa"></a>

### Elisp

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


<a id="org9f9f755"></a>

### Rust

1.  PreRequisites

    First of all, you'll need rust installed (obviously). Afterwards, it would be a good idea to have the
    rust-analyzer server running on background. You can install it by doing the following
    
        
          $ git clone https://github.com/rust-analyzer.git
        $ cd rust-analyzer
        $ cargo xtask install --server # will install rust-analyzer into $HOME/.cargo/bin
    
    By the way, it would be a good idea to check the changelog before cloning the HEAD of the repo, since
    some versions might not work for every machine.

2.  Config

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


<a id="orgb7c978a"></a>

### C/C++

    (use-package cuda-mode)
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


<a id="org52b2e44"></a>

### Python

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


<a id="org772cc1f"></a>

### Javascript

1.  Web Mode

        
        (use-package web-mode
          :mode "\\.html$'" "\\.jsx$" "\\.tsx$"
          :init 
          (setq web-mode-markup-indent-offset 4)
          (setq web-mode-css-indent-offset 4)
          (setq web-mode-code-indent-offset 4)
          )

2.  RJSX mode

    It has all the features from js2 , and all the js files i've been using till now are .js
    files, so i don't really need to differentiate between modes
    
        (use-package rjsx-mode
          :mode "\\.js\\'"
          :hook (rjsx-mode . lsp-deferred)
          :init
          (setq indent-tabs-mode t)
          (setq js2-basic-offset 4))

3.  Tide

        (defun setup-tide-mode()
          "Setup function for tide."
          (interactive)
          (tide-setup)
          (flycheck-mode +1)
          (setq flycheck-check-syntax-automatically '(save mode-enabled)))
        
          (use-package tide
        	:after (rjsx-mode company flycheck)
        	:hook (rjsx-mode . setup-tide-mode))

4.  Prettier

    It is important to have prettier installed through npm beforehand though
    `npm install -g prettier`
    
        (use-package prettier-js
          :after (rjsx-mode)
          :hook (rjsx-mode . setup-tide-mode))


<a id="org690220f"></a>

### Yaml

    ;; yaml-mode doesn't derive from prog-mode, but we can at least enable
    ;; whitespace-mode and apply cleanup.
    (use-package yaml-mode
      :after lsp-mode
      :config
      (add-hook 'yaml-mode-hook 'whitespace-mode)
      (add-hook 'yaml-mode-hook 'subword-mode))


<a id="org13c708d"></a>

## Company Mode

[Company Mode](http://company-mode.github.io/) provides a nicer in-buffer completion interface than `completion-at-point` which is more reminiscent of what you would expect from an IDE.  We add a simple configuration to make the keybindings a little more useful (`TAB` now completes the selection and initiates completion at the current location if needed).

We also use [company-box](https://github.com/sebastiencs/company-box) to further enhance the look of the completions with icons and better overall presentation.

    
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


<a id="org12e5ba6"></a>

### Company Backends

1.  Golang

2.  Ruby

          (use-package company-inf-ruby
        :after (company ruby-mode)
        :config (add-to-list 'company-backends 'company-inf-ruby))

3.  JavaScript

        
          (use-package ac-js2
        :after (company tide js2-mode web-mode)
        :config (add-to-list 'company-backends 'ac-js2))

4.  Web

        
        	;; HTML company backend
        	   (use-package company-web
        	 :after (company web-mode)
        	 :config (add-to-list 'company-backends 'company-web))
        ;; WIP missing CSS backend

5.  eLisp

        ;; Add `company-elisp' backend for elisp.
        (add-hook 'emacs-lisp-mode-hook
        		  '(lambda ()
        			 (require 'company-elisp)
        			 (push 'company-elisp company-backends)))


<a id="orgaca95f9"></a>

## Projectile

[Projectile](https://projectile.mx/) is a project management library for Emacs which makes it a lot easier to navigate around code projects for various languages.  Many packages integrate with Projectile so it's a good idea to have it installed even if you don't use its commands directly.

    
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


<a id="org2b530ee"></a>

## Magit

[Magit](https://magit.vc/) is the best Git interface I've ever used.  Common Git operations are easy to execute quickly using Magit's command panel system.

    
    ;; (use-package magit
      ;; :custom
      ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
    
    ;; NOTE: Make sure to configure a GitHub token before using this package!
    ;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
    ;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
    ;; (use-package forge)


<a id="org2df86ab"></a>

## Rainbow Delimiters

[rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters) is useful in programming modes because it colorizes nested parentheses and brackets according to their nesting depth.  This makes it a lot easier to visually match parentheses in Emacs Lisp code without having to count them yourself.

    
    (use-package rainbow-delimiters
      :hook (prog-mode . rainbow-delimiters-mode))


<a id="orgad997c7"></a>

# Terminals


<a id="org6561392"></a>

## Term-mode

`term-mode` is a built-in terminal emulator in Emacs. Because it is written in Emacs Lisp, you
can start using it inmediately with very little configuration. If you are on Linux or macOs,
`term-mode` is a great choice to get started because it supports fairly complex terminal applications
(`htop`, `vim` , etc) and works pretty reliably.

However, because it is written in ELisp, it can be slower than the other options like `vterm`.
The speed will only be an issue if you regularly run console apps with a lot of output.

-   **line-mode**: It enables you to use normal Emacs keybindings while moving around in the terminal
    buffer ( it's similar to NORMAL mode in vim)
-   **char-mode**: It's similar to insert mode in VIM, meaning that , it allows you to write in
    the terminal buffer.

With `evil-collection` installed, you will automatically switch to `char-mode` whenever you
enter insert mode, and when escaping, you will return to `line-mode`.

You can try running a terminal with `M-x term!`. :)


<a id="orgc7d5b9f"></a>

### Useful key bindings:

-   `C-c C-p` / `C-c C-n` - go back and forward in the buffer's promts( also `[[` and `]\]` with
    evil-mode)
-   `C-c C-k` - Enter char-mode
-   `C-c C-j` - Return to line-mode
-   If you have `evil-collection` term-mode will work the way i mentioned before.

    (use-package term
      :commands term
      :config
      (setq explicit-shell-file-name "zsh") ;; You can change this to bash, fish, etc
      ;;(setq explicit-zsh-args '()) ;; Use it to set especific shell args
      (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))


<a id="orgeff0abe"></a>

### Term-mode 256color

The `eterm-256color` package enhances the output of `term-mode` to enable handling of a
wider range of color codes so that many popular terminal applications look as you would
expect them to.  Keep in mind that this package requires `ncurses` to be installed on your
machine so that it has access to the `tic` program.  Most Linux distributions come with
this program installed already so you may not have to do anything extra to use it.

    
    (use-package eterm-256color
      :after term
      :hook (term-mode . eterm-256color-mode))


<a id="org4a3fd3e"></a>

## Vterm

[vterm](https://github.com/akermu/emacs-libvterm/) is an improved terminal emulator package which uses a compiled native module to
interact with the underlying terminal applications. This enables it to be much faster
than `term-mode` and to also provide a more complete terminal emulation experience.
Make sure that you have the [necessary dependencies](https://github.com/akermu/emacs-libvterm/#requirements) installed before trying to use
`vterm` because there is a module that will need to be compiled before you can use it
successfully.

    
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


<a id="org65881a7"></a>

### Keybindings for opening vterm in other windows with `SPACE v +options`

    (bonk/leader-keys
      "vt" '(vterm-other-window :which-key "vterm in new window")
      "vb" '(vterm :which-key "open new buffer for vterm"))


<a id="orgceb91df"></a>

## Shell-mode

TBD (i'm pretty satisfied with vterm, but i could try this one day)


<a id="orgd13e013"></a>

## Eshell

TBD (i'm pretty satisfied with vterm, but i could try this one day)


<a id="org5563da2"></a>

# File Management


<a id="org3174a2a"></a>

## Backup-files


<a id="orgab1d429"></a>

### Configuration

I don't like when emacs creates backup files next to the originals, since it can be tedious
to commit changes on a project.

    ;; Backup and Autosave Directories
      (setq temporary-file-directory "~/.tmp/emacs/")
      (setq auto-save-file-name-transforms
    	`((".*" ,temporary-file-directory t)))
      (setq backup-directory-alist            '((".*" . "~/.Trash")))


<a id="org700ec80"></a>

## Dired


<a id="org4eaa188"></a>

### Key Bindings

1.  Navigation

2.  Emacs / Evil

    -   `n` / `j` - next line
    -   `p` / `k` - previous line
    -   `j` / `J` - jump to file in buffer
    -   `RET` - select file or directory
    -   `^` - go to parent directory
    -   `S-RET` / `g O` - Open file in "other" window
    -   `M-RET` - Show file in other window without focusing (previewing files)
    -   `g o` (`dired-view-file`) - Open file but in a "preview" mode, close with `q`
    -   `g` / `g r` Refresh the buffer with `revert-buffer`
        after changing configuration (and after filesystem changes!)

3.  Marking Files

    -   `m` - Marks a file
    -   `u` - Unmarks a file
    -   `U` - Unmarks all files in buffer
    -   `* t` / `t` - Inverts marked files in buffer
    -   `% m` - Mark files in buffer using regular expression
    -   `*` - Lots of other auto-marking functions
    -   `k` / `K` - "Kill" marked items (refresh buffer with `g` / `g r` to get them back)
    -   Many operations can be done on a single file if there are no active marks!

4.  Copying and Renaming files

    -   `C` - Copy marked files (or if no files are marked, the current file)
    -   Copying single and multiple files
    -   `U` - Unmark all files in buffer
    -   `R` - Rename marked files, renaming multiple is a move!
    -   `% R` - Rename based on regular expression: `^test` , `old-\&`

5.  Power Command:

    `C-x C-q` (`dired-toggle-read-only`) - Makes all file names in the buffer
    editable directly to rename them!  Press `Z Z` to confirm renaming or `Z Q` to abort.

6.  Deleting Files

    -   `D` - Delete marked file
    -   `d` - Mark file for deletion
    -   `x` - Execute deletion for marks
    -   `delete-by-moving-to-trash` - Move to trash instead of deleting permanently

7.  Creating and extracting archives

    -   `Z` - Compress or uncompress a file or folder to (`.tar.gz`)
    -   `c` - Compress selection to a specific file
    -   `dired-compress-files-alist` - Bind compression commands to file extension

8.  Other common operations

    -   `T` - Touch (change timestamp)
    -   `M` - Change file mode
    -   `O` - Change file owner
    -   `G` - Change file group
    -   `S` - Create a symbolic link to this file
    -   `L` - Load an Emacs Lisp file into Emacs


<a id="org07c2c5f"></a>

### Configuration

    
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


<a id="org021d3fb"></a>

## NeoTree


<a id="orge817412"></a>

### KeyBindings

-   `n` next line, `p` previous line。
-   `SPC` or `RET` or `TAB` Open current item if it is a file. Fold/Unfold current item if it is a directory.
-   `U` Go up a directory
-   `g` Refresh
-   `A` Maximize/Minimize the NeoTree Window
-   `H` Toggle display hidden files
-   `O` Recursively open a directory
-   `C-c C-n` Create a file or create a directory if filename ends with a ‘/’
-   `C-c C-d` Delete a file or a directory.
-   `C-c C-r` Rename a file or a directory.
-   `C-c C-c` Change the root directory.
-   `C-c C-p` Copy a file or a directory.


<a id="org6444441"></a>

### Configuration

    
    (use-package neotree
      :defer t
      :custom
      (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))


<a id="orgeaf6f74"></a>

# Org-Roam

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


<a id="org240f5cb"></a>

# Chats


<a id="org368a4ba"></a>

## Discord

    
    (use-package elcord
      :straight t
      :custom
      (elcord-display-buffer-details nil)
      :config
      (elcord-mode))


<a id="org2ffa2a0"></a>

# Structure Templates

    (with-eval-after-load 'org
    	(require 'org-tempo)
      (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
      (add-to-list 'org-structure-template-alist '("py" . "src python"))
      (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
      (add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
      (add-to-list 'org-structure-template-alist '("js" . "src javascript")))


<a id="org4e666a7"></a>

# Applications


<a id="org8071158"></a>

## example config 'Some App'

    
    value=4

