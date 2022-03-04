
# Table of Contents

1.  [Basic UI config](#org6239ca0)
    1.  [General conf](#orgca73ad3)
    2.  [Font Configuration](#org7d8451a)
2.  [Package And System setup](#org5c7847a)
3.  [Keybinding Configuration](#org70b60cf)
4.  [UI Configuration](#orga6866d4)
    1.  [Command Log Mode](#orgbfe44d4)
    2.  [Color Theme](#orgd4834b4)
    3.  [Better Modeline](#org9086ba1)
    4.  [Which Key](#orge2907ef)
    5.  [Ivy and Counsel](#org8ba4a30)
    6.  [Helpful Help Commands](#org39572cf)
    7.  [Text Scaling](#org2b0b3cf)
    8.  [Rule mode](#org92acb9b)
    9.  [Ido (tool for buffer)](#org8385a92)
5.  [Org Mode](#orgc274d4c)
    1.  [Basic configuration](#org1bfc463)
    2.  [Better Font Faces](#org3cf6926)
    3.  [Nicer Heading Bullets](#orgdcbf0bf)
    4.  [Center Org Buffers](#orgffc73ac)
    5.  [Auto-tangle Configuration FIles](#orgb9e1c01)
    6.  [Configure Babel Languages](#orgf0b37ea)
6.  [Development](#orgbd5f00b)
    1.  [General configs](#orged74f72)
    2.  [Languages](#orgced23aa)
        1.  [IDE Features with lsp-mode](#org925d318)
        2.  [Yasnippets](#org7130121)
        3.  [Flycheck](#org44a3b3a)
        4.  [TypeScript](#org4d503e9)
        5.  [Ruby](#org3dee510)
        6.  [Golang](#orge0d0fde)
        7.  [Elisp](#org931c602)
        8.  [Rust](#orgda45c7f)
        9.  [C/C++](#org62c8699)
        10. [Javascript](#orgb5386f6)
        11. [Yaml](#org90093c8)
    3.  [Company Mode](#org3bb13f5)
        1.  [Company Backends](#orgbb4454f)
    4.  [Projectile](#org61de8c9)
    5.  [Magit](#orgc6e0c15)
    6.  [Rainbow Delimiters](#orgf339ae9)
7.  [Terminals](#orgda877ed)
    1.  [Term-mode](#org7ff5a45)
        1.  [Useful key bindings:](#org1716518)
        2.  [Term-mode 256color](#org4fb524e)
    2.  [Vterm](#orga892b76)
        1.  [Keybindings for opening vterm in other windows with `SPACE v +options`](#orga04ffad)
    3.  [Shell-mode](#orgb04adae)
    4.  [Eshell](#org1808c26)
8.  [File Management](#orga69fcd3)
    1.  [Backup-files](#orgb1cb48c)
        1.  [Configuration](#orgdad8bee)
    2.  [Dired](#orgabf5fb4)
        1.  [Key Bindings](#org519e483)
        2.  [Configuration](#org6e2c401)
    3.  [NeoTree](#org832d657)
        1.  [KeyBindings](#org8438c2b)
        2.  [Configuration](#org395ed0a)
9.  [Structure Templates](#org2bdd679)
10. [Applications](#org41c0d36)
    1.  [example config 'Some App'](#orgd1c70a9)



<a id="org6239ca0"></a>

# Basic UI config


<a id="orgca73ad3"></a>

## General conf

    
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


<a id="org7d8451a"></a>

## Font Configuration

    
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


<a id="org5c7847a"></a>

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


<a id="org70b60cf"></a>

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


<a id="orga6866d4"></a>

# UI Configuration


<a id="orgbfe44d4"></a>

## Command Log Mode

[command-log-mode](https://github.com/lewang/command-log-mode) is useful for displaying a panel showing each key binding you use in a panel on the right side of the frame.  Great for live streams and screencasts!

    
    (use-package command-log-mode
      :commands command-log-mode)


<a id="orgd4834b4"></a>

## Color Theme

[doom-themes](https://github.com/hlissner/emacs-doom-themes) is a great set of themes with a lot of variety and support for many different Emacs modes.  Taking a look at the [screenshots](https://github.com/hlissner/emacs-doom-themes/tree/screenshots) might help you decide which one you like best.  You can also run `M-x counsel-load-theme` to choose between them easily.

    
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


<a id="org9086ba1"></a>

## Better Modeline

[doom-modeline](https://github.com/seagle0128/doom-modeline) is a very attractive and rich (yet still minimal) mode line configuration for Emacs.  The default configuration is quite good but you can check out the [configuration options](https://github.com/seagle0128/doom-modeline#customize) for more things you can enable or disable.

**NOTE:** The first time you load your configuration on a new machine, you'll need to run \`M-x all-the-icons-install-fonts\` so that mode line icons display correctly.

    
    (use-package all-the-icons)
    
    (use-package doom-modeline
      :init (doom-modeline-mode 1)
      :custom ((doom-modeline-height 15)))


<a id="orge2907ef"></a>

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


<a id="org8ba4a30"></a>

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


<a id="org39572cf"></a>

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


<a id="org2b0b3cf"></a>

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


<a id="org92acb9b"></a>

## Rule mode

    	;; Use ruler in text-mode
    ;;    (add-hook 'text-mode-hook
       ;;       (function (lambda ()
    	   ;;	   (setq ruler-mode-show-tab-stops t)
    		   ;;   (ruler-mode 1))))


<a id="org8385a92"></a>

## Ido (tool for buffer)

    (use-package ido
      :config
      (ido-mode 'buffers) ;; only use this line to turn off ido for file names!
      (setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
    							 "*Messages*" "Async Shell Command")))


<a id="orgc274d4c"></a>

# Org Mode


<a id="org1bfc463"></a>

## Basic configuration

    
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


<a id="org3cf6926"></a>

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


<a id="orgdcbf0bf"></a>

## Nicer Heading Bullets

    (use-package org-bullets
      :after org
      :hook (org-mode . org-bullets-mode)
      :custom
      (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


<a id="orgffc73ac"></a>

## Center Org Buffers

    (defun bonk/org-mode-visual-fill ()
      (setq visual-fill-column-width 100
    		visual-fill-column-center-text t)
      (visual-fill-column-mode 1))
    
    (use-package visual-fill-column
      :after org
      :hook (org-mode . bonk/org-mode-visual-fill))


<a id="orgb9e1c01"></a>

## Auto-tangle Configuration FIles

    ;; Automatically tangle our Emacs.org config file when we save it
    (defun bonk/org-babel-tangle-config ()
      (when (string-equal (buffer-file-name)
    					  (expand-file-name "~/github/emacs-config/config.org"))
    	;; Dynamic scoping to the rescue
    	(let ((org-confirm-babel-evaluate nil))
    	  (org-babel-tangle)))
      (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'bonk/org-babel-tangle-config))))


<a id="orgf0b37ea"></a>

## Configure Babel Languages

To execute or export code in `org-mode` code blocks, you'll need to set up `org-babel-load-languages` for each language you'd like to use.  [This page](https://orgmode.org/worg/org-contrib/babel/languages.html) documents all of the languages that you can use with `org-babel`.

    (with-eval-after-load 'org
      (org-babel-do-load-languages
    	'org-babel-load-languages
    	'((emacs-lisp . t)
    	  (python . t)
    	  (typescript . t)
    	  (go . t)
    	  (rust . t)))
    
      (push '("conf-unix" . conf-unix) org-src-lang-modes))


<a id="orgbd5f00b"></a>

# Development


<a id="orged74f72"></a>

## General configs

Here is the config for wether using tabs, how to indent, how many spaces, etc.

    
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


<a id="orgced23aa"></a>

## Languages


<a id="org925d318"></a>

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
          (lsp-solargraph-multi-root nil))

2.  lsp-ui

    [lsp-ui](https://emacs-lsp.github.io/lsp-ui/) is a set of UI enhancements built on top of `lsp-mode` which make Emacs feel even more
    like an IDE.  Check out the screenshots on the `lsp-ui` homepage (linked at the beginning
    of this paragraph) to see examples of what it can do.
    
        
        (use-package lsp-ui
          :hook (lsp-mode . lsp-ui-mode)
          :custom
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


<a id="org7130121"></a>

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


<a id="org44a3b3a"></a>

### Flycheck

Flycheck is one of the two main packages for code checks in the background. The
other one is Flymake. I use Flycheck because it allows me to define a custom “advanced”
checker.

    
    (use-package flycheck
      :after company)
    (setq flycheck-disabled-checkers '(ruby ruby-reek ruby-rubocop ruby-rubylint yaml-ruby))


<a id="org4d503e9"></a>

### TypeScript

This is a basic configuration for the TypeScript language so that `.ts` files activate `typescript-mode` when opened.  We're also adding a hook to `typescript-mode-hook` to call `lsp-deferred` so that we activate `lsp-mode` to get LSP features every time we edit TypeScript code.

    
    (use-package typescript-mode
      :mode "\\.ts\\'"
      :hook (typescript-mode . lsp-deferred))

**Important note!**  For `lsp-mode` to work with TypeScript (and JavaScript) you will need to install a language server on your machine.  If you have Node.js installed, the easiest way to do that is by running the following command:

    
    npm install -g typescript-language-server typescript

This will install the [typescript-language-server](https://github.com/theia-ide/typescript-language-server) and the TypeScript compiler package.


<a id="org3dee510"></a>

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


<a id="orge0d0fde"></a>

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


<a id="org931c602"></a>

### Elisp


<a id="orgda45c7f"></a>

### Rust


<a id="org62c8699"></a>

### C/C++


<a id="orgb5386f6"></a>

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


<a id="org90093c8"></a>

### Yaml

    ;; yaml-mode doesn't derive from prog-mode, but we can at least enable
    ;; whitespace-mode and apply cleanup.
    (use-package yaml-mode
      :after lsp-mode
      :config
      (add-hook 'yaml-mode-hook 'whitespace-mode)
      (add-hook 'yaml-mode-hook 'subword-mode))


<a id="org3bb13f5"></a>

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


<a id="orgbb4454f"></a>

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


<a id="org61de8c9"></a>

## Projectile

[Projectile](https://projectile.mx/) is a project management library for Emacs which makes it a lot easier to navigate around code projects for various languages.  Many packages integrate with Projectile so it's a good idea to have it installed even if you don't use its commands directly.

    
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


<a id="orgc6e0c15"></a>

## Magit

[Magit](https://magit.vc/) is the best Git interface I've ever used.  Common Git operations are easy to execute quickly using Magit's command panel system.

    
    (use-package magit
      :custom
      (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
    
    ;; NOTE: Make sure to configure a GitHub token before using this package!
    ;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
    ;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
    ;; (use-package forge)


<a id="orgf339ae9"></a>

## Rainbow Delimiters

[rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters) is useful in programming modes because it colorizes nested parentheses and brackets according to their nesting depth.  This makes it a lot easier to visually match parentheses in Emacs Lisp code without having to count them yourself.

    
    (use-package rainbow-delimiters
      :hook (prog-mode . rainbow-delimiters-mode))


<a id="orgda877ed"></a>

# Terminals


<a id="org7ff5a45"></a>

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


<a id="org1716518"></a>

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


<a id="org4fb524e"></a>

### Term-mode 256color

The `eterm-256color` package enhances the output of `term-mode` to enable handling of a
wider range of color codes so that many popular terminal applications look as you would
expect them to.  Keep in mind that this package requires `ncurses` to be installed on your
machine so that it has access to the `tic` program.  Most Linux distributions come with
this program installed already so you may not have to do anything extra to use it.

    
    (use-package eterm-256color
      :after term
      :hook (term-mode . eterm-256color-mode))


<a id="orga892b76"></a>

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


<a id="orga04ffad"></a>

### Keybindings for opening vterm in other windows with `SPACE v +options`

    (bonk/leader-keys
      "vt" '(vterm-other-window :which-key "vterm in new window")
      "vb" '(vterm :which-key "open new buffer for vterm"))


<a id="orgb04adae"></a>

## Shell-mode

TBD (i'm pretty satisfied with vterm, but i could try this one day)


<a id="org1808c26"></a>

## Eshell

TBD (i'm pretty satisfied with vterm, but i could try this one day)


<a id="orga69fcd3"></a>

# File Management


<a id="orgb1cb48c"></a>

## Backup-files


<a id="orgdad8bee"></a>

### Configuration

I don't like when emacs creates backup files next to the originals, since it can be tedious
to commit changes on a project.

    ;; Backup and Autosave Directories
      (setq temporary-file-directory "~/.tmp/emacs/")
      (setq auto-save-file-name-transforms
    	`((".*" ,temporary-file-directory t)))
      (setq backup-directory-alist            '((".*" . "~/.Trash")))


<a id="orgabf5fb4"></a>

## Dired


<a id="org519e483"></a>

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


<a id="org6e2c401"></a>

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


<a id="org832d657"></a>

## NeoTree


<a id="org8438c2b"></a>

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


<a id="org395ed0a"></a>

### Configuration

    
    (use-package neotree
      :config
      (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))


<a id="org2bdd679"></a>

# Structure Templates

    (with-eval-after-load 'org
    	(require 'org-tempo)
      (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
      (add-to-list 'org-structure-template-alist '("py" . "src python"))
      (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
      (add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
      (add-to-list 'org-structure-template-alist '("js" . "src javascript")))


<a id="org41c0d36"></a>

# Applications


<a id="orgd1c70a9"></a>

## example config 'Some App'

    
    value=4

