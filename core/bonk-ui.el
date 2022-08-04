(provide 'bonk-ui)

(setup (:pkg command-log-mode :straight t)
  (command-log-mode 1))

(setup (:pkg all-the-icons :straight t))

  (setup (:pkg minions :straight t)
    (:hook-into doom-modeline-mode))
  
  (setup (:pkg doom-modeline :straight t)
    (:hook-into after-init-hook)
    (:option doom-modeline-height 15
             doom-modeline-bar-width 6
             doom-modeline-lsp t
             doom-modeline-github nil
             doom-modeline-mu4e nil
             doom-modeline-irc t
             doom-modeline-minor-modes t
             doom-modeline-persp-name nil
             doom-modeline-buffer-file-name-style 'truncate-except-project
             doom-modeline-major-mode-icon nil)
    (custom-set-faces '(mode-line ((t (:height 0.85))))
                      '(mode-line-inactive ((t (:height 0.85))))))

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

(setup (:pkg ivy-rich :straight t)
  (ivy-rich-mode 1))

(setup (:pkg counsel :straight t)
  (:global "C-M-j"  counsel-switch-buffer
          "C-r"  counsel-minibuffer-history)
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

(setup ido
  (ido-mode 'buffers) ;; only use this line to turn off ido for file names!
  (setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
							 "*Messages*" "Async Shell Command")))
