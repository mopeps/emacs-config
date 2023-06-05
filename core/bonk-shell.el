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

(bonk/set-leader-keys
  "Vt" '(vterm-other-window :which-key "vterm in new window")
  "Vb" '(vterm :which-key "open new buffer for vterm"))

(setup (:pkg exec-path-from-shell :straight t)
      :ensure t
      (exec-path-from-shell-initialize))

(setup (:pkg load-env-vars :straight t)
  :ensure t)

  (defvar @-dotenv-file-name ".env"
  "The name of the .env file."
  )

(defun @-find-env-file ()
  "Find the closest .env file in the directory hierarchy."

  (let* ((env-file-directory (locate-dominating-file "." @-dotenv-file-name))
		(file-name (concat env-file-directory @-dotenv-file-name)))
	(when (file-exists-p file-name)
		file-name))
  )

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

(provide 'bonk-shell)
