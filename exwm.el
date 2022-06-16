(defun bonk/run-in-background (command)
	 (let ((command-parts (split-string command "[]+")))
	   (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

   (defun bonk/exwm-init-hook ()
	 (exwm-workspace-switch-create 1)

	 ;; (eshell)

	 (display-battery-mode 1)

	 (bonk/start-panel)

	 (bonk/run-in-background "dunst")
	 (bonk/run-in-background "pasystray")
	 (bonk/run-in-background "blueman-applet"))


   (defun bonk/exwm-update-class ()
	 (exwm-workspace-rename-buffer exwm-class-name))

   (defun bonk/exwm-update-title ()
	 (pcase exwm-class-name
	   ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

   ;; (defun bonk/configure-window-by-class ()
   ;;   (interactive)
   ;;   (pcase exwm-class-name
   ;; 	("Firefox" (exwm-workspace-move-window 2))
   ;; 	("Sol" (exwm-workspace-move-window 3))
   ;; 	("mpv" (exwm-floating-toggle-floating)
   ;; 	 (exwm-layout-toggle-mode-line))))

   (use-package exwm
	 :config
	 ;; Set the default number of workspaces
	 (setq exwm-workspace-number 5)

	 ;; When window "class" updates, use it to set the buffer name
	 (add-hook 'exwm-update-class-hook #'bonk/exwm-update-class)

	 ;; When window title updates, use it to set the buffer name
	 (add-hook 'exwm-update-title-hook #'bonk/exwm-update-title)

	 ;; Configure windows as they're created
	 ;; (add-hook 'exwm-update-finish-hook #'bonk/exwm-window-by-class)

	 ;; When EXWM starts up, do some extra configuration
	 (add-hook 'exwm-init-hook #'bonk/exwm-init-hook)


	 ;; These keys should always pass through to Emacs
	 (setq exwm-input-prefix-keys '(?\C-x
			 ?\C-u
			 ?\C-h
			 ?\M-x
			 ?\M-`
			 ?\M-&
			 ?\M-:
			 ?\C-\M-j  ;; Buffer list
			 ?\C-\ ))  ;; Ctrl+Space

	 ;; Ctrl+Q will enable the next key to be sent directly
	 (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
 ;; Set the screen resolution
	 ;; Ensure screen updates with xrandr will refresh EXWM frames
	 (require 'exwm-randr)
	 (exwm-randr-enable)
	 (start-process-shell-command "xrandr" nil "xrandr --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI1 --off --output VIRTUAL1 --off")
	 ;; Set up global key bindings.  These always work, no matter the input state!
	 ;; Keep in mind that changing this list after EXWM initializes has no effect.
;; Workspace switching
 (setq exwm-input-global-keys
		`(([?\s-\C-r] . exwm-reset)
		  ([?\s-w] . exwm-workspace-switch)
		  ([?\s-i] . exwm-input-toggle-keyboard)
		  ([?\s-r] . hydra-exwm-move-resize/body)
		  ([?\s-e] . dired-jump)
		  ([?\s-E] . (lambda () (interactive) (dired "~")))
		  ([?\s-Q] . (lambda () (interactive) (kill-buffer)))
		  ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
		  ,@(mapcar (lambda (i)
					  `(,(kbd (format "s-%d" i)) .
						 (lambda ()
						  (interactive)
						  (exwm-workspace-switch-create ,i))))
					 (number-sequence 0 9))))
 (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
	 (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))
