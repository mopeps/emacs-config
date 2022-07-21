(provide 'main)
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))
(require 'main-lib)
(require 'bonk-package)
(require 'bonk-evil)
(require 'bonk-editing)
(require 'bonk-ui)
(require 'bonk-ide)
(require 'bonk-startup)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
		  (lambda ()
			(message "Bonk Emacs loaded in %s."
					 (emacs-init-time))))

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(setup (:pkg no-littering :straight t)
  (require 'no-littering))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(setq large-file-warning-threshold nil)

(setq vc-follow-symlinks t)

(setup (:pkg diminish :straight t))

;; Comment this line if you don't have a email setup or if you don't want to set up any email
 (load-file "./email.el")

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
