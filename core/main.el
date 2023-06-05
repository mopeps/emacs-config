(provide 'main)
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))
(require 'main-lib)
(require 'bonk-package)
(require 'bonk-modules)
(require 'bonk-evil)
(require 'bonk-editing)
(require 'bonk-ui)
(require 'bonk-project-explorer)
(require 'bonk-ide)
(require 'bonk-shell)
(require 'bonk-startup)
(require 'bonk-compile)
(require 'bonk-tex)


;; Profile emacs startup
(add-hook 'emacs-startup-hook
		  (lambda ()
			(message "Bonk Emacs loaded in %s."
					 (emacs-init-time))))

;; Comment this line if you don't have a email setup or if you don't want to set up any email
 (load-file "./email.el")

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
