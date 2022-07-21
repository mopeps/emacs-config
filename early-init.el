(setq gc-cons-threshold (* 50 1000 1000))

(customize-set-variable 'load-prefer-newer noninteractive)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(setq package-check-signature nil) 
;; probably not necessary


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(when (featurep 'native-compile)
(setq comp-speed 2)
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-deferred-compilation t)
  (when (fboundp 'startup-redirect-eln-cache)
	(if (version< emacs-version "29")
		(add-to-list 'native-comp-eln-load-path (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))
	  (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))))

  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Remove some unneeded UI elements (the user can turn back on anything they wish)
  (setq inhibit-startup-message t)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (push '(mouse-color . "white") default-frame-alist)

(customize-set-variable 'initial-major-mode 'fundamental-mode)
