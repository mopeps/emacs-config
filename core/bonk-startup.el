(provide 'bonk-startup)

;; Check the system used
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-BSD     (or ON-MAC (eq system-type 'berkeley-unix)))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(set-language-environment "utf-8")
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

(setup (:pkg base16-theme :straight t))
(setup (:pkg spacegray-theme :straight t))
(setup (:pkg doom-themes :straight t))

(add-hook 'emacs-startup-hook
		  (lambda ()
			(custom-set-faces
			 '(default ((t (:font "CaskaydiaCove Nerd Font-15", :slant 'italic))))
			 '(fixed-pitch ((t (:inherit (default)))))
			 '(fixed-pitch-serif ((t (:inherit (default)))))
			 '(variable-pitch ((t (:font "Fira Code 15"))))))
		  (load-theme 'base16-gruvbox-dark-hard t))

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

(if (daemonp)
	(add-hook 'after-make-frame-functions
			  (lambda (frame)
				(select-frame frame)
				)))
