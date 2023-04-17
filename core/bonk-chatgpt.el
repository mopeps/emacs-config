

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
	:straight t
	:ensure t
	:bind
	(("C-=" . embark-act)
	 ([remap describe-bindings] . embark-bindings)
	 :map embark-defun-map
	 ("M-t" . chatgpt-gen-tests-for-region)
	 :map embark-general-map
	 ("M-c" . chatgpt-prompt))
	:custom
	(embark-indicators
	 '(embark-highlight-indicator
	   embark-isearch-highlight-indicator
	   embark-minimal-indicator))
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				 nil
				 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package chatgpt
  :straight (chatgpt :type git :host github :repo "emacs-openai/chatgpt"))
