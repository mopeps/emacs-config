(setup (:pkg pdf-tools :straight t
    			 :type git
    				 :host github
    				 :repo "dalanicolai/pdf-tools"
    			   :branch "pdf-roll"
    				 :files ("lisp/*.el" "README" ("build" "Makefile") ("build" "server") (:exclude "lisp/tablist.el" "lisp/tablist-filter.el") ))
    	(evil-set-initial-state 'pdf-view-mode 'normal))
    (add-hook 'doc-view-mode-hook (lambda () (require 'pdf-tools)))

    (with-eval-after-load 'pdf-tools
      (pdf-tools-install)
      (setq-default pdf-view-display-size 'fit-width))

    (setup (:pkg pdf-view-restore :straight t)
      :load-after 'pdf-tools
      :defer t
      :init
      (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

    (setup (:pkg image-roll :straight t
    :type git
    :host github
    :repo "dalanicolai/image-roll.el")
  :load-after 'pdf-tools
:defer t)

    (add-to-list 'display-buffer-alist
                 `("\\*.pdf\\*"
                   (window-width . 0.33)
                   (dedicated . side)  ;; Make the pdf a dedicated side-window
                   (side . left)       ;; to the left so it always stays open.
          		   (window-parameters . ((no-other-window . t)
                                         (no-delete-other-windows . t)))))
              ;;; crafted-pdf-reader.el ends here

(provide 'bonk-tex)
