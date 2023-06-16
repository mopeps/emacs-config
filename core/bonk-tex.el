(setup (:pkg pdf-tools :straight t)
   (evil-set-initial-state 'pdf-view-mode 'normal) )
(add-hook 'doc-view-mode-hook (lambda () (require 'pdf-tools)))

(with-eval-after-load 'pdf-tools
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width))

(setup (:pkg pdf-view-restore :straight t)
  :load-after 'pdf-tools
  :defer t
  :init
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

;;; crafted-pdf-reader.el ends here

(provide 'bonk-tex)
