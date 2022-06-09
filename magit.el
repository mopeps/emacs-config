(use-package magit
:commands magit-status
:custom
(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)
