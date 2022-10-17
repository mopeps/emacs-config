(provide 'bonk-evil)

(setup (:pkg undo-tree :straight t)
	(setq undo-tree-auto-save-history nil)
	(global-undo-tree-mode 1))

  (setup (:pkg evil :straight t)
	(setq evil-want-integration t)
	(setq evil-want-keybinding nil)
	(setq evil-want-C-u-scroll t)
	(setq evil-want-C-i-jump nil)
;; Activate evil-mode
	(evil-mode 1)
	(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
	(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

	;; Use visual line motions even outside of visual-line-mode buffers
	(evil-global-set-key 'motion "j" 'evil-next-visual-line)
	(evil-global-set-key 'motion "k" 'evil-previous-visual-line)
	(evil-set-initial-state 'pdf-view-mode 'normal)
	(evil-set-initial-state 'messages-buffer-mode 'normal)
	(evil-set-initial-state 'dashboard-mode 'normal))

(setup (:pkg evil-collection :straight t)
  ;; Is this a bug in evil-collection?
  (setq evil-collection-company-use-tng nil)
  (:load-after evil
	(:option evil-collection-outline-bind-tab-p nil
			 (remove evil-collection-mode-list) 'lispy
			 (remove evil-collection-mode-list) 'org-present)
	(evil-collection-init)))

(setup (:pkg which-key :straight t)
  ;; (diminish 'which-key-mode)
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(defun close-all-buffers ()
(interactive)
  (mapc 'kill-buffer (buffer-list)))

(setup (:pkg hydra :straight t)
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; Make ESC quit prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  (setup (:pkg general :straight t)
		 (general-create-definer bonk/leader-keys
		   :keymaps '(normal insert visual emacs)
		   :prefix "SPC"
		   :global-prefix "C-SPC"))

  (bonk/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text")
	"t"  '(:ignore t :which-key "toggles")
	"tt" '(counsel-load-theme :which-key "choose theme")
	;; Window navigation
	"h" '(evil-window-left :which-key "move to left window")
	"l" '(evil-window-right :which-key "move to right window")
	"j" '(evil-window-down :which-key "move to below window")
	"k" '(evil-window-up :which-key "move to above window")

	"H" '(evil-window-far-left :which-key "move to left window")
	"L" '(evil-window-far-right :which-key "move to right window")
	;; Window Splitting
	"v" '(evil-window-vsplit :which-key "vertical window split")
	"s" '(evil-window-split :which-key "window split")
	"c" '(evil-window-delete :which-key "close current window")
	;; Buffer options
	"DD" '(kill-this-buffer :which "kills the current buffer")
	"Vcc" '(vterm-send-C-c :which "kills current vterm process")
	"nn" '(neotree-toggle :which "toggles neotree")
	"A" '(close-all-buffers :which "kills all buffers")
	;; Origami options
	;;"oon" '(origami-open-node :which "opens current origami node")
   ;; "ooc" '(origami-close-node :which "closes current origami node")
   ;; "oO" '(origami-open-all-nodes :which "opens all origami node")
   ;; "oC" '(origami-close-all-nodes :which "closes all origami node")
   ;; "orO" '(origami-open-node-recursively :which "opens all origami node below recursively")
  ;;  "orC" '(origami-close-node-recursively :which "closes all origami node below recursively")
;; Guix
	"G"  '(:ignore t :which-key "Guix")
	"Gg" '(guix :which-key "Guix")
	"Gi" '(guix-installed-user-packages :which-key "user packages")
	"GI" '(guix-installed-system-packages :which-key "system packages")
	"Gp" '(guix-packages-by-name :which-key "search packages")
	"GP" '(guix-pull :which-key "pull")
	;; Org-Present
	"oP" '(org-present :which "launches org-present-mode")
	)
