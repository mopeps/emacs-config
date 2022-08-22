(provide 'bonk-editing)

(defun bonk/org-no-line-number ()
	  (display-line-numbers-mode 0))


(setup (:pkg org :straight t)
	  (:also-load org-tempo)
	 (setq org-ellipsis " ▾"
		   org-hide-emphasis-markers t
		   org-src-fontify-natively t
		   org-fontify-quote-and-verse-blocks t
		   org-src-tab-acts-natively t
		   org-edit-src-content-indentation 2
		   org-hide-block-startup nil
		   org-src-preserve-indentation nil
		   org-startup-folded 'content
		   org-cycle-separator-lines 2
		   org-capture-bookmark nil)

	(setq org-refile-targets '((nil :maxlevel . 1)
							   (org-agenda-files :maxlevel . 1)))

	(setq org-outline-path-complete-in-steps nil)
	(setq org-refile-use-outline-path t)

	(org-babel-do-load-languages
	  'org-babel-load-languages
	  '((emacs-lisp . t)
		))

	(push '("conf-unix" . conf-unix) org-src-lang-modes))

(setup (:pkg org-superstar :straight t)
	(:load-after org)
	(:hook-into org-mode)
	(:option org-superstar-remove-leading-stars t
			 org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setup org-faces
  ;; Make sure org-indent face is available
  (:also-load org-indent)
  (:when-loaded
	;; Increase the size of various headings
	(set-face-attribute 'org-document-title nil :font "CaskaydiaCove Nerd Font" :weight 'bold :height 1.3)

	(dolist (face '((org-level-1 . 1.2)
					(org-level-2 . 1.1)
					(org-level-3 . 1.05)
					(org-level-4 . 1.0)
					(org-level-5 . 1.1)
					(org-level-6 . 1.1)
					(org-level-7 . 1.1)
					(org-level-8 . 1.1)))
	  (set-face-attribute (car face) nil :font "CaskaydiaCove Nerd Font" :weight 'medium :height (cdr face)))

	;; Ensure that anything that should be fixed-pitch in Org files appears that way
	(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
	(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
	(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
	(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
	(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
	(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
	(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
	(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
	(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

	;; Get rid of the background on column views
	(set-face-attribute 'org-column nil :background nil)
	(set-face-attribute 'org-column-title nil :background nil)))

;; TODO: Others to consider
;; '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;; '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-property-value ((t (:inherit fixed-pitch))) t)
;; '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;; '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;; '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; This is needed as of Org 9.2
(setup (:pkg org-tempo)
  (:when-loaded
	(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
	(add-to-list 'org-structure-template-alist '("py" . "src python"))
	(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
	(add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
	(add-to-list 'org-structure-template-alist '("li" . "src lisp"))
	(add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
	(add-to-list 'org-structure-template-alist '("js" . "src javascript"))
	(add-to-list 'org-structure-template-alist '("cpp" . "src C++"))
	(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
	(add-to-list 'org-structure-template-alist '("py" . "src python"))
	(add-to-list 'org-structure-template-alist '("go" . "src go"))
	(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
	(add-to-list 'org-structure-template-alist '("r" . "src R :noweb yes :exports both :results graphics :file ./fig_1?.png"))
	(add-to-list 'org-structure-template-alist '("json" . "src json"))))

(defun prob-buffer (buffer-name)
	"Creates a new probability and statistics buffer for school."
	(interactive "sSet new buffer Name: ")
	(let (($buf (generate-new-buffer buffer-name)))
	  (switch-to-buffer $buf)
	  (insert
	 "#+author:\n#+TITLE:
#+STARTUP: latexpreview
#+OPTIONS: toc:t
#+LATEX_CLASS: org-plain-extarticle
#+LATEX_CLASS_OPTIONS: [a4paper, 14pt]
#+LATEX_HEADER: \\usepackage{unicode-math}
#+LATEX_HEADER: \\usepackage{amsfonts}
#+PROPERTY: header-args:python :session hello
#+PROPERTY: header-args:python+ :async yes")
	  (funcall 'org-mode)
	  (setq buffer-offer-save t)))

(setup (:pkg org-pomodoro :straight t)

  (bonk/leader-keys
    "op"  '(org-pomodoro :which-key "pomodoro")))

(require 'org-protocol)

(defun bonk/org-mode-visual-fill ()
	(setq visual-fill-column-width 100
		  visual-fill-column-center-text t)
	(visual-fill-column-mode 1))

(setup (:pkg visual-fill-column :straight t)
      (:hook-into org-mode)
      (bonk/org-mode-visual-fill))

(setup (:pkg evil-org :straight t)
  (:hook-into org-mode org-agenda-mode)
  (require 'evil-org)
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation todo insert textobjects additional))
  (evil-org-agenda-set-keys))

(bonk/leader-keys
  "o"   '(:ignore t :which-key "org mode")

  "oi"  '(:ignore t :which-key "insert")
  "oil" '(org-insert-link :which-key "insert link")

  "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")

  "oa"  '(org-agenda :which-key "status")
  "ot"  '(org-todo-list :which-key "todos")
  "oc"  '(org-capture t :which-key "capture")
  "ox"  '(org-export-dispatch t :which-key "export"))

(setup (:pkg ob-rust :straight t))
	(setup (:pkg ob-go :straight t))
	(setup (:pkg ob-typescript :straight t))
	(setup (:pkg ob-ipython :straight t))
(setup (:pkg jupyter :straight t))
	(with-eval-after-load 'org
	  (org-babel-do-load-languages
		'org-babel-load-languages
		'((emacs-lisp . t)
		  (python . t)
		  (ipython . t)
		  (jupyter . t)
		  (R . t)
		  (typescript . t)
		  (go . t)
		  (scheme . t)
		  (rust . t)))
	  (org-babel-jupyter-override-src-block "python")

	  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(setup (:pkg org-make-toc :straight t)
  (:hook-into org-mode))

(use-package org-roam
  :after org-mode
  :straight t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Notes/Roam/")
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-capture-templates
	'(("d" "default" plain
	   #'org-roam-capture--get-point
	   "%?"
	   :file-name "%<%Y%m%d%H%M%S>-${slug}"
	   :head "#+title: ${title}\n"
	   :unnarrowed t)
	  ("ll" "link note" plain
	   #'org-roam-capture--get-point
	   "* %^{Link}"
	   :file-name "Inbox"
	   :olp ("Links")
	   :unnarrowed t
	   :immediate-finish)
	  ("lt" "link task" entry
	   #'org-roam-capture--get-point
	   "* TODO %^{Link}"
	   :file-name "Inbox"
	   :olp ("Tasks")
	   :unnarrowed t
	   :immediate-finish)))
  (org-roam-dailies-directory "Journal/")
  (org-roam-dailies-capture-templates
	'(("d" "default" entry
	   #'org-roam-capture--get-point
	   "* %?"
	   :file-name "Journal/%<%Y-%m-%d>"
	   :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
	  ("t" "Task" entry
	   #'org-roam-capture--get-point
	   "* TODO %?\n  %U\n  %a\n  %i"
	   :file-name "Journal/%<%Y-%m-%d>"
	   :olp ("Tasks")
	   :empty-lines 1
	   :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
	  ("j" "journal" entry
	   #'org-roam-capture--get-point
	   "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
	   :file-name "Journal/%<%Y-%m-%d>"
	   :olp ("Log")
	   :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
	  ("l" "log entry" entry
	   #'org-roam-capture--get-point
	   "* %<%I:%M %p> - %?"
	   :file-name "Journal/%<%Y-%m-%d>"
	   :olp ("Log")
	   :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
	  ("m" "meeting" entry
	   #'org-roam-capture--get-point
	   "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
	   :file-name "Journal/%<%Y-%m-%d>"
	   :olp ("Log")
	   :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")))
  :bind (:map org-roam-mode-map
		  (("C-c n l"   . org-roam)
		   ("C-c n f"   . org-roam-find-file)
		   ("C-c n d"   . org-roam-dailies-find-date)
		   ("C-c n c"   . org-roam-dailies-capture-today)
		   ("C-c n C r" . org-roam-dailies-capture-tomorrow)
		   ("C-c n t"   . org-roam-dailies-find-today)
		   ("C-c n y"   . org-roam-dailies-find-yesterday)
		   ("C-c n r"   . org-roam-dailies-find-tomorrow)
		   ("C-c n g"   . org-roam-graph))
		 :map org-mode-map
		 (("C-c n i" . org-roam-insert))
		 (("C-c n I" . org-roam-insert-immediate))))
