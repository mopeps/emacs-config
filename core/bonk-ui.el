(provide 'bonk-ui)

(setup (:pkg command-log-mode :straight t)
	(command-log-mode 1))

(setup (:pkg all-the-icons :straight t))

    (setup (:pkg minions :straight t)
  	(:hook-into telephone-line-mode))
(use-package telephone-line
  :custom 
  (telephone-line-primary-left-separator 'telephone-line-cubed-left)
  (telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left)
  (telephone-line-primary-right-separator 'telephone-line-cubed-right)
  (telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (telephone-line-height 24)
  (telephone-line-evil-use-short-tag t)  
  :config
  (telephone-line-defsegment telephone-line-pdf-segment ()
			     (if (eq major-mode 'pdf-view-mode)
				 (propertize (pdf-view-page-number)
					     'face '(:inherit)
					     'display '(raise 0.0)
					     'mouse-face '(:box 1)
					     'local-map (make-mode-line-mouse-map
							 'mouse-1 (lambda ()
								    (interactive)
								    (pdf-view-goto-page))))))
  (telephone-line-defsegment telephone-line-winum-segment ()
			     (propertize winum--mode-line-segment
					 'face '(:box (:line-width 2 :color "cyan" :style released-button))		
					 'display '(raise 0.0)
					 'mouse-face '(:box 1)))
  (setq telephone-line-lhs '((accent . (telephone-line-winum-segment
					telephone-line-pdf-segment
					telephone-line-vc-segment
					telephone-line-erc-modified-channels-segment
					telephone-line-process-segment))
			     (nil . (telephone-line-projectile-segment telephone-line-buffer-segment))))
  (telephone-line-mode t))

    ;; (setup (:pkg doom-modeline :straight t)
    ;; 	(:hook-into after-init-hook)
    ;; 	(:option doom-modeline-height 15
    ;; 			 doom-modeline-bar-width 4
    ;; 			 doom-modeline-lsp t
    ;; 			 doom-modeline-github nil
    ;; 			 doom-modeline-mu4e nil
    ;; 			 doom-modeline-irc t
    ;; 			 doom-modeline-minor-modes t
    ;; 			 doom-modeline-modal-icon t
    ;; 			 doom-modeline-persp-name nil
    ;; 			 doom-modeline-buffer-file-name-style 'truncate-except-project
    ;; 			 doom-modeline-major-mode-icon nil)
    ;; 	(setq all-the-icons-scale-factor 0.9)
    ;; 	(custom-set-faces '(mode-line ((t (:height 0.85, :family "Fira Code"))))
    ;; 					  '(mode-line-inactive ((t (:height 0.85, :family "Fira Code"))))))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(setup (:pkg ivy-rich :straight t)
  (ivy-rich-mode 1))

(setup (:pkg counsel :straight t)
  (:global "C-M-j"  counsel-switch-buffer
          "C-r"  counsel-minibuffer-history)
  (counsel-mode 1))

(use-package helpful
	:commands (helpful-callable helpful-variable helpful-command helpful-key)
	:custom
	(counsel-describe-function-function #'helpful-callable)
	(counsel-describe-variable-function #'helpful-variable)
	:bind
	([remap describe-function] . counsel-describe-function)
	([remap describe-command] . helpful-command)
	([remap describe-variable] . counsel-describe-variable)
	([remap describe-key] . helpful-key))

(use-package hydra
	:defer t)

  (defhydra hydra-text-scale (:timeout 4)
	"scale text"
	("j" text-scale-increase "in")
	("k" text-scale-decrease "out")
	("f" nil "finished" :exit t))

(bonk/set-leader-keys
	"ts" '(hydra-text-scale/body :which-key "scale text"))

(setup ido
  (ido-mode 'buffers) ;; only use this line to turn off ido for file names!
  (setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
							  "*Messages*" "Async Shell Command")))

(defvar +ligatures-extra-symbols
  '(;; org
    :name          "»"
    :src_block     "»"
    :src_block_end "«"
    :quote         "“"
    :quote_end     "”"
    ;; Functional
    :lambda        "λ"
    :def           "ƒ"
    :composition   "∘"
    :map           "↦"
    ;; Types
    :null          "∅"
    :true          "𝕋"
    :false         "𝔽"
    :int           "ℤ"
    :float         "ℝ"
    :str           "𝕊"
    :bool          "𝔹"
    :list          "𝕃"
    ;; Flow
    :not           "￢"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⟼"
    :yield         "⟻"
    ;; Other
    :union         "⋃"
    :intersect     "∩"
    :diff          "∖"
    :tuple         "⨂"
    :pipe          "" ;; FIXME: find a non-private char
    :dot           "•")
  "Maps identifiers to symbols, recognized by `set-ligatures'.

This should not contain any symbols from the Unicode Private Area! There is no
universal way of getting the correct symbol as that area varies from font to
font.")

(defun set-ligatures! (modes &rest plist)
  "Associates string patterns with icons in certain major-modes.

  MODES is a major mode symbol or a list of them.
  PLIST is a property list whose keys must match keys in
`+ligatures-extra-symbols', and whose values are strings representing the text
to be replaced with that symbol. If the car of PLIST is nil, then unset any
pretty symbols previously defined for MODES.

This function accepts one special property:

  :alist ALIST
    Appends ALIST to `prettify-symbols-alist' literally, without mapping text to
    `+ligatures-extra-symbols'.

For example, the rule for emacs-lisp-mode is very simple:

  (set-ligatures! 'emacs-lisp-mode
    :lambda \"lambda\")

This will replace any instances of \"lambda\" in emacs-lisp-mode with the symbol
assicated with :lambda in `+ligatures-extra-symbols'.

Pretty symbols can be unset for emacs-lisp-mode with:

  (set-ligatures! 'emacs-lisp-mode nil)"
  (declare (indent defun))
  (if (null (car-safe plist))
      (dolist (mode (ensure-list modes))
        (delq! mode +ligatures-extra-alist 'assq))
    (let (results)
      (while plist
        (let ((key (pop plist)))
          (if (eq key :alist)
              (prependq! results (pop plist))
            (when-let (char (plist-get +ligatures-extra-symbols key))
              (push (cons (pop plist) char) results)))))
      (dolist (mode (ensure-list modes))
        (setf (alist-get mode +ligatures-extra-alist)
              (if-let (old-results (alist-get mode +ligatures-extra-alist))
                  (dolist (cell results old-results)
                    (setf (alist-get (car cell) old-results) (cdr cell)))
                results))))))

(defvar +ligatures-extra-alist '((t))
  "A map of major modes to symbol lists (for `prettify-symbols-alist').")

(defvar +ligatures-composition-alist
  '((?!  . "\\(?:!\\(?:==\\|[!=]\\)\\)")                                      ; (regexp-opt '("!!" "!=" "!=="))
    (?#  . "\\(?:#\\(?:###?\\|_(\\|[#(:=?[_{]\\)\\)")                         ; (regexp-opt '("##" "###" "####" "#(" "#:" "#=" "#?" "#[" "#_" "#_(" "#{"))
    (?$  . "\\(?:\\$>>?\\)")                                                  ; (regexp-opt '("$>" "$>>"))
    (?%  . "\\(?:%%%?\\)")                                                    ; (regexp-opt '("%%" "%%%"))
    (?&  . "\\(?:&&&?\\)")                                                    ; (regexp-opt '("&&" "&&&"))
    (?*  . "\\(?:\\*\\(?:\\*[*/]\\|[)*/>]\\)?\\)")                            ; (regexp-opt '("*" "**" "***" "**/" "*/" "*>" "*)"))
    (?+  . "\\(?:\\+\\(?:\\+\\+\\|[+:>]\\)?\\)")                              ; (regexp-opt '("+" "++" "+++" "+>" "+:"))
    (?-  . "\\(?:-\\(?:-\\(?:->\\|[>-]\\)\\|<[<-]\\|>[>-]\\|[:<>|}~-]\\)\\)") ; (regexp-opt '("--" "---" "-->" "--->" "->-" "-<" "-<-" "-<<" "->" "->>" "-}" "-~" "-:" "-|"))
    (?.  . "\\(?:\\.\\(?:\\.[.<]\\|[.=>-]\\)\\)")                             ; (regexp-opt '(".-" ".." "..." "..<" ".=" ".>"))
    (?/  . "\\(?:/\\(?:\\*\\*\\|//\\|==\\|[*/=>]\\)\\)")                      ; (regexp-opt '("/*" "/**" "//" "///" "/=" "/==" "/>"))
    (?:  . "\\(?::\\(?:::\\|[+:<=>]\\)?\\)")                                  ; (regexp-opt '(":" "::" ":::" ":=" ":<" ":=" ":>" ":+"))
    (?\; . ";;")                                                              ; (regexp-opt '(";;"))
    (?0  . "0\\(?:\\(x[a-fA-F0-9]\\).?\\)") ; Tries to match the x in 0xDEADBEEF
    ;; (?x . "x") ; Also tries to match the x in 0xDEADBEEF
    ;; (regexp-opt '("<!--" "<$" "<$>" "<*" "<*>" "<**>" "<+" "<+>" "<-" "<--" "<---" "<->" "<-->" "<--->" "</" "</>" "<<" "<<-" "<<<" "<<=" "<=" "<=<" "<==" "<=>" "<===>" "<>" "<|" "<|>" "<~" "<~~" "<." "<.>" "<..>"))
    (?<  . "\\(?:<\\(?:!--\\|\\$>\\|\\*\\(?:\\*?>\\)\\|\\+>\\|-\\(?:-\\(?:->\\|[>-]\\)\\|[>-]\\)\\|\\.\\(?:\\.?>\\)\\|/>\\|<[<=-]\\|=\\(?:==>\\|[<=>]\\)\\||>\\|~~\\|[$*+./<=>|~-]\\)\\)")
    (?=  . "\\(?:=\\(?:/=\\|:=\\|<[<=]\\|=[=>]\\|>[=>]\\|[=>]\\)\\)")         ; (regexp-opt '("=/=" "=:=" "=<<" "==" "===" "==>" "=>" "=>>" "=>=" "=<="))
    (?>  . "\\(?:>\\(?:->\\|=>\\|>[=>-]\\|[:=>-]\\)\\)")                      ; (regexp-opt '(">-" ">->" ">:" ">=" ">=>" ">>" ">>-" ">>=" ">>>"))
    (??  . "\\(?:\\?[.:=?]\\)")                                               ; (regexp-opt '("??" "?." "?:" "?="))
    (?\[ . "\\(?:\\[\\(?:|]\\|[]|]\\)\\)")                                    ; (regexp-opt '("[]" "[|]" "[|"))
    (?\\ . "\\(?:\\\\\\\\[\\n]?\\)")                                          ; (regexp-opt '("\\\\" "\\\\\\" "\\\\n"))
    (?^  . "\\(?:\\^==?\\)")                                                  ; (regexp-opt '("^=" "^=="))
    (?w  . "\\(?:wwww?\\)")                                                   ; (regexp-opt '("www" "wwww"))
    (?{  . "\\(?:{\\(?:|\\(?:|}\\|[|}]\\)\\|[|-]\\)\\)")                      ; (regexp-opt '("{-" "{|" "{||" "{|}" "{||}"))
    (?|  . "\\(?:|\\(?:->\\|=>\\||=\\|[]=>|}-]\\)\\)")                        ; (regexp-opt '("|=" "|>" "||" "||=" "|->" "|=>" "|]" "|}" "|-"))
    (?_  . "\\(?:_\\(?:|?_\\)\\)")                                            ; (regexp-opt '("_|_" "__"))
    (?\( . "\\(?:(\\*\\)")                                                    ; (regexp-opt '("(*"))
    (?~  . "\\(?:~\\(?:~>\\|[=>@~-]\\)\\)"))                                  ; (regexp-opt '("~-" "~=" "~>" "~@" "~~" "~~>"))
  "An alist of all ligatures used by `+ligatures-extras-in-modes'.

The car is the character ASCII number, cdr is a regex which will call
`font-shape-gstring' when matched.

Because of the underlying code in :ui ligatures module, the regex should match a
string starting with the character contained in car.

This variable is used only if you built Emacs with Harfbuzz on a version >= 28")

(defvar +ligatures-in-modes
  '(not special-mode comint-mode eshell-mode term-mode vterm-mode Info-mode
        elfeed-search-mode elfeed-show-mode)
  "List of major modes where ligatures should be enabled.

  If t, enable it everywhere (except `fundamental-mode').
  If the first element is 'not, enable it in any mode besides what is listed.
  If nil, don't enable ligatures anywhere.")

(defvar +ligatures-extras-in-modes t
  "List of major modes where extra ligatures should be enabled.

Extra ligatures are mode-specific substituions, defined in
`+ligatures-extra-symbols' and assigned with `set-ligatures!'. This variable
controls where these are enabled.

  If t, enable it everywhere (except `fundamental-mode').
  If the first element is 'not, enable it in any mode besides what is listed.
  If nil, don't enable these extra ligatures anywhere (though it's more
efficient to remove the `+extra' flag from the :ui ligatures module instead).")

(defvar +ligatures--init-font-hook nil)

(defun +ligatures--correct-symbol-bounds (ligature-alist)
  "Prepend non-breaking spaces to a ligature.

This way `compose-region' (called by `prettify-symbols-mode') will use the
correct width of the symbols instead of the width measured by `char-width'."
  (let ((len (length (car ligature-alist)))
        (acc (list   (cdr ligature-alist))))
    (while (> len 1)
      (setq acc (cons #X00a0 (cons '(Br . Bl) acc))
            len (1- len)))
    (cons (car ligature-alist) acc)))

(defun +ligatures--enable-p (modes)
  "Return t if ligatures should be enabled in this buffer depending on MODES."
  (unless (eq major-mode 'fundamental-mode)
    (or (eq modes t)
        (if (eq (car modes) 'not)
            (not (apply #'derived-mode-p (cdr modes)))
          (apply #'derived-mode-p modes)))))

(defun +ligatures-init-buffer-h ()
  "Set up ligatures for the current buffer.

Extra ligatures are mode-specific substituions, defined in
`+ligatures-extra-symbols', assigned with `set-ligatures!', and made possible
with `prettify-symbols-mode'. This variable controls where these are enabled.
See `+ligatures-extras-in-modes' to control what major modes this function can
and cannot run in."
  (when after-init-time
    (let ((in-mode-p
           (+ligatures--enable-p +ligatures-in-modes))
          (in-mode-extras-p
           (and (modulep! +extra)
                (+ligatures--enable-p +ligatures-extras-in-modes))))
      (when in-mode-p
        (if (boundp '+ligature--composition-table)
            (setq-local composition-function-table +ligature--composition-table)
          (run-hooks '+ligatures--init-font-hook)
          (setq +ligatures--init-font-hook nil)))
      (when in-mode-extras-p
        (prependq! prettify-symbols-alist
                   (alist-get major-mode +ligatures-extra-alist)))
      (when (and (or in-mode-p in-mode-extras-p)
                 prettify-symbols-alist)
        (when prettify-symbols-mode
          (prettify-symbols-mode -1))
        (prettify-symbols-mode +1)))))


;;
;;; Bootstrap

;;;###package prettify-symbols
;; When you get to the right edge, it goes back to how it normally prints
(setq prettify-symbols-unprettify-at-point 'right-edge)

;;   ;; (add-hook! 'bonk-init-ui-hook :append
;;   ;;   (defun +ligatures-init-h ()
;;   ;;     (add-hook 'after-change-major-mode-hook #'+ligatures-init-buffer-h)))

;; (cond
;;  ;; The emacs-mac build of Emacs appears to have built-in support for ligatures,
;;  ;; using the same composition-function-table method
;;  ;; https://bitbucket.org/mituharu/emacs-mac/src/26c8fd9920db9d34ae8f78bceaec714230824dac/lisp/term/mac-win.el?at=master#lines-345:805
;;  ;; so use that instead if this module is enabled.
;;  ((and IS-MAC (fboundp 'mac-auto-operator-composition-mode))
;;   (add-hook 'doom-init-ui-hook #'mac-auto-operator-composition-mode 'append))

;;  ;; Harfbuzz and Mac builds do not need font-specific ligature support
;;  ;; if they are above emacs-27.
;;  ((and (> emacs-major-version 27)
;;        (or (featurep 'ns)
;;            (string-match-p "HARFBUZZ" system-configuration-features))
;;        (featurep 'composite))  ; Emacs loads `composite' at startup
;;   (defvar +ligature--composition-table (make-char-table nil))
;;   (add-hook! 'doom-init-ui-hook :append
;;     (defun +ligature-init-composition-table-h ()
;;       (dolist (char-regexp +ligatures-composition-alist)
;;         (set-char-table-range
;;          +ligature--composition-table
;;          (car char-regexp) `([,(cdr char-regexp) 0 font-shape-gstring])))
;;       (set-char-table-parent +ligature--composition-table composition-function-table))))

;;  ;; Fallback ligature support for certain, patched fonts. Install them with
;;  ;; `+ligatures/install-patched-font'
;;  ((defmacro +ligatures--def-font (id font-plist &rest alist)
;;     (declare (indent 2))
;;     (let ((alist-var (intern (format "+ligatures-%s-font-alist" id)))
;;           (setup-fn  (intern (format "+ligatures-init-%s-font-h" id))))
;;       `(progn
;;          (setf (alist-get ',id +ligatures--font-alist) (list ,@font-plist))
;;          (defvar ,alist-var ',alist ,(format "Name of the %s ligature font." id))
;;          (defun ,setup-fn (&rest _)
;;            (cl-destructuring-bind (name &key _url files range)
;;                (or (alist-get ',id +ligatures--font-alist)
;;                    (error "No ligature font called %s" ',id))
;;              (when range
;;                (set-fontset-font t range name nil 'prepend))
;;              (setq-default prettify-symbols-alist
;;                            (append (default-value 'prettify-symbols-alist)
;;                                    (mapcar #'+ligatures--correct-symbol-bounds ,alist-var)))))
;;          (add-hook '+ligatures--init-font-hook #',setup-fn))))

;;   (defvar +ligatures--font-alist ())

;;   (cond ((modulep! +fira)         (load! "+fira"))
;;         ((modulep! +iosevka)      (load! "+iosevka"))
;;         ((modulep! +hasklig)      (load! "+hasklig"))
;;         ((modulep! +pragmata-pro) (load! "+pragmata-pro")))))
