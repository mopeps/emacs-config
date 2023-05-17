;; Backup and Autosave Directories
  (setq temporary-file-directory "~/.tmp/emacs/")
  (setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))
  (setq backup-directory-alist            '((".*" . "~/.Trash")))

(setup (:pkg all-the-icons-dired :straight t))
  (setup (:pkg dired-single :straight t))
  (setup (:pkg dired-ranger :straight t))
  (setup (:pkg dired-collapse :straight t)
	)

  (setup dired
	(setq dired-listing-switches "-agho --group-directories-first"
		  dired-omit-files "^\\.[^.].*"
		  dired-omit-verbose nil
		  dired-hide-details-hide-symlink-targets nil
		  delete-by-moving-to-trash t)

	;;(autoload 'dired-omit-mode "dired-x")

	(add-hook 'dired-load-hook
			  (lambda ()
				(interactive)
				(dired-collapse)))

	(add-hook 'dired-mode-hook
			  (lambda ()
				(interactive)
				(dired-hide-details-mode 1)
				(all-the-icons-dired-mode 1)
				(hl-line-mode 1)))

	(evil-collection-define-key 'normal 'dired-mode-map
	  "h" 'dired-single-up-directory
	  "H" 'dired-omit-mode
	  "l" 'dired-single-buffer
	  "y" 'dired-ranger-copy
	  "X" 'dired-ranger-move
	  "p" 'dired-ranger-paste))

  (setup (:pkg dired-rainbow :straight t)
	(:load-after dired
	  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
	  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
	  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
	  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
	  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
	  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
	  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
	  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
	  (dired-rainbow-define log "#c17d11" ("log"))
	  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
	  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
	  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
	  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
	  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
	  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
	  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
	  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
	  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
	  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
	  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))

  (eval-when-compile (require 'cl))
  (defun bonk/dired-link (path)
	(lexical-let ((target path))
	  (lambda () (interactive) (message "Path: %s" target) (dired target))))
  (bonk/leader-keys
	"d"   '(:ignore t :which-key "dired")
	"dd"  '(dired :which-key "Here")
	"dh"  `(,(bonk/dired-link "~/") :which-key "Home")
	"dn"  `(,(bonk/dired-link "~/Notes") :which-key "Notes")
	"dw"  `(,(bonk/dired-link "~/working") :which-key "Working")
	"dp"  `(,(bonk/dired-link "~/Protos-prog") :which-key "Working")
	"dg"  `(,(bonk/dired-link "~/github") :which-key "Github")
	"do"  `(,(bonk/dired-link "~/Downloads") :which-key "Downloads")
	"dv"  `(,(bonk/dired-link "~/Videos") :which-key "Videos")
	"d."  `(,(bonk/dired-link "~/.config") :which-key "dotfiles-config")
	"dl"  `(,(bonk/dired-link "~/.local") :which-key "dotfiles-local")
	"de"  `(,(bonk/dired-link "~/.emacs.d") :which-key ".emacs.d"))

(setq dired-listing-switches "-al --group-directories-first")

(setup (:pkg image+ :straight t))
(setup (:pkg image-dired+ :straight t))
(setup (:pkg ranger :straight t)
  :options
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-on-disable t)
  (setq ranger-show-hidden nil)
  (setq helm-descbinds-window-style 'same-window)
  (setq ranger-footer-delay 0.2)
  (setq ranger-preview-delay 0.150)
  (setq ranger-preview-file nil)
  (setq ranger-show-literal nil)
  (setq ranger-width-preview 0.55)
  (setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
  (setq ranger-max-preview-size 5)
  (setq ranger-dont-show-binary t))

(bonk/leader-keys
  "r"  '(ranger :which-key "Ranger Current Dir"))

(setup (:pkg projectile :straight t)
  (:global "C-c p" projectile-command-map)
  (projectile-mode)
    (setq projectile-project-search-path '("~/." "~/github" "~/working"))
  (setq projectile-switch-project-action #'projectile-dired))

(setup (:pkg counsel-projectile :straight t)
  (counsel-projectile-mode))

(provide 'bonk-project-explorer)
