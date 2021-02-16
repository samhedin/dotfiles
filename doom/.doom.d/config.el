;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Sam Yousefzadegan Hedin"
      user-mail-address "sam.hedin@gmail.com")

(setq auth-sources '("/home/sam/.authinfo"))

(setq doom-font (font-spec :family "Fira Code" :size 13.0 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "sans" :size 13.0))
;; (setq doom-font (font-spec :family "Fira Code" :size 19.5 :weight 'normal)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 19.5))

(setq display-line-numbers-type nil)
(setq confirm-kill-emacs nil)
(setq which-key-idle-delay 0.001)

(defun sneaky-save-buffer (&rest _r)
  (unless (or  (string-match-p (regexp-quote "*") (buffer-name))
               (string-match-p (regexp-quote "pdf") (buffer-name)))
    (save-buffer)))

(dolist (f '(grep winum-select-window-by-number evil-switch-to-windows-last-buffer magit-status projectile-compile-project recompile))
  (advice-add f :before #'sneaky-save-buffer))

(add-hook 'focus-out-hook 'sneaky-save-buffer)

(setq doom-theme 'doom-Iosvkem)
(let ((time  (string-to-number (format-time-string "%H"))))
  (if (or (< time 8) (> time 17))
      (load-theme 'doom-one t)
    (load-theme 'doom-one-light t)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(defun copy-current-file-path ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun pdfgrep ()
  (interactive)
  (grep (format "pdfgrep --ignore-case --recursive --page-number '%s' ." (read-string "Enter term: "))))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(load! "keybinds.el")
(keybinds-mode)

(global-hl-line-mode 0)

(setq-default line-spacing 8)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)
(advice-add #'rainbow-delimiters-mode :override #'ignore)

(setq projectile-enable-caching nil)

(after! treemacs
  (set-face-attribute 'treemacs-root-face nil :height 1.0  :underline nil)
  (setq treemacs-width 50)
  (treemacs-resize-icons 15))

(after! centaur-tabs
  (setq centaur-tabs-set-close-button nil))

(setq org-startup-with-latex-preview t)

;; (add-hook 'org-mode-hook 'LaTex-math-mode)

(after! org
  (setq-default fill-column 120)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (auto-fill-mode)
  (set-face-attribute 'org-block-begin-line nil :inherit 'org-block :height 0.8 :background nil)
  (set-face-attribute 'org-block-end-line nil :inherit 'org-block :height 0.8 :background nil)
  (set-face-attribute 'org-meta-line nil :height 0.8 :background nil)
  (setq org-latex-toc-command "\\tableofcontents \\clearpage")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (ein .t)
     (emacs-lisp .t)
     (julia . t)
     (latex . t)
     (jupyter . t)
     (rust . t)
     (sh . t)))

  (setq org-latex-minted-options '(("breaklines" "true")
                                   ("breakanywhere" "true")))


  ;; (setq org-latex-listings 'minted
  ;;       org-latex-packages-alist '(("" "minted"))
  ;;       org-latex-pdf-process
  ;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  )



;;  Install dash docsets with these functions.
                                        ;(dolist (f '("Julia" "Python_3" "NumPy" "SciPy" "Mono" "Pandas"))
                                        ; (dash-docs-install-docset f))

;; (dolist (f '("scikit-learn" "PyTorch" "TensorFlow 2"))
;;   (dash-docs-install-user-docset f))

(add-hook 'csharp-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("Mono" "Unity 3D"))))

(add-hook 'python-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("Python 3" "NumPy" "SciPy" "scikit-learn" "TensorFlow 2" "Pandas"))))

(add-hook 'julia-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("Julia"))))
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("Julia"))))

(after! lsp
  (setq lsp-signature-render-documentation nil))

(setq lsp-csharp-server-path "/run/current-system/sw/bin/omnisharp")

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))
;; (setq rustic-compile-display-method 'display-buffer-other-frame)

(setq lsp-julia-default-environment "~/.julia/environments/v1.5")

(global-paren-face-mode)                ; Not working? Try customizing the `shadow` face.
(setq-default paren-face-regexp "[][(){};]")
(setq-default paren-face-modes (append '(rustic-mode org-mode python-mode csharp-mode) paren-face-modes))

;; Did pdf-tools break? Try
;; (pdf-tools-install)

(after! pdf-view
  (setq pdf-view-resize-factor 1.10)
  (setq pdf-view-midnight-colors '("#dddddd" . "#262829")))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-details-mode)
	    (dired-sort-toggle-or-edit)))

(setq rm-blacklist "")
(rich-minority-mode)
(mini-modeline-mode)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'csharp-mode-hook 'remove-dos-eol)

(setq large-file-warning-threshold 100000000)

 (setq ein:output-area-inlined-images t)

(defun my-preview-latex ()
  "Preview LaTeX from the current cell in a separate buffer.

Handles only markdown and code cells, but both in a bit different
ways: on the former, its input is being rendered, while on the
latter - its output."
  (interactive)
  (let* ((cell (ein:worksheet-get-current-cell))
	 (text-to-render
	  (cond ((ein:markdowncell-p cell) (slot-value cell :input))
		((ein:codecell-p cell)
		 (plist-get (car (cl-remove-if-not
				  (lambda (e) (string= (plist-get e :name) "stdout"))
				  (slot-value cell :outputs)))
			    :text))
		(t (error "Unsupported cell type"))))
	 (buffer (get-buffer-create " *ein: LaTeX preview*")))
    (with-current-buffer buffer
      (when buffer-read-only
	(toggle-read-only))
      (unless (= (point-min) (point-max))
	(delete-region (point-min) (point-max)))
      (insert text-to-render)
      (goto-char (point-min))
      (org-mode)
      (org-toggle-latex-fragment 10)
      (special-mode)
      (unless buffer-read-only
	(toggle-read-only))
      (display-buffer
       buffer
       '((display-buffer-below-selected display-buffer-at-bottom)
         (inhibit-same-window . t)))
      (fit-window-to-buffer (window-in-direction 'below)))))
(setq dash-docs-browser-func 'eww)
