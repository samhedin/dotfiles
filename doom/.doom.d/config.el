;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Sam Yousefzadegan Hedin"
      user-mail-address "sam.hedin@gmail.com")

(setq auth-sources '("/home/sam/.authinfo"))

(setq doom-font (font-spec :family "Fira Code" :size 13.0 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "sans" :size 13.0))

(setq display-line-numbers-type nil)
(setq confirm-kill-emacs nil)
(setq which-key-idle-delay 0.001)

(defun sneaky-save-buffer (&rest _r)
  (when (buffer-file-name)
    (save-buffer)))

(dolist (f '(grep winum-select-window-by-number evil-switch-to-windows-last-buffer magit-status projectile-compile-project recompile TeX-command-run-all +term/toggle julia-snail-send-buffer-file))
  (advice-add f :before #'sneaky-save-buffer))

(add-hook 'focus-out-hook 'sneaky-save-buffer)

(use-package eww
  :defer t
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface))

(use-package shr-tag-pre-highlight
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
	       '(pre . shr-tag-pre-highlight))
  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
		  'eww-display-html--override-shr-external-rendering-functions))))

(global-hl-line-mode -1)

(setq doom-theme 'doom-Iosvkem)
(let ((time  (string-to-number (format-time-string "%H"))))
  (if (or (< time 8) (> time 18))
      (load-theme 'doom-spacegrey t)
    (load-theme 'doom-one-light t)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(setq org-roam-directory "~/git/privat/roam")
(setq org-directory "~/git/privat/roam/")

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

(setq-default line-spacing 12)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)
(advice-add #'rainbow-delimiters-mode :override #'ignore)

(setq projectile-enable-caching nil)

(after! treemacs
  (set-face-attribute 'treemacs-root-face nil :height 1.0  :underline nil)
  (setq treemacs-width 50)
  (treemacs-resize-icons 15))

(add-hook 'prog-mode-hook (lambda () (hl-line-mode -1)))

(after! centaur-tabs
  (setq centaur-tabs-set-close-button nil))

(setq org-startup-with-latex-preview t)
(add-hook 'julia-mode-hook 'julia-snail-mode)
;; (add-hook 'org-mode-hook 'LaTex-math-mode)
(use-package! org
  :config
  (setq-default fill-column 120)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
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
     (ess-julia .t)
     (latex . t)
     (jupyter . t)
     (rust . t)
     (sh . t)))

  (setq org-latex-minted-options '(("breaklines" "true")
                                   ("breakanywhere" "true")))
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
	  "bibtex %b"
	  "pdflatex -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -interaction nonstopmode -output-directory %o %f")))

(use-package! org-ref
  :after-call org-mode-hook)

;;  Install dash docsets with these functions.
;;(dolist (f '("Julia" "Python_3" "NumPy" "SciPy" "Unity_3D"))
;;(dash-docs-install-docset f))

;; (dolist (f '("scikit-learn" "PyTorch" "TensorFlow 2"))
;;   (dash-docs-install-user-docset f))

(setq dash-docs-enable-debugging nil)

(set-docsets! '(csharp-mode) "Unity 3D")
(set-docsets! '(python-mode)"Python 3" "NumPy" "SciPy" "scikit-learn")
(set-docsets! '(haskell-mode) "Haskell")
(set-docsets! '(julia-mode) "Julia")

(add-hook 'help-mode-hook (lambda ()
                            (visual-line-mode)))
(add-hook 'julia-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("Julia"))))
(add-hook 'haskell-mode-hook
          (lambda () (setq lsp-haskell-server-path "/run/current-system/sw/bin/haskell-language-server")))


(use-package! lsp-mode
  :config
  (setq lsp-enable-folding t)             ; Needed for julia atm
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-enable-file-watchers t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-render-documentation t))


(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))
;; (setq rustic-compile-display-method 'display-buffer-other-frame)

(global-paren-face-mode)                ; Not working? Try customizing the `shadow` face.
(setq-default paren-face-regexp "[][(){};]")
(setq-default paren-face-modes (append '(rustic-mode org-mode python-mode csharp-mode) paren-face-modes))

;; Did pdf-tools break? Try
;; (pdf-tools-install)

(after! pdf-view
  (setq pdf-view-resize-factor 1.10)
  (setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) . ,(face-attribute 'default :background))))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-details-mode)
	    (dired-sort-toggle-or-edit)))

(setq rm-blacklist "")
(rich-minority-mode)
(mini-modeline-mode)
(setq max-mini-window-height 0.05)
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'csharp-mode-hook 'remove-dos-eol)

(setq large-file-warning-threshold 100000000)

(setq ein:output-area-inlined-images t)


(setq browse-url-browser-function 'eww)

(setq lsp-csharp-server-path "/run/current-system/sw/bin/omnisharp")
