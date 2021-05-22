;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Sam Yousefzadegan Hedin"
      user-mail-address "sam.hedin@gmail.com")

(setq auth-sources '("/home/sam/.authinfo"))

(setq doom-font (font-spec :family "Fira Code" :size 13.5 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "sans" :size 13.5))

(setq display-line-numbers-type nil)
(setq confirm-kill-emacs nil)
(setq which-key-idle-delay 0.001)

(defun sneaky-save-buffer (&rest _r)
  (when (buffer-file-name)
    (save-buffer)))

(dolist (f '(grep winum-select-window-by-number evil-switch-to-windows-last-buffer magit-status projectile-compile-project recompile TeX-command-run-all +term/toggle julia-snail-send-buffer-file +workspace/switch-to))
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
  (if (or (< time 7) (> time 19))
      (load-theme 'modus-vivendi t)
    (load-theme 'modus-operandi t)))

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

(setq-default line-spacing 9)
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
;;
;; Use this for non breaking space, then insert with C-x 8 <space>
(use-package! org
  :config
  (setq-default fill-column 120)
  (org-toggle-link-display)
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
     (emacs-lisp .t)))

  (setq org-latex-minted-options '(("breaklines" "true")
                                   ("breakanywhere" "true")))
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
	  "bibtex %b"
	  "pdflatex -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -interaction nonstopmode -output-directory %o %f")))

(use-package! org-ref
  :after-call org-mode-hook)

;; REGION BEGIN
;; blablah
;; REGION END

;; (after! python
;;   (defun python-send-delimiters ()
;;     (interactive)
;;     (let ((beg nil) (end nil))
;;       (save-excursion
;;         (re-search-backward (rx "# section begin"))
;;         (setq beg (+ (length "# section begin")  (point)))
;;         (forward-char)
;;         (re-search-forward (rx "# section end"))
;;         (setq end (- (point) 1)))
;;       (python-shell-send-string (buffer-substring-no-properties beg end)))))

;;  Install dash docsets with these functions.
;; (dolist (f '("Julia" "Python_3" "NumPy" "Haskell"))
;; (dash-docs-install-docset f))
;; (dolist (f '("scikit-learn" "PyTorch"))
;;   (dash-docs-install-user-docset f))

(setq dash-docs-enable-debugging nil)

(set-docsets! '(python-mode)"Python 3" "NumPy" "scikit-learn" "PyTorch")
(set-docsets! '(haskell-mode) "Haskell")
(set-docsets! '(julia-mode) "Julia")

(add-hook 'help-mode-hook (lambda ()
                            (visual-line-mode)))
(add-hook 'haskell-mode-hook
          (lambda () (setq lsp-haskell-server-path "/run/current-system/sw/bin/haskell-language-server")))


(use-package! lsp-mode
  :config
  (setq lsp-enable-folding t)           ; Needed for julia atm
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-enable-file-watchers t)
  (setq lsp-signature-auto-activate t)
  (setq dap-python-debugger 'debugpy)
  (setq lsp-signature-render-documentation t))
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))
(global-paren-face-mode)                ; Not working? Try customizing the `shadow` face.
(setq-default paren-face-regexp "[][(){};]")
(setq-default paren-face-modes (append '(rustic-mode org-mode python-mode) paren-face-modes))


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
(setq max-mini-window-height 0.01)
(setq large-file-warning-threshold 100000000)
(setq browse-url-browser-function 'eww)

;; https://gist.github.com/yorickvP/6132f237fbc289a45c808d8d75e0e1fb
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
				      :buffer nil
				      :command '("wl-copy" "-f" "-n")
				      :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))
;; (setq interprogram-cut-function 'wl-copy)
;; (setq interprogram-paste-function 'wl-paste)
(setq ein:output-area-inlined-images t)
(setq magit-repository-directories '(("/home/sam/git/" . 2)))
