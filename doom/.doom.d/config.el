;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Source Code Pro" :size 11.5 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'kaolin-temple)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(setq-default doom-modeline-height 5)
(setq-default doom-modeline--vcs-icon nil)
(setq-default doom-modeline-buffer-encoding nil)
(setq-default doom-modeline--vcs-text nil)
(setq-default doom-modeline-major-mode-icon nil)
(setq-default doom-modeline-percent-position nil)

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

(setq which-key-idle-delay 0.1)
(map! :leader "C-t" 'counsel-M-x)
(map! :g "C-1" 'winum-select-window-1)
(map! :g "C-2" 'winum-select-window-2)
(map! :g "C-3" 'winum-select-window-3)
(map! :g "C-4" 'winum-select-window-4)
(map! :g "C-5" 'winum-select-window-5)
(map! :i "C-SPC" 'evil-normal-state)

(map! :map evil-normal-state-map "C-<tab>" nil)
(map! :map evil-visual-state-map "C-<tab>" nil)
(map! :g "C-<tab>" 'evil-switch-to-windows-last-buffer)

(map! :i "C-¡" "::")
(map! :i "C-@" "->")
(map! :i "C-£" "<-")
(map! :i "C-e" 'move-end-of-line)
(map! :i "C-n" 'forward-char)

(map! "<f5>"
      (lambda ()
        (interactive)
        (if (eq doom-theme 'kaolin-light)
        (load-theme 'kaolin-temple)
        (load-theme 'kaolin-light))))

(map! :map lispy-mode-map-c-digits
      :g "C-1" nil
      :g "C-2" nil
      :niv "C-3" nil
      :niv "C-4" nil
      :niv "C-5" nil
      :niv "C-6" nil
      )
(map! :map lispy-mode-map "C-," nil)
(map! :map lispy-mode-map-lispy "C-," nil)

(after! treemacs
  (set-face-attribute 'treemacs-root-face nil :height 1.0  :underline nil)
  (setq treemacs-width 30)
  (treemacs-resize-icons 15))

(after! company
  (define-key company-active-map (kbd "C-<return>") #'company-complete-selection)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "C-SPC") nil))
(map! :g "<f12>" 'eval-expression)

(remove-hook! (prog-mode text-mode conf-mode special-mode) 'hl-line-mode)

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))

(setq-default line-spacing 8)
(global-paren-face-mode)
(setq-default paren-face-regexp "[][(){}]")
(setq-default paren-face-modes (append '(rustic-mode org-mode) paren-face-modes))

(advice-add #'rainbow-delimiters-mode :override #'ignore)
