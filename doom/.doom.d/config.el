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
;; (setq doom-theme 'kaolin-light)

(let ((time  (string-to-number (format-time-string "%H"))))
  (if (or (< time 5) (> time 19))
      (load-theme 'kaolin-temple t)
    (load-theme 'kaolin-light t)))

(map! "<f5>"
      (lambda ()
        (interactive)
        (if (eq doom-theme 'kaolin-light)
            (load-theme 'kaolin-temple)
          (load-theme 'kaolin-light))))

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
(setq confirm-kill-emacs nil)

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

(setq which-key-idle-delay 0.1)
(map! :leader "C-t" 'counsel-M-x)

(map! :map evil-normal-state-map "C-." nil)
(map! :map evil-insert-state-map "C-." nil)
(map! :g "C-." 'evil-avy-goto-char)

(map! :map evil-normal-state-map "C-<tab>" nil)
(map! :map evil-normal-state-map "q" nil)
(map! :map evil-insert-state-map "C-<tab>" nil)
(map! :map evil-visual-state-map "C-<tab>" nil)
(map! :g "C-<tab>" 'evil-switch-to-windows-last-buffer)
(map! :g  "C-\"" 'recompile)
(map! :g  "C-!" 'kill-compilation)


(map! :i "C-¡" "::"
      :i "C-@" "->"
      :i "C-£" "<-"
      :i "C-e" 'move-end-of-line
      :i "C-n" 'forward-char)

(after! treemacs
  (set-face-attribute 'treemacs-root-face nil :height 1.0  :underline nil)
  (setq treemacs-width 30)
  (treemacs-resize-icons 15))

(map! :map undo-fu-mode-map "C-_" nil)
(map! :g "C-_" '+popup/toggle)
(after! company
  (map! :map company-active-map
        "C-<return>" #'company-complete-selection
        "<return>" nil
        "RET" nil
        "C-SPC" nil))
(map! :g "<f12>" 'eval-expression)
(map! :leader "!" 'shell-command)
(map! :g "C-l" 'switch-to-buffer)

(remove-hook! (prog-mode text-mode conf-mode special-mode) 'hl-line-mode)


(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))

(setq-default line-spacing 8)
(global-paren-face-mode)
(setq-default paren-face-regexp "[][(){}]")
(setq-default paren-face-modes (append '(rustic-mode org-mode) paren-face-modes))

(add-hook 'prog-mode-hook 'highlight-parentheses-mode)
(advice-add #'rainbow-delimiters-mode :override #'ignore)

(defun sneaky-save-buffer (&rest _r)
  (save-buffer))
(advice-add 'magit-status :before #'sneaky-save-buffer)
(advice-add 'projectile-compile-project :before #'sneaky-save-buffer)
(advice-add 'recompile :before #'sneaky-save-buffer)
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)
            (dired-sort-toggle-or-edit)))

(use-package lsp-haskell
  :ensure t
  :config
  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
  ;; Comment/uncomment this line to see interactions between lsp client/server.
  ;;(setq lsp-log-io t)
  )

(setq rm-blacklist "")
(rich-minority-mode)
(mini-modeline-mode)
