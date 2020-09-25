;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sam Hedin"
      user-mail-address "sam.hedin@gmail.com")

;; EXWM
;;
;; (start-process-shell-command
;;  "keyboard" nil "setxkbmap se dvorak_ep")
;; (start-process-shell-command
;;  "xset" nil "xset r rate 200 50")

;; (require 'exwm)
;; (require 'exwm-config)
;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)

;; (setq exwm-input-global-keys
;;       `(([?\s-r] . exwm-reset)
;;         ([?\s-c] . exwm-input-release-keyboard)
;;         ([?\s-t] . counsel-M-x)
;;         ([?\s-d] . (lambda (command)
;;                      (interactive (list (read-shell-command "$ ")))
;;                      (start-process-shell-command command nil command)))
;;         ([?\s-w] . exwm-workspace-switch)
;;         ,@(mapcar (lambda (i)
;;                     `(,(kbd (format "s-%d" i)) .
;;                       (lambda ()
;;                         (interactive)
;;                         (exwm-workspace-switch-create ,i))))
;;                   (number-sequence 0 9))))

;; (setq exwm-input-simulation-keys
;;       '(([?\C-b] . [left])
;;         ([?\C-f] . [right])
;;         ([?\C-p] . [up])
;;         ([?\C-n] . [down])
;;         ([?\C-a] . [home])
;;         ([?\C-e] . [end])
;;         ([?\M-v] . [prior])
;;         ([?\C-v] . [next])
;;         ([?\C-d] . [delete])
;;         ([?\C-k] . [S-end delete])))


;; (require 'exwm-randr)
;; (setq exwm-randr-workspace-output-plist '(1 "DisplayPort-1"
;;                                             2 "HDMI-A-0"))
;; (add-hook 'exwm-randr-screen-change-hook
;;           (lambda ()
;;             (start-process-shell-command
;;              "xrandr" nil "xrandr --output DisplayPort-1 --mode 1920x1080 --rate 120 --output HDMI-A-0 --mode 1920x1080 --right-of DisplayPort-1")))
;; (exwm-randr-enable)
;; (exwm-enable)

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
      doom-variable-pitch-font (font-spec :family "sans" :size 11.0))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-Iosvkem)

;; https://www.reddit.com/r/emacs/comments/ilujry/doomthemes_miramare_oldhope_flatwhite/
(let ((time  (string-to-number (format-time-string "%H"))))
  (if (or (< time 7) (> time 17))
      (load-theme 'doom-Iosvkem t)
    (load-theme 'doom-one-light t)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)
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

(after! treemacs
  (set-face-attribute 'treemacs-root-face nil :height 1.0  :underline nil)
  (setq treemacs-width 25)
  (treemacs-resize-icons 15))

(remove-hook! (prog-mode text-mode conf-mode special-mode) 'hl-line-mode)

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))

(setq-default line-spacing 8)
(global-paren-face-mode)
(setq-default paren-face-regexp "[][(){}]")
(setq-default paren-face-modes (append '(rustic-mode org-mode python-mode) paren-face-modes))

(add-hook 'prog-mode-hook 'highlight-parentheses-mode)
(advice-add #'rainbow-delimiters-mode :override #'ignore)

(defun sneaky-save-buffer (&rest _r)
  (unless (string-match-p (regexp-quote "*") (buffer-name))
    (save-buffer)))

(advice-add 'grep :before #'sneaky-save-buffer)
(advice-add 'winum-select-window-by-number :before #'sneaky-save-buffer)
(advice-add 'evil-switch-to-windows-last-buffer :before #'sneaky-save-buffer)
(advice-add 'magit-status :before #'sneaky-save-buffer)
(advice-add 'projectile-compile-project :before #'sneaky-save-buffer)
(advice-add 'recompile :before #'sneaky-save-buffer)
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)
            (dired-sort-toggle-or-edit)))
(setq projectile-enable-caching nil)

;; (use-package lsp-haskell
;;   :config
;;   (setq lsp-haskell-process-path-hie "/home/sam/.local/bin/haskell-language-server-wrapper"))
(setenv "PATH" (concat (getenv "PATH") ":" "/home/sam/.local/bin/"))
(setq exec-path (append exec-path '("/home/sam/.local/bin")))

(setq rm-blacklist "")
(rich-minority-mode)
(mini-modeline-mode)

;; Did pdf-tools break? Try
;; (pdf-tools-install)
(after! lsp
  (setq lsp-signature-render-documentation nil))

(after! pdf-view
  (setq pdf-view-midnight-colors '("#D9D2C9" . "#2b2b2F")))

(add-hook! 'org-mode-hook
  (setq-default fill-column 90)
  (auto-fill-mode)
  (setq org-latex-pdf-process '("latexmk -pdf -outdir=%o %f")))


(after! pdf-view
  (setq pdf-view-resize-factor 1.10)
  (setq lazy-highlight-cleanup nil))
(ivy-prescient-mode)
