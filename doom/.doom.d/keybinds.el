;;; ../dotfiles/doom/.doom.d/keybinds.el -*- lexical-binding: t; -*-
(defvar keybinds-mode-map (make-sparse-keymap)
  "Keymap for `my-mode'.")

;;;###autoload
(define-minor-mode keybinds-mode
  "Keybinds"
  :lighter " keybinds"
  :global t
  :init-value t
  :keymap keybinds-mode-map)

;;;###autoload
(define-globalized-minor-mode global-keybinds-mode keybinds-mode keybinds-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((keybinds-mode . ,keybinds-mode-map)))
(map! :map keybinds-mode-map
      :g "C-1" 'winum-select-window-1
      :g "C-2" 'winum-select-window-2
      :g "C-3" 'winum-select-window-3
      :g "C-4" 'winum-select-window-4
      :g "C-5" 'winum-select-window-5
      :g "C-!" 'kill-compilation
      :g "C-;" 'iedit-mode
      :g "<C-tab>" 'evil-switch-to-windows-last-buffer
      :i "C-f" 'forward-char
      :g "C-." 'evil-avy-goto-char
      :g "<f12>" 'eval-expression
      :g "C-l" 'ivy-switch-buffer
      :g "C-q" '+ivy/switch-workspace-buffer
      :g "C-_" '+popup/toggle
      :n ">" 'sp-slurp-hybrid-sexp
      :n "<" 'sp-forward-barf-sexp
      :g "<right>" 'evil-window-right
      :g "<left>" 'evil-window-left
      :g "<down>" 'doom/escape
      :leader
      "k" #'browse-kill-ring
      "r" #'recompile
      "!" 'shell-command
      "l" 'lsp-describe-thing-at-point
      "<up>" 'counsel-M-x)

(define-key evil-insert-state-map (kbd "C-SPC") 'evil-normal-state)
(map! :map evil-motion-state-map
      "<right>" nil
      "<left>" nil)

(map! :map special-mode-map
      "h" nil)
(map! :map evil-normal-state-map
      ">" nil
      "<" nil
      "C-." nil
      "<C-tab>" nil
      "q" nil)

(after! org
  (map! :map org-mode-map
        "C-c C-p" #'org-latex-export-to-pdf))

(after! julia-snail
  (map! :map keybinds-mode-map
        :n "TAB" nil
        "TAB" nil)
  (map! :map julia-snail-mode-map
        :n "<return>" #'julia-snail-send-dwim
        "TAB" nil
        :n "TAB" nil))

(after! company
  (map! :map company-active-map
        "<C-return>" #'company-complete-selection
        "<return>" nil
        "RET" nil
        "C-SPC" nil)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.01))

(after! ein
  (map! :map ein:notebook-mode-map
        :n "<return>" 'ein:worksheet-execute-cell-km
        :n "<left>" #'ein:worksheet-insert-cell-above
        :n "<right>" #'ein:worksheet-insert-cell-below-km)
  (map! :leader
        (:prefix ("e" . "ein notebooks")
         "a" #'ein:worksheet-insert-cell-above-km
         "b" #'ein:worksheet-insert-cell-below-km
         "d" #'ein:worksheet-delete-cell)))


(defcustom change-insert-keybindings '()
  "Alist of keys to strings."
  :type '(alist :key-type key-sequence :value-type string)
  :set (lambda (sym new)
	 (set-default sym new)
	 (dolist (kv change-insert-keybindings)
	   (map! :map keybinds-mode-map :i (car kv)
		 (lambda ()
		   (interactive)
		   (insert (cdr kv)))))))

(defun change-insert (key text)
  "Bind KEY to (insert TEXT)."
  (interactive "KWhat keybind do you wish to change? \nMWhat text should it insert? ")
  (when (and (interactive-p)
	     (y-or-n-p (format "Are you sure you want [%s  \"%s\"]? "
			       (key-description key)
			       text)))
    (setf (alist-get key change-insert-keybindings nil nil #'equal) text)
    (customize-save-variable 'change-insert-keybindings change-insert-keybindings)))

(use-package! lispyville
  :init
  (setq lispyville-key-theme
        '((operators normal)
          c-w
          text-objects
          additional-motions
          operators
          (prettify insert)
          (atom-movement normal visual)
          slurp/barf-lispy
          additional
          additional-insert)))

(provide 'keybinds)
