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
      :g  "C-\"" 'recompile
      :g "C-!" 'kill-compilation
      :i "C-¡" "::"
      :i "C-@" "->"
      :i "C-£" "<-"
      :g "C-;" 'iedit-mode
      :g "<f12>" 'eval-expression
      :g "C-l" 'ivy-switch-buffer
      :g "C-q" '+ivy/switch-workspace-buffer
      :g "C-_" '+popup/toggle
      )

(after! company
  (map! :map company-active-map
        "<C-return>" #'company-complete-selection
        "<return>" nil
        "RET" nil
        "C-SPC" nil)
  (setq company-minimum-prefix-length 1))

(provide 'keybinds)
