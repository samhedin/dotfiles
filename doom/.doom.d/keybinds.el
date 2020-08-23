;;; ../dotfiles/doom/.doom.d/keybinds.el -*- lexical-binding: t; -*-
(defvar keybinds-mode-map (make-sparse-keymap)
  "Keymap for `my-mode'.")

(map! :map keybinds-mode-map
      :g "C-1" 'winum-select-window-1
      :g "C-2" 'winum-select-window-2
      :g "C-3" 'winum-select-window-3
      :g "C-4" 'winum-select-window-4
      :g "C-5" 'winum-select-window-5
:g "C-<tab>" 'evil-switch-to-windows-last-buffer
:g  "C-\"" 'recompile
"C-!" 'kill-compilation
      :i "C-¡" "::"
      :i "C-@" "->"
      :i "C-£" "<-"
      :i "C-e" 'move-end-of-line
      :i "C-n" 'forward-char
      :g "C-." 'evil-avy-goto-char
      :g "<f12>" 'eval-expression
      :leader "!" 'shell-command
      :leader "C-t" 'counsel-M-x
      :g "C-l" 'switch-to-buffer
      :g "C-_" '+popup/toggle)

(map! :map evil-normal-state-map
      "C-." nil
      "C-<tab>" nil
      "q" nil
      )

(map! :map undo-fu-mode-map "C-_" nil)

(after! company
  (map! :map company-active-map
        "C-<return>" #'company-complete-selection
        "<return>" nil
        "RET" nil
        "C-SPC" nil))

(define-key keybinds-mode-map (kbd "C-SPC") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-SPC") nil)


;;;###autoload
(define-minor-mode keybinds-mode
  "Keybinds"
  :lighter " keybinds"
  :init-value t
  :keymap keybinds-mode-map)

;;;###autoload
(define-globalized-minor-mode global-keybinds-mode keybinds-mode keybinds-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((keybinds-mode . ,keybinds-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-my-mode ()
  "Turn off my-mode."
  (keybinds-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-my-mode)

(provide 'keybinds-mode)
