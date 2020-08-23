;;; ../dotfiles/doom/.doom.d/keybinds.el -*- lexical-binding: t; -*-

;;;###autoload
(define-minor-mode keybinds-mode
  "Keybinds"
  :lighter " keybinds"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'ivy-switch-buffer)
            map))
(keybinds-mode)
(provide 'keybinds-mode)
