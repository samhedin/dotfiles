;;; ../dotfiles/doom/.doom.d/keybinds.el -*- lexical-binding: t; -*-
(defvar keybinds-mode-map (make-sparse-keymap)
  "Keymap for `my-mode'.")

(define-key keybinds-mode-map (kbd "C-1") 'winum-select-window-1)
(define-key keybinds-mode-map (kbd "C-2") 'winum-select-window-2)
(define-key keybinds-mode-map (kbd "C-3") 'winum-select-window-3)
(define-key keybinds-mode-map (kbd "C-3") 'winum-select-window-3)
(define-key keybinds-mode-map (kbd "C-SPC") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-SPC") nil)
(map! :map keybinds-mode-map "<f12>" 'eval-expression)
(map! :leader "!" 'shell-command)
(map! :g "C-l" 'switch-to-buffer)

(define-key )

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
