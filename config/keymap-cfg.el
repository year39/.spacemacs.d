;;-----------------------------------------------------------
;; Emacs Key Mapping
;; Sep 30th, 2019
;;------------------------------------------------------------
(provide 'keymap-cfg)

;;____________________________________________________________
;; miscellaneous mapping

;; magit-status exit key
(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd "q") 'delete-frame)))

;; other frame
(global-set-key (kbd "<f12>") 'other-frame)

;; don't jump to previous line
(global-set-key [(control backspace)] 'my/kill-backward-smart)

;; projectile mode
(global-set-key (kbd "C-<return>") 'projectile-commander)
(unbind-key "C-<return>" emmet-mode-keymap)

;; lsp go to implementation
(global-set-key (kbd "M-/") 'lsp-goto-implementation)

;; kill buffer
(global-set-key (kbd "C-k") 'my/kill-buffer)

;; helm-occur for search :)
(global-set-key (kbd "C-s") 'helm-occur)

;; C-r for helm kill ring
(global-set-key (kbd "C-r") 'helm-show-kill-ring)

;; set-mark symbol under cursor
(global-set-key (kbd "S-SPC") 'my/set-mark-symbol-at-point)

;; C-r for helm eshell history
(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-r") 'helm-eshell-history)))

;; Go to beginning or end of current expression
;; This also works for parenthesis pairs.
(global-set-key (kbd "M-<left>") 'backward-sexp)
(global-set-key (kbd "M-<right>") 'forward-sexp)

;; Move line up/down: M-<up> & M-<down> => see cust-generic.el!
(global-set-key (kbd "M-<up>") 'my/move-line-up)
(global-set-key (kbd "M-<down>") 'my/move-line-down)

;; helm imenu
(global-set-key (kbd "M-i") 'helm-imenu)

;; Make iBuffer the default buffer management package
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Find files using helm
(global-set-key (kbd "C-x f") 'helm-find-files)

;; remove some of org mode key bindings
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-<return>") nil)
            (define-key org-mode-map (kbd "S-<return>") nil)
            (define-key org-mode-map (kbd "M-<return>") nil)))

;; ace-window
(global-set-key (kbd "C-<tab>") 'ace-window)
