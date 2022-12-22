;;-----------------------------------------------------------
;; Emacs Misc Setup
;; July 12th, 2017
;;------------------------------------------------------------
(provide 'misc-cfg)

;;____________________________________________________________
;; Prompt before closing
(setq confirm-kill-emacs 'yes-or-no-p)

;;____________________________________________________________
;; Don't prompt confirmation about processes.
(setq confirm-kill-processes nil)

;;____________________________________________________________
;; Password cache
(setq password-cache t)
(setq password-cache-expiry nil)

;;____________________________________________________________
;; Remove duplicates in commands history
(setq history-delete-duplicates t)

;; auto fill mode
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Set default tab width
(setq tab-width 4)

;; Replace highlight when typing
(delete-selection-mode 1)

;;____________________________________________________________
;; Frames & Windows

;; memorize windows setup, 1 or 3 windows
(setq my/window-setup-mem 1)

;; Split emacs windows and make the first window active
(defun my/setup-frame ()
  "Split the main frame"
  (delete-other-windows)
  (split-window-below)
  (other-window 1)
  (eshell 1) ;windows #3 is always the shell interface
  (split-window-right)
  (other-window 1)
  (eshell 2)
  (other-window 1) ;make window #1 is the active window
  (split-window-right)
  (enlarge-window 8)
  (setq my/window-setup-mem 3))

;; Toggle emacs windows setup 1 or 3 windows (C-c SPC)
(defun my/toggle-windows-setup ()
  "Toggle emacs windows to either 1 window or 3 windows"
  (interactive)
  (if (eq my/window-setup-mem 3)
      (progn
        (delete-other-windows)
        (setq my/window-setup-mem 1))
    (my/setup-frame)))

;; Split emacs windows to 4 windows for shell work
(defun my/eshell-setup ()
  "Split the main frame to 4 eshell frames"
  (interactive)
  (delete-other-windows)
  (eshell)
  (split-window-below)
  (split-window-right)
  (other-window 1)
  (eshell 2)
  (other-window 1)
  (eshell 3)
  (split-window-right)
  (other-window 1)
  (eshell 4)
  (other-window 1)) ;make window #1 is the active window

;; visit file or directory on current line
(put 'dired-find-alternate-file 'disabled nil)

;; kills the Dired buffer, then visits the current line's file or directory
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)))

;; do not fucking truncate the damn line!
(setq truncate-partial-width-windows 400)

;;___________________________________________________________
;; save emacs customizations somewhere
(setq custom-file "~/.spacemacs.d/config/emacs.el")

;;___________________________________________________________
;; Default Backup directory
(setq backup-directory-alist '(("." . "~/.spacemacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 10   ; how many of the new to keep
      kept-old-versions 5    ; and how many of the old
      )

;;___________________________________________________________
;; Turn off autosave
(setq auto-save-default nil)
