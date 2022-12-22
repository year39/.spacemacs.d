;;-----------------------------------------------------------
;; Programming Modes
;; July 12th, 2017
;;------------------------------------------------------------
(provide 'programming-cfg)

(setq gdb-many-windows t
      gdb-show-main    t)

;; use emacs sudo in eshell
(require 'esh-module)
(add-to-list 'eshell-modules-list 'eshell-tramp)

;;____________________________________________________________
;; automatically fill comments but not code
(add-hook 'c-mode-common-hook
          (lambda ()
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))))

;;____________________________________________________________
;; ref: https://lists.gnu.org/archive/html/help-gnu-emacs/2010-01/msg00527.html
(c-add-style "mycodingstyle"
             '((c-comment-only-line-offset . 0)
               (c-hanging-braces-alist . ((substatement-open before after)))
               (c-offsets-alist . ((topmost-intro        . 0)
                                   (topmost-intro-cont   . 0)
                                   (substatement         . 3)
                                   (substatement-open    . 0)
                                   (statement-case-open  . 3)
                                   (statement-cont       . 3)
                                   (access-label         . -3)
                                   (inclass              . 3)
                                   (inline-open          . 3)
                                   (innamespace          . 0)
                                   ))))
;; c/c++ mode
(add-hook 'c-mode-common-hook
          '(lambda()
             (c-set-style "mycodingstyle")
             (setq tab-width 4)
             (setq c-basic-offset tab-width)))

;;____________________________________________________________
;; unix conf settings
(add-hook 'conf-unix-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (setq indent-tabs-mode nil)
            (setq tab-width 1)))

;;____________________________________________________________
;; yaml settings
(add-hook 'yaml-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (setq indent-tabs-mode nil)
            (setq tab-width 1)))

;;____________________________________________________________
;; Eshell Settings

;; limit buffer size of eshell
(setq eshell-buffer-maximum-lines 999)
;;(add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

;; .clang-format is a yaml file
(add-to-list 'auto-mode-alist '(".clang-format" . yaml-mode))
(add-to-list 'auto-mode-alist '(".yaml" . yaml-mode))

;; Qt .qrc file
(add-to-list 'auto-mode-alist '(".qrc" . nxml-mode))

;; Unix config mode
(add-to-list 'auto-mode-alist '(".dts" . conf-unix-mode))
(add-to-list 'auto-mode-alist '(".dtsi" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("kconfig" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("kconfig.board" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("_defconfig" . conf-unix-mode))

;;____________________________________________________________
;; Eshell prompt

(setq eshell-prompt-function
      (lambda ()
        (let* ((home-dir (expand-file-name "~"))
               ;; replace home dir with "~"
               (cur-dir (replace-regexp-in-string
                         home-dir "~" (eshell/pwd))))
          (concat
           (propertize "┌─[")
           (propertize (user-login-name)'face `(:foreground 'default))
           (propertize "@" 'face `(:foreground 'default))
           (propertize (system-name) 'face `(:foreground 'default))
           (propertize "]─[")
           (propertize cur-dir 'face `(:foreground 'default))
           (propertize "]\n")
           (propertize "└─>")
           (propertize (if (= (user-uid) 0) " # " " $ "))))))
