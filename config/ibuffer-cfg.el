;;-----------------------------------------------------------
;; ibuffer Package Configurations
;; July 11th, 2017
;;------------------------------------------------------------
(provide 'ibuffer-cfg)

;; no prompts
(setq ibuffer-expert t)

;; hide empty groups
(setq ibuffer-show-empty-filter-groups nil)

;; hide certain buffers
(setq ibuffer-never-show-predicates nil)
(add-to-list 'ibuffer-never-show-predicates "^\\*")	;; hide if name starts with *
(add-to-list 'ibuffer-never-show-predicates "^\\magit*")

;; Shorten the direcoty
(defun str-replace-all (str pats)
  (if (null pats)
      str
    (let* ((pat (car pats))
           (lhs (car pat))
           (rhs (cdr pat)))
      (replace-regexp-in-string lhs rhs (str-replace-all str (cdr pats))))))

(defvar my/ibuffer-short-paths
  '(
    ("/home/dj" . "~")
    (".*:/home/dj" . "[root]~")
    ("/$" . "")))

(define-ibuffer-column my/ibuffer-format
  (:name "Directory")
  (if (buffer-file-name buffer)
      (str-replace-all (file-name-directory (buffer-file-name buffer)) my/ibuffer-short-paths)
    (or dired-directory "")))

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 30 30 :left :elide)
              " "
              my/ibuffer-format)))

;; keep the buffer list up to date
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "home")))
