;;-----------------------------------------------------------
;; Generic functions
;; June 10th, 2017
;;------------------------------------------------------------
(provide 'library)

;;____________________________________________________________
;; fill entire buffer
(defun my/fill-buffer ()
  (interactive)
  (fill-region (point-min) (point-max)))

;;____________________________________________________________
;; cd to project's directory (.git)
(defun my/cd ()
  (interactive)
  (eshell/cd (vc-git-root ".")))

;;____________________________________________________________
;; set-mark symbol at point
(defun my/set-mark-symbol-at-point ()
  (interactive)
  (unless (region-active-p)
    (let (bounds pos1 pos2)
      (setq bounds (bounds-of-thing-at-point 'symbol))
      (setq pos1 (car bounds))
      (setq pos2 (cdr bounds))
      (goto-char pos1)
      (push-mark pos1 t t)
      (goto-char pos2)
      (setq transient-mark-mode  (cons 'only transient-mark-mode)))))

;;____________________________________________________________
;; C-Backspace kill one word, a space character or a new line character
(defun my/kill-backward-smart ()
  (interactive)
  (cond
   ((looking-back (rx (char word)) 1)
    (backward-kill-word 1))
   ((looking-back (rx (char blank)) 1)
    (delete-horizontal-space t))
   (t
    (backward-delete-char 1))))

;;____________________________________________________________
;; kill this buffer
(defun my/kill-buffer ()
  (interactive)
  (kill-buffer (buffer-name)))

;;____________________________________________________________
(defun my/move-line-up ()
  "Move line up"
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;;____________________________________________________________
(defun my/move-line-down ()
  "Move line down"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode t))

;;____________________________________________________________
(defun my/save-obj-to-file (obj &optional filename)
  "Save symbol object whose name is OBJ to the byte compiled version of FILENAME, if FILENAME is provided. If not, the OBJ name is used as FILENAME.
OBJ runtime lisp object (add the single-quote mark ' to the object when reference)
FILE is an elisp file with extention '.el'. The file is stored in '~/.emacs.d/obj', unless the name is a directory.
Note: All the objects stored inside '~/.emacs.d/obj/*.elc' are automatically loaded on startup of emacs using `load'."

  ;;if filename not provided, use the OBJ name
  (if filename
      (progn
	(unless (string-match-p (regexp-quote "/") filename)
	  (setq filename (concat "~/.emacs.d/obj/" filename)))
	(unless (string-match-p (regexp-quote ".el") filename)
	  (setq filename (concat filename ".el"))))
    (setq filename (concat "~/.emacs.d/obj/"
			   (symbol-name obj) ".el")))
  (with-temp-file filename
    (erase-buffer)
    (let* ((obj-name (symbol-name obj))
	   (obj-val (format "(setq %s (eval-when-compile %s))"
			    obj-name
			    obj-name)))
      (insert obj-val)))
  (if (byte-compile-file filename) ;generate the .elc file
      (progn
	(delete-file filename) ;remove the .el file
	(message "`%s' stored to `%s'"
		 obj (concat filename "c")))
    (error (format "Compilation of (%s) failed!" filename))))


;;____________________________________________________________
(defun my/move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	  (if (string-match dir "\\(?:/\\|\\\\)$")
	      (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (progn
	(message "%s moved to %s" (buffer-name) dir)
	(copy-file filename newname 1)
	(delete-file filename)
	(set-visited-file-name newname)
	(set-buffer-modified-p nil) t))))


;;____________________________________________________________
(defun my/rename-file-buffer (new-name)
  "Rename both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!"
		   new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;;____________________________________________________________
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer
         (cond ((and (derived-mode-p 'magit-mode)
                     (eq (with-current-buffer buffer major-mode)
                         'magit-status-mode))
                nil)
               ((memq (with-current-buffer buffer major-mode)
                      '(magit-process-mode
                        magit-revision-mode
                        magit-diff-mode
                        magit-stash-mode))
                nil)
               (t
                '(display-buffer-same-window))))))
