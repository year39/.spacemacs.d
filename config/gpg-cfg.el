;;-----------------------------------------------------------
;; GPG Setup
;; Created:	Jan 26th, 2019
;;------------------------------------------------------------
(provide 'gpg-cfg)
(require 'epa-file)
(epa-file-enable)

; in case emacs cannot find GPG on macos
(when (string= system-type "darwin")
  (custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg")))

;; enable org mode for .gpg files
(add-to-list 'auto-mode-alist '("\\.gpg\\'" . org-mode))

;; Disable external passphrase entry
(setenv "GPG_AGENT_INFO" nil)
(setq epa-pinentry-mode 'loopback)
