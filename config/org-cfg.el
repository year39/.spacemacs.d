;;------------------------------------------------------------
;; Orgmode Config
;; May 13, 2017
;;------------------------------------------------------------
(provide 'org-cfg)

(setq org-startup-folded 'overview)
(setq org-agenda-files '("~/Dropbox/org"))
(setq org-directory "~/Dropbox/org")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(setq org-capture-templates
      '(
        ("t" "Task" entry
         (file+headline "~/Dropbox/org/tasks.org" "Tasks")
         "* TODO %?" :empty-lines 1)
        ("m" "Meeting" entry
         (file+headline "~/Dropbox/org/cal.org" "Meeting")
         "* TODO Meeting %^{Meeting Title}
- Required: [%^{Required}]
- Location: [%^{Location}]
- Agenda:   [%^{meeting agends}]
	SCHEDULED: %^t" :empty-lines 1)
        ("f" "Follow Up" entry
         (file+headline "~/Dropbox/org/cal.org" "Follow Up")
         "* TODO Follow up with %^{Person name} about %^{Topic}
	DEADLINE: %^t" :empty-lines 1)
        ))

;; Use 12-hour clock instead of 24-hour in agenda view
(setq org-agenda-timegrid-use-ampm 1)

;; Start with headers collapsed
(setq org-startup-folded t)

;; If idle for more than 15 minutes, resolve the things by asking what to do
;; with the clock time
(setq org-clock-idle-time 15)


;;____________________________________________________________
