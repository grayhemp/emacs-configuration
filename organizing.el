; Minor modes
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

;; Use org-mode in org, org_archive and txt files
(add-to-list 'auto-mode-alist
	     `(,(concat "\\(\\.org\\|\\.org_archive\\|\\.txt\\)$") .
	       org-mode))

;; Set global keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Make windmove work in org-mode
(setq org-replace-disputed-keys t)

;; Add time stamp and note to the task when it's done
(setq org-log-done 'time)

;; Insert state change notes and time stamps into a drawer
(setq org-log-into-drawer t)

;; Add the REPORT drawer
(setq org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "REPORT"))

;; Agenda files
; Put them to ~/.emacs.d/custom.el with C-c [

;; Agenda clock report parameters (no links, 10 levels deep)
(setq org-agenda-clockreport-parameter-plist
      (quote (:level nil :maxlevel 10 :narrow 57! :indent t :tcolumns 1)))

;; Task states
(setq org-todo-keywords '((sequence "TODO(t)" "|"
				    "DONE(d)" "CANCELLED(c)" "ENTRUSTED(e)")))

;; Set state colors
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "firebrick2" :weight bold))
	("DONE" . (:foreground "forestgreen" :weight bold))
	("CANCELLED" . (:foreground "forestgreen" :weight bold))
	("ENTRUSTED" . (:foreground "forestgreen" :weight bold))))

;; Stuck tasks settings
(setq org-stuck-projects '("+LEVEL=1/-DONE-CANCELLED-ENTRUSTED" ("*")))

;; Keep tasks with dates off the global todo lists
(setq org-agenda-todo-ignore-with-date t)

;; Allow deadlines due soon to appear on the global todo lists
(setq org-agenda-todo-ignore-deadlines (quote far))

;; Keep tasks scheduled in the future off the global todo lists
(setq org-agenda-todo-ignore-scheduled (quote future))

;; Show closed, clocked and habit entries in agenda
(setq org-agenda-log-mode-items '(clock closed state))

;; Make agenda to keep startup visibility
(setq org-agenda-inhibit-startup nil)

;; Set key for capturing
(global-set-key "\C-cc" 'org-capture)

;; Capture default location
(setq org-default-notes-file "~/.emacs.d/organizing/notes.org")

;; Capture templates
(setq org-capture-templates
      '(("l" "Life Coach" entry
	 (file+datetree "~/.emacs.d/organizing/journal.org")
	 "* Life Coach\n%[~/.emacs.d/organizing/lifecoach.txt]\n")
	("j" "Journal" entry (file+datetree "~/.emacs.d/organizing/journal.org")
	 "* %^{Title}\n%?")
	("n" "Note" entry (file "~/.emacs.d/organizing/notes.org")
	 "* %^{Title}\n  CREATED: %U\n%?")
	("p" "Pyramidal" entry
	 (file+olp "~/.emacs.d/organizing/pyramidal.org"
		   "TODO Test memorization and build base images" "0")
	 "* %^{Word or phrase}\n%?")))

;; Collapse all the drawers in pyramidal.org when capturing a word
(add-hook 'org-capture-before-finalize-hook
	  '(lambda ()
	     (if (equal (org-capture-get :key) "p")
		 (with-current-buffer "pyramidal.org"
		   (org-global-cycle)))))

;; If we are in the pyramidal learning buffer refile only within the
;; current buffer
(add-hook 'org-mode-hook
	  '(lambda ()
	     (interactive)
	     (if (string-equal (buffer-name) "pyramidal.org")
		 (let()
		   (make-local-variable 'org-refile-targets)
		   (make-local-variable 'org-refile-use-outline-path)
		   (setq org-refile-targets '((nil :maxlevel . 2)))
		   (setq org-refile-use-outline-path 1)))))

;; Refile target files up to 10 levels deep
(setq org-refile-targets
      '((org-agenda-files :maxlevel . 10)
	(nil :maxlevel . 10)
	("~/.emacs.d/organizing/journal.org" :maxlevel . 10)
	("~/.emacs.d/organizing/someday.org" :maxlevel . 10)
	("~/.emacs.d/organizing/trash.org" :maxlevel . 10)))

;(with-current-buffer "pyramidal.org" (print org-refile-use-outline-path))

;; Allows refile to level 1 tasks
(setq org-refile-use-outline-path 'file)

;; Targets to refile complete in steps
(setq org-outline-path-complete-in-steps t)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Reset check boxes when task is done if RESET_CHECK_BOXES is "t"
;; http://orgmode.org/worg/org-contrib/
(require 'org-checklist)

;; Use org-clock
(require 'org-clock)

;; Turn org-clock sound on
(setq org-clock-sound t)

;; Set default countdown timer time to 25 min (Pomodoro)
(setq org-timer-default-timer 25)

;; The countdown timer is started automatically when a task is
;; clocking in, if it has not been started yet.
(setq org-timer-current-timer nil)
(setq pomodoro-is-active nil)
(add-hook 'org-clock-in-prepare-hook
	  '(lambda ()
	     (if (not pomodoro-is-active)
		 (let ((minutes (read-number "Start timer: " 25)))
		   (if org-timer-current-timer (org-timer-stop)) ;(org-timer-cancel-timer)
		   (org-timer-set-timer minutes)))
	     (setq pomodoro-is-active t)))
;(setq org-clock-in-prepare-hook nil)

;; The timer is finished automatically when a task is clocking
;; out. When finishing the timer it asks for a time interval of a
;; break, 5 minutes by default.
(add-hook 'org-clock-out-hook
	  '(lambda ()
	     (if (not org-clock-clocking-in)
		 (let ((minutes (read-number "Take a break: " 5)))
		   (org-timer-stop) ;(org-timer-cancel-timer)
		   (org-timer-set-timer minutes)
		   (setq pomodoro-is-active nil)))))
;(setq org-clock-out-hook nil)

;; Raise a notification after countdown done
(add-hook
 'org-timer-done-hook
 '(lambda ()
    (notifications-notify) ; :sound-name "alarm-clock-elapsed")
    (start-process-shell-command
     "mplayer" nil
     "mplayer /usr/share/sounds/freedesktop/stereo/complete.oga")))
;(setq org-timer-done-hook nil)

;; Erase all reminders and rebuilt them for today from the agenda
(defun rebuild-appt-reminders ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'rebuild-appt-reminders)

;; Initial appointments setup
(rebuild-appt-reminders)

;; If we leave Emacs running overnight - reset the appointments one
;; minute after midnight
(run-at-time "24:01" nil 'rebuild-appt-reminders)

;; The appointment notification facility
(setq
  appt-message-warning-time 15 ;; warn 15 min in advance
  appt-display-mode-line t     ;; show in the modeline
  appt-display-format 'window) ;; use our func

;; Substitute appt function with ours.
(setq appt-disp-window-function
      (lambda (min-to-app new-time msg)
	(interactive)
	(org-notify (format "%s: app't" msg) t)))

;; Activate appointments so we get notifications
(appt-activate t)
