;; Starting emacs server
(server-start)

;; Custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Add path to the local site-lisp
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Add exec-path for /usr/local/bin
(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; Use desctop notifications
(require 'notifications)

;; Use common lisp
(require 'cl)

;; Use compile
(require 'compile)

;; Close the compilation window if there was no error at all.
(setq compilation-exit-message-function
      (lambda (status code msg)
	;; If M-x compile exists with a 0
	(when (and (eq status 'exit) (zerop code))
	  ;; then bury the *compilation* buffer, so that C-x b doesn't go there
  	  (bury-buffer "*compilation*")
  	  ;; and return to whatever were looking at before
  	  (replace-buffer-in-windows "*compilation*"))
	;; Always return the anticipated result of compilation-exit-message-function
	(cons msg code)))

;; Uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; Save recent files acros sessions
(require 'recentf)
(recentf-mode t)

;; Interact with OS clipboard
(setq x-select-enable-clipboard t)

;; Turn off scrollbar, toolbar and menubar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode 1)

;; Turn on column number mode
(column-number-mode t)

;; Highlight brackets
(show-paren-mode t)

;; Delete seleted text when typing
(delete-selection-mode t)

;; Font
;(set-default-font "Ubuntu Mono 12")

;; Input method
(setq default-input-method "russian-computer")

;; Copy original file when creating backup
(setq backup-by-copying t)

;; File path mirroring for backup files
(defun my-backup-file-name (fpath)
  (let (backup-root bpath)
    (setq backup-root "~/.emacs.d/files-backup")
    (setq bpath (concat backup-root fpath "~"))
    (make-directory (file-name-directory bpath) bpath)
    bpath
    )
  )
(setq make-backup-file-name-function 'my-backup-file-name)

;; Dired Mode extra features
(load "dired-x")
(setq dired-omit-files "^\\.\\.?$")
      ;(concat dired-omit-files "^\\.{1,2}$"))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

;; Direx Mode
(require 'direx)
(global-set-key (kbd "C-x C-j") 'direx:find-directory)

;; Windmove (S-arrows)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Winner (C-c left/right)
(when (fboundp 'winner-mode) (winner-mode 1))

;; Fix flyspell problem
(setq flyspell-issue-welcome-flag nil)

;; The shell mode completions problem workaround
(defun comint-close-completions ()
  (if comint-dynamic-list-completions-config
      (progn
	(set-window-configuration comint-dynamic-list-completions-config)
	(setq comint-dynamic-list-completions-config nil))))
(defadvice comint-send-input (after close-completions activate)
  (comint-close-completions))
(defadvice comint-dynamic-complete-as-filename
  (after close-completions activate)
  (if ad-return-value (comint-close-completions)))
(defadvice comint-dynamic-simple-complete (after close-completions activate)
  (if (member ad-return-value '('sole 'shortest 'partial))
      (comint-close-completions)))
(defadvice comint-dynamic-list-completions (after close-completions activate)
  (comint-close-completions)
  (if (not unread-command-events)
      ;; comint's "Type space to flush" swallows space. put it back in.
      (setq unread-command-events (listify-key-sequence " "))))

;; Unbing easy keys
(defvar no-easy-keys-minor-mode-map (make-keymap)
  "no-easy-keys-minor-mode keymap.")
(let ((f (lambda (m)
	   `(lambda () (interactive)
	      (message (concat "No! use " ,m " instead."))))))
  (dolist (l '(("<left>" . "C-b") ("<right>" . "C-f") ("<up>" . "C-p")
	       ("<down>" . "C-n")
	       ("<C-left>" . "M-f") ("<C-right>" . "M-b") ("<C-up>" . "M-{")
	       ("<C-down>" . "M-}")
	       ("<M-left>" . "M-f") ("<M-right>" . "M-b") ("<M-up>" . "M-{")
	       ("<M-down>" . "M-}")
	       ("<delete>" . "C-d") ("<C-delete>" . "M-d")
	       ("<M-delete>" . "M-d") ("<next>" . "C-v") ("<C-next>" . "M-x <")
	       ("<prior>" . "M-v") ("<C-prior>" . "M-x >")
	       ("<home>" . "C-a") ("<C-home>" . "M->")
	       ("<C-home>" . "M-<") ("<end>" . "C-e") ("<C-end>" . "M->")))
    (define-key no-easy-keys-minor-mode-map
      (read-kbd-macro (car l)) (funcall f (cdr l)))))
(define-minor-mode no-easy-keys-minor-mode
  "A minor mode that disables the arrow-keys, pg-up/down, delete
  and backspace."  t " no-easy-keys"
  'no-easy-keys-minor-mode-map :global t)
(no-easy-keys-minor-mode 1)

;; fontify code in code blocks
(setq org-src-fontify-natively t)

;; make long lines in the whitespace mode look more readable
(custom-set-faces
 '(whitespace-line ((t (:background "Wheat")))))

;; Invoking a login shell
(setq explicit-bash-args '( "--login" "--noediting" "-i"))
