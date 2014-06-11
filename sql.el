;; Minor modes
;(add-hook 'sql-mode-hook 'whitespace-mode)

;; 4 spaces instead of tab
(add-hook 'sql-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil
		   tab-width 4
		   indent-line-function 'insert-tab)
	     (define-key sql-mode-map (kbd "C-j")
	       '(lambda()
		  (interactive)
		  (delete-horizontal-space t)
		  (newline)
		  (indent-relative-maybe)))
	     (define-key sql-mode-map "\C-c\C-xb" 'sql-set-sqli-buffer)))

;(setq sql-mode-hook nil)
