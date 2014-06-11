;; Minor modes
(add-hook 'conf-mode-hook 'whitespace-mode)

;; 4 spaces instead of tab
(add-hook 'conf-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode t
		   tab-width 4
		   indent-line-function 'insert-tab)
	     (define-key conf-mode-map (kbd "C-j")
	       '(lambda()
		  (interactive)
		  (delete-horizontal-space t)
		  (newline)
		  (indent-relative-maybe)))))

;(setq conf-mode-hook nil)
