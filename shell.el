;; Minor modes
(add-hook 'sh-mode-hook 'whitespace-mode)

;; 4 spaces instead of tab
(add-hook 'sh-mode-hook
	  '(lambda ()
	     (if (string-match "grayhemp" (buffer-file-name))
		 (setq tab-width 2
		       sh-basic-offset 2
		       sh-indentation 2)
	       (setq tab-width 4
		     sh-basic-offset 4
		     sh-indentation 4))
	     (setq indent-tabs-mode nil)))

;(setq sh-mode-hook nil)
