;; Minor modes
(add-hook 'sh-mode-hook 'whitespace-mode)

;; 4 spaces instead of tab
(add-hook 'sh-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil
		   tab-width 4
		   indent-line-function 'insert-tab)))

;(setq sh-mode-hook nil)
