;; Minor modes
(add-hook 'java-mode-hook 'subword-mode)
(add-hook 'java-mode-hook 'whitespace-mode)

;; Indentation settings
(add-hook 'java-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil
		   tab-width 4)
	     (c-set-offset 'arglist-intro '+)))

;(setq java-mode-hook nil)
