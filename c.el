;; Minor modes
(add-hook 'c-mode-hook 'subword-mode)
(add-hook 'c-mode-hook 'whitespace-mode)

;; Indentation settings
(add-hook 'c-mode-hook
	  '(lambda ()
	     (setq c-default-style "linux")
	     (c-toggle-hungry-state 1)
	     ; layout for long function signatures
	     (c-set-offset 'arglist-intro '+)))

;(setq c-mode-hook nil)
