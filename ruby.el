;; Minor modes
(add-hook 'ruby-mode-hook 'whitespace-mode)
(add-hook 'ruby-mode-hook 'subword-mode)

;; Indentation settings
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (setq ruby-deep-indent-paren nil)))
