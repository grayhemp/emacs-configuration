;; Minor modes
(add-hook 'ruby-mode-hook 'whitespace-mode)
(add-hook 'ruby-mode-hook 'subword-mode)

(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

;; Indentation settings
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (setq ruby-deep-indent-paren nil)))
