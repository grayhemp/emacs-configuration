;; Major mode
(require 'terraform-mode)


;; Indentation settings
(add-hook 'terraform-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil
		   tab-width 2)))

;; Minor modes
(add-hook 'terraform-mode-hook 'whitespace-mode)
(add-hook 'terraform-mode-hook 'subword-mode)
