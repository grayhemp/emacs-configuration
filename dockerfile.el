(require 'dockerfile-mode)

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

; Minor modes
(add-hook 'dockerfile-mode-hook 'auto-fill-mode)
(add-hook 'dockerfile-mode-hook 'whitespace-mode)
(add-hook 'dockerfile-mode-hook 'subword-mode)


;; Indentation settings
(add-hook 'dockerfile-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode t
		   tab-width 4)))

;(setq dockerfile-mode-hook nil)
