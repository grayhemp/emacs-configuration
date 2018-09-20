(require 'dockerfile-mode)

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

; Minor modes
(add-hook 'dockerfile-mode-hook 'auto-fill-mode)
(add-hook 'dockerfile-mode-hook 'whitespace-mode)
(add-hook 'dockerfile-mode-hook 'subword-mode)


;; Indentation settings
(add-hook 'dockerfile-mode-hook
	  '(lambda ()
	     (if (string-match "grayhemp" (buffer-file-name))
		 (setq tab-width 2)
	       (setq tab-width 4))
	     (setq indent-tabs-mode nil)))

;(setq dockerfile-mode-hook nil)
