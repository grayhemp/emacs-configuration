; Minor modes
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
;(add-hook 'markdown-mode-hook 'whitespace-mode)
(add-hook 'markdown-mode-hook 'subword-mode)

;; Indentation settings
(add-hook 'markdown-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil
		   tab-width 4)))

;; Use markdown-mode for specific file types
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Indentation settings
(add-hook 'markdown-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil
		   tab-width 4)))
