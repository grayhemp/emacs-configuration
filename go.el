;; Major mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/go-mode/")
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; Minor modes
(add-hook 'go-mode-hook 'whitespace-mode)
(add-hook 'go-mode-hook 'subword-mode)

;; Indentation settings
(add-hook 'go-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil
		   tab-width 2)))
