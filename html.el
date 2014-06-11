;; Minor modes
(add-hook 'html-mode-hook 'whitespace-mode)

;; Auto-fill and TT highlight
(add-hook 'html-mode-hook
	  '(lambda ()
	     (auto-fill-mode 1)
	     (highlight-regexp "\\[%\\(.\\|\n\\)*?%\\]"
			       'font-lock-preprocessor-face)))

;(setq html-mode-hook nil)

;; What files to invoke the new html-mode for?
(add-to-list 'auto-mode-alist '("\\.inc\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.[sj]?html?\\'" . html-mode))

;; To avoid the "External entity HTML not found" error
;(setq sgml-warn-about-undefined-entities nil)

;; Configure css-mode
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '2)
