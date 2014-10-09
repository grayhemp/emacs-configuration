;; Configure mmm-mode (C-c % C-b to reparce if submode is not
;; determined automatically)
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
;(setq mmm-submode-decoration-level 0)

;; Set up an mmm group for fancy html editing
(mmm-add-group 'fancy-html
	       '((html-css-attribute
		  :submode css-mode
		  :face mmm-declaration-submode-face
		  :front "style=\""
		  :back "\"")))

;; What features should be turned on in this html-mode?
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-js))
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil embedded-css))
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil fancy-html))

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
