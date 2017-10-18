;; Minor modes
(add-hook 'c-mode-hook 'subword-mode)
(add-hook 'c-mode-hook 'whitespace-mode)

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Indentation settings
;; (add-hook 'c-mode-hook
;; 	  '(lambda ()
;; 	     (setq c-default-style "linux"
;; 		   indent-tabs-mode t
;; 		   tab-width 4)
;; 	     (c-toggle-hungry-state 1)
;; 	     ; layout for long function signatures
;; 	     (c-set-offset 'arglist-intro '+)))

;(setq c-mode-hook nil)
