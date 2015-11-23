;; Minor modes
(add-hook 'yaml-mode-hook 'subword-mode)
(add-hook 'yaml-mode-hook 'whitespace-mode)

(add-to-list 'auto-mode-alist '("\\.raml$" . yaml-mode))

;(setq yaml-mode-hook nil)
