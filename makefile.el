(add-to-list 'auto-mode-alist '("Makefile\\'" . makefile-mode))

; Minor modes
(add-hook 'makefile-mode-hook 'auto-fill-mode)
(add-hook 'makefile-mode-hook 'whitespace-mode)
(add-hook 'makefile-mode-hook 'subword-mode)
