;; Major mode
(require 'terraform-mode)

;; Minor modes
(add-hook 'terraform-mode-hook 'whitespace-mode)
(add-hook 'terraform-mode-hook 'subword-mode)
