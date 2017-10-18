;; Major mode
(require 'hcl-mode)

;; Minor modes
(add-hook 'hcl-mode-hook 'whitespace-mode)
(add-hook 'hcl-mode-hook 'subword-mode)
