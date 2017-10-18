;; Major mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/bats-mode/")
(require 'bats-mode)
(add-to-list 'auto-mode-alist '("\\.bats$" . bats-mode))

;; Minor modes
(add-hook 'bats-mode-hook 'whitespace-mode)
(add-hook 'bats-mode-hook 'subword-mode)
