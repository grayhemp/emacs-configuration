;; Major mode
(load "haskell-mode")

;; Minor modes
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'whitespace-mode)
(add-hook 'haskell-mode-hook 'subword-mode)
