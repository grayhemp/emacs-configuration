;; Minor modes
(add-hook 'perl-mode-hook 'whitespace-mode)
(add-hook 'perl-mode-hook 'subword-mode)

;; Indentation settings
(add-hook 'perl-mode-hook
	  '(lambda ()
	     (if (string-match "~/Projects/test" (buffer-file-name))
		 (setq indent-tabs-mode nil)
	       (setq indent-tabs-mode t))
	     (setq tab-width 4)))

;; perlcritic
(autoload 'perlcritic "perlcritic" "" t)
(autoload 'perlcritic-region "perlcritic" "" t)
(autoload 'perlcritic-mode "perlcritic" "" t)
(eval-after-load "perl-mode"
  '(add-hook 'perl-mode-hook 'perlcritic-mode))

;(setq perl-mode-hook nil)
