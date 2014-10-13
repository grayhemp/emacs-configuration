;; Minor modes
(add-hook 'js-mode-hook 'whitespace-mode)
(add-hook 'js-mode-hook 'subword-mode)

;; Indentation settings
(add-hook 'js-mode-hook
	  '(lambda ()
	     (electric-indent-mode t) ; Auto-indent on closing braces
	     (setq indent-tabs-mode nil
		   tab-width 2)))

;; Configure SWS and Jade modes
(add-to-list 'load-path "~/.emacs.d/site-lisp/jade-mode")
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; Use JSHint for style checking by C-c C-u and do auto-check on save
(setq jshint-cli
      (concat "jshint --verbose --show-non-errors "
	      "--config ~/.emacs.d/site-lisp/jshintrc.json "))
(setq compilation-error-regexp-alist-alist
      (cons '(jshint-cli
	      "^\\([a-zA-Z\.0-9_/-]+\\): line \\([0-9]+\\), col \\([0-9]+\\)"
	      1 ;; file
	      2 ;; line
	      3 ;; column
	      )
	    compilation-error-regexp-alist-alist))
(setq compilation-error-regexp-alist
      (cons 'jshint-cli compilation-error-regexp-alist))
(defun whitespace-clean-and-compile ()
  (interactive)
  (whitespace-cleanup)
  (compile compile-command))
(add-hook 'js-mode-hook
	  '(lambda ()
	     (set (make-local-variable 'compile-command)
		  (let ((file buffer-file-name)) (concat jshint-cli file)))
	     (set (make-local-variable 'compilation-read-command) nil)
	     (add-hook 'after-save-hook
		       '(lambda () (compile compile-command))
		       nil "local")
	     (local-set-key "\C-c\C-u" 'whitespace-clean-and-compile)))

;(setq js-mode-hook nil)

;; Setup Node.js REPL
(require 'js-comint)
(add-hook 'js-mode-hook
	  '(lambda ()
	     (local-set-key "\C-c!" 'run-js)
	     (local-set-key "\C-x\C-e" 'js-send-last-sexp)
	     (local-set-key "\C-cb" 'js-send-buffer)
	     (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
	     (local-set-key "\C-cl" 'js-load-file-and-go)
	     (local-set-key "\C-c\C-r" 'js-send-region)
	     (local-set-key "\C-c\C-j" 'js-send-line)))

;; Fixes the escape issue with node-repl in js-comint.el
(defun node-repl-comint-preoutput-filter (output)
  (if (equal (buffer-name) "*js*")
      (progn
	;; Uncomment these to debug the IO of the node process
	;; (setq js-node-output output)
	;; (message (concat "\n----------\n" output "\n----------\n"))
	;; Replaced ^ with \^ to indicate that doesn't have to be
	;; at start of line
	(replace-regexp-in-string
	 "\\\[0K" ""
	 (replace-regexp-in-string
	  "\\\[1G" ""
	  (replace-regexp-in-string
	   "\\\[0J" ""
	   (replace-regexp-in-string
	    "\\\[3G" ""
	    (replace-regexp-in-string
	     "\\\[0G" ""
	     (replace-regexp-in-string
	      "\\[2C" ""
	      (replace-regexp-in-string
	       "\\[0K" ""
	       (replace-regexp-in-string
		"\\\[5G" ""
		(replace-regexp-in-string
		 "^M" "" output))))))))))
    output))
(add-hook 'comint-preoutput-filter-functions 'node-repl-comint-preoutput-filter)
(add-hook 'comint-output-filter-functions 'node-repl-comint-preoutput-filter)
