;; In psql turn off pager usage and adjust prompts
(setq sql-postgres-options '("-X" ""
			     "-P" "pager=off"
			     "-v" "PROMPT1=%n@%m:%> %~%R%#\n"
			     "-v" "PROMPT2="
			     "-v" "PROMPT3="))
