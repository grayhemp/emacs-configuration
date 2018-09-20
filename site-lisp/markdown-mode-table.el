;;; markdown-mode-table.el --- Pipe table support in Markdown mode.

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

;; Author: Dmitry Safronov <saf.dmitry@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple pipe table looks like this:

;;    | Right | Left | Center | Default |
;;    |------:|:-----|:------:|---------|
;;    |    12 | 12   | 12     | 12      |
;;    |   123 | 123  | 123    | 123     |
;;    |     1 | 1    | 1      | 1       |

;; First line contains column headers. Second line contains a separator
;; line between the headers and the content. Each following line is a row
;; in the table. Columns are always separated by the pipe character.
;; The colons indicate column alignment.

;; A table is re-aligned automatically each time you press `<TAB>` or
;; `<RET>` inside the table. `<TAB>` also moves to the next field (`<RET>`
;; to the next row) and creates new table rows at the end of the table or
;; before horizontal separator lines. The indentation of the table is set
;; by the first line. Column centering inside Emacs is not supported.

;; Beginning pipe characters are required for proper detection of table
;; borders inside Emacs. Any line starting with `|-` or `|:` is considered
;; as a horizontal separator line and will be expanded on the next re-align
;; to span the whole table width. No padding is allowed between the
;; beginning pipe character and header separator symbol. So, to create the
;; above table, you would only type

;;    |Right|Left|Center|Default|
;;    |-

;; and then press `<TAB>` to align the table and start filling in cells.

;; Then you can jump with the `<TAB>` from one cell to the next or with
;; `S-<TAB>` to the previous one. `<RET>` will jump to the to the next cell
;; in the same column, and create a new row if there is no such cell or if
;; the next row is beyond a separator line.

;; You can also convert selected region to a pipe table. Basic editing
;; capabilities comprise inserting, deleting, and moving of columns and rows,
;; and table sorting and transposing:

;; - `M-<left/right>`: Move the current column left/right.
;; - `M-S-<left>`: Delete the current column.
;; - `M-S-<right>`: Insert a new column to the left of the cursor position.
;; - `M-<up/down>`: Move the current row up/down.
;; - `M-S-<up>`: Delete the current row or horizontal separator line.
;; - `M-S-<down>`: Insert a new row above the current row. With a prefix
;;    argument, the line is created below the current one.

;; - `C-c ^`: Sort table lines alpabetically or numerically.
;; - `C-c |`: Convert selected region to a pipe table.
;; - `C-c C-x C-t`: Transpose the pipe table at point.

;; Limitations:

;; The code try to handle markup hiding correctly when calculating column
;; width. However, columns containig hidden markup may not always be
;; properly aligned.

;;; Code:

(require 'cl-lib)
(require 'markdown-mode)

;;;; General helper functions

(defmacro markdown-with-gensyms (symbols &rest body)
  (declare (debug (sexp body)) (indent 1))
  `(let ,(mapcar (lambda (s)
                   `(,s (make-symbol (concat "--" (symbol-name ',s)))))
                 symbols)
     ,@body))

(defun markdown-split-string (string &optional separators)
  "Splits STRING into substrings at SEPARATORS.
SEPARATORS is a regular expression. If nil it defaults to
`split-string-default-separators'. This version returns no empty
strings if there are matches at the beginning and end of string."
  (let ((start 0) notfirst list)
    (while (and (string-match
                 (or separators split-string-default-separators)
                 string
                 (if (and notfirst
                          (= start (match-beginning 0))
                          (< start (length string)))
                     (1+ start) start))
                (< (match-beginning 0) (length string)))
      (setq notfirst t)
      (or (eq (match-beginning 0) 0)
          (and (eq (match-beginning 0) (match-end 0))
               (eq (match-beginning 0) start))
          (push (substring string start (match-beginning 0)) list))
      (setq start (match-end 0)))
    (or (eq start (length string))
        (push (substring string start) list))
    (nreverse list)))

(defun markdown-string-width (s)
  "Return width of string S.
This version ignores characters with invisibility property
`markdown-markup'."
  (let (b)
    (when (or (eq t buffer-invisibility-spec)
              (member 'markdown-markup buffer-invisibility-spec))
      (while (setq b (text-property-any
                      0 (length s)
                      'invisible 'markdown-markup s))
        (setq s (concat
                 (substring s 0 b)
                 (substring s (or (next-single-property-change
                                   b 'invisible s)
                                  (length s))))))))
  (string-width s))

(defun markdown-sort-remove-markup (s)
  "Remove Markdown markup from string S.
This version removes characters with invisibility property
`markdown-markup'."
  (let (b)
    (while (setq b (text-property-any
                    0 (length s)
                    'invisible 'markdown-markup s))
      (setq s (concat
               (substring s 0 b)
               (substring s (or (next-single-property-change
                                 b 'invisible s)
                                (length s)))))))
  s)

;;;; Functions for maintaining pipe tables

(defconst markdown-pipe-table-line-regexp "^[ \t]*|"
  "Regexp matching any line inside a pipe table.")

(defconst markdown-pipe-table-hline-regexp "^[ \t]*|[-:]"
  "Regexp matching hline inside a pipe table.")

(defconst markdown-pipe-table-dline-regexp "^[ \t]*|[^-:]"
  "Regexp matching dline inside a pipe table.")

(defconst markdown-pipe-table-border-regexp "^[ \t]*[^| \t]"
  "Regexp matching any line outside a pipe table.")

(defun markdown-pipe-table-at-point-p ()
  "Return non-nil when point is inside a pipe table."
  (save-excursion
    (beginning-of-line)
    (and (looking-at-p markdown-pipe-table-line-regexp)
         (not (markdown-code-block-at-point-p)))))

(defun markdown-pipe-table-hline-at-point-p ()
  "Return non-nil when point is on a hline in a pipe table.
This function assumes point is on a table."
  (save-excursion
    (beginning-of-line)
    (looking-at-p markdown-pipe-table-hline-regexp)))

;;;###autoload
(defun markdown-pipe-table-begin ()
  "Find the beginning of the pipe table and return its position.
This function assumes point is on a table."
  (cond
   ((save-excursion
      (and (re-search-backward markdown-pipe-table-border-regexp nil t)
           (line-beginning-position 2))))
   (t (point-min))))

;;;###autoload
(defun markdown-pipe-table-end ()
  "Find the end of the pipe table and return its position.
This function assumes point is on a table."
  (save-excursion
    (cond
     ((re-search-forward markdown-pipe-table-border-regexp nil t)
      (match-beginning 0))
     (t (goto-char (point-max))
        (skip-chars-backward " \t")
        (if (bolp) (point) (line-end-position))))))

;;;###autoload
(defun markdown-pipe-table-get-dline ()
  "Return index of the table data line at point.
This function assumes point is on a table."
  (let ((pos (point)) (end (markdown-pipe-table-end)) (cnt 0))
    (save-excursion
      (goto-char (markdown-pipe-table-begin))
      (while (and (re-search-forward
                   markdown-pipe-table-dline-regexp end t)
                  (setq cnt (1+ cnt))
                  (< (point-at-eol) pos))))
    cnt))

;;;###autoload
(defun markdown-pipe-table-get-column ()
  "Return table column at point.
This function assumes point is on a table."
  (let ((pos (point)) (cnt 0))
    (save-excursion
      (beginning-of-line)
      (while (search-forward "|" pos t) (setq cnt (1+ cnt))))
    cnt))

(defun markdown-pipe-table-get-cell (&optional n)
  "Return the content of the cell in column N of current row.
N defaults to column at point. This function assumes point is on
a table."
  (and n (markdown-pipe-table-goto-column n))
  (skip-chars-backward "^|\n") (backward-char 1)
  (if (looking-at "|[^|\r\n]*")
      (let* ((pos (match-beginning 0))
             (val (buffer-substring (1+ pos) (match-end 0))))
        (goto-char (min (point-at-eol) (+ 2 pos)))
        ;; Trim whitespaces
        (setq val (replace-regexp-in-string "\\`[ \t]+" "" val)
              val (replace-regexp-in-string "[ \t]+\\'" "" val)))
    (forward-char 1) ""))

;;;###autoload
(defun markdown-pipe-table-goto-dline (N)
  "Go to the Nth data line in the pipe table at point.
Return t when the line exists, nil otherwise. This function
assumes point is on a table."
  (goto-char (markdown-pipe-table-begin))
  (let ((end (markdown-pipe-table-end)) (cnt 0))
    (while (and (re-search-forward
                 markdown-pipe-table-dline-regexp end t)
                (< (setq cnt (1+ cnt)) N)))
    (= cnt N)))

;;;###autoload
(defun markdown-pipe-table-goto-column (n &optional on-delim)
  "Go to the Nth column in the pipe table line at point.
With optional argument ON-DELIM, stop with point before the left
delimiter of the cell. If there are less than N cells, just go
beyond the last delimiter. This function assumes point is on a
table."
  (beginning-of-line 1)
  (when (> n 0)
    (while (and (> (setq n (1- n)) -1)
                (search-forward "|" (point-at-eol) t)))
    (if on-delim
        (backward-char 1)
      (when (looking-at " ") (forward-char 1)))))

;;;###autoload
(defmacro markdown-pipe-table-save-cell (&rest body)
  "Save cell at point, execute BODY and restore cell.
This function assumes point is on a table."
  (declare (debug (body)))
  (markdown-with-gensyms (line column)
    `(let ((,line (copy-marker (line-beginning-position)))
           (,column (markdown-pipe-table-get-column)))
       (unwind-protect
           (progn ,@body)
         (goto-char ,line)
         (markdown-pipe-table-goto-column ,column)
         (set-marker ,line nil)))))

;;;###autoload
(defun markdown-pipe-table-blank-line (s)
  "Convert a pipe table line S into a line with blank cells."
  (if (string-match "^[ \t]*|-" s)
      (setq s (mapconcat
               (lambda (x) (if (member x '(?| ?+)) "|" " "))
               s ""))
    (while (string-match "|\\([ \t]*?[^ \t\r\n|][^\r\n|]*\\)|" s)
      (setq s (replace-match
               (concat "|" (make-string (length (match-string 1 s)) ?\ ) "|")
               t t s)))
    s))

;;;###autoload
(defun markdown-pipe-table-colfmt (fmtspec)
  "Process column alignment specifier for pipe tables."
  (when (stringp fmtspec)
    (mapcar (lambda (x)
              (cond ((string-match-p "^:.*:$" x) 'c)
                    ((string-match-p "^:"     x) 'l)
                    ((string-match-p ":$"     x) 'r)
                    (t 'd)))
            (markdown-split-string fmtspec "\\s-*|\\s-*"))))

;;;###autoload
(defun markdown-pipe-table-align ()
  "Align pipe table at point.
This function assumes point is on a table."
  (interactive)
  (let ((begin (markdown-pipe-table-begin))
        (end (copy-marker (markdown-pipe-table-end))))
    (markdown-pipe-table-save-cell
     (goto-char begin)
     (let* (fmtspec
            ;; Store table indent
            (indent (progn (looking-at "[ \t]*") (match-string 0)))
            ;; Split table in lines and save column format specifier
            (lines (mapcar (lambda (l)
                             (if (string-match-p "\\`[ \t]*|[-:]" l)
                                 (progn (setq fmtspec (or fmtspec l)) nil) l))
                           (markdown-split-string (buffer-substring begin end) "\n")))
            ;; Split lines in cells
            (cells (mapcar (lambda (l) (markdown-split-string l "\\s-*|\\s-*"))
                           (remq nil lines)))
            ;; Calculate maximum number of cells in a line
            (maxcells (if cells
                          (apply #'max (mapcar #'length cells))
                        (user-error "Empty table")))
            ;; Empty cells to fill short lines
            (emptycells (make-list maxcells "")) maxwidths)
       ;; Calculate maximum width for each column
       (dotimes (i maxcells)
         (let ((column (mapcar (lambda (x) (or (nth i x) "")) cells)))
           (push (apply #'max 1 (mapcar #'markdown-string-width column))
                 maxwidths)))
       (setq maxwidths (nreverse maxwidths))
       ;; Process column format specifier
       (setq fmtspec (markdown-pipe-table-colfmt fmtspec))
       ;; Compute formats needed for output of table lines
       (let ((hfmt (concat indent "|"))
             (rfmt (concat indent "|"))
             hfmt1 rfmt1 fmt)
         (dolist (width maxwidths (setq hfmt (concat (substring hfmt 0 -1) "|")))
           (setq fmt (pop fmtspec))
           (cond ((equal fmt 'l) (setq hfmt1 ":%s-|" rfmt1 " %%-%ds |"))
                 ((equal fmt 'r) (setq hfmt1 "-%s:|" rfmt1  " %%%ds |"))
                 ((equal fmt 'c) (setq hfmt1 ":%s:|" rfmt1 " %%-%ds |"))
                 (t              (setq hfmt1 "-%s-|" rfmt1 " %%-%ds |")))
           (setq rfmt (concat rfmt (format rfmt1 width)))
           (setq hfmt (concat hfmt (format hfmt1 (make-string width ?-)))))
         ;; Replace modified lines only
         (dolist (line lines)
           (let ((line (if line
                           (apply #'format rfmt (append (pop cells) emptycells))
                         hfmt))
                 (previous (buffer-substring (point) (line-end-position))))
             (if (equal previous line)
                 (forward-line)
               (insert line "\n")
               (delete-region (point) (line-beginning-position 2))))))
       (set-marker end nil)))))

;;;###autoload
(defun markdown-pipe-table-insert-row (&optional arg)
  "Insert a new row above the row at point into the pipe table.
With optional argument ARG, insert below the current row."
  (interactive "P")
  (unless (markdown-pipe-table-at-point-p)
    (user-error "Not at a pipe table"))
  (let* ((line (buffer-substring
                (line-beginning-position) (line-end-position)))
         (new (markdown-pipe-table-blank-line line)))
    (beginning-of-line (if arg 2 1))
    (unless (bolp) (insert "\n"))
    (insert-before-markers new "\n")
    (beginning-of-line 0)
    (re-search-forward "| ?" (line-end-position) t)))

;;;###autoload
(defun markdown-pipe-table-delete-row ()
  "Delete row or horizontal line at point from the pipe table."
  (interactive)
  (unless (markdown-pipe-table-at-point-p)
    (user-error "Not at a pipe table"))
  (let ((col (current-column)))
    (kill-region (point-at-bol)
                 (min (1+ (point-at-eol)) (point-max)))
    (unless (markdown-pipe-table-at-point-p) (beginning-of-line 0))
    (move-to-column col)))

;;;###autoload
(defun markdown-pipe-table-move-row (&optional up)
  "Move table line at point down.
With optional argument UP, move it up."
  (interactive "P")
  (unless (markdown-pipe-table-at-point-p)
    (user-error "Not at a pipe table"))
  (let* ((col (current-column)) (pos (point))
         (tonew (if up 0 2)) txt hlinep)
    (beginning-of-line tonew)
    (unless (markdown-pipe-table-at-point-p)
      (goto-char pos) (user-error "Cannot move row further"))
    (setq hlinep (looking-at markdown-pipe-table-hline-regexp))
    (goto-char pos) (beginning-of-line 1) (setq pos (point))
    (setq txt (buffer-substring (point) (1+ (point-at-eol))))
    (delete-region (point) (1+ (point-at-eol)))
    (beginning-of-line tonew)
    (insert txt) (beginning-of-line 0)
    (move-to-column col)))

;;;###autoload
(defun markdown-pipe-table-move-row-up ()
  "Move table row at point up."
  (interactive)
  (markdown-pipe-table-move-row 'up))

;;;###autoload
(defun markdown-pipe-table-move-row-down ()
  "Move table row at point down."
  (interactive)
  (markdown-pipe-table-move-row nil))

;;;###autoload
(defun markdown-pipe-table-insert-column ()
  "Insert a new column into the pipe table."
  (interactive)
  (unless (markdown-pipe-table-at-point-p)
    (user-error "Not at a pipe table"))
  (let* ((col (max 1 (markdown-pipe-table-get-column)))
         (begin (markdown-pipe-table-begin))
         (end (copy-marker (markdown-pipe-table-end))))
    (markdown-pipe-table-save-cell
     (goto-char begin)
     (while (< (point) end)
       (markdown-pipe-table-goto-column col t)
       (if (markdown-pipe-table-hline-at-point-p)
           (insert "|---")
         (insert "|   "))
       (forward-line)))
    (set-marker end nil)
    (markdown-pipe-table-align)))

;;;###autoload
(defun markdown-pipe-table-delete-column ()
  "Delete column at point from the pipe table."
  (interactive)
  (unless (markdown-pipe-table-at-point-p)
    (user-error "Not at a pipe table"))
  (let ((col (markdown-pipe-table-get-column))
        (begin (markdown-pipe-table-begin))
        (end (copy-marker (markdown-pipe-table-end))))
    (markdown-pipe-table-save-cell
     (goto-char begin)
     (while (< (point) end)
       (markdown-pipe-table-goto-column col t)
       (and (looking-at "|[^|\n]+|")
            (replace-match "|"))
       (forward-line)))
    (set-marker end nil)
    (markdown-pipe-table-goto-column (max 1 (1- col)))
    (markdown-pipe-table-align)))

;;;###autoload
(defun markdown-pipe-table-move-column (&optional left)
  "Move table column at point to the right.
With optional argument LEFT, move it to the left."
  (interactive "P")
  (unless (markdown-pipe-table-at-point-p)
    (user-error "Not at a pipe table"))
  (let* ((col (markdown-pipe-table-get-column))
         (col1 (if left (1- col) col))
         (colpos (if left (1- col) (1+ col)))
         (begin (markdown-pipe-table-begin))
         (end (copy-marker (markdown-pipe-table-end))))
    (when (and left (= col 1))
      (user-error "Cannot move column further left"))
    (when (and (not left) (looking-at "[^|\n]*|[^|\n]*$"))
      (user-error "Cannot move column further right"))
    (markdown-pipe-table-save-cell
     (goto-char begin)
     (while (< (point) end)
       (markdown-pipe-table-goto-column col1 t)
       (when (looking-at "|\\([^|\n]+\\)|\\([^|\n]+\\)|")
         (replace-match "|\\2|\\1|"))
       (forward-line)))
    (set-marker end nil)
    (markdown-pipe-table-goto-column colpos)
    (markdown-pipe-table-align)))

;;;###autoload
(defun markdown-pipe-table-move-column-left ()
  "Move table column at point to the left."
  (interactive)
  (markdown-pipe-table-move-column 'left))

;;;###autoload
(defun markdown-pipe-table-move-column-right ()
  "Move table column at point to the right."
  (interactive)
  (markdown-pipe-table-move-column nil))

;;;###autoload
(defun markdown-pipe-table-next-row ()
  "Go to the next row (same column) in the pipe table.
Create new table lines if required."
  (interactive)
  (unless (markdown-pipe-table-at-point-p)
    (user-error "Not at a pipe table"))
  (if (or (looking-at "[ \t]*$")
          (save-excursion (skip-chars-backward " \t") (bolp)))
      (newline)
	(markdown-pipe-table-align)
    (let ((col (markdown-pipe-table-get-column)))
      (beginning-of-line 2)
      (if (or (not (markdown-pipe-table-at-point-p))
              (markdown-pipe-table-hline-at-point-p))
          (progn
            (beginning-of-line 0)
            (markdown-pipe-table-insert-row 'below)))
      (markdown-pipe-table-goto-column col)
      (skip-chars-backward "^|\n\r")
      (when (looking-at " ") (forward-char 1)))))

;;;###autoload
(defun markdown-pipe-table-forward-cell ()
  "Go to the next cell in the pipe table.
Create new table lines if required."
  (interactive)
  (unless (markdown-pipe-table-at-point-p)
    (user-error "Not at a pipe table"))
  (markdown-pipe-table-align)
  (let ((end (markdown-pipe-table-end)))
    (when (markdown-pipe-table-hline-at-point-p) (end-of-line 1))
    (condition-case nil
        (progn
          (re-search-forward "|" end)
          (if (looking-at "[ \t]*$")
              (re-search-forward "|" end))
          (if (and (looking-at "[-:]")
                   (re-search-forward "^[ \t]*|\\([^-:]\\)" end t))
              (goto-char (match-beginning 1)))
          (if (looking-at "[-:]")
              (progn
                (beginning-of-line 0)
                (markdown-pipe-table-insert-row 'below))
            (when (looking-at " ") (forward-char 1))))
      (error (markdown-pipe-table-insert-row 'below)))))

;;;###autoload
(defun markdown-pipe-table-backward-cell ()
  "Go to the previous cell in the pipe table."
  (interactive)
  (unless (markdown-pipe-table-at-point-p)
    (user-error "Not at a pipe table"))
  (markdown-pipe-table-align)
  (when (markdown-pipe-table-hline-at-point-p) (end-of-line 1))
  (condition-case nil
      (progn
        (re-search-backward "|" (markdown-pipe-table-begin))
        (re-search-backward "|" (markdown-pipe-table-begin)))
    (error (user-error "Cannot move to previous table cell")))
  (while (looking-at "|\\([-:]\\|[ \t]*$\\)")
    (re-search-backward "|" (markdown-pipe-table-begin)))
  (when (looking-at "| ?") (goto-char (match-end 0))))

;;;###autoload
(defun markdown-pipe-table-transpose ()
  "Transpose pipe table at point.
Horizontal separator lines will be eliminated."
  (interactive)
  (unless (markdown-pipe-table-at-point-p)
    (user-error "Not at a pipe table"))
  (let* ((table (buffer-substring-no-properties
                 (markdown-pipe-table-begin) (markdown-pipe-table-end)))
         ;; Convert table to a Lisp structure
         (table (delq nil
                      (mapcar
                       (lambda (x)
                         (unless (string-match-p
                                  markdown-pipe-table-hline-regexp x)
                           (markdown-split-string x "\\s-*|\\s-*")))
                       (markdown-split-string table "[ \t]*\n[ \t]*"))))
         (dline_old (markdown-pipe-table-get-dline))
         (col_old (markdown-pipe-table-get-column))
         (contents (mapcar (lambda (_)
                             (let ((tp table))
                               (mapcar
                                (lambda (_)
                                  (prog1
                                      (pop (car tp))
                                    (setq tp (cdr tp))))
                                table)))
                           (car table))))
    (goto-char (markdown-pipe-table-begin))
    (re-search-forward "|") (backward-char)
    (delete-region (point) (markdown-pipe-table-end))
    (insert (mapconcat
             (lambda(x)
               (concat "| " (mapconcat 'identity x " | " ) "  |\n"))
             contents ""))
    (markdown-pipe-table-goto-dline col_old)
    (markdown-pipe-table-goto-column dline_old))
  (markdown-pipe-table-align))

;;;###autoload
(defun markdown-pipe-table-sort-lines (with-case &optional sorting-type)
  "Sort table lines according to the column at point.

The position of point indicates the column to be used for
sorting, and the range of lines is the range between the nearest
horizontal separator lines, or the entire table of no such lines
exist. If point is before the first column, user will be prompted
for the sorting column. If there is an active region, the mark
specifies the first line and the sorting column, while point
should be in the last line to be included into the sorting.

The command then prompts for the sorting type which can be
alphabetically or numerically. Sorting in reverse order is also
possible. With prefix argument WITH-CASE, alphabetic sorting will
be case-sensitive.

If SORTING-TYPE is specified when this function is called from a
Lisp program, no prompting will take place. SORTING-TYPE must be
a character, any of (?a ?A ?n ?N) where the capital letters
indicate that sorting should be done in reverse order."
  (interactive "P")
  (unless (markdown-pipe-table-at-point-p)
    (user-error "Not at a pipe table"))
  ;; Set sorting type and column used for sorting
  (let ((column (let ((c (markdown-pipe-table-get-column)))
                  (cond ((> c 0) c)
                        ((called-interactively-p 'any)
                         (read-number "Use column N for sorting: "))
                        (t 1))))
        (sorting-type
         (or sorting-type
             (read-char-exclusive
              "Sort type: [a]lpha [n]umeric (A/N means reversed): "))))
    (save-restriction
      ;; Narrow buffer to appropriate sorting area
      (if (region-active-p)
          (narrow-to-region
           (save-excursion
             (progn
               (goto-char (region-beginning)) (line-beginning-position)))
           (save-excursion
             (progn
               (goto-char (region-end)) (line-end-position))))
        (let ((start (markdown-pipe-table-begin))
              (end (markdown-pipe-table-end)))
          (narrow-to-region
           (save-excursion
             (if (re-search-backward
                  markdown-pipe-table-hline-regexp start t)
                 (line-beginning-position 2)
               start))
           (if (save-excursion (re-search-forward
                                markdown-pipe-table-hline-regexp end t))
               (match-beginning 0)
             end))))
      ;; Determine arguments for `sort-subr'
      (let* ((sort-fold-case (not with-case))
             (extract-key-from-cell
              (cl-case sorting-type
                ((?a ?A) #'markdown-sort-remove-markup) ;; #'identity)
                ((?n ?N) #'string-to-number)
                (t (user-error "Invalid sorting type: %c" sorting-type))))
             (predicate
              (cl-case sorting-type
                ((?n ?N) #'<)
                ((?a ?A) #'string<))))
        ;; Sort selected area
        (goto-char (point-min))
        (sort-subr (memq sorting-type '(?A ?N))
                   (lambda ()
                     (forward-line)
                     (while (and (not (eobp))
                                 (not (looking-at
                                       markdown-pipe-table-dline-regexp)))
                       (forward-line)))
                   #'end-of-line
                   (lambda ()
                     (funcall extract-key-from-cell
                              (markdown-pipe-table-get-cell column)))
                   nil
                   predicate)
        (goto-char (point-min))))))

;;;###autoload
(defun markdown-pipe-table-convert-region (begin end &optional separator)
  "Convert region from BEGIN to END to a pipe table.

If every line contains at least one TAB character, the function
assumes that the material is tab separated (TSV). If every line
contains a comma, comma-separated values (CSV) are assumed. If
not, lines are split at whitespace into cells.

You can use a prefix argument to force a specific separator: C-u
forces CSV, C-u C-u forces TAB, C-u C-u C-u will prompt for a
regular expression to match the separator, and a numeric argument
C-u N indicates that at least N consecutive spaces, or
alternatively a TAB should be used as the separator."
  (interactive "r\nP")
  (let* ((begin (min begin end)) (end (max begin end)) re)
    (goto-char begin) (beginning-of-line 1)
    (setq begin (point-marker))
    (goto-char end)
    (if (bolp) (backward-char 1) (end-of-line 1))
    (setq end (point-marker))
    (when (equal separator '(64))
      (setq separator (read-regexp "Regexp for cell separator: ")))
    (unless separator
      ;; Get the right cell separator
      (goto-char begin)
      (setq separator
            (cond
             ((not (re-search-forward "^[^\n\t]+$" end t)) '(16))
             ((not (re-search-forward "^[^\n,]+$" end t)) '(4))
             (t 1))))
    (goto-char begin)
    (if (equal separator '(4))
        ;; Parse CSV
        (while (< (point) end)
          (cond
           ((looking-at "^") (insert "| "))
           ((looking-at "[ \t]*$") (replace-match " |") (beginning-of-line 2))
           ((looking-at "[ \t]*\"\\([^\"\n]*\\)\"")
            (replace-match "\\1") (if (looking-at "\"") (insert "\"")))
           ((looking-at "[^,\n]+") (goto-char (match-end 0)))
           ((looking-at "[ \t]*,") (replace-match " | "))
           (t (beginning-of-line 2))))
      (setq re
            (cond
             ((equal separator '(4))  "^\\|\"?[ \t]*,[ \t]*\"?")
             ((equal separator '(16)) "^\\|\t")
             ((integerp separator)
              (if (< separator 1)
                  (user-error "Cell separator must contain one or more spaces")
                (format "^ *\\| *\t *\\| \\{%d,\\}" separator)))
             ((stringp separator) (format "^ *\\|%s" separator))
             (t (error "Invalid cell separator"))))
      (while (re-search-forward re end t) (replace-match "| " t t)))
    (goto-char begin)
    (markdown-pipe-table-align)))

;;;; Functions for context-sensitive key bindings

(defun markdown-enter ()
  (interactive)
  (cond ((markdown-pipe-table-at-point-p)
         (call-interactively #'markdown-pipe-table-next-row))
        (t (call-interactively #'markdown-enter-key))))

(defun markdown-tab ()
  (interactive)
  (cond ((markdown-pipe-table-at-point-p)
         (call-interactively #'markdown-pipe-table-forward-cell))
        (t (call-interactively #'markdown-cycle))))

;; FIXME: Adapt original `markdown-shifttab' function
(defun markdown-shifttab-1 ()
  (interactive)
  (cond ((markdown-pipe-table-at-point-p)
         (call-interactively #'markdown-pipe-table-backward-cell))
        (t (call-interactively #'markdown-shifttab))))

(defun markdown-metaup ()
  (interactive)
  (cond ((markdown-pipe-table-at-point-p)
         (call-interactively #'markdown-pipe-table-move-row-up))
        (t (call-interactively #'markdown-move-up))))

(defun markdown-metadown ()
  (interactive)
  (cond ((markdown-pipe-table-at-point-p)
         (call-interactively #'markdown-pipe-table-move-row-down))
        (t (call-interactively #'markdown-move-down))))

(defun markdown-metaleft ()
  (interactive)
  (when (markdown-pipe-table-at-point-p)
    (call-interactively #'markdown-pipe-table-move-column-left)))

(defun markdown-metaright ()
  (interactive)
  (when (markdown-pipe-table-at-point-p)
    (call-interactively #'markdown-pipe-table-move-column-right)))

(defun markdown-shiftmetaup ()
  (interactive)
  (when (markdown-pipe-table-at-point-p)
    (call-interactively #'markdown-pipe-table-delete-row)))

(defun markdown-shiftmetadown ()
  (interactive)
  (when (markdown-pipe-table-at-point-p)
    (call-interactively #'markdown-pipe-table-insert-row)))

(defun markdown-shiftmetaleft ()
  (interactive)
  (when (markdown-pipe-table-at-point-p)
    (call-interactively #'markdown-pipe-table-delete-column)))

(defun markdown-shiftmetaright ()
  (interactive)
  (when (markdown-pipe-table-at-point-p)
    (call-interactively #'markdown-pipe-table-insert-column)))

(define-key markdown-mode-map (kbd "<tab>") 'markdown-tab)
(define-key markdown-mode-map (kbd "S-<tab>") 'markdown-shifttab-1)
(define-key markdown-mode-map (kbd "<backtab>") 'markdown-shifttab-1)
(define-key markdown-mode-map (kbd "<S-iso-lefttab>") 'markdown-shifttab-1)

(define-key markdown-mode-map (kbd "<return>") 'markdown-enter)
(define-key markdown-mode-map (kbd "M-<up>") 'markdown-metaup)
(define-key markdown-mode-map (kbd "M-<down>") 'markdown-metadown)
(define-key markdown-mode-map (kbd "M-<right>") 'markdown-metaright)
(define-key markdown-mode-map (kbd "M-<left>") 'markdown-metaleft)
(define-key markdown-mode-map (kbd "M-S-<up>") 'markdown-shiftmetaup)
(define-key markdown-mode-map (kbd "M-S-<down>") 'markdown-shiftmetadown)
(define-key markdown-mode-map (kbd "M-S-<left>") 'markdown-shiftmetaleft)
(define-key markdown-mode-map (kbd "M-S-<right>") 'markdown-shiftmetaright)

(define-key markdown-mode-map (kbd "C-c ^") 'markdown-pipe-table-sort-lines)
(define-key markdown-mode-map (kbd "C-c |") 'markdown-pipe-table-convert-region)
(define-key markdown-mode-map (kbd "C-c C-x C-t") 'markdown-pipe-table-transpose)

(provide 'markdown-mode-table)

;;; markdown-mode-table.el ends here

