;;; Insight --- Summary
;;;
;;; Commentary:
;;;
;;; Functions for creating views into tagged lists
;;; Here, used to track desired capabilities for the STARK system
;;;
;;; Code:

(defun ist-number-caps ()
  "Number my list of desired capabilities."
  (interactive)
  (let ((count 1))
    (while (not (eobp))
      (search-forward "-")
      (backward-char 1)
      (if (bolp)
          (progn
            (delete-char 1)
            (insert (format "%03d." count))
            (setq count (+ count 1)))
        (forward-char 1))
      )))

(defun ist-add-dividers ()
  "Place dividers in list to add category annotations."
  (interactive)
  (while (not (eobp))
    (search-forward ". ")
    (insert "| ")))

(defun ist-indent-caps ()
  "Indent extra lines of capabilities properly."
  (interactive)
  (while (not (eobp))
    (search-forward "|")
    (forward-char 2)
    (let ((col (- (current-column) 1)))
      (fill-paragraph)
      (forward-line)
      (back-to-indentation)
      (if (> col 0)
          (while (not (= col (current-column)))
            (insert " "))))))

(defun ist-cat-query-caps (lis)
  "Narrow the capability list to those matching a category query (LIS)."
  (interactive "sCategory List: ")
  (let ((args (split-string lis))
        (bufname (format "*cap-search [%s]*" lis))
        (oldbuf (current-buffer))
        (count 0))
    (with-current-buffer (get-buffer-create bufname)
      (erase-buffer))
    (save-excursion
      (while (search-forward ". " nil t)
        (let* ((beg (point))
               (end (- (search-forward "|") 2))
               (cats (split-string (buffer-substring-no-properties beg end)))
               (match t))
          (if (dolist (arg args match)
                (if (not (member arg cats))
                    (setq match nil)))
              (progn (setq count (+ 1 count))
                     (setq beg (line-beginning-position))
                     (while (not (or (and (bolp) (= (following-char) 10)) (eobp)))
                       (forward-line))
                     (setq end (if (eobp)
                                   (point-max)
                                 (+ (point) 1)))
                     (with-current-buffer bufname
                       (insert-buffer-substring oldbuf beg end)))))))
    (pop-to-buffer bufname '(display-buffer-reuse-mode-window (mode . fundamental-mode)))
    (goto-char (point-min))
    (insert (format "From: %s\nSearched: %s\nTotal: %d\n\n" oldbuf lis count))))

(defun ist-str-query-caps (str)
  "Narrow the capability list to those matching a string query (STR)."
  (interactive "sSearch String: ")
  (let ((bufname (format "*cap-search ['%s']*" str))
        (oldbuf (current-buffer))
        (count 0))
    (with-current-buffer (get-buffer-create bufname)
      (erase-buffer))
    (save-excursion
      (while (search-forward "| " nil t)
        (let ((beg (point))
              (end (progn
                     (while (not (or (and (bolp) (= (following-char) 10)) (eobp)))
                       (forward-line))
                     (point))))
          (goto-char beg)
          (if (word-search-forward-lax str end t)
              (progn (setq count (+ 1 count))
                     (search-backward ". ")
                     (setq beg (line-beginning-position))
                     (while (not (or (and (bolp) (= (following-char) 10)) (eobp)))
                       (forward-line))
                     (setq end (if (eobp)
                                   (point-max)
                                 (+ (point) 1)))
                     (with-current-buffer bufname
                       (insert-buffer-substring oldbuf beg end)))))))
    (pop-to-buffer bufname '(display-buffer-reuse-mode-window (mode . fundamental-mode)))
    (goto-char (point-min))
    (insert (format "From: %s\nSearched: '%s'\nTotal: %d\n\n" oldbuf str count))))

(defun ist-kill-cap-search ()
  "Kill all extant capability search buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (if (string-prefix-p "*cap-search" (buffer-name buffer))
        (kill-buffer buffer))))

(defun ist-cap-word-freq ()
  "Count word frequencies in a capability list."
  (interactive)
  (let ((bufname "*word-freq*")
        (oldbuf (current-buffer))
        (words ())
        (count ()))
    (with-current-buffer (get-buffer-create bufname)
      (erase-buffer))
    (save-excursion
      (while (search-forward "| " nil t)
        (let ((beg (point))
              (end (progn
                     (while (not (or (and (bolp) (= (following-char) 10)) (eobp)))
                       (forward-line))
                     (point))))
          (setq words (append words (split-string
                                     (downcase (buffer-substring-no-properties beg end))
                                     "[ _/(),.\"\f\t\n\r\v]+"))))))
    (dolist (word words)
      (if (assoc word count)
          (cl-incf (cdr (assoc word count)))
        (push (cons word 1) count)))
    (setq count (sort count (lambda (a b) (> (cdr a) (cdr b)))))
    (with-current-buffer bufname
      (insert "| word | # |\n|-\n")
      (dolist (word count)
        (insert (format "| %s | %d |\n" (car word) (cdr word))))
      (goto-char (point-min))
      (org-table-align))
    (pop-to-buffer bufname)))
