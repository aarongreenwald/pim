(global-set-key (kbd "C-<f11>") 'pim-sql-scratch)

;; todo delay until sql is loaded

(add-hook 'sql-mode-hook (lambda () (define-key sql-mode-map (kbd "C-e") 'pim-exec-query-selection)))

(add-hook 'sql-mode-hook (lambda () (define-key sql-mode-map (kbd "C-E") 'pim-exec-query-selection-write)))

(add-hook 'sql-mode-hook (lambda ()
			   (setq pim-sql-current-overlay (make-overlay 1 1))
			   (overlay-put pim-sql-current-overlay 'face '(:background "brightblack"))))

;; Interactive sql: M-x sql-sqlite
;;;;;;;;;;;;;;;;;;;;;;;
;; How to get sqli mode to output to a grid?

(defun pim-run-query (query bufname write-mode &optional keymap temp-buffer)
  (plz 'post "http://localhost:4321/api/queries/exec"
       :headers '(("Content-Type" . "application/json"))
       :body (json-encode  `(("format" . "csv")
			     ("sql" . ,query)
			     ("writeMode" . ,write-mode)))
       ;;  :as #'json-read
       :then `(lambda (data)
		(progn
		  (if (equal ',write-mode 0) 
		      (insert-to-pim-grid-buffer ',bufname data ',keymap ',temp-buffer)
		    (message (concat "Succeeded: " data)))))
  ))

;; todo run sql query under point
(defun pim-exec-query-selection ()
  (interactive)
  (pim-highlight-current-sql-statement)
  ;; todo save execution history (comint?)
  (message "Running query...")
  ;; (setq sql (buffer-substring-no-properties (mark) (point)))
  (setq sql (pim-get-current-sql-statement))
  (pim-run-query sql "pim-query" 0))

(defun pim-exec-query-selection-write ()
  (interactive)
  (pim-highlight-current-sql-statement)
  (message "Executing query...")
  (setq sql (buffer-substring-no-properties (mark) (point)))
  (pim-run-query sql nil 1)
  )

(defun pim-get-current-sql-statement-region ()
  (interactive)
  
  (save-excursion
    (search-backward "\n\n" nil -1)
    (unless (equal (point) 1) (forward-char))
    (setq begin (point))
    (search-forward "\n\n" nil -1)
    (setq end (point))
    (list begin end)))

(defun pim-get-current-sql-statement ()
  (interactive)
  (setq vals (pim-get-current-sql-statement-region))
  (setq begin (nth 0 vals))
  (setq end (nth 1 vals))
  (buffer-substring-no-properties begin end))



;; todo this should be called constantly? Or only on demand? 
(defun pim-highlight-current-sql-statement ()
  (interactive)
  (setq vals (pim-get-current-sql-statement-region))
  (setq begin (nth 0 vals))
  (setq end (nth 1 vals))
  (move-overlay pim-sql-current-overlay begin end (current-buffer)))


(defun pim-sql-scratch ()
  (interactive)
  ;; todo save contents to tmp file in home
  (get-buffer-create "pim-sql_scratch")
  (pop-to-buffer "pim-sql_scratch")
  (sql-mode))



