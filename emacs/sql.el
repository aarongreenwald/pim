;; Interactive sql: M-x sql-sqlite
(defun csv-to-ctbl (csv)
  "Shows a csv in a ctable grid"
  (ctbl:popup-table-buffer-easy (parse-csv-to-list csv))
 )

(defun parse-csv-line (line)
  (split-string line ",")
  )

(defun parse-csv-to-list (csv)
  "Given a csv, returns a list of lists that can be fed to ctbl"
  (setq lines (split-string csv "\n"))
  (mapcar #'parse-csv-line lines)
  )

(defun csv-to-pim-dir (csv)
  "Given a csv, create a pim-dir buffer"
  ;; think about the API here. I can convert to lists first, or just iterate over the
  ;; csv and insert to the buffer properly formatted
  (setq lines (split-string csv "\n"))
  (dolist (line lines)
    (insert "\n")
    (insert (concat "Formatting" line))
    )
  )


(defun gen-row (lst)

)
(defun insert-csv-line (txt)
  (setq lst (progn
          (insert "\n<table>\n")
          (setq str txt)
          (gen-row (split-string str ","))
          (insert "\n</table>\n")
          ))
  )


