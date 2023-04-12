;; install package "plz", ctable, csv-mode
(require 'plz)
(require 'ctable)

;;TODO - how to get json / csv into ctbl? 
;; (ctbl:popup-table-buffer-easy 
;;  '((1 2 3 4) (5 6 7 8) (9 10 11 12)))

(defun csv-text (bufname text)
   "Inserts text into BUFNAME and formats as csv."
   (get-buffer-create bufname)
   (pop-to-buffer bufname)
   (erase-buffer)
   (csv-mode)
   (csv-header-line)
   (csv-align-mode)
   (insert text)
   (goto-char 1))

;;;;;;;;;;;;;
;; Look into ses-mode, other grid-modes? cell-mode? ftable? csv-mode? table.el? 
;; https://vallyscode.github.io/note/tabulated-list-mode/

(plz 'post "http://localhost:4321/api/queries/exec"
  :headers '(("Content-Type" . "application/json"))
  :body (json-encode '(("format" . "csv")
		       ("sql" . "select * from v_file limit 50")))
;;  :as #'json-read
  :then (lambda (alist)
	  (progn
	    (csv-text "Query results" alist)
	    (message "Done"))))
