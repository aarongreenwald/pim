;; install package "plz", ctable
(require 'plz)
(require 'ctable)
(require 'cl)
(require 'color)
(require 'csv-mode)

;;TODO - how to get json / csv into ctbl? 
;; (ctbl:popup-table-buffer-easy 
;;  '((1 2 3 4) (5 6 7 8) (9 10 11 12)))

;;;;;;;;;;;;;;;;;
;; Look into ses-mode, other grid-modes? cell-mode? ftable? table.el? 
;; https://vallyscode.github.io/note/tabulated-list-mode/

(defvar pim-mode-current-dir nil
  "Name of the current directory in pim-mode")
(make-variable-buffer-local 'pim-mode-current-dir)

(defun csv-text (bufname text dir)
   "Inserts text into BUFNAME and sets up pim-dir-mode."
   (get-buffer-create bufname)
   (pop-to-buffer bufname)
   ;; so that the insert will work, pim-dir-mode will set it back to read-only
   (read-only-mode 0)
   (erase-buffer)
   (insert text)
   (pim-dir-mode)
   (setq pim-mode-current-dir dir)
   )

;; gimmick copied from the internet - delete eventually
;; once I have better coloring
(defun color-columns  (&optional separator)
  ;;(font-lock-mode 1) ;;not sure what this is for? 
  (let* ((separator (or separator ?\,))
         (n (count-matches (string separator) (point-at-bol) (point-at-eol)))
         (colors (loop for i from 0 to 1.0 by (/ 1.0 n)
                       collect (apply #'color-rgb-to-hex 
                                      (color-hsl-to-rgb i 0.3 0.5)))))
  
  (loop for i from 1 to n
	for c in colors
        for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
	do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))

(defvar csv-fields-list nil
  "List of field names in the csv")
(make-variable-buffer-local 'csv-field-index-header-fields-list)

(defun set-widths (&optional separator)
  "Sets custom column widths in csv-mode"
  (let ((i 0))
    (dolist (field csv-fields-list)
      (setq i (+ i 1))
      ;; TODO more heuristics based on column name
      (if (member field '("sha1" "sha256"))
	  (csv-align-set-column-width i 10)	
      ))))
       

(defun csv-populate-fields-list ()
  (save-excursion
    (goto-char (point-min))
    (let* ((text (buffer-substring (point) (line-end-position)))
           (fields (split-string text ",")))
      (setq csv-fields-list fields))))

;; TODO how does autoload work?
;; TODO separate into pim-table and pim-dir
;;;###autoload
(define-derived-mode pim-dir-mode csv-mode "pim-dir"
  "Major mode for pim drive directories."
  (csv-populate-fields-list)
  (csv-header-line)
  (csv-align-mode)
  (read-only-mode)
  (color-columns)
  (display-line-numbers-mode 0)
  (set-widths)
  
  ;; after inserting text we're at the end
  ;; this might not be the best place to do this
  ;; (goto-char 1)
  (goto-line 2) ;skip the header. todo: remove/hide the header
  )

(define-key pim-dir-mode-map (kbd "r") 'pim-refresh-dir)
(define-key pim-dir-mode-map (kbd "e") 'pim-open-dir)
(define-key pim-dir-mode-map (kbd "b") 'pim-back-dir)

(defun pim-run-query (query bufname)
  (plz 'post "http://localhost:4321/api/queries/exec"
       :headers '(("Content-Type" . "application/json"))
       :body (json-encode  `(("format" . "csv")
			     ("sql" . ,query)))
       ;;  :as #'json-read
       :then `(lambda (alist)
	       (progn
		 (csv-text ',bufname alist)
		 (message "Completed query"))))

  )

(defun pim-ls-dir (&optional dir)
  (interactive)
  (or dir (setq dir "./"))

  (message (concat "Loading: " dir))
  
  (plz 'post "http://localhost:4321/api/drive/ls-dir"
    :headers '(("Content-Type" . "application/json"))
    :body (json-encode  `(("format" . "csv")
			  ("path" . ,dir)))
    ;;  :as #'json-read
    :then `(lambda (result)
	     (progn
	       (csv-text "ls-dir" result ,dir)
	       (message (concat "Retrieved: " ,dir))))))

(defun pim-open-dir ()
  (interactive)  
  (setq line (buffer-substring (line-beginning-position) (line-end-position)))
  (csv-align-mode)
  (setq fields (split-string line ","))
  (setq dir (nth 0 fields)) ;; TODO improve this logic, use named fields
  (csv-align-mode)
  (setq fullname (concat pim-mode-current-dir dir "/"))
  (pim-ls-dir fullname)
  )

(defun pim-refresh-dir ()
  (interactive)
  (pim-ls-dir pim-mode-current-dir)
  )


(defun pim-back-dir ()
  (interactive)
  (if (string-equal pim-mode-current-dir "./") ;; todo this isn't working
      (message "Already at root")
    (progn
        (setq sections (split-string pim-mode-current-dir "/"))
	(setq dir (concat (string-join (butlast sections 2) "/") "/"))
	(pim-ls-dir dir)	
	)    
    ))

(defun pim-sample-query ()
  (interactive)
  (pim-run-query "select * from v_file limit 50" "pim-query"))

