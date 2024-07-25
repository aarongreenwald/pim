(defun parse-csv-line-to-list (line)
  (split-string line ",")
  )

(defun parse-csv-to-list (csv)
  ;; TODO: handle escaping somehow. And consider the best place to handle formatting
  ;; eg rounding currency numbers, human readable byte-sizes, date formatting, etc
  ;; I can push the logic down to sql for reusability but if I want to be able to
  ;; expose/use the raw value I need to send it all to the frontend and hide it in
  ;; text properties. 
  "Given a csv, returns a list of lists that can be fed to ctbl"
  (setq lines (split-string csv "\n"))
  (mapcar #'parse-csv-line-to-list lines)
  )

(defun insert-text-to-buffer (bufname text)
  "Inserts text into BUFNAME and opens it."
   (get-buffer-create bufname)
   (pop-to-buffer bufname)
   ;; so that the insert will work, the mode will set it back to read-only
   (read-only-mode 0)
   (erase-buffer)
   (insert text))

(defun csv-to-ctbl (csv)
  "Shows a csv in a ctable grid"
  (ctbl:popup-table-buffer-easy (parse-csv-to-list csv))
  )

(defun color-columns  (&optional separator)
  "Gimmick I copied from the internet, colors columns in csv-mode. Delete eventually"
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
  "Assuming a mode based on csv-mode: list of field names in the csv")
(make-variable-buffer-local 'csv-field-index-header-fields-list)

(defvar csv-fields-list nil
  "Assuming a mode based on csv-mode: list of field names in the csv")
(make-variable-buffer-local 'csv-field-index-header-fields-list)

(defun csv-populate-fields-list ()
  (save-excursion
    (goto-char (point-min))
    (let* ((text (buffer-substring (point) (line-end-position)))
           (fields (split-string text ",")))
      (setq csv-fields-list fields))))

(defun set-widths (&optional separator)
  "Sets custom column widths in csv-mode"
  (let ((i 0))
    (dolist (field csv-fields-list)
      (setq i (+ i 1))
      ;; TODO more heuristics based on column name
      (if (member field '("sha1" "sha256"))
	  (csv-align-set-column-width i 10)	
      ))))
       

;; pim-grid based on csv-mode, deprecated in favor of ctable
;; pim-grid should allow arbitrary funtions on cell under point as well row perhaps
(define-derived-mode pim-grid-deprecated-mode csv-mode "pim-grid-deprecated"
  "Major mode for output of pim sql queries."
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



;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Menu - Sample Code
;;;;;;;;;;;;;;;;;;;;;;;


(defun query-pim ()
  (interactive)
  (shell-command "echo hello"))

(defun create-note ()
  (interactive)
  (shell-command "echo hello")
  )


(define-key-after
  global-map
  [menu-bar pim-menu]
  (cons "PIM" (make-sparse-keymap "hoot hoot"))
  'tools )

(define-key
  global-map
  [menu-bar pim-menu query-pim]
  '("Query Database" . query-pim))


(define-key
  global-map
  [menu-bar pim-menu create-note]
  '("Create Note" . create-note))
