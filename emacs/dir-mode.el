;; TODO how does autoload work?
;; TODO separate into pim-table and pim-dir
;; pim-table: generic grid for query output
;; pim-dir: custom output, don't base on csv-mode for finer control
;; pim-dir can also be used in "file-mode", ie results of search that aren't
;; a single directory, some different columns and different shortcuts
;; pim-table should allow arbitrary funtions on cell under point as well row perhaps
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

(defun send-to-pim-dir-buffer (data dir)
  (csv-text "ls-dir" data)
  (pim-dir-mode)
  (setq pim-mode-current-dir dir))

(defvar pim-mode-current-dir nil
  "Name of the current directory in pim-mode")
(make-variable-buffer-local 'pim-mode-current-dir)


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
	       (send-to-pim-dir-buffer result ,dir)
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

(define-key pim-dir-mode-map (kbd "r") 'pim-refresh-dir)
(define-key pim-dir-mode-map (kbd "e") 'pim-open-dir)
(define-key pim-dir-mode-map (kbd "b") 'pim-back-dir)
