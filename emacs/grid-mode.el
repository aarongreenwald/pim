;; Currently based on csv-mode, can it be based on ctable?
;;;;;;;;;;;;;;;;;
;; Look into ses-mode, other grid-modes? cell-mode? ftable? table.el? 
;; https://vallyscode.github.io/note/tabulated-list-mode/

;; pim-grid should allow arbitrary funtions on cell under point as well row perhaps
(define-derived-mode pim-grid-mode csv-mode "pim-grid"
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

(defun generate-column-model (headers)
  "Expects a list of headers"
  (mapcar #'generate-column headers)
  )

(defun generate-column (name)
  ;; TODO can I show elipsis when the width truncated?
  ;; ctbl:show-cell-in-tooltip automatically whenever entering a truncated cell? 
  (make-ctbl:cmodel
   :title name
   :sorter 'ctbl:sort-number-lessp ;;todo what can I do with this?
   ;;   :min-width 50
   :max-width (cond ((member name '("sha1" "sha256")) 15)
		(40))
   :align (cond ((member name '("usd" "ils" "amount")) 'right)
		('left))
   ;; :click-hooks ;;header click hooks
   )
  )

(defun insert-to-pim-grid-buffer (bufname text)
  "Expects a csv in text, inserts to a buffer and sets to pim-grid-mode"
  (setq list (parse-csv-to-list text))
  (setq headers (pop list))

  (setq param (copy-ctbl:param ctbl:default-rendering-param))

  ;; todo styling the table
  ;; (setf (ctbl:param-fixed-header param) t) ; set header parameters
  ;; (setf (ctbl:param-hline-colors param)    ; horizontal line color
  ;;       '((0 . "#00000") (1 . "#909090") (-1 . "#ff0000") (t . "#00ff00"))) 
  ;; (setf (ctbl:param-draw-hlines param)     ; horizontal line draw conditions
  ;;       (lambda (model row-index)
  ;;         (cond ((memq row-index '(0 1 -1)) t)
  ;;               (t (= 0 (% (1- row-index) 5))))))
  ;; (setf (ctbl:param-bg-colors param)       ; cell background color
  ;;       (lambda (model row-id col-id str)
  ;;         (cond ((string-match "CoCo" str) "LightPink")
  ;;               ((= 0 (% (1- row-index) 2)) "Darkseagreen1")
  ;;               (t nil))))

  (let*( (column-model (generate-column-model headers))
    (data list)
    (model 
     (make-ctbl:model
      :column-model column-model
      :data data))
    (component
     (ctbl:create-table-component-buffer
      :model model
      :param param)))
  

    ;; Click hook sample for tables of known type
    ;; For everything else, set a variable with the current row
    ;; in ctbl:cp-add-selection-change-hook and then use it in other functions
    ;; See https://github.com/kiwanami/emacs-ctable#event-handling

    ;; (ctbl:cp-add-selection-change-hook
    ;;  component (lambda () (message "CTable : Select Hook %S"
    ;;                       (ctbl:cp-get-selected component))))
    
    ;; (ctbl:cp-add-click-hook
    ;;  component (lambda () 
    ;;            (let ((row (ctbl:cp-get-selected-data-row component)))
    ;;              (message "CTable : Click Hook [%S]" row)
    ;;              ;; increment ID column
    ;;              (when (= 0 (cdr (ctbl:cp-get-selected component)))
    ;;                (message ">> %S" row)
    ;;                (cl-incf (car row)))
    ;;              (ctbl:cp-update component)))) 

    ;; TODO use a named buffer, optionally discard with q, and
    ;; set a custom mode in the buffer that derives from whatever mode
    ;; ctbl uses (table-mode?)
    ;; Note: I can add keybindings to ctbl:table-mode-map

    ;; more samples: https://github.com/kiwanami/emacs-ctable/blob/master/samples/large-table.el
    (pop-to-buffer (ctbl:cp-get-buffer component))

    ;; move to the custom mode
    (display-line-numbers-mode 0)
    ) 

  ;; The old way
  ;;(insert-text-to-buffer bufname text)
  ;;(pim-grid-mode)
  )
