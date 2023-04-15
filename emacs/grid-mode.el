;;;;;;;;;;;;;;;;;
;; Look into ses-mode, other grid-modes? cell-mode? ftable? table.el? 
;; https://vallyscode.github.io/note/tabulated-list-mode/

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

(define-derived-mode pim-grid-mode ctbl:table-mode "pim-grid"
  "Major mode for output of pim sql queries."  
  (display-line-numbers-mode 0)
  (goto-line 3) ;skip the header and separated
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

  ;; open-table-buffer expects passed buffer to exist if it doesn't
  (get-buffer-create bufname)
  (pop-to-buffer bufname)

  (let*( (column-model (generate-column-model headers))
    (data list)
    (model 
     (make-ctbl:model
      :column-model column-model
      :data data))
    (component
     ;; this approach works, but setting the mode to pim-grid-mode breaks
     ;; ctbl:component
     (ctbl:open-table-buffer
      :model model
      :param param
      :buffer bufname)))    
;;    (pim-grid-mode)

     ;; Alternative to open-table-buffer: get the raw text so we can insert directly to the buffer we choose
     ;; https://github.com/kiwanami/emacs-ctable/blob/48b73742757a3ae5736d825fe49e00034cc453b5/readme.md#view-components
     ;; (ctbl:get-table-text      
     ;;  :model model
     ;;  :param param)))

    ;; (insert-text-to-buffer bufname component)
    ;; (pim-grid-mode)   
    ;; (setq-local ctbl:component component) ;;doesn't seem to work right

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
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

    ))
