;;;;;;;;;;;;;;;;;
;; Look into ses-mode, other grid-modes? cell-mode? ftable? table.el? 
;; https://vallyscode.github.io/note/tabulated-list-mode/
;; more samples: https://github.com/kiwanami/emacs-ctable/blob/master/samples/large-table.el
;; https://github.com/kiwanami/emacs-ctable/blob/48b73742757a3ae5736d825fe49e00034cc453b5/ctable.el

;;;;;;;;;;;;;;;;;;
;; Documentation: 
;; * hjkl to navigate, also pnbf
;; * c: jump to column
;; * e, a: first, last columns
;; TODO:
;; * How to add the keymap override to the doc? 
;; * mark/unmark cells/rows, change their color and use their values in functions (sum, avg, etc)
;; * Common functions on current value that run queries, ideally pass a specific keymap when inserting to grid that specifies which keys call which functions, the rest can be called interactively. On a generic sql query, there are no special functions
;; * Optionally set buffer to be disposable instead of reusing a permanent buffer, so that stacking is possible

(defun insert-to-pim-grid-buffer (bufname text &optional keymap temp-buffer)
  "Expects a csv in text, inserts to a buffer and sets to pim-grid-mode"
  (setq data-list (parse-csv-to-list text))
  (setq headers (pop data-list))

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

  (setq pim-grid-data (list headers data-list))
  
  (let* ((column-model (generate-column-model headers))
    (data data-list)
    (model 
     (make-ctbl:model
      :column-model column-model
      :data data))
    component)    

    (setq component
	  ;; this approach works, but setting the mode to pim-grid-mode breaks
	  ;; ctbl:component? Timing might be an issue
	  (ctbl:open-table-buffer
	   :model model
	   :param param	   
	   :buffer bufname
	   :custom-map keymap))
     ;; Alternative to open-table-buffer: get the raw text so we can insert directly to the buffer we choose
     ;; https://github.com/kiwanami/emacs-ctable/blob/48b73742757a3ae5736d825fe49e00034cc453b5/readme.md#view-components
     ;; (ctbl:get-table-text      
     ;;  :model model
     ;;  :param param)))

    ;; (insert-text-to-buffer bufname component)
    ;; (pim-grid-mode)   
    ;; (setq-local ctbl:component component) ;;doesn't seem to work right
    
    (ctbl:cp-add-selection-change-hook
     ctbl:component (lambda ()
		      ;; shut off line numbers because for some reason the line numbers
		      ;; mode is turned on only on the first selection change
		      (if display-line-numbers-mode (display-line-numbers-mode 0))
		      (ctbl:show-cell-in-tooltip)))

    (pim-grid-mode 1)
    (if temp-buffer (local-set-key (kbd "q") 'kill-buffer-and-window))
    ))


(defun generate-column-model (headers)
  "Expects a list of headers"
  (mapcar #'generate-column headers))

(defun generate-column (name)
  ;; TODO can I show elipsis when the width truncated?
  ;; ctbl:show-cell-in-tooltip automatically whenever entering a truncated cell? 
  (make-ctbl:cmodel
   :title name
   :sorter 'ctbl:sort-number-lessp ;;todo what can I do with this?
   ;;   :min-width 50
   :max-width (cond ((member name '("sha1" "sha256")) 15)
		(40))
   :align (cond ((member name '("usd" "ils" "amount" "size")) 'right)
		('left))
   ;; :click-hooks ;;header click hooks
   ))


(define-minor-mode pim-grid-mode
  "Minor mode for output of pim sql queries.

Ideally this would be a major mode derived from ctbl:table-mode 
but that breaks the state inside the mode (ctbl:component etc)
So this will have to do. 
"
  :init-value nil
  :lighter " pim-grid"

  )

(add-hook 'pim-grid-mode-on-hook
	  (lambda ()
	    (display-line-numbers-mode 0)
	    ))

;; Sample code for processing data
;; (defun ctbl:cp-get-selected-data-cell (component)
;;   "Return the selected cell data. If no cell is selected, return nil."
;;   (let* ((rows (ctbl:component-sorted-data component))
;;          (cell-id (ctbl:component-selected component))
;;          (row-id (car cell-id)) (col-id (cdr cell-id)))
;;     (if row-id
;;         (nth col-id (nth row-id rows))
;;       nil)))

(defun pim-query-get-selected-cell-name ()
  (interactive)
  (setq column (cdr (ctbl:cp-get-selected ctbl:component)))
  (nth column (nth 0 pim-grid-data)))

(defun pim-query-get-selected-cell-value ()
  (interactive)
  (ctbl:cp-get-selected-data-cell ctbl:component))

(defun pim-query-get-selected-row()
  (interactive)
  ;;todo consider converting list to alist using the column headers
  ;;in (nth 0 pim-grid-data)
  (ctbl:cp-get-selected-data-row ctbl:component))

(defvar pim-grid-data nil
  "Raw data in the grid - first element is headers, second element is list of lists (rows of fields)")
(make-variable-buffer-local 'pim-grid-data)

;; TODO this pollutes ctbl, if I could get pim-grid to work as a major/minor mode
;; reliably this can be moved to there. 
(define-key ctbl:table-mode-map (kbd "s") 'ctbl:show-cell-in-tooltip)
(define-key ctbl:table-mode-map (kbd "?") 'describe-mode)
