(use-package f)

(unless (file-exists-p "~/.pim/auth")
  (f-mkdir "~/.pim")
  (f-touch "~/.pim/auth"))

(unless (file-exists-p "~/.pim/host")
  (f-mkdir "~/.pim")
  (f-touch "~/.pim/host"))


(setq pim-host (f-read-text "~/.pim/host" 'utf-8)) ;;  "http://localhost:4321/api/")
(defun pim-load-cookie () (f-read-text "~/.pim/auth" 'utf-8)) ;; how slow is this? Probably negligible. 

(defun pim-login(password)
  (interactive (list (read-from-minibuffer "Password: ")))
  ;; cookie is in header, but maybe an alternate login that returns
  ;; a token in the body is easier.  
  (plz 'post (concat pim-host "login")
       :headers '(("Content-Type" . "application/json"))
       :body (json-encode  `(("password" . ,password)))
       :as #'response
       :then `(lambda (data)
		(progn		  
		  (message "Login successful")
		  (f-write-text (alist-get 'set-cookie (plz-response-headers data)) 'utf-8 "~/.pim/auth")
  )
  )))

(cl-defun pim-api-request (method path &key body then as)
  ;;todo handle auth failures, try to login in and then run the request again. 
  (let ((cookie (pim-load-cookie)))
    (plz method (concat pim-host path)
      :headers `(("Content-Type" . "application/json")
		 ("Cookie" . ,cookie))
      :body (if body (json-encode body) nil)
      :as (or as #'json-read)
      :then then)))

