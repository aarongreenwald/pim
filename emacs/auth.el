(use-package f)

(unless (file-exists-p "~/.pim/auth")
  (f-mkdir "~/.pim")
  (f-touch "~/.pim/auth"))

(unless (file-exists-p "~/.pim/host")
  (f-mkdir "~/.pim")
  (f-touch "~/.pim/host"))

(setq pim-host (f-read-text "~/.pim/host" 'utf-8))
(defun pim-connect-local ()
  "Switch connection to local server. No login is performed, the assumption is that localhost has no auth."
  (interactive)
  (setq pim-host "http://localhost:4321/api/"))

(defun pim-connect ()
  "Connect to production pim - hostname should be in ~/.pim/host, including the '/api/' suffix.
Function also verifies the available cookie is still valid, if not performs a login."
  (interactive)
  (setq pim-host (f-read-text "~/.pim/host" 'utf-8))
    (let ((cookie (pim-load-cookie))) ;;todo if there's no cookie, skip directly to pim-login
      (plz 'get (concat pim-host "login")
	:headers `(("Cookie" . ,cookie))
	:as #'string
	:then `(lambda (is-logged-in)
		 (progn
		   (if (string-equal is-logged-in "true")
		       (message "Connected successfully")
		     (setq password (read-from-minibuffer "Password: "))
		     (pim-login password)))))))


(defun pim-load-cookie () (f-read-text "~/.pim/auth" 'utf-8)) ;; how slow is this? Probably negligible. 

(defun pim-login(password)
;;  (interactive (list (read-from-minibuffer "Password: "))) <-- not to be used directly, use pim-connect instead
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
;;also: capture the new cookie and save it.
  (let ((cookie (pim-load-cookie)))
    (plz method (concat pim-host path)
      :headers `(("Content-Type" . "application/json")
		 ("Cookie" . ,cookie))
      :body (if body (json-encode body) nil)
      :as (or as #'json-read)
      :then then)))

