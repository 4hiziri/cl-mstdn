(in-package :cl-user)
(defpackage cl-mstdn
  (:use :cl))
(in-package :cl-mstdn)

(ql:quickload 'dexador)
(ql:quickload 'json)
(ql:quickload 'cl-annot)

;;; private

;; :TODO test
(defun strings (&rest strs)
  "concatenate 'string"
  (apply #'concatenate (cons 'string strs)))

(defun instance-url (instance &rest paths)
  (strings "https://" instance (apply #'strings paths)))

(defun get-query (alist)
  "((a . b) (c . d)) => ?a=b&c=d"
  (if alist
      (strings "?"
	       (reduce (lambda (x y) (strings x "&" y))
		       (mapcar (lambda (pair) (strings (car pair) "=" (cdr pair)))
			       alist)))
      ""))

(defmacro push-pair (name val exists-p place)
  `(if ,exists-p (push (cons ,name ,val) ,place)))

;; :TODO stream api implement, write description

@export
(defun request-client-token (instance &optional (scopes "read write follow"))
  (json:decode-json-from-string
   (dex:post (instance-url instance "/api/v1/apps")
	     :content `(("client_name" . ,*me*)
			("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
			("scopes" . ,scopes))))) ;; add website

;; :TODO change grant_type
@export
(defun register-client (instance token username password &optional (scope "read write follow"))
  (json:decode-json-from-string
   (dex:post (instance-url instance "/oauth/token")
	     :content `(("client_id" . ,(cdr (assoc :client--id token)))
			("client_secret" . ,(cdr (assoc :client--secret token)))
			("grant_type" . "password")
			("username" . ,username)
			("password" . ,password)
			("scope" . ,scope)))))

@export
(defun auth-header (token)
  (list (cons "Authorization"
	      (format nil "Bearer ~A" (cdr (assoc :access--token token))))))

;;; accounts
@export
(defun fetch-account (instance token usr-id)
  (json:decode-json-from-string
   (dex:get (instance-url instance "/api/v1/accounts/" (princ-to-string usr-id))
	    :headers (auth-header token))))

@export
(defun get-current-user (instance token)
  (json:decode-json-from-string
   (dex:get (instance-url instance "/api/v1/accounts/verify_credentials")
		:headers (auth-header token))))

;; :TODO unifying by it's item?
@export
(defun update-current-user (instance token content)
  "content:
display_name = The name to display in the user's profile
note = A new biography for the user
avatar = A base64 encoded image to display as the user's avatar 
(e.g. data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAUoAAADrCAYAAAA...)
header = A base64 encoded image to display as the user's header image 
(e.g. data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAUoAAADrCAYAAAA...)
"
  (dex:request (instance-url instance "/api/v1/accounts/update_credentials")
	       :method :patch
	       :headers (auth-header token)
	       :content content))

;; :WARN max_id and since_id do wired behaviour
@export
(defun get-followers (instance token user-id &key
					       (max-id nil max-id-p)
					       (since-id nil since-id-p)
					       (limit 0 limit-p))
  (let ((querys nil)
	(uid (princ-to-string user-id)))
    (push-pair "max_id" (princ-to-string max-id) max-id-p querys)
    (push-pair "since_id" (princ-to-string since-id) since-id-p querys)
    (push-pair "limit" (princ-to-string limit) limit-p querys)    
    (json:decode-json-from-string
     (dex:get (instance-url instance "/api/v1/accounts/" uid "/following" (get-query querys)) ;; rev?
	      :headers (auth-header token)))))

;; TODO query
;; TODO another user
;; return statuses
@export
(defun get-account-status (instance token user-id &key
						    (only-media nil media-p)
						    (exclude-rep nil rep-p)
						    (max-id nil max-p)
						    (since-id nil since-p)
						    (limit nil limit-p))
  (let ((querys nil)
	(uid (princ-to-string user-id)))
    (push-pair "only_media" only-media media-p querys) ;; need test. It will take t or nil
    (push-pair "exclude_replies" exclude-rep rep-p querys) ;; need test. It will take t or nil
    (push-pair "max_id" (princ-to-string max-id) max-p querys)    
    (push-pair "since_id" (princ-to-string since-id) since-p querys)
    (push-pair "limit" (princ-to-string limit) limit-p querys)    
    (json:decode-json-from-string
     (dex:get (strings "https://" instance "/api/v1/accounts/" uid "/statuses")
	      :headers (auth-header token)))))

;; return relationships
@export
(defun account-method-account (instance token account method)
  (let ((user-id (princ-to-string (cdr (assoc :id account)))))
    (json:decode-json-from-string
     (dex:post (instance-url instance "/api/v1/accounts/" (format nil "~A/~A" user-id method))
	       :headers (auth-header token)))))

;; :TODO research HOW TO Follow
@export
(defun follow-account (instance token account)
  (account-method-account instance token account "follow"))

@export
(defun unfollow-account (instance token account)
  (account-method-account instance token account "unfollow"))

@export
(defun block-account (instance token account)
  (account-method-account instance token account "block"))

@export
(defun unblock-account (instance token account)
  (account-method-account instance token account "unblock"))

@export
(defun mute-account (instance token account)
  (account-method-account instance token account "mute"))

@export
(defun unmute-account (instance token account)
  (account-method-account instance token account "unmute"))

;; TODO array after implement accounts struct
@export
(defun account-relations (instance token &optional (account nil account-p))
  (let ((user-id (if account-p ;; :TODO duplicated
		     (princ-to-string (cdr (assoc :id account)))
		     "")))
    (json:decode-json-from-string
     (dex:get (instance-url instance "/api/v1/accounts/relationships" (if account-p
									  (strings "?id=" user-id)
									  ""))
	      :headers (auth-header token)))))

;; :TODO limit branch
;; return accounts
@export
(defun search-accounts (instance token query &optional (limit nil limit-p))
  (let ((querys nil))
    (push-pair "q" query t querys)
    (push-pair "limit" limit limit-p querys)
    (json:decode-json-from-string
     (dex:get (instance-url instance "/api/v1/accounts/search" (get-query (reverse querys)))
	      :headers (auth-header token)))))

;;; apps
;; :TODO implement query
@export
(defun fetch-method (instance token method querys)
  (json:decode-json-from-string
   (dex:get (instance-url instance "/api/v1/" method querys)
	    :headers (auth-header token))))

@export
(defun fetch-user-blocks (instance token &optional
					   (max-id nil max-p)
					   (since-id nil since-p)
					   (limit nil limit-p))
  (let ((querys nil))
    (push-pair "max_id" (princ-to-string max-id) max-p querys)    
    (push-pair "since_id" (princ-to-string since-id) since-p querys)
    (push-pair "limit" (princ-to-string limit) limit-p querys)
    (fetch-method instance token "blocks" querys)))

@export
(defun fetch-user-favo (instance token &optional
					 (max-id nil max-p)
					 (since-id nil since-p)
					 (limit nil limit-p))
  (let ((querys nil))    
    (push-pair "max_id" (princ-to-string max-id) max-p querys)    
    (push-pair "since_id" (princ-to-string since-id) since-p querys)
    (push-pair "limit" (princ-to-string limit) limit-p querys)    
    (fetch-method instance token "favourites" querys)))

@export
(defun fetch-user-follow-req
    (instance token &optional
					 (max-id nil max-p)
					 (since-id nil since-p)
					 (limit nil limit-p))
  (let ((querys nil))    
    (push-pair "max_id" (princ-to-string max-id) max-p querys)    
    (push-pair "since_id" (princ-to-string since-id) since-p querys)
    (push-pair "limit" (princ-to-string limit) limit-p querys)    
    (fetch-method instance token "follow_requests" querys)))

@export
(defun auth-follow-req (instance token permit-method id)
  (dex:post (instance-url instance
			  (format nil "/api/v1/follow_requests/~A/~A" id permit-method))
	    :headers (auth-header token)
	    :content `(("id" . ,id))))

@export
(defun authorize-follow-req (instance token id)
  (auth-follow-req instance token "authorize" id))

@export
(defun reject-follow-req (instance token id)
  (auth-follow-req instance token "reject" id))

;; return accounts
@export
(defun follow (instance token user-uri)
  (json:decode-json-from-string
   (dex:post (instance-url instance "/api/v1/follows")
	     :headers (auth-header token)
	     :content `(("uri" . ,user-uri)))))

@export
(defun instances-info (instance)
  (json:decode-json-from-string
   (dex:get (instance-url instance "/api/v1/instance"))))

;; :TODO researh how to use
@export
(defun upload-media (instance token file)
  (dex:post (instance-url instance "/api/v1/media")
	    :headers (auth-header token)
	    :content `(("file" . ,file))))

;; :TODO parameter
@export
(defun fetch-user-mutes (instance token &optional max-id since-id limit)
  (json:decode-json-from-string
   (dex:get (instance-url instance "/api/v1/mutes")
	    :headers (auth-header token))))

@export
(defun fetch-user-notifications (instance token &optional max-id since-id limit)
  (json:decode-json-from-string
   (dex:get (instance-url instance "/api/v1/notifications")
	    :headers (auth-header token))))

@export
(defun fetch-user-notification (instance token id)
  (json:decode-json-from-string
   (dex:get (strings
			 "https://"
			 instance
			 (format nil "/api/v1/notifications/~A" id))
	    :headers (auth-header token))))

@export
(defun clear-user-notifications (instance token)
  (json:decode-json-from-string
   (dex:get (instance-url instance "/api/v1/notifications/clear")
	    :headers (auth-header token))))

@export
(defun fetch-user-reports (instance token)
  (json:decode-json-from-string
   (dex:get (instance-url instance "/api/v1/reports")
	    :headers (auth-header token))))

@export
(defun report-user (instance token account-id status-ids comment)
  (json:decode-json-from-string
   (dex:post (instance-url instance "/api/v1/reports")
	     :headers (auth-header token)
	     :content `(("account_id" . ,account-id)
			("status_ids" . ,status-ids)
			("comment" . ,comment)))))

;;; search
;; :TODO resolve check
@export
(defun search-contents (instance query resolve)
  (json:decode-json-from-string
   (dex:get (strings
			 "https://"
			 instance
			 "/api/v1/search"
			 (format nil "?q=~A&resolve=~A" query resolve)))))

;;; statuses
@export
(defun fetch-status-method (instance account-id method)
  (json:decode-json-from-string
   (dex:get (strings
			 "https://"
			 instance
			 "/api/v1/statuses/"
			 (format nil "~A" account-id)
			 method))))

@export
(defun fetch-status (instance account-id)
  (fetch-status-method instance account-id ""))

@export
(defun fetch-status-context (instance account-id)
  (fetch-status-method instance account-id "/context"))

@export
(defun fetch-status-card (instance account-id)
  (fetch-status-method instance account-id "/card"))

@export
(defun fetch-status-method-with-param (instance account-id method &optional max-id since-id limit)
  (json:decode-json-from-string
   (dex:get (strings
			 "https://"
			 instance
			 "/api/v1/statuses/"
			 (format nil "~A" account-id)
			 method))))

@export
(defun get-who-method (instance status-id method &optional max-id since-id limit)
  (json:decode-json-from-string   
   (dex:get (strings
			 "https://"
			 instance
			 "/api/v1/statuses/"
			 (princ-to-string status-id)
			 "/"
			 method))))

@export
(defun get-who-reblog (instance status-id &optional max-id since-id limit)
  (get-who-method instance status-id "reblogged_by"))

@export
(defun get-who-favourited (instance status-id &optional max-id since-id limit)
  (get-who-method instance status-id "favourited_by"))

@export
(defun post-new-status (instance token status-txt)
  "form data:
    status: The text of the status
    in_reply_to_id (optional): local ID of the status you want to reply to
    media_ids (optional): array of media IDs to attach to the status (maximum 4)
    sensitive (optional): set this to mark the media of the status as NSFW
    spoiler_text (optional): text to be shown as a warning before the actual content
    visibility (optional): either \"direct\", \"private\", \"unlisted\" or \"public\"
"
  (dex:post (instance-url instance "/api/v1/statuses")
	    :headers (auth-header token)
	    :content `(("status" . ,status-txt))))

@export
(defun delete-status (instance token status-id)
  (json:decode-json-from-string
   (dex:request (instance-url instance "/api/v1/statuses/" (princ-to-string status-id))
		:method :delete
		:headers (auth-header token))))

@export
(defun status-method (instance token status-id method)
  (json:decode-json-from-string
   (dex:post (strings
			  "https://"
			  instance
			  "/api/v1/statuses/"
			  (princ-to-string status-id)
			  "/"
			  method)
	     :headers (auth-header token))))

;; boost
@export
(defun reblog-status (instance token status-id)
  (status-method instance token status-id "reblog"))

@export
(defun unreblog-status (instance token status-id)
  (status-method instance token status-id "unreblog"))

@export
(defun favourite-status (instance token status-id)
  (status-method instance token status-id "favourite"))

@export
(defun unfavourite-status (instance token status-id)
  (status-method instance token status-id "unfavourite"))


;;; timeline
@export
(defun get-home-timeline (instance token &optional max-id since-id limit)
  (json:decode-json-from-string
   (dex:get (instance-url instance "/api/v1/timelines/home")
	    :headers (auth-header token))))

@export
(defun get-public-timeline (instance &optional local max-id since-id limit)
  (json:decode-json-from-string
   (dex:get (instance-url instance "/api/v1/timelines/public"))))

@export
(defun get-hashtag-timeline (instance hash-tag &optional local max-id since-id limit)
  (json:decode-json-from-string
   (dex:get (instance-url instance "/api/v1/timelines/tag/" hash-tag))))

;;; stream API
;;; https://github.com/tootsuite/documentation/blob/master/Using-the-API/Streaming-API.md

;; :TODO websocket implement
;;; https://gist.github.com/twi-light-sparkle/59302f013de9d7ef5821
