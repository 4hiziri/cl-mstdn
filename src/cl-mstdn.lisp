(in-package :cl-user)
(defpackage cl-mstdn
  (:use :cl))
(in-package :cl-mstdn)
;; :TODO stream api implement, write description
;; :TODO handling record not found error
;; :TODO rename function

(ql:quickload 'dexador)
(ql:quickload 'cl-json)
(ql:quickload 'cl-annot)
(ql:quickload 'split-sequence)
(cl-annot:enable-annot-syntax)

;;; private
(defparameter *me* "test-framework")

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


(defun auth-header (token)
  (list (cons "Authorization"
	      (format nil "Bearer ~A" (access-token-access-token token)))))

(defun getalist (key alist)
  (cdr (assoc key alist)))

(defun list-mstdn-array (array-name list)
  (reduce (lambda (x y) (strings x "&" y))
	    (mapcar (lambda (x) (strings array-name "[]=" (princ-to-string x)))
		    list)))

(defun password-256-key (password)
  (let ((digester (ironclad:make-digest :sha256)))
    (ironclad:update-digest digester (ironclad:ascii-string-to-byte-array password))
    (ironclad:produce-digest digester)))

(defun encrypt-string (str password)
  (let* ((key (password-256-key password))
	 (cipher (ironclad:make-cipher :aes
				       :key key
				       :mode :ctr
				       :initialization-vector
				       (subseq (password-256-key
						(ironclad:byte-array-to-hex-string key))
					       0 16)))
	 (plain-text (ironclad:ascii-string-to-byte-array str))
	 (encrypted (make-array (length plain-text) :element-type '(unsigned-byte 8))))
    (ironclad:encrypt cipher plain-text encrypted)
    (ironclad:byte-array-to-hex-string encrypted)))

(defun client-token-str (token)
  (strings ":id=" (princ-to-string (client-token-id token)) "&"
	   ":redirect-uri=" (client-token-redirect-uri token) "&"
	   ":client-id=" (client-token-client-id token) "&"
	   ":secret-key=" (client-token-secret-key token)))

(defun str-client-token (str)
  (let ((list (mapcar (lambda (x) (cons (read-from-string (car x)) (cadr x)))
		      (mapcar (lambda (x) (split-sequence:split-sequence #\= x))
			      (split-sequence:split-sequence #\& str)))))
    (make-client-token :id (parse-integer (getalist :id list))
		       :redirect-uri (getalist :redirect-uri list)
		       :client-id (getalist :client-id list)
		       :secret-key (getalist :secret-key list))))

(defun access-token-str (token)
  (strings ":access-token=" (access-token-access-token token) "&"
	   ":token-type=" (access-token-token-type token) "&"
	   ":scope=" (prin1-to-string (access-token-scope token)) "&"
	   ":created-time=" (princ-to-string (access-token-created-time token))))

(defun str-access-token (str)
  (let ((list (mapcar (lambda (x) (cons (read-from-string (car x)) (cadr x)))
		      (mapcar (lambda (x) (split-sequence:split-sequence #\= x))
			      (split-sequence:split-sequence #\& str)))))
    (make-access-token :access-token (getalist :access-token list)
		       :token-type (getalist :token-type list)
		       :scope (read-from-string (getalist :scope list))
		       :created-time (read-from-string (getalist :created-time list)))))

;;;; struct
;; Should I use Local-time?
(defstruct Client-token
  id
  redirect-uri
  client-id
  secret-key)

(defstruct Access-token
  access-token
  token-type
  scope
  created-time)

(defstruct Account
  id
  user-name
  acct
  display-name
  locked-p
  created-time
  followers-count
  following-count
  note
  url
  avatar
  avatar-static ;; If user uses gif file as avatar pic, static image's url is returned
  header
  header-static)


(defstruct App
  name
  (website ""))

(defstruct Attachment
  id
  type
  url
  remote-url
  preview-url
  text-url)

(defstruct Card
  url
  title
  description
  image)

(defstruct Context
  ancestors
  descendants)

(defstruct Mstdn-error
  mstdn-error)

(defstruct Mstdn-instance
  uri
  title
  description
  email)

(defstruct Mention
  url
  user-name
  acc
  account-id)

(defstruct Notification
  id
  type
  created-time
  account
  status)

(defstruct Relationship
  id
  following-p
  followed-by-p
  block-p
  mute-p
  requested-p)

(defstruct Report
  id
  action-taken)

(defstruct Result
  accounts
  statuses
  hashtags)

(defstruct Status
  id
  uri
  url
  account
  reply-to-status
  reply-to-account
  reblog-status
  content
  created-time
  reblogs-count
  favourites-count
  reblogged
  favourited
  sensitive
  spoiler-text
  visibility
  media-attach
  mentions
  tags
  app)

(defstruct Tag
  name
  url)

;;; json-struct
(defun json-client-token (json-alist)
  (make-client-token :id (getalist :id json-alist)
		     :redirect-uri (getalist :redirect--uri json-alist)
		     :client-id (getalist :client--id json-alist)
		     :secret-key (getalist :client--secret json-alist)))

(defun json-access-token (json-alist)  
  (make-access-token :access-token (getalist :access--token json-alist)
		     :token-type (getalist :token--type json-alist)
		     :scope (mapcar (lambda (x) (read-from-string (strings ":" x)))
				    (split-sequence:split-sequence #\space
								   (getalist :scope json-alist)))
		     :created-time (getalist :created--at json-alist)))

(defun json-account (json-alist)
  (if json-alist
      (make-account :id (getalist :id json-alist)
		    :user-name (getalist :username json-alist)
		    :acct (getalist :acct json-alist)
		    :display-name (getalist :display--name json-alist)
		    :locked-p (getalist :locked json-alist) ;; :TODO test locked state
		    :created-time (getalist :created--at json-alist)
		    :followers-count (getalist :followers--count json-alist)
		    :following-count (getalist :following--count json-alist)
		    :note (getalist :note json-alist)
		    :url (getalist :url json-alist)
		    :avatar (getalist :avatar json-alist)
		    :avatar-static (getalist :avatar--static json-alist)
		    :header (getalist :header json-alist)
		    :header-static (getalist :header--static json-alist))
      nil))

(defun json-app (json-alist)
  (if json-alist
      (make-app :name (getalist :name json-alist)
		:website (getalist :website json-alist))
      nil))

(defun json-attachment (json-alist)
  (if json-alist      
      (make-attachment :id (getalist :id json-alist)
		       :type (getalist :type json-alist)
		       :url (getalist :url json-alist)
		       :remote-url (getalist :remote--url json-alist)
		       :preview-url (getalist :preview--url json-alist)
		       :text-url (getalist :text--url json-alist))
      nil))

(defun json-card (json-alist)
  (make-card :url (getalist :url json-alist)
	     :title (getalist :title json-alist)
	     :description (getalist :description json-alist)
	     :image (getalist :image json-alist)))

(defun json-context (json-alist)
  (make-context :ancestors (mapcar (lambda (x) (json-status x))
				   (getalist :ancestors json-alist))
		:descendants (mapcar (lambda (x) (json-status x))
				     (getalist :descendants json-alist))))

(defun json-error (json-alist)
  (make-mstdn-error :mstdn-error (getalist :error json-alist)))

(defun json-instance (json-alist)
  (make-mstdn-instance :uri (getalist :uri json-alist)
		       :title (getalist :title json-alist)
		       :description (getalist :description json-alist)
		       :email (getalist :email json-alist)))

(defun json-mention (json-alist)
  (make-mention :url (getalist :url json-alist)
		:user-name (getalist :username json-alist)
		:acc (getalist :acct json-alist)
		:account-id (getalist :id json-alist)))

(defun json-notification (json-alist)
  (make-notification :id (getalist :id json-alist)
		     :type (getalist :type json-alist)
		     :created-time (getalist :created--at json-alist)
		     :account (json-account (getalist :account json-alist))
		     :status (json-status (getalist :status json-alist))))

(defun json-relation (json-alist)
  (make-relationship :id (getalist :id json-alist)
		     :following-p (getalist :following json-alist)
		     :followed-by-p (getalist :followed--by json-alist)
		     :block-p (getalist :blocking json-alist)
		     :mute-p (getalist :muting json-alist)
		     :requested-p (getalist :requested json-alist)))

(defun json-report (json-alist)
  (make-report :id (getalist :id json-alist)
	       :action-taken (getalist :action--taken json-alist)))

(defun json-result (json-alist)
  (make-result :accounts (mapcar (lambda (x) (json-account x))
				 (getalist :accounts json-alist))
	       :statuses (mapcar (lambda (x) (json-account x))
				 (getalist :statuses json-alist))
	       :hashtags (getalist :hashtags json-alist))) ;; :TODO extract struct?

(defun json-tag (json-alist)
  (if json-alist
      (make-tag :name (getalist :name json-alist)
		:url (getalist :url json-alist))
      nil))



(defun json-status (json-alist)
  (if json-alist
      (make-status :id (getalist :id json-alist)
		   :uri (getalist :uri json-alist)
		   :url (getalist :url json-alist)
		   :account (json-account (getalist :account json-alist))
		   :reply-to-status (getalist :in--reply--to--account-id json-alist)
		   :reply-to-account (getalist :in--reply--to--id json-alist)
		   :reblog-status (json-status (getalist :reblog json-alist))
		   :content (getalist :content json-alist)
		   :created-time (getalist :created--at json-alist)
		   :reblogs-count (getalist :reblogs--count json-alist)
		   :favourites-count (getalist :favourites--count json-alist)
		   :reblogged (getalist :reblogged json-alist)
		   :favourited (getalist :favourited json-alist)
		   :sensitive (getalist :sensitive json-alist) ;; TODO what is returned
		   :spoiler-text (getalist :spoiler--text json-alist)
		   :visibility (getalist :visibility json-alist)
		   :media-attach (mapcar #'json-attachment (getalist :media--attachments json-alist))
		   :mentions (mapcar #'json-mention (getalist :mentions json-alist))
		   :tags (mapcar #'json-tag (getalist :tags json-alist))
		   :app (json-app (getalist :application json-alist)))
      nil))
;; TODO make functions return these struct

;;; public
@export
(defun request-client-token (instance &key (scopes "read write follow") (website "https://github.com/4hiziri/cl-mstdn.git"))
  (json-client-token
   (json:decode-json-from-string
    (dex:post (instance-url instance "/api/v1/apps")
	      :content `(("client_name" . ,*me*)
			 ("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
			 ("scopes" . ,scopes)
			 ("website" . ,website))))))

;; :TODO not supported?
(defun request-authorization-url (instance client-token)
  (let ((querys nil))
    (push (cons "client_id" (princ-to-string (client-token-id client-token))) querys)
    (push (cons "responce_type" "token") querys)
    (push (cons "redirect_uris" "urn:ietf:wg:oauth:2.0:oob") querys)
    (push (cons "scope" "read%20write%20follow") querys) ;; test, imple as arg
    (princ (instance-url instance "/oauth/authorize" (get-query querys)))))

;; :TODO change grant_type
@export
(defun register-client (instance token username pass &key (scope "read write follow"))
  (json-access-token
   (print (json:decode-json-from-string
	   (dex:post (instance-url instance "/oauth/token")
		     :content `(("client_id" . ,(client-token-client-id token))
				("client_secret" . ,(client-token-secret-key token))
				("grant_type" . "password")
				("username" . ,username)
				("password" . ,pass)
				("scope" . ,scope)))))))


;;; accounts
@export
(defun fetch-account (instance token usr-id)
  (json-account
   (json:decode-json-from-string
    (dex:get (instance-url instance "/api/v1/accounts/" (princ-to-string usr-id))
	     :headers (auth-header token)))))

@export
(defun get-current-user (instance token)
  (json-account   
   (json:decode-json-from-string
    (dex:get (instance-url instance "/api/v1/accounts/verify_credentials")
	     :headers (auth-header token)))))

;; :TODO unifying by it's item?
;; :TODO content -> key
@export
(defun update-current-user (instance token content)
  "content is a-list
content:
display_name = The name to display in the user's profile
note = A new biography for the user
avatar = A base64 encoded image to display as the user's avatar 
(e.g. data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAUoAAADrCAYAAAA...)
header = A base64 encoded image to display as the user's header image 
(e.g. data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAUoAAADrCAYAAAA...)
"
(json-account
 (json:decode-json-from-string
  (dex:request (instance-url instance "/api/v1/accounts/update_credentials")
	       :method :patch
	       :headers (auth-header token)
	       :content content))))

;; :WARN max_id and since_id do wired behaviour
(defun get-follow-info (instance token method uid querys)
  (mapcar (lambda (x) (json-account x))
	    (json:decode-json-from-string
	     (dex:get (instance-url instance
				    "/api/v1/accounts/"
				    (princ-to-string uid)
				    "/"
				    method
				    querys) ;; rev?
		      :headers (auth-header token)))))

@export
(defun get-followers (instance token uid &key
					   (max-id nil max-id-p)
					   (since-id nil since-id-p)
					   (limit 0 limit-p))
  (let ((querys nil))
    (push-pair "max_id" (princ-to-string max-id) max-id-p querys)
    (push-pair "since_id" (princ-to-string since-id) since-id-p querys)
    (push-pair "limit" (princ-to-string limit) limit-p querys)    
    (get-follow-info instance token "followers" uid (get-query querys))))

@export
(defun get-followings (instance token uid &key
					    (max-id nil max-id-p)
					    (since-id nil since-id-p)
					    (limit 0 limit-p))
  (let ((querys nil))
    (push-pair "max_id" (princ-to-string max-id) max-id-p querys)
    (push-pair "since_id" (princ-to-string since-id) since-id-p querys)
    (push-pair "limit" (princ-to-string limit) limit-p querys)
    (get-follow-info instance token "following" uid (get-query querys))))

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
    (mapcar (lambda (x) (json-status x))
	    (json:decode-json-from-string
	     (dex:get (strings "https://" instance "/api/v1/accounts/" uid "/statuses")
		      :headers (auth-header token))))))

@export
(defun account-method-account (instance token user-id method)
  (json-relation
   (json:decode-json-from-string
    (dex:post (instance-url instance "/api/v1/accounts/" (format nil "~A/~A" user-id method))
	      :headers (auth-header token)))))

@export
(defun follow-account (instance token user-id)
  (account-method-account instance token user-id "follow"))

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

@export
(defun account-relations (instance token ids)
  (mapcar #'json-relation 
	  (json:decode-json-from-string
	   (dex:get (instance-url instance "/api/v1/accounts/relationships?" (list-mstdn-array "id" ids))
		    :headers (auth-header token)))))

@export
(defun search-accounts (instance token query &optional (limit nil limit-p))
  (let ((querys nil))
    (push-pair "q" query t querys)
    (push-pair "limit" limit limit-p querys)
    (mapcar #'json-account
	    (json:decode-json-from-string
	     (dex:get (instance-url instance "/api/v1/accounts/search" (get-query (reverse querys)))
		      :headers (auth-header token))))))

;;; apps
;; :TODO implement query
(defun fetch-method (instance token method querys)
  (mapcar #'json-account
	  (json:decode-json-from-string
	   (dex:get (instance-url instance "/api/v1/" method querys)
		    :headers (auth-header token)))))

@export
(defun fetch-user-blocks (instance token &key
					   (max-id nil max-p)
					   (since-id nil since-p)
					   (limit nil limit-p))
  (let ((querys nil))
    (push-pair "max_id" (princ-to-string max-id) max-p querys)    
    (push-pair "since_id" (princ-to-string since-id) since-p querys)
    (push-pair "limit" (princ-to-string limit) limit-p querys)
    (fetch-method instance token "blocks" querys)))

@export
(defun fetch-user-favo (instance token &key
					 (max-id nil max-p)
					 (since-id nil since-p)
					 (limit nil limit-p))
  (let ((querys nil))    
    (push-pair "max_id" (princ-to-string max-id) max-p querys)    
    (push-pair "since_id" (princ-to-string since-id) since-p querys)
    (push-pair "limit" (princ-to-string limit) limit-p querys)    
    (fetch-method instance token "favourites" querys)))

@export
(defun fetch-user-follow-req (instance token &key
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
	    :content `(("id" . ,id)))
  nil)

@export
(defun authorize-follow-req (instance token id)
  (auth-follow-req instance token "authorize" id))

@export
(defun reject-follow-req (instance token id)
  (auth-follow-req instance token "reject" id))

@export
(defun follow (instance token user-uri)
  (json-account
   (json:decode-json-from-string
    (dex:post (instance-url instance "/api/v1/follows")
	      :headers (auth-header token)
	      :content `(("uri" . ,user-uri))))))

@export
(defun instances-info (instance)
  (json-instance
   (json:decode-json-from-string
    (dex:get (instance-url instance "/api/v1/instance")))))

;; :TODO researh how to use
@export
(defun upload-media (instance token file-data)
  (json-attachment
   (dex:post (instance-url instance "/api/v1/media")
	     :headers (auth-header token)
	     :content `(("file" . ,file-data)))))

@export
(defun fetch-user-mutes (instance token &key
					       (max-id nil max-p)
					       (since-id nil since-p)
					       (limit nil limit-p))
  (let ((querys nil))
    (push-pair "max_id" (princ-to-string max-id) max-p querys)    
    (push-pair "since_id" (princ-to-string since-id) since-p querys)
    (push-pair "limit" (princ-to-string limit) limit-p querys)    
    (fetch-method instance token "mutes" querys)))

@export
(defun fetch-user-notifications (instance token &key
						  (max-id nil max-p)
						  (since-id nil since-p)
						  (limit nil limit-p))
  (let ((querys nil))
    (push-pair "max_id" (princ-to-string max-id) max-p querys)    
    (push-pair "since_id" (princ-to-string since-id) since-p querys)
    (push-pair "limit" (princ-to-string limit) limit-p querys)    
    (fetch-method instance token "notifications" querys)))

@export
(defun fetch-user-notification (instance token id)
  (mapcar #'json-notification
	  (json:decode-json-from-string
	   (dex:get (instance-url instance (format nil "/api/v1/notifications/~A" id))
		    :headers (auth-header token)))))

@export
(defun clear-user-notifications (instance token)
  (dex:get (instance-url instance "/api/v1/notifications/clear")
	   :headers (auth-header token))
  nil)

@export
(defun fetch-user-reports (instance token)
  (mapcar #'json-report
	  (json:decode-json-from-string
	   (dex:get (instance-url instance "/api/v1/reports")
		    :headers (auth-header token)))))

;; :TODO test
@export
(defun report-user (instance token account-id status-ids comment)
  (json-report
   (json:decode-json-from-string
    (dex:post (instance-url instance "/api/v1/reports")
	      :headers (auth-header token)
	      :content `(("account_id" . ,account-id)
			 ("status_ids" . ,(list-mstdn-array "status_ids" status-ids))
			 ("comment" . ,comment))))))

;;; search
;; TODO resolve check
@export
(defun search-contents (instance query resolve)
  (let ((querys nil))
    (push-pair "q" query t querys)
    (push-pair "resolve" resolve t querys)
    (json-result
     (json:decode-json-from-string
      (dex:get (instance-url instance "/api/v1/search" (get-query querys)))))))

;;; statuses
(defun fetch-status-method (instance status-id method)
  (json:decode-json-from-string
   (dex:get (instance-url instance "/api/v1/statuses/" (princ-to-string status-id) method))))

@export
(defun fetch-status (instance status-id)
  (json-status (fetch-status-method instance status-id "")))

@export
(defun fetch-status-context (instance status-id)  
  (json-context (fetch-status-method instance status-id "/context")))

@export
(defun fetch-status-card (instance status-id)
  (json-card (fetch-status-method instance status-id "/card")))

;; :TODO rename
(defun get-who-method (instance status-id method querys)
  (mapcar #'json-account
	  (json:decode-json-from-string
	   (dex:get (instance-url instance
				  "/api/v1/statuses/"
				  (princ-to-string status-id)
				  "/"
				  method
				  querys)))))

@export
(defun get-who-reblog (instance status-id &key (max-id nil max-p)
					    (since-id nil since-p)
					    (limit nil limit-p))
  (let ((querys nil))    
    (push-pair "max_id" (princ-to-string max-id) max-p querys)
    (push-pair "since_id" (princ-to-string since-id) since-p querys)    
    (push-pair "limit" (princ-to-string limit) limit-p querys)
    (get-who-method instance status-id "reblogged_by" querys)))

@export
(defun get-who-favourite (instance status-id &key (max-id nil max-p)
					    (since-id nil since-p)
					    (limit nil limit-p))
  (let ((querys nil))    
    (push-pair "max_id" (princ-to-string max-id) max-p querys)
    (push-pair "since_id" (princ-to-string since-id) since-p querys)    
    (push-pair "limit" (princ-to-string limit) limit-p querys)
    (get-who-method instance status-id "favourited_by" querys)))


;;; MEMO val isn't needed
@export
(defun post-new-status (instance token status-txt &key
						    (in-rep-to-id nil rep-p)
						    (media-ids nil media-p)
						    (sensitive nil sensitive-p)
						    (spoiler-text nil spoiler-p)
						    (visibility nil visibility-p))
  "form data:
    status: The text of the status
    in_reply_to_id (optional): local ID of the status you want to reply to
    media_ids (optional): array of media IDs to attach to the status (maximum 4)
    sensitive (optional): set this to mark the media of the status as NSFW
    spoiler_text (optional): text to be shown as a warning before the actual content
    visibility (optional): either \"direct\", \"private\", \"unlisted\" or \"public\"
"
  (let ((param nil))
    (push-pair "status" status-txt t param)
    (push-pair "in_reply_to_id" in-rep-to-id rep-p param)
    (push-pair "media_ids" media-ids media-p param)
    (push-pair "sensitive" sensitive sensitive-p param)
    (push-pair "spoiler_text" spoiler-text spoiler-p param)
    (push-pair "visibility" visibility visibility-p param)
    (json-status
     (json:decode-json-from-string
      (dex:post (instance-url instance "/api/v1/statuses")
		:headers (auth-header token)
		:content param)))))

@export
(defun delete-status (instance token status-id)
  (dex:request (instance-url instance "/api/v1/statuses/" (princ-to-string status-id))
		:method :delete
		:headers (auth-header token))
  nil)

(defun status-method (instance token status-id method)
  (json-status
   (json:decode-json-from-string
    (dex:post (instance-url instance "/api/v1/statuses/" (princ-to-string status-id) "/" method)
	      :headers (auth-header token)))))

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
(defun get-home-timeline (instance token &key (max-id nil max-p)
					   (since-id nil since-p)
					   (limit nil limit-p))
  (let ((querys nil))
    (push-pair "max_id" (princ-to-string max-id) max-p querys)    
    (push-pair "since_id" (princ-to-string since-id) since-p querys)
    (push-pair "limit" (princ-to-string limit) limit-p querys)
    (mapcar #'json-status
	    (json:decode-json-from-string
	     (dex:get (instance-url instance "/api/v1/timelines/home" querys)
		      :headers (auth-header token))))))

@export
(defun get-public-timeline (instance &key (local nil local-p)
				       (max-id nil max-p)
				       (since-id nil since-p)
				       (limit nil limit-p))
  (let ((querys nil))
    (push-pair "local" local local-p querys)
    (push-pair "max_id" (princ-to-string max-id) max-p querys)    
    (push-pair "since_id" (princ-to-string since-id) since-p querys)
    (push-pair "limit" (princ-to-string limit) limit-p querys)
    (mapcar #'json-status
	    (json:decode-json-from-string
	     (dex:get (instance-url instance "/api/v1/timelines/public" (get-query querys)))))))

@export
(defun get-hashtag-timeline (instance hash-tag &key (local nil local-p)
						(max-id nil max-p)
						(since-id nil since-p)
						(limit nil limit-p))
  (let ((querys nil))
    (push-pair "local" local local-p querys)
    (push-pair "max_id" (princ-to-string max-id) max-p querys)    
    (push-pair "since_id" (princ-to-string since-id) since-p querys)
    (push-pair "limit" (princ-to-string limit) limit-p querys)
    (mapcar #'json-status
	    (json:decode-json-from-string
	     (dex:get (instance-url instance "/api/v1/timelines/tag/" hash-tag (get-query querys)))))))

;;; stream API
;;; https://github.com/tootsuite/documentation/blob/master/Using-the-API/Streaming-API.md

;; :TODO websocket implement
;;; https://gist.github.com/twi-light-sparkle/59302f013de9d7ef5821
