;;; Node inspect response handler
;;; See https://chromedevtools.github.io/devtools-protocol/tot/Debugger
;;; for how to parse the JSON structures

(require 'load-relative)

(defun handle-method (params-obj)
  "Parses a JSON method object"
  (let* ((method-obj (assoc 'method params-obj))
	 (method-name (cdr method-obj))
	 (value))
    (cond ((equal method-name "Debugger.scriptParsed")
	   (setq value (assoc 'params inspect-obj))
	   (handle-method-script-parsed value))
	  ((equal method-name "Runtime.executionContextCreated")
	   (print (format "todo: Runtime.executionContextCreated %s"
			  method-name params-obj)))
	  ((equal method-name "Runtime.executionContextDestroyed")
	   (print (format "todo: Runtime.executionContextDestroyed %s"
			  params-obj)))
	  ((equal method-name "Runtime.execeptionThrown")
	   (print (format "todo: Runtime.executionThrown %s"
			  params-obj)))
	  (t (print (format "Don't know how to handle %s in %s"
			    method-name params-obj))))))

(defun handle-method-script-parsed (params-obj)
  "Parses a JSON method object"
  (let ((script-id (assoc 'scriptId params-obj))
	(url (assoc 'url params-obj)))
    (if script-id
	(setf (gethash (cdr script-id) node-inspect-script-ids) params-obj)
      (message "Null script id in %s" params-obj))
    (if url
	(setf (gethash (cdr url) node-inspect-urls) params-obj)
      (message "Null url in %s" params-obj))))

(defun handle-response-id (inspect-obj)
  "Handles a response id"
  (assert (listp inspect-obj))
  (let ((id (assoc 'result inspect-obj)))
    (assert id)
    (setq node-inspect-responses
	  (plist-put node-inspect-responses id inspect-obj))))

(defun handle-error-response (inspect-obj)
  "Handles an error response"
  (assert (listp inspect-obj))
  (let* ((id (assoc 'id inspect-obj))
	 (err (assoc 'error inspect-obj))
	 (code (assoc 'code err))
	 (msg (assoc 'message err))
	 (data (assoc 'data err)))
    (assert id)
    (node-inspect-buffer-append "errors" inspect-obj)
    (print (format "id: %d, %s, data: %s"
		   (cdr id) (cdr msg) (cdr data)))
    (setq node-inspect-responses
	  (plist-put node-inspect-responses id inspect-obj))))

(provide-me "node-inspect-")
