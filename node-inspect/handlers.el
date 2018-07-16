;;; Node inspect response handler
;;; See https://chromedevtools.github.io/devtools-protocol/tot/Debugger
;;; for how to parse the JSON structures

(require 'load-relative)

;;; FIXME: buffer localize and possibly put into a structure.
(defvar node-inspect-program-state nil
  "The state the debugged program is in: pause, resumed, ..."
  )

(defun handle-method (params-obj)
  "Parses a JSON method object"
  (let* ((method-obj (assoc 'method params-obj))
	 (method-name (cdr method-obj))
	 (value))
    (cond ((equal method-name "Debugger.scriptParsed")
	   (setq params (assoc 'params inspect-obj))
	   (handle-method-script-parsed params))
	  ((equal method-name "Debugger.paused")
	   (setq params (assoc 'params inspect-obj))
	   (handle-method-Debugger-paused params))
	  ((equal method-name "Debugger.resumed")
	   (handle-method-Debugger-resumed inspect-obj))
	  ((equal method-name "Runtime.consoleAPICalled")
	   (setq params (assoc 'params inspect-obj))
	   (handle-method-Runtime-consoleAPICalled params-obj))
	  ((equal method-name "Runtime.executionContextCreated")
	   (setq params (assoc 'params inspect-obj))
	   (handle-method-Runtime-executionContextCreated params-obj))
	  ((equal method-name "Runtime.executionContextDestroyed")
	   (setq params (assoc 'params inspect-obj))
	   (handle-method-Runtime-executionContextDestroyed params-obj))
	  ((equal method-name "Runtime.exceptionThrown")
	   (setq params (assoc 'params inspect-obj))
	   (print (format "todo: Runtime.exceptionThrown %s"
			  params-obj)))
	  (t (print (format "Don't know how to handle %s in %s"
			    method-name params-obj))))))

;;; https://chromedevtools.github.io/devtools-protocol/tot/Runtime#event-consoleAPICalled
(defun handle-method-Runtime-consoleAPICalled (params-obj)
  "handle Runtime.consoleAPICalled object:
Issued when console API was called."
  (let ((type (assoc 'type params-obj))
	(args (assoc 'args params-obj))
	(executionContextId (assoc 'executionContextId params-obj))
	(timestamp (assoc 'timestamp params-obj))
	(stackTrace (assoc 'stackTrace params-obj))
	(context (assoc 'context params-obj)))
      (print (format "args: %s" args))))

;;; https://chromedevtools.github.io/devtools-protocol/tot/Runtime#event-consoleAPICalled
(defun handle-method-Runtime-executionContextCreated (params-obj)
  "handle Runtime.exectionContextCreated object:
Issued when console API was called."
  (let ((context (assoc 'context params-obj)))
      (print (format "context: %s" context))))

;;; https://chromedevtools.github.io/devtools-protocol/tot/Runtime#event-consoleAPICalled
(defun handle-method-Runtime-executionContextDestroyed (params-obj)
  "handle Runtime.exectionContextDestroyed object:
Issued when console API was called."
  (let ((type (assoc 'type params-obj))
	(args (assoc 'args params-obj))
	(executionContextId (assoc 'executionContextId params-obj))
	(timestamp (assoc 'timestamp params-obj))
	(stackTrace (assoc 'stackTrace params-obj))
	(context (assoc 'context params-obj)))
      (print (format "args: %s" args))))

;;; https://chromedevtools.github.io/devtools-protocol/tot/Debugger#event-paused
(defun handle-method-Debugger-paused (params-obj)
  "handle a Debugger.paused object. Fired when the virtual machine stopped on breakpoint or exception or any other stop criteria.
"
  (let* ((callFrames (assoc 'callFrames params-obj))
	(reason (assoc 'reason params-obj))
	;; optional
	(data (assoc 'data params-obj))
	(hitBreakpoints (assoc 'hitBreakpoints params-obj))
	(asyncStackTrace (assoc 'asyncStackTrace params-obj))
	;; the folowing are optional and experimental
	(asyncStackTraceId (assoc 'asyncStackTraceId params-obj))
	(asyncCallStackTraceId (assoc 'asyncStackTraceId params-obj))
	)
    ;; FIXME: capture callFrames
    (setq node-inspect-program-state 'paused)
    (assert (consp reason))
    (print (format "Debugger paused; reason %s" (cdr reason)))))

;;; https://chromedevtools.github.io/devtools-protocol/tot/Debugger#event-resumed
(defun handle-method-Debugger-resumed (params-obj)
  "handle a Debugger.resume object. Fired when the virtual machine resumed execution.
"
  (setq node-inspect-program-state 'resumed)
  (print "Debugger resumed"))

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
