;;; Towards an Emacs interface to node inspect
;;;

(require 'websocket)
(require 'realgud)
(require 'json)
(require 'cl)
(eval-when-compile (require 'cl))

(defvar node-inspect-response-ids '()
  "property list node response ids seen far"
  )

(defvar node-inspect-script-ids (make-hash-table :test 'equal)
  "Hash table of all of the node scriptIds seen so far"
  )

(defvar node-inspect-urls (make-hash-table :test 'equal)
  "Hash table of all of the node method urls seen so far"
  )

(defun parse-inspect-responses (responses)
  "Parses a list of JSON strings from response from a node inspect process"
  (assert (listp responses))
  (dolist (response responses)
    (parse-inspect-response response)))

(defun unknown-response(node-inspect-reponse)
  (message "Don't know what to do with %s" node-inspect-response))

(defun parse-inspect-response (node-inspect-response)
  "Parses a single a JSON string from response from a node inspect process"
  (assert (stringp node-inspect-response))
  (let ((inspect-obj (json-read-from-string node-inspect-response))
	(value))
    (print inspect-obj)
    (cond ((assoc 'method inspect-obj)
	   (handle-method inspect-obj))
	  ;; Note error has to come before id
	  ((assoc 'error inspect-obj)
	   (handle-error-response inspect-obj))
	  ((assoc 'id inspect-obj)
	   (handle-response-id inspect-obj))
	  (t unknown-response))))

(defun handle-method (params-obj)
  "Parses a JSON method object"
  (let* ((method-obj (assoc 'method params-obj))
	 (method-name (cdr method-obj))
	 (value))
    (cond ((equal method-name "Debugger.scriptParsed")
	   (setq value (assoc 'params inspect-obj))
	   (handle-method-script-parsed value))
	  ((equal method-name "Runtime.executionContextDestroyed")
	   (print (format "todo: Runtime.executionConctextDestroyed %s"
			  method-name params-obj)))
	  ((equal method-name "Runtime.execeptionThrown")
	   (print (format "todo: Runtime.executionThrown %s"
			  method-name params-obj)))
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
    (setq node-inspect-response-ids
	  (plist-put node-inspect-response-ids id inspect-obj))))

(defun handle-error-response (inspect-obj)
  "Handles a error response"
  (assert (listp inspect-obj))
  (let* ((id (assoc 'id inspect-obj))
	 (err (assoc 'error inspect-obj))
	 (code (assoc 'code err))
	 (msg (assoc 'message err))
	 (data (assoc 'data err)))
    (assert id)
    (message "id: %d, %s, data: %s"
	     (cdr id) (cdr msg) (cdr data))
    (setq node-inspect-response-ids
	  (plist-put node-inspect-response-ids id inspect-obj))))

(defun realgud-inspect-run (name)
  (let* ((node-inspect-name (format "node-inspect-%s" name))
	 (node-inspect-buffer (get-buffer-create (format "*%s*" node-inspect-name))))

    (with-current-buffer node-inspect-buffer
      (erase-buffer)
      (let ((node-inspect-proc
	     (start-process node-inspect-name node-inspect-buffer
			    "node" "--inspect-brk" name))
	    (match-point))
	;; node inspect is running. Find the websocket it is listening on, and connect to that.
	(sleep-for 1)
	(goto-char (point-min))
	(setq match-point (re-search-forward "Debugger listening on \\(ws://.+$\\)" nil t 1))
	(if (not match-point)
	    (error (format "\"node --inspect-brk %s\"failed" name))
	  ;; Found socket in response. Now connect to that.
	  (let* ((ws-url (buffer-substring (match-beginning 1) (match-end 1)))
		 (node-inspect-msgs nil)
		 (node-inspect-errs nil)
		 (node-inspect-msgs nil)
		 (node-inspect-closed nil)
		 (id 1)
		 (node-inspect-ws
		  (websocket-open
		   ws-url
		   :on-message (lambda (_websocket frame)
				 (push (websocket-frame-text frame) node-inspect-msgs)
				 (message "ws frame: %S" (websocket-frame-text frame)))
		   :on-close (lambda (_websocket) (setq node-inspect-closed t)))))

	    (defun node-inspect-request(method-params)
	      (let ((prefix (format "{\"id\":%d,\"method\":\"%s\""
				    id (car method-params)))
		    (suffix (if (cdr method-params)
				(format ",\"params\":{%s}}"
					(cadr method-params))
			      "}")))
		(setq id (1+ id))
		(concat prefix suffix)))

	    (sleep-for 0.6)
	    (assert (websocket-openp node-inspect-ws))
	    (sleep-for 0.6)
	    (assert (eq 'open (websocket-ready-state node-inspect-ws)))

	    ;; Should get back
	    ;; {"method": "Runtime.executionContextCreated",
	    ;;  "params": {
	    ;;     "context" {
	    ;;       "id": 1, "origin": "", "name":  "/usr/bin/node[20164]"
	    ;;     } } }
	    ;; ...
	    (dolist
		(cmd (mapcar
		      'node-inspect-request
		      '(("Runtime.enable")
			("Profiler.enable")
			("Profiler.setSamplingInterval"    "\"interval\":100")
			("Debugger.enable")
			("Debugger.setPauseOnExceptions"   "\"state\":\"none\"")
			("Debugger.setAsyncCallStackDepth" "\"maxDepth\":0")
			("Debugger.setBlackboxPatterns"    "\"patterns\":\"[]\"")
			("Debugger.setPauseOnExceptions"   "\"state\":\"none\"")
			("Runtime.runIfWaitingForDebugger"))))
	      (websocket-send-text node-inspect-ws cmd))

	    (sleep-for 1)
	    (message "%s" node-inspect-msgs)

	    ;; Terminate connection
	    (websocket-close node-inspect-ws)

	    ;; Terminate node inspect process
	    (kill-process node-inspect-proc)
	    ))))))

(realgud-inspect-run "example/gcd.js")


(defun realgud-inspect-run (name)
  (let* ((node-inspect-name (format "node-inspect-%s" name))
	 (node-inspect-buffer (get-buffer-create (format "*%s*" node-inspect-name))))

    (with-current-buffer node-inspect-buffer
      (erase-buffer)
      (let ((node-inspect-proc
	     (start-process node-inspect-name node-inspect-buffer
			    "node" "--inspect-brk" name))
	    (match-point))
	;; node inspect is running. Find the websocket it is listening on, and connect to that.
	(sleep-for 1)
	(goto-char (point-min))
	(setq match-point (re-search-forward "Debugger listening on \\(ws://.+$\\)" nil t 1))
	(if (not match-point)
	    (error (format "\"node --inspect-brk %s\"failed" name))
	  ;; Found socket in response. Now connect to that.
	  (let* ((ws-url (buffer-substring (match-beginning 1) (match-end 1)))
		 (node-inspect-msgs nil)
		 (node-inspect-errs nil)
		 (node-inspect-msgs nil)
		 (node-inspect-closed nil)
		 (id 1)
		 (node-inspect-ws
		  (websocket-open
		   ws-url
		   :on-message (lambda (_websocket frame)
				 (push (websocket-frame-text frame) node-inspect-msgs)
				 (message "ws frame: %S" (websocket-frame-text frame)))
		   :on-close (lambda (_websocket) (setq node-inspect-closed t)))))

	    (defun node-inspect-request(method-params)
	      (let ((prefix (format "{\"id\":%d,\"method\":\"%s\""
				    id (car method-params)))
		    (suffix (if (cdr method-params)
				(format ",\"params\":{%s}}"
					(cadr method-params))
			      "}")))
		(setq id (1+ id))
		(concat prefix suffix)))

	    (sleep-for 0.6)
	    (assert (websocket-openp node-inspect-ws))
	    (sleep-for 0.6)
	    (assert (eq 'open (websocket-ready-state node-inspect-ws)))

	    ;; Should get back
	    ;; {"method": "Runtime.executionContextCreated",
	    ;;  "params": {
	    ;;     "context" {
	    ;;       "id": 1, "origin": "", "name":  "/usr/bin/node[20164]"
	    ;;     } } }
	    ;; ...
	    (dolist
		(cmd (mapcar
		      'node-inspect-request
		      '(("Runtime.enable")
			("Profiler.enable")
			("Profiler.setSamplingInterval"    "\"interval\":100")
			("Debugger.enable")
			("Debugger.setPauseOnExceptions"   "\"state\":\"none\"")
			("Debugger.setAsyncCallStackDepth" "\"maxDepth\":0")
			("Debugger.setBlackboxPatterns"    "\"patterns\":\"[]\"")
			("Debugger.setPauseOnExceptions"   "\"state\":\"none\"")
			("Runtime.runIfWaitingForDebugger"))))
	      (websocket-send-text node-inspect-ws cmd))

	    (sleep-for 1)
	    (message "%s" node-inspect-msgs)

	    ;; Terminate connection
	    (websocket-close node-inspect-ws)

	    ;; Terminate node inspect process
	    (kill-process node-inspect-proc)
	    ))))))

(realgud-inspect-run "example/gcd.js")
