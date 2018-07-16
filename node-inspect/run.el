;;; Towards an Emacs interface to node --inspect
;;; See https://chromedevtools.github.io/devtools-protocol/tot/Debugger
;;; for how to parse the JSON structures

;; Package-Requires: ((load-relative "1.2") (cl-lib "0.5") (emacs "25"))

;; Press C-x C-e at the end of the next line configure the program in
;; for building via "make" to get set up.
;; (compile (format "EMACSLOADPATH=:%s:%s:%s ./autogen.sh" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "loc-changes.elc"))))

(require 'load-relative)
(require-relative-list '("buffers" "cmds" "handlers") "node-inspect-")

(require 'websocket)
(require 'realgud)
(require 'json)
(require 'cl)
(eval-when-compile (require 'cl))

;;; Note these variables should all be buffer local to some
;;; as-yet-to-be determined base.
(defvar node-inspect-responses '()
  "list of responses received"
  )

(defvar node-inspect-process nil
  "When non-nil a process buffer for a 'node --inspect' invocation"
  )

(defvar node-inspect-script-ids (make-hash-table :test 'equal)
  "Hash table of all of the node scriptIds seen"
  )

(defvar node-inspect-urls (make-hash-table :test 'equal)
  "Hash table of all of the node method urls seen"
  )

(defun node-inspect-initialize (name)
  "Initialize node-inspect variables and buffers"
  (setq node-inspect-name name)
  (let ((buffer))
    (dolist (buffer-type node-inspect-buffer-types)
    (setq buffer
	  (get-buffer-create (node-inspect-buffer-name buffer-type name)))
    (with-current-buffer buffer (erase-buffer)))))

(defun node-inspect-reset-vars()
  "Reset all variables saved"
  (setq node-inspect-last-id 0)
  (setq node-inspect-requests '())
  (setq node-inspect-responses '())
  (setq node-inspect-ids '())
  (setq node-inspect-script-ids (make-hash-table :test 'equal))
  (setq node-inspect-urls (make-hash-table :test 'equal))
)

(defun unknown-response(node-inspect-reponse)
  (print (format "Don't know what to do with %s" node-inspect-response)))

(defun parse-inspect-response (node-inspect-response)
  "Parses a single a JSON string from response from a node inspect process"
  (assert (stringp node-inspect-response))
  (node-inspect-buffer-append "responses" node-inspect-response)
  (let ((inspect-obj (json-read-from-string node-inspect-response))
	(value))
    ;; (node-inspect-buffer-append "responses" inspect-obj)
    (print inspect-obj)
    (cond ((assoc 'method inspect-obj)
	   (handle-method inspect-obj))
	  ;; Note error has to come before id
	  ((assoc 'error inspect-obj)
	   (handle-error-response inspect-obj))
	  ((assoc 'id inspect-obj)
	   (handle-response-id inspect-obj))
	  (t unknown-response))))

(defun parse-inspect-response-from-region (from to)
  (interactive "r")
  (let ((text (buffer-substring-no-properties from to)))
    (parse-inspect-response text)))

(defun node-inspect-initialize-cmds()
  (dolist
      (cmd (mapcar
	    'node-inspect-request
	    '(("Runtime.enable")
	      ("Profiler.enable")
	      ("Profiler.setSamplingInterval"    "\"interval\":100")
	      ("Debugger.enable")
	      ("Debugger.setPauseOnExceptions"   "\"state\":\"none\"")
	      ("Debugger.setAsyncCallStackDepth" "\"maxDepth\":0")
	      ;;("Debugger.setBlackboxPatterns"    "\"patterns\":\"[]\"")
	      ("Debugger.setPauseOnExceptions"   "\"state\":\"none\"")
	      ("Runtime.runIfWaitingForDebugger"))))
    (node-inspect-send-cmd node-inspect-ws cmd))
  )

;; See https://nodejs.org/en/docs/guides/debugging-getting-started/
;; for information on options.
(defun node-inspect-run (name)
  "Run file NAME under node --inspect and track interaction via a websocket"
  (let* ((node-inspect-name (node-inspect-buffer-name "process" name))
	 (node-inspect-buffer (get-buffer-create node-inspect-name)))

    (with-current-buffer node-inspect-buffer
      (erase-buffer)
      (node-inspect-initialize name)
      (let ((match-point))
	(setq node-inspect-process
	      (start-process node-inspect-name node-inspect-buffer
			     "node" "--inspect-brk" name))
	;; node inspect is running. Find the websocket it is listening on, and connect to that.
	(sleep-for 1)
	(goto-char (point-min))
	(setq match-point (re-search-forward "Debugger listening on \\(ws://.+$\\)" nil t 1))
	(if (not match-point)
	    (error (format "\"node --inspect-brk %s\" failed" name))
	  ;; Found socket in response. Now connect to that.
	  (let* ((ws-url (buffer-substring (match-beginning 1) (match-end 1)))
		 (node-inspect-msgs nil)
		 (node-inspect-errs nil)
		 (node-inspect-msgs nil)
		 (node-inspect-closed nil)
		 (response nil))
	    (setq node-inspect-ws
		  (websocket-open
		   ws-url
		   :on-message (lambda (_websocket frame)
				 (setq response (websocket-frame-text frame))
				 (print (format "ws frame: %S\n" response))
				 (parse-inspect-response response))
		   :on-close (lambda (_websocket) (setq node-inspect-closed t))))

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

	    (node-inspect-initialize-cmds)
	    (sleep-for 1)
	    ))))))

;;; FIXME remove global variables
(defun node-inspect-terminate()
  (interactive "")
  ;; Terminate connection
  (websocket-close node-inspect-ws)

  ;; Terminate node inspect process
  (kill-process node-inspect-process))


;; (node-inspect-run "../example/gcd.js")

(provide-me "node-inspect-")
