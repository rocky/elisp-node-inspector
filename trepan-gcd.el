(require 'websocket)
(require 'realgud)
(eval-when-compile (require 'cl))

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
	(when match-point
	  ;; Found socket in response. Now connect to that.
	  (let* ((ws-url (buffer-substring (match-beginning 1) (match-end 1)))
		 (node-inspect-msgs nil)
		 (node-inspect-errs nil)
		 (node-inspect-msgs nil)
		 (node-inspect-closed nil)
		 (node-inspect-ws
		  (websocket-open
		   ws-url
		   :on-message (lambda (_websocket frame)
				 (push (websocket-frame-text frame) node-inspect-msgs)
				 (message "ws frame: %S" (websocket-frame-text frame)))
		   :on-close (lambda (_websocket) (setq node-inspect-closed t)))))
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
	    (dolist (cmd '("{\"id\":1,\"method\":\"Runtime.enable\"}"
		      "{\"id\":2,\"method\":\"Profiler.enable\"}"
		      "{\"id\":3,\"method\":\"Profiler.setSamplingInterval\",\"params\":{\"interval\":100}}"
		      "{\"id\":4,\"method\":\"Debugger.enable\"}"
                      "{\"id\":5,\"method\":\"Debugger.setPauseOnExceptions\",\"params\":{\"state\":\"none\"}}"
		      "{\"id\":6,\"method\":\"Debugger.setAsyncCallStackDepth\",\"params\":{\"maxDepth\":0}}"
                      "{\"id\":7,\"method\":\"Debugger.setBlackboxPatterns\",\"params\":{\"patterns\":[]}}"
		      "{\"id\":8,\"method\":\"Debugger.setPauseOnExceptions\",\"params\":{\"state\":\"none\"}}"
		      "{\"id\":9,\"method\":\"Runtime.runIfWaitingForDebugger\"}"))
		    (websocket-send-text node-inspect-ws cmd))

	    (sleep-for 1)
	    (message "%s" node-inspect-msgs)

	    ;; Terminate connection
	    (websocket-close node-inspect-ws)

	    ;; Terminate node inspect process
	    (kill-process node-inspect-proc)
	    ))))))

(realgud-inspect-run "gcd.js")
