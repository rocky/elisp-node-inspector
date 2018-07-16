;; v8 debugger protocol commands

;;; Note the emacs command will use the corresponding gdb-counterpart names.
;;; So gdb's "continue" is v8 javscript debuggers "resume".

(require 'websocket)
(require 'load-relative)
(require-relative-list '("buffers") "node-inspect-")

;;; Note these variables should all be buffer local to some
;;; as-yet-to-be determined base.
(defvar node-inspect-requests '()
  "list of requests issued")

(defvar node-inspect-last-id 0
  "Last id used in a node-inspect request" )

(defvar node-inspect-ws nil
  "When non-nil a websocket to a remote v8 debugged program")

(defun node-inspect-request(method-params)
  "Format method-params as json"
  (let ((prefix (format "{\"id\":%d,\"method\":\"%s\""
			node-inspect-last-id (car method-params)))
	(suffix (if (cdr method-params)
		    (format ",\"params\":{%s}}"
			    (cadr method-params))
		  "}")))
    (plist-put node-inspect-requests node-inspect-last-id method-params)
    (node-inspect-buffer-append "requests" (cons node-inspect-last-id method-params))
    (setq node-inspect-last-id (1+ node-inspect-last-id))
    (concat prefix suffix)))

(defun node-inspect-send-cmd(node-websocket cmd)
  (websocket-send-text node-websocket cmd))
(provide-me "node-inspect-")

;; FIXME: assumes global node-inspect-ws
(defun node-inspect-cmd-continue()
  (interactive "")
  (node-inspect-send-cmd node-inspect-ws
			 (node-inspect-request '("Debugger.resume"))))

;; FIXME: assumes global node-inspect-ws
;; https://chromedevtools.github.io/devtools-protocol/tot/Runtime#method-evaluate
(defun node-inspect-cmd-evaluate(eval-string)
  "Evaluates expresion"
  (interactive "s")
  (node-inspect-send-cmd node-inspect-ws
			 (node-inspect-request
			  '("Debugger.resume" (format "\"expression\": \"%s\""
						      eval-string)))))
;; FIXME: assumes global node-inspect-ws
(defun node-inspect-cmd-step()
  (interactive "")
  (node-inspect-send-cmd node-inspect-ws
			 (node-inspect-request
			  '("Debugger.stepInto" "\"breakOnAsyncCall\": \"true\""))))
