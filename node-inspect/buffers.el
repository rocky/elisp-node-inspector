;;; The various buffers we keep around for node --inspect info

(defconst node-inspect-buffer-types
  '("requests" "responses" "errors")
  "The kinds of buffer we create for showing node
inspect information")

(defvar node-inspect-name nil
  "base name of project/main js that we are using"
  )

(defconst node-inspect-buffer-types
  '("requests" "responses" "errors")
  "The kinds of buffer we create for showing node
inspect information")

(defun node-inspect-buffer-name (buffer-type &optional opt-name)
  (let ((name (or opt-name node-inspect-name)))
    (format "*Node Inspect %s %s*" buffer-type name)))


(defun node-inspect-buffer-append(buffer-type item)
  "Append ITEM to BUFFER"
  (let ((buffer-name (node-inspect-buffer-name buffer-type)))
    (with-current-buffer buffer-name
      (goto-char (point-max))
      (insert (format "%s\n" item))
      )))

(provide-me "node-inspect-")
