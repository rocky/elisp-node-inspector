;;; node-inspect.el --- A modular front-end for interacting with external debuggers

;; Author: Rocky Bernstein <rocky@gnu.org>
;; Version: 1.0.0
;; Package-Type: multi
;; Package-Requires: ((load-relative "1.2") (loc-changes "1.2") (test-simple  "1.2.0") (cl-lib "0.5") (emacs "24"))
;; URL: http://github.com/node-inspect/node-inspect/
;; Keywords: node, nodejs, debugger

;; Copyright (C) 2018 Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; V8 protcol debugger for node --inspect
;;

;;; Code:

;; Press C-x C-e at the end of the next line configure the program in
;; for building via "make" to get set up.
;; (compile (format "EMACSLOADPATH=:%s:%s:%s ./autogen.sh" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "loc-changes.elc"))))

(require 'load-relative)

;; FIXME: extend require-relative for "autoload".
(defun node-inspect:load-features()
  (progn
    (require-relative-list
     '(
       "./node-inspect/run"
       "./node-inspect/handlers"
       ) "node-inspect-")
    (node-inspect:loaded-features)
    )
  )

(defun node-inspect-feature-starts-with(feature prefix)
  "node-inspect-strings-starts-with on stringified FEATURE and PREFIX."
  (declare (indent 1))
  (string-prefix-p (symbol-name feature) prefix)
  )

(defun node-inspect:loaded-features()
  "Return a list of loaded debugger features. These are the features
that start with 'node-inspect-' and 'node-inspect:'"

  (delq nil
		(mapcar (lambda (x) (and (string-match-p "^\\(node-inspect:\\|node-inspect-\\)" (symbol-name x)) x))
				features)))

(defun node-inspect:unload-features()
  "Remove all features loaded from this package. Used in
`node-inspect:reload-features'. See that."
  (let ((removal-set (node-inspect:loaded-features)))
	(dolist (feature removal-set)
	  (unload-feature feature t))
	removal-set)) ; return removed set

(defun node-inspect:reload-features()
  "Reload all features loaded from this package. Useful if have
changed some code or want to reload another version, say a newer
development version and you already have this package loaded."
  (interactive "")
  (node-inspect:unload-features)
  (node-inspect:load-features)
  )

;; Load everything.
(node-inspect:load-features)

;;; Autoloads-related code

;; This section is needed because package.el doesn't recurse into subdirectories
;; when looking for autoload-able forms.  As a workaround, we statically
;; generate our own autoloads, and force Emacs to read them by adding an extra
;; autoloded form.

;;;###autoload
(defconst node-inspect--recursive-autoloads-file-name "node-inspect-recursive-autoloads.el"
  "Where to store autoloads for subdirectory contents.")

;;;###autoload
(defconst node-inspect--recursive-autoloads-base-directory
  (file-name-directory
   (if load-in-progress load-file-name
     buffer-file-name)))

;;;###autoload
(with-demoted-errors "Error in Node-Inspect's autoloads: %s"
  (load (expand-file-name node-inspect--recursive-autoloads-file-name
                          node-inspect--recursive-autoloads-base-directory)
        t t))

(defun node-inspect--rebuild-recursive-autoloads ()
  "Update Node-Inspect's recursive autoloads.
This is needed because the package.el infrastructure doesn't
process autoloads in subdirectories; instead we create an
additional autoloads file of our own, and we load it from an
autoloaded form.  Maintainers should run this after adding
autoloaded functions, and commit the resulting changes."
  (interactive)
  (let ((generated-autoload-file
         (expand-file-name node-inspect--recursive-autoloads-file-name
                           node-inspect--recursive-autoloads-base-directory)))
    (when (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file))
    (dolist (name (with-no-warnings
                    (directory-files-recursively
                     node-inspect--recursive-autoloads-base-directory "" t)))
      (when (file-directory-p name)
        (update-directory-autoloads name)))))

(provide-me)

;;; node-inspect.el ends here
