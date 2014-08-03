;;; This is a wrapper around USER-MANUAL and XREF
;;; Collects the outputs from the tools in strings
;;; then preprocesses the strings to save in Markdown format

(cl:defpackage "CODE-DOCS"
  (:use "COMMON-LISP")
  (:export #:create-xrefdb-for-source
           #:save-docs-for-source
           #:build-documentation))

(in-package "CODE-DOCS")

(let ((xref-dbcreated (make-hash-table :test #'equal)))
  (defun spit-call-tree-to-stream (source-file)
    "spit-call-tree-to-stream: outputs XREF caller tree to a stream.
     Checks if XREF database for file is already created otherwise creates it.

     Arguments: source-file - full file path as string"
    (when (not (gethash source-file xref-dbcreated))
      (xref:xref-file source-file nil nil)
      (setf (gethash source-file xref-dbcreated) t))
    (setf string-call-tree (make-array '(0) :element-type 'base-char
                                       :fill-pointer 0 :adjustable t))
    (xref:print-caller-trees :compact t))

  (defun create-xrefdb-for-source (source-dir &optional (file-type "lisp"))
    "create-xrefdb-for-source: creates XREF database for all source files in SOURCE-DIR.
     Marks XREF-DBCREATED for the source file to TRUE.

     Arguments: source-dir - source directory path in string
                file-type - source file extension (default is lisp)"
    (setq source-files (directory (make-pathname :name :wild :type file-type
                                                 :defaults (concatenate 'string source-dir "**/"))))
    (xref:clear-tables)
    (dolist (source-file source-files)
      (setq source-file (namestring source-file))
      (xref:xref-file source-file nil nil)
      (setf (gethash source-file xref-dbcreated) t)
      ))
  )

(defmacro escape-markdown-before-output (&rest forms)
  "Escapes all the output strings for markdown format characters before
   passing them to standard output. FORMS represent valid LISP forms that
   produces output to standard output stream."
  `(let ((forms-output (make-array '(0) :element-type 'base-char
                                   :fill-pointer 0 :adjustable t)))
     ;; capture output to a string
     (with-output-to-string (*standard-output* forms-output)
       (progn ,@forms))
     ;; escape each line of output for markdown
     (with-input-from-string (stream forms-output)
       (do ((line (read-line stream nil "EOF")))
           ((string= line "EOF"))
         (if (zerop (length line)) (format t "~%") ;; only new-line character
             (progn
               (setq escaped-line (escape-line-markdown line))
               (format t "~A~%" escaped-line)))
         (setq line (read-line stream nil "EOF"))))))

(flet ((spit-documentation-for-file (source-file)
         (setq file-name (file-namestring source-file))
         (setq dir-path (directory-namestring source-file))
         (format t "~2&## Rudimentary Documentation for ~:@(~A~)" file-name)
         (format t "~2&*Containing Directory ~A*" dir-path)
         (format t "~2&### API Documentation~2%")
         (user-man:create-user-manual source-file :output-format :markdown)))

  (defun save-docs-for-source (source-dir &optional (file-type "lisp") doc-dir)
    "save-docs-for-source: saves source documentation in user_manual.md file.
     Also assumes that XREF database has been created for files in SOURCE-DIR.

     Arguments: source-dir - soure directory path in string
                file-type - source file extension (default is lisp)
                doc-dir - directory where user_manual.md is saved (default is source-dir"
    (when (not doc-dir) (setq doc-dir source-dir))
    (setq source-files (directory (make-pathname :name :wild :type file-type
                                                 :defaults (concatenate 'string source-dir "**/"))))
    (setq doc-file (make-pathname :name "user_manual" :type "md" :defaults doc-dir))
    (with-open-file (s doc-file :direction :output :if-exists :supersede)
      (let ((*standard-output* s))
        (dolist (source-file source-files)
          (spit-documentation-for-file source-file))
        (format t "~3&## Dependency Documentations")
        (format t "~2&### File Dependencies~2%")
        (escape-markdown-before-output
         (xref:print-file-dependencies))
        (format t "~2&### Call Dependencies~2%")
        (format t "~2&#### Function/Macro Calls~2%")
        (escape-markdown-before-output
         (xref:display-database :callers))
        (format t "~2&#### Variable Readers~2%")
        (escape-markdown-before-output
         (xref:display-database :readers))
        (format t "~2&#### Variable Setters~2%")
        (escape-markdown-before-output
         (xref:display-database :setters)))))
  )

(defmacro build-documentation (for file-type files in source-dir save at doc-dir)
  `(progn
     (setq xref:*handle-package-forms* '(lisp::in-package))
     (create-xrefdb-for-source ,source-dir ,file-type)
     (save-docs-for-source ,source-dir ,file-type ,doc-dir)))

(defun escape-line-markdown (line)
  "Escapes the Markdown format characters if present in LINE"
  (when (stringp line)
    (setq line-chars (mapcan #'(lambda (x) (case x
                                             ((#\* #\_ #\# #\` #\!) (list #\\ x))
                                             (otherwise (list x)))) (coerce line 'list)))
    (coerce line-chars 'string)))


