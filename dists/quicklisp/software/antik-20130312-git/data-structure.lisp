;; Keyword table data structure
;; Jamison Hope 2011-01-18 12:58:19EST data-structure.lisp
;; Time-stamp: <2011-05-23 22:31:56EDT data-structure.lisp>
;; $Id: $

(defun create-table ()
  (let ((ht (make-hash-table)))
    `#(,ht ,nil)))       ; map, list

(defun insert-object (table object keywords)
  (if (null keywords)
      (push object (cl:aref table 1))
    (mapcar (lambda (key)
              (let ((rest (remove key keywords :count 1))
                    (next-table (gethash key (cl:aref table 0))))
                (unless next-table
                  (setf next-table (create-table))
                  (setf (gethash key (cl:aref table 0)) next-table))
                (insert-object next-table object rest)))
            keywords)))

(defun flatten-table (table)
  (if (null table) '()
    (let ((hashed '()))
      (maphash (lambda (key value)
                 (setf hashed (append hashed (flatten-table value))))
               (cl:aref table 0))
      (delete-duplicates (append hashed (cl:aref table 1))))))

;;; To do: return second value with tags for each object.
(defun find-object (table keywords)
  (if (null keywords)
      (flatten-table table)
    (find-object (gethash (car keywords) (cl:aref table 0))
                 (cdr keywords))))

(defun remove-object (table object)
  (when table
    (maphash (lambda (key value)
               (remove-object value object))
             (cl:aref table 0))
    (setf (cl:aref table 1) (remove object (cl:aref table 1)
                                 :test #'equal))))

#|
(setf *table* (create-table))
#(#<HASH-TABLE :TEST EQL :COUNT 0 {1003ECF3E1}> NIL)
* (insert-object *table* "JRH" '(jamie initials))
((("JRH")) (("JRH")))
* (find-object *table* '(jamie initials))
("JRH")
* (find-object *table* '(jamie))
("JRH")
* (find-object *table* '(initials))
("JRH")
* (find-object *table* '(initials liam))
NIL
* (insert-object *table* "LMH" '(liam initials))
((("LMH")) (("LMH")))
* (find-object *table* '(initials liam))
("LMH")
* (find-object *table* '(liam initials))
("LMH")
* (find-object *table* '(liam))
("LMH")
* (find-object *table* '(initials))
("JRH" "LMH")
* (remove-object *table* "JRH")
NIL
* (find-object *table* '(initials))
("LMH")
|#
