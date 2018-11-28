;;;; -*- Mode: Lisp; -*-


(setq famTree (make-hash-table :test #'string= ))



(defstruct person 
  (name nil)
  (parents (list))
  (children (list))
  (spouses (list))
)



(defun storeperson (name struct tree)
;  "Students need to write this! This should enter the person structure in STRUCT into the hashtable in FAMY-TREE with the key in SYMBOL-NAME."
(setf (gethash name tree) struct))


(defun personstruct (name tree)
 ; "Returns a structure of type person corresponding to the key SYMBOL-NAME in the hashtable FAMILY-TREE. If there is no one in the tree with the name in SYMBOL-NAME, returns NIL."
  (gethash name tree nil))


(defun getparents (p)
  (person-parents (gethash p famTree)))

(defun getchildren (p)
  (person-children (gethash p famTree)))

(defun getspouses (p)
  (person-spouses (gethash p famTree)))

(defun ancestors (p tree)
  (let ((parent1 (get-person (first (person-parents p))tree ))
        (parent2 (get-person (second (person-parents p))tree )))
    (if (null parent1)
         nil
       (append (list parent1 parent2) (ancestors parent1 tree )(ancestors parent2 tree )))))






(defun family()
 (let ((line nil) (tokens nil) (p1 nil) (p2 nil) (p3 nil)
       (line "(E John Mary Zach)")
;  (with-open-file (str Testcase.txt
;                     :direction INPUT
;                     :if-does-not-exist )
;    (do ((line (read-line stream nil)
;             (read-line stream nil)))
;        ((null line))
      (print line)
      (setf tokens (split-sequence " " line))
      (cond ((string= (first tokens) "E")
             (;Person creation and spouse children assignment here
              (print (second words))
              ))
            ((string = (first tokens) "X")
             (;Xquery stuff here
              
              ))
            ((string = (first tokens) "W"))
            (;Wquery stuff here

             ))

))










 )