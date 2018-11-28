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
(setf (gethash name tree) person))


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
 (let ((line nil) (tokens nil) (p1 nil) (p2 nil) (p3 nil))
       (setf queries (list 'E 'X 'W))
       (setf line (list (E John Mary Zach)))
;  (with-open-file (str Testcase.txt
;                     :direction INPUT
;                     :if-does-not-exist )
;    (do ((line (read-line stream nil)
;             (read-line stream nil)))
;        ((null line))
      (print line)
     ; (let tokens (split-sequence " " line))
      (cond ((equal (first line) (first queries))
             (;Person creation and spouse children assignment here
              (format t ~a~% line)
              ; Test to see if tree
            ;  (setf p1 (gethash (second line) famTree))
           ;   (setf p1 (make-person :name (second line)))
           ;   (setf storeperson(p1 person famTree))

            ;  (setf p2 (gethash (second line) famTree))
            ;  (setf p2 (make-person :name (second line)))
           ;   (setf storeperson(p2 person famtree))
              ))
            ((equal (first line) (second queries))
             (;Xquery stuff here
              (print (second line))
              ))
            ((equal (first line) (second queries))
            (;Wquery stuff here
              (print (second line))
             ))

)))