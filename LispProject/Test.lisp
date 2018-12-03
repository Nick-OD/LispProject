;;;; -*- Mode: Lisp; -*-

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

(defun addspouse (spouse-name p)
  (setf (person-spouses p) (cons spouse-name (person-spouses p)))
;  (setf (person-spouses p) (sort (person-spouses p) #'string<
)

(defun addchild (child-name p)
  (setf (person-children p) (cons child-name (person-children p)))
)

(defun getparents (p)
  (person-parents (gethash p famTree)))

(defun getchildren (p)
  (person-children (gethash p famTree)))

(defun getspouses (p)
  (person-spouses (gethash p famTree)))

(defun ancestors (p tree)
  (let ((parent1 (personstruct(first (person-parents p))tree ))
        (parent2 (personstruct (second (person-parents p))tree )))
    (if (null parent1)
         nil
       (append (list(person-name parent1) (person-name parent2)) (ancestors parent1 tree )(ancestors parent2 tree )))))

; is functions
(defun isspouse (p1 p2)
; Return boolean true or false
(let ((spouse nil))
(if  (member (person-name p1) (person-spouses p2) :test #'equal) (setf spouse t))
(if spouse t nil))) ; If spouse, spouse is true, else spouse is false 

)

(defun ischild (p1 p2)

)

(defun issibling (p1 p2 tree)

)

(defun iscousin (p1 p2 tree)

)

(defun isancestor (p1 p2 tree)

)

(defun isunrelated (p1 p2 tree)

)








(defun family()
  (let ((tree (make-hash-table :size 1000 :test #'equal)))
 (let ((line nil) (tokens nil) (p1 nil) (p2 nil) (p3 nil))
(let (line (read *standard-input*))
   (loop for line
         until (eq line nil)
         do (
             print line
      (cond ((string= (first line) "E" )
             ;Person creation and spouse children assignment here
              (p1 (second line))
              (p2 (third line))
              
              ; Test to see if tree, store if not
              (if (= (personstruct p1 tree) nil)
                (setf person1 (storeperson p1 (make-person :name p1) tree))
                )
              (if (= (personstruct p2 tree) nil)
                  (setf person2 (storeperson p2 (make-person :name p2) tree))
                )
              
              (loop for key being each hash-key of tree
                    using (hash-value value)
                    collect (list key value))
              (format t "~%" tree)

            )
            ((string= (first line) "X")
             ;Xquery stuff here
              (print (second line))


              )
            ((string= (first line) "W")
            ;Wquery stuff here
              (print (second line))
              ))

)))))))
