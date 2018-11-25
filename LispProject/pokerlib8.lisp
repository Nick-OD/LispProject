;;;; -*- Mode: Lisp; -*-

;;Texas Hold'em GAME DRIVER
;;Author: Frank Klassner
;;Course: CSC 4500
;;Last Edit: 4 May 2013

;;EXAMPLE: After loading and compiling this file, 
;;         Try typing (holdem-game-driver *test-players* :verbose t :pauser t)
;;         to see a step-by-step execution.
;;         Type (holdem-game-driver *test-players* :verbose t)
;;         to see a whole game generated at once.
;;
;;Scroll down to "make-holdem-agent" definition to see how to program your
;;own poker agent.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;USEFUL UTILITY FUNCTIONS
;;;;NOT LIMITED TO TEXAS HOLDEM

(defmacro while (test &rest body)
  `(loop 
    (when (not ,test) (return nil))
    ,@body))


(defun mapc-= (fn list)
  "Returns T iff all applications of the function in FN on each item 
in LIST result in the same numerically equal number."
  (when list
    (let ((old (funcall fn (first list))))
      (dolist (item list t)
        (when (not (= old (setf old (funcall fn item))))
          (return nil))))))

(defun extremum (list &key (decider #'(lambda (x y) (max x y))) (key #'identity))
  "Returns the item in LIST that is considered an EXTREMUM according to the
function in DECIDER.  This can find 'biggest' or 'littlest' items, depending
on what you put in DECIDER."
  (let ((mx (first list)))
    (dolist (item (rest list))
      (setf mx (funcall decider (funcall key mx) (funcall key item))))
    mx))


(defun randomize-list (itemlist)
  "Generates a new copy of ITEMLIST with a random reordering of the items in it."
  (let ((newlist (LIST))
        (listhash (make-array (length itemlist) :initial-element nil)))
    (dotimes (i (length itemlist))
      (let ((itemhash nil))
        (while (aref listhash (setf itemhash (random (length itemlist)))))
        (setf (aref listhash itemhash) (nth itemhash itemlist))
        (setf newlist (cons (nth itemhash itemlist) newlist))))
    newlist))


(defun correctnumbits (i n k)
  "Helper function for COMBOGENERATOR."
  (declare (optimize (safety 0) (speed 3) (fixnum-safety 0)))
  (= (the integer (loop for j from 0 to (1- n) summing (the integer (if (logbitp (the integer j) (the integer i)) 1 0))))
     (the integer k)))


(defun combogenerator (list k)
  "Unoptimized function for creating a list of all 'length-of-list choose k' combinations."
  (let* ((n (length list)))
    (loop for i from 0 to (1- (expt 2 n)) nconcing
          (when (correctnumbits i n k)
              (list (loop for j from (1- n) downto 0 nconcing
                          (when (logbitp j i)
                            (list (nth (abs (- j (1- n))) list)))))))))


(let ((indexlist '((2 3 4 5 6) (1 3 4 5 6) (1 2 4 5 6) (1 2 3 5 6) (1 2 3 4 6) (1 2 3 4 5)
                   (0 3 4 5 6) (0 2 4 5 6) (0 2 3 5 6) (0 2 3 4 6) (0 2 3 4 5) (0 1 4 5 6)
                   (0 1 3 5 6) (0 1 3 4 6) (0 1 3 4 5) (0 1 2 5 6) (0 1 2 4 6) (0 1 2 4 5)
                   (0 1 2 3 6) (0 1 2 3 5) (0 1 2 3 4))))
(defun comboemitter7c5 (i items)
  "Returns the 0-based ith combination in the list of 21 combinations that you can make choosing 5 elements at a time from 7 elements."
    (when (not (<= 0 i 20)) (error "COMBOEMITTER7c5 can only emit 0th thru 20th of the 21 possible 7c5 combinations of 7 items; I [~A] is an illegal combination." i))
    (when (not (= (length items) 7)) (error "COMBOEMITTER7c5 can only work on a list of 7 items; ITEMS [~A] has an illegal number of items." items))
    (let ((index (nth i indexlist)))
      (list (nth (first index) items) (nth (second index) items) (nth (third index) items) (nth (fourth index) items) (nth (fifth index) items))))
  )

(let ((indexlist '((1 2 3 4 5) (0 2 3 4 5) (0 1 3 4 5) (0 1 2 4 5) (0 1 2 3 5) (0 1 2 3 4))))
(defun comboemitter6c5 (i items)
  "Returns the 0-based ith combination in the list of 6 combinations that you can make choosing 5 elements at a time from 6 elements."
    (when (not (<= 0 i 5)) (error "COMBOEMITTER6c5 can only emit 0th thru 5th of the 6 possible 6c5 combinations of 6 items; I [~A] is an illegal combination." i))
    (when (not (= (length items) 6)) (error "COMBOEMITTER6c5 can only work on a list of 6 items; ITEMS [~A] has an illegal number of items." items))
    (let ((index (nth i indexlist)))
      (list (nth (first index) items) (nth (second index) items) (nth (third index) items) (nth (fourth index) items) (nth (fifth index) items))))
  )

(defun permugenerator (list)
  (if (null list)
      (list nil)
      (mapcan #'(lambda (x)
                  (mapcar #'(lambda (y) (cons x y))
                         (permugenerator (remove x list))))
             list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;GENERIC CARD DATA STRUCTURES AND MAINTENANCE CODE;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;CARD DECK CODE:
(defstruct (deck
            (:print-function print-deck)
            (:copier nil)
            (:constructor make-deck))
  (cards nil)
  (hash nil)
  (top 0))


(defun copy-deck (deck)
  "Returns a copy of DECK that is memory-distinct from DECK."
  (let ((d (make-deck))
        (h (make-hash-table :size 52 :test #'eql)))
    (setf (deck-cards d) (copy-seq (deck-cards deck)))
    (setf (deck-top d) (deck-top deck))
    (maphash #'(lambda (key val) ;; populate new "h" hash with same contents of old hash;  NO COPIES ARE MADE OF CONTENTS!
                 (setf (gethash key h) val)) 
             (deck-hash deck))
    (setf (deck-hash d) h)
    d))


(defun print-deck (obj str depth)
  "Displays a DECK structure to show how many deals are left in it."
  (declare (ignore depth))
  (format str "<DECK (~d deals)>" (- (length (deck-cards obj)) (deck-top obj)))
  obj)


(defun create-deck (&key (size 52))
  "Creates a DECK structure with SIZE number of playing cards.
Cards are represented by numbers between 0 and 51, inclusive."
  (let ((d (make-deck))
        (cardhash (make-hash-table :size 52 :test #'eql))
        (a (make-array (list size))))
    (dotimes (i size) ;create the deck of cards in some shuffled order
      (let ((card nil))
        (while (gethash (setf card (random 52)) cardhash nil))
        (setf (gethash card cardhash) card)
        (setf (aref a i) card)))
    (setf (deck-cards d) a)
    (setf (deck-hash d) cardhash)
    (setf (deck-top d) 0)
    d))



(defun deal-card (deck)
  "Pops the next card in DECK. Returns NIL when deck is empty."
  (let ((oldtop (deck-top deck))
        (cards (deck-cards deck)))
    (when (< oldtop (length cards))
      (incf (deck-top deck))
      (aref cards oldtop))))


(defun shuffle-deck (deck)
  "Restocks DECK with 52 shuffled cards."
  (let ((cardhash (deck-hash deck))
        (size (length (deck-cards deck)))
        (a (deck-cards deck)))
    (clrhash cardhash)
    (dotimes (i size) ;create the deck of cards in some shuffled order
      (let ((card nil))
        (while (gethash (setf card (random 52)) cardhash nil)) ;; look for random values in hashtable until open slot found
        (setf (gethash card cardhash) card) ;; put random value in hashtable
        (setf (aref a i) card))) ;; put the card in the deck
    (setf (deck-top deck) 0)
    deck))


(defun printcard (card &optional (stream nil))
  "Displays a card number as a string showing rank and suit."
  (format stream "~A~A" (rank card) (suit card)))

(defun suit (card)
  (nth (floor card 13) '(C D H S)))

(defun rank (card)
  (nth (rem card 13) '(2 3 4 5 6 7 8 9 T J Q K A)))

(defun ranknum (card)
  (+ 2 (rem card 13)))

(defun suitnum (card)
  (floor card 13))


(defun cardsymbol-to-num (symbol)
  "Returns the number in [0..51] that the cardsymbol represents (i.e. '2c --> 0)."
  (let ((st (format nil "~A" symbol)))
    (+ (* 13 (case (aref st 1)
               (#\C 0) (#\D 1) (#\H 2) (#\S 3) (t -1)))
       (case (aref st 0)
         (#\2 0) (#\3 1) (#\4 2) (#\5 3) (#\6 4) (#\7 5) (#\8 6)
         (#\9 7) (#\T 8) (#\J 9) (#\Q 10) (#\K 11) (#\A 12) (t -1)))))


(defun sym-to-hand (list)
  "Returns a list of the numbers that represent the cardsymbols in LIST.
The numbers will be sorted according to their cards' ranks.
EG: '(2C TH 4C) will return  '(0 2 34)." 
  (sort (mapcar #'cardsymbol-to-num list) #'< :key #'ranknum))


(defun hand-to-sym (list)
  "Returns a list of the cardsymbol atoms that the integers 
in LIST represent.  No sorting is done.
EG: '(0 34 2) will return  '(2C TH 4C)." 
  (mapcar #'num-to-cardsymbol list))



(defun num-to-cardsymbol (num)
  "Returns the atom cardsymbol that the integer NUM represents."
  (intern (format nil "~A~A" (rank num) (suit num)) 'user))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;STRUCTURES AND SUPPORT CODE FOR MAINTAINING A HOLDEM POT

(defstruct (holdempot
            (:copier nil)
            )
  (value 0)
  (members 0)
  (required-commitment 0)
  (commitments nil))



(defun copy-holdempot (p)
  (make-holdempot
   :value (holdempot-value p)
   :members (copy-list (holdempot-members p))
   :required-commitment (holdempot-required-commitment p)
   :commitments (mapcar #'copy-list (holdempot-commitments p))))

(defun create-holdempot (members)
  (make-holdempot
   :value 0
   :members (copy-list members)
   :required-commitment 0
   :commitments (loop for m in members collecting (cons m 0))))


(defun holdemround-pot (holdemround)
  "Returns the sum of all pots."
  (and (typep holdemround 'HOLDEMROUND)
       (holdempot-value (holdemround-internalpot holdemround))))


(defun unmet-player-commitment (playerid pot)
  "Returns the amount of money the agent with PLAYERID must contribute to POT to remain in the game."
  (if (member playerid (holdempot-members pot))
      (- (holdempot-required-commitment pot) (cdr (assoc playerid (holdempot-commitments pot))))
      0))



(defun generate-winner-check-pots (pot)
  "Creates the split pots list structure suitable for 
storing in the HOLDEMROUND-POTS slot of a HOLDEMROUND."
  (let* ((remaining-pot (holdempot-value pot))
         (buyinlist (sort (copy-list (holdempot-commitments pot)) #'< :key #'cdr))
         (levellist (loop for c in buyinlist collecting
                          (let ((partners nil))
                            (loop for x in buyinlist doing
                                  (when (<= (cdr c) (cdr x))
                                    (push (first x) partners)))
                            (list partners (cdr c)))))
         (prev-level 0) (prevhash nil)
         (hashval) (potlist nil)
         (pothash (make-hash-table :test #'equal)))
    (setf levellist (remove-if #'(lambda (x) (equal (second x) 0)) levellist))
    (loop for level in levellist doing
          (setf prevhash (first level))
          (setf hashval (gethash (first level) pothash 0))
          (setf (gethash (first level) pothash)
                (+ hashval (* (length (first level)) (- (second level) prev-level))))
          (setf remaining-pot (- remaining-pot (* (length (first level)) (- (second level) prev-level))))
          (setf prev-level (second level)))

    (incf (gethash prevhash pothash) remaining-pot) ;;add whatever's left to last subpot
    (maphash #'(lambda (key val) (push (list key val) potlist)) pothash)
    (sort potlist #'> :key #'second)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PREDICATES AND CODE SPECIFICALLY FOR TEXAS HOLDEM
;;
;;

;;These predicates all assume hand card codes numbers are sorted in ascending
;;rank order. They return nil if the hand does not meet the category test.
;;
;;If the hand DOES meet the predicate test, they return a list of numbers that
;;can be compared to results from other predicates to break ties between hands
;;of the same category, or to compare hands from different categories.
;;SEE BETTERHAND, PICK-WINNER, and HAND-STRENGTH for examples of how the results 
;;of these predicates can be compared or used.

(defun flushp (hand)
  "HAND is a list of numbers, not symbols!"
  (when (and (listp hand) (= (length hand) 5) (mapc-= #'suitnum hand))
    (list (categorycode :FLUSH)
          (ranknum (fifth hand)) (ranknum (fourth hand)) (ranknum (third hand)) 
          (ranknum (second hand)) (ranknum (first hand)))))


(defun straightp (hand)
  "HAND is a list of numbers, not symbols!"
  (when (and (listp hand) (= (length hand) 5))
    (let ((ranks (mapcar #'ranknum hand)))
      (cond ((equal ranks '(2 3 4 5 14))
             (list (categorycode :STRAIGHT) 5))
            ((and (= (first ranks) (- (fifth ranks) 4)) (apply #'< ranks))
             (list (categorycode :STRAIGHT) (fifth ranks)))
            (t nil)))))


(defun straightflushp (hand)
  "HAND is a list of numbers, not symbols!"
  (when (and (listp hand) (= (length hand) 5) (mapc-= #'suitnum hand))
    (let ((ranks (mapcar #'ranknum hand)))
      (cond ((equal ranks '(2 3 4 5 14))
             (list (categorycode :STRAIGHTFLUSH) 5))
            ((and (= (first ranks) (- (fifth ranks) 4)) (apply #'< ranks))
             (list (categorycode :STRAIGHTFLUSH) (fifth ranks)))
            (t nil)))))


(let ((rankcts (make-array '(15)))
      (multict (make-array '(5))))
  (defun pairp (hand)
    "HAND is a list of numbers, not symbols."
    (dotimes (i 15)
      (setf (aref rankcts i) 0))
    (dotimes (i 5)
      (setf (aref multict i) 0))
    (loop for card in hand doing 
          (incf (aref rankcts (ranknum card))))
    (loop for r from 2 to 14 doing
          (incf (aref multict (aref rankcts r))))
    (when (and (= (aref multict 2) 1)
               (= (aref multict 3) 0))
        `(,(categorycode :PAIR) 
          ,(loop for rank from 14 downto 2 doing
                 (when (= (aref rankcts rank) 2)
                   (return rank)))
          ,@(loop for rank from 14 downto 2 appending
                  (when (= (aref rankcts rank) 1)
                    (list rank))))))


  (defun tripsp (hand)
    "HAND is a list of numbers, not symbols."
    (when (and (listp hand) (= (length hand) 5))
      (dotimes (i 15)
        (setf (aref rankcts i) 0))
      (dotimes (i 5)
        (setf (aref multict i) 0))
      (loop for card in hand doing 
            (incf (aref rankcts (ranknum card))))
      (loop for r from 2 to 14 doing
            (incf (aref multict (aref rankcts r))))
      (when (and (= (aref multict 2) 0)  ;there must be exactly zero pairs
                 (= (aref multict 3) 1)) ;and exactly one triple
        `(,(categorycode :TRIPS)
          ,(loop for rank from 14 downto 2 doing
                 (when (= (aref rankcts rank) 3)
                   (return rank)))
          ,@(loop for rank from 14 downto 2 appending
                  (when (= (aref rankcts rank) 1)
                    (list rank)))))))
    

  (defun fullhousep (hand)
    (when (and (listp hand) (= (length hand) 5))
      (dotimes (i 15)
        (setf (aref rankcts i) 0))
      (dotimes (i 5)
        (setf (aref multict i) 0))
      (loop for card in hand doing 
            (incf (aref rankcts (ranknum card))))
      (loop for r from 2 to 14 doing
            (incf (aref multict (aref rankcts r))))
      (when (and (= (aref multict 2) 1)  ;there must be exactly one pair
                 (= (aref multict 3) 1)) ;and exactly one triple
        (list (categorycode :FULLHOUSE)
              (loop for rank from 14 downto 2 doing
                    (when (= (aref rankcts rank) 3)
                      (return rank)))
              (loop for rank from 14 downto 2 doing
                    (when (= (aref rankcts rank) 2)
                      (return rank)))))))


 (defun twopairp (hand)
   (when (and (listp hand) (= (length hand) 5))
     (dotimes (i 15)
       (setf (aref rankcts i) 0))
     (dotimes (i 5)
       (setf (aref multict i) 0))
     (loop for card in hand doing 
           (incf (aref rankcts (ranknum card))))
     (loop for r from 2 to 14 doing
           (incf (aref multict (aref rankcts r))))
     (when (and (= (aref multict 2) 2)  ;there must be exactly two pairs
                (= (aref multict 1) 1)) ;and exactly one unmatched card
       `(,(categorycode :TWOPAIR)
         ,@(loop for rank from 14 downto 2 appending
                 (when (= (aref rankcts rank) 2)
                   (list rank)))
         ,@(loop for rank from 14 downto 2 appending
                 (when (= (aref rankcts rank) 1)
                   (list rank)))))))


 (defun fourkindp (hand)
    (when (and (listp hand) (= (length hand) 5))
      (dotimes (i 15)
        (setf (aref rankcts i) 0))
      (dotimes (i 5)
        (setf (aref multict i) 0))
      (loop for card in hand doing 
            (incf (aref rankcts (ranknum card))))
      (loop for r from 2 to 14 doing
            (incf (aref multict (aref rankcts r))))
      (when (and (= (aref multict 4) 1)
                 (= (aref multict 1) 1))
        (list (categorycode :FOURKIND)
              (loop for rank from 14 downto 2 doing
                    (when (= (aref rankcts rank) 4)
                      (return rank)))
              (loop for rank from 14 downto 2 doing
                    (when (= (aref rankcts rank) 1)
                      (return rank)))))))

 (defun highcardp (hand)
   (when (and (listp hand) (= (length hand) 5))
     (dotimes (i 15)
       (setf (aref rankcts i) 0))
     (dotimes (i 5)
       (setf (aref multict i) 0))
     (loop for card in hand doing
           (incf (aref rankcts (ranknum card))))
     (loop for r from 2 to 14 doing
           (incf (aref multict (aref rankcts r)))) 
     (when (and (not (mapc-= #'suitnum hand))
                (= (aref multict 1) 5))
       (cons (categorycode :HIGHCARD)
             (sort (mapcar #'ranknum hand) #'>))))) 

)
    

(defun hand-strength (h)
  "Returns a single number that encodes the ranking of hand H, which must be a list of 5 numbers from 0 to 51."
  (let ((h1 (sort (copy-list h) #'< :key #'ranknum))
        (handrankcodes nil))
    (cond ((null h1) 1)
          ((setf handrankcodes (straightflushp h1)) (+ (expt 2 20) (* (expt 2 16) (second handrankcodes))))
          ((setf handrankcodes (fourkindp h1)) (+ (expt 2 20) (* (expt 2 12) (second handrankcodes)) (third handrankcodes))) 
          ((setf handrankcodes (fullhousep h1)) (+ (expt 2 20) (* (expt 2 8) (second handrankcodes)) (third handrankcodes)))
          ((setf handrankcodes (flushp h1)) (+ (expt 2 19) (loop for rank in (rest handrankcodes) summing (expt 2 rank))))
          ((setf handrankcodes (straightp h1)) (+ (expt 2 18) (second handrankcodes)))
          ((setf handrankcodes (tripsp h1)) (+ (expt 2 17) (* (expt 2 13) (second handrankcodes)) (* (expt 2 8) (third handrankcodes)) (fourth handrankcodes)))
          ((setf handrankcodes (twopairp h1)) (+ (expt 2 16) (* (expt 2 12) (second handrankcodes)) (* (expt 2 7) (third handrankcodes)) (fourth handrankcodes)))
          ((setf handrankcodes (pairp h1)) (+ (* (expt 2 12) (second handrankcodes)) (* (expt 2 8) (third handrankcodes)) (* (expt 2 4) (fourth handrankcodes)) (fifth handrankcodes)))
          (t
           (+ (loop for i in (mapcar #'ranknum h1) summing (expt 2 (- i 2))))))))


(defun hand-ranking (h)
  "Returns a list of numbers that encodes the ranking of hand H, which must be a list of 5 numbers from 0 to 51. 
 This list can be used in, for example, the BETTERHAND function to compare two poker hands.  Use the function
HANDTYPE on H if you need to know what the symbolic ranking is. Use HAND-STRENGTH to get a numeric measure of
how strong the hand is.  The contents of H must be sorted in ascending rank order."
  (cond ((straightflushp h))
        ((fourkindp h))
        ((fullhousep h))
        ((flushp h))
        ((straightp h))
        ((tripsp h))
        ((twopairp h))
        ((pairp h))
        (t
         (cons (categorycode :highcard) (sort (mapcar #'ranknum h) #'>)))))

(defun hand-ranking2 (h)
  "Returns a list of numbers that encodes the ranking of hand H, which must be a list of 5 numbers from 0 to 51. 
 This list can be used in, for example, the BETTERHAND function to compare two poker hands.  Use the function
HANDTYPE on H if you need to know what the symbolic ranking is. Use HAND-STRENGTH to get a numeric measure of
how strong the hand is.  The contents of H must be sorted in ascending rank order."
  (cond ((highcardp h))
        ((pairp h))
        ((twopairp h))
        ((tripsp h))
        ((straightp h))
        ((flushp h))
        ((fullhousep h))
        ((fourkindp h))
        ((straightflushp h))))



(defun categorycode (category)
  (position category
            '(:HIGHCARD :PAIR :TWOPAIR :TRIPS :STRAIGHT :FLUSH :FULLHOUSE :FOURKIND :STRAIGHTFLUSH)))


(defun best-texas-holdem-hand (public private)
 "Returns a 5-card hand (5 numbers 0-51) representing the highest-ranked hand that can be made from the
cards in the list PUBLIC and the two cards in the list PRIVATE.  Neither PUBLIC nor PRIVATE need to be
sorted in any order.  PUBLIC can be of length 3, 4, or 5.  PUBLIC and PRIVATE must contain numbers from
0 to 51 representing cards."
  (extremum (combogenerator (sort (nconc (copy-list public) (copy-list private)) #'< :key #'ranknum) 5) :decider #'betterhand))




(defun best-texas-holdem-hand2 (public private)
 "Returns a 5-card hand (five numbers 0-51) representing the highest-ranked hand that can be made from the
cards in the list PUBLIC and the two cards in the list PRIVATE.  Neither PUBLIC nor PRIVATE need to be
sorted in any order.  PUBLIC must be of length 5.  PUBLIC and PRIVATE must contain numbers from 0 to 51
representing cards. This function uses space and time optimizations over BEST-TEXAS-HOLDEM-HAND, and
is strictly intended as a helper for CHANCES-OF-BEATING-OPPONENT."
  (let* ((cardset (sort (append public private) #'< :key #'ranknum))
              (besthand (comboemitter7c5 0 cardset))
              (newhand))
         (loop for i from 1 to 20 doing
               (setf newhand (comboemitter7c5 i cardset))
               (when (hand> newhand besthand)
                 (setf besthand newhand)))
         besthand))



(defun best-texas-holdem-hand3 (public private)
 "Returns a 5-card hand (5 numbers 0-51) representing the highest-ranked hand that can be made from the
cards in the list PUBLIC and the two cards in the list PRIVATE.  Neither PUBLIC nor PRIVATE need to be
sorted in any order.  PUBLIC can be of length 3, 4, or 5.  PUBLIC and PRIVATE must contain numbers from
0 to 51 representing cards. This function uses space and time optimizations over BEST-TEXAS-HOLDEM-HAND."
  (case (length public)
    (5 (let* ((cardset (sort (append public private) #'< :key #'ranknum))
              (besthand (comboemitter7c5 0 cardset))
              (newhand))
         (loop for i from 1 to 20 doing
               (setf newhand (comboemitter7c5 i cardset))
               (when (hand> newhand besthand)
                 (setf besthand newhand)))
         besthand))
    (4 (let* ((cardset (sort (append public private) #'< :key #'ranknum))
              (besthand (comboemitter6c5 0 cardset))
              (newhand))
         (loop for i from 1 to 5 doing
               (setf newhand (comboemitter6c5 i cardset))
               (when (hand> newhand besthand)
                 (setf besthand newhand)))
         besthand))
    (3 (let ((cardset (sort (append public private) #'< :key #'ranknum)))
         cardset))))



(defun num-hands-beating-opponent (public private opponent)
  (let ((cardset (append public private))
        (opponentcardset (append public opponent)))
    (case (length cardset)
      (7 (if (hand> (extremum (combogenerator (sort cardset #'< :key #'ranknum) 5) :decider #'betterhand)
                    (extremum (combogenerator (sort opponentcardset #'< :key #'ranknum) 5) :decider #'betterhand))
             1
             0))
      (6 (let ((universe (loop for c from 0 to 51 nconcing (when (and (not (member c cardset)) (not (member c opponent))) (list c)))))
           (loop for addedcard in universe summing
                 (let ((fullcardset (cons addedcard (copy-list cardset)))
                       (fullopponentcardset (cons addedcard (copy-list opponentcardset))))
                   (if (hand> (extremum (combogenerator (sort fullcardset #'< :key #'ranknum) 5) :decider #'betterhand)
                              (extremum (combogenerator (sort fullopponentcardset #'< :key #'ranknum) 5) :decider #'betterhand))
                       1
                       0)))))
      (5 (let* ((universe1 (loop for c from 0 to 51 nconcing (when (and (not (member c cardset)) (not (member c opponent))) (list c)))))
           (loop for addedcard1 in universe1 summing
                   (loop for addedcard2 in (rest (member addedcard1 universe1)) summing
                         (let ((fullcardset (cons addedcard1 (cons addedcard2 (copy-list cardset))))
                               (fullopponentcardset (cons addedcard1 (cons addedcard2 (copy-list opponentcardset)))))
                           (if (hand> (extremum (combogenerator (sort fullcardset #'< :key #'ranknum) 5) :decider #'betterhand)
                                      (extremum (combogenerator (sort fullopponentcardset #'< :key #'ranknum) 5) :decider #'betterhand))
                               1
                             0))))))
      ;when no public cards are specified, we assume we're preflop and to speed calculations up we only determine statistics for flop (i.e. best hands in 5 cards, not 7)
      (2 (let ((universe (loop for c from 0 to 51 nconcing (when (and (not (member c private)) (not (member c opponent))) (list c)))))
           (loop for addedcard1 in universe summing
                 (loop for addedcard2 in (rest (member addedcard1 universe)) summing
                       (loop for addedcard3 in (rest (member addedcard2 universe)) summing
                             (let ((fullcardset (cons addedcard1 (cons addedcard2 (cons addedcard3 (copy-list private)))))
                                   (fullopponentcardset (cons addedcard1 (cons addedcard2 (cons addedcard3 (copy-list opponent))))))
                         (if (hand> (sort fullcardset #'< :key #'ranknum)
                                    (sort fullopponentcardset #'< :key #'ranknum))
                             1
                             0)))))))
      (t (error "NUM-HANDS-BEATING-OPPONENT called with PUBLIC [~A] and PRIVATE [~A] lists of cards whose combined lengths do not add up to 2, 5, 6, or 7." public private)))))




(defun chances-of-beating-opponent (public private opponent)
  "Returns the percentage of all possible hands in which the PRIVATE hand beats the OPPONENT's private hand in heads-up play, 
given the common cards in PUBLIC.  Note that all lists must be numeric card codes, not symbols. Note also that
when PUBLIC is empty, to speed up calculations the function only calculates percentages for beating the opponent
over the flop (not all possible 5-card common card sets, only a 3-card common card set)."
  (when (or (not (listp private)) (not (= 2 (length private))))
    (error "CHANCES-OF-BEATING-OPPONENT called with PRIVATE card list [~A] whose length is not exactly 2." private))
  (when (or (not (listp private)) (not (= 2 (length opponent))))
      (error "CHANCES-OF-BEATING-OPPONENT called with OPPONENT card list [~A] whose length is not exactly 2." opponent))
  (case (length public)
    (5 (if (hand> (best-texas-holdem-hand2 (copy-list public) (copy-list private))
                  (best-texas-holdem-hand2 (copy-list public) (copy-list opponent)))
           1.0
           0.0))
    (4 (let ((universe (loop for c from 0 to 51 nconcing (when (and (not (member c private)) (not (member c opponent)) (not (member c public))) (list c)))))
         (/ (loop for addedcard in universe summing
               (let ((fullpublic (append (list addedcard) (copy-list public))))
                 (if (hand> (best-texas-holdem-hand2 fullpublic (copy-list private))
                            (best-texas-holdem-hand2 fullpublic (copy-list opponent)))
                     1
                     0)))
            44.0)))
    (3 (let* ((universe (loop for c from 0 to 51 nconcing (when (and (not (member c private)) (not (member c opponent)) (not (member c public))) (list c)))))
         (/ (loop for addedcard1 in universe summing
               (loop for addedcard2 in (rest (member addedcard1 universe)) summing
                     (let ((fullpublic (append (list addedcard1 addedcard2) (copy-list public))))
                       (if (hand> (best-texas-holdem-hand2 fullpublic (copy-list private))
                                  (best-texas-holdem-hand2 fullpublic (copy-list opponent)))
                           1
                           0))))
            990.0)))
      ;when no public cards are specified, we assume we're preflop and to speed calculations up we only determine statistics for flop (i.e. best hands in 5 cards, not 7)
    (0 (let ((universe (loop for c from 0 to 51 nconcing (when (and (not (member c private)) (not (member c opponent))) (list c)))))
         (/ (loop for addedcard1 in universe summing
                  (loop for addedcard2 in (rest (member addedcard1 universe)) summing
                        (loop for addedcard3 in (rest (member addedcard2 universe)) summing
                              (let ((mycardset (cons addedcard1 (cons addedcard2 (cons addedcard3 (copy-list private)))))
                                    (oppcardset (cons addedcard1 (cons addedcard2 (cons addedcard3 (copy-list opponent))))))
                                (if (hand> (sort mycardset #'< :key #'ranknum)
                                           (sort oppcardset #'< :key #'ranknum))
                                    1
                                    0)))))
            17296.0)))
    (t (error "CHANCES-OF-BEATING-OPPONENT called with PUBLIC [~A] list of cards whose length is not 0, 3, 4, or 5." public))))


(defun chances-of-beating-opponent2 (public private opponent)
  "Returns the percentage of all possible hands in which the PRIVATE hand beats the OPPONENT's private hand in heads-up play, 
given the common cards in PUBLIC.  Note that all lists must be numeric card codes, not symbols. Note also that
when PUBLIC is empty, to speed up calculations the function only calculates percentages for beating the opponent
over the flop (not all possible 5-card common card sets, only a 3-card common card set)."
  (when (or (not (listp private)) (not (= 2 (length private))))
    (error "CHANCES-OF-BEATING-OPPONENT called with PRIVATE card list [~A] whose length is not exactly 2." private))
  (when (or (not (listp private)) (not (= 2 (length opponent))))
      (error "CHANCES-OF-BEATING-OPPONENT called with OPPONENT card list [~A] whose length is not exactly 2." opponent))
  (case (length public)
    (5 (if (hand2> (best-texas-holdem-hand2 (copy-list public) (copy-list private))
                   (best-texas-holdem-hand2 (copy-list public) (copy-list opponent)))
           1.0
           0.0))
    (4 (let ((universe (loop for c from 0 to 51 nconcing (when (and (not (member c private)) (not (member c opponent)) (not (member c public))) (list c)))))
         (/ (loop for addedcard in universe summing
               (let ((fullpublic (append (list addedcard) (copy-list public))))
                 (if (hand2> (best-texas-holdem-hand2 fullpublic (copy-list private))
                             (best-texas-holdem-hand2 fullpublic (copy-list opponent)))
                     1
                     0)))
            44.0)))
    (3 (let* ((universe (loop for c from 0 to 51 nconcing (when (and (not (member c private)) (not (member c opponent)) (not (member c public))) (list c)))))
         (/ (loop for addedcard1 in universe summing
               (loop for addedcard2 in (rest (member addedcard1 universe)) summing
                     (let ((fullpublic (append (list addedcard1 addedcard2) (copy-list public))))
                       (if (hand2> (best-texas-holdem-hand2 fullpublic (copy-list private))
                                   (best-texas-holdem-hand2 fullpublic (copy-list opponent)))
                           1
                           0))))
            990.0)))
      ;when no public cards are specified, we assume we're preflop and to speed calculations up we only determine statistics for flop (i.e. best hands in 5 cards, not 7)
    (0 (let ((universe (loop for c from 0 to 51 nconcing (when (and (not (member c private)) (not (member c opponent))) (list c)))))
         (/ (loop for addedcard1 in universe summing
                  (loop for addedcard2 in (rest (member addedcard1 universe)) summing
                        (loop for addedcard3 in (rest (member addedcard2 universe)) summing
                              (let ((mycardset (cons addedcard1 (cons addedcard2 (cons addedcard3 (copy-list private)))))
                                    (oppcardset (cons addedcard1 (cons addedcard2 (cons addedcard3 (copy-list opponent))))))
                                (if (hand2> (sort mycardset #'< :key #'ranknum)
                                            (sort oppcardset #'< :key #'ranknum))
                                    1
                                    0)))))
            17296.0)))
    (t (error "CHANCES-OF-BEATING-OPPONENT called with PUBLIC [~A] list of cards whose length is not 0, 3, 4, or 5." public))))



(defun betterhand (h1 h2)
  "Returns higher-ranked hand of H1 and H2 (H1 if hands are same)."
  (let* ((rating1 (hand-ranking h1))
         (rating2 (hand-ranking h2))
         (betterone h1))
    (loop for i in rating1
          for j in rating2 doing
          (cond ((> i j)
                 (setf betterone h1)
                 (return))
                ((< i j)
                 (setf betterone h2)
                 (return))))
    betterone))


(defun hand> (h1 h2)
  "Returns t iff H1 is a higher ranked hand than H2."
  (let ((result (pick-winner h1 h2)))
    (equal :h1 result)))

(defun hand2> (h1 h2)
  "Returns t iff H1 is a higher ranked hand than H2."
  (let ((result (pick-winner2 h1 h2)))
    (equal :h1 result)))


(defun hand>= (h1 h2)
  "Returns t iff H1 is a higher ranked hand than H2 or equally ranked to H2."
  (let ((result (pick-winner h1 h2)))
    (or (equal :h1 result)
        (equal :tie result))))
           

(defun pick-winner (h1 h2)
  "Returns :H1 or :H2 or :TIE to indicate which hand is stronger.  This is different from
the BETTERHAND function in that it does not return the actual better hand, just the winner indicator."
  (let* ((r1 (hand-ranking h1))
         (r2 (hand-ranking h2))
         (betterone :TIE))
    (loop for i in r1
          for j in r2 doing
          (cond ((> i j)
                 (setf betterone :H1)
                 (return))
                ((< i j)
                 (setf betterone :H2)
                 (return))))
    betterone))

(defun pick-winner2 (h1 h2)
  "Returns :H1 or :H2 or :TIE to indicate which hand is stronger.  This is different from
the BETTERHAND function in that it does not return the actual better hand, just the winner indicator."
  (let* ((r1 (hand-ranking2 h1))
         (r2 (hand-ranking2 h2))
         (betterone :TIE))
    (loop for i in r1
          for j in r2 doing
          (cond ((> i j)
                 (setf betterone :H1)
                 (return))
                ((< i j)
                 (setf betterone :H2)
                 (return))))
    betterone))

(defun handtype (hand)
  (nth (first (hand-ranking hand)) 
       '(:HIGHCARD :PAIR :TWOPAIR :TRIPS :STRAIGHT :FLUSH :FULLHOUSE :FOURKIND :STRAIGHTFLUSH)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;STRUCTURE AND SUPPORT CODE FOR TRANSMITTING AND ACCESSING 
;;;;INFORMATION IN A HOLDEM ROUND STATE

(defstruct (holdemround
            (:print-function print-holdemround)
            (:copier nil))
  (players (LIST 0)) ;; list of active players in game.
  (commoncards nil)
  (playercards nil) ;; an array of N+1 pairs of cards, only visible at end of game, except for current player
  (actions nil) ;; a list of actions taken in the game so far
  (pots (list nil 0)) ;; the amount of money in the pots
                 ;; a pot amount is preceded by a list of the players eligible for it,
  (internalpot nil)
  (playerbanks nil) ;; an array of N+1 bankrolls; players can see each others' bankrolls
  (dealerpos 0) ;; a number from 0 to N-1 indicating which position in PLAYERS is holding the "dealer button."
  (blind nil) ;; a number indicating the "big blind."
  (bet 0) ;; a number indicating the current bet that must be called
  (turnpos 0) ;; position of the player in PLAYERS who next needs to make a move 
  (roundcount 0) ;; a number indicating the current round being played in the current game. In a completed game, this will show how many rounds had been played.
)


;; The HOLDEMROUND-ACTIONS list can contain lists of the following formats:
;; (<ID> :fold)        --> Player <ID> folds
;; (<ID> :check)       --> Player <ID> checks
;; (<ID> :raise <AMT>) --> Player <ID> adds <AMT> to current bet level, so he's really
;;                         adding <REQUIREDBET>+<AMT> to the pot
;; (<ID> :allin <AMT>) --> Player <ID> bets entire bankroll; AMT is calculated by gamedirector as
;;                         (<ID> :raise (- <WHOLEBANK> <REQUIREDBET>))
;; (<ID> :call <AMT>)  --> Player <ID> adds his current required bet <AMT> to pot.
;; (0 :setup)          --> The "first" action in a round
;; (0 :flop)           --> Game director deals flop card
;; (0 :turn)           --> Game director deals turn card
;; (0 :river)          --> Game director deals river card
;; (0 :cleanup)        --> Game director is showing final game configuration to all non-busted players
;;
;; Note that Game Director will automatically verify and update player bankrolls!
;; Note that Game Director will automatically add big and small blind actions to start of round actions
;; The first action in the action list is always the most recent
;; Common cards are listed in order --> 1st three are flop, 4th is turn, 5th is river.



(defun print-holdemround (obj str depth)
  "Displays a HOLDEMGAME structure."
  (declare (ignore depth))
  (format str "<HOLDEMROUND players:~A deal:~D pot:~F commoncards:~A playercards:~A turn:~D>"
          (holdemround-players obj) (holdemround-dealerpos obj) (holdemround-pots obj)
          (hand-to-sym (holdemround-commoncards obj))
          (loop for i from 1 to (1- (length (holdemround-playercards obj))) collecting
                (hand-to-sym (aref (holdemround-playercards obj) i)))
          (holdemround-turnpos obj)))

(defun display-holdemround (round seating stream)
  (case (length seating)
    (4 (display-holdemround-4players round seating stream))
    (5 (display-holdemround-5players round seating stream))))


(defun display-holdemround-4players (round seating stream)
  "Displays a holdem round's info in a very pretty layout."
  (format stream "~%----------------------------------------------------------------------~%                             ~16A~%                             POT:~A~%                             BLIND:~D~%                             BET:~A~%~%"
          (hand-to-sym (holdemround-commoncards round))
          (holdemround-pot round)
          (holdemround-blind round)
          (holdemround-bet round))
  (case (holdemround-dealerpos round)
    (0 (format stream "    DEALER~%"))
    (1 (format stream "                    DEALER~%"))
    (2 (format stream "                                    DEALER~%"))
    (3 (format stream "                                                    DEALER~%")))
  (format stream "    ~12A    ~12A    ~12A    ~12A~%    BANK:~8A   BANK:~8A   BANK:~8A   BANK:~8A~%"
          (if (not (player-foldedp 1 round))
              (hand-to-sym (aref (holdemround-playercards round) 1))
              "_______")
          (if (not (player-foldedp 2 round))
              (hand-to-sym (aref (holdemround-playercards round) 2))
              "_______")
          (if (not (player-foldedp 3 round))
              (hand-to-sym (aref (holdemround-playercards round) 3))
              "_______")
          (if (not (player-foldedp 4 round))
              (hand-to-sym (aref (holdemround-playercards round) 4))
              "_______")
          (format nil "~7,1F" (aref (holdemround-playerbanks round) 1))
          (format nil "~7,1F" (aref (holdemround-playerbanks round) 2))
          (format nil "~7,1F" (aref (holdemround-playerbanks round) 3))
          (format nil "~7,1F" (aref (holdemround-playerbanks round) 4)))
  (apply #'(lambda (a b c d)
             (format t "    ~12A    ~12A    ~12A    ~12A~%" a b c d))
         (loop for agent in seating collecting
               (subseq (holdemagent-namestring agent) 0 (min 11 (length (holdemagent-namestring agent)))))) 
  (case (holdemround-turnpos round)
    (0 (format stream "    ^TURN^~%"))
    (1 (format stream "                    ^TURN^~%"))
    (2 (format stream "                                    ^TURN^~%"))
    (3 (format stream "                                                    ^TURN^~%")))
  (format stream "----------------------------------------------------------------------~%")
  nil)


(defun display-holdemround-5players (round seating stream)
  "Displays a holdem round's info in a very pretty layout."
  (format stream "~%-----------------------------------------------------------------------------------~%                                    ~16A~%                                    POT:~A~%                                    BLIND:~D~%                                    BET:~A~%~%"
          (hand-to-sym (holdemround-commoncards round))
          (holdemround-pot round)
          (holdemround-blind round)
          (holdemround-bet round))
  (case (holdemround-dealerpos round)
    (0 (format stream "    DEALER~%"))
    (1 (format stream "                    DEALER~%"))
    (2 (format stream "                                    DEALER~%"))
    (3 (format stream "                                                    DEALER~%"))
    (4 (format stream "                                                                   DEALER~%")))
  (format stream "    ~12A    ~12A    ~12A    ~12A    ~12A~%    BANK:~8A   BANK:~8A   BANK:~8A   BANK:~8A   BANK:~8A~%"
          (if (not (player-foldedp 1 round))
              (hand-to-sym (aref (holdemround-playercards round) 1))
              "_______")
          (if (not (player-foldedp 2 round))
              (hand-to-sym (aref (holdemround-playercards round) 2))
              "_______")
          (if (not (player-foldedp 3 round))
              (hand-to-sym (aref (holdemround-playercards round) 3))
              "_______")
          (if (not (player-foldedp 4 round))
              (hand-to-sym (aref (holdemround-playercards round) 4))
              "_______")
          (if (not (player-foldedp 5 round))
              (hand-to-sym (aref (holdemround-playercards round) 5))
              "_______")
          (format nil "~7,1F" (aref (holdemround-playerbanks round) 1))
          (format nil "~7,1F" (aref (holdemround-playerbanks round) 2))
          (format nil "~7,1F" (aref (holdemround-playerbanks round) 3))
          (format nil "~7,1F" (aref (holdemround-playerbanks round) 4))
          (format nil "~7,1F" (aref (holdemround-playerbanks round) 5)))
  (apply #'(lambda (a b c d e)
             (format t "    ~12A    ~12A    ~12A    ~12A    ~12A~%" a b c d e))
         (loop for agent in seating collecting
               (subseq (holdemagent-namestring agent) 0 (min 11 (length (holdemagent-namestring agent)))))) 
  (case (holdemround-turnpos round)
    (0 (format stream "    ^TURN^~%"))
    (1 (format stream "                    ^TURN^~%"))
    (2 (format stream "                                    ^TURN^~%"))
    (3 (format stream "                                                    ^TURN^~%"))
    (4 (format stream "                                                                    ^TURN^~%")))
  (format stream "------------------------------------------------------------------------------------~%")
  nil)



(defun create-holdemround (numplayers &optional (bank 10000) (blind 50) (dealerpos 0))
  (let ((g (make-holdemround :players (loop for i from 1 to numplayers collecting i))))
    (setf (holdemround-playercards g) (make-array (1+ numplayers) :initial-element nil))
    (setf (holdemround-playerbanks g) (make-array (1+ numplayers) :initial-element bank))
    (setf (holdemround-internalpot g) (make-holdempot :members (loop for i from 1 to numplayers collecting i)
                                                      :value 0
                                                      :required-commitment 0
                                                      :commitments (loop for i from 1 to numplayers collecting (cons i 0))))
    (setf (holdemround-dealerpos g) dealerpos)
    (setf (holdemround-blind g) blind)
    (setf (holdemround-turnpos g) (mod (+ dealerpos 2) numplayers))
    g))


(defun copy-holdemround (round &key (pointofview nil) (reveal-unfolded nil))
  "If POINTOFVIEW is a number, then make a copy of the round which only includes the info from
that playernumber's card information.  If REVEAL-UNFOLDED is T, then make a copy of
the round that includes private cards of all players who have not folded."
  (let* ((seatcount (length (holdemround-playerbanks round)))
         (banks (holdemround-playerbanks round))
         (playercards (holdemround-playercards round)))
    (make-holdemround
     :players (copy-list (holdemround-players round))
     :playerbanks (make-array seatcount
                              :initial-contents
                              (loop for i from 0 to (1- seatcount) collecting (aref banks i)))
     :commoncards (copy-list (holdemround-commoncards round))
     :internalpot (copy-holdempot (holdemround-internalpot round))
     :playercards (make-array seatcount
                              :initial-contents
                              (loop for i from 0 to (1- seatcount) collecting
                                    (cond ((and pointofview reveal-unfolded)
                                           (cond ((= i pointofview)
                                                  (copy-list (aref playercards i)))
                                                 ((not (player-foldedp i round))
                                                  (copy-list (aref playercards i)))
                                                 (t nil)))
                                          (pointofview
                                           (when (= i pointofview)
                                             (copy-list (aref playercards i))))
                                          (reveal-unfolded
                                           (when (not (player-foldedp i round))
                                             (copy-list (aref playercards i))))
                                          (t (copy-list (aref playercards i))))))
     :pots (loop for p in (holdemround-pots round) collecting
                 (list (copy-list (first p)) (second p)))
     :actions (mapcar #'copy-list (holdemround-actions round)) 
     :blind (holdemround-blind round)
     :turnpos (holdemround-turnpos round)
     :dealerpos (holdemround-dealerpos round)
     :bet (holdemround-bet round))))



;;anything that starts with a 0 is a dealer action that
;;indicates how far back a raise session can be counted.
(defun number-of-raises (round)
  "Returns the number of raises since the most recently dealt card."
  (let ((cnt 0))
    (loop for event in (holdemround-actions round) doing
          (cond ((equal 0 (first event))
                 (return))
                ((equal :raise (second event))
                 (incf cnt))
                ((and (equal :allin (second event))
                      (plusp (third event))) ;;allins with negatives are really not raises, just attempts at meeting bets
                 (incf cnt))))
    cnt))


(defun player-bustedp (playerid round)
  "Returns T iff the player is out of the game because their money is all gone."
  (and (not (player-allinp playerid round))
       (zerop (aref (holdemround-playerbanks round) playerid))))

(defun player-allinp (playerid round)
  "Returns T iff the player agent with ID = PLAYERID has gone allin in ROUND."
  (find-if #'(lambda (x) (and (= (first x) playerid) (eq (second x) :allin))) (holdemround-actions round)))

(defun player-foldedp (playerid round)
  "Returns T iff the player agent with ID = PLAYERID has already folded in ROUND."
  (find-if #'(lambda (x) (and (= (first x) playerid) (eq (second x) :fold))) (holdemround-actions round)))


(defun required-player-bet (playerid round)
  "Returns the minimum bet that the agent with PLAYERID must make in ROUND."
  (let ((p (holdemround-internalpot round)))
    (if (member playerid (holdempot-members p))
        (- (holdempot-required-commitment p) (cdr (assoc playerid (holdempot-commitments p))))
        0)))


(defun more-than-one-player-in-play (round)
  "Returns T iff two or more players are still contesting a pot."
  (let ((participants (holdempot-members (holdemround-internalpot round)))) 
    (> (length participants) 1)))

(defun more-than-one-actionable-player-in-play (round)
  "Returns T iff more than one player can still place bets."
  (> (actionable-players round) 1))


(defun number-of-folded-or-busted-players (round)
  (loop for playerid in (holdemround-players round) summing
        (if (or (player-bustedp playerid round)
                (player-foldedp playerid round))
            1
            0)))

(defun actionable-players (round)
  "Counts the number of players who can still place bets"
  (loop for playerid in (holdemround-players round) summing
        (if (and (plusp (aref (holdemround-playerbanks round) playerid))
                 (not (player-foldedp playerid round)))
            1
            0)))

(defun number-of-players-with-money (round)
  (loop for playerid in (holdemround-players round) summing
        (if (plusp (aref (holdemround-playerbanks round) playerid)) 1 0)))

(defun player-ids-with-money (round)
  (loop for id in (holdemround-players round) nconcing
        (when (plusp (aref (holdemround-playerbanks round) id))
          (list id))))



(defun fold-player-out (playerid round)
  "Remove player agent with PLAYERID from pots in ROUND because they have folded."
  (setf (holdempot-members (holdemround-internalpot round))
        (remove playerid (holdempot-members (holdemround-internalpot round))))
  (setf (holdempot-commitments (holdemround-internalpot round))
        (remove-if #'(lambda (x) (= playerid (first x))) (holdempot-commitments (holdemround-internalpot round))))
  (setf (holdemround-pots round) (generate-winner-check-pots (holdemround-internalpot round)))
  round)




(defun reset-holdemround (round)
  "Resets the ROUND's state information to reflect the beginning of a new round,
 with a new dealer, but everyone's bankroll information is kept the same, and
no one has cards dealt yet."
  (setf (holdemround-pots round) (list (list (copy-list (holdemround-players round)) 0)))
  (setf (holdemround-bet round) 0)
  (setf (holdemround-actions round) nil)
  (loop for p from 0 to (1- (length (holdemround-playercards round))) doing
        (setf (aref (holdemround-playercards round) p) nil))
  (setf (holdemround-commoncards round) nil)
  (setf (holdemround-dealerpos round) (mod (+ 1 (holdemround-dealerpos round)) (length (holdemround-players round))))
  ;;advance dealer position to the next player who has money..e.g. hasn't busted.
  (while (not (plusp (aref (holdemround-playerbanks round)
                           (nth (holdemround-dealerpos round) (holdemround-players round)))))
         (setf (holdemround-dealerpos round) (mod (+ 1 (holdemround-dealerpos round)) (length (holdemround-players round)))))
  round)

                                               





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;SIMPLE STRUCTURE FOR USERS TO PASS AGENT FUNCTIONS INTO
;;;;;HOLDEM GAME DRIVER.
(defstruct holdemagent 
  (namestring "TEX")
  (ID 0)
  (agentfunction #'(lambda (roundstate id) (declare (ignore roundstate id)) (LIST :fold))))


;;;;HOW TO PROGRAM A HOLDEM AGENT
;;;;
;;;; Every agent is represented as a HOLDEMAGENT structure.  
;;;; The agentfunction slot holds the function that defines 
;;;; how the agent will choose what action to take based on
;;;; the state of the card game.  Any agent function must be
;;;; defined to take 2 arguments.  The first is a data structure
;;;; that defines the current state of the game (pots, money
;;;; in players' banks, the cards in the player's hand, etc).
;;;; the second argument is a number indicating the position
;;;; in the player array (e.g. seats around the table) of the
;;;; player in order of play.
;;;;
;;;; If you're not sure how to figure out what to bet, your
;;;; code can always take a look at what is stored in the "bet"
;;;; slot on the holdemround structure passed in as the first
;;;; argument.  That is, you are guaranteed that the game engine
;;;; will correctly set the value accessed by (holdemround-bet ....)
;;;; to be the bet you must make without any raises if you just
;;;; want to stay in the current hand.

;;;;holdem agents can return one of the following:
;;;; (LIST :fold)
;;;; (LIST :check)
;;;; (LIST :allin)
;;;; (LIST :call)
;;;; (LIST :raise <AMT>)  --> adds <AMT> to current bet
;;
;;Illegal actions will be treated as FOLDS by the game engine.
;;A player MUST respond "ALLIN" if it wants to stay in but doesn't have enough to call, otherwise, a
;;call that exhausts the player's bank but doesn't meet the current bet will be considered an illegal
;;action and will be equivalent to a fold.
;;


;;;;EXAMPLE HOLDEM AGENTS
(defparameter *folder-agent*
  (make-holdemagent 
   :namestring "FOLDER" ;;ALWAYS FOLDS
   :ID 101 ;; change to some number in [1..4] if you're using this in a game
   :agentfunction #'(lambda (roundstate id) (declare (ignore roundstate id)) (LIST :fold))))


(defparameter *aggressive-caller-agent*
 (make-holdemagent 
   :namestring "CALLER" ;; CALLS UNTIL AN ALLIN IS NEEDED, THEN GOES ALLIN
   :ID 102 ;; change to some number in [1..4] if you're using this in a game
   :agentfunction #'(lambda (roundstate id)
                  (cond ((zerop (holdemround-bet roundstate))
                         (LIST :check)) ;; essentially, a call, but we agreed in class
                                        ;; when required bet is zero, we must check.
                        ((< (holdemround-bet roundstate)
                            (aref (holdemround-playerbanks roundstate) id))
                         (LIST :call))
                        ((LIST :allin))))))

(defparameter *less-aggressive-caller-agent*
 (make-holdemagent 
   :namestring "WeakCALLER" ;;CALLS UNTIL AN ALLIN IS NEEDED, THEN FOLDS
   :ID 103 ;; change to some number in [1..4] if you're using this in a game
   :agentfunction #'(lambda (roundstate id)
                  (cond ((zerop (holdemround-bet roundstate))
                         (LIST :check))
                        ((< (holdemround-bet roundstate)
                            (aref (holdemround-playerbanks roundstate) id))
                         (LIST :call))
                        ((LIST :fold))))))


(defparameter *raiser-agent*
 (make-holdemagent 
   :namestring "BIGRAISER" ;;RAISES UNTIL RAISELIMIT IS REACHED, then just calls or goes allin (or folds if there's a problem)
   :ID 104 ;; change to some number in [1..4] if you're using this in a game
   :agentfunction #'(lambda (roundstate id)
                  (let ((mybank (aref (holdemround-playerbanks roundstate) id)))
                    (cond ((zerop (holdemround-bet roundstate))
                           (cond ((and (< (number-of-raises roundstate) 3)
                                       (> mybank (holdemround-blind roundstate)))
                                  (LIST :raise (holdemround-blind roundstate)))
                                 ((and (< (number-of-raises roundstate) 3)
                                       (<= mybank (holdemround-blind roundstate)))
                                  (LIST :allin))
                                 ((and (= (number-of-raises roundstate) 3)
                                       (> mybank (holdemround-bet roundstate)))
                                  (LIST :call))
                                 ((and (= (number-of-raises roundstate) 3)
                                       (< mybank (holdemround-bet roundstate)))
                                  (LIST :allin))
                                 (t (LIST :fold))))
                          ((> mybank (+ (holdemround-blind roundstate) 
                                        (holdemround-bet roundstate)))
                           (LIST :raise (holdemround-blind roundstate)))
                          (t ;; can't cover the bet or the blind
                           (LIST :allin)))))))

(defparameter *tex-raiser-agent*
 (make-holdemagent 
   :namestring "BigRaiseTEX" ;;RAISES UNTIL RAISELIMIT IS REACHED, then just calls or goes allin (or folds if there's a problem)
   :ID 105 ;; change to some number in [1..4] if you're using this in a game
   :agentfunction #'(lambda (roundstate id)
                  (let ((mybank (aref (holdemround-playerbanks roundstate) id)))
                    (cond ((zerop (holdemround-bet roundstate))
                           (cond ((and (< (number-of-raises roundstate) 3)
                                       (> mybank (holdemround-blind roundstate)))
                                  (LIST :raise (holdemround-blind roundstate)))
                                 ((and (< (number-of-raises roundstate) 3)
                                       (<= mybank (holdemround-blind roundstate)))
                                  (LIST :allin))
                                 ((and (= (number-of-raises roundstate) 3)
                                       (> mybank (holdemround-bet roundstate)))
                                  (LIST :call))
                                 ((and (= (number-of-raises roundstate) 3)
                                       (< mybank (holdemround-bet roundstate)))
                                  (LIST :allin))
                                 (t (LIST :fold))))
                          ((> mybank (+ (holdemround-blind roundstate) 
                                        (holdemround-bet roundstate)))
                           (LIST :raise (holdemround-blind roundstate)))
                          (t ;; can't cover the bet or the blind
                           (LIST :allin)))))))

(defparameter *TEST-PLAYERS*
  (list *aggressive-caller-agent* *less-aggressive-caller-agent* *folder-agent* *raiser-agent*))

(defparameter *TEST-PLAYERS-FIVE*
  (list *aggressive-caller-agent* *less-aggressive-caller-agent* *folder-agent* *raiser-agent* *tex-raiser-agent*))


;;(defparameter *CSC4500-AGENTS* (list *teambigmoney* *team404* *jose* *straightshooter*))
;;(defparameter *POKER-AGENTS*
;;  (list *BAMF-AGENT* *VALL-AGENT* *DV-AGENT1* *OUR-AGENT*))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;TEXAS HOLDEM GAME DRIVER CODE
;;;;

(defun place-bet (playerid round &optional (bettype :call) (amount 0))
  "Adjusts all pots and player's bank based on PLAYERIS's choice 
of BETTYPE. NO checking for legitimate pot or bet values is done.
In the case of raises, this assumes that PLAYERID is eligible for the
current pot."
  (let* ((pot (holdemround-internalpot round)))
    (case bettype
      (:call (let ((unmet (unmet-player-commitment playerid pot)))
               (push (LIST playerid :call unmet) (holdemround-actions round))
               (incf (holdempot-value pot) unmet)
               (decf (aref (holdemround-playerbanks round) playerid) unmet)
               (setf (cdr (assoc playerid (holdempot-commitments pot)))
                           (holdempot-required-commitment pot)))) ;; player currently fully committed to pot
      (:raise (let ((unmet (unmet-player-commitment playerid pot)))
                (push (LIST playerid :raise amount) (holdemround-actions round))
                (incf (holdempot-required-commitment pot) amount)
                (incf (holdempot-value pot) (+ unmet amount))
                (incf (cdr (assoc playerid (holdempot-commitments pot))) (+ unmet amount))
                (decf (aref (holdemround-playerbanks round) playerid) (+ unmet amount))))
      (:allin (let ((bank-value (aref (holdemround-playerbanks round) playerid))
                    (required-bet (required-player-bet playerid round)))
                (cond ((>= bank-value required-bet)
                       (push (list playerid :allin (- bank-value required-bet)) (holdemround-actions round))
                       (incf (holdempot-value pot) bank-value)
                       (incf (holdempot-required-commitment pot) (- bank-value required-bet)) ;; "raise"
                       (incf (cdr (assoc playerid (holdempot-commitments pot))) bank-value)
                       (setf (aref (holdemround-playerbanks round) playerid) 0))
                      (t ;;desperation allin, no "raise"
                         (push (list playerid :allin (- bank-value required-bet)) (holdemround-actions round))
                         (incf (holdempot-value pot) bank-value)
                         (incf (cdr (assoc playerid (holdempot-commitments pot))) bank-value)
                         (setf (aref (holdemround-playerbanks round) playerid) 0)))))) ;;player as committed as can be
    (setf (holdemround-pots round) (generate-winner-check-pots pot)))
    round)



(defun agent-response (gamestate agent-function playerid &optional (suppress-agent-software-errors nil))
  (if suppress-agent-software-errors
      (let ((result-list (multiple-value-list (ignore-errors (funcall agent-function gamestate playerid)))))
        (if (and (null (first result-list))
                 (typep (second result-list) 'condition)
                 (null (nthcdr 2 result-list)))
            (let ((c (second result-list)))
              (typecase c
                (conditions:division-by-zero (format t "~%WARNING: Suppressed ~A" (conditions::division-by-zero-printer c nil)))
                (conditions:unbound-variable (format t "~%WARNING: Suppressed Error: The variable ~A is unbound." (conditions::cell-error-name c)))
                (conditions:arithmetic-type-error (format t "~%WARNING: Suppressed ~A" (conditions::arithmetic-type-error-printer c nil)))
                (conditions:undefined-function (format t "~%WARNING: Suppressed ~A" (conditions::undefined-function-printer c nil)))
                (otherwise (format t "%WARNING: Suppressed Condition ~A" c)))
              (format t "~%WARNING: This ERROR CONDITION was coerced to a :FOLD.~%")
              (list :fold))
          (first result-list)))
      (funcall agent-function gamestate playerid)))



(defun preflop-stage (baseround seating deck &optional (verbose nil) (pauser nil) (suppress-agent-software-errors nil))
  (let* ((stagedone nil)
         (players (holdemround-players baseround))
         (numplayers (length players))
         (banks (holdemround-playerbanks baseround))
         (dealpos (holdemround-dealerpos baseround))
         (cardpos 0) (raisecount 0)
         (turnpos dealpos) 
         (stageend-pos nil) ;;position after player who could end the stage if they don't raise.
         (stageend-id nil) ;;player after the one who could end the stage if they don't raise.
         (playerid 0))
    (declare (ignore stageend-id))
    (when (more-than-one-player-in-play baseround)
      (when verbose
        (format t "~%~A IS NOW DEALER." 
                (holdemagent-namestring (find-if #'(lambda (x) (= (holdemagent-id x) (nth dealpos players))) seating))))
 
      ;;set up and deduct blinds, handling allin's and limited pots appropriately
      (setf (holdemround-actions baseround) (LIST (LIST 0 :setup)))

      ;;set up small blind
      (setf turnpos (mod (+ 1 turnpos) numplayers))
      (setf playerid (nth turnpos players))
      (while (not (plusp (aref (holdemround-playerbanks baseround) playerid)))
             (setf turnpos (mod (+ 1 turnpos) numplayers))
             (setf playerid (nth turnpos players)))
      (let ((requiredbet (/ (holdemround-blind baseround) 2)))
        (setf (holdempot-required-commitment (holdemround-internalpot baseround)) requiredbet) ;;smallblind required
        (cond ((< requiredbet (aref banks playerid))
               (when verbose
                 (format t "~%~A HAS SMALLBLIND BET ACTION=(CALL ~A) [REQUIRED BET IS NOW ~A]~%" 
                         (holdemagent-namestring (find-if #'(lambda (x) (equal (holdemagent-id x) playerid)) seating))
                         requiredbet
                         (* requiredbet 2)))
               (place-bet playerid baseround :call))
              (t ;;not enough money to cover blind, force allin
                 (when verbose
                   (format t "~%~A HAS SMALLBLIND BET ACTION=(ALLIN ~A) [REQUIRED BET IS NOW ~A)]~%"
                           (holdemagent-namestring (find-if #'(lambda (x) (equal (holdemagent-id x) playerid)) seating))
                           (aref banks playerid)
                           (* 2 requiredbet)))
                 (place-bet playerid baseround :allin)))
        (setf (holdempot-required-commitment (holdemround-internalpot baseround)) (* 2 requiredbet)))
      ;; the above setf establishes the blind as a required pot contribution for all remaining players AND the small blind

      ;;setup big blind
      (setf turnpos (mod (+ 1 turnpos) numplayers))
      (setf playerid (nth turnpos players))
      (while (not (plusp (aref (holdemround-playerbanks baseround) playerid)))
             (setf turnpos (mod (+ 1 turnpos) numplayers))
             (setf playerid (nth turnpos players)))
      (let ((requiredbet (required-player-bet playerid baseround)))
        (cond ((< requiredbet (aref banks playerid))
               (place-bet playerid baseround :call)
               (when verbose
                 (format t "~A HAS BIGBLIND BET ACTION=(CALL ~A)~%" 
                         (holdemagent-namestring (find-if #'(lambda (x) (equal (holdemagent-id x) playerid)) seating))
                         requiredbet)))
              (t ;;not enough money to cover blind, force allin
                 (when verbose
                   (format t "~A HAS BIGBLIND BET ACTION=(ALLIN ~A)~%"
                           (holdemagent-namestring (find-if #'(lambda (x) (equal (holdemagent-id x) playerid)) seating))
                           (aref banks playerid)))
                 (place-bet playerid baseround :allin))))

      ;;deal cards to players either in pot or with...go around table twice
      (setf cardpos dealpos)
      (dotimes (i (* 2 numplayers))
        (let ((card (deal-card deck)))
          (setf cardpos (mod (+ 1 cardpos) numplayers))
          (setf playerid (nth cardpos players))
        (when (or (plusp (aref (holdemround-playerbanks baseround) playerid))
                  (member playerid (holdempot-members (holdemround-internalpot baseround))))
          (setf (aref (holdemround-playercards baseround) playerid)
                (cons card (aref (holdemround-playercards baseround) playerid))))))
      
      ;;collect bets
      (setf turnpos (mod (+ 1 turnpos) numplayers))
      (setf playerid (nth turnpos players))
      (setf stageend-pos turnpos
            stageend-id playerid
            stagedone (zerop (actionable-players baseround)))

      (when verbose (format t "YIELDING THE FOLLOWING START STATE:"))
      (when (and verbose stagedone)
        (display-holdemround baseround seating t)
        (format t "NO MORE ACTIONS POSSIBLE~%"))
      (setf (holdemround-bet baseround) (required-player-bet playerid baseround))
      (while (not stagedone)
        (when (and (plusp (aref banks playerid)) ;;when player has something to bet, and hasn't folded, ask what it wants to do
                   (not (player-foldedp playerid baseround)))
          (let* ((agentstruct (find-if #'(lambda (x) (equal (holdemagent-id x) playerid)) seating))
                 (response nil)
                 (gamestate))
            (setf (holdemround-turnpos baseround) turnpos)
            (setf (holdemround-bet baseround) (required-player-bet playerid baseround))
            (setf gamestate (copy-holdemround baseround :pointofview playerid))
            (when verbose
              (display-holdemround baseround seating t))
            (when pauser
              (format t "SEE PLAYER ACTION>")
              (read-line))
            (setf response (agent-response gamestate (holdemagent-agentfunction agentstruct) playerid suppress-agent-software-errors))
            (when verbose
              (format t "ACTION=~A" response))
            (case (first response)
              (:call (cond ((< (holdemround-bet gamestate) (aref banks playerid))
                            (place-bet playerid baseround :call))
                           (t
                            (cond ((and verbose (< (holdemround-bet gamestate) (aref banks playerid))) 
                                   (warnmsg :CALL-WITH-LOW-FUNDS))
                                  ((and verbose (= (holdemround-bet gamestate) (aref banks playerid)))
                                   (warnmsg :CALL-IN-PLACE-OF-ALLIN)))
                            (fold-player-out playerid baseround)
                            (push (list playerid :fold) (holdemround-actions baseround)))))
              (:allin (let* ((betvalue (aref banks playerid))
                             (raisevalue (- betvalue (holdemround-bet gamestate))))
                        (cond ((plusp raisevalue)
                               (place-bet playerid baseround :allin)
                               (setf stageend-pos turnpos stageend-id playerid))
                              ((not (plusp raisevalue))
                               (place-bet playerid baseround :allin))
                              (t
                               (when verbose (warnmsg :STRANGE-STATE))
                               (fold-player-out playerid baseround)
                               (push (list playerid :fold) (holdemround-actions baseround))))))
              (:raise (let* ((raisevalue (second response))
                             (totalbet (+ raisevalue (holdemround-bet gamestate))))
                        (cond ((and (< totalbet (aref banks playerid))
                                    (>= raisevalue (holdemround-blind gamestate))
                                    ;;(< raisecount 3)
                                    )
                               (place-bet playerid baseround :raise raisevalue)
                               (incf raisecount)
                               (setf stageend-pos turnpos stageend-id playerid)) ;;betting needs to go around back to this player
                              (t
                               (cond ;; ((and verbose (>= raisecount 3)) (warnmsg :RAISE-PASSES-RAISELIMIT)) ;; ignore for no-limit
                                     ((and verbose (> totalbet (aref banks playerid))) (warnmsg :RAISE-EXCEEDS-BANKROLL))
                                     ((and verbose (= totalbet (aref banks playerid))) (warnmsg :RAISE-IN-PLACE-OF-ALLIN))
                                     ((and verbose (< raisevalue (holdemround-blind gamestate))) (warnmsg :RAISE-MIN-IGNORED)))
                               (fold-player-out playerid baseround)
                               (push (list playerid :fold) (holdemround-actions baseround))))))
              (:check (cond ((= (holdemround-bet gamestate) 0)
                             (push (list playerid :check) (holdemround-actions baseround)))
                            (t
                             (fold-player-out playerid baseround)
                             (push (list playerid :fold) (holdemround-actions baseround)))))
              (:fold (fold-player-out playerid baseround)
                     (push (list playerid :fold) (holdemround-actions baseround)))
              (t (when verbose (warnmsg :UNRECOGNIZED-RESPONSE))
                 (fold-player-out playerid baseround)
                 (push (list playerid :fold) (holdemround-actions baseround))))))
        ;;advance to next seat at the table 
        (setf turnpos (mod (+ 1 turnpos) numplayers))
        (setf playerid (nth turnpos players))
        (setf (holdemround-turnpos baseround) turnpos)        
        (setf stagedone (or (= stageend-pos turnpos)
                            (zerop (actionable-players baseround))
                            (= (1- numplayers) (number-of-folded-or-busted-players baseround))))
        )))
    baseround)



(defun flop-stage (baseround seating deck &optional (verbose nil) (pauser nil))
  (let* ((numplayers (length (holdemround-players baseround)))
         (players (holdemround-players baseround))
         (dealpos (holdemround-dealerpos baseround))
         (turnpos  (mod (+ 1 dealpos) numplayers))
         (playerid nil)
         (banks (holdemround-playerbanks baseround))
         (stagedone (not (more-than-one-actionable-player-in-play baseround)))
         (raisecount 0)
         (stageend-id nil) (stageend-pos dealpos))
    (declare (ignore stageend-id))
    (when (more-than-one-player-in-play baseround)
      ;;deal the flop cards
      (dotimes (i 3)
        (setf (holdemround-commoncards baseround)
              (append (holdemround-commoncards baseround) (list (deal-card deck)))))
      (setf (holdemround-turnpos baseround) turnpos)
      (push (LIST 0 :flop) (holdemround-actions baseround))

      (setf playerid (nth turnpos players))
      (setf (holdemround-bet baseround) 0)
      (setf stageend-pos turnpos
            stageend-id playerid
            raisecount 0)
      (while (not stagedone)
         (when (and (plusp (aref banks playerid)) ;;when player has something to bet, and hasn't folded, ask what it wants to do
                    (not (player-foldedp playerid baseround)))
           (let* ((agentstruct (find-if #'(lambda (x) (equal (holdemagent-id x) playerid)) seating))
                  (response nil)
                  (gamestate nil))
             (setf (holdemround-turnpos baseround) turnpos)
             (setf (holdemround-bet baseround) (required-player-bet playerid baseround))
             (setf gamestate (copy-holdemround baseround :pointofview playerid))
             (when verbose
               (display-holdemround baseround seating t))
             (when pauser
               (format t "SEE PLAYER ACTION>")
               (read-line))
             (setf response (funcall (holdemagent-agentfunction agentstruct) gamestate playerid))            
             (when verbose
               (format t "ACTION=~A" response))
            (case (first response)
              (:call (cond ((< (holdemround-bet gamestate) (aref banks playerid))
                            (place-bet playerid baseround :call))
                           (t
                            (cond ((and verbose (< (holdemround-bet gamestate) (aref banks playerid))) 
                                   (warnmsg :CALL-WITH-LOW-FUNDS))
                                  ((and verbose (= (holdemround-bet gamestate) (aref banks playerid)))
                                   (warnmsg :CALL-IN-PLACE-OF-ALLIN)))
                            (fold-player-out playerid baseround)
                            (push (list playerid :fold) (holdemround-actions baseround)))))
              (:allin (let* ((betvalue (aref banks playerid))
                             (raisevalue (- betvalue (holdemround-bet gamestate))))
                        (cond ((and (plusp raisevalue)
                                   ;; (< raisecount 3)
                                    )
                               (place-bet playerid baseround :allin)
                               (incf raisecount)
                               (setf stageend-pos turnpos stageend-id playerid))
                              ((not (plusp raisevalue))
                               (place-bet playerid baseround :allin))
                              ;;(t
                              ;; (when (and verbose (>= raisecount 3)) (warnmsg :ALLIN-PASSES-RAISELIMIT))
                              ;; (fold-player-out playerid baseround)
                              ;; (push (list playerid :fold) (holdemround-actions baseround)))
                              )))
              (:raise (let* ((raisevalue (second response))
                             (totalbet (+ raisevalue (holdemround-bet gamestate))))
                        (cond ((and (< totalbet (aref banks playerid))
                                    (>= raisevalue (holdemround-blind gamestate))
                                    ;;(< raisecount 3)
                                    )
                               (place-bet playerid baseround :raise raisevalue)
                               (incf raisecount)
                               (setf stageend-pos turnpos stageend-id playerid)) ;;betting needs to go around back to this player
                              (t
                               (cond ;;((and verbose (>= raisecount 3)) (warnmsg :RAISE-PASSES-RAISELIMIT))
                                     ((and verbose (> totalbet (aref banks playerid))) (warnmsg :RAISE-EXCEEDS-BANKROLL))
                                     ((and verbose (= totalbet (aref banks playerid))) (warnmsg :RAISE-IN-PLACE-OF-ALLIN))
                                     ((and verbose (< raisevalue (holdemround-blind gamestate))) (warnmsg :RAISE-MIN-IGNORED)))
                               (fold-player-out playerid baseround)
                               (push (list playerid :fold) (holdemround-actions baseround))))))
              (:check (cond ((= (holdemround-bet gamestate) 0)
                             (push (list playerid :check) (holdemround-actions baseround)))
                            (t
                             (fold-player-out playerid baseround)
                             (push (list playerid :fold) (holdemround-actions baseround)))))
              (:fold (fold-player-out playerid baseround)
                     (push (list playerid :fold) (holdemround-actions baseround)))
              (t (when verbose (warnmsg :UNRECOGNIZED-RESPONSE))
                 (fold-player-out playerid baseround)
                 (push (list playerid :fold) (holdemround-actions baseround))))))
         ;;advance to next seat at the table 
         (setf turnpos (mod (+ 1 turnpos) numplayers))
         (setf playerid (nth turnpos players))
         (setf stagedone (or (= stageend-pos turnpos)
                             (zerop (actionable-players baseround))
                             (= (1- numplayers) (number-of-folded-or-busted-players baseround))))
         )))
  baseround)


(defun turn-stage (baseround seating deck &optional (verbose nil) (pauser nil))
  (let* ((numplayers (length (holdemround-players baseround)))
         (players (holdemround-players baseround))
         (dealpos (holdemround-dealerpos baseround))
         (turnpos  (mod (+ 1 dealpos) numplayers))
         (playerid nil)
         (banks (holdemround-playerbanks baseround))
         (stagedone (not (more-than-one-actionable-player-in-play baseround)))
         (raisecount 0)
         (stageend-id nil) (stageend-pos dealpos))
    (declare (ignore stageend-id))
  ;;deal the turn card
  (when (more-than-one-player-in-play baseround)
    (setf (holdemround-commoncards baseround)
          (append (holdemround-commoncards baseround) (list (deal-card deck))))
    (setf (holdemround-bet baseround) 0)
    (setf (holdemround-turnpos baseround) turnpos)
    (push (LIST 0 :turn) (holdemround-actions baseround))
    (setf playerid (nth turnpos players))
    (setf stageend-pos turnpos
          stageend-id playerid
          raisecount 0)
    (while (not stagedone)
       (when (and (plusp (aref banks playerid)) ;;if player has money, and hasn't folded, ask what it wants to do
                  (not (player-foldedp playerid baseround)))
         (let* ((agentstruct (find-if #'(lambda (x) (equal (holdemagent-id x) playerid)) seating))
                (response nil)
                (gamestate nil))
           (setf (holdemround-turnpos baseround) turnpos)
           (setf (holdemround-bet baseround) (required-player-bet playerid baseround))           
           (setf gamestate (copy-holdemround baseround :pointofview playerid))
           
           (when verbose
             (display-holdemround baseround seating t))
           (when pauser
             (format t "SEE PLAYER ACTION>")
             (read-line))
           (setf response (funcall (holdemagent-agentfunction agentstruct) gamestate playerid))
           (when verbose
             (format t "ACTION=~A" response))
            (case (first response)
              (:call (cond ((< (holdemround-bet gamestate) (aref banks playerid))
                            (place-bet playerid baseround :call))
                           (t
                            (cond ((and verbose (< (holdemround-bet gamestate) (aref banks playerid))) 
                                   (warnmsg :CALL-WITH-LOW-FUNDS))
                                  ((and verbose (= (holdemround-bet gamestate) (aref banks playerid)))
                                   (warnmsg :CALL-IN-PLACE-OF-ALLIN)))
                            (fold-player-out playerid baseround)
                            (push (list playerid :fold) (holdemround-actions baseround)))))
              (:allin (let* ((betvalue (aref banks playerid))
                             (raisevalue (- betvalue (holdemround-bet gamestate))))
                        (cond ((and (plusp raisevalue)
                                    ;;(< raisecount 3)
                                    )
                               (place-bet playerid baseround :allin)
                               (incf raisecount)
                               (setf stageend-pos turnpos stageend-id playerid))
                              ((not (plusp raisevalue))
                               (place-bet playerid baseround :allin))
                              (t
                               ;;(when (and verbose (>= raisecount 3)) (warnmsg :ALLIN-PASSES-RAISELIMIT))
                               (fold-player-out playerid baseround)
                               (push (list playerid :fold) (holdemround-actions baseround))))))
              (:raise (let* ((raisevalue (second response))
                             (totalbet (+ raisevalue (holdemround-bet gamestate))))
                        (cond ((and (< totalbet (aref banks playerid))
                                    (>= raisevalue (holdemround-blind gamestate))
                                    ;;(< raisecount 3)
                                    )
                               (place-bet playerid baseround :raise raisevalue)
                               (incf raisecount)
                               (setf stageend-pos turnpos stageend-id playerid)) ;;betting needs to go around back to this player
                              (t
                               (cond ;;((and verbose (>= raisecount 3)) (warnmsg :RAISE-PASSES-RAISELIMIT))
                                     ((and verbose (> totalbet (aref banks playerid))) (warnmsg :RAISE-EXCEEDS-BANKROLL))
                                     ((and verbose (= totalbet (aref banks playerid))) (warnmsg :RAISE-IN-PLACE-OF-ALLIN))
                                     ((and verbose (< raisevalue (holdemround-blind gamestate))) (warnmsg :RAISE-MIN-IGNORED)))
                               (fold-player-out playerid baseround)
                               (push (list playerid :fold) (holdemround-actions baseround))))))
              (:check (cond ((= (holdemround-bet gamestate) 0)
                             (push (list playerid :check) (holdemround-actions baseround)))
                            (t
                             (fold-player-out playerid baseround)
                             (push (list playerid :fold) (holdemround-actions baseround)))))
              (:fold (fold-player-out playerid baseround)
                     (push (list playerid :fold) (holdemround-actions baseround)))
              (t (when verbose (warnmsg :UNRECOGNIZED-RESPONSE))
                 (fold-player-out playerid baseround)
                 (push (list playerid :fold) (holdemround-actions baseround))))))

       ;;advance to next seat at the table 
       (setf turnpos (mod (+ 1 turnpos) numplayers))
       (setf playerid (nth turnpos players))
       (setf stagedone (or (= stageend-pos turnpos)
                           (zerop (actionable-players baseround))
                           (= (1- numplayers) (number-of-folded-or-busted-players baseround))))
       )))
  baseround)




(defun river-stage (baseround seating deck &optional (verbose nil) (pauser nil))
  (let* ((numplayers (length (holdemround-players baseround)))
         (players (holdemround-players baseround))
         (dealpos (holdemround-dealerpos baseround))
         (turnpos  (mod (+ 1 dealpos) numplayers))
         (playerid nil)
         (banks (holdemround-playerbanks baseround))
         (stagedone (not (more-than-one-actionable-player-in-play baseround)))
         (raisecount 0)
         (stageend-id nil) (stageend-pos dealpos))
    (declare (ignore stageend-id))
  ;;deal the river card
  (when (more-than-one-player-in-play baseround)
    (setf (holdemround-commoncards baseround)
          (append (holdemround-commoncards baseround) (list (deal-card deck))))
    (setf (holdemround-bet baseround) 0)
    (setf (holdemround-turnpos baseround) turnpos)
    (push (LIST 0 :river) (holdemround-actions baseround))
    (setf playerid (nth turnpos players))
    (setf stageend-pos turnpos
          stageend-id playerid
          raisecount 0)
    (while (not stagedone)
       (when (and (plusp (aref banks playerid)) ;;if player has money, and hasn't folded, ask what it wants to do
                  (not (player-foldedp playerid baseround)))
         (let* ((agentstruct (find-if #'(lambda (x) (equal (holdemagent-id x) playerid)) seating))
                (response nil)
                (gamestate nil))
           (setf (holdemround-bet baseround) (required-player-bet playerid baseround))
           (setf (holdemround-turnpos baseround) turnpos)
           (setf gamestate (copy-holdemround baseround :pointofview playerid))
           (when verbose
             (display-holdemround baseround seating t))
           (when pauser
             (format t "SEE PLAYER ACTION>")
             (read-line))
           (setf response (funcall (holdemagent-agentfunction agentstruct) gamestate playerid))
           (when verbose
             (format t "ACTION=~A" response))
            (case (first response)
              (:call (cond ((< (holdemround-bet gamestate) (aref banks playerid))
                            (place-bet playerid baseround :call))
                           (t
                            (cond ((and verbose (< (holdemround-bet gamestate) (aref banks playerid))) 
                                   (warnmsg :CALL-WITH-LOW-FUNDS))
                                  ((and verbose (= (holdemround-bet gamestate) (aref banks playerid)))
                                   (warnmsg :CALL-IN-PLACE-OF-ALLIN)))
                            (fold-player-out playerid baseround)
                            (push (list playerid :fold) (holdemround-actions baseround)))))
              (:allin (let* ((betvalue (aref banks playerid))
                             (raisevalue (- betvalue (holdemround-bet gamestate))))
                        (cond ((and (plusp raisevalue)
                                    ;;(< raisecount 3)
                                    )
                               (place-bet playerid baseround :allin)
                               (incf raisecount)
                               (setf stageend-pos turnpos stageend-id playerid))
                              ((not (plusp raisevalue))
                               (place-bet playerid baseround :allin))
                             ;; (t
                             ;;  (when (and verbose (>= raisecount 3)) (warnmsg :ALLIN-PASSES-RAISELIMIT))
                             ;;  (fold-player-out playerid baseround)
                             ;;  (push (list playerid :fold) (holdemround-actions baseround)))
                              )))
              (:raise (let* ((raisevalue (second response))
                             (totalbet (+ raisevalue (holdemround-bet gamestate))))
                        (cond ((and (< totalbet (aref banks playerid))
                                    (>= raisevalue (holdemround-blind gamestate))
                                  ;;  (< raisecount 3)
                                    )
                               (place-bet playerid baseround :raise raisevalue)
                               (incf raisecount)
                               (setf stageend-pos turnpos stageend-id playerid)) ;;betting needs to go around back to this player
                              (t
                               (cond ;;((and verbose (>= raisecount 3)) (warnmsg :RAISE-PASSES-RAISELIMIT))
                                     ((and verbose (> totalbet (aref banks playerid))) (warnmsg :RAISE-EXCEEDS-BANKROLL))
                                     ((and verbose (= totalbet (aref banks playerid))) (warnmsg :RAISE-IN-PLACE-OF-ALLIN))
                                     ((and verbose (< raisevalue (holdemround-blind gamestate))) (warnmsg :RAISE-MIN-IGNORED)))
                               (fold-player-out playerid baseround)
                               (push (list playerid :fold) (holdemround-actions baseround))))))
              (:check (cond ((= (holdemround-bet gamestate) 0)
                             (push (list playerid :check) (holdemround-actions baseround)))
                            (t
                             (fold-player-out playerid baseround)
                             (push (list playerid :fold) (holdemround-actions baseround)))))
              (:fold (fold-player-out playerid baseround)
                     (push (list playerid :fold) (holdemround-actions baseround)))
              (t (when verbose (warnmsg :UNRECOGNIZED-RESPONSE))
                 (fold-player-out playerid baseround)
                 (push (list playerid :fold) (holdemround-actions baseround))))))
       ;;advance to next seat at the table 
       (setf turnpos (mod (+ 1 turnpos) numplayers))
       (setf playerid (nth turnpos players))
       (setf stagedone (or (= stageend-pos turnpos)
                           (zerop (actionable-players baseround))
                           (= (1- numplayers) (number-of-folded-or-busted-players baseround))))
       )))
    baseround)


(defun cleanup-stage (baseround seating deck &optional (verbose nil) (pauser nil))
  (declare (ignore deck))
  (push (LIST 0 :cleanup) (holdemround-actions baseround))
  (when verbose 
        (format t "~%FINAL GAME COFIGURATION BEFORE DETERMINING WINNERS:")
        (display-holdemround baseround seating t))
  (when verbose
    (format t "~%$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"))
  (loop for pot in (holdemround-pots baseround) doing
        (setf baseround (determine-pot-winner (first pot) (second pot) baseround seating verbose pauser)))
  ;;notify all players of unfolded players' cards
  (loop for playerid in (holdemround-players baseround) doing
        (let ((gamestate (copy-holdemround baseround :pointofview playerid :reveal-unfolded t))
              (agentstruct (find-if #'(lambda (x) (equal (holdemagent-id x) playerid)) seating)))
          (when (plusp (aref (holdemround-playerbanks baseround) playerid))
            (funcall (holdemagent-agentfunction agentstruct) gamestate playerid)))) ;;ignore response
  (when verbose
    (format t "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$~%~%END OF ROUND: BANKROLLS ARE UPDATED, and NON-BUSTED PLAYERS NOTIFIED OF THIS STATE (their own private cards not shown here if they folded, but they do get this info):")
    (display-holdemround (copy-holdemround baseround :reveal-unfolded t) seating t))
  (when pauser
    (format t "START NEXT ROUND>")
    (read-line))
  baseround)


(defun determine-pot-winner (potplayers pot round seating &optional (verbose nil) (pauser nil))
  (declare (ignore pauser))
  (cond ((= (length potplayers) 1)
         (when verbose
           (format t "~%POT WINNER:~A ~D~%" 
                   (holdemagent-namestring 
                    (find-if #'(lambda (x) (equal (holdemagent-id x) (first potplayers))) seating))
                   pot))
         (incf (aref (holdemround-playerbanks round) (first potplayers)) pot))
        (t
         (let ((besthands (loop for id in potplayers collecting
                                (list id (best-texas-holdem-hand (copy-list (holdemround-commoncards round))
                                                                 (copy-list (aref (holdemround-playercards round) id))))))
               (winners nil)
               (numplayers (length potplayers)))
           (loop for h1 in besthands doing
                 (let ((winnervote 0))
                   (loop for h2 in besthands doing
                         (case (pick-winner (second h1) (second h2))
                           (:h1 (incf winnervote))
                           (:tie (incf winnervote))))
                   (when (= winnervote numplayers)
                     (setf winners (cons (first h1) winners)))))
           (when verbose
             (format t "~%POT COMPARISON (~A ~D)~%" potplayers pot))
           (loop for i in besthands doing
                 (cond (verbose 
                        (format t "    ~d: ~A [~A] ~A~%"
                                (first i) (hand-to-sym (second i)) (handtype (second i))
                                (cond ((member (first i) winners)
                                       (incf (aref (holdemround-playerbanks round) (first i)) (/ pot (length winners)))
                                       (format nil "~A WINS ~D" 
                                               (holdemagent-namestring 
                                                (find-if #'(lambda (x) (equal (holdemagent-id x) (first i))) seating))
                                               (/ pot (length winners))))
                                      (t " "))))
                       (t  (cond ((member (first i) winners)
                                  (incf (aref (holdemround-playerbanks round) (first i)) (/ pot (length winners)))))))))))
  round)





(defun warnmsg (situation)
  (case situation
    (:ALLIN-PASSES-RAISELIMIT
     (format t "WARNING: This ALLIN action is a raise that exceeded the number of raises (3) allowed in this betting cycle.~%")
     (format t "WARNING: It was coerced to a FOLD.  The correct nonfold response when the maximum number of allowed raises~%")
     (format t "WARNING: has been reached is to CALL the bet.~%"))
    (:RAISE-PASSES-RAISELIMIT
     (format t "WARNING: This RAISE action is a raise that exceeded the number of raises (3) allowed in this betting cycle.~%")
     (format t "WARNING: It was coerced to a FOLD.  The correct nonfold response when the maximum number of allowed raises~%")
     (format t "WARNING: has been reached is to CALL the bet.~%"))
    (:CHECK-IGNORED
     (format t "WARNING: This CHECK was coerced to a FOLD because the required bet is greater than 0.~%")
     (format t "WARNING: You can only CHECK when the required bet is 0.~%"))
    (:RAISE-MIN-IGNORED
     (format t "WARNING: This RAISE was coerced to a FOLD because its value was less than the current round's blind.~%")
     (format t "WARNING: If your bankroll was low, you should go ALLIN. An ALLIN whose implicit increment is less than the~%")
     (format t "WARNING: minimum bet will not be counted as a raise.~%"))
    (:CALL-WITH-LOW-FUNDS
     (format t "WARNING: This CALL was coerced to a FOLD because your bankroll had insufficient money to cover the required bet.~%")
     (format t "WARNING: When funds are insufficient to cover the minimum bet, an ALLIN is always allowed, and does not~%")
     (format t "WARNING: count as a raise.~%"))
    (:RAISE-EXCEEDS-BANKROLL
     (format t "WARNING: This RAISE was coerced to a FOLD because it, along with the bet you're matching, exceeds your~%")
     (format t "WARNING: bankroll.  Remember that a RAISE when you have a required bet means you are promising to add~%")
     (format t "WARNING: the raise amount AND the remaining required bet to the pot.~%"))
    (:UNRECOGNIZED-RESPONSE
     (format t "WARNING: This response was unrecognizable as (:FOLD), (:CHECK), (:RAISE <amt>), (:ALLIN), (:CALL),~%")
     (format t "WARNING: and so it was coerced to a FOLD.~%"))
    (:RAISE-IN-PLACE-OF-ALLIN
     (format t "WARNING: This RAISE was coerced to a FOLD because its value + the required bet exactly matched your~%")
     (format t "WARNING: bankroll.  Whenever a bet response will exhaust your bankroll, you are required to send~%")
     (format t "WARNING: an ALLIN response so that other players can be alerted to your bankroll status.~%"))
    (:CALL-IN-PLACE-OF-ALLIN
     (format t "WARNING: This CALL was coerced to a FOLD because this required bet exactly matched your bankroll.~%")
     (format t "WARNING: Whenever a bet response will exhaust your bankroll, you are required to send an ALLIN~%")
     (format t "WARNING: response so that other players can be alerted to your bankroll status.~%"))
    ))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;When VERBOSE is set to t, a trace of the game will be printed out
;;When PAUSER is set to t, the driver will generate a prompt and wait
;;                         for you to hit ENTER before displaying the
;;                         next state.
;;
;;EXAMPLE: Try typing (holdem-game-driver *test-players* :verbose t :pauser t)
;;         to see a step-by-step execution.
;;         Type (holdem-game-driver *test-players* :verbose t)
;;         to see a whole game generated at once.
(defun holdem-game-driver (agentlist &key (blindroundcount 10) (startblind 100) (bank 10000) (verbose nil) (pauser nil))
  (let ((roundnum 0)
        (seating (randomize-list agentlist))
        (deck nil)
        (baseroundstate (create-holdemround (length agentlist) bank startblind)))
    ;;set up tournament
    (loop for i from 1 to (length agentlist) doing (setf (holdemagent-id (nth (1- i) seating)) i))
    (setf (holdemround-players baseroundstate) (loop for p in seating collecting (holdemagent-id p)))

    (while (> (number-of-players-with-money baseroundstate) 1)
       (incf roundnum)
       (setf (holdemround-roundcount baseroundstate) roundnum)
       (setf deck (create-deck :size (+ 5 (* 2 (length (holdemround-players baseroundstate))))))
       (setf baseroundstate (reset-holdemround baseroundstate))

       (when (and (> roundnum blindroundcount) (= (mod roundnum blindroundcount) 1)) ;; time to double the blind
         (setf (holdemround-blind baseroundstate)
               (* 2 (holdemround-blind baseroundstate))))
       (setf (holdemround-internalpot baseroundstate) (create-holdempot (player-ids-with-money baseroundstate)))
       (when verbose
         (format t "~%~%~%~%****************************************************************************~%START OF ROUND ~D" roundnum))
       (setf baseroundstate (preflop-stage baseroundstate seating deck verbose pauser))
       (setf baseroundstate (flop-stage baseroundstate seating deck verbose pauser))
       (setf baseroundstate (turn-stage baseroundstate seating deck verbose pauser))
       (setf baseroundstate (river-stage baseroundstate seating deck verbose pauser))
       (setf baseroundstate (cleanup-stage baseroundstate seating deck verbose pauser))
       (when verbose
         (format t "~%****************************************************************************")))
    baseroundstate))



