;;;; Poznamka: 
;;;;   Nasledujuce priklady je mozne riesit pomocou rekurzie 
;;;;   (pocitanie smerom dnu aj von) alebo pomocou iteracie

;;; Sucet dvoch nezapornych celociselnych scitancov
;;; 
;;;   (op-plus 3 5) -> 8
;;;
;;; Napoveda: riesenie zalozte na pouziti '1+' a/alebo '1-'
(defun op-plus (a b)
  (cond ((zerop b) a)
        (t (op-plus (1+ a) (1- b))))
  )

;;; Sucet dvoch celociselnych scitancov
;;; 
;;;   (op-plus  3  5) ->  8
;;;   (op-plus -3  5) ->  2
;;;   (op-plus -3 -5) -> -8
;;;
;;; Napoveda: riesenie zalozte na pouziti '1+' a/alebo '1-'

(defun op-plus (a b)
  (cond
    ((> b 0) (op-plus (1+ a) (1- b)))
    ((< b 0) (op-plus (1- a) (1+ b)))
    (t a)))


;;; Rozdiel dvoch nezapornych celych cisel
;;;
;;;   (op-minus 5 3) ->  2
;;;   (op-minus 3 5) -> -2
;;;
;;; Napoveda: riesenie zalozte na pouziti '1+' a/alebo '1-'

(defun op-minus (a b)
  (cond ((zerop b) a)
        (t (op-minus (1- a) (1- b))))
  )


;;; Rozdiel dvoch celych cisel
;;;
;;;   (op-minus  3  5) -> -2
;;;   (op-minus  3 -5) ->  8
;;;   (op-minus -3 -5) ->  2
;;;
;;; Napoveda: riesenie zalozte na pouziti '1+' a/alebo '1-'

(defun op-minus (a b)
  (cond
    ((> b 0) (op-minus (1- a) (1- b)))
    ((< b 0) (op-minus (1+ a) (1+ b)))
    (t a)))


;;; Sucin dvoch celociselnych sucinitelov
;;;
;;;   (op-krat  3  4) -> 12
;;;   (op-krat -3  4) -> -12
;;;   (op-krat -3 -4) ->  12
;;;
;;; Napoveda: riesenie zalozte na pouziti '+'

(defun op-krat (a b)
  (cond
    ((zerop b) 0)
    ((> b 0) (+ a (op-krat a (1- b))))
    (t (+ (- a) (op-krat a (1+ b))))
    ))






(defun op-krat (a b)
  (cond ((zerop b) 0)
        ((> b 0) (+ a (op-krat a (1- b))))
        (t  (+ (- a) (op-krat a (1+ b))))
        ))


;;; Kontrola delitelnosti dvoch celych cisel
;;;
;;;   (p-del  12  4) -> t
;;;   (p-del -12  4) -> t
;;;   (p-del -12 -4) -> t
;;;   (p-del  13 -4) -> nil
;;;
;;; Napoveda: riesenie zalozte na pouziti '-'

(defun op-del (a b)
  (cond ((zerop a) t)
        ( (< a 0) (op-del (- a) b))
        ( (< b 0) (op-del a (- b)))
        ( (> b a) nil)
        (t (op-del (- a b) b))
        ))

;;; Celociselne delenie dvoch celych cisel
;;;
;;;   (op-celdel 12 4) -> 3
;;;   (op-celdel -13  4) -> -3
;;;   (op-celdel -13 -4) ->  3
;;;
;;; Napoveda: riesenie zalozte na pouziti '-'

(defun op-celdel (a b &optional (acc 0))
  (cond
    ((< b 0) (- (op-celdel a (- b))))
    ((< a 0) (- (op-celdel (- a) b)))
    ((> b a) acc)
    (t (op-celdel (- a b) b (1+ acc)))
    )
)



(defun op-celdel (a b &optional (acc 0))
  (cond
    ((< a 0) (- (op-celdel (- a) b)))
    ((< b 0) (- (op-celdel a (- b))))
    ((> b a) acc)
    (t (op-celdel (- a b) b (1+ acc))))
  )

(defun op-celdel (a b &optional (acc 0))
  (cond
    ((and (< a 0) (< b 0)) (op-celdel (- a) (- b)))
    ((< b 0) (- (op-celdel a (- b))))
    ((< a 0) (- (op-celdel (- a) b)))
    ((> b a) acc)
    (t (op-celdel (- a b) b (1+ acc))))
  )


;;; Umocnovanie pomocou nezaporneho celociselneho exponentu
;;;
;;;   (op-mocn  2 4) -> 16
;;;   (op-mocn -3 2) ->  9
;;;   (op-mocn -3 3) -> -27
;;;
;;; Napoveda: riesenie zalozte na pouziti '*'

(defun op-mocn (a b)
  (cond
    ((zerop b) 1)
    (t (* a (op-mocn a (1- b))))
    ))


;;; Celociselna odmocnina nezaporneho celeho cisla
;;;
;;;   (op-odm 16) -> 4
;;;   (op-odm 24) -> 4
;;;
;;; Napoveda: riesenie zalozte na pouziti '-' 

(defun op-odm (num &optional (start 1) (acc 0))
  (cond
    ((< num start) acc)
    (t (op-odm (- num start) (+ start 2) (1+ acc)))
        ))

;;; Porovnavanie dvoch celych cisel
;;;
;;;   (p->  4  5) -> nil
;;;   (p->  5  4) -> t
;;;   (p-> -5 -4) -> nil
;;;
;;; Napoveda: riesenie zalozte na pouziti '1+' a/alebo '1-'
;;;
;;; Analogicky navrhnite predikaty 'p-<' a 'p-='

(defun p-< (a b)
  (cond
    ((and (zerop a) (zerop b)) nil)
    ((and (plusp a) (plusp b)) (p-< (1- a) (1- b)))
    ((and (minusp a) (minusp b)) (p-< (1+ a) (1+ b)))
    ((plusp a) nil)
    ((minusp a) t)
    (t (plusp b))
    ))

(defun p-> (a b)
  (cond
    ((and (zerop a) (zerop b)) nil)
    ((and (plusp a) (plusp b)) (p-> (1- a) (1- b)))
    ((and (minusp a) (minusp b)) (p-> (1+ a) (1+ b)))
    ((plusp a) t)
    ((minusp a) nil)
    (t (minusp b)); a je na nule
    ))

(defun p-= (a b)
  (cond
    ((and (zerop a) (zerop b)) t)
    ((and (plusp a) (plusp b))(p-> (1- a) (1- b)))
    ((and (minusp a) (minusp b)) (p-> (1+ a) (1+ b)))
    (t nil); a je na nule
    ))


;;; Test parnosti a neparnosti celych cisel
;;;
;;;   (p-neparne  4) -> nil
;;;   (p-neparne -3) -> t
;;;   (p-parne  4) -> t
;;;   (p-parne -3) -> nil
;;;
;;; Napoveda: riesenie zalozte na pouziti '1+' a/alebo '1-'

(defun p-parne (n)
  (cond
    ((zerop n) t)
    ((plusp n) (not (p-parne (1- n))))
    ((minusp n) (not (p-parne (1+ n)))))
  )

(defun p-neparne (n)
  (cond
    ((zerop n) nil)
    ((plusp n) (not (p-neparne (1- n))))
    ((minusp n) (not (p-neparne (1+ n)))))
  )


;;; Najvacsi spolocny delitel dvoch nezapornych celych cisel 
;;;
;;;    (op-maxsd 15 9) -> 3
;;;    (op-maxsd  9 9) -> 9
;;;    (op-mxsd  17 5) -> 1
;;;    
;;; Napoveda: riesenie zalozte na pouziti 'mod'

(defun op-maxsd (a b &optional (num (min a b)))
  (cond
    ((and (zerop (mod a num)) (zerop (mod b num))) num)
    (t (op-maxsd a b (1- num))))
  )


;;; Najmensi spolocny nasobok dvoch nezapornych celych cisel
;;;
;;;    (op-minsn 6 9) -> 18
;;;    (op-minsn 6 6) -> 6
;;;    
;;; Napoveda: riesenie zalozte na pouziti 'mod'


(defun op-minsd (a b &optional (num  (min a b)))
  (cond
    ((and (zerop (mod num a)) (zerop (mod num b))) num)
    (t (op-minsd a b (+ (min a b) num))))
  )

;;; Odmocnina nezaporneho realneho cisla (s presnostou na 0.01)
;;;
;;;   (op-rodm 2.0) -> 1.4142
;;;
;;; Napoveda: riesenie zalozte na pouziti Newtonovej metody postupnych 
;;;   aproximacii:
;;;     Ak y je (nepresny) odhad korena cisla x, potom presnejsi 
;;;     odhad sa ziska ako priemer hodnot y a x/y.
;;;
(defun op-rodm (x &optional (y 1.0))
  (cond
    ((< (abs (- (* y y) x)) 0.01) y)
    (t (op-rodm x (/ (+ y (/ x y)) 2.0)))
   ))

;;; Alternativa: pozadovana presnost je vstupnym argumentom

(defun op-rodm (num &optional (epsi 0.01) (guess 1.0))
  (cond
    ((< (abs (- (* guess guess) num)) epsi) guess)
    (t (op-rodm num epsi (/ (+ guess (/ num guess)) 2.0))))
  )

;;; Faktorial kladneho celeho cisla
;;;
;;;   (op-fakt 5) -> 120
;;;
;;; Napoveda: :-)

(defun op-fakt (a)
  (cond ((zerop a) 1)
        (t (* a (op-fakt (1- a))))
        ))


;;; Tlac rozkladu kladneho celeho cisla do suctu jednotlivych radov
;;;
;;;   (rozloz 2345) -> 2*10^3 + 3*10^2 + 4*10^1 + 5*10^0
;;;
;;; Napoveda: riesenie moze vyzadovat zistenie poctu cifier, z ktorych
;;;           sa cislo sklada

(defun rozloz (cislo &optional (acc 0))
  (cond
    ((zerop (mod cislo 10)) t)
    (t  (rozloz (/ (- cislo (mod cislo 10)) 10) (1+ acc))
        (if (zerop acc)
            (format t "~D*10^~D"  (mod cislo 10) acc)
            (format t "~D*10^~D + "  (mod cislo 10) acc)))
    ))


(defun rozloz (cislo &optional (acc 0))
  (cond
    ((zerop (mod cislo 10)) t)
    (t (rozloz (/ (- cislo (mod cislo 10)) 10) (1+ acc))
       (if (zerop acc)
           (format t "~D*10^~D" (mod cislo 10) acc)
           (format t "~D*10^~D + " (mod cislo 10) acc))
           ))
    )

;;; Tlac kladneho celeho cisla ako postupnosti jeho cifier
;;;
;;;   (cifry 2345) -> 2 3 4 5
;;;
;;; Napoveda: mozno budete potrebovat 'floor' a 'mod'

(defun cifry (num)
  (cond
    ((zerop num) t)
    (t (cifry (/ (- num (mod num 10)) 10)) (format t "~D " (mod num 10)))
    ))


;;; Tlac kosostvorca v strede rastra rozmeru k x k (k je neparne
;;; kladne cele cislo)
;;;
;;;   (obraz 7) ->
;;;     . . . * . . .
;;;     . . * * * . .
;;;     . * * * * * .
;;;     * * * * * * *
;;;     . * * * * * .
;;;     . . * * * . .
;;;     . . . * . . .
;;;
;;; Napoveda: pocet hviezdiciek v riadku zavisi na cisle riadku
;;;           a celkovom pocte riadkov.



(defun aux-print-riadok (stars dots)
  (cond
  ((<= stars 0) t)
  ((zerop dots) (format t "* ") (aux-print-riadok (1- stars) dots ))
  (t  (format t ". ") (aux-print-riadok stars (1- dots))  (format t ". "))
  ))

(defun obraz (size &optional (acc 1))
  (cond
  ((< size acc) t)
  (t (aux-print-riadok acc (/ (- size acc) 2)) (format t "~%") (obraz size (+ 2 acc)) (aux-print-riadok (- acc 2) (/ (- size (- acc 2)) 2))  (format t "~%"))
  ))


(defun aux-print (stars dots)
  (cond
    ((<= stars 0) t)
    ((zerop dots) (format t "* ") (aux-print (1- stars) dots))
    (t (format t ". ") (aux-print stars (1- dots)) (format t ". "))))

(defun obraz (size &optional (acc 1))
  (cond
    ((< size acc) t)
    (t (aux-print acc (/ (- size acc) 2) ) (format t "~%")  (obraz size (+ acc 2)) (aux-print (- acc 2)  (/ (- size (- acc 2)) 2) ) (format t "~%"))))

;;; Rozklad nezaporneho celeho cisla na jeho sucinitele - sucinitele vytlacte
;;; do riadku za sebou;;;
;;;   (rozklad 210)    -> 2 3 5 7
;;;   (rozklad 95095)  -> 5 7 11 13 19
;;;   (rozklad 123456) -> 2 2 2 2 2 2 3 643
;;;
;;; Napoveda: - pre tlac pouzite format
;;;           - pre kontrolu delitelnosti pouzite (mod 5 3) -> 2

(defun rozklad (cislo &optional (start 2))
  (cond
    ((= cislo start) (format t "~D" start) t)
    ((zerop (mod cislo start)) (format t "~D " start) (rozklad (/ cislo start) start))
    (t (rozklad cislo (1+ start)))
    ))


(defun rozklad (cislo &optional (acc 2))
  (cond
    ((= cislo acc)  (format t "~D" acc) t)
    ((zerop (mod cislo acc)) (format t "~D " acc) (rozklad (/ cislo acc) acc))
    (t (rozklad cislo (1+ acc))))
  )
