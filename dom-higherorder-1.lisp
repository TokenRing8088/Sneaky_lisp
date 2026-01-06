;;;; Poznamka:
;;;;   Nasledujuce priklady je mozne riesit pomocou pouzitia 
;;;;   funkcii vyssieho radu (bez nutnosti explicitneho pouzitia
;;;;   rekurzie alebo iteracie)
;;;;
;;;;   Alternativne riesenie je pouzit rekurziu alebo iteraciu
;;;;   bez nutnosti pouzitia funkcii vyssieho radu


;;; Generovanie postupnosti
;;;
;;;   (postupnost 5) -> (1 2 3 4 5)
;;;
;;; Napoveda: v rieseni nepouzivajte rekurziu ani iteraciu
(defun postupnost (dlzka)
  (reverse (maplist #'length (make-list dlzka)))
  )


;;;
;;; Alternativa: - generujte rad 1/1,1/2,...
(defun postupnost (dlzka)
  (reverse (maplist #'(lambda (x) (/ 1 (length x))) (make-list dlzka)))
  )

;;;              - generujte rad c^(n-1),c^(n-2),...,c^1,c^0
(defun postupnost (c n)
  (maplist #'(lambda (x) (expt c (1- (length x)))) (make-list n))
  )

;;;              - generujte postupnost (min, min+1, ..., max-1, max)
(defun postupnost (min max)
 (reverse (maplist #'(lambda (x) (+ (1- (length x)) min)) (make-list (1+ (- max min)))))
  )

;;; Generovanie postupnosti (1 2 3 3 3 4 4 4 4 ...) podla jedneho
;;; zadaneho parametra
;;;
;;;   (postupnost 4) -> (1 2 2 3 3 3 4 4 4 4)
(defun postupnost (dlzka)
  (mapcan #'(lambda (x) (make-list x :initial-element x)) (reverse (maplist #'length (make-list dlzka))))
  )

;;; Vypocet hodnoty polynomu
;;;
;;;   (polynom 2 '(1 3 0 2)) -> 22
;;;
;;; Napoveda: riesenie zalozte na pouziti 'reduce'
;; horny schema
;;  (format t "+ ~D (* ~D ~D) = ~D ~%" koef koren acc (+ koef (* koren acc)))
(defun polynom (koren zoznam)
  (reduce #'(lambda (acc koef) (+ koef (* koren acc))) zoznam))

;;; Zadany lubovolny pocet mnozin - najst prvky ktore su v kazdej mnozine
;;;
;;;   (op-intersection '((1 2 3 5) (2 3 4) (3 4 5 6))) -> (3)
;;;
(defun op-intersection (zoznam)
  (reduce #'(lambda (z1 z2) (remove-if-not #'(lambda (x) (member x z2) ) z1)) zoznam)
  )


;;; Napoveda: v rieseni nepouzivajte rekurziu ani iteraciu
;;;
;;; Alternativa: - najst prvky, ktore su iba v jednej z mnozin
(defun op-intersection (zoznam)
  (remove-if #'(lambda (x) (< 1 (count x (apply #'append zoznam)))) (remove-duplicates (apply #'append zoznam)))
)


;;;              - najst prvky, ktore su aspon v jednej z mnozin
;; destruktivne
(defun op-intersection (zoznam)
  (remove-duplicates (mapcan #'(lambda (x) x) zoznam)))

;; nedestruktivne
(defun op-intersection (zoznam)
  (remove-duplicates (apply #'append zoznam)))

;;; Je zadany zoznam bodov na ploche (reprezentovanych dvojprvkovymi 
;;; zoznamami). Najdite ten bod, ktory je najdalej od pociatku.
;;;
;;;   (bod-najdalej '((3 4)(1 2)(6 2)(4 2))) -> (6 2) 
;;;
;;; Napoveda: riesenie zalozte na pouziti 'reduce'

(defun bod-najdalej (zoznam)
  (reduce #'(lambda (z1 z2) (if (> ( math z1)  (math z2)) )
              z1
              z2
              )
          zoznam)
  )

;;; Urcenie hodnoty cisla z cifier pri zadanej ciselnej baze
;;;
;;;   (hodnota 10 '(2 0 1 1)) -> 2011
;;;
;;; Napoveda: v rieseni pouzivajte 'mapcar'
;;;           - pre mocnenie pouzite (expt 2 3) -> 8

(defun hodnota (baza cifry)
  (reduce #'+ (mapcar #'(lambda (x y) (* x (expt baza y)) ) cifry (maplist #'(lambda (z) (1- (length z))) (make-list (length cifry)))))
  )

;;; Pre standardnu 'sudoku' plochu (9 riadkov, 9 stlpcov, 
;;; 9 stvorcov 3x3) generujte vsetky policka z ktorych
;;; pozostava n-ty riadok (cislovanie zacina od 0)
;;;
;;;   (op-riadok 3) -> ((3 0)(3 1)(3 2)(3 3)(3 4)(3 5)(3 6)(3 7)(3 8))
;;;
;;; Napoveda: riesenie zalozte na 'mapcar'
;;;

(defun op-riadok (cislo)
  (mapcar #'(lambda (x) (list cislo x)) (mapcar #'(lambda (x) (1- x)) (reverse (maplist #'length (make-list 9)))))
  )

;;; Alternativa: policka stvorca x,y 
;;;   (op-stvorec 0 0) -> 
;;;       ((0 0)(0 1)(0 2)(1 0)(1 1)(1 2)(2 0)(2 1)(2 2))

(defun op-stvorec (riadok stlpec)
  (mapcar #'(lambda (r s) (list r s)) (mapcan #'(lambda (y) (list y y y)) (list riadok (1+ riadok) (+ 2 riadok))) (mapcar #'(lambda (x) (+ (mod (1- x) 3) stlpec)) (reverse (maplist #'length (make-list 9)))))
 )


;;; Zo zoznamu navzajom roznych cisel odstrante tie cisla ktore kazia 
;;; vzostupne usporiadanie zoznamu
;;;
;;;   (urob-vzostup '(2 7 6 4 8 5 9)) -> (2 4 5 9)
;;;


(defun urob-vzostup (zoznam)
  (remove-if #'(lambda (x) (some #'(lambda (y) (> x y)) (cdr (member x zoznam)))) zoznam)
  )

;;; Napoveda: riesenie zalozte na pouziti 'remove-if' 
;;;           - na ziskanie zvysku zoznamu zacinajuceho nejakym 
;;;             cislom pouzite member
;;;
;;; Alternativa: riesenie zalozte na 'remove-if-not'
(defun urob-vzostup (zoznam)
  (remove-if-not #'(lambda (x) (every #'(lambda (y) (< x y)) (cdr (member x zoznam)))) zoznam)
  )

;;; Rozklad cisla na sucinitele, ak cislo pozostava z navzajom roznych
;;; sucinitelov
;;;
;;;   (rozklad 210) -> (2 3 5 7)
;;;
(defun rozklad (num)
 (reverse (remove-if-not #'(lambda (y) (and (< 1 y)
                                     (= 2 (length
                                           (remove-if-not #'(lambda (z) (zerop (mod y z))) (maplist #'length (make-list y))))) ))
                  (remove-if-not #'(lambda (x) (zerop (mod num x))) (maplist #'length (make-list num)))
                  ))
  )

;;;
;;; Napoveda: v rieseni nepouzivajte rekurziu ani iteraciu
;;;           - pre kontrolu delitelnosti pouzite (mod 5 3) -> 2
   

;;; Zadane su dve mnoziny a (ne)rovnost - kontrola ci plati (ne)rovnost
;;;
;;;   (kontrola '(1 2 3 4 5) '(3 4 5 6 7) #'>)  -> nil
;;;   (kontrola '(4 5) '(3 4) #'>)              -> t
;;;
;;; Napoveda: riesenie zalozte na pouziti 'some' a 'every'
(defun kontrola (z1 z2 op)
  (every op z1 z2)
  )


;;; Zadane dve mnoziny a (ne)rovnost - opravy aby platila (ne)rovnost
;;;
;;;  (oprava '(1 2 3 4 5) '(3 4 5 6 7) #'>)  -> (4 5) (3 4)
;;;  (oprava '(1 2 3 4 5) '(3 4 5 6 7) #'=)  -> (3 4 5) (3 4 5)
;;;
;;; Napoveda: riesenie zalozte na pouziti funkcii vyssieho radu

(defun oprava (z1 z2 op)
  (cond
    ((eq op #'=) (format t "~a ~a" (reverse (intersection z1 z2)) (reverse (intersection z2 z1))))
    ((eq op #'<) (format t "~a ~a" (reverse (cdr (intersection z1 z2))) (reverse (butlast (intersection z2 z1)))))
    ((eq op #'>) (format t "~a ~a" (reverse (butlast (intersection z1 z2))) (reverse (cdr (intersection z2 z1)))))
    )
  )

(defun oprava (z1 z2 op)
  (format t "~a ~a"
          (remove-if-not #'(lambda (x) (some #'(lambda (y) (funcall op x y)) z2)) z1)
          (remove-if-not #'(lambda (y) (some #'(lambda (x) (funcall op x y)) z1)) z2)))


;;; Vytvarajte vsetky rozne dvojice
;;;
;;;   (dvojice '(a b c) '(1 2)) -> ((a 1) (a 2) (b 1) (b 2) (c 1) (c 2))
;;;
;;; Napoveda: riesenie zalozte na pouziti 'mapcar'

(defun dvojice (z1 z2)
  (mapcar #'(lambda (x y) (list x y)) (mapcan #'(lambda (prvok) (make-list (length z2) :initial-element prvok )) z1) (mapcan (lambda (z) (declare (ignore z)) (copy-list z2)) z1))
  )


;;; Vytvarajte vsetky rozne kombinacie z prvkov zadaneho zoznamu
;;;
;;;   (kombinacie '(a b c)) -> ((a b c) (a b) (a c) (a) (b c) (b) (c) nil)
;;;   (kombinacie '())      -> (nil)
;;;
;;; Napoveda: riesenie zalozte na sucasnom pouziti 'mapcar' a rekurzie
;;; - pre odhalenie rekurzivneho pravidla porovnajte vystup
;;;     pre zoznam (a b c) a zoznam (b c)
(defun kombinacie (zoznam)
  (cond
    ((null zoznam) (list nil))
    (t (append (mapcar #'(lambda (x) (cons (car zoznam) x)) (kombinacie (cdr zoznam))) (kombinacie (cdr zoznam))))
    ))



;;; Otacanie matice zadanej po riadkoch o 90 stupnov v smere a 
;;; proti smeru hodinovych ruciciek
;;;
;;;    (otoc+ '((1 2 3)(4 5 6)(7 8 9))) ->
;;;        ((7 4 1)(8 5 2)(9 6 3))
;;;    (otoc- '((1 2 3)(4 5 6)(7 8 9))) ->
;;;        ((3 6 9)(2 5 8)(1 4 7))
;;;
;;; Napoveda: maticu najprv transponujte (vymente riadky za stlpce)
;;;           a nasledne upravte zmenou poradia prvkov

(defun transp (matica)
  (apply #'mapcar #'list matica)
  )

(defun otoc+ (matica)
  (transp (reverse matica))
  )

(defun otoc- (matica)
  (reverse (transp matica))
  )
