;;;; Poznamka:
;;;;   Nasledujuce priklady je mozne riesit pomocou pouzitia 
;;;;   rekurzie


;;; Maximalna hlbka listoveho uzlu v strome
;;;
;;;   (tree-hlbka '(((a b) c) (a (s (f) g) q))) -> 4
;;;   (tree-hlbka '(((a b) c) (a (s () g) q)))  -> 3
;;; 
;;; Napoveda: - pre zistenie maximalnej hpodnoty pouzite 'max'
;;;           - zapis (((a b) c) (a (s (f) g) q)) reprezentuje strom
;;;
;;;             +
;;;             |
;;;         +---+---+
;;;         |       |
;;;       +-+-+  +--+--+
;;;       |   c  a  |  q
;;;     +-+-+     +-+-+ 
;;;     a   b     s | g
;;;                 +
;;;                 f
(defun tree-hlbka (strom &optional (hlbka 0))
  (cond
    ((null strom) hlbka)
    ((atom strom) hlbka)
    (t (max (tree-hlbka (car strom) (1+ hlbka)) (tree-hlbka (cdr strom) hlbka)))
    )
  )

;;; Vytvorenie zoznamu listovych uzlov stromu
;;;
;;;   (tree>list '(((a b) c) (a (s (f) g) q))) -> (a b c a s f g q)
;;;   (tree>list '(((a b) c) (a (s () g) q)))  -> (a b c a s g q)
;;;
;;; Napoveda: zapis (((a b) c) (a (s (f) g) q)) reprezentuje strom
;;;
;;;             +
;;;             |
;;;         +---+---+
;;;         |       |
;;;       +-+-+  +--+--+
;;;       |   c  a  |  q
;;;     +-+-+     +-+-+ 
;;;     a   b     s | g
;;;                 +
;;;                 f
(defun tree>list (strom)
  (cond
    ((null strom) nil)
    ((atom strom) (list strom))
    (t (append (tree>list (car strom)) (tree>list (cdr strom))))
    ))

;;; Test pritomnosti prvku v strome
;;;
;;;   (tree-member 5 '(1 (2 (4 a b) c) (3 a (5 s (6 f) g) q))) -> (5 s (6 f) g)
;;;   (tree-member 0 '(1 (2 (4 a b) c) (3 a (5 s (6 f) g) q))) -> nil
;;;
;;; Napoveda: zapis (1 (2 (4 a b) c) (3 a (5 s (6 f) g) q)) reprezentuje strom
;;;               1
;;;          +----+----+
;;;          2         3
;;;        +-+-+   +---+---+
;;;        4   c   a   5   q
;;;      +-+-+       +-+-+
;;;      a   b       s 6 g
;;;                    +
;;;                    f
(defun tree-member (prvok strom)
  (cond
    ((null strom) nil)
    ((atom strom) nil)
    ((eq (car strom) prvok) strom)
    (t (or (tree-member prvok (car strom)) (tree-member prvok (cdr strom))))
   ))
  
;;; Aritmeticke vypocty v infixnej notacii
;;;
;;;   (tree-calc '((3 * 5) + 9))             -> 24
;;;   (tree-calc '((3 * 5) + (9 - (2 * 6)))) -> 12
;;;  
;;; Napoveda: - pre testovanie ci objekt je cislo pouzite 'numberp'
;;;           - symboly '*', '-' a '*' referuju na prislusnu funkciu
;;;             svojim 'fvalue' slotom

(defun tree-calc (strom)
  (cond
    ((numberp strom) strom)
    ((atom strom) strom)
    (t (funcall (second strom)
                (tree-calc (first strom))
                (tree-calc (third strom))))
    ))

  
;;; Prechadzanie uzlami binarneho stromu
;;;
;;;   (preorder '(1 2 (3 (5 8 9) (6 4 7))))  -> (1 2 3 5 8 9 6 4 7)
;;;   (inorder '(1 2 (3 (5 8 9) (6 4 7))))   -> (2 1 8 5 9 3 4 6 7)
;;;   (postorder '(1 2 (3 (5 8 9) (6 4 7)))) -> (2 8 9 5 4 7 6 3 1)
;;;
;;; Napoveda: zapis (1 2 (3 (5 8 9) (6 4 7))) reprezentuje strom
;;;   
;;;         1
;;;     +---+---+
;;;     2       3
;;;         +---+---+
;;;         5       6
;;;       +-+-+   +-+-+
;;;       8   9   4   7
(defun preorder (strom)
  (cond
    ((null strom) nil)
    ((atom strom) (list strom))
    (t (append
        (preorder (first strom))
        (preorder (second strom))
        (preorder (third strom))))
    ))

(defun inorder (strom)
  (cond
    ((null strom) nil)
    ((atom strom) (list strom))
    (t (append
        (inorder (second strom))
        (inorder (first strom))
        (inorder (third strom))))
    ))

(defun postorder (strom)
  (cond
    ((null strom) nil)
    ((atom strom) (list strom))
    (t (append
       
        (postorder (second strom))
        (postorder (third strom))
        (postorder (first strom))))
    ))

;;; Transformacia stromu (zmena reprezentacie listovych uzlov
;;;
;;;   (dopln '(2 (1 nil nil) (6 (4 nil (5 nil nil)) (8 nil nil)))) ->
;;;         (2 1 (6 (4 nil 5) 8))
;;;   
;;;         2
;;;     +---+---+
;;;     1       6
;;;         +---+---+
;;;         4       8
;;;         +-+
;;;           5
;;;
;;; Napoveda: -
;;;

(defun dopln (strom)
  (cond
    ((null strom) nil)
    ((atom strom) strom)
    ((and (null (second strom)) (null (third strom)))
     (first strom))
    (t (list
        (dopln (first strom))
        (dopln (second strom))
        (dopln (third strom)))
       )
    ))

;;; Alternativa: opacna transformacia

(defun dopln-inverse (strom)
  (cond
    ((null strom) nil)
    ((atom strom) (list strom nil nil))
    (t (list
        (first strom)
        (dopln-inverse (second strom))
        (dopln-inverse (third strom))))
    
    ))


;;; Budovanie binarneho stromu
;;;
;;;   (list>strom '(2 6 4 1 5 8)) ->
;;;            (2 (1 nil nil) (6 (4 nil (5 nil nil)) (8 nil nil)))  
;;;   
;;;         2
;;;     +---+---+
;;;     1       6
;;;         +---+---+
;;;         4       8
;;;         +-+
;;;           5
;;;
;;; Napoveda: - pouzite rekurziu
;;;           - rozlozte na dve podulohy: pridanie prvku do stromu
;;;             a vytvorenie stromu
;;;
(defun aux-pridaj-prvok (prvok strom)
  (cond
    ((null strom) (list prvok nil nil))
    ((< prvok (first strom))
     (list
      (first strom)
      (aux-pridaj-prvok prvok (second strom))
      (third strom)))
    (t
     (list
        (first strom)
        (second strom)
        (aux-pridaj-prvok prvok (third strom))
        ))
    ))

(defun list>strom (zoznam &optional (strom))
  (cond
    ((null zoznam) strom)
    (t (list>strom (cdr zoznam) (aux-pridaj-prvok (car zoznam) strom))))
  )


;;; Alternativa: podulohu vytvorenia stromu rieste pomocou 'reduce'
(defun list>strom (zoznam)
  (reduce #'(lambda (strom prvok) (aux-pridaj-prvok prvok strom)) zoznam :initial-value nil)
  )


;;; Zamena prvkov v strome - kazdy uzol, ktory splna zadanu podmienku,
;;; je potrebne zamenit za nil
;;;
;;;    (zamen #'evenp '((1 (4 6)) ((5 3) 2))) ->
;;;                                    ((1 (nil nil) ((5 3) nil))
;;;
;;; Napoveda: zapis ((1 (4 6)) ((5 3) 2))) reprezentuje strom
;;;
;;;               +
;;;               |
;;;       +-------+-------+
;;;       |               |
;;;    +--+--+         +--+--+
;;;    1     |         |     2
;;;       +--+--+   +--+--+
;;;       4     6   5     3
;;;
(defun zamen (op strom)
  (cond
    ((null strom) strom)
    ((atom strom)
     (if (funcall op strom)
         nil
         strom))
    (t (list (zamen op (first strom)) (zamen op (second strom))))
    ))


;;; Alternativa: strom ktory nesie informaciu aj v nelistovych
;;;              uzloch
(defun zamen (op strom)
  (cond
    ((null strom) strom)
    ((atom strom)
     (if (funcall op strom)
         nil
         strom))
    (t (list (zamen op (first strom)) (zamen op (second strom)) (zamen op (third strom))))
    ))

;;; Redukcia stromu - vypustit kazdu vetvu na konci ktorej je nil
;;;
;;;    (redukuj '((1 (nil nil)) ((5 3) nil))) -> (1 (5 3))
;;;
;;; Napoveda: zapis ((1 (nil nil)) ((5 3) nil))) reprezentuje strom
;;;
;;;               +
;;;               |
;;;       +-------+-------+
;;;       |               |
;;;    +--+--+         +--+--+
;;;    1     |         |    nil
;;;       +--+--+   +--+--+
;;;      nil   nil  5     3
(defun redukuj (strom)
  (cond
    ((null strom) nil)
    ((atom strom) strom)
    ((null (redukuj (first strom))) (redukuj (second strom)))
    ((null (redukuj (second strom))) (redukuj (first strom)))
    (t (list (redukuj (first strom)) (redukuj (second strom))))
    ))



;;; Vyplnenie stromu - do zadaneho prazdneho stromu je potrebne vpisat
;;; informaciu k jednotlivym uzlom podla pravidiel:
;;;   - listove uzly budu cislovane postupnostou zacinajucou hodntou 1
;;;     smerom zlava doprava
;;;   - nelistovy uzol bude suctom hodnot svojich potomkov
;;;
;;;    (vypln '((nil (nil nil)) ((nil nil) nil (nil)))) -> 
;;;                       (28 (6 1 (5 2 3)) (22 (9 4 5) 6 (7 7)))
;;;
;;; Napoveda: - zapis (5 2 3) reprezentuje podstrom
;;;                5
;;;             +--+--+
;;;             2     3
;;;           - zapis ((nil (nil nil)) ((nil nil) nil (nil))) reprezentuje
;;;             strom
;; Vystup pre puchrika -> hold up toto nie je binarny strom -> i feel trickstered
;; Z listoveho stromu mam zrobit n-arny korenovy -> ale ujooooooooooooo 

;;;                +
;;;                |
;;;       +--------+---------+
;;;       |                  |
;;;    +--+--+         +-----+-----+
;;;   nil    |         |    nil    |
;;;       +--+--+   +--+--+        +
;;;      nil   nil nil   nil      nil


;;;                28
;;;                |
;;;       6--------+---------22
;;;       |                  |
;;;    +--+--5         9-----+-----7
;;;    1     |         |     6     |
;;;       +--+--+   +--+--+        +
;;;       2     3   4     5        7
(defun aux-pocet (strom) ;; ratam pocet listov
  (cond
    ((null strom) 1)
    (t (reduce #'+ (mapcar #'aux-pocet strom)))
    ))

;; nemam sajnu co robim len dzabem random prikazy bez srandy
(defun ocisluj (strom &optional (acc 0))
  (cond
    ((null strom) (1+ acc))
    ((null (cdr strom)) (list (ocisluj (car strom) acc)))
    (t  (cons
         (ocisluj (car strom) acc)
         (ocisluj (cdr strom) (+ acc (aux-pocet (car strom))))
         ))
    ))

(defun aux-suma (strom)
  (cond
    ((null strom) 0)
    ((atom strom) strom)
    (t (reduce #'+ (mapcar #'aux-suma strom)))
    ))

;;; '((nil (nil nil)) ((nil nil) nil (nil)))
;;; '((1 (2 3)) ((4 5) 6 (7)))
;;; '(28 (6 1 (5 2 3)) (22 (9 4 5) 6 (7 7)))

(defun vypln (strom)
  (vypln-wrapper (ocisluj strom))
  )

;; https://www.youtube.com/watch?v=Ho1LgF8ys-c -> celkom dobre zhrnuta praca so stromami
(defun vypln-wrapper (help)
  (cond
    ((atom help) help)
    (t (cons (aux-suma help) (mapcar #'vypln-wrapper help)))
    )
  )


;;; Zo stromu je potrebne vybrat vsetky uzly, ktore sa nachadzaju v
;;; zadanej pozadovanej hlbke.
;;;   - korenovy uzol je v hlbke 1
;;;   - hlbka narasta smerom k listovym uzlom
;;; Pozadovana hlbka je dana dvomi hodnotami - minimalnou a maximalnou
;;; hlbkou.
;;;
;;;    (vyber 2 3 '(a (d 1 (k 4 6)) (b (g 5 3) (o 2)))) ->
;;;                                               (1 k d g o b)
;;;
;;; Napoveda: zapis (a (d 1 (k 4 6)) (b (g 5 3) (o 2)))) reprezentuje
;;;           strom
;;;
;;;               +
;;;               a
;;;       +-------+-------+
;;;       d               b
;;;    +--+--+         +--+--+
;;;    1     k         g     o
;;;       +--+--+   +--+--+  +
;;;       4     6   5     3  2

(defun vyber (min max strom &optional (cur 1))
  (cond
    ((or (null strom) (> cur max)) nil)
    ((atom strom) 
     (if (>= cur min)
         (list strom)
         nil))
    (t (append 
        (vyber min max (second strom) (1+ cur))
        (vyber min max (third strom) (1+ cur))
        (if (>= cur min) 
            (list (car strom)) 
            nil)))))

;;; Transformacia aritmetickych vyrazov - Binarne operatory
;;; '+' (scitanie) a '*' (nasobenie) vytvaraju spolu so svojimi
;;; argumentami binarny strom. Ulohou je transformovat tento strom
;;; do tvaru ineho binarneho stromu, kde v ziadnej ceste od
;;; korenoveho uzla k listovemu uzlu nie je operator scitania
;;; vo vacsej hlbke ako operator nasobenia (teda pri vyhodnocovani
;;; vyrazu je mozne najprv urobit vsetky nasobenia a az nasledne
;;; vsetky sucty).
;;;
;;;    (transf '((a + (b * c)) * ((d + e) + f))) ->
;;;       (((a * d) + (a * e)) + (a * f)) +
;;;            ((((b * c) * d) + ((b * c) * e)) + ((b * c) * f))
;;;
;;; Napoveda: zapis ((a + (b * c)) * ((d + e) + f))) reprezentuje
;;;           binarny strom
;;;
;;;               *
;;;       -----------------
;;;       +               +
;;;    -------         -------
;;;    a     *         +     f
;;;       -------   -------
;;;       b     c   d     e
;;;           doporucujeme odvodit transformacne pravidla z prikladov
;;;              (a * (b + c))  -> ((a * b) + (a * c))
;;;              ((a + b) * c)  -> ((a * c) + (b * c))

(defun transf (strom)
  (cond
    ((atom strom) strom)
    ((eql (second strom) '+)
     (list
      (transf (first strom))
      '+
      (transf (third strom))))
    ((eql (second strom) '*) (aux-nasobenie (transf (first strom)) (transf (third strom))))
    ))

;; binarny strom ()
(defun aux-nasobenie (left right)
  (cond
    ((and (listp left) (eql '+ (second left)))
     (list (aux-nasobenie (first left) right) '+ (aux-nasobenie (third left) right)))
    ((and (listp right) (eql '+ (second right)))
     (list (aux-nasobenie left (first right)) '+ (aux-nasobenie left  (third right))))
    (t (list left '* right))
    ))
