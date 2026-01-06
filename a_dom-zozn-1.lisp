;;;; Poznamka:
;;;;   Nasledujuce priklady je mozne riesit pomocou rekurzie
;;;;   (mozne pocitanie smerom dnu aj von)
;;;;
;;;;   Alternativne riesenie je pouzit funkcie pre pracu so zoznamami
;;;;   bez rekurzivnej struktury.


;;; Pocetnost vyskytu prvku v zozname
;;;
;;;   (op-vyskyt 3 '(1 3 4 3 2)) -> 2
;;;   (op-vyskyt 5 '(1 3 4 3 2)) -> 0
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'

(defun op-vyskyt (num zoznam &optional (acc 0))
  (cond
    ((null zoznam) acc)
    ((= (car zoznam) num) (op-vyskyt num (cdr zoznam) (1+ acc)))
    (t (op-vyskyt num (cdr zoznam) acc))
    ))

;;; Sucet cisel v zozname
;;;
;;;   (op-sucet '(1 2 3 4)) -> 10
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'
;;;
;;; Alternativa: zoznam moze obsahovat aj neciselne prvky ktore sa do
;;;              suctu nezahrnaju

(defun op-sucet (zoznam &optional (acc 0))
  (cond
    ((null zoznam) acc)
    (t (op-sucet (cdr zoznam) (+ acc (car zoznam))))
    ))

(defun op-sucet-alt (zoznam &optional (acc 0))
  (cond
    ((null zoznam) acc)
    ((numberp (car zoznam))  (op-sucet-alt (cdr zoznam) (+ acc (car zoznam))))
    (t (op-sucet-alt (cdr zoznam) acc))
    ))

;;; Test pritomnosti prvku v zozname
;;;
;;;   (op-member 3 '(1 2 3 4)) -> (3 4)
;;;   (op-member 5 '(1 2 3 4)) -> nil
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'
(defun op-member (num zoznam)
  (cond
    ((not zoznam) nil)
    ((= num (car zoznam)) zoznam)
    (t (op-member num (cdr zoznam)))
    ))

;;; Pristup k n-temu prvku zoznamu
;;;
;;;   (op-nth 3 '(1 2 3 4 5)) -> 4
;;;   (op-nth 5 '(1 2 3))     -> nil
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'

(defun op-nth (num zoznam &optional (idx 0))
  (cond
    ((not zoznam) nil)
    ((= idx num) (car zoznam))
    (t (op-nth num (cdr zoznam) (1+ idx)))
    ))

;;; Obratenie poradia prvkov zoznamu
;;;
;;;   (op-reverse '(1 2 3 4)) -> (4 3 2 1)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car', 'cdr' a 'cons'

(defun op-reverse (zoznam &optional reversed)
  (cond
    ((null zoznam) reversed)
    (t (op-reverse (cdr zoznam) (cons (car zoznam) reversed)))))


;;; Spojenie dvoch zoznamov do jedneho zoznamu
;;;
;;;   (op-append '(1 2 3) '(4 5 6 7)) -> (1 2 3 4 5 6 7)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car', 'cdr' a 'cons'

(defun op-append (z1 z2)
  (cond
    ((null z1) z2)
    (t  (cons (car z1) (op-append (cdr z1) z2)))
    ))

(defun op-append+ (zoznam &optional (new))
  (cond
    ((null zoznam)
     (if (null new)
         nil
         (op-append+ new)
         ))
    ((listp (car zoznam)) (op-append+ (car zoznam) (cdr zoznam)))
    (t (cons (car zoznam) (op-append+ (cdr zoznam) new)))
    ))

;;; Spristupnenie posledneho prvku zoznamu
;;;
;;;   (op-last '(1 2 3 4)) -> 4
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car', 'cdr' a 'length'
;;;

(defun op-last (zoznam &optional (size (length zoznam )))
  (cond
   ( (= size 1) (car zoznam))
    (t (op-last (cdr zoznam) (1- size)))))

;;; Alternativa: bez pocitania dlzky zoznamu
;;;
;; EZ
(defun op-last (zoznam)
  (cond
    ((null (cdr zoznam)) (car zoznam))
    (t (op-last (cdr zoznam)))))

;;; Alternativa2: Spristupnenie zoznamu okrem posledneho prvku
;;;
;;;   (op-butlast '(1 2 3 4)) -> (1 2 3)

(defun op-butlast (zoznam)
  (op-reverse (cdr (op-reverse zoznam)))
  )


;;; Vlozenie prvku na n-tu poziciu
;;;
;;;   (op-vloz 0 2 '(1 3 5 7)) -> (1 3 0 5 7)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car', 'cdr' a 'cons'

(defun op-vloz (cislo ciel zoznam &optional (idx 0))
  (cond
    ((= ciel idx) (cons cislo zoznam))
    (t (cons (car zoznam) (op-vloz cislo ciel (cdr zoznam) (1+ idx)))) 
    ))


(defun op-vloz (cislo ciel zoznam &optional (idx 0))
  (cond
    ((= ciel idx) (cons cislo zoznam))
    (t (cons (car zoznam) (op-vloz cislo ciel (cdr zoznam) (1+ idx))))
    ))


;;; Vlozenie prvku do zoznamu za/pred vzor
;;;
;;;   (op-vlozLR 0 3 :za '(1 2 3 4 3)) -> (1 2 3 0 4 3)
;;;   (op-vlozLR 0 3 :pred '(1 2 3 4)) -> (1 2 0 3 4)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car', 'cdr' a 'cons'


(defun op-vlozLR (cislo vzor smer zoznam)
  (cond
    ((null zoznam) nil)
    ((= vzor (car zoznam))
     (if (eq smer :za)
         (cons (car zoznam) (cons cislo (cdr zoznam)))
         (cons cislo zoznam)))
    (t (cons (car zoznam) (op-vlozLR cislo vzor smer (cdr zoznam)))))
    )



(defun op-vlozLR (cislo vzor kam zoznam)
  (cond
    ((null zoznam) nil)
    ((= vzor (car zoznam))
     (if (eq kam :za )
         (cons (car zoznam) (cons cislo (cdr zoznam)))
         (cons cislo zoznam)))
    (t (cons (car zoznam) (op-vlozLR cislo vzor kam (cdr zoznam))) )
    )
  )

;;; https://youtu.be/L6tHMDaGgho?si=e9ZxtSPnmt72fELa snad pomoze
;;; Alternativa: - uvazovat vsetky vyskyty vzoru
(defun op-vlozLR (cislo vzor kam zoznam)
  (cond
    ((null zoznam) nil)
    ((= vzor (car zoznam))
     (if (eq kam :za )
         (cons (car zoznam) (cons cislo (cdr (cons (car zoznam) (op-vlozLR cislo vzor kam (cdr zoznam))))) )
         (cons cislo (cons (car zoznam) (op-vlozLR cislo vzor kam (cdr zoznam))))))
    (t (cons (car zoznam) (op-vlozLR cislo vzor kam (cdr zoznam))) )
    )
  )


(defun op-vlozLR (cislo vzor smer zoznam)
  (cond
    ((null zoznam) nil)
    ((= vzor (car zoznam))
     (if (eq smer :za)
         (cons (car zoznam) (cons cislo (cdr (cons (car zoznam) (op-vlozLR cislo vzor smer (cdr zoznam))))))
         (cons cislo (cons (car zoznam) (op-vlozLR cislo vzor smer (cdr zoznam))))))
    (t (cons (car zoznam) (op-vlozLR cislo vzor smer (cdr zoznam)))))
  )




;;;              - uvazovat iba urcity pocet prvych vyskytov
(defun op-vlozLR (cislo vzor kam zoznam vyskyty &optional (v_idx 0))
  (cond
    ((null zoznam) nil)
    ((= vzor (car zoznam)) (format t "~D <= ~D " v_idx vyskyty)
      (if (<= (1+ v_idx) vyskyty) (if (eq kam :za )
                 (cons (car zoznam) (cons cislo (cdr (cons (car zoznam) (op-vlozLR cislo vzor kam (cdr zoznam) vyskyty (1+ v_idx))))) )
                 (cons cislo (cons (car zoznam) (op-vlozLR cislo vzor kam (cdr zoznam) vyskyty (1+ v_idx)))))))
    (t (cons (car zoznam) (op-vlozLR cislo vzor kam (cdr zoznam) vyskyty v_idx)))
    )
  )

(defun op-vlozLR (cislo vzor smer zoznam vyskyty &optional (v_idx 0))
  (cond
    ((null zoznam) nil)
    ((and (= vzor (car zoznam)) (<= (1+ v_idx) vyskyty))
     (if (eq smer :za)
         (cons (car zoznam) (cons cislo (cdr (cons (car zoznam) (op-vlozLR cislo vzor smer (cdr zoznam) vyskyty (1+ v_idx))))))
         (cons cislo (cons (car zoznam) (op-vlozLR cislo vzor smer (cdr zoznam) vyskyty (1+ v_idx))))))
    (t (cons (car zoznam) (op-vlozLR cislo vzor smer (cdr zoznam) vyskyty v_idx))))
  )




;;;              - uvazovat iba parne vyskyty
(defun op-vlozLR (cislo vzor kam zoznam &optional (v_idx 0))
  (cond
    ((null zoznam) nil)
    ((= vzor (car zoznam))
     (if (evenp (1+ v_idx)) (if (eq kam :za )
                                (cons (car zoznam) (cons cislo (cdr (cons (car zoznam) (op-vlozLR cislo vzor kam (cdr zoznam) (1+ v_idx))))) )
                                (cons cislo (cons (car zoznam) (op-vlozLR cislo vzor kam (cdr zoznam) (1+ v_idx)))))
         (cons (car zoznam) (op-vlozLR cislo vzor kam (cdr zoznam) (1+ v_idx))))
     )
    (t (cons (car zoznam) (op-vlozLR cislo vzor kam (cdr zoznam) v_idx)))
    )
  )

(defun op-vlozLR (cislo vzor smer zoznam &optional (v_idx 0))
  (cond
    ((null zoznam) nil)
    ((and (= vzor (car zoznam)) (evenp (1+ v_idx)))
     (if (eq smer :za)
         (cons (car zoznam) (cons cislo (cdr (cons (car zoznam) (op-vlozLR cislo vzor smer (cdr zoznam) (1+ v_idx))))))
         (cons cislo (cons (car zoznam) (op-vlozLR cislo vzor smer (cdr zoznam) (1+ v_idx))))))
    ((= vzor (car zoznam)) (cons (car zoznam) (op-vlozLR cislo vzor smer (cdr zoznam) (1+ v_idx))))
    (t (cons (car zoznam) (op-vlozLR cislo vzor smer (cdr zoznam) v_idx))))
  )



(defun op-append (z1 z2)
  (cond
    ((null z1) z2)
    ((cons (car z1) (op-append (cdr z1) z2)))
    ))

;; bez akumulatora
(defun op-reverse (zoznam)
  (cond
    ((null zoznam) nil)
    (t (op-append (op-reverse (cdr zoznam)) (list (car zoznam)))
     )))


;; s akumulatorom pre deti
(defun op-reverse (zoznam &optional (acc))
  (cond
    ((null zoznam) acc)
    (t (op-reverse (cdr zoznam) (cons (car zoznam) acc))
       )))

;;; Redukcia viacnasobneho vyskytu prvkov v zozname
;;;
;;;   (op-redukcia '(1 2 1 4 2 3 5 1)) -> (1 2 4 3 5)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'

(defun in (num ls)
  (cond
    ((null ls) nil)
    ((eql num (car ls)) t)
    (t (in num (cdr ls)))
    ))


(defun op-redukcia (zoznam &optional (acc))
  (cond
    ((null zoznam) (reverse acc))
    ((in (car zoznam) acc) (op-redukcia (cdr zoznam) acc))
    (t (op-redukcia (cdr zoznam) (cons (car zoznam) acc)))
    ))


;;; Prienik dvoch zoznamov (vysledny zoznam bude obsahovat iba
;;; tie prvky, ktore sa nachadzaju v oboch zoznamoch)
;;;
;;;   (op-prienik '(1 2 3) '(2 3 4)) -> (2 3)

(defun in (element zoznam)
  (cond
    ((null zoznam) nil)
    ((eql element (car zoznam)) t)
    (t (in element (cdr zoznam)))
    ))

(defun op-prienik (z1 z2 &optional (pr))
  (cond
    ((null z1) (reverse pr))
    ((in (car z1) z2)  (op-prienik (cdr z1) z2 (cons (car z1) pr)))
    (t (op-prienik (cdr z1) z2))
    ))
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'
;;;
;;; Alternativa: - zjednotenie (prvky nachadzajuce sa aspon v
;;;                    jednom zozname)

(defun op-prienik2 (z1 z2)
  (cond
    ((null z1) z2)
    ((in (car z1) z2) (op-prienik2 (cdr z1) z2))
    (t (op-prienik2 (cdr z1) (cons (car z1) z2)))
    )
  )


;;; - xor (prvky nachadzajuce sa iba v jednom zozname)


(defun in (element zoznam)
  (cond
    ((null zoznam) nil)
    ((eql element (car zoznam)) t)
    (t (in element (cdr zoznam)))
    ))

(defun op-rozdiel (z1 z2 &optional (pr))
  (cond
    ((null z1) (reverse pr))
    ((not (in (car z1) z2))  (op-rozdiel (cdr z1) z2 (cons (car z1) pr)))
    (t (op-rozdiel (cdr z1) z2 pr))
    ))

(defun op-prienik3 (z1 z2)
  (append                 ; <--- DÔLEŽITÉ: Spojí výsledky
   (op-rozdiel z1 z2)    ; Vráti (1)
   (op-rozdiel z2 z1)))  ; Vráti (4)

;;;

;;; Opakovanie prvkov zoznamu
;;;
;;;   (op-opakovanie 3 '(1 2 3)) -> (1 1 1 2 2 2 3 3 3)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'

(defun aux-pridavaj (cislo zoznam pocet)
  (cond
    ((zerop pocet) zoznam)
    (t (aux-pridavaj cislo (cons cislo zoznam) (1- pocet)))
    )
  ) 

(defun op-opakovanie (pocet zoznam &optional (klon))
  (cond
    ((null zoznam) (reverse klon))
    (t (op-opakovanie pocet (cdr zoznam) (aux-pridavaj (car zoznam) klon pocet)) )
    )
  )
;;;
;;; Alternativa:
;;;   (op-opakovanie2 3 '(1 2 3)) -> (1 2 3 1 2 3 1 2 3)


(defun op-reverse (zoznam &optional (reversed))
  (cond
    ((null zoznam) reversed)
    (t (op-reverse (cdr zoznam) (cons (car zoznam) reversed)))))

(defun op-copy-list (zoznam &optional (new))
  (cond
    ((null zoznam) (op-reverse new))
    (t (op-copy-list (cdr zoznam) (cons (car zoznam) new)))))

(defun op-append (z1 z2)
  (cond
    ((null z1) z2)
    (t (cons (car z1) (op-append (cdr z1) z2)))
    )
  )

(defun op-opakovanie2 (pocet zoznam &optional (klon))
  (cond
    ((zerop pocet) klon)
    (t (op-opakovanie2 (1- pocet) zoznam (op-append (op-copy-list zoznam) klon)))
    )
  )

;;; Rozdelenie zoznamu na podzoznamy podla vzoru
;;;
;;;   (op-rozdel 3 '(1 2 3 4 3 2 1)) -> ((1 2) (4) (2 1))
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'



(defun op-rozdel (delic zoznam &optional (new))
  (cond
    ((null zoznam) (list (reverse new)))
    ((= delic (car zoznam)) (cons (reverse new) (op-rozdel delic (cdr zoznam) nil)))
    (t (op-rozdel delic (cdr zoznam) (cons (car zoznam) new)))
    ))



(defun op-rozdel (vzor zoznam &optional new)
  (cond
    ((null zoznam) (list (reverse new)))
    ((eql (car zoznam) vzor) (cons (reverse new) (op-rozdel vzor (cdr zoznam) nil)))
    (t (op-rozdel vzor (cdr zoznam) (cons (car zoznam) new)))))

;;; Kompresia zoznamu
;;;
;;;   (op-kompres '(1 1 2 0 0 0 1 2 2)) -> ((2 1) 2 (3 0) 1 (2 2))
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' 'cdr'
;;;
(defun op-kompres (zoznam &optional (new) (acc 1))
  (cond
    ((null zoznam) (reverse new))
    ((null (second zoznam)) (op-kompres (cdr zoznam) (if (< 1 acc)
                                               (cons (list acc (car zoznam)) new)
                                               (cons (car zoznam) new))
                                        1 ))
    ((and (not (= (first zoznam) (second zoznam))) (< 1 acc)) (op-kompres (cdr zoznam) (cons (list acc (car zoznam)) new)))
    ((= (first zoznam) (second zoznam)) (op-kompres (cdr zoznam) new (1+ acc)))
    (t (op-kompres (cdr zoznam) (cons (car zoznam) new)))
    ))



(defun op-kompres (zoznam &optional (acc 1) (new))
  (cond
    ((null zoznam) (reverse new))
    ((null (second zoznam)) (op-kompres (cdr zoznam) 1 (if (> acc 1)
                                                           (cons (list acc (car zoznam)) new)
                                                           (cons (car zoznam) new))) )
    ((and (not (= (first zoznam) (second zoznam))) (> acc 1)) (op-kompres (cdr zoznam) 1 (cons (list acc (car zoznam)) new)))
    ((= (first zoznam) (second zoznam)) (op-kompres (cdr zoznam) (1+ acc) new))
    (t (op-kompres (cdr zoznam) 1 (cons (car zoznam) new)))
    )
  )
;;; Alternativa: Dekompresia zoznamu (reverzna operacia)

(defun op-dekompres (zoznam &optional (new))
  (cond
    ((null zoznam) (reverse new))
    ((listp (car zoznam)) (op-dekompres (cdr zoznam) (append (make-list (first (car zoznam)) :initial-element (second (car zoznam)) ) new)))
    (t (op-dekompres (cdr zoznam) (cons (car zoznam) new))))
  )

;;; Zotriedenie zoznamu
;;;
;;;   (op-sort '(3 2 4 1 5)) -> (1 2 3 4 5)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'
;;;
;;; Algoritmus:
;;;   Zo zonamu sa vyberie prvok P a zoznam sa rozdeli na dva
;;;   zoznamy: M (prvky mensie ako P) a V (prvky vacsie ako P).
;;;   M a V sa zotriedia a vysledny zoznam vznikne spojenim 
;;;   M + P + V

(defun aux-vacsie (prvok zoznam)
  (cond
    ((null zoznam) nil)
    ((> (car zoznam) prvok) (cons (car zoznam) (aux-vacsie prvok (cdr zoznam))))
    (t (aux-vacsie prvok (cdr zoznam)))
    )
  )
(defun aux-mensie (prvok zoznam)
  (cond
    ((null zoznam) nil)
    ((< (car zoznam) prvok) (cons (car zoznam) (aux-mensie prvok (cdr zoznam))))
    (t (aux-mensie prvok (cdr zoznam)))
    )
  )

(defun op-sort (zoznam)
  (cond
    ((null zoznam) nil)
    (t (append
        (op-sort (aux-mensie (car zoznam) (cdr zoznam)))
        (list (car zoznam))
        (op-sort (aux-vacsie (car zoznam) (cdr zoznam)))
        )))
  )



(defun op-sort (zoznam)
  (if (null zoznam)
      nil
      (append (op-sort (remove-if #'(lambda (x) (> x (car zoznam))) (cdr zoznam))) (list (car zoznam)) (op-sort (remove-if #'(lambda (x) (< x (car zoznam))) (cdr zoznam))))
      )
  )

;;; Transpozicia matice zadanej ako zoznam riadkov
;;;
;;;    (transp '((1 2 3)(4 5 6)(7 8 9))) -> ((1 4 7)(2 5 8)(3 6 9))
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'


(defun aux-riadok (matica idx &optional (riadok))
  (cond
    ((null matica) (reverse riadok))
    (t (aux-riadok
        (cdr matica)
        idx
        (cons (nth idx (car matica)) riadok)))
    )
  )




(defun aux-riadok (matica idx &optional (riadok))
  (cond
    ((null matica) (reverse riadok))
    (t (aux-riadok (cdr matica) idx (cons (nth idx (car matica)) riadok)))
    )
  )

(defun transp (matica &optional (stlpce  (length (car matica))) (tmatrix))
  (cond
    ((zerop stlpce) tmatrix)
    (t (transp matica (1- stlpce) (cons (aux-riadok matica (1- stlpce)) tmatrix)))
    )
  )

;;; Obojsmerny pohyb v zozname - prvy argument udava startovaciu 
;;; poziciu v zozname, selektovany prvok v zozname udava dalsi 
;;; posun v zozname
;;;
;;;   (vyber 3 '(2 9 4 1 6 3 2 7 6 5 2)) -> (1 6 2)
;;;   (vyber 0 '(2 9 4 1 6 3 2 7 6 5 2)) -> (2 4 2 6)
;;;   (vyber 2 '(2 9 4 1 6 -3 -2 7 6 5 -2)) -> (4 -2 6 -2 6)


 
;;; Napoveda: zoznam si reprezentujte ako trojprvkovu strukturu
;;;           (a b c), kde
;;;             - b je aktualny prvok 
;;;             - a je zoznam prvkov nachadzajucich sa pred
;;;                 aktualnym prvkom (v reverzovanom pradi)
;;;             - c je zoznam prvkov nachadzajucich sa za
;;;                 aktualnym prvkom
;;;           riesenie zalozte na pouziti 'car' a 'cdr'


(defun posun (pred aktualny po idx new)
  (cond
    ((null aktualny) (reverse new))
    ((zerop idx) (posun pred aktualny po aktualny (cons aktualny new)))
    ((plusp idx) (posun (cons aktualny pred) (car po) (cdr po) (1- idx) new)); kladny idx
    (t (posun (cdr pred) (car pred) (cons aktualny po) (1+ idx) new)); zaporny idx
    ))


(defun vyber (idx zoznam)
  (posun '() (car zoznam) (cdr zoznam) idx '())
  )

(defun op-nth (idx zoznam)
  (cond
    ((zerop idx) (car zoznam))
    (t (op-nth (1- idx) (cdr zoznam)))
    )
  )

(defun vyber (idx zoznam  &optional (new))
  (cond
    ((null (op-nth idx zoznam)) (reverse new))
    (t (vyber (+ idx (op-nth idx zoznam)) zoznam (cons (op-nth idx zoznam) new)))
    )
  )

;;; Rozdelenie zoznamu na podzoznamy podla zotriedenia, kde 
;;; v kazdom podzozname cisla musia byt zotriedene vzostupne 
;;; alebo zostupne. Susedne podzoznamy sa prekryvaju v jednom
;;; prvku (posledny prvok podzoznamu je zaroven prvym prvkom 
;;; nasledujuceho podzoznamu) 
;;;
;;;   (rozdel '(1 2 3 -1 -3 6 2 9)) -> 
;;;                       ((1 2 3)(3 -1-3)(-3 6)(6 2)(2 9))
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a 'cdr'

(defun rozdel (zoznam &optional (result) (cast) (smer 0))
  (cond
    ((null zoznam) (reverse (cons (reverse cast) result)))
    ((null cast) (rozdel (cdr zoznam) result (list (car zoznam)) 0)) ; nemam nic v casti
    ((zerop smer) (rozdel (cdr zoznam) result (cons (car zoznam) cast)
                          (if (< (car cast) (car zoznam))
                              1 ;; cast/vysek < ako druhy prvok v zozname = stupam/vzostup
                              -1 ;; mensia = klesanie
                              )));; tu setnem smer dajak asi cez ifko
    ((and (= 1 smer) (< (car cast) (car zoznam))) (rozdel (cdr zoznam) result (cons (car zoznam) cast) smer)); hoooooooore
    ((and (= -1 smer) (> (car cast) (car zoznam))) (rozdel (cdr zoznam) result (cons (car zoznam) cast) smer)); doooooooole

    ;; cize co musim zrobic asi:
    ;; dat mnozinu/vysek/cast do result. Vynulovat sicko. Dat bacha bo musim dac predchadzajuci cize ziaden (cdr zoznam)
    (t (rozdel zoznam (cons (reverse cast) result) (list (car cast)) 0))
    ))

;;; Vyhladenie zoznamu s lubovolnym vnorenim podzoznamov 
;;; do tvaru zoznamu hodnot
;;;
;;;   (vyhlad '((1) 2 ((3 (4) ((5) 6)) 7))) -> (1 2 3 4 5 6 7)
;;;
;;; Napoveda: riesenie zalozte na pouziti 'car' a cdr'

(defun vyhlad (zoznam &optional (new))
  (cond
    ((null zoznam) new)
    (t (vyhlad (cdr zoznam)
               (if (listp (car zoznam))
                   (vyhlad (car zoznam) new)
                   (append new (list (car zoznam)))
                   )
               ))))

;;; Vytvarajte alternativne podoby zoznamu, kde vzdy pouzijete
;;; iba jednu z povolenych alternativ
;;;
;;;   (alternativy '(a (b1 b2) c (d1 d2 d3) f) ->
;;;          ((a b1 c d1 f) (a b1 c d2 f) (a b1 c d3 f)
;;;           (a b2 c d1 f) (a b2 c d2 f) (a b2 c d3 f))
;;;
;;; Napoveda: nesnazte sa ulohu vyriesit definovanim iba jednej
;;;           funkcie
;;;           riesenie zalozte na pouziti 'car' a 'cdr'


(defun alternativy (zoznam)
  (cond
    ((null zoznam) (list nil)) ; konec
    ((listp (car zoznam)) (aux-spoj-moznosti (car zoznam) (alternativy (cdr zoznam)))) ; (ak v zozname mam zoznam alternativ)
    (t (aux-spoj-moznosti (list (car zoznam)) (alternativy (cdr zoznam)))) ; (som v zozname kde su len prvky)
    ))

(defun aux-spoj-prvok (prvok minizoznam)
  (cond
    ((null minizoznam) nil)
    (t (cons (cons prvok (car minizoznam))
             (aux-spoj-prvok prvok (cdr minizoznam))))
    ))

(defun aux-spoj-moznosti (moznosti hotove)
  (cond
    ((null moznosti) nil)
    (t (append
        (aux-spoj-prvok (car moznosti) hotove)
        (aux-spoj-moznosti (cdr moznosti) hotove)
        ))
    ))

;; len tak
(defun aux-pocet-alternativ (zoznam &optional (sum 1))
  (cond
    ((null zoznam) sum)
    ((listp (car zoznam)) (aux-pocet-alternativ (cdr zoznam) (* sum (length (car zoznam)))))
    (t (aux-pocet-alternativ (cdr zoznam) sum))
    ))

(defun alternativy (zoznam)
  (cond
    ((null zoznam) (list nil))
    ((listp (car zoznam)) (aux-spoj (car zoznam) (alternativy (cdr zoznam))))
    (t (aux-spoj (list (car zoznam)) (alternativy (cdr zoznam))))))

(defun aux-spoj (moznosti hotove-kombinacie)
  (mapcan #'(lambda (jeden-prvok) (mapcar #'(lambda (kombinacia) (cons jeden-prvok kombinacia)) hotove-kombinacie)) moznosti))
