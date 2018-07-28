#lang racket
(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================


(define init-database
  (λ ()
    (list null '() )))


(define create-table 
  (λ (table columns-name)
    (if  (list? (car columns-name) )
         (apply list table columns-name)
         (apply list table (map list columns-name)))))

(define get-name
  (λ (table)
    (car table)))

(define get-columns 
  (λ (table)
    (map (λ (list) (car list) ) (cdr table) )))

(define get-tables
  (λ (db)
    (cadr db)))

(define get-table 
  (λ (db table-name)
     (car (filter (λ(list)
                    (equal?  (car list) table-name)) (cadr db))) ))

(define add-table 
  (λ (db table)
     (list (first db) (apply cons table (cdr db)))))


(define remove-table 
  (λ (db table-name)
    (list "db" (remove (get-table db table-name) (cadr db)))))


;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================

(define (make-database db table-list)
  (list db table-list))

(define db 
  (make-database "db" (list
            (create-table "Studenți" '( ("Număr matricol" 123 124 125 126 )
                            ("Nume" "Ionescu" "Popescu" "Popa" "Georgescu" )
                            ("Prenume" "Gigel" "Maria" "Ionel" "Ioana" )
                            ("Grupă"   "321CA" "321CB" "321CC" "321CD" )
                            ("Medie"   9.82 9.91 9.99 9.87) )   )
            
            (create-table "Cursuri" '( ("Anul" "I" "II" "III" "IV" "I" "III" )
                            ("Semestru"      "I" "II" "I" "I" "II" "II" )
                            ("Disciplină"    "Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date")
                            ("Număr credite"  5 6 5 6 5 5 )
                            ("Număr teme"     2 3 3 3 3 0) ) )   )))


;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================


(define (add-elem-to-col element coloane)
  (filter-not null? (map (λ (coloana)
           (if (equal?  (car coloana) (car element))
               (append coloana (cdr element) )
                '())
           ) coloane)))


(define (myFind col_name record)
  (map (λ (element)
    (if (equal? col_name (car element))
        #t
        #f)) record))

; verifica daca intr-o lista de booleni exista minim un true
(define (oneMatch list)
  (if (equal? 1 (length list))
      (car list)
      (or (car list) (oneMatch (cdr list)))))  

(define (findRecordElement col_name record )
  (if (equal? col_name (caar record))
       (cdar record)
       (findRecordElement col_name (cdr record))))




(define insert
  (λ (db table-name record)
    (add-table (remove-table db table-name)
      (cons table-name
       (map (λ (col)
           (if (oneMatch (myFind (car col) record))
               (flatten (append col (findRecordElement (car col) record)))
               (flatten (append col  '(null)))))
         (cdr(get-table db table-name)))))
    ))


;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================

(define (select-col name cols)
   (filter-not null?
       (map (λ(col)
         (if (equal? name (car col))
             (cdr col)
             '())) cols)))

(define simple-select
  (λ (db table-name columns)
   (map car (filter-not null? (let loop ([cols (cdr (get-table db table-name))]
                        [columns columns])
                        (if (null? columns)
                            '()
                            (cons (select-col (car columns) cols ) (loop  cols (cdr columns) ))))))))

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================

(define conditions
 (list (list >= "Medie" 9.9)) )


(define (min list)
  (car (sort list <)))

(define (max list)
  (car (sort list >)))


(define (count list)
  (length (remove-duplicates list)))

(define (sum list)
  (let aduna([list list]
             [suma 0])
        (if (equal? 1 (length list))
            (+ suma (car list))
            (aduna (cdr list) (+ suma (car list) )))))

(define (avg list)
   (/ (sum list) (length list) ))

(define (sort-asc list )
   (if (number? (car list))
       (sort list <)
       (sort list string<?)))

(define (sort-desc list )
   (if (number? (car list))
       (sort list >)
       (sort list string>?)))

;imi determina ce operatie trebuie sa aplic pe coloana
(define (det-operation var)
  (cond
   [ (equal? 'min var) min ]
   [ (equal? 'max var) max ]
   [ (equal? 'sum var) sum ]
   [ (equal? 'avg var) avg ]
   [ (equal? 'count var) count ]
   [ (equal? 'sort-asc var) sort-asc ]
   [ (equal? 'sort-desc var) sort-desc ]))

(define (intersection-list l1 l2)
   (if (null? l2)
        '()
       (if (null? l1)
           '()
            (if (member (car l1) l2)
                (cons (car l1) (intersection-list (cdr l1) (remove (car l1) l2) ) )
                (intersection-list (cdr l1) (remove (car l1) l2))))))


(define (merge-all-indexes l)
        (if (equal? 2  (length l))
             (intersection-list (car l) (cadr l))
             (merge-all-indexes  (cons (intersection-list (car l) (cadr l)) (cddr l) ) ) ))


(define l
  (list (list 1 2 3 4 5)  (list 23 4 12 56 1 5) (list 67 12 3 1 4 9)))


(define (get-indexes db table-name conditions)
  (map (λ(condition)
         (indexes-where
          (car (simple-select db table-name (list (cadr condition))))
          (λ(arg)
            (if ((car condition) arg (caddr condition)) #t #f)) )) conditions))


(define (filtered-indexes db table-name conditions)
     (if (equal? 1 (length (get-indexes db table-name conditions)))
          (car (get-indexes db table-name conditions))
          (merge-all-indexes (get-indexes db table-name conditions))))


(define (getElemByIndex indexList list)
  (map (λ(index)
         (list-ref list index)) 
       indexList))

 
(define (apply-display db table-name coloane index-list )
  (map (λ(coloana)
         (if (pair? coloana)
             ( (det-operation  (car coloana)) (getElemByIndex  index-list (car(simple-select db table-name (list(cdr coloana)) )) ) )
             (getElemByIndex  index-list (car (simple-select db table-name (list coloana)))))) coloane))


(define (listTo a b)
  (if (equal? a b)
      (list a )
      (cons a (listTo (+ a 1) b) )))

(define select
  (λ (db table-name columns conditions)
     (if (null? conditions)
     (apply-display db table-name columns (listTo 0 (- (length (car (simple-select db table-name (get-columns (get-table db table-name))))) 1)) )    
     (apply-display db table-name columns (filtered-indexes db table-name conditions) )) ))



;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================


(define values
   (list (cons "Număr credite" 20) (cons "Număr teme" 7) ))

(define (myReplace l index value)
   (if (equal? 0 index)
       (cons value (cdr l))
       (cons (car l) (myReplace (cdr l) (sub1 index) value) )))



(define (change-values-at-indexes col index-list val)
     (if (equal? 0 (length index-list))
         col
        (change-values-at-indexes (myReplace col (car index-list) val )  (cdr index-list) val)))


(define (update-table table values index-list)
  (cons (car table)(map  (λ(col)
          (if (oneMatch (myFind (car col) values))
              (change-values-at-indexes col (map add1 index-list) (findRecordElement (car col) values))
               col)) (cdr table))))


(define update
  (λ (db table-name values conditions)
     (if (null? conditions)
         (add-table  (remove-table db table-name) (list table-name '() ) )
         (add-table  (remove-table db table-name) (update-table (get-table db table-name) values (filtered-indexes db table-name conditions)) ) )))


;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================

(define (remove-elem-at-index l index)
   (if (equal? index 0)
       (cdr l)
       (cons (car l)(remove-elem-at-index (cdr l) (sub1 index)))))


(define (remove-elem-from-col col index-list)
   (if (null? index-list)
        col
        (remove-elem-from-col (remove-elem-at-index  col (car  index-list)) (map sub1 (cdr index-list)) )))

(define (remove-elem-from-all-col table index-list )
       (cons (car table) (map  (λ (col)
                                 (remove-elem-from-col col (map add1 index-list))) (cdr table))))


(define delete
  (λ (db table-name conditions)
     (if (null? conditions)
         (add-table  (remove-table db table-name) (list table-name '() ) )
         (add-table  (remove-table db table-name) (remove-elem-from-all-col (get-table db table-name) (filtered-indexes db table-name conditions)) ) )))


;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================
(define natural-join
  (λ (db tables columns conditions)
    'your-code-here))





