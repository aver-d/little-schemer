#lang racket

(define (puts . args)
  (display
    (apply
      string-append
      (add-between (map print-to-string args) " ")))
  (display "\n"))

(define (print-to-string arg) (format "~v" arg))


(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond 
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member? 
  (lambda (a lat)
    (cond 
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))


(define rember
  (lambda (a lat)
    (cond 
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat) (rember a (cdr lat)))))))))

(define firsts 
  (lambda (l)
    (cond
      ((null? l) '())
      (else      (cons (car (car l)) (firsts (cdr l)))))))

(define insertR-1 
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons (car lat) (cons new (cdr lat))))
      (else                (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL-1
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else                (cons (car lat) (insertL new old (cdr lat)))))))
  
 (define subst
   (lambda (new old lat)
     (cond 
       ((null? lat) '())
       ((eq? old (car lat)) (cons new (cdr lat)))
       (else                (cons (car lat) (subst new old (cdr lat)))))))
 
 
 (define subst2
   (lambda (new o1 o2 lat)
     (cond
       ((null? lat) '())
       ((or (eq? o1 (car lat)) (eq? o2 (car lat))) 
            (cons new (cdr lat)))
       (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))
   
 
 (define multirember
  (lambda (a lat)
    (cond 
      ((null? lat)       '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else              (cons (car lat) (multirember a (cdr lat)))))))

     
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
        (cond 
          ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
          (else                (cons (car lat) (multiinsertR new old (cdr lat)))))))))
            

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond 
         ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
         (else                (cons (car lat) (multiinsertL new old (cdr lat)))))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond 
         ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
         (else                (cons (car lat) (multisubst new old (cdr lat)))))))))
  
  
(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))
  
 
(define plus
  (lambda (a b)   
    (cond
      ((zero? b) a)
      (else (plus (add1 a) (sub1 b))))))

(define sub 
  (lambda (a b)   
    (cond
      ((zero? b) a)
      (else (sub (sub1 a) (sub1 b))))))
  
  
(define addtup
  (lambda (tup)
    (cond 
      ((null? tup) 0)
      (else (plus (car tup) (addtup (cdr tup)))))))

(define mul
  (lambda (a b)
    (cond 
      ((zero? b) 0)
      (else (plus a (mul a (sub1 b))))))) 

(define tup+
  (lambda (tup1 tup2)
    (cond ((null? tup1) tup2)
          ((null? tup2) tup1)
         ;((and (null? tup1) (null? tup2)) '())
          (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
  
  
(define greaterthan 
  (lambda (a b)
    (cond
      ((zero? a) #f)
      ((zero? b) #t)     
      (else (greaterthan (sub1 a) (sub1 b))))))

(define lessthan 
  (lambda (a b)
    (cond
      ((zero? b) #f)
      ((zero? a) #t)     
      (else (lessthan (sub1 a) (sub1 b))))))

(define num-eq 
  (lambda (a b)
    (cond 
      ((< a b) #f)
      ((> b a) #f)
      (else #t))))

(define pow
  (lambda (n k)
    (cond 
      ((zero? k) 1)
      (else (mul n (pow n (sub1 k)))))))

(define div
  (lambda (n m)
    (cond 
      ((< n m) 0)
      (else (add1 (div (sub n m) m))))))

(define length*
  (lambda (lat)
    (cond 
      ((null? lat) 0)
      (else (add1 (length* (cdr lat)))))))
          
    
(define pick
  (lambda (n lat)
    (cond 
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
    
(define take*
  (lambda (n lat)
    (cond 
      ((zero? n) '())
      (else (cons (car lat) (take* (sub1 n) (cdr lat)))))))
    
 (define rempick
   (lambda (n lat)
     (cond 
       ((zero? (sub1 n)) (cdr lat))
       (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
       
 
 (define no-nums
   (lambda (lat)
     (cond 
       ((null? lat) '())
       (else (cond
               ((number? (car lat)) (no-nums (cdr lat)))
               (else (cons (car lat) (no-nums (cdr lat)))))))))
 
(define all-nums
  (lambda (lat)
    (cond 
      ((null? lat) '())
      (else (cond
              ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))
    
(define eqan?
  (lambda (a1 a2)
    (cond 
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))
  
(define occur
  (lambda (a lat)
    (cond 
      ((null? lat) 0)
      (else 
        (cond 
          ((eq? a (car lat)) (add1 (occur a (cdr lat))))
          (else (occur a (cdr lat))))))))
    
(define one?
  (lambda (n)
    (= n 1)))
    
(define rempick2
  (lambda (n lat)
    (cond 
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
    
(define rember*
  (lambda (a l)
    (cond 
      ((null? l) '())
      ((atom? (car l))
         (cond 
           ((eq? a (car l)) (rember* a (cdr l)))
           (else (cons (car l) (rember* a (cdr l))))))        
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond 
      ((null? l) '())
      ((atom? (car l))
         (cond 
           ((eq? old (car l)) (cons old (cons new (cdr l))))
           (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l) (insertR* new old (cdr l))))))))
    
    
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
         (cond 
           ((eq? a (car l)) (add1 (occur* a (cdr l))))
           (else (occur* a (cdr l)))))
      (else (plus (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond 
      ((null? l) '())
      ((atom? (car l))
         (cond 
           ((eq? old (car l)) (cons new (subst* new old (cdr l))))
           (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l) (insertR* new old (cdr l))))))))


(define insertL*
  (lambda (new old l)
    (cond 
      ((null? l) '())
      ((atom? (car l))
         (cond 
           ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
           (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l) (insertL* new old (cdr l))))))))
    
;(define member? 
;  (lambda (a lat)
;    (cond 
;      ((null? lat) #f)
;      (else (or (eq? (car lat) a)
;                (member? a (cdr lat)))))))

(define member*
  (lambda (a l)
    (cond 
      ((null? l) 
         #f)
      ((atom? (car l)) 
        (or (eq? (car l) a) (member* a (cdr l))))
      (else 
        (or (member* a (car l)) (member* a (cdr l)))))))
         
(define leftmost 
  (lambda (l)
    (cond 
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))
      

(define eqlist? 
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) 
         #t)
      ((or (null? l1) (null? l2))  
         #f)
      ((and (null? l1) (atom? l2))
         #f)     
      ((and (null? l2) (atom? l1))
         #f)
      ((and (atom? (car l1)) (atom? (car l2)))
        (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)))
         #f)
      (else 
        (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))
      
(define equal?
  (lambda (s1 s2)
    (cond 
      ((and (atom? s1) (atom? s2))
         (eqan? s1 s2))
      ((or (atom? s1) (atom? s2))
          #f)             
      (else 
        (eqlist? s1 s2)))))
      
 
(define rember2
  (lambda (s l)
    (cond 
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember2 s (cdr l)))))))
     
      
(define numbered? 
  (lambda (aexp)
    (cond 
      ((atom? aexp) 
         (number? aexp))
      (else
         (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))
      
;(define value-1
;  (lambda (nexp)
;    (cond
;      ((atom? nexp) nexp)
;      ((eq? (car (cdr nexp)) 'plus)
;         (plus (value (car nexp)) (value (car (cdr (cdr nexp))))))
;      ((eq? (car (cdr nexp)) 'mul)
;         (mul (value (car nexp)) (value (car (cdr (cdr nexp))))))
;      (else
;         (pow (value (car nexp)) (value (car (cdr (cdr nexp)))))))))
    
    
(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

;(define value
;  (lambda (nexp)
;    (cond 
;      ((atom? nexp) nexp)
;      ((eq? (operator nexp) 'plus)
;         (plus (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
;      ((eq? (operator nexp) 'mul)
;         (mul (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
;      (else
;         (pow (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))
;      
      
(define (sero? n)
  (null? n))
 
(define (edd1 n)
  (cons '() n))

(define (zub1 n)
  (cdr n))
      
(define (plus$ n1 n2)
  (cond 
    ((sero? n2) n1)
    (else (plus$ (edd1 n1) (zub1 n2)))))
      

(define (set?-1 lat)
  (cond
    ((null? lat) #t)
    (else (cond
            ((member? (car lat) (cdr lat)) #f)
            (else (set? (cdr lat)))))))
      
      
(define (set?-2 lat)
  (cond
    ((null? lat) #t)
    (else (and (not (member? (car lat) (cdr lat)))
                (set?-2 (cdr lat))))))

(define (set? lat)
  (cond 
    ((null? lat) #t)
    ((member? (car lat) (cdr lat)) #f)
    (else (set? (cdr lat)))))


(define (makeset-1 lat)
  (cond 
    ((null? lat)   '())
    ((member? (car lat) (cdr lat)) 
       (makeset (cdr lat)))
    (else 
       (cons (car lat) (makeset (cdr lat))))))
     
(define (makeset lat)
  (cond
    ((null? lat) '())
    (else (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))))
      
(define (subset-1? set1 set2)
  (cond 
    ((null? set1) #t)
    ((member? (car set1) set2) (subset-1? (cdr set1) set2))
    (else #f)))

(define (subset? set1 set2)
  (cond 
    ((null? set1) #t)
    (else (and (member? (car set1) set2) (subset? (cdr set1) set2)))))

(define (eqset? set1 set2)
  (and (subset? set1 set2) (subset? set2 set1)))

(define (intersect? set1 set2)
  (cond
    ((null? set1) #f)
    (else (or (member? (car set1) set2) (intersect? (cdr set1) set2)))))
     
(define (intersect set1 set2)
  (cond 
    ((null? set1) '())
    ((member? (car set1) set2) 
       (cons (car set1) (intersect (cdr set1) set2)))
    (else
       (intersect (cdr set1) set2))))
  
(define (union set1 set2)
  (cond 
    ((null? set1) set2)
    ((member? (car set1) set2)
       (union (cdr set1) set2))
    (else 
       (union (cdr set1) (cons (car set1) set2)))))
       

(define (intersectall l-set)
  (cond 
    ((null? (cdr l-set))
       (car l-set))
    (else 
       (intersect (car l-set) (intersectall (cdr l-set))))))
  
(define (a-pair? x)
  (cond 
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    ((null? (cdr (cdr x))) #t)
    (else #f)))
  
(define (fun? rel)
  (set? (firsts rel)))
      
 
(define (revrel-1 rel)
  (cond 
    ((null? rel) '())
    (else (cons (cons (car (cdr (car rel))) (cons (car (car rel)) '()))
                (revrel-1 (cdr rel))))))
    
(define (build s1 s2)
  (cons s1 (cons s2 '()))) 
      
(define (revrel rel)
  (cond
    ((null? rel) '())
    (else (cons (build (second (car rel)) (first (car rel)))
                (revrel (cdr rel))))))
       
(define (revpair pair)
  (build (second pair) (first pair)))
         

(define (seconds l)
  (cond 
    ((null? l) '())
    ((cons (second (car l)) (seconds (cdr l))))))

(define (fullfun? fun)
  (set? (seconds fun)))

(define (one-to-one? fun)
  (fun? (revrel fun)))
      

(define (rember-f-1 test a l)
  (cond 
    ((null? l) '())
    ((test a (car l)) 
       (rember-f test a (cdr l)))
    (else 
       (cons (car l) (rember-f test a (cdr l))))))


(define (eq?-c a)
  (lambda (x) 
    (eq? a x)))

(define (rember-f test)
  (lambda (a l)
    (cond 
      ((null? l) '())
      ((test a (car l))
          (cdr l))
      (else 
         (cons (car l) ((rember-f test) a (cdr l)))))))

(define (insertL-f test?)
  (lambda (new old l)
    (cond 
      ((null? l) '())
      ((test? old (car l))
         (cons new l))
      (else
         (cons (car l) ((insertL-f test?) new old (cdr l)))))))

(define (insertR-f test?)
  (lambda (new old l)
    (cond 
      ((null? l) '())
      ((test? old (car l))
         (cons old (cons new (cdr l))))
      (else
         (cons (car l) ((insertR-f test?) new old (cdr l)))))))

(define (seqL new old l)
  (cons new (cons old l)))

(define (seqR new old l)
  (cons old (cons new l)))

(define (insert-g seq)
  (lambda (new old l)
    (cond 
      [(null? l) '()]
      [(eq? (car l) old) 
         (seq new old (cdr l))]
      [else
         (cons (car l) ((insert-g seq) new old (cdr l)))])))
 
(define insertL (insert-g seqL))
(define insertR (insert-g seqR))


(define (atom-to-function x)
  (cond 
    [(eq? x 'plus) plus]
    [(eq? x 'mul) mul]
    [else pow]))
    

(define (multirember-f test?)
  (lambda (a lat)
    (cond 
      [(null? lat) '()]
      [(test? (car lat) a) 
         ((multirember-f test?) a (cdr lat))]
      [else
         (cons (car lat) ((multirember-f test?) a (cdr lat)))])))

(define multirember-eq? (multirember-f eq?))




(define (multirember&co a lat col)
  (cond 
    [(null? lat)
       (col '() '())]
    [(eq? (car lat) a)
       (multirember&co a (cdr lat) (lambda (newlat seen)
                                     (puts "eq" newlat seen)
                                     (col newlat (cons (car lat) seen))))]
    [else 
       (multirember&co a (cdr lat) (lambda (newlat seen)
                                     (puts "not-eq" newlat seen)
                                     (col (cons (car lat) newlat) seen)))]))
    

(define (a-friend x y)
  (null? y))

(define (new-friend newlat seen)
  (a-friend newlat (cons '() seen)))

(define (last-friend x y)
  (length x))


(define (multiinsertLR&co new oldL oldR lat col)
  (cond 
    [(null? lat) 
       (col '() 0 0)]
    [(eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new (cons oldL newlat)) (add1 L) R)))]
    
    [(eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR (cons new newlat)) L (add1 R))))]
     
    [else 
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons (car lat) newlat) L R)))]))
               

(define (even? n)
  (= (* (floor (/ n 2)) 2) n))


(define (evens-only* l)
  (cond 
    [(null? l) '()]
    [(number? (car l))
       (cond 
         [(even? (car l))
            (cons (car l) (evens-only* (cdr l)))]
         [else
            (evens-only* (cdr l))])]
    [else
       (cons (evens-only* (car l)) (evens-only* (cdr l)))]))
           

;(define (evens-only*&co l col)
;  (cond 
;    [(null? l)
;       (col '() 1 0)]
;    [(atom? (car l))
;       [(cond 
;          [(even? (car l))
;             (evens-only*&co (cdr l) (lambda (newl p s)
;                                       (col (cons (car l) newl) (* (car l) p) s)))]
;          [else 
;             (evens-only*&co (cdr l) (lambda (newl p s)
;                                       (col newl p (+ (car l) s))))])]]
;    [else
;       (evens-only*&co (car l))
    
      
(define (looking a lat)
  (keep-looking a (pick 1 lat) lat))
      

(define (keep-looking a sorn lat)
  (cond
    [(numbered? sorn)
       (keep-looking a (pick sorn lat) lat)]
    [else 
      (eq? a sorn)]))
  
(define (shift pair)
  (build (first (first pair)) (build (second (first pair)) (second pair))))
  

(define (align pora)
  (cond
    [(atom? pora) pora]
    [(a-pair? (first pora))
       (align (shift pora))]
    [else 
       (build (first pora) (align (second pora)))]))


(define (length2* pora)
  (cond
    [(atom? pora) 1]
    [else
       (+ (length2* (first pora)) (length2* (second pora)))]))

