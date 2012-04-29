;;; A library for geometric algebra computations in the 5-generator
;;; conformal model of R^3.

(declare (usual-integrations + - * /))

(define-structure generator
  symbol
  %signature
  order)

(define (generator-signature g) ; a fluid-bindable hook
  (generator-%signature g))

(define (generator< g1 g2)
  (< (generator-order g1) (generator-order g2)))

(define (generator= g1 g2)
  (= (generator-order g1) (generator-order g2)))

(define (generator> g1 g2)
  (> (generator-order g1) (generator-order g2)))

(define g1 (make-generator 'e1 1 1))
(define g2 (make-generator 'e2 1 2))
(define g3 (make-generator 'e3 1 3))
(define g+ (make-generator 'e+ 1 4))
(define g- (make-generator 'e- -1 5))

(define (symbol->generator s)
  (cdr (assq s `((e1 . ,g1)
                 (e2 . ,g2)
                 (e3 . ,g3)
                 (e+ . ,g+)
                 (e- . ,g-)))))

(define-structure (term (constructor %make-term))
  basis
  coeff)

(define (make-term basis coeff)
  (define (normalize-by-basis basis coeff)
    (cond ((null? basis)
           (values basis coeff))
          (else
           (let ((g1 (car basis))
                 (rest (cdr basis)))
             (receive (new-rest new-coeff)
               (normalize-by-basis rest coeff)
               (insert-generator g1 new-rest new-coeff))))))
  (define (insert-generator g rest coeff)
    (cond ((null? rest)
           (values (list g) coeff))
          ((generator< g (car rest))
           (values (cons g rest) coeff))
          ((generator= g (car rest))
           (values (cdr rest) (* coeff (generator-signature g))))
          ((generator> g (car rest))
           (receive (new-sub-rest new-coeff)
             (insert-generator g (cdr rest) coeff)
             (values (cons (car rest) new-sub-rest)
                     (* -1 new-coeff))))))
  (receive (new-basis new-coeff)
    (normalize-by-basis basis coeff)
    (%make-term new-basis new-coeff)))

(define (lexicographically elt< x y)
  (let ((nx (length x)) (ny (length y)))
    (cond ((< nx ny) #t)
          ((> nx ny) #f)
          (else
           (let lp ((x x) (y y))
             (cond ((null? x) #f)       ; same
                   ((elt< (car x) (car y)) #t)
                   ((elt< (car y) (car x)) #f)
                   (else (lp (cdr x) (cdr y)))))))))

(define (term< term1 term2)
  (lexicographically
   generator< (term-basis term1) (term-basis term2)))

(define (term* term1 term2)
  (make-term (append (term-basis term1)
                     (term-basis term2))
             (* (term-coeff term1)
                (term-coeff term2))))

(define (term^ term1 term2)
  (fluid-let ((generator-signature (lambda (g) 0)))
    (term* term1 term2)))

(define (term-dot term1 term2)
  (if (or (null? (term-basis term1))
          (null? (term-basis term2)))
      (make-term '() 0)
      (term-grade-project
       (term* term1 term2)
       (abs (- (length (term-basis term1))
               (length (term-basis term2)))))))

(define (term-grade-project term grade)
  (if (= grade (length (term-basis term)))
      term
      (make-term '() 0)))

(define (term-reverse term)
  (make-term (reverse (term-basis term)) (term-coeff term)))

;;; A multivector is a formal sum of terms, where each term is a
;;; scalar multiplied by a basis element of the geometric algebra.
;;; This is represented as an alist from basis elements to their
;;; coefficients.  The basis elements are normalized by
;;; anticommutativity to be lists of generators in the order e1, e2,
;;; e3, e+, and e-.
(define-structure
  (multivector (constructor %make-multivector))
  terms)

(define (ignorable? x)
  (and (real? x)
       (< (abs x) 1e-10)))

(define (make-multivector terms)
  (define (canonicalize terms)
    (cond ((or (null? terms) (null? (cdr terms)))
           terms)
          (else
           (let ((first (car terms))
                 (second (cadr terms))
                 (rest (cddr terms)))
             (if (equal? (term-basis first) (term-basis second))
                 (canonicalize
                  (cons (make-term (term-basis first)
                                   (+ (term-coeff first)
                                      (term-coeff second)))
                        rest))
                 (cons first (canonicalize (cons second rest))))))))
  (define (clean terms)
    (filter (lambda (term)
              (not (ignorable? (term-coeff term))))
            terms))
  (%make-multivector
   (clean (canonicalize (sort terms term<)))))

(define (mv->scalar mv)
  (cond ((null? (multivector-terms mv)) 0)
        ((null? (term-basis (car (multivector-terms mv))))
         (term-coeff (car (multivector-terms mv))))
        (else 0)))

(define (careful-mv->scalar mv)
  (for-each
   (lambda (term)
     (if (and (not (null? (term-basis term)))
              (not (ignorable? (term-coeff term))))
         (warn "Forgetting nonzero term in" (multivector->list mv))))
   (multivector-terms mv))
  (mv->scalar mv))

(define (pairwise f lst lst2)
  (apply append
         (map (lambda (elt1)
                (map (lambda (elt2)
                       (f elt1 elt2))
                     lst2))
              lst)))

(define ((termwise f) mv1 mv2)
  (make-multivector
   (pairwise
    f (multivector-terms mv1) (multivector-terms mv2))))

(define ((termwise1 f) mv)
  (make-multivector (map f (multivector-terms mv))))

(define (geom+ mv1 mv2)
  (make-multivector
   (append (multivector-terms mv1) (multivector-terms mv2))))

(define (zero-mv? multivector)
  (null? (multivector-terms multivector)))

(define geom* (termwise term*))

(define geom^ (termwise term^))

(define dot (termwise term-dot))

(define mv-reverse (termwise1 term-reverse))

(define (term->list term)
  `(,(term-coeff term) ,@(map generator-symbol (term-basis term))))

(define (multivector->list mv)
  `(+ ,@(map term->list (multivector-terms mv))))

(define (list->term lst)
  (make-term (map symbol->generator (cdr lst)) (car lst)))

(define (list->multivector lst)
  (make-multivector (map list->term (cdr lst))))

(define id (list->multivector '(+ (1))))
(define e1 (list->multivector '(+ (1 e1))))
(define e2 (list->multivector '(+ (1 e2))))
(define e3 (list->multivector '(+ (1 e3))))
(define e+ (list->multivector '(+ (1 e+))))
(define e- (list->multivector '(+ (1 e-))))

(define (scalar->multivector x)
  (list->multivector `(+ (,x))))

(define (scalar+mv x mv)
  (geom+ (scalar->multivector x) mv))

(define (mv+scalar mv x)
  (geom+ mv (scalar->multivector x)))

(define (scalar*mv x mv)
  (geom* (scalar->multivector x) mv))

(define (mv*scalar mv x)
  (geom* mv (scalar->multivector x)))

(define (mv/scalar mv x)
  (mv*scalar mv (/ x)))

(define (scalar? x)
  (or (real? x) (abstract-number? x)))

(assign-operation '+ geom+ multivector? multivector?)
(assign-operation '+ scalar+mv scalar? multivector?)
(assign-operation '+ mv+scalar multivector? scalar?)

(assign-operation '* geom* multivector? multivector?)
(assign-operation '* scalar*mv scalar? multivector?)
(assign-operation '* mv*scalar multivector? scalar?)

(assign-operation '/ mv/scalar multivector? scalar?)

(define einf (+ e- e+))
(define e0 (* 1/2 (+ e- (* -1 e+))))
(define I (* e1 e2 e3 e+ e-))
(define I-1 (* -1 e- e+ e3 e2 e1))

(define (dual mv)
  (* mv I-1))

(define (inverse blade)
  (/ (mv-reverse blade)
     (mv->scalar (* blade (mv-reverse blade)))))

(define (one-vector? mv)
  (every (lambda (term)
           (= 1 (length (term-basis term))))
         (multivector-terms mv)))

(define ((project blade) mv)
  ;; TODO null blades
  (* (dot mv (inverse blade)) blade))

(define (antipode point)
  (let* ((homogenized
          (/ point (careful-mv->scalar (dot (* -1 point) e-))))
         (in_r4 (+ homogenized (* -1 e-)))
         (antipode (* -1 in_r4))
         (back (+ antipode e-)))
    back))

(define (great-sphere point1 point2 point3)
  ;; This will contain the other antipodes too
  (let ((answer
         (reduce geom^ id
          (list point1 point2 point3 (antipode point1)))))
    (if (not (one-vector? (dual answer)))
        (error "Great sphere wasn't" answer (dual answer)))
    answer))

(define (really-plane? sphere)
  (zero-mv? (geom^ sphere einf)))

(define (inside-sphere? sphere point)
  (let* ((inner-product-sphere (dual sphere))
         (discriminant
          (/ (mv->scalar (dot inner-product-sphere point))
             (mv->scalar (dot inner-product-sphere einf))
             (mv->scalar (dot point einf)))))
    (if (not (one-vector? inner-product-sphere))
        (warn "Insiding not the dual of a sphere"
              (multivector->list inner-product-sphere)))
    (>= discriminant 0)))

(define (sphere-center sphere)
  (let ((inner-product-sphere (dual sphere)))
    (if (not (one-vector? inner-product-sphere))
        (warn "Centering not the dual of a sphere"
              (multivector->list inner-product-sphere)))
    (mv->3-vector
     (/ ((project (* e1 e2 e3)) inner-product-sphere)
        (- (mv->scalar (dot inner-product-sphere einf)))))))

(define (mv->3-vector mv)
  (map mv->scalar (list (dot mv e1) (dot mv e2) (dot mv e3))))

(define (sphere-radius sphere)
  (let ((inner-product-sphere (dual sphere)))
    (if (not (one-vector? inner-product-sphere))
        (warn "Radiusing not the dual of a sphere"
              (multivector->list inner-product-sphere)))
    (sqrt
     (/ (mv->scalar (* inner-product-sphere inner-product-sphere))
        (square (mv->scalar (dot inner-product-sphere einf)))))))

(define (plane-normal plane)
  (let ((inner-product-plane (dual plane)))
    (mv->3-vector
     ((project (* e1 e2 e3)) inner-product-plane))))

(define (plane-scale plane)
  (define (3-norm vec)
    (sqrt (apply + (map * vec vec))))
  (3-norm (plane-normal plane)))

(define (plane-unit-normal plane)
  (map / (plane-normal plane) (make-list 3 (plane-scale plane))))

(define (plane-displacement plane)
  (- (/ (careful-mv->scalar (dot (dual plane) e0))
        (plane-scale plane))))

;; This implicitly projects the input 4-vector conformally down to R3
;; (and represents the result in the conformal algebra).
(define (r4-vector->mv x y z w)
  (+ (* x e1) (* y e2) (* z e3) (* w e+) e-))
