#lang racket
; Marco Arana, Carnet: 00037822

; Ejercicio 1
(define (contar-positivos lista)
    (length (filter (lambda (x) (> x 0)) lista))
)

(displayln (contar-positivos '(3 -2 7 0 -5 9))) 
(newline)

; Ejercicio 2
(define (pares-elevados lista)
    (map (lambda (x) (expt x 2)) (filter (lambda (x) (= (modulo x 2) 0)) lista))
)

(displayln (pares-elevados '(1 2 3 4 5 6 7 8))) 
(newline)

; Ejercicio 3
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1))))
)

(displayln (factorial 5)) 
(newline)

; Ejercicio 4
(define (cubo lista)
  (map (lambda (x) (expt x 3)) lista)
)

(displayln (cubo '(2 3 4))) 
(newline)

; Ejercicio 5
(define (suma-impares lista)
  (foldl + 0 (filter (lambda (x) (not(= (modulo x 2) 0))) lista))
)

(displayln (suma-impares '(1 2 3 4 5 6 7))) 
(newline)

; Ejercicio 6
(define (tiene-negativos lista)
  (ormap (lambda (x) (< x 0)) lista)
)

(displayln (tiene-negativos '(5 9 -3 2)))
(newline)

; Ejercicio 7
(define (suma-acumulada lista)
  (reverse
   (foldl
    (lambda (x acc)
      (cons (+ x (if (null? acc) 0 (first acc))) acc))
    '()
    lista))
)

(displayln (suma-acumulada '(1 2 3 4)))
(newline)

; Ejercicio 8
(define (concatenar-cadenas lista)
 (foldl string-append "" (reverse lista))
)

(displayln (concatenar-cadenas '("Hola" " " "Mundo")))
(newline)

; Ejercicio 9
(define (mayores5 lista)
  (map (lambda (y) (* y 2)) (filter (lambda (x) (> x 5)) lista))
)

(displayln (mayores5 '(3 6 8 2 10))) 
(newline)

; Ejercicio 10
(define (invertir lista)
  (foldl (lambda (x acc)
           (cons x acc))
         '()
         lista)
)

(displayln (invertir '(1 2 3 4))) 
(newline)

; Ejercicio 11
(define (aplicar-todo f lista)
  (map f lista)
)

(displayln (aplicar-todo (lambda (x) (expt x 2)) '(1 2 3 4))) 
(newline)

; Ejercicio 12
(define (promedio-mayores-5 lista)
    (exact->inexact (/ (foldl + 0 (filter (lambda (x) (> x 5)) lista)) (length (filter (lambda (x) (> x 5)) lista))))
)

(displayln (promedio-mayores-5 '(3 8 10 4 9 2 7))) 
(newline)