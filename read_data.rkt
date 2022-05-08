#lang racket

; Abrir archivo
(define file-read-inv (open-input-file "C:\\Users\\marco\\Documents\\Tareas\\Implementación\\Evidencia_2\\data\\i4.txt"))

; Leer datos
(define inv-productos (read file-read-inv))
(define inv-monedas (read file-read-inv))

(displayln "PRODUCTOS")
(for-each (lambda (producto) (displayln producto)) inv-productos)

(displayln "\nMONEDAS")
(for-each (lambda (moneda) (displayln moneda)) inv-monedas)

; Cerrar archivo
(close-input-port file-read-inv)
