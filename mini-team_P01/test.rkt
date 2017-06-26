#lang plai
(require "practica1.rkt")

;ec-lin :: Int -> Int -> Int
(test (ec-lin 1 1) -1)
(test (ec-lin 2 3) -3/2)
(test (ec-lin 8 1) -1/8)
(test (ec-lin 6 9) -3/2)
(test (ec-lin 5 10) -2)

;area-heron :: Int -> Int -> Int -> Int
(test (area-heron 3 25 26) 36)
(test (area-heron 3 4 5) 6)
(test (area-heron 10.09 3.83 10.46) 19.24)
(test (area-heron 4 5 6) 9.92)
(test (area-heron 7 4 5) 9.79)

; triangulo-rec? :: Int -> Int -> Int -> Bool
(test (triangulo-rec? 5 3 4) #t)
(test (triangulo-rec? 5 12 13) #t)
(test (triangulo-rec? 1 7 9) #f)
(test (triangulo-rec? 3 4 5) #t)
(test (triangulo-rec? 3 4 8) #f)

; s-digito :: Int -> Int
(test (s-digito 8) 8)
(test (s-digito 984) 3)
(test (s-digito 1729) 1)
(test (s-digito 123456) 3)
(test (s-digito 654371) 8)

; Invierte :: Int -> Int
(test (invierte 1) 1)
(test (invierte 321) 123)
(test (invierte 1729) 9271)
(test (invierte 69) 96)
(test (invierte 303) 303)

; elimina-dup :: [a] -> [a]
(test (elimina-dup '(1 1 2 2 3 3 1 1 1 2 3)) '(1 2 3 1 2 3))
(test (elimina-dup '(b c c a a a a b b)) '(b c a b))
(test (elimina-dup '(1 7 2 9)) '(1 7 2 9))
(test (elimina-dup '(1 2 2 2 2)) '(1 2))
(test (elimina-dup '(4 7 4 4)) '(4 7 4))

;binarios :: [Int] -> [String]
(test (binarios '(1 2 3)) '("1" "10" "11"))
(test (binarios '(1 7 2 9)) '("1" "111" "10" "1001"))
(test (binarios '(14 15)) '("1110" "1111"))
(test (binarios '(4 5 6)) '("100" "101" "110"))
(test (binarios '(12)) '("1100"))

;primos :: [Int]x -> [Int]
(test (primos '(1 2 3)) '(2 3))
(test (primos '(1 7 2 9)) '(7 2))
(test (primos '(1 2 3 4 5 6 7 8 9 10)) '(2 3 5 7))
(test (primos '(4)) '())
(test (primos '(1 2 3)) '(2 3))

;reversar :: [a] -> [a]
(test (reversar '(1 7 2 9)) '(9 2 7 1))
(test (reversar '(1)) '(1))
(test (reversar '(1 2)) '(2 1))
(test (reversar '(5 6 7)) '(7 6 5))
(test (reversar '(a b c)) '(c b a))

;reversal :: [a] -> [a]
(test (reversal '(1 7 2 9)) '(9 2 7 1))
(test (reversal '(1)) '(1))
(test (reversal '(1 2)) '(2 1))
(test (reversal '(5 6 7)) '(7 6 5))
(test (reversal '(a b c)) '(c b a))

;concatena :: lista -> lista -> lista
(test (concatena '(1 2 3) '(4 5)) '(1 2 3 4 5))
(test (concatena '(1 2 3) '(4)) '(1 2 3 4))
(test (concatena '(1 2 3) '()) '(1 2 3))
(test (concatena '(a b) '(c)) '(a b c))
(test (concatena '(5) '(4)) '(5 4))

;mapea :: funcion f -> [a] ->[b]
(test (mapea add1 '(1 2 3)) '(2 3 4))
(test (mapea even? '(1 2 3 4 5 6 7 8 9 10 11 12)) '(#f #t #f #t #f #t #f #t #f #t #f #t))
(test (mapea add1 '(1)) '(2))
(test (mapea add1 '(5 5)) '(6 6))
(test (mapea add1 '(7 5 3)) '(8 6 4))

;filtra :: predicado pred -> [a] -> [pred(a)]
(test (filtra integer? '(4 5.5 6)) '(4 6))
(test (filtra negative? '(-4 5.5 6)) '(-4))
(test (filtra zero? '(4 5.5 6)) '())
(test (filtra even? '(4 5 6)) '(4 6))
(test (filtra positive? '(4 5 6)) '(4 5 6))

;toma :: [a] -> n 
(test (toma '(5 6 7) 2) '(5 6))
(test (toma '(5 6 7) 3) '(5 6 7))
(test (toma '(5 6 7) 1) '(5))
(test (toma '(1 0 1 0 1) 3) '(1 0 1))
(test (toma '(5 6 7) 3) '(5 6 7))

;quita :: [a] -> n 
(test (quita '(5 6 7) 2) '(7))
(test (quita '(5 6 7) 3) '())
(test (quita '(5 6 7) 1) '(6 7))
(test (quita '(1 0 1 0 1) 3) '(0 1))
(test (quita '(0 1 2 3 4 5 6 7 8 9) 2) '(2 3 4 5 6 7 8 9))
