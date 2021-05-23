fibbenchif:
	cabal v2-run --enable-profiling lisp "(begin (define (fib n) (if (== n 1) 1 (if (== n 0) 1 (+ (fib (- n 1)) (fib (- n 2)))))) (fib 32))" --  +RTS -p
fibbenchcase:
	cabal v2-run --enable-profiling lisp "(begin (define (fib n) (case n ((0 1) 1) (else (+ (fib (- n 1)) (fib (- n 2)))))) (fib 32))" --  +RTS -p
	
sumbench:
	cabal v2-run --enable-profiling lisp "(begin (define (sumtr n x) (case n ((0) x) (else (sumtr (- n 1) (+ x n))))) (sumtr 1000000 0))" --  +RTS -p

