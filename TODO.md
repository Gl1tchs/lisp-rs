# A Scheme interpreter written in rust.

```scheme
(define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
```

A Scheme implementation in rust for mathematical calculations. It doesn't support
any string operation at all only functions, integers, floats and lists with
conditions and variables.

## Keywords 

- define: defines a new variable it can only be *integer, float, list or lambda*
- if: checks for the condition if true evaluates the third argument otherwise evaluates the fourth argument.
- begin: evaluates each expression individualy but only returns the last one.
- apply: applies given function / operator to all elements of given list / atomic.
- lambda: defines a function with parameters, e.g. (lambda (x y) (x * y))
- \+, add: Addition operation.
- \-, sub: Subtraction operation.
- \*, mul: Multiplication operation.
- \/, div: Division operation.
- mod: Computes the remainder of division, e.g., (mod a b) -> a % b
- expt: Exponentiation, e.g., (expt base exp) -> base ^ exp
- sqrt: Square root, e.g., (sqrt x) -> √x
- abs: Absolute value, e.g., (abs x) -> |x|
- min: Returns the smallest value in a list, e.g., (min a b c ...)
- max: Returns the largest value in a list, e.g., (max a b c ...)
- \>, gt: Greater-than comparison, e.g., (> a b)
- \<,  lt: Less-than comparison, e.g., (< a b)
- \>\=, gte: Greater-than-or-equal comparison, e.g., (>= a b)
- \<\=, lte: Less-than-or-equal comparison, e.g., (<= a b)
