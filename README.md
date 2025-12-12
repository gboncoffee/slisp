# slisp - smol lisp

Small (smol) interpreter for a Lispic language.

# Usage

Run without arguments for a REPL, run with arguments to run scripts. When
running scripts, it prints the value of the last expression.

# Examples

```lisp
; A ; introduces comments.

;
; Primitives.
;

; Numbers
42
; > 42
3.14
; > 3.14
; Quoted expressions
'5
; > '5
'(foo)
; > '(foo)
; Strings, creates quoted expressions.
"hello world"
; > '(72 101 108 108 111 44 32 87 111 114 108 100 33)
; nil, the null/false constant.
nil
> nil
; t, the true constant.
t
> t

;
; Builtin functions.
;

; puts, prints anything followed by a newline, returning nil. Quoted expressions
; are printed as text if able.
(puts "Hello, World!")
; > Hello, World!
; > nil
; println, same as puts but quoted expressions are never printed as text.
(println "Hello, World!")
; > '(72 101 108 108 111 44 32 87 111 114 108 100 33)
; > nil
; readln, reads a line of input as a quoted expression.
(readln)
; < Gabriel
; > '(71 97 98 114 105 101 108 10)

; Arithmetic.
(+ 1 2)
; > 3
(- 1 2)
; > -1
(* 1 2)
; > 2

; Comparison.
(= 1 1)
; > t

; def, defines a global value.
(def 'x 5)

; unquote, evals a quoted expression.
(unquote '(puts "Hello, World!"))
; > Hello, World!
; > nil
(unquote t)
; > t

; let, defines a local name and evals a quoted expression.
(let 'name "Gabriel" '(puts name))
; > Gabriel
; > nil

; if, unquotes the second expression if the first is truthy, otherwise unquotes
; the third.
(if (= 1 0)
    '(puts "equal")
    '(puts "not equal"))
; > not equal
; > nil

; : returns the last argument (useful for causing side-effects in sequence).
(:
    (puts "hello")
    (puts "world")
    t)
; > hello
; > world
; > t

; cons, constructs quoted expressions.
(cons 1 (cons 2 (cons 3 nil)))
; > '(1 2 3)

; head, returns the first element of a quoted expression.
(head '(1 2 3))
; > 1

; tail, returns the quoted expression without the first element, or nil if the
; expression is empty.
(tail '(1 2 3))
; > '(2 3)
(tail '(1))
; > '()
(tail '())
; > nil

; eval, compiles and evaluates a quoted expression.
(* 2 (eval (readln)))
; < 3.14
; > 6.28

; Functions are quoted expressions with the list of arguments and the expression
; to evaluate.
(def 'plus-one '((x) (+ x 1)))
(plus-one 1)
; > 2
```
