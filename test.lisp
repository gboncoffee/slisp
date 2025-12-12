; A factorial function.
(def 'factorial '((x)
    (if (= x 0)
        1
        '(* x (factorial (- x 1))))))

; println prints.
(println (factorial 5))

; : returns the last argument.
(:
    (puts "Hello, World!")
    (puts "foo bar"))
