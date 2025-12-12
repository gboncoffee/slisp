(def 'factorial '((x)
    (if (= x 0)
        1
        '(* x (factorial (- x 1))))))

(println (factorial 5))

(:
    (println "Hello, World!")
    (println "foo bar"))
