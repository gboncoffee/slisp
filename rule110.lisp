(def 'one (head "1"))
(def 'zero (head "0"))

(def 'drop-last '((list)
    (if (= (tail list) '())
        nil
        '(cons (head list) (drop-last (tail list))))))

(def 'null '((list)
    (= nil (tail list))))

(def 'map '((f list)
    (if (null list)
        ''()
        '(cons (f (head list)) (map f (tail list))))))

(def 'zero-to-space '((char)
    (if (= zero char)
        (head " ")
        char)))

(def 'pretty-print-list '((list)
    (puts (map zero-to-space list))))

(def 'rule110 '((a b c)
    (let 'list (cons a (cons b (cons c nil)))
        '(if (= list "111")
            zero
            '(if (= list "110")
                one
                '(if (= list "101")
                    one
                    '(if (= list "100")
                        zero
                        '(if (= list "011")
                            one
                            '(if (= list "010")
                                one
                                '(if (= list "001")
                                    one
                                    zero))))))))))

(def 'rule110-rec '((input)
    (if (= '() (tail (tail input)))
        '(cons (rule110 (head input) (head (tail input)) zero) nil)
        '(cons
            (rule110 (head input) (head (tail input)) (head (tail (tail input))))
            (rule110-rec (tail input))))))

(def 'apply-rule110 '((input)
    (rule110-rec (cons one input))))

(def 'run-rule110 '((input times)
    (if (= times 0)
        t
        '(let 'new (apply-rule110 input)
            '(:
                (pretty-print-list new)
                (run-rule110 new (- times 1)))))))

(let 'input (drop-last (readln)) '
    (let 'times (eval (readln)) '
        (run-rule110 input times)))

t
