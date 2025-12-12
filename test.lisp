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
