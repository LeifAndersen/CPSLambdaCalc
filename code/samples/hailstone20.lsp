(letrec [(even? (λ (n)
                   (if (= n 0)
                       #t
                       (if (= n 1)
                           #f
                           (even? (- n 2))))))]
  (letrec [(div2* (λ (n s)
                     (if (= (* 2 n) s)
                         n
                         (if (= (+ (* 2 n) 1) s)
                             n
                             (div2* (- n 1) s)))))]
    (letrec [(div2 (λ (n)
                      (div2* n n)))]
      (letrec [(hailstone* (λ (n count)
                                     (if (= n 1)
                                         count
                                         (if (even? n)
                                             (hailstone* (div2 n) (+ count 1))
                                             (hailstone* (+ (* 3 n) 1) (+ count 1))))))]
        (letrec [(hailstone (λ (n)
                              (hailstone* n 0)))]
          (hailstone 20))))))
