(letrec [(even? (λ (n)
                   (if (= n 0)
                       #t
                     (if (= n 1)
                         #f
                         (even? (- x 2))))))]
  (letrec [(div2* (λ (n s)
                     (if (= (* 2 n) s)
                         n
                         (if (= (+ (* 2 n) 1) s)
                             n
                             (div2* (- n 1) s)))))]
    (letrec [(div2 (λ (n)
                      (div2* n n)))]
      (letrec [hailstone* n count (λ (n count)
                                     (if (= n 1)
                                         count
                                         (if (even? n)
                                             (hailstone* (div2 x) (+ count 1))
                                             (hailstone* (+ (* 3 x) 1) (+ count 1)))))]
        (letrec [hailstone (λ (n)
                              (hailstone* n 0))]
          (hailstone 5))))))
