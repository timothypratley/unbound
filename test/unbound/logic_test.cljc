(ns unbound.logic-test
  (:require [clojure.test :refer :all]
            [unbound.logic :refer :all]))

(deftest unify-test

  (testing "Unifying atoms"
    (is (= {} (unify 1 1)))
    (is (= {} (unify 'a 'a)))
    (is (= {} (unify nil nil)))
    (is (= {} (unify false false))))

  (testing "Non-unifying atoms"
    (is (false? (unify 'a 'b))))


  (testing "Variables always unify"
    (is (= '{X Y}
           (unify 'X 'Y)))

    (is (= '{X 2}
           (unify 'X 2)))

    (is (= '{X (foo 1)}
           (unify 'X '(foo 1))))

    (is (= '{Y (foo (bar baz))}
           (unify '(foo (bar baz)) 'Y))))


  (testing "Unifying complex expressions"
    (is (= {}
           (unify '(foo 1) '(foo 1))))

    (is (= '{X 1}
           (unify '(foo X) '(foo 1))))

    (is (= '{X Y}
           (unify '(foo X) '(foo Y))))

    (is (= '{X 1
             Y 2}
           (unify '(foo Y 1) '(foo 2 X))))

    (is (= '{X 1
             Y Z}
           (unify '(foo (bar Y) 1) '(foo (bar Z) X))))

    (is (= '{Y Z
             X Z}
           (unify '(foo X X) '(foo Y Z))))

    (is (= '{X 2
             Y 1
             Z 1}
           (unify '(foo Z X 1) '(foo Y 2 Z))))

    (is (= '{X 2
             Z Y}
           (unify '(foo Z X (bar Z)) '(foo Y 2 (bar Z)))))

    (is (= '{}
           (unify '(foo _ _) '(foo 1 2))))

    (is (= '{Z 1
             Y 1
             X (bar 1)}
           (unify '(foo X X Y Z) '(foo (bar Y) (bar Z) 1 1))))

    (is (= '{}
           (unify '() '())))

    (is (= '{X 1}
           (unify '(X) '(1))))

    (is (= '{}
           (unify '{1 2} '{1 2})))

    (is (= '{X 2}
           (unify '{1 X} '{1 2})))

    (is (= '{X 1}
           (unify '#{2 X} '#{1 2})))

    (is (= '{X 1}
           (unify '#{X 2} '#{1 2})))

    (is (= '{Z 2, Y 1, X 2}
           (unify '(X 1 (foo Z)) '(2 Y (foo X))))))

  (testing "Non-unifying complex expressions"
    (is (false? (unify '(foo 1) '(foo 2))))
    (is (false? (unify '(foo X 1) '(foo 2 X))))
    (is (false? (unify '(foo X X Y Z) '(foo (bar Y) (bar Z) 1 2))))
    (is (false? (unify '(foo X) '(foo 1 2))))
    (is (false? (unify '#{X 1} '#{2 X}))))

  (testing "List unification"
    (is (= '{}
           (unify '(:a :b :c & [:d :e]) '(:a & [:b :c :d :e]))
           ;;(unify '(:a :b :c | [:d :e]) '(:a :b :c :d :e))
           (unify '(:a :b :c & [:d :e]) '(:a :b :c :d :e))))
    (is (= '{A :a
             B :b
             C :c
             Tail (:d :e)}
           (unify '([A B C] | Tail) '(:a :b :c :d :e))
           (unify '(A B C & Tail) '(:a :b :c :d :e))))))

(deftest query-test
  (let [line-facts '[(vertical (line (point X _) (point X _)))
                     (horizontal (line (point _ Y) (point _ Y)))]]
    (is (= {}
           (first (query line-facts '(vertical (line (point 1 1) (point 1 3)))))))
    (is (= '{Y 1}
           (first (query line-facts '(horizontal (line (point 1 1) (point 2 Y)))))))
    (is (= '{P (point _ 3)}
           (first (query line-facts '(horizontal (line (point 2 3) P)))))))

  (let [facts-and-rule '[(f a)
                         (f b)
                         (g a)
                         (g b)
                         (h b)
                         ((k X) :- (and (f X) (g X) (h X)))]]
    (is (= '{Y b}
           (first (query facts-and-rule '(k Y))))))

  (let [knowledge-base  '[(loves vincent mia)
                          (loves marcellus mia)
                          ((jealous A B) :- (and (loves A C) (loves B C)))]]
    (is (= '{Y vincent
             X vincent}
           (first (query knowledge-base '(jealous X Y)))))))
