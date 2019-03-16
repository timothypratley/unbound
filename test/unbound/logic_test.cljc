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
           (unify '#{X 2} '#{1 2}))))

  (testing "Non-unifying complex expressions"
    (is (false? (unify '(foo 1) '(foo 2))))
    (is (false? (unify '(foo X 1) '(foo 2 X))))
    (is (false? (unify '(foo X X Y Z) '(foo (bar Y) (bar Z) 1 2))))
    (is (false? (unify '(foo X) '(foo 1 2))))
    (is (false? (unify '#{X 1} '#{2 X})))))