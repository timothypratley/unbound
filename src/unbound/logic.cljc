(ns unbound.logic
  (:require [clojure.set :as set]))

(def variable-leaders
  (set (seq "_?ABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(defn variable?
  "Returns true if the input argument is free to be bound.
  Unbound vars are unwieldy, so symbols are also considered to be variables if they follow variable naming convention.
  In Prolog variable names with an upper-case letter.
  Symbols starting with ? will also be considered variables as per Datomic.
  There is a special variable _ which means bind to anything."
  [x]
  (or (and (var? x) (not (bound? x)))
      (and (symbol? x) (contains? variable-leaders (first (name x))))))

(defn complex? [x]
  (coll? x))

(defn constant? [x]
  (not (or (variable? x)
           (complex? x))))

(declare compatible?)

(defn all-compatible? [[instantiation & more] environment]
  (if instantiation
    (if-let [expanded-environment (compatible? instantiation environment)]
      (recur more expanded-environment)
      false)
    environment))

(defn unify
  "Compares two expressions which may contain unbound variables.

  Returns a map of variable bindings required to make the expressions equal,
  or false if that is not possible.
  An empty map result means that the expressions unify without any bindings.
  If bindings are required to unify the expressions,
  then the result is a map containing keys which are unbound variables, and values which are the binding required.

  Unify is similar to equals, but solves for variables in the equation.
  Unification is described in section 2.1 of the Learn Prolog Now book.
  (http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse5)
  1. If term1 and term2 are constants, then term1 and term2 unify if and only if they are the same atom, or the same number.
  2. If term1 is a variable and term2 is any type of term, then term1 and term2 unify, and term1 is instantiated to term2.
     Similarly, if term2 is a variable and term1 is any type of term, then term1 and term2 unify,
     and term2 is instantiated to term1.
     (So if they are both variables, they’re both instantiated to each other, and we say that they share values.)
  3. If term1 and term2 are complex terms, then they unify if and only if:
     a. They have the same functor and arity, and
     b. all their corresponding arguments unify, and
     c. the variable instantiations are compatible.
        (For example, it is not possible to instantiate variable X to mia when unifying one pair of arguments,
         and to instantiate X to vincent when unifying another pair of arguments.)
  4. Two terms unify if and only if it follows from the previous three clauses that they unify."
  [x y]
  (cond (and (constant? x)
             (constant? y))
        (and (= x y) {})

        (or (variable? x) (variable? y))
        (if (or (= x '_)
                (= y '_)
                (= x y))
          {}
          (if (variable? x)
            {x y}
            {y x}))

        (and (complex? x) (complex? y))
        (and
          (= (count x) (count y))
          ;; TODO: While unification may work on small sets/maps, they won't always align linearly.
          (let [instantiations (map unify x y)]
            (and
              (every? identity instantiations)
              (all-compatible? instantiations {}))))

        :else
        false))

(defn substitute [v variable assignment]
  (if (= v variable)
    assignment
    (if (complex? v)
      (map #(substitute % variable assignment) v)
      v)))

(defn assign [environment variable assignment]
  (persistent!
    (reduce-kv
      (fn [acc k v]
        (if (= k variable)
          acc
          (assoc! acc k (substitute v variable assignment))))
      (transient {variable (if (contains? environment assignment)
                             (get environment assignment)
                             assignment)})
      environment)))

(defn compatible? [[[variable assignment] & more] environment]
  (if variable
    (if (= variable assignment)
      environment
      (if (contains? environment variable)
        (let [existing (get environment variable)]
          (if-let [e (unify existing assignment)]
            (compatible? e environment)
            false))
        (if-let [e (assign environment variable assignment)]
          (recur more e)
          false)))
    environment))

(defn rule? [xs]
  (and (seqable? xs)
       (= :- (second xs))))

(defn replace-variables [expression generated-variables]
  (if (variable? expression)
    (get generated-variables expression)
    (if (seqable? expression)
      (for [sub-expression expression]
        (replace-variables sub-expression generated-variables))
      expression)))

(defn every [pred coll acc]
  (if (nil? (seq coll))
    acc
    (if-let [result (pred (first coll))]
      (recur pred (next coll) result)
      false)))

(defn apply-rules [knowledge-base question]
  (first
    (remove false?
            (for [fact-or-rule knowledge-base]
              (if (rule? fact-or-rule)
                (if-let [e (unify question (first fact-or-rule))]
                  (let [body (-> (nth fact-or-rule 2)
                                 (replace-variables (set/map-invert e)))]
                    (condp = (first body)
                      'and (every #(apply-rules knowledge-base %) (rest body) {})
                      'or (or (some #(apply-rules knowledge-base %) (rest body)) false)
                      (apply-rules knowledge-base body)))
                  false)
                (unify question fact-or-rule))))))

(defn query [knowledge-base question]
  (let [variables (set (filter variable? (tree-seq seqable? seq question)))
        generated-variables (into {}
                                  (for [variable variables]
                                    [variable (gensym (str "_" variable))]))
        new-question (replace-variables question generated-variables)]
    (if-let [result (apply-rules knowledge-base new-question)]
      ;; rename the generated names back to the input names
      (set/rename-keys (select-keys result (vals generated-variables))
                       (set/map-invert generated-variables))
      false)))
