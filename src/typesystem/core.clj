(ns typesystem.core
  (:require [clojure.string :as string]))


(def ^:dynamic *type-map* (atom {}))

(defrecord FunType [args-types return-type])
(defrecord ValueType [value-type])

(defn add-type [sym type]
  (assert
   (or (instance? FunType type) (instance? ValueType type))
   (str "not a type: " type))
  (swap! *type-map* (fn [m] (assoc m sym type))))

(defn get-type [sym]
  (get @*type-map* sym))

(defn type->string [t]
  (cond
    (instance? FunType t)   "function"
    (instance? ValueType t) (pr-str (:value-type t))
    :else                   (pr-str t)))

(defn type=? [a b]
  (cond
    (= nil a)  (do (println "No type check possible") true)
    (= nil b)  (do (println "No type check possible") true)
    :else      (= a b)))

(defn types=? [lis-1 lis-2]
  (every? true?
          (map #(type=? %1 %2)
               lis-1
               lis-2)))

(declare arg-list-to-fun-type)

;; nicht unbedingt möglich, im ersten Schritt auch einfach nur "flache"
;; Funktionen erlauben.
(defn form->type
  [t]
  (cond
    (symbol? t) (->ValueType t)
    (list? t)   (arg-list-to-fun-type t)))

(defn arg-list-to-fun-type
  [lis]
  (let [arg-syms   (take-while #(not (= '-> %)) lis)
        return-sym (first (rest (drop-while #(not (= '-> %)) lis)))]

    (assert (some #{'->} lis)
            (str "Arrow missing"))
    (assert (= (count (drop-while #(not (= '-> %)) lis))
               2)
            (str "Return type missing"))
    ;; könnte auch einfach `(map ->ValueType args)` da stehen im ersten Wurf
    (->FunType (map form->type arg-syms) (form->type return-sym))))

(defmacro add-fn-type [sym t]
  (add-type sym (arg-list-to-fun-type t))
  :added)

(defn add-arg-types [arg-syms arg-types]
  (mapv #(add-type %1 %2)
          arg-syms
          arg-types))

(declare infer-type)

(defn check-return-type
  [fun-type body]
  (assert (type=? (:return-type fun-type)
                  (infer-type (last body)))
          (str "Return type doesnt match. Got: "
               (type->string (infer-type (last body)))
               " expected: "
               (type->string (:return-type fun-type)))))

(defmacro defun [name the-type arg-syms & body]
  (let [fun-type (arg-list-to-fun-type the-type)]
    (assert (= (count arg-syms) (count (:args-types fun-type)))
            "number of arguments not the same as number of type definition arguments")
    (add-type name fun-type)

    (let [old (atom @*type-map*)]
      (binding [*type-map* old]
        (add-arg-types arg-syms (:args-types fun-type))
        (check-return-type fun-type body)
        `(defn ~name ~arg-syms ~@body)))))

(defn infer-value-type [form]
  (cond
    (number? form)
    (->ValueType 'Number)

    (string? form)
    (->ValueType 'String)))

(defn infer-symbol-type [form]
  (get-type form))

(defn infer-fun-with-args-type [form]
  (let [fun-type (get-type (first form))
        arg-types (mapv infer-type (rest form))]

    (assert (types=? (:args-types fun-type) arg-types)
            (str (first form)
                 ": function wants as argument(s): "
                 (string/join ", " (mapv type->string (:args-types fun-type)))
                 ". Got: "
                 (string/join ", " (mapv type->string arg-types))))
    (:return-type fun-type)))

(defn infer-type [form]
  (cond
    (symbol? form) (infer-symbol-type form)
    (list? form)   (infer-fun-with-args-type form)
    :else          (infer-value-type form)))


(defun flap (Number -> Number)
  [x]
  x)



(defun flup (Number String Number -> Number)
  [x y z]
  x)


(defun add-1 (Number -> Number)
  [x]
  (+ x 3))


#_(defmacro remove-type [sym]
  (swap! *type-map* (fn [m] (dissoc m sym))))

#_(remove-type flap)





#_(add-fn-type + (Number Number -> Number))





#_(defun add-1 (String -> Int)
  [x]
  (inc x))

#_(defun make-list (Int -> [Int])
  [x]
  x)






#_(add-fn-type inc (Int -> Int))



