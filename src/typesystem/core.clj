(ns typesystem.core
  (:gen-class))


(def ^:dynamic *type-map* (atom {}))

(defrecord FunType [args-types return-type])
(defrecord ValueType [value-type])
(defrecord ListType [item-type])

(defn add-type [sym type]
  (assert
   (or (instance? FunType type) (instance? ValueType type) (instance? ListType type))
   (str "not a type: " type))
  (swap! *type-map* (fn [m] (assoc m sym type))))


(defmacro add-fn-type [name t]
  (add-type name (list-to-fun-type t))
  :added)

(defn get-type [sym]
  (get @*type-map* sym 'Any))

(defn get-type-as-string [t]
  (cond
    (instance? FunType t)   "function"
    (instance? ValueType t) (pr-str (:value-type t))
    (instance? ListType t)  (str "[" (pr-str (:item-type t)) "]")
    :else                   (pr-str t)))

(defn same-type? [a b]
  (cond
    (= 'Any a) true
    (= 'Any b) true
    (= nil a)  (do (println "No type check possible") true)
    (= nil b)  (do (println "No type check possible") true)
    :else      (= a b)))

(defn same-types? [a b]
  (every? true?
          (map #(same-type? %1 %2)
               a
               b)))

(defn parse-type
  [t]
  (cond
    (symbol? t) (->ValueType t)
    (list? t)   (list-to-fun-type t)
    (vector? t) (->ListType (first t))))

(defn arg-list-to-fun-type
  [lis]
  (let [args        (take-while #(not (= '-> %)) lis)
        return-type (first (rest (drop-while #(not (= '-> %)) lis)))]

    (assert (some #{'->} lis)
            (str "Arrow missing"))
    (assert (= (count (drop-while #(not (= '-> %)) lis))
               2)
            (str "Return type missing"))
    (->FunType (map parse-type args) (parse-type return-type))))

(defn is-of-type
  [v t]
  (= t (get-type v)))

(defn add-arg-types [args arg-types]
  (mapv #(add-type %1 %2)
          args
          arg-types))

(defmacro defun [name the-type args & body]
  (let [fun-type (arg-list-to-fun-type the-type)]
    (assert (= (count args) (count (:args-types fun-type)))
            "number of arguments not the same as number of type definition arguments")
    (add-type name fun-type)

    (let [old (atom @*type-map*)]
      (binding [*type-map* old]
        (add-arg-types args (:args-types fun-type))
        (assert (same-type? (:return-type fun-type)
                            (infer-type (last body)))
                (str "Return type doesnt match. Got: "
                     (get-type-as-string (infer-type (last body)))
                     " expected: "
                     (get-type-as-string (:return-type fun-type))))
        `(defn ~name ~args ~@body)))))

(defn infer-value [form]
  'Any)

(defn infer-symbol [form]
  (get-type form))

(defn infer-fun-with-args [form]
  (let [fun-type (get-type (first form))
        arg-types (mapv infer-type (rest form))]

    (assert (same-types? (:args-types fun-type)
                         arg-types)
            (str (first form)
                 ": function wants as argument(s): "
                 (clojure.string/join ", " (mapv get-type-as-string (:args-types fun-type)))
                 ". Got: "
                 (clojure.string/join ", " (mapv get-type-as-string arg-types))))
    (:return-type fun-type)))


(defn infer-list-type [lis]
  (if (empty? lis)
    (->ListType nil)
    (->ListType
     (reduce (fn [acc el]
               (if (same-type? acc (infer-type el))
                 acc
                 (throw (Exception. (str "not all list items are of same type: got " acc " and " (infer-type el))))))
             (infer-type (first lis))
             lis))))

(defn infer-type [form]
  (cond
    (symbol? form) (infer-symbol form)

    (and (list? form)
         (= 'list (first form)))
    (infer-list-type form)

    (list? form)   (infer-fun-with-args form)
    :else          (infer-value form)))




(defun flup (Int String Int -> Int)
  [x y z]
  x)


#_(add-fn-type + (Int Int -> Int))





#_(defun add-1 (String -> Int)
  [x]
  (inc x))

#_(defun make-list (Int -> [Int])
  [x]
  x)






#_(add-fn-type inc (Int -> Int))



