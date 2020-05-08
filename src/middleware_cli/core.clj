(ns middleware-cli.core
  (:refer-clojure :exclude [fnil])
  (:require [clojure.pprint :as pprint]
            [clojure.tools.cli :as cli]
            [middleware-cli.wrappers :as wrap]))

;; Examples of general purpose middleware

(defn before
  [f before-fn]
  (comp f before-fn))

(defn do-before!
  [f before-fn!]
  (fn [& args]
    (do (apply before-fn! args)
        (apply f args))))


(defn after
  [f after-fn]
  (comp after-fn f))


(defn do-after!
  [f after-fn!]
  (fn [& args]
    (let [result (apply f args)]
      (apply after-fn! args)
      result)))



;; More example of the middleware pattern

(defn fnil
  "If the first argument to a function is nil, replace it with a fixed value."
  [f value]
  (fn [& args]
    (if (nil? (first args))
      (apply f (cons value (rest args)))
      (apply f args))))


(def conj-vec
  "Conj into a vector by default"
  (fnil conj []))


(def conj-set
  "Conj into a set by default"
  (fnil conj #{}))




(defn timed
  "Time the execution of a function wherever it is called."
  [f]
  (fn [& args]
    (time (apply f args))))


(def timed-assoc
  (timed assoc))




(defn log-activity
  "Always print the arguments and result for a function."
  [f]
  (fn [& args]
    (let [result (apply f args)]
      (println (pr-str (cons f args)) "=>" (pr-str result))
      result)))


(def log-plus
  (log-activity +))




;;
;; Demo of CLI app
;;


(def cli-options
  [["-l" "--limit LIMIT" "An upper bound"
    :default 10
    :default-descr "Ten"
    :parse-fn #(Long/valueOf %)]
   [nil "--entity EID" "A specific entity"]
   ["-h" "--help" "Display the help message"]])

(def env-options
  {"MYAPP_LIMIT" "--limit"})



(defn print-state
  [input]
  (do (pprint/pprint input)
      {:exit 0}))


(wrap/defmain print-state
  :cli-options cli-options
  :env-options env-options)



