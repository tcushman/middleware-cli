(ns middleware-cli.wrappers
  (:require [clojure.tools.cli :as cli]))


(def EXIT_SUCCESS 0)
(def EXIT_MISSING_CODE_CLEAN -1)
(def EXIT_MISSING_CODE_FAIL -2)
(def EXIT_ARGUMENT_ERRORS -3)


(defn- get-env-args
  [env-arg-map environment]
  (mapcat (fn [[k v]]
            (when-some [opt (get environment k)]
              [opt v]))
          env-arg-map))


(defn environment-arguments
  "Middleware to prepend arguments from the environment to the
command line arguments.  Requires a map of environment names to
command line options."
  [handler env-arg-map]
  (fn [{:keys [cli/environment]
        :as   cli-state}]
    (handler
      (update cli-state
              :cli/raw-arguments
              #(concat (get-env-args env-arg-map environment)
                       %)))))


(defn- map-keys
  [f m]
  (zipmap (map f (keys m))
          (vals m)))


(defn- qualify-keys
  [ns m]
  (map-keys (fn [k]
              (keyword (name ns) (name k)))
            m))


(defn parse-command-line
  "Middleware to parse command line arguments."
  [handler cli-opts]
  (fn [{:keys [cli/environment cli/raw-arguments]
        :as   state}]
    (handler
      (merge state
             (qualify-keys "cli"
               (cli/parse-opts raw-arguments
                               cli-opts))))))


(defn handle-argument-errors
  "Middleware to identify errors and exit."
  [handler]
  (fn [{:keys [cli/errors cli/summary]
        :as   app-state}]
    (if (empty? errors)
      (handler app-state)
      (do (println summary)
          (doseq [err errors]
            (println err))
          EXIT_ARGUMENT_ERRORS))))


(defn handle-help
  "Middleware to display help summary."
  [handler help-option]
  (fn [{:keys [cli/options cli/summary]
        :as   app-state}]
    (if (contains? options help-option)
      (do (println summary)
          EXIT_SUCCESS)
      (handler app-state))))


(defn uncaught-exceptions
  "Middleware to catch, log, and return uncaught exceptions."
  [handler]
  (fn [input]
    (try (handler input)
      (catch Throwable t
        (do (println "Uncaught Exception!")
            (.printStackTrace t *err*)
            t)))))


(defn- exit-value
  "Extract a numeric exit code from a result."
  [x]
  (cond (integer? x)
        x
        (map? x)
        (recur (:exit x))
        (instance? Throwable x)
        (:exit (ex-data x)
               EXIT_MISSING_CODE_FAIL)
        :else
        EXIT_MISSING_CODE_CLEAN))


(defn pre-exit-cleanup
  [handler]
  (fn [input]
    (try
      (handler input)
      (finally
        (shutdown-agents)))))



(defn exit-code
  "Middleware to return a numeric exit code."
  [handler]
  (fn [input]
    (exit-value (handler input))))



(defn init
  "Construct a fresh application state with
arguments and an environment."
  [args env]
  {:cli/raw-arguments args
   :cli/environment   env})







(defn main
  "Default middleware wrapper for command line applications."
  [handler {:keys [cli-options env-options environment
                   validate help-option]}]
  (-> handler
      (cond-> help-option
              (handle-help))
      (handle-argument-errors)
      (cond-> validate
              (comp validate))
      (parse-command-line cli-options)
      (cond-> (not-empty env-options)
              (environment-arguments env-options))
      (pre-exit-cleanup)
      (uncaught-exceptions)
      (exit-code)
      ))





(defmacro defmain
  [handler & {:keys [env-options cli-options validate]
              :as   settings}]
  `(defn ~'-main
     [& args#]
     (System/exit
       ((main ~handler ~settings)
         (init args# (System/getenv))))))


(defn opt
  "Construct a function for retrieving an option."
  [k]
  {:pre [(keyword? k)]}
  (comp k :cli/options))


(defn env
  "Construct a function for retrieving an environment value."
  [s]
  {:pre [(string? s)]}
  #(get-in % [:cli/environment s]))


(defn- ->fn
  "Return a function for a value.
Lists of functions will be composed.
(->fn +) => +
(->fn :key) => :key
(->fn [:a :b]) => (comp :b :a)"
  [x]
  (cond (sequential? x)
        (reduce comp (reverse (map ->fn x)))

        (ifn? x)
        x

        :else
        (throw
          (Exception.
            (format "Cannot create function for %s"
                    (pr-str x))))))


(defn unaryf
  "Transform a function of N arguments into a function of one argument."
  [f arg-fns]
  (let [arg-fns (mapv ->fn arg-fns)]
    (fn [input]
      (apply f (for [f arg-fns]
                 (f input))))))


(defn injection
  [reducer f arg-fns]
  (let [uf (unaryf f arg-fns)]
    (fn [input]
      (reducer input (f input)))))


(defn- ->seq
  [x]
  (if (sequential? x)
    x
    [x]))


(defn assoc-as
  [dest]
  (fn [m v]
    (assoc-in m (->seq dest) v)))


(defn inject
  [handler dest f arg-fns]
  (let [dest-path (->seq dest)
        inject-fn (injection #(assoc-in % dest-path)
                             f
                             arg-fns)]
    (fn [input]
      (handler
        (if (get-in input dest-path)
          input
          (assoc-in input dest-path
                    (inject-fn input)))))))

