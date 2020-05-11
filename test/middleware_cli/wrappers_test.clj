(ns middleware-cli.wrappers-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.tools.cli :as cli]
            [middleware-cli.wrappers :as wrap]))


(defn includes?
  [substr]
  (fn [s]
    (string/includes? s substr)))

(deftest test-handle-help
  (let [success    (Object.)
        wrapped-fn (wrap/handle-help :result :help)
        help-msg   "This is a help message"]
    (are [help-key options expected-result output?]
      (is (output?
            (with-out-str
              (is (identical? expected-result
                              ((wrap/handle-help :result help-key)
                                {:result      success
                                 :cli/options options
                                 :cli/summary help-msg}))))))

      :help  {}                          success        empty?
      :hilfe {:help true}                success        empty?
      :help  {:help nil}                 wrap/EXIT_HELP (includes? help-msg)
      :hilfe {:hilfe true :more "stuff"} wrap/EXIT_HELP (includes? help-msg)
      )))


(deftest test-handle-argument-errors
  (let [wrapped-fn (wrap/handle-argument-errors :result)
        help-msg   "This is my help message"]
    (are [input expected-result output?]
      (is (output?
            (with-out-str
              (is (= expected-result
                     (wrapped-fn input))))))

      {:cli/errors ["abcdefg" "wxyz"]
       :cli/summary help-msg}
      wrap/EXIT_ARGUMENT_ERRORS
      (every-pred
        (includes? help-msg)
        (includes? "\nabcdefg\n")
        (includes? "\nwxyz\n"))

      {:result     1234
       :cli/errors []
       :cli/summary "foo"}
      1234
      string/blank?
      )))


(deftest test-fmap-keys
  (are [input f expected]
    (= expected (#'wrap/fmap-keys f input))

    nil         str  nil
    {}          inc  {}
    {:a 1 :b 2} name {"a" 1 "b" 2}
    {1 :a 2 :b} inc  {2 :a 3 :b}
    ))


(deftest test-qualify-keys
  (are [input ns expected]
    (= expected (#'wrap/qualify-keys ns input))

    nil             :xyz  nil
    {}              :abc  {}
    {:a 1 :b 2}     :foo  #:foo{:a 1 :b 2}
    {:a/x 0 :b/y 0} :bar  #:bar{:x 0 :y 0}
    {:a/n 0 :b/n 0} :q    #:q{:n 0}
    ))


(deftest test-parse-opts
  (is #{:cli/errors :cli/summary :cli/arguments :cli/options}
      (keys (#'wrap/parse-opts [] []))))


(deftest test-parse-command-line
  (is (= #:cli{:arguments     []
               :errors        nil
               :summary       "      --abc VALUE"
               :options       {:abc "123"}
               :raw-arguments ["--abc" "123"]}
         ((wrap/parse-command-line identity [[nil "--abc VALUE"]])
           {:cli/raw-arguments ["--abc" "123"]}))))


(deftest test-get-env-args
  (are [env args-map expected]
    (= expected (#'wrap/get-env-args args-map env))
    
    nil                      nil                 []
    {}                       nil                 []
    nil                      {}                  []
    {"MY_ABC" "one"}         {}                  []
    {}                       {"MY_XYZ" "--xyz"}  []
    {"MY_ABC" "one"}         {"MY_ABC" "--abc"}  ["--abc" "one"]
    {"ONE" "11" "TWO" "22"}  {"TWO" "--two"}     ["--two" "22"]
    ))


(deftest test-environment-arguments
  (are [args env args-map expected]
    (= expected ((wrap/environment-arguments :cli/raw-arguments args-map)
                  (wrap/init args env)))
    
    nil     nil             nil                []
    []      {}              {}                 []
    ["abc"] {}              {}                 ["abc"]
    []      {"foo" "bar"}   {}                 []
    []      {}              {"foo" "--foo"}    []
    []      {"FIZZ" "buzz"} {"FIZZ" "--fizz"}  ["--fizz" "buzz"]
    ["123"] {"MY_XYZ" "hi"} {"MY_XYZ" "--xyz"} ["--xyz" "hi" "123"]
    ))


(deftest test-finally!
  (let [token   (Object.)
        counter (atom 0)]
    (and (is (identical? token
               ((wrap/finally! identity
                               (fn [in]
                                 (and (is (identical?  token in))
                                      (swap! counter inc))))
                 token)))
         (is (= 1 @counter))))

  (let [counter (atom 0)]
    (and (is (thrown? ArithmeticException
               ((wrap/finally! (fn [_] (/ 1 0))
                               (fn [in]
                                 (and (is (= {} in))
                                      (swap! counter inc))))
                 {})))
         (is (= 1 @counter)))))

