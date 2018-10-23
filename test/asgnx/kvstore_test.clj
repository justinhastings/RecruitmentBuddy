(ns asgnx.kvstore-test
  (:require [clojure.test :refer :all]
            [clojure.core.async :refer [<!!]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test.check.generators :as gen]
            [asgnx.kvstore :refer :all]))


(deftest state-put-test
  (testing "Correct addition of state elements"
    (is (= {:a 1} (state-put {} [:a] 1)))
    (is (= {:a 1 :b {:c :d}} (state-put {:a 1 :b 2} [:b] {:c :d})))
    (is (= {:a 1 :b {:c {:f :e}}} (state-put {:a 1 :b {:c {:d 1}}} [:b :c] {:f :e})))
    (is (= {nil nil} (state-put nil nil nil)))
    (is (= {:a {:b {:d 1}}} (state-put nil [:a :b :d] 1)))
    (is (= {:a {:b {:d 1}}} (state-put {} [:a :b :d] 1)))))

(deftest state-remove-test
  (testing "Correct removal of state elements"
    (is (= {} (state-remove {} [:a])))
    (is (= {:a 1} (state-remove {:a 1 :b 2} [:b])))
    (is (= {:a 1}  (state-remove {:a 1 :b {:c {:d 1}}} [:b :c])))
    (is (= nil (state-remove nil nil)))
    (is (= {:a {:b {:d 1}}} (state-remove {:a {:b {:d 1}}} [:a :b :f])))
    (is (= {:a {:b {:d 1}}} (state-remove {:a {:b {:d 1 :e 3}}} [:a :b :e])))
    (is (= {} (state-remove {:a {:b {:d 1}}} [:a :b :d])))))

(deftest state-get-test
  (testing "Correct fetching of state elements"
    (is (= nil (state-get {} [:a])))
    (is (= 2 (state-get {:a 1 :b 2} [:b])))
    (is (= 1 (state-get {:a 1 :b {:c {:d 1}}} [:b :c :d])))
    (is (= nil (state-get nil nil)))
    (is (= {:d 1} (state-get {:a {:b {:d 1}}} [:a :b])))
    (is (= {:b {:d 1 :e 3}} (state-get {:a {:b {:d 1 :e 3}}} [:a])))
    (is (= {} (state-get {:a {:b {:d {}}}} [:a :b :d])))))

(deftest state-keys-test
  (testing "Correct listing of keys at a given path"
    (is (= nil (state-keys {} [:a])))
    (is (= [:a :b] (state-keys {:a 1 :b 2} [])))
    (is (= [:d] (state-keys {:a 1 :b {:c {:d 1}}} [:b :c])))
    (is (= [:d :e :b] (state-keys {:a {:b {:d 1 :e 2 :b 3}}} [:a :b])))
    (is (= [:b] (state-keys {:a {:b {:d 1 :e 3}}} [:a])))
    (is (= nil (state-keys {:a {:b {:d {}}}} [:a :b :f])))))
