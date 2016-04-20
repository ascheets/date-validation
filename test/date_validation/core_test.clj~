(ns date-validation.core-test
  (:require [clojure.test :refer :all]
            [date-validation.core :refer :all]))

(deftest leapYear-test

  (is (= false (leapYear 200)))
  (is (= true (leapYear 400)))
  (is (= false (leapYear 321)))
  (is (= true (leapYear 2016)))
  (is (= false (leapYear 100)))
  (is (= true (leapYear 2000)))

)

(deftest validDay-test

  (is (= '(true true true false true true false) (map validDay dates)))

  ;;how to print the valid dates
  ;;(def validDates (filter validDay test-dates))
  ;;(println validDates)

)

(deftest get-dates-tests

  (is (= '("01/01/1973" "01/02/1973" "01/03/1973") (take 3 (get-dates "01/01/1973"))))
  (is (= '("01/30/1973" "01/31/1973" "02/01/1973") (take 3 (get-dates "01/30/1973"))))
  (is (= '("02/27/1973" "02/28/1973" "03/01/1973") (take 3 (get-dates "02/27/1973"))))
  (is (= '("02/27/1976" "02/28/1976" "02/29/1976" "03/01/1976") (take 4 (get-dates "02/27/1976"))))
  (is (= '("12/30/1973" "12/31/1973" "01/01/1974") (take 3 (get-dates "12/30/1973"))))
  (is (= "04/19/2014" (nth (get-dates "04/18/2014") 1)))
  (is (= "04/20/2014" (nth (get-dates "04/18/2014") 2)))
  (is (= "03/19/2014" (nth (get-dates "01/01/2014") 77)))
  (is (= "04/18/2014" (nth (get-dates "01/01/1970") 16178)))

)
