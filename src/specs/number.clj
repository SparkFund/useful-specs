(ns specs.number
  (:require [clojure.spec :as s]
            [clojure.test.check.generators :as gen])
  (:import [java.math BigDecimal MathContext RoundingMode]))

(defn decimal-in
  "Specs a decimal number. The number type may be anything that bigdec
   accepts. Options:

    :precision - the number of digits in the unscaled value (default none)
    :scale     - the number of digits to the right of the decimal (default none)
    :min       - minimum value (inclusive, default none)
    :max       - maximum value (inclusive, default none)

   A decimal satifies this spec if its precision and scale are not greater
   than the specified precision and scale, if given.

   Note that the java math definition of precision and scale may not be the
   same as e.g. your database. For example, -1E-75M has a precision of 1 and a
   scale of 75. For sanest results, you should specify both, though the spec
   does not require both."
  [& options]
  (let [{:keys [precision scale min max]} options]
    (when (or precision scale min max)
      (prn precision scale min max))
    (letfn [(pred [d]
              (try
                (let [d (bigdec d)]
                  (and (or (not precision)
                           (>= precision (.precision d)))
                       (or (not scale)
                           (let [d-scale (.scale d)]
                             (and (not (neg? d-scale))
                                  (>= scale d-scale))))
                       (or (not min)
                           (>= d min))
                       (or (not max)
                           (>= max d))))
                (catch Exception _ false)))
            (gen []
              (let [min (or min
                            (and precision
                                 (-> BigDecimal/ONE
                                     (.movePointRight precision)
                                     dec
                                     .negate)))
                    max (or max
                            (and precision
                                 (-> BigDecimal/ONE
                                     (.movePointRight precision)
                                     dec)))]
                (gen/let [p (gen/double* {:infinite? false :NaN? false :min min :max max})]
                  (let [mc (when precision
                             (MathContext. precision RoundingMode/HALF_UP))]
                    (cond-> (bigdec p)
                      scale
                      (.setScale scale BigDecimal/ROUND_HALF_UP)
                      precision
                      (.round mc))))))]
      (s/spec pred :gen gen))))

(s/def ::precision
  pos-int?)

(s/def ::scale
  (s/spec (fn [x] (and (int? x) (not (neg? x))))
          :gen #(gen/large-integer* {:min 0})))

(s/def ::min
  number?)

(s/def ::max
  number?)

(s/fdef decimal-in
  :args (s/and (s/keys* :opt-un [::precision ::scale ::min ::max])
               #(let [{:keys [min max precision scale]} %]
                  (and (or (not (and min max))
                           (>= max min))
                       (or (not precision)
                           (pos? precision))
                       (or (not scale)
                           (not (neg? scale)))
                       (or (not (and precision scale))
                           (>= precision scale)))))
  :ret s/spec?
  :fn #(let [{:keys [ret args]} %
             {:keys [min max]} args]
         (and (or (not min)
                  (s/valid? ret min))
              (or (not max)
                  (s/valid? ret max)))))
