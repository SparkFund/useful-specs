(ns specs.string
  "Experiments expressing string specs as char seq specs"
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.string :as string]))

(defn charseq-in
  [& {:keys [count min-count max-count chars]}]
  (s/coll-of (or chars char?)
             :count count
             :min-count min-count
             :max-count max-count))

(s/def ::count
  (s/or :zero zero? :positive pos-int?))

(s/def ::min-count
  ::count)

(s/def ::max-count
  ::count)

(s/def ::chars
  (s/coll-of char? :kind set? :into #{}))

(s/fdef charseq-in
        :args (s/keys* :opt-un [::count ::min-count ::max-count ::chars])
        :ret s/spec?)

(defn string-like
  [spec-or-k]
  (let [spec (#'s/specize spec-or-k)]
    (reify s/Spec
      (conform* [_ s]
        (if (string? s)
          (s/conform* spec (vec s))
          ::s/invalid))
      (unform* [_ cs]
        (string/join (s/unform* spec cs)))
      (explai
          n* [_ path via in x]
        ;; TODO possibly stringify the problems
        (s/explain* spec path via in (vec x)))
      (gen* [_ overrides path rmap]
        (gen/fmap string/join (s/gen* spec overrides path rmap)))
      (with-gen* [_ gfn]
        (throw (Exception. "Not yet implemented")))
      (describe* [_]
        ;; TODO ns qualify the string-like symbol?
        (cons `string-like (s/describe* spec))))))

(defn string-in
  [& options]
  (string-like (apply charseq-in options)))

(s/fdef string-in
        :args (s/keys* :opt-un [::count ::min-count ::max-count ::chars])
        :ret s/spec?)

(s/def ::hostname-charseq
  (let [chars (set (str "abcdefghijklmnopqrstuvwxyz"
                        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                        "0123456789"))
        chars-plus-hyphen (conj chars \-)]
    (s/and
     (charseq-in :chars chars-plus-hyphen :min-count 1 :max-count 64)
     (s/cat :prefix (s/+ chars)
            :sections (s/* (s/cat :hyphen (s/+ #{\-})
                                  :suffix (s/+ chars)))))))

(s/def ::hostname
  (string-like ::hostname-charseq))
