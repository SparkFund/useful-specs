(ns specs.internet
  "Provides fns and specs for common representations of Internet-related values"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.test.check.generators :as gen])
  (:import [java.net URI]
           [java.util.regex Pattern]))

(s/def ::hostpart
  (letfn [(pred [s]
            (re-matches #"\A(?:\p{Alnum}|\p{Alnum}(?:\p{Alnum}|-)*\p{Alnum})\z" s))
          (gen []
            (let [middle-char (gen/fmap char
                                        (gen/one-of [(gen/choose 48 57)
                                                     (gen/choose 65 90)
                                                     (gen/choose 97 122)
                                                     (gen/return 45)]))]
              (gen/let [length (gen/choose 1 64)]
                (let [chars-gen (if (= 1 length)
                                  (gen/vector gen/char-alphanumeric 1)
                                  (gen/let [first-char gen/char-alphanumeric
                                            last-char gen/char-alphanumeric
                                            middle-chars (gen/vector middle-char
                                                                     (- length 2))]
                                    (gen/return (-> [first-char]
                                                    (into middle-chars)
                                                    (conj last-char)))))]
                  (gen/fmap string/join chars-gen)))))]
    (s/spec pred :gen gen)))

(defn hostname
  "Returns a spec for an Internet hostname that conforms to RFC1123. Options may
   be given via varargs:

   :domains - the set of allowed domains
   :min-depth - the minimum number of parts
   :max-depth - the maximum number of parts"
  [& options]
  (let [{:keys [domains min-depth max-depth]} options
        hostpart-spec (s/get-spec ::hostpart)
        domain-re (when (seq domains)
                    (re-pattern (str "\\."
                                     (string/join "|" (map #(Pattern/quote (string/lower-case %)) domains))
                                     "\\Z")))]
    (letfn [(pred [s]
              (and (>= 253 (count s))
                   (let [parts (string/split s #"\.")]
                     (and (or (not max-depth)
                              (>= max-depth (count parts)))
                          (or (not min-depth)
                              (>= (count parts) min-depth))
                          (not (string/starts-with? s "."))
                          (not (string/ends-with? s "."))
                          (every? (partial s/valid? hostpart-spec) parts)
                          (or (not domain-re)
                              (re-find domain-re (string/lower-case s)))))))
            (gen []
              (let [min-needed (or min-depth 2)
                    max-needed (or max-depth 4)
                    domain-part (when (seq domains)
                                  (gen/elements domains))]
                (if domain-part
                  (gen/let [domain domain-part]
                    (let [domain-count (count (string/split domain #"\."))
                          parts-gen (gen/vector (s/gen hostpart-spec)
                                                (- min-needed domain-count)
                                                (- max-needed domain-count))]
                      (gen/let [parts parts-gen]
                        (gen/return (string/join "." (conj parts domain))))))
                  (let [parts-gen (gen/vector (s/gen hostpart-spec)
                                              min-needed max-needed)]
                    (gen/fmap (partial string/join ".") parts-gen)))))]
      (s/spec pred :gen gen))))

(def tlds
  "The set of top-level domains assigned by the IANA as of 2016-07-07 from
   http://data.iana.org/TLD/tlds-alpha-by-domain.txt"
  (->> (rest (line-seq (io/reader (io/resource "tlds-alpha-by-domain.txt"))))
       (map string/lower-case)
       (into #{})))

(s/def ::fully-qualified-hostname
  (hostname :domains tlds :min-depth 2))

(s/def ::fully-qualified-common-hostname
  (hostname :domains #{"com" "edu" "net" "org"} :min-depth 2 :max-depth 4))

(s/def ::common-hostname
  (let [common-aliases #{"localhost"}]
    (s/spec
     (s/or :alias common-aliases :hostname ::fully-qualified-common-hostname)
     :gen #(gen/one-of [(gen/elements common-aliases)
                        (s/gen (s/get-spec ::fully-qualified-common-hostname))]))))

(s/def ::ip-address
  (letfn [(pred [s]
            (let [parts (string/split s #"\.")]
              (and (= (count parts) 4)
                   (every? (fn [part]
                             (try
                               (let [n (edn/read-string part)]
                                 (and (integer? n)
                                      (>= 256 n 0)))
                               (catch Exception _ false)))
                           parts))))
          (gen []
            (gen/fmap (partial string/join ".") (gen/vector (gen/choose 0 255) 4)))]
    (s/spec pred :gen gen)))

(s/def ::ip-port
  (s/int-in 1 65536))

(s/def ::privileged-ip-port
  (s/int-in 1 1024))

(s/def ::unprivileged-ip-port
  (s/int-in 1025 65536))

(s/def ::local-email-part
  (let [chars (set (str "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                        "abcdefghijklmnopqrstuvwxyz"
                        "0123456789"
                        "!#$%&'*+-/=?^_`{|}~"))
        dot-chars (conj chars \.)]
    (letfn [(pred [s]
              (and (>= 64 (count s) 1)
                   (not (string/starts-with? s "."))
                   (not (string/ends-with? s "."))
                   (every? dot-chars s)
                   (not (re-find #"\.\." s))))
            (gen []
              (gen/fmap string/join (gen/vector (gen/elements chars) 1 64)))]
      (s/spec pred :gen gen))))

(defn email-address
  [& options]
  (let [{:keys [hosts domains]} options
        hostname-spec (hostname :domains domains)
        local-spec (s/get-spec ::local-email-part)
        hosts (when (seq hosts) (set (map string/lower-case hosts)))]
    (letfn [(pred [s]
              (let [parts (string/split s #"@")]
                (and (= 2 (count parts))
                     (s/valid? local-spec (first parts))
                     (if (seq hosts)
                       (contains? hosts (string/lower-case (second parts)))
                       (s/valid? hostname-spec (second parts))))))
            (gen []
              (gen/let [local-part (s/gen local-spec)
                        hostname-part (if (seq hosts)
                                        (gen/elements hosts)
                                        (s/gen hostname-spec))]
                (gen/return (str local-part "@" hostname-part))))]
      (s/spec pred :gen gen))))

(s/def ::email-address
  (email-address :domains tlds))

(s/def ::common-email-address
  (email-address :hosts #{"gmail.com" "yahoo.com" "outlook.com" "aol.com"}))

(defn url
  [& options]
  (let [{:keys [schemes hosts]} options
        schemes (when (seq schemes) (set schemes))
        hosts (when (seq hosts) (set hosts))]
    (letfn [(pred [s]
              (try
                (let [uri (URI. s)]
                  (and (or (not (seq schemes))
                           (contains? schemes (.getScheme uri)))
                       (or (not (seq hosts))
                           (contains? hosts (.getHost uri)))))
                (catch Exception _ false)))
            (gen []
              (let [schemes (or schemes #{"http" "https"})]
                (gen/let [scheme (gen/elements schemes)
                          host (if (seq hosts)
                                 (gen/elements hosts)
                                 (s/gen ::fully-qualified-hostname))]
                  (str scheme "://" host "/"))))]
      (s/spec pred :gen gen))))

(s/def ::web-url
  (url :schemes #{"http" "https"}))
