# useful-specs

A collection of generally useful specs with generators.

The current focus is Internet-related values: hostnames, email
addresses, urls, etc.

`[sparkfund/useful-specs "0.1.1"]`

## Usage

``` clj
(require '[clojure.spec :as s])
(require '[specs.internet :as si])

(s/conform ::si/email-address "user@example.com") ; => "user@example.com"
(s/conform (si/hostname :domains ["example.com"]) "www.example.com") ; => "www.example.com"

(require '[specs.number :as number])

(s/conform (number/decimal-in :scale 3 :precision 1) 1.3) ; => 1.3
```

## License

Copyright © 2016 SparkFund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
