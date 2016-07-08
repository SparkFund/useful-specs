# useful-specs

A collection of generally useful specs with generators.

The current focus is Internet-related values: hostnames, email
addresses, urls, etc.

`[sparkfund/useful-specs "0.1.0"]`

## Usage

``` clj
(require '[clojure.spec :as s])
(require '[specs.internet :as si])

(s/conform ::si/email-address "user@example.com") ; => "user@example.com"
(s/conform (s/hostname :domains ["example.com"]) "www.example.com") ; => "www.example.com"
```

## License

Copyright © 2016 SparkFund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
