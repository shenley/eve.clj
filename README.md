# wallet

Will be evolving over time. Currently it can pull market transaction for characters and corporations, and apply some filters.

## Usage

REPL usage only for now. Maybe I'll make a simple text UI as well :D. This is meant to be a library for use in other applications.


## Examples

Get totals for all transactions of type "85" (Bounty Prizes) in the time range (previous week)

```clojure
(eve.wallet/balance-info 
              {:keyID "<key id>"
               :vCode "<verification code>"}
              {:refTypeID (partial = "85")
               :date (fn [date]
                       (t/after? (f/parse date) (t/minus (t/now) (t/weeks 1))))})
```

## License

Copyright © 2017 Sean Henley

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
