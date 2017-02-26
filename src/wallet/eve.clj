(ns wallet.eve
  "eve api related helpers"
  (:require [clojure.data.xml :as xml]
            [clojure.string :as str]
            [clojure.tools.logging :as log]))

;; will do this better at some point
(def api {:keyID "<opsec>"
          :vCode "<opsec>"})

(def eve-api-url "https://api.eveonline.com/char/WalletJournal.xml.aspx")

(defn- query-params
  "takes a map of options and converts into a url encoded query string ready for appending"
  [options]
  (->> options
       (keep (fn [opt-pair]
               (let [param (name (first opt-pair))
                     value (second opt-pair)]
                 (if (and param value)
                   (str param "=" value)
                   nil))))
       (str/join "&")))

(defn- tag-contents
  "Gets the a list of xml data structures for all children that match the tag name"
  [xml tag-name]
  (filter #(= tag-name (:tag %))
          (:content xml)))

(defn- str->num
  "converts a string to a number, returning nil on failure"
  [num-str]
  (let [n (read-string num-str)]
    (if (number? n) n nil)))

(defn- data-rows
  "Extracts the row data from the xml, converting to a list of maps of attributes of each row"
  [xml]
  (-> (tag-contents xml :result)
      (first)
      (tag-contents :rowset)
      (first)
      (tag-contents :row)
      (->> (map :attrs))))

(defn get-transactions
  [start num]
  (let [query-url (str eve-api-url "?" (query-params (merge {:fromID start
                                                             :rowCount num}
                                                            api)))]
    (log/info "Querying API: " query-url)
    (-> (slurp query-url)
        (xml/parse-str)
        (data-rows))))

(defn- min-ref-id
  "Gets the smallest refID in the current batch of transactions, use to walk backwards"
  [batch]
  (apply min (map #(str->num (:refID %)) batch)))

(defn get-all-transactions
  "Recursively grab all transactions and merge together"
  []
  (loop [start nil
         transactions '()]
    (let [batch (get-transactions start 2560)
          batch-size (count batch)]
      (log/info "Batch size: " batch-size)
      (log/info "Starting refd: " start)
      (if (= 0 batch-size)
        transactions
        (recur (min-ref-id batch)
               (concat transactions batch))))))

(defn- filter-transactions
  "filters transactions by one or more parameters"
  [filters]
  )

(defn balance-info
  "Takes a list of transactions and returns the sum of all transactions" 
  [filters]
  (->> (get-all-transactions)
       (reduce (fn [acc t]
                 (let [amount (str->num (:amount t))
                       adder-fn (partial + amount)]
                   (as-> acc res
                     (update res :delta adder-fn)
                     (if (> amount 0)
                       (update res :totalIncome adder-fn)
                       (update res :totalExpense adder-fn)))))
               {:delta 0
                :totalIncome 0
                :totalExpense 0})))

(defn stations
  "Takes a list of transactions and returns a map of stations and number transactions at each"
  []
  (reduce (fn [stations t]
            (let [station (:argName1 t)]
              (if (string? station)
                (update stations 
                        station
                        (fnil inc 0))
                stations)))
          {}
          (get-all-transactions)))

