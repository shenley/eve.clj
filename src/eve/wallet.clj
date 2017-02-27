(ns eve.wallet
  "eve api related helpers"
  (:require [clojure.data.xml :as xml]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [clj-time.core :as t]
            [clj-time.format :as f]))

(def eve-api-url "https://api.eveonline.com/corp/WalletJournal.xml.aspx")

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
  "Get transactions via REST api,
   * api    map representing eve api key. Must contain :keyID and :vCode fields.
   * start  record t start from (used when getting multiple pages), nil for first page.
   * num    number of records to fetch"
  [api start num]
  (let [query-url (str eve-api-url "?" (query-params (merge {:fromID start
                                                             :rowCount num
                                                             :accountKey 1000}
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
  [api]
  (loop [start nil
         transactions '()]
    (let [batch (get-transactions api start 2560)
          batch-size (count batch)]
      (log/info "Batch size: " batch-size)
      (log/info "Starting refd: " start)
      (if (= 0 batch-size)
        transactions
        (recur (min-ref-id batch)
               (concat transactions batch))))))


;;
;; {:refTypeID (partial = 2)
;;  :
;;
;;
;;

(defn filter-transaction
  [filters transaction]
  (reduce-kv (fn [match filter condition]
               (and match
                    (if condition
                      (condition (get transaction filter))
                      true)))
             true
             filters))

(defn filter-transactions
  "filters transactions by one or more parameters"
  [filters transactions]
  (->> transactions
       (filter (partial filter-transaction filters))))

(defn balance-info
  "Takes a list of transactions and returns the sum of all transactions

  * Filters consist of 2 parts
    * a keyname matching the keyname in a transaction (xml attribute name from api)
    * a predicate function evaluat=ing whether a transaction shoulkd be kept or removed

  Example:
  {:keyTypeID (partial = \"85\") ;; matches Bountry Prizes type
   :date (fn [date]
           (t/after? (f/parse date) (t/minus (t/now) (t/weeks 1))))}" 
  [api filters]
  (let [transactions (get-all-transactions api)]
    (->> transactions
         (filter-transactions filters)
         (reduce (fn [acc t]
                   (let [amount (str->num (:amount t))
                         adder-fn (partial + amount)]
                     (as-> acc res
                       (update res :delta adder-fn)
                       (if (> amount 0)
                         (update res :totalIncome adder-fn)
                         (update res :totalExpense adder-fn))
                       (update res :count inc))))
                 {:delta 0
                  :totalIncome 0
                  :totalExpense 0
                  :count 0}))))


