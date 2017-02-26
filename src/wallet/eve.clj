(ns wallet.eve
  "eve api related helpers"
  (:require [clojure.data.xml :as xml]
            [clojure.string :as str]))

(def api {:keyID "6026950"
          :vCode "Pcma9PzxQ1fhaH1RdAMnZsoMCLdsMCuZCW4UfIMkZ28xH02At7mmR9qZMkuEbInz"})

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
  [xml tag-name]
  (filter #(= tag-name (:tag %))
          (:content xml)))

(defn- str->num
  "converts a string to a number, returning nil on failure"
  [str]
  (let [n (read-string str)]
    (if (number? n) n nil)))

(defn- data-rows
  [xml]
  (-> (tag-contents xml :result)
      (first)
      (tag-contents :rowset)
      (first)
      (tag-contents :row)
      (->> (map :attrs))))

(defn get-transactions
  [start num]
  (-> (slurp (str eve-api-url "?" (query-params (merge {:fromID start
                                                        :rowCount num}
                                                       api))))
      (xml/parse-str)
      (data-rows)))


(defn- min-ref-id
  [batch]
  (-> (map #(str->num (:refID %)) batch)
      (min)))

(defn get-month-transactions
  "Recursively grab all transactions and merge together"
  []
  (loop [start nil
         transactions '()]
    (let [batch (get-transactions start 2560)
          batch-size (count batch)]
      (println (str "Batch size: " batch-size))
      (if (= 0 batch-size)
        transactions
        (recur (min-ref-id batch)
               (concat transactions batch))))))

(defn balance-info
  "Takes a list of transactions and returns the sum of all transactions" 
  []
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
           :totalExpense 0}
          (get-month-transactions)))

(defn stations
  "Takes a list of transactions and returns a map of stations and number transactions at each"
  [transactions]
  (reduce (fn [stations t]
            (let [station (:argName1 t)]
              (if (string? station)
                (update stations 
                        station
                        (fnil inc 0))
                stations)))
          {}
          transactions))
