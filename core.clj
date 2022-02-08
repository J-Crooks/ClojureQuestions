(ns taskone.core
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.set :as set])
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Squaring Numbers;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn squareNumbers
  [& numbers]
  (into [] (map #(if (integer? %)
                   (* % %)
                   (Double/parseDouble (format "%.2f" (* % %))))
                (remove (complement number?) numbers))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Counting Coins ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn CountingCoins
  [amount coins]
  (defn backtrack [tree]
    (loop [newTree tree i (dec (count tree))]
      (if (> i -1)
        (if (= (dec (count (vec (sort coins)))) ((last newTree) :cp))
          (recur (pop newTree) (dec i))
          (assoc-in newTree [(dec (count newTree)) :cp] (inc ((last newTree) :cp))))
        newTree)))

  (defn getNextAmount [tree]
    (- ((last tree) :amount) (get (vec (sort coins)) ((last tree) :cp))))

  (defn addBranch [tree]
    (conj tree {:amount (- ((last tree) :amount) (get (vec (sort coins)) ((last tree) :cp)))
                :cp     (get (last tree) :cp)}))

  (defn checkPara []
    (if (and (integer? amount) (vector? coins))
      (if (every? int? coins)
        true
        false)
      false))

  (if (checkPara)
    (loop [tree [{:amount amount :cp 0}] perm 0]
      (if (empty? tree)
        (str perm " unique combinations!")
        (if (= 0 (getNextAmount tree))
          (recur (backtrack tree) (inc perm))
          (if (< (getNextAmount tree) 0)
            (recur (backtrack tree) perm)
            (recur (addBranch tree) perm)))))
    "Inputs not valid! [Integer Vector[Integer]] required"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Kinder Gardeners ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn kinderGardeners
  [child]
  (def children ["Alice"
                 "Bob"
                 "Charlie"
                 "David"
                 "Eve"
                 "Fred"
                 "Ginny"
                 "Harriet"
                 "Ileana"
                 "Joseph"
                 "Kincaid"
                 "Larry"])
  (defn getFlowers
    [index]
    (def window ["VRCGVVRVCGGCCGVRGCVCGCGV"
                 "VRCCCGCRRGVCGCRVVCVGCGCV"])
    (defn matchLetter [letter]
      (cond
        (= letter "R") "radishes"
        (= letter "G") "grass"
        (= letter "V") "violets"
        (= letter "C") "clover"))

    (mapv #(matchLetter %) (flatten (map #(str/split (subs % (* index 2) (+ (* index 2) 2)) #"(?!^)") window))))
  (if (some #{child} children)
    (getFlowers (.indexOf children child))
    "No child with this name exists!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Temperature Records ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;--------------------------;;DATA;;--------------------------;;
(defn getData []
  (defrecord line [Year Day Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec])
  (defn toint [coll]
    (mapv #(Integer/parseInt %) coll))
  (let [src (slurp "https://www.metoffice.gov.uk/hadobs/hadcet/cetdl1772on.dat")
        lines (str/split src #"\n")
        words (mapv #(str/split (.trim %) #"\s*\s+") lines)
        values (mapv #(toint %) words)
        records (mapv #(apply ->line %) values)]
    records))
;;TYPE STRUCTURED DATA
(def database (getData))
(def splitData (partition 31 database))
(def months ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])
;;HELPER FUNCTIONS;;
(defn removeEmptyData [coll]
  (filter #(not= -999 %) coll))
(defn average [coll]
  (Double/parseDouble (format "%.2f" (double (/ (reduce + coll) (count coll))))))
(defn makeYear [coll]
  (mapv #(assoc % 0 (+ 1772 (first %))) coll ))
;;--------------------------;; Warmest Day per Month ;;--------------------------;;
(defn MaxMonth []
  (defn getDates [month]
    (defn getInfo [coll keyname]
      {:Day (str (get coll :Day) "/" keyname "/" (get coll :Year)) :Temp (get coll (keyword keyname))})
    (defn maxTemperature [keyname]
      (apply max (map (keyword keyname) database)))
    (map #(getInfo % month) (filter #(= (maxTemperature month) (get % (keyword month))) database)))

  (defn display [coll]
    {:Temp (get (into [] (distinct (map :Temp coll))) 0) :Date (into [] (map :Day coll))})

  (mapv #(display %) (map #(getDates %) months)))
;;--------------------------;; Maximum and Minimum Year ;;--------------------------;;
(defn MinMaxMeanYears []
  (defn getMinMax []
    (defn getMean [coll]
      (average (removeEmptyData (apply concat (map #(vals (dissoc % :Year :Day)) coll)))))
    (def means (map-indexed vector (mapv #(getMean %) splitData)))
    (map #(assoc % 0 (+ 1772 (get % 0))) (filter #(or (= (apply max-key second means) %) (= (apply min-key second means) %)) means)))

  (zipmap [:Cold :Warm] (map #(zipmap [:Year :Temp] %) (getMinMax))))
;;--------------------------;; Mean Temperature per Month ;;--------------------------;;
(defn MeanMonth []
  (defn YearMonthMeans []
    (defn monthlyMeansPerYear [yeardata]
      (map #(average (removeEmptyData (map (keyword %) yeardata))) months))
    (defn organiseMeans [meanData]
      (map #(concat (vector (first %)) (second %)) (makeYear (map-indexed vector meanData))))
    (defn addKeys [data]
      (map #(zipmap [:Year :Jan :Feb :Mar :Apr :May :Jun :Jul :Aug :Sep :Oct :Nov :Dec] %) data))
    (addKeys (organiseMeans (mapv #(monthlyMeansPerYear %) splitData))))

  (defn databaseMonthMeans []
    (defn getMean [keyname]
      {:Month keyname :Temp (average (removeEmptyData (map (keyword keyname) database)))})
    (sort-by :Temp (map #(getMean %) months)))

  (defn getVariants [monthMeanData]
    (def yearMeans (YearMonthMeans))
    (def keyname (keyword (monthMeanData :Month)))
    (defn closeORfar [x coll op]
      (op (sort-by #(Math/abs (- x %)) coll)))
    (defn getClosestRec [func]
      (def closeORfarTemp (closeORfar (get monthMeanData :Temp) (map keyname yearMeans) func))
      (set/rename-keys (select-keys (into {} (filter #(= closeORfarTemp (% keyname)) yearMeans)) [:Year keyname]) {keyname :Temp}))
    {:CloseVar (getClosestRec first) :GreatVar (getClosestRec last)})

  ;(mapv #(conj % (getVariants %)) (databaseMonthMeans)))
  (YearMonthMeans))
;;--------------------------;; Maximum Seasonal Temperature Per Year ;;--------------------------;;
(defn MaxTempSeasonPerYear []
  (def seasonoffset [{:offset 0 :season "Spring"} {:offset 1 :season "Summer"} {:offset 2 :season "Autumn"} {:offset 3 :season "Winter"}])

  (defn getSeasonData [coll offset]
    (defn partIntoSeasons [coll]
      (map-indexed vector (reduce concat (map #(partition 3 (map (into {} %) [:Mar :Apr :May :Jun :Jul :Aug :Sep :Oct :Nov :Dec :Jan :Feb])) coll))))
    (map second (filter #(= 0 (mod (- (first %) (offset :offset)) 4)) (partIntoSeasons coll))))

  (defn getAvergePerSeasonYear [coll]
    (map #(average %) (map #(removeEmptyData %) (map #(apply concat (getSeasonData coll %)) seasonoffset))))

  (def means (map #(zipmap [:Year :Spring :Summer :Autumn :Winter] %) (makeYear (mapv #(into [] (flatten %)) (map-indexed vector (mapv #(getAvergePerSeasonYear %) splitData))))))

  (defn getMax [keyname coll]
    (select-keys (apply max-key (keyword keyname) coll) [:Year (keyword keyname)]))

  (mapv #(getMax (% :season) means) seasonoffset))
;;--------------------------;; Median, Mode, Range per Month ;;--------------------------;;
(defn Statistics []
  (defn getMedianModeRange [keyname]
    (def sorted (into [] (sort (removeEmptyData (map (keyword keyname) database)))))
    (def middle (double (dec (/ (count sorted) 2))))
    (def median
      (if (odd? (count sorted))
        (/ (+ (get sorted (int (Math/ceil middle))) (get sorted (int (Math/floor middle)))) 2)
        (get sorted (int middle))))
    (def mode (first (last (sort-by second (frequencies sorted)))))
    (def getrange (- (last sorted) (first sorted)))
    {:Month keyname :Median median :Mode mode :Range getrange})
  (mapv #(getMedianModeRange %) months))