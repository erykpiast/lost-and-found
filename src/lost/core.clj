(ns lost.core
  (:require [clojure.data.json :as json])
  (:require [clojure.string :as str])
  (:gen-class))

(defn tap
  "Call function with argument, ignore its result and return that argument"
  [f]
  (fn [arg] (f arg) f)
)

(defn parse-int
  [str]
  (read-string (second (re-find #"^0*(\d+)" str)))
)

(defn parse-resolution
  "Parse resolution string (WxH) to map"
  [restring]
  (let [[ w h ] (str/split restring #"x")]
    {:width (parse-int w)
     :height (parse-int h) }))

(defn parse-date
  "Parse EXIF date to map"
  [exif-date]
  (if (= 0 (count exif-date))
    nil
    (let [[ date time ] (str/split exif-date #" ")
          [ year month day ] (str/split date #":")
          [ hour minute second ] (str/split time #":")]
      {
       :year (parse-int year)
       :month (parse-int month)
       :day (parse-int day)
       :hour (parse-int hour)
       :minute (parse-int minute)
       :second (parse-int second)
       })))

(defn parse-line
  "Parse single line to file descriptor; line format:
  filename;[camera];WxH;sizeInBytesB;[YYYY:MM:DD HH:MM:SS];"
  [line]
  (let [[
         filename
         camera
         resolution
         size
         date
         ] (str/split line #";")]
    {
     :filename filename
     :camera camera
     :resolution (parse-resolution resolution)
     :size (parse-int size)
     :date (parse-date date) }))

(def parse-file #(map parse-line (str/split-lines %)))

(def not-empty? (complement empty?))

(def keep-only-with-camera #(filter (comp not-empty? :camera) %))

(def group-by-year (partial group-by #(get-in % [:date :year])))

(def group-by-month (partial group-by #(get-in % [:date :month])))

(defn cons-in
  "Cons item to sequence existing under given path in given map"
  [map path item]
  (update-in map path #(cons item %))
)

(defn map-values
  "Iterate over values of a hashmap creating new one"
  [f coll]
  (into {} (map (fn [[key value]] [key (f value)]) coll))
)

(defn filter-values
  [p coll]
  (into {} (filter (fn [[key value]] [key (p value)]) coll))
)

(defn
  partition-by-date-present
  [coll]
  (group-by
   #(if (:date %) :withDate :withoutDate)
   coll))

(defn
  partition-by-camera-present
  [coll]
  (group-by
   #(if (empty? (:camera %)) :withoutCamera :withCamera)
   coll))

(defn
  extract-field
  [field]
  (let [e (fn e [tree] (if (contains? tree field)
                          (field tree)
                          (if (map? tree)
                            (map-values e tree)
                            (map e tree))))]
    e)
)

(def extract-filename (extract-field :filename))
(def extract-camera (extract-field :camera))
(defn keep-by-canon-only
  [coll]
  (filter-values #(= "Canon EOS 450D" (:camera %)) coll))

(defn create-pictures-json
  [filename]
  (let [result (-> filename
                   slurp
                   parse-file
                   partition-by-camera-present
                   (update-in [:withCamera] partition-by-date-present)
                   (update-in [:withCamera :withDate] (comp
                                                       #(map-values keep-by-canon-only %)
                                                       
                                                       #(map-values group-by-month %)
                                                       group-by-year))
                   extract-filename
                   )]
    (json/pprint result)))

(def call1 #(%1 %2))


(defn flatten-tree
  ([name tree]
   (condp call1 tree
     map? (flatten
           (map
            (fn [[k v]]
              (flatten-tree
               (str name "/" k)
               v))
            tree))
     coll? (flatten (map (partial flatten-tree name) tree))
     (str name "/" tree)))
  ([tree] (flatten-tree "" tree))
)


(defn source-dest
  [file-path]
  [(re-find #"/[^/]+$" file-path) file-path])

(def map-> #(map %2 %1))

(def new-lines (partial clojure.string/join "\n"))

(defn mv
  [[[src dest] & tail]]
  (if (empty? tail)
    true
    (recur tail))
)

(defn move-files
 [filename ext source-dir dest-dir]
 (-> filename
     slurp
     json/read-str
     flatten-tree
     (map-> source-dest)
     (map-> (partial map #(str % "." ext)))
     (map-> (fn [[src dest]]
              [(str source-dir src) (str dest-dir dest)]))
     (map-> (fn [[src dest]] (str "cp -av " src " " dest)))
     (new-lines)))


(defn -main1
  [filename]
  (create-pictures-json filename))

(defn -main
  [& args]
  (println (apply move-files args))
)


