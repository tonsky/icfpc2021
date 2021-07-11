(ns icfpc.reaper
  (:require
   [clj-http.client :as http]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [jsonista.core :as json]
   [icfpc.main :as main]))

(defn area [{:keys [hole]}]
  (let [min-x (reduce min (map first hole))
        max-x (reduce max (map first hole))
        min-y (reduce min (map second hole))
        max-y (reduce max (map second hole))]
    (* (- max-x min-x) (- max-y min-y))))

(defn -main [& args]
  (let [dir      (io/file "problems")
        problems (->>
                   (for [file (next (file-seq dir))
                         :let [[_ id] (re-matches #"(\d+).problem" (.getName file))
                               id (Long/parseLong id)
                               {:keys [submitted best]} (get @main/*ladder id)]
                         :when (not= submitted best)]
                     (-> file
                      (slurp)
                      (json/read-value json/keyword-keys-object-mapper)
                      (assoc :id id)))
                   (map (fn [problem]
                          (let [area (area problem)]
                            (assoc problem :area area :complexity (Math/pow area (count (:vertices (:figure problem))))))))
                   (sort-by :complexity))]
    (println (str/join "\n" (map (juxt :id :area #(count (:vertices (:figure %)))) problems)))))

