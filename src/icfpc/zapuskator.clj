(ns icfpc.zapuskator
  (:require
   [clj-http.client :as http]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.shell :as sh]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [jsonista.core :as json]
   [icfpc.main :as main])
  (:import
   [java.lang ProcessBuilder$Redirect]
   [java.util Date]
   [java.util.concurrent Executors TimeUnit]))

(defn area [{:keys [hole]}]
  (let [min-x (reduce min (map first hole))
        max-x (reduce max (map first hole))
        min-y (reduce min (map second hole))
        max-y (reduce max (map second hole))]
    (* (- max-x min-x) (- max-y min-y))))

(defn factorial [k n]
  (try
    (reduce * 1 (range k (inc n)))
    (catch Exception e
      Double/MAX_VALUE)))

(defn update-complexity [problem]
  (let [{:keys [id area hole figure submitted best]} problem
        {:keys [edges vertices]} figure
        complexity (if (= 0 best)
                     (*
                       (factorial (- (count vertices) (count hole)) (count vertices))
                       (Math/pow area (- (count vertices) (count hole))))
                     (Math/pow area (count vertices)))]
    (assoc problem :complexity complexity)))

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
                      (assoc :id id :submitted submitted :best best)))
                   (map #(-> % (assoc :area (area %)) update-complexity))
                   (sort-by (juxt
                              :complexity
                              #(if (nil? (:submitted %)) 0 1)
                              #(count (:vertices (:figure %))))))]
    (doseq [{:keys [id area hole figure complexity submitted best] :as problem} problems]
      (println (str "id=" id
                 ", complexity=" complexity
                 ", vertices=" (count (:vertices figure))
                 ", area=" area
                 ", submitted=" submitted
                 ", best=" best)))

    (let [pool      (Executors/newFixedThreadPool 8)
          *done     (atom 0)
          *killed   (atom 0)
          *failures (atom 0)
          lock      (Object.)]
      (doseq [problem problems]
        (.submit pool
          ^Runnable (fn []
            (try
              (locking lock
                (println (str "Done: " @*done ", failed: " @*failures ", killed: " @*killed ", left: " (- (count problems) @*done @*failures @*killed))))
              (let [pb      (ProcessBuilder. (into-array String ["./bruteforce" (str (:id problem)) (str (:best problem))]))
                    buffer  (make-array Byte/TYPE 102400)
                    _       (.redirectErrorStream pb true)
                    process (.start pb)
                    input   (.getInputStream process)]
                (future
                  (when-not (.waitFor process 30 TimeUnit/MINUTES)
                    (locking lock
                      (println "Killing" (:id problem) "after 10 seconds"))
                    (swap! *killed inc)
                    (.destroyForcibly process)))
                (loop []
                  (let [size (.read input buffer)]
                    (when (pos? size)
                      (do
                        (locking lock
                          (.write System/out buffer 0 size)
                          (.flush System/out))
                        (recur)))))
                (swap! *done inc))
              (catch Exception e
                (locking lock
                  (println (str "! [" (:id problem) "] Failed " (.getMessage e))))
                (swap! *failures inc)))))))))
