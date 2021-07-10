(ns icfpc.main
  (:require
   [clj-http.client :as http]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [jsonista.core :as json])
  (:import
   [java.util Date]
   [org.jetbrains.skija BackendRenderTarget ColorSpace DirectContext Font FontStyle Matrix33 Paint PaintMode Path PixelGeometry Surface SurfaceColorFormat SurfaceOrigin SurfaceProps Typeface]
   [org.jetbrains.jwm App EventFrame EventKeyboard EventMouseMove EventReconfigure EventResize Key LayerMetal Window]))

(defonce *problem-id (atom nil))
(defonce *problem (atom nil))
(defonce *solution (atom nil))
(defonce *last-solution (atom nil))
(defonce *best-solution (atom nil))
(defonce *best-solution-score (atom nil))
(defonce *meta (atom nil))
(defonce *scale (atom 1))
(defonce *offset (atom [0 0]))
(defonce *key (atom nil))
(defonce *mouse (atom [0 0]))
(defonce *last-mouse (atom nil))
(defonce *vertex (atom nil))
(defonce *combinations (atom nil))
(defonce *combination-id (atom nil))
(def font (Font. (Typeface/makeFromName "SF Mono" (-> FontStyle/NORMAL (.withWeight 500))) 29.0))

(defn color
  ([^long l]
   (.intValue (Long/valueOf l)))
  ([^long l ^long alpha]
   (color
     (bit-or
       (-> (bit-and l 0x00FFFFFF))
       (-> (bit-and alpha 0xFF) (bit-shift-left 24))))))

(defn distance [[x1 y1] [x2 y2]]
  (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))))

(defn score [hole {:keys [edges vertices]}]
  (let [closest (fn [h] (apply min-key #(distance h %) vertices))]
    (->> hole
      (map (fn [h] (distance h (closest h))))
      (reduce + 0))))

(defn load-best-solution! [id]
  (reset! *best-solution nil)
  (reset! *best-solution-score nil)
  (reset! *meta nil)
  (let [dir (io/file "solutions" (str id))]
    (when (.exists dir)
      (let [min-dislikes (apply min
                           (for [file (next (file-seq dir))
                                 :when (re-matches #"(\d+).solution" (.getName file))
                                 :let [[_ dislikes] (re-matches #"(\d+).solution" (.getName file))]]
                             (Long/parseLong dislikes)))
            best-solution (-> (str "solutions/" id "/" min-dislikes ".solution") slurp (json/read-value json/keyword-keys-object-mapper))]
        (reset! *best-solution best-solution)
        (reset! *best-solution-score (score (:hole @*problem) best-solution)))
      (when (.exists (io/file dir "meta.edn"))
        (let [meta (edn/read-string (slurp (io/file dir "meta.edn")))]
          (reset! *meta meta))))))

(defn load-problem! [id]
  (let [problem (-> (str "problems/" id ".problem")
                  (slurp)
                  (json/read-value json/keyword-keys-object-mapper))
        max-x (reduce max (map first (concat (:hole problem) (:vertices (:figure problem)))))
        max-y (reduce max (map second (concat (:hole problem) (:vertices (:figure problem)))))]
    (reset! *problem problem)
    (reset! *solution (:figure problem))
    (reset! *scale (min (/ 1600 max-y) (/ 2800 max-x)))
    (reset! *offset [(-> max-x (* @*scale) (->> (- 3000)) (/ 2))
                     (-> max-y (* @*scale) (->> (- 1800)) (/ 2))])
    (reset! *vertex nil)
    (reset! *combinations nil)
    (reset! *combination-id nil)
    (load-best-solution! id)))

(add-watch *problem-id :load (fn [_ _ _ id] (load-problem! id)))

(defn combinations [xs k]
  (if (= k 1)
    (map vector xs)
    (for [x xs
          c (combinations (remove #(= % x) xs) (dec k))]
      (cons x c))))

(defn next-combination []
  (let [{:keys [hole figure epsilon]} @*problem
        holes     (count (:hole @*problem))
        vertices  (count (:vertices @*solution))
        edges-set (set (map sort (:edges @*solution)))]
    (when (>= vertices holes)
      (when (nil? @*combinations)
        (reset! *combinations (combinations (range vertices) holes))
        (reset! *combination-id -1))
      (reset! *combination-id
        (loop [cid (+ @*combination-id 1)]
          (cond
            (>= cid (count @*combinations)) (recur 0)
            (let [combination  (nth @*combinations cid) ;; [1 2 3 4 5] ...
                  permutations (combo/combinations combination 2)] ;; [1 2] [2 3] ...
              (every? true?
                (for [perm  permutations
                      :when (edges-set (sort perm))
                      :let [[from to] perm ;; [3 5]
                            [x1 y1] (nth hole (.indexOf combination from))
                            [x2 y2] (nth hole (.indexOf combination to))
                            [x1' y1'] (nth (:vertices figure) from)
                            [x2' y2'] (nth (:vertices figure) to)
                            d  (distance [x1 y1] [x2 y2])
                            d' (distance [x1' y1'] [x2' y2'])
                            ratio (Math/abs (double (- (/ d d') 1)))
                            good? (<= ratio (/ epsilon 1000000))]]
                  good?))) cid
            :else (recur (inc cid)))))
      (let [max-x (reduce max (map first hole))
            max-y (reduce max (map second hole))]
        (doseq [v (range vertices)]
          (swap! *solution assoc-in [:vertices v] [(rand-int max-x) (rand-int max-y)])))
      (doseq [[v i] (map vector (nth @*combinations @*combination-id) (range))]
        (swap! *solution assoc-in [:vertices v] (nth (:hole @*problem) i))))))

(defn factorial [n]
  (try
    (reduce * 1 (range 1 (inc n)))
    (catch Exception e
      Double/NaN)))

(defn draw-hole [canvas]
  (let [scale @*scale
        hole (:hole @*problem)]
    (with-open [path  (Path.)
                paint (-> (Paint.) (.setColor (color 0xFFDDDDDD)))]
      (.moveTo path (ffirst hole) (second (first hole)))
      (doseq [[x y] (next hole)]
        (.lineTo path x y))
      (.closePath path)
      (.transform path (Matrix33/makeScale scale))
      (.drawPath canvas path paint)
      (.setColor paint (color 0xFF000000))
      #_(doseq [[[x y] idx] (map vector hole (range))]
        (.drawString canvas (str "#" idx " [" x " " y "]") (* scale x) (* scale y) font paint)))))

(defn draw-grid [canvas]
  (let [scale @*scale
        hole (:hole @*problem)]
    (with-open [paint (-> (Paint.) (.setColor (color 0xFFCCCCCC)) (.setMode PaintMode/STROKE) (.setStrokeWidth 2))]
      (let [max-x (reduce max (map first hole))
            max-y (reduce max (map second hole))]
        (doseq [x (range 0 max-x 10)
                y (range 0 max-y 10)]
          (.drawLine canvas 0 (* scale y) (* scale max-x) (* scale y) paint)
          (.drawLine canvas (* scale x) 0 (* scale x) (* scale max-y) paint))))) )

(defn paint [canvas]
  (let [scale @*scale
        {:keys [hole epsilon figure]} @*problem
        {:keys [edges vertices]} @*solution]
    (.clear canvas (color 0xFFFFFFFF))
    
    (.save canvas)
    (.translate canvas (first @*offset) (second @*offset))
    
    (draw-hole canvas)
    ; (draw-grid canvas)

    ;; figure
    (with-open [paint (-> (Paint.) (.setColor (color 0x20CC3333)) (.setMode PaintMode/STROKE) (.setStrokeWidth 4))]
      (doseq [[from to] (:edges figure)
              :let [[x1 y1] (nth (:vertices figure) from)
                    [x2 y2] (nth (:vertices figure) to)]]
        (.drawLine canvas (* scale x1) (* scale y1) (* scale x2) (* scale y2) paint)))

    ;; best-solution
    (when-some [{:keys [edges vertices]} @*best-solution]
      (with-open [paint (-> (Paint.) (.setColor (color 0x203333CC)) (.setMode PaintMode/STROKE) (.setStrokeWidth 4))]
        (doseq [[from to] edges
                :let [[x1 y1] (nth vertices from)
                      [x2 y2] (nth vertices to)]]
          (.drawLine canvas (* scale x1) (* scale y1) (* scale x2) (* scale y2) paint))))

    ;; solution
    (with-open [text   (Paint.)
                line   (-> (Paint.) (.setMode PaintMode/STROKE) (.setStrokeWidth 4))
                circle (-> (Paint.) (.setColor (color 0xFFCC3333)) (.setMode PaintMode/STROKE) (.setStrokeWidth 1))]
      (doseq [[from to] edges]
        (let [[x1 y1] (nth vertices from)
              [x2 y2] (nth vertices to)
              [x1' y1'] (nth (:vertices figure) from)
              [x2' y2'] (nth (:vertices figure) to)
              d  (distance [x1 y1] [x2 y2])
              d' (distance [x1' y1'] [x2' y2'])
              ratio (Math/abs (double (- (/ d d') 1)))
              good? (<= ratio (/ epsilon 1000000))]
          (when-not good?
            (.drawCircle canvas (* scale x1) (* scale y1) (* scale (Math/sqrt d')) circle)
            (.drawCircle canvas (* scale x2) (* scale y2) (* scale (Math/sqrt d')) circle))
          (.setColor line (if good? (color 0xFF33CC33) (color 0xFFCC3333)))
          (.drawLine canvas (* scale x1) (* scale y1) (* scale x2) (* scale y2) line)
          (.setColor text (if good? (color 0xFF33CC33) (color 0xFFCC3333)))
          (.drawString canvas (str ratio) (* scale (/ (+ x1 x2) 2)) (* scale (/ (+ y1 y2) 2)) font text))))

    ;; current vertex
    (when-some [vertex @*vertex]
      (let [[x y] (nth (:vertices @*solution) vertex)]
        (with-open [paint (-> (Paint.) (.setColor (color 0x80CC3333)))]
          (.drawCircle canvas (* scale x) (* scale y) 30 paint))))

    (.restore canvas)

    ;; stats
    (with-open [paint (-> (Paint.) (.setColor (color 0xFF000000)))]
      (let [draw-string (fn [str]
                          (.drawString canvas str 0 0 font paint)
                          (.translate canvas 0 40))]
        (.save canvas)
        (.translate canvas 40 60)
        (draw-string (str "Problem " @*problem-id))
        (draw-string (str "Holes " (count (:hole @*problem))))
        (draw-string (str "Vertices " (count (:vertices @*solution))))
        (draw-string (str "Edges " (count (:edges @*solution))))
        (draw-string (str "Epsilon " (float (/ (:epsilon @*problem) 1000000))))
        (draw-string (str "Score " (score hole @*solution)))
        (when-some [combination-id @*combination-id]
          (draw-string (str "Combination " combination-id " of " (count @*combinations))))
        (when (<= (count (:hole @*problem)) (count (:vertices @*solution)))
          (draw-string (str "Total combinations "
                         (/ (factorial (count (:vertices @*solution)))
                           (factorial (- (count (:vertices @*solution)) (count (:hole @*problem))))))))
        (draw-string (str "Best score " (if-some [score @*best-solution-score] score "—")))
        (doseq [[k v] @*meta]
          (draw-string (str (name k) " " v)))
        (draw-string "[S] Save solution")
        (draw-string "[U] Upload best solution")

        (.restore canvas)))))

(defn -main [& args]
  (reset! *problem-id 1)
  (App/init)
  (let [window  (App/makeWindow)
        layer   (LayerMetal.)
        _       (.attach layer window)
        context (DirectContext/makeMetal (.getDevicePtr layer) (.getQueuePtr layer))]
    (.setEventListener window
      (reify java.util.function.Consumer
        (accept [this event]
          (cond
            (instance? EventReconfigure event)
            (.reconfigure layer)

            (instance? EventResize event)
            (.resize layer (.getWidth event) (.getHeight event))

            (and (instance? EventKeyboard event) (.isPressed event))
            (when (nil? @*key)
              (reset! *key (.getKeyCode event))
              (condp = (.getKeyCode event)
                Key/RIGHT
                (let [next-id (loop [id (+ @*problem-id 1)]
                                (cond
                                  (> id 78) (recur 1)
                                  (not (.exists (io/file (str "problems/" id ".problem")))) (recur (inc id))
                                  :else id))]
                  (reset! *problem-id next-id))
                Key/LEFT
                (let [next-id (loop [id (- @*problem-id 1)]
                                (cond
                                  (< id 1) (recur 78)
                                  (not (.exists (io/file (str "problems/" id ".problem")))) (recur (dec id))
                                  :else id))]
                  (reset! *problem-id next-id))
                Key/S
                (let [score (score (:hole @*problem) @*solution)
                      file (io/file (str "solutions/" @*problem-id "/" score ".solution"))]
                  (.mkdirs (.getParentFile file))
                  (spit file (json/write-value-as-string @*solution))
                  (load-best-solution! @*problem-id))
                Key/C
                (next-combination)
                Key/SPACE
                (do
                  (reset! *last-mouse @*mouse)
                  (reset! *last-solution @*solution))
                Key/U
                (when-some [best-solution @*best-solution]
                  (let [problem-id @*problem-id
                        best-solution-score @*best-solution-score
                        url (str "https://poses.live/api/problems/" problem-id "/solutions")]
                    (println "Submitting" best-solution-score "solution to" url)
                    (http/post url
                      {:body (json/write-value-as-string @*best-solution)
                       :headers {"Authorization" (str "Bearer " (System/getenv "ICFPC2021_TOKEN"))}
                       :content-type :json
                       :retry-handler (fn [ex try-count http-context] false)
                       :async? true}
                      (fn [response]
                        (println "Accepted:" response)
                        (spit (io/file (str "solutions/" problem-id "/meta.edn"))
                          (pr-str {:uploaded-score best-solution-score
                                   :uploaded-inst (Date.)}))
                        (when (= problem-id @*problem-id)
                          (load-best-solution! problem-id)))
                      (fn [exception] (println "Refused:" (.getMessage exception))))))
                ;; else
                nil))

            (and (instance? EventKeyboard event) (not (.isPressed event)))
            (do
              (reset! *key nil)
              (reset! *last-mouse nil)
              (reset! *last-solution nil))

            (instance? EventMouseMove event)
            (do
              (reset! *mouse [(.getX event) (.getY event)])
              (if (= @*key Key/SPACE)
                (let [dx (-> (- (.getX event) (first @*last-mouse)) (/ @*scale) (double) (Math/round))
                      dy (-> (- (.getY event) (second @*last-mouse)) (/ @*scale) (double) (Math/round))]
                  (if-some [vertex @*vertex]
                    (reset! *solution (update-in @*last-solution [:vertices vertex] (fn [[x y]] [(+ x dx) (+ y dy)])))
                    (reset! *solution (update @*last-solution :vertices #(mapv (fn [[x y]] [(+ x dx) (+ y dy)]) %)))))
                (let [close (for [[[x y] i] (map vector (:vertices @*solution) (range))
                                  :let [x' (+ (* @*scale x) (first @*offset))
                                        y' (+ (* @*scale y) (second @*offset))
                                        dist (max (Math/abs (int (- x' (.getX event)))) (Math/abs (int (- y' (.getY event)))))]
                                  :when (< dist 30)]
                              i)]
                  (if (empty? close)
                    (reset! *vertex nil)
                    (reset! *vertex (apply min close))))))

            (instance? EventFrame event)
            (with-open [rt      (BackendRenderTarget/makeMetal (.getWidth layer) (.getHeight layer) (.nextDrawableTexturePtr layer))
                        surface (Surface/makeFromBackendRenderTarget
                                  context
                                  rt
                                  SurfaceOrigin/TOP_LEFT
                                  SurfaceColorFormat/BGRA_8888
                                  (ColorSpace/getDisplayP3)
                                  (SurfaceProps. PixelGeometry/RGB_H))]
              (paint (.getCanvas surface))
              (.flushAndSubmit surface)
              (.swapBuffers layer)
              (.requestFrame window))

            :else
            (println event)))))
    (.move window 400 200)
    (.resize window 3000 1800)
    (.show window)
    (.requestFrame window)
    (App/start)))