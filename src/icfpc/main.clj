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
   [org.jetbrains.skija BackendRenderTarget ColorSpace DirectContext Font FontStyle Matrix33 Paint PaintMode Path PixelGeometry Rect Surface SurfaceColorFormat SurfaceOrigin SurfaceProps Typeface]
   [org.jetbrains.jwm App EventFrame EventKeyboard EventMouseMove EventReconfigure EventResize Key LayerMetal Window]))

(defonce *problem-id (atom nil))
(defonce *problem (atom nil))
(defonce *solution (atom nil))
(defonce *saved-solution (atom nil))
(defonce *saved-solutions (atom nil))
(defonce *saved-solution-idx (atom nil))
(defonce *last-solution (atom nil))
(defonce *meta (atom (edn/read-string (slurp "solutions/meta.edn"))))
(defonce *scale (atom 1))
(defonce *offset (atom [0 0]))
(defonce *key (atom nil))
(defonce *mouse (atom [0 0]))
(defonce *last-mouse (atom nil))
(defonce *vertex (atom nil))
(defonce *combinations (atom nil))
(defonce *combination-id (atom nil))
(def font (Font. (Typeface/makeFromName "SF Mono" (-> FontStyle/NORMAL (.withWeight 500))) 29.0))

(defn zip [& seqs]
  (apply map vector seqs))

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

(defn load-ladder []
  (println "Loading ladder from https://poses.live/problems")
  (->> (http/get "https://poses.live/problems" {:headers {"Cookie" (System/getenv "ICFPC2021_COOKIE")}})
    :body
    (re-seq #"<tr><td><a href=\"/problems/(\d+)\">\d+</a></td><td>(\d+|❌)?</td><td>(\d+)?</td></tr>")
    (map (fn [[_ id submitted best]]
           [(some-> id Long/parseLong)
            {:submitted (if (= "❌" submitted) -1 (some-> submitted Long/parseLong))
             :best (some-> best Long/parseLong)}]))
    (into {})))

(def *ladder (atom (load-ladder)))

(def lock (Object.))

(defn update-file [file f & args]
  (locking lock
    (let [content  (-> file slurp edn/read-string)
          content' (apply f content args)]
      (when (= file "solutions/meta.edn")
        (reset! *meta content'))
      (spit file (pr-str content')))))

(defn load-saved-solutions! [id]
  (reset! *saved-solution-idx nil)
  (reset! *saved-solutions
    (let [dir (io/file (str "solutions/" id))]
      (when (.exists dir)
        (sort-by #(Long/parseLong %)
          (for [file (next (file-seq dir))
                :let [[_ score] (re-matches #"(\d+).solution" (.getName file))]
                :when (some? score)]
            score))))))

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
    (load-saved-solutions! id)))

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

      (.setColor paint (color 0xFF808080))
      (doseq [[x y] hole]
        (.drawRect canvas (Rect/makeXYWH (- (* scale x) 4) (- (* scale y) 4) 8 8) paint))

      ;; bonuses
      (doseq [{:keys [position problem bonus]} (:bonuses @*problem)
              :let [[x y] position]]
        (.drawRect canvas (Rect/makeXYWH (- (* scale x) 10) (- (* scale y) 10) 20 20) paint)
        (.drawString canvas (str (str/capitalize bonus) " for #" problem) (+ (* scale x) 20) (+ (* scale y) 10) font paint)))))

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
        {:keys [edges vertices]} @*solution
        *all-good? (atom true)]
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
            (reset! *all-good? false)
            (when (and @*vertex (= (nth (:vertices @*solution) @*vertex) [x2 y2]))
              (.drawCircle canvas (* scale x1) (* scale y1) (* scale (Math/sqrt d')) circle))
            (when (and @*vertex (= (nth (:vertices @*solution) @*vertex) [x1 y1]))
              (.drawCircle canvas (* scale x2) (* scale y2) (* scale (Math/sqrt d')) circle)))
          (.setColor line (if good? (color 0xFF33CC33) (color 0xFFCC3333)))
          (.drawLine canvas (* scale x1) (* scale y1) (* scale x2) (* scale y2) line)
          (.setColor text (if good? (color 0xFF33CC33) (color 0xFFCC3333)))
          (.drawString canvas (str ratio) (* scale (/ (+ x1 x2) 2)) (* scale (/ (+ y1 y2) 2)) font text)))
      (-> line (.setColor (color 0xFF808080)) (.setMode PaintMode/FILL))
      (doseq [[[x y] i] (zip vertices (range))]
        (.drawRect canvas (Rect/makeXYWH (- (* scale x) 4) (- (* scale y) 4) 8 8) line)
        (.drawString canvas (str "#" i " " x "," y) (* scale x) (* scale y) font text)))

    ;; current vertex
    (when-some [vertex @*vertex]
      (let [[x y] (nth (:vertices @*solution) vertex)]
        (with-open [paint (-> (Paint.) (.setColor (color 0x80CC3333)))]
          (.drawCircle canvas (* scale x) (* scale y) 30 paint))))

    (.restore canvas)

    ;; stats
    (with-open [paint (-> (Paint.) (.setColor (color 0xFF000000)))]
      (let [draw-string (fn draw-string
                          ([str] (draw-string str 0xFF000000))
                          ([str color-long]
                            (.setColor paint (color color-long))
                            (.drawString canvas str 0 0 font paint)
                            (.translate canvas 0 40)))
            {:keys [submitted best]} (get @*ladder @*problem-id)
            score (score hole @*solution)]
        (.save canvas)
        (.translate canvas 40 60)
        (draw-string (str "Problem " @*problem-id))
        ; (draw-string (str "Holes " (count (:hole @*problem))))
        ; (draw-string (str "Vertices " (count (:vertices @*solution))))
        ; (draw-string (str "Edges " (count (:edges @*solution))))
        (draw-string (str "Epsilon " (float (/ (:epsilon @*problem) 1000000))))
        (when-some [[from bonus] (first (for [[from {:keys [problem bonus]}] @*meta
                                              :when (= problem @*problem-id)]
                                          [from bonus]))]
          (draw-string (str "Bonus: " bonus " from #" from) 0xFF3333CC))
        (draw-string (str "Current score " score)
          (cond
            (not @*all-good?) 0xFFCC3333
            (<= score best) 0xFF33CC33
            (and
              (not (empty? @*saved-solutions))
              (< score (Long/parseLong (first @*saved-solutions)))) 0xFF33CC33
            :else 0xFF000000))
        (draw-string (str "Best score " best)
          (cond
            (nil? submitted) 0xFFCC3333
            (> submitted best) 0xFFFF8033
            :else 0xFF33CC33))
        (when-not (empty? @*saved-solutions)
          (draw-string "Saved:"))
        (doseq [[name i] (zip @*saved-solutions (range))
                :let [submitted? (= (str submitted) name)]]
          (draw-string (str (if (= i @*saved-solution-idx) "> " "  ") name (when submitted? " [Submitted]"))))
        (when-some [combination-id @*combination-id]
          (draw-string (str "Combination " combination-id " of " (count @*combinations))))
        (when (<= (count (:hole @*problem)) (count (:vertices @*solution)))
          (draw-string (str "Total combinations "
                         (/ (factorial (count (:vertices @*solution)))
                           (factorial (- (count (:vertices @*solution)) (count (:hole @*problem))))))))
        ; (draw-string (str "Best score " (if-some [score @*best-solution-score] score "—")))
        ; (doseq [[k v] @*meta]
        ;   (draw-string (str (name k) " " v)))
        (draw-string "[C] Next combination")
        (draw-string "[S] Save")
        (draw-string "[U] Upload")

        (.restore canvas)))))

(defn upload! []
  (when (some? @*saved-solution-idx) ;; check if saved
    (let [problem-id @*problem-id
          url        (str "https://poses.live/api/problems/" problem-id "/solutions")
          bonus      (first
                       (for [bonus  (:bonuses @*problem)
                             vertex (:vertices @*solution)
                             :when  (= (:position bonus) vertex)]
                         bonus))]
      (println "Eligible for bonus" (:bonus bonus) "for problem" (:problem bonus))
      (update-file "solutions/meta.edn"
        (fn [solutions]
          (if (some? bonus)
            (update solutions @*problem-id merge bonus)
            (update solutions @*problem-id dissoc :bonus :position :problem))))

      (println "Submitting solution to" url)
      (http/post url
        {:body (json/write-value-as-string @*solution)
         :headers {"Authorization" (str "Bearer " (System/getenv "ICFPC2021_TOKEN"))}
         :content-type :json
         :retry-handler (fn [ex try-count http-context] false)
         :async? true}
        (fn [response]
          (println "Accepted:" response)
          (update-file "solutions/meta.edn" update @*problem-id assoc
            :uploaded-score (score (:hole @*problem) @*solution)
            :uploaded-inst (Date.))
          (.start (Thread. #(do (Thread/sleep 5000) (reset! *ladder (load-ladder))))))
        (fn [exception] (println "Refused:" (.getMessage exception)))))))

(defn -main [& args]
  (reset! *problem-id 42)
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
            (do
              (.reconfigure layer)
              (.requestFrame window))

            (instance? EventResize event)
            (do
              (.resize layer (.getWidth event) (.getHeight event))
              (.requestFrame window))

            (and (instance? EventKeyboard event) (.isPressed event))
            (when (nil? @*key)
              (.requestFrame window)
              (reset! *key (.getKeyCode event))
              (condp = (.getKeyCode event)
                Key/RIGHT
                (let [next-id (loop [id (+ @*problem-id 1)]
                                (cond
                                  (> id 132) (recur 1)
                                  (not (.exists (io/file (str "problems/" id ".problem")))) (recur (inc id))
                                  :else id))]
                  (reset! *problem-id next-id))

                Key/LEFT
                (let [next-id (loop [id (- @*problem-id 1)]
                                (cond
                                  (< id 1) (recur 132)
                                  (not (.exists (io/file (str "problems/" id ".problem")))) (recur (dec id))
                                  :else id))]
                  (reset! *problem-id next-id))

                Key/DOWN
                (when-some [saved-solutions (not-empty @*saved-solutions)]
                  (let [i (-> (or @*saved-solution-idx -1) (+ 1) (mod (count saved-solutions)))
                        score (nth saved-solutions i)
                        json (-> (str "solutions/" @*problem-id "/" score ".solution")
                               (slurp)
                               (json/read-value json/keyword-keys-object-mapper))]
                    (reset! *saved-solution-idx i)
                    (reset! *solution json)))

                Key/UP
                (when-some [saved-solutions (not-empty @*saved-solutions)]
                  (let [i (-> (or @*saved-solution-idx 0) (+ (count saved-solutions)) (- 1) (mod (count saved-solutions)))
                        score (nth saved-solutions i)
                        json (-> (str "solutions/" @*problem-id "/" score ".solution")
                               (slurp)
                               (json/read-value json/keyword-keys-object-mapper))]
                    (reset! *saved-solution-idx i)
                    (reset! *solution json)))

                Key/S
                (let [score (score (:hole @*problem) @*solution)
                      file (io/file (str "solutions/" @*problem-id "/" score ".solution"))]
                  (.mkdirs (.getParentFile file))
                  (spit file (json/write-value-as-string @*solution))
                  (load-saved-solutions! @*problem-id)
                  (let [i (.indexOf @*saved-solutions (str score))]
                    (reset! *saved-solution-idx i)))

                Key/C
                (next-combination)

                Key/SPACE
                (do
                  (reset! *last-mouse @*mouse)
                  (reset! *last-solution @*solution))

                Key/U
                (upload!)
                ; else
                nil))

            (and (instance? EventKeyboard event) (not (.isPressed event)))
            (do
              (reset! *key nil)
              (reset! *last-mouse nil)
              (reset! *last-solution nil))

            (instance? EventMouseMove event)
            (do
              (.requestFrame window)
              (reset! *mouse [(.getX event) (.getY event)])
              (if (= @*key Key/SPACE)
                (let [dx (-> (- (.getX event) (first @*last-mouse)) (/ @*scale) (double) (Math/round))
                      dy (-> (- (.getY event) (second @*last-mouse)) (/ @*scale) (double) (Math/round))]
                  (when (or (> (Math/abs dx) 0) (> (Math/abs dy) 0))
                    (reset! *saved-solution-idx nil))
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
              (.swapBuffers layer))

            :else
            (println event)))))
    (.move window 400 200)
    (.resize window 3000 1800)
    (.show window)
    (.requestFrame window)
    (App/start)))