(ns core
  (:require [overtone.midi :as m]
            [overtone.at-at :as at-at]
            [overtone.music.pitch :as p])
  (:import javax.sound.midi.ShortMessage))

(declare steps)
(def output (m/midi-out "Virtual"))
(def pool (at-at/mk-pool))

;; TIMING GENERATION FUNCTIONS

(defn euclidean
  "Euclidean rhythm generator.
   Computes N steps evenly to M beats with offset O"
  ([n m]
   (euclidean n m 0))
  ([n m offset]
   (loop [front (repeat (* n 2) [:tick])
          back (repeat (* (- m n) 2) [:tock])]
     (let [a (mapv (fn [a b] (into a b)) front back)
           b (cond
               (zero? (count back))
               front

               (< (count front) (count back))
               (vec (drop (count front) back))

               :else
               (vec (drop (count back) front)))]
       (if (and (pos? (count a)) (> (count b) 1))
         (recur a b)
         (->>
           (flatten [a b])
           (take m)
           (map-indexed (fn [index t]
                          (if (= :tick t)
                            (+ index offset)
                            nil)))
           (remove nil?)))))))

(defn euclidean-seq [beats steps offset]
  (->> (iterate (fn [x] (map (partial + steps) x)) (euclidean beats steps offset))
       flatten))

(defn generate-euclidean [{:keys [ts notes beats steps offset note-duration channel] :or {offset 1 channel 0 ts (at-at/now)}}]
  (->>
      (map p/note (cycle notes))
      (interleave (euclidean-seq beats steps offset))
      (partition 2)
      (map
        (fn [[index note]]
          [(+ ts (* note-duration index))
           #(m/midi-note output note 100 note-duration channel)]))))

;; SONG PARTS

(defn generate-gong [ts]
  (generate-euclidean {:notes         [:c2 :d2 :a2 :b2 :c3]
                       :beats         4
                       :steps         steps
                       :offset        (rand-int 2)
                       :ts            ts
                       :note-duration 300}))

(defn generate-dabruka [ts]
  (generate-euclidean {:notes         [:c2]
                       :beats         4
                       :steps         steps
                       :offset        2
                       :channel       1
                       :ts            ts
                       :note-duration 300}))

(defn generate-metal [ts]
  (generate-euclidean {:notes         [:c3 :d3 :c4 :d4]
                       :beats         (inc (rand-int 3))
                       :steps         steps
                       :offset        2
                       :channel       2
                       :ts            ts
                       :note-duration 300}))

(defn generate-strings [ts on-or-off]
  (at-at/at
    ts
    #(doseq [note (conj (take 3 (p/chord-degree :i :a3 :minor)) (p/note :a1))]
       (if (= on-or-off :on)
         (m/midi-note-on output note 100 3)
         (m/midi-note-off output note 3)))
    pool))

(defn generate-kick [ts]
  (generate-euclidean {:notes         [:e4]
                       :beats         1
                       :steps         steps
                       :offset        0
                       :channel       4
                       :ts            ts
                       :note-duration 300}))


(let [melody [:a5 :e5 :c5 :b5 :a5 :c5 :d5 :g5]
      offset-steps (euclidean-seq 3 22 0)
      melody-generator (apply concat
                              (map-indexed
                                (fn [i offset]
                                  (let [soff (mod i (count melody))]
                                    (->> melody (drop soff))))
                                offset-steps))
      current-offset (atom 0)]
  (defn generate-melody [ts steps]
    (doseq [[index note] (map-indexed
                           vector
                           (->> melody-generator
                                (drop @current-offset)
                                (take steps)))
            :let [ts (+ ts (* 600 index))]]
      (at-at/at
        ts
        #(m/midi-note output (p/note note) 100 600 5)
        pool))
    (swap! current-offset + steps)))

;; SONG CONSTRUCTION & PLAYGROUND

(def steps 7)
(def parts
  [#'generate-gong]
  #_[#'generate-gong #'generate-kick]
  #_[#'generate-gong #'generate-kick #'generate-dabruka]
  #_[#'generate-gong #'generate-kick #'generate-dabruka #'generate-metal]
  )

#_(def strings
  (future
    (generate-strings (at-at/now) :on)))
#_(def melody
  (future
    (generate-melody (at-at/now) 20)))

(def composition
  (future
    (let [steps 7
          duration (* steps 300)]
      #_(generate-strings (at-at/now) :on)
      (at-at/every
        duration
        (fn []
          (let [ts (at-at/now)]
            #_(generate-melody ts (inc (rand-int 3)))
            (doseq [part parts]
              (doseq [[ts pfn] (take steps (part ts))]
                (at-at/at ts pfn pool)))))
        pool))))

(defn stop []
  (at-at/stop-and-reset-pool! pool :strategy :kill)
  (generate-strings (at-at/now) :off)
  (future-cancel composition))

(comment
  (def player
    (future
      (generate-strings (at-at/now) :off) pool))
  (stop)

  (defn play [degree]
    (doseq [note (p/chord-degree degree :c3 :ionian)]
      (m/midi-note output note 100 400)))
  (do
    (def job-1 (at-at/every 1500 (partial play :ii) pool))
    (def job-2 (at-at/every 1500 (partial play :v) pool :initial-delay 750))
    (def job-3 (at-at/every 1500 (partial play :i) pool :initial-delay 1000)))
  (do
    (at-at/stop job-1)
    (at-at/stop job-2)
    (at-at/stop job-3)))
