(ns core
  (:require [overtone.at-at :as at-at]
            [overtone.midi :as m]
            [overtone.music.pitch :as p]))

;; ████████╗░█████╗░██╗░██████╗████████╗███████╗
;; ╚══██╔══╝██╔══██╗██║██╔════╝╚══██╔══╝██╔════╝
;; ░░░██║░░░███████║██║╚█████╗░░░░██║░░░█████╗░░
;; ░░░██║░░░██╔══██║██║░╚═══██╗░░░██║░░░██╔══╝░░
;; ░░░██║░░░██║░░██║██║██████╔╝░░░██║░░░███████╗
;; ░░░╚═╝░░░╚═╝░░╚═╝╚═╝╚═════╝░░░░╚═╝░░░╚══════╝
;; Pikkujoulu party 2021
;; DOES EINSTEIN DREAM OF EUCLIDEAN SHEEP?
;;
;; Generative composition with Clojure, Overtone and Ableton Live
;;
;; Uses Euclidean Rhythm algorithm with some random variations to
;; generate interesting rhytmic ambient piece.
;;
;; Grab the earphones and take a listen!
;;
;; Maybe browse the source too with the arrow keys. Press gg to jump back.
;; (Don't worry. This is the VIM editor so you can't exit it by mistake!)

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
           #(m/midi-note output note 80 note-duration channel)]))))

;; SONG PARTS

(defn generate-gong [ts steps note-duration]
  (generate-euclidean {:notes         [:c2 :d2 :a2 :b2 :c3]
                       :beats         4
                       :steps         steps
                       :offset        (rand-int 2)
                       :ts            ts
                       :note-duration note-duration}))

(defn generate-dabruka [ts steps note-duration]
  (generate-euclidean {:notes         [:c2]
                       :beats         4
                       :steps         steps
                       :offset        2
                       :channel       1
                       :ts            ts
                       :note-duration note-duration}))

(defn generate-metal [ts steps note-duration]
  (generate-euclidean {:notes         [:c3 :g3 :c4 :d4]
                       :beats         (inc (rand-int 3))
                       :steps         steps
                       :offset        2
                       :channel       2
                       :ts            ts
                       :note-duration note-duration}))

(defn generate-strings [ts on-or-off]
  (at-at/at
    ts
    #(doseq [note (conj (take 3 (p/chord-degree :i :a3 :minor)) (p/note :a1))]
       (if (= on-or-off :on)
         (m/midi-note-on output note 100 3)
         (m/midi-note-off output note 3)))
    pool))

(defn generate-kick [ts steps note-duration]
  (generate-euclidean {:notes         [:d4]
                       :beats         1
                       :steps         steps +1
                       :offset        0
                       :channel       4
                       :ts            ts
                       :note-duration note-duration}))

(let [melody #_[:a5 :e5 :c5 :b5 :a5 :c5 :d5 :g5]
             (p/chord-degree :i :a5 :dorian)
      offset-steps (euclidean-seq 3 22 0)
      melody-generator (apply concat
                              (map-indexed
                                (fn [i offset]
                                  (let [soff (mod i (count melody))]
                                    (->> melody (drop soff))))
                                offset-steps))
      ;; We live in an immutable world, so make it mutable!
      ;; Every call of generate-melody starts from where the last one left
      current-offset (atom 0)]
  (defn generate-melody [ts steps note-duration]
    (doseq [[index note] (map-indexed
                           vector
                           (->> melody-generator
                                (drop @current-offset)
                                (take steps)))
            :let [ts (+ ts (* note-duration 2 index))]]
      (at-at/at
        ts
        #(m/midi-note output note #_(p/note note) 100 (* note-duration 2) 5)
        pool))
    (swap! current-offset + steps)))

;; SONG CONSTRUCTION & PLAYGROUND

(def parts
  #_[#'generate-gong]
  #_[#'generate-gong #'generate-kick]
  #_[#'generate-gong #'generate-kick #'generate-dabruka]
  [#'generate-gong #'generate-kick #'generate-dabruka #'generate-metal]
  )

(defn composition-fn [steps note-duration]
  (let [ts (at-at/now)]
    (generate-melody ts (inc (rand-int 3)) note-duration)
    (doseq [part parts]
      (doseq [[ts pfn] (take steps (part ts steps note-duration))]
        (at-at/at ts pfn pool)))))

(def composition
  (future
    (let [steps 7
          note-duration 250
          duration (* steps note-duration)]
      (generate-strings (at-at/now) :on)
      (at-at/every duration (partial #'composition-fn steps note-duration) pool))))

(defn stop []
  (at-at/stop-and-reset-pool! pool :strategy :kill)
  (generate-strings (at-at/now) :off)
  (future-cancel composition))

(comment
  (stop)

  (defn play [degree]
    (doseq [note (p/chord-degree degree :c3 :ionian)]
      (m/midi-note output note 100 400 5)))
  (do
    (def job-1 (at-at/every 1500 (partial play :ii) pool))
    (def job-2 (at-at/every 1500 (partial play :v) pool :initial-delay 750))
    (def job-3 (at-at/every 1500 (partial play :i) pool :initial-delay 1000)))
  (do
    (at-at/stop job-1)
    (at-at/stop job-2)
    (at-at/stop job-3)))
