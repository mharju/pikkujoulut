(ns core
  (:require [overtone.live :as l]
            [overtone.midi :as m]
            [overtone.at-at :as at-at]
            [overtone.music.pitch :as p]))

(def output (m/midi-out "Virtual"))
(def pool (at-at/mk-pool))

(defn euclidean
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

(defn schedule-euclidean [{:keys [hits notes note velocity duration note-duration channel offset] :or {offset 0}}]
  (doseq [ts (euclidean hits notes offset)]
    (at-at/at
      (+ (at-at/now) (* ts duration))
      #(m/midi-note output (if (keyword? note) (p/note note) note) velocity (or note-duration duration) channel) pool)))

(defn schedule-chord [{:keys [degree root-note mode num-notes note-duration]}]
  (let [root (+ (p/note root-note) -12 (p/degree->interval degree mode))
        [a b c d] (p/chord-degree degree root-note mode)]
    (do
      (schedule-euclidean {:hits     (/ num-notes 4)
                           :notes    num-notes
                           :note     (- root 24)
                           :velocity (+ (rand-int 20) 80)
                           :duration note-duration
                           :channel  1})
      (schedule-euclidean {:hits     5
                           :notes    num-notes
                           :note     a
                           :velocity (+ (rand-int 20) 80)
                           :duration note-duration
                           :note-duration (/ note-duration 2)
                           :channel  0})
      (schedule-euclidean {:hits     3
                           :notes    num-notes
                           :offset   1
                           :note     b
                           :velocity (+ (rand-int 20) 80)
                           :duration note-duration
                           :note-duration (/ note-duration 2)
                           :channel  0})
      (schedule-euclidean {:hits     3
                           :notes    num-notes
                           :offset   1
                           :note     c
                           :velocity (+ (rand-int 50) 50)
                           :duration (/ note-duration 2)
                           :note-duration (/ note-duration 4)
                           :channel  0}))))

(defn schedule-drums [{:keys [num-notes note-duration]}]
  ;; Kick
  (schedule-euclidean {:hits     (/ num-notes 2)
                       :notes    num-notes
                       :note     (rand-nth [:c2 :c#2 :d2])
                       :velocity (+ 80 (rand-int 20))
                       :duration note-duration
                       :channel  2})

  ;; Snare/snaps
  (schedule-euclidean {:hits     (/ num-notes 4)
                       :notes    num-notes
                       :offset   2
                       :note     :g2
                       :velocity (+ 80 (rand-int 20))
                       :duration note-duration
                       :channel  2})

  ;; Snaps
  (schedule-euclidean {:hits     (/ num-notes 2)
                       :notes    num-notes
                       :note     (rand-nth [:e2 :f2 :f#2])
                       :velocity (+ 80 (rand-int 20))
                       :duration note-duration
                       :channel  2})

  ;; Hats
  (schedule-euclidean {:hits num-notes
                       :notes (/ num-notes (inc (rand-int 2)))
                       :note (rand-nth [:g#2 :a2])
                       :velocity (+ 80 (rand-int 20))
                       :duration note-duration
                       :channel 2}))

(def progression-chain
  {:i   {:i 0   :ii 0   :iii 0   :iv 0.6 :v 0.2 :vi 0.2 :vii 0}
   :ii  {:i 0.8 :ii 0   :iii 0   :iv 0   :v 0   :vi 0   :vii 0.2}
   :iii {:i 1.0 :ii 0   :iii 0   :iv 0   :v 0   :vi 0   :vii 0}
   :iv  {:i 0.1 :ii 0   :iii 0.1 :iv 0   :v 0.2 :vi 0.6 :vii 0}
   :v   {:i 0.2 :ii 0   :iii 0   :iv 0.8 :v 0   :vi 0   :vii 0}
   :vi  {:i 0.5 :ii 0   :iii 0   :iv 0.5 :v 0   :vi 0   :vii 0}
   :vii {:i 1.0 :ii 0   :iii 0   :iv 0   :v 0   :vi 0   :vii 0}})

(defn markov [probabilities current-state]
  (let [rnd (rand)
        states (get probabilities current-state)]
    (reduce
      (fn [acc [k prob]]
        (if (pos? prob)
          (let [prev (or (second (second (last acc))) 0.0)]
            (if (< prev rnd (+ prev prob))
              (reduced k)
              (conj acc [k [prev (+ prev prob)]])))
          acc))
      []
      states)))

(def composition
  (future-call
    (fn []
      (let [num-notes 8 #_(+ 8 (* 2 (rand-int 5)))
            note-duration 250
            duration (* num-notes note-duration)
            mode (rand-nth [:major :melodic-minor :dorian :mixolydian])
            root-note (rand-nth [:c5 :c#5 :d5 :f5])
            degrees (take 50 (iterate (partial markov progression-chain) :i))]

        ;; Chords
        (->> degrees
             (map-indexed (fn [index degree]
                            (at-at/at
                              (+ (at-at/now) (* index duration))
                              (fn []
                                (schedule-drums {:num-notes     num-notes
                                                 :note-duration note-duration})
                                (schedule-chord {:degree        degree
                                                 :root-note     root-note
                                                 :mode          mode
                                                 :num-notes     num-notes
                                                 :note-duration note-duration})) pool)))
             doall)))))

(defn stop []
  (at-at/stop-and-reset-pool! pool :strategy :kill)
  (future-cancel composition))

(comment
  (l/demo (l/sin-osc))
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
