(ns core
  (:require [overtone.live :as l]
            [overtone.midi :as m]
            [overtone.at-at :as at-at]
            [overtone.music.pitch :as p])
  (:import javax.sound.midi.ShortMessage))

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

(defn schedule-euclidean [{:keys [notes beats steps offset note-duration channel] :or {offset 1 channel 0}}]
  (let [now (at-at/now)
        timing (->> (iterate (fn [x] (map (partial + steps) x)) (euclidean beats steps offset)) flatten)]
    (->>
      (map p/note (cycle notes))
      (interleave timing)
      (partition 2)
      (map
        (fn [[index note]]
          (at-at/at
            (+ now (* note-duration index))
            #(m/midi-note output note 100 note-duration channel)
            pool))))))

(defn unmute-part [part-num]
  (m/midi-control output (+ 102 part-num) 0 part-num))
(defn mute-part [part-num]
  (m/midi-control output (+ 102 part-num) 127 part-num))


(defn schedule-gong []
  (schedule-euclidean {:notes [:c2 :d2 :a2 :b2 :c3]
                       :beats 4
                       :steps 7
                       :offset (rand-int 2)
                       :note-duration 300}))

(defn schedule-dabruka []
  (schedule-euclidean {:notes [:c2]
                       :beats 4
                       :steps 7
                       :offset 2
                       :note-duration 300
                       :channel 1}))

(defn schedule-metal []
  (schedule-euclidean {:notes [:c#2 :d2 :d#2 :e2 :f2 :d2 :f2 :e5]
                       :beats (rand-int 4)
                       :steps 7
                       :offset 0
                       :note-duration 300
                       :channel 2}))

(defn schedule-strings [on-or-off]
  (doseq [note (conj (p/chord-degree :i :c2 :minor) (p/note :c1))]
    (if (= on-or-off :on)
      (m/midi-note-on output note 100 3)
      (m/midi-note-off output note 3))))

(defn schedule-kick []
  (schedule-euclidean {:notes [:e4]
                       :beats 1
                       :steps 7
                       :offset 0
                       :note-duration 300
                       :channel 4}))
(def composition
  (future
    (let [steps (* 7 50)
          parts [schedule-gong schedule-dabruka schedule-metal schedule-kick]]
      (schedule-strings :on)
      (doseq [part parts]
        (doall (take steps (part)))))))

(defn stop []
  (at-at/stop-and-reset-pool! pool :strategy :kill)
  (schedule-strings :off)
  (future-cancel composition))

(comment
  (schedule-strings :on)
  (schedule-strings :off)
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
