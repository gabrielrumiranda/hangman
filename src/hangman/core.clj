(ns hangman.core
  (:gen-class))

(require '[clojure.string :as str])

(def gallows-stages
  ["" " O " " O\n |" " O\n/|" " O\n/|\\"
   " O\n/|\\\n/" " O\n/|\\\n/ \\"])

(defn display-gallows [attempts]
  (println (nth gallows-stages attempts)))

(defn initialize-game [word, player1, player2, max-attempt]
  {:word (str/lower-case word)
   :guessed #{}
   :incorrect 0
   :max-attempt max-attempt
   :current-player player1
   :player1 player1
   :player2 player2
   :winner nil})

(defn display-word [word guessed]
  (apply str (map (fn [letter] (if (guessed letter) letter "_")) word)))

(defn display-status [game]
  (println "\nPalavra:")
  (println (display-word (:word game) (:guessed game)))
  (println (str "Tentativas falhas: " (:incorrect game) "/" ) (:max-attempt game))
  (display-gallows (:incorrect game)))

(defn announce-winner [game]
  (if (:winner game)
    (println (str "\nParabéns, " (:winner game) "! Você venceu!"))
    (do
      (println "\nO jogo acabou! O homem foi enforcado!")
      (println (str "A palavra era: " (:word game))))))

(defn switch-player [game]
  (if (= (:current-player game) (:player1 game))
    (assoc game :current-player (:player2 game))
    (assoc game :current-player (:player1 game))))

(defn game-over? [game]
  (or (>= (:incorrect game) (:max-attempt game)) (:winner game)))

(defn process-guess [game guess]
  (let [word (:word game) guessed (:guessed game)]
    (if (contains? guessed guess)
      (do
        (println "Tentativa já feita")
        game)

      (if (contains? (set word) guess)
        (do
          (println "Acertou")
          (let [new-guessed (conj guessed guess)]
            (if (every? #(contains? new-guessed %) word)
              (assoc game :guessed new-guessed :winner (:current-player game))
              (assoc game :guessed new-guessed))))

        (do
          (println "Errou")
          (let [new-incorrect (inc (:incorrect game))]
            (assoc game :guessed (conj guessed guess) :incorrect new-incorrect)))))))

(defn play-game []
  (println "Bem vindo, ao jogo da forca")

  (let [player1 (do (print "Nome do jogador 1: ") (flush) (read-line))
        player2 (do (print "Nome do jogador 2: ") (flush) (read-line))
        word (do (print "Digite a palavra secreta: ") (flush) (read-line))
        max-attempts 6
        game (initialize-game word player1 player2 max-attempts)]

    (loop [game game]
      (if (game-over? game)
        (announce-winner game)
        (do
          (println (str "\nÉ a vez de " (:current-player game)))
          (display-status game)
          (let [guess (do (print "Advinhe uma letra ou algum palpite: ") (flush) (read-line))]
            (if (and (= (count guess) 1) (Character/isLetter (first guess)))
              (let [updated-game (-> game (process-guess (first guess)) (switch-player))]
                (recur updated-game))
              (do
                (println "Entrada inválida. Tente novamente.")
                (recur game)))))))))

(defn -main
  [& args]
  (play-game))
