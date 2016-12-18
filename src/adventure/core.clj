(ns adventure.core
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
               )
  (:gen-class))

(def currentMap
  {
   :healingRoom {:desc "You feel an overwhelming calming sensation. You see a bed ahead along with a comfortable pillow and some blankets. All you want to do right now is sleep."

           :title "the healingRoom "
           :dir {:south :hallwayA
                  }
           :content #{"cellphone" "blanket" "pillow" "glass of wine"}
           }

           :friendRoom {:desc "This bedroom seems awfully familiar. Perhaps it belongs to a friend of yours. And look, on the ground is a sheet of paper. This might be useful"
                   :title "friend's room "
                   :dir {:east :hallwayA}
                   :content #{"paper"}
                 }

   :hallwayA {:desc "A connecting hallway reveals itself"
              :title "hallway into the healingRoom"
              :dir {:north :healingRoom
                    :south :hallwayB
                    :east :bathroom
                    :west :friendRoom}
              :content #{"pencil"}
            }



   :bathroom {:desc "Hah, this is perfect. Must have had too much water a while back...."
              :title "the bathroom "
              :dir {:west :hallwayA}
              :content #{"toothbrush"}
            }
   :living_room {:desc "This living room needs some upgrade. The furniture looks moldy. However, you stumble yourself with an abacus on the ground. This might be useful."
              :title "the living room "
              :todo "east to the hallway"
              :dir {:east :hallwayB
                  }
              :puzzle [
                    {:question "Think of a number. Double it. Add ten. Half it. Take away the number you started with. Your answer is five. Enter 5 to proceed."
                     :answer "5"
                     :hint ""}
                    {:question "___ + 5 + 7 = 20"
                     :answer "8"
                     :hint ""}
                    {:question "Number ___ is afraid because seven ate nine"
                     :answer "6"
                     :hint ""}
                    {:question "What number you cannot divide by?"
                     :answer "0"
                     :hint ""}
                    {:question "Convert the largest month into an integer"
                     :answer "12"
                     :hint ""}
                    {:question "An octopus has ___ tentacles"
                      :answer "8"
                      :hint ""}
                   {:question "Cow has ___ stomachs"
                        :answer "4"
                        :hint ""}
              ]
              :content #{"abacus"}
            }

   :hallwayB {:desc "This hallway was a little bigger than the previous one you saw, but there is an awful draft slipping through. Man these landlords are slacking! "
              :title "the hallway next to the entrance"
              :todo "north: healingRoom, south: entrance, east: farmStand, west: living room"
              :dir {:north :hallwayA
                    :south :entrance
                    :east :farmStand
                    :west :living_room }
              :content #{}}

              :entrance {:desc "Should you step through this? It seems quite intising..."
                     :title "the entrance "
                     :todo "north to the hallway"
                     :dir {:north :hallwayB
                           :south :path
                       }
                     :content #{}}

   :farmStand {:desc "By now you must be starving. You notice that there is some food placed on the table."
              :title "the farmStand "
              :todo "west to the hallway"
              :dir {:west :hallwayB
               }
              :content #{"meal" "apple"}}



    :path {:desc "This actually might lead somewhere. You continue to walk."
           :title "the path"
           :dir { :north :entrance
                  :south :testingCenter
             }
           :content #{"potions", "stone"}}

    :brickBuilding {:desc "The building is new. It's getting slightly chilly. You should check the inside of this building, maybe you will find a cozy coffee shop to sit down at"
           :title "brickBuilding "
           :dir {:north :testingCenter
                 :in :coffeeShop}
           :content #{}
         }

         :testingCenterBasement {:desc "There's a creature here that maybe you should stay away from...."
                   :title "testingCenter Basement"
                   :dir {:up :testingCenter}
                   :content #{}}

    :coffeeShop {:desc "It was as if you never had coffee before. The aromas of the coffee smells delicious, and even taking a whiff of it makes you stay awake"
           :title "coffeeShop shack"
           :dir {:out :brickBuilding}
           :content #{"coffee", "cup", "sugar"}}

    :testingCenter {:desc "There's an exam waiting? No way!"
            :title "testingCenter "
            :dir {:north :path
                  :down :testingCenterBasement
                  :south :brickBuilding
                  :east :grassyField
                }
            :content #{"cookies"}
    }




              :seat {:desc "Finally somewhere to sit down"
                     :title "A chair"
                     :dir {
                          :leave :Library
                          :out :Library
                       }
                      :content #{}
                     }

    :grassyField {:desc "You feel that your body has exhausted itself. Time for a long deserved nap"
             :title "field with grass"
             :dir {
               :east :Library
               :west :testingCenter
               }
             :content #{}}
    :Library {:desc "My oh my, there are so many people here. This must be a very invaluable institution"
               :title "the Library "
               :dir {
                  :west :grassyField
                  :north :cobbleStreet
                  :east :busStation
                  :find :seat
                  :in :seat
               }
               :content #{}
             }

    :cobbleStreet {:desc "Cobble stones paved the road beneath your feet"
            :title "cobbleStreet "
            :dir {:south :Library
                  :west :bar
                  }
                :content #{}}

                :nyc {:desc "Welcome to New York City! After a long ride, you just want to get out of the bus and explore this place"
                          :title "nyc"
                          :dir {
                            :take :busStation
                            :south :chinatown
                            :north :manhanttan
                            }
                        :content #{}
                          }


    :bar {:desc "Time for a nice drink"
          :title "Bar"
          :todo "drink"
          :dir { :east :cobbleStreet}
          :content #{"alcohol", "drinks"}
          }

          :chinatown {:desc "Boy, should have brushed up on my chinese"
                      :title "ChinaTown"
                      :dir {
                          :north :nyc
                          :in :singingBar
                        }
                      :content #{}
                      }

    :busStation {:desc "This place can take you outside of campus, time to do some exploration on your own!"
          :title "the Bus Station"
          :dir {
             :west :Library
             :take :nyc
            }
          :content #{}
          }



    :manhanttan {:desc "You are at the top of manhanttan. Everything just feels so small..."
                  :title "manhanttan"
                  :dir { :south :nyc}
                  :content #{"telescope"}}

                  :singingBar {:desc "There are some pretty ladies here"
                        :title "singingBar"
                        :dir {
                             :out :chinatown
                          }
                        :content #{"mic"}}

       })


(def exam
  {:hp 250
   :att 30
   :defeat 0})








(def adventurer
  {:location :healingRoom
   :name ""
   :social 10
   :before #{}
    :inventory #{}
   :hp 150
   :skill 10



   :fight 0})


   (defn cry [adv]
     (let [currentRoom (adv :location)]
       (if (= currentRoom :testingCenterBasement)
             (do (println "Let me recharge!")(assoc-in adv [:hp] 100))
             (do (println "Can't perform this now")adv))))

;function for talk
(defn talk [adv]
  (let [currentRoom (adv :location)]
  (if (= currentRoom :seat)
      (if (>= (adv :social) 50)(do (println "This girl seems happy talking to you")(update-in adv [:inventory] #(conj % "girlfriend")))
                               (do (println "The girl doesn't seem interested in you...")adv)
                               )
    (do (println "Why not just talk to yourself... -chuckle-")adv))))

    (defn status [adv mon]
      (if (< (adv :hp) 1) (do (println "Perhaps this isn't the best thing for you, let's move back into the healingRoom for you to rest")(sleep (assoc-in (assoc-in adv [:fight] 0) [:location] :healingRoom)))
      (let [fight (adv :fight)
            de (mon :defeat)]
          (if (and (= fight 1)(= de 0)) adv
      (let [location (adv :location)]
        (print (str "You are at " (-> currentMap location :title)))
        (when-not ((adv :before) location)
          (print (-> currentMap location :desc) ) )
        (update-in adv [:before] #(conj % location)))))))

        (defn jump [adv]
          (let [currentRoom (adv :location)]
           (if (not (= currentRoom :manhanttan)) (do (println "What a beautiful sight!")adv)
              (do (println "You suddenly slipped and fell into a giant hole\n What happened next was extremely blurry for you to remember, but you have gained a new skill")
              (update-in (assoc-in adv [:location] :healingRoom) [:inventory] #(conj % "bean"))))))

;the function for sleep which require the adv to be in the healingRoom and restore the health to full
(defn sleep [adv]
 (let [currentRoom (adv :location)]
   (if (= currentRoom :healingRoom)
     (do (println "Sleep hasn't felt this good in days!")(assoc-in adv [:hp] 100))
     (do (println "Unfortunately it is not socially acceptable for you to sit here. Please go somewhere else")adv))))

     (defn dance [adv]
      (let [currentRoom (adv :location)]
        (if (= currentRoom :bar)
          (do (println "High school swing dancing is coming handy")(assoc-in adv [:social] 10))
          (do (println "Unfortunately, you cannot perform this now")adv))))




;the function for "status" which prints out the o_adventurers current status
(defn adv_status [adv]
  (
    do (println (str ""(-> adv :name)". HP: "(-> adv :hp) ". Skill Point: " (-> adv :skill) ". Social Point: " (-> adv :social)))adv
  )
)

;fight : get into battling mode
(defn battling [adv mon]
  (let [currentRoom (adv :location)
        de (mon :defeat)]
      (if (not (= currentRoom :testingCenterBasement)) (do (println "Phew, I think I don't have to take an exam here")adv)
          (if (= de 0)(do (println "Oh no, I think something bad is going to happen!")(assoc-in adv [:fight] 1))
                      (do (println "Congratulations.. All the big monsters have been defeated by you!")adv)))))

;function for jump


;go to the room given location
(defn go [dir adv]
  (let [currentRoom (adv :location)
        inv ( adv :inventory)]
   (if-let [dest (get-in currentMap [currentRoom :dir dir])]
     (if (and (= dest :entrance)(not (inv "key"))) (do (println "You tried to go through the entrance but you forgot your key. Your key is in the living room in case you have forgotten.")adv) (assoc-in adv [:location] dest))
     (do (println "You cannot go that direction. ")
         adv) )))

         (defn drink [adv obj]
           (let [inv (adv :inventory)]
             (if (inv (name obj))
               (case (name obj)
                 "alcohol"  (do (println "This liquor taste swirls in your mouth and causes you funny feelings "(str (name obj))" I need more of this")(update-in (update-in adv [:social] + 10) [:inventory] #(disj % "alcohol")))
                 "coffee"   (do (println "This is better than anything I've ever tasted "(str (name obj))" Where can I get more of this!.")(update-in (update-in adv [:skill] + 15) [:inventory] #(disj % "coffee")))
                 (do (println "You should not drink this, it doesn't look safe") adv))
              (do (println "Nothing can be found "(str (name obj))" sorry")adv))))


(defn generate_puzzle [n puz]
  (let [num (rand-int n)

       ans (get-in puz [num :answer] )
       ques (get-in puz [num :question] )
       _ (println (str ques))
       answer (read-line)]
  (if (= answer ans)true false)))


(defn puzzles [adv]
    (let [currentRoom (adv :location)
          puz (get-in currentMap [currentRoom :puzzle])
          inv (adv :inventory)]
    (if (nil? puz)
        (if (inv "key")(do (println "The key already exists")adv)adv)
        (if (generate_puzzle (count puz) puz)
          (let [cont (get-in currentMap [currentRoom :content])]
              (if (inv "key") (do (println "You already have the key") adv)
                                    (do (println "You have obtained the key, try not to drop it")(update-in adv [:inventory] #(conj % "key")))))
              (if (inv "key") (do (println "You have the key already, but are more than welcome to try again")adv)
                              (do (println "Opps")adv))
         ))))




(defn monster_attack [adv mon num obj]
    (let [currentRoom (adv :location)
          hp (get-in adv [:hp])
          monster_hp (get-in mon [:hp])
          att (rand-int (+ (mon :att) 1))

          ]
          (if (and (< monster_hp 40)(= obj "potions")) (assoc-in (update-in (update-in adv [:inventory] #(conj % "exam")) [:inventory] #(disj % obj)) [:fight] 0)
          (if (< (- monster_hp num) 1) (do (println "Oh my goodness, you have successfully defeated this giant monster!")(assoc-in (update-in adv [:inventory] #(disj % obj)) [:fight] 0))
          (if (< (- hp att) 1) (do (println "You feel a feeling of fatigue overwhelming you")(update-in (update-in adv [:inventory] #(disj % obj)) [:hp] - att))
          (cond
            (= att 0) (do (println "The monster had missed, your current hitpoint is"(str (- hp att)))(update-in (update-in adv [:inventory] #(disj % obj)) [:hp] - att))
            (<= att 5) (do (println "The monster had used Injacuration, your health got reduced to"(str (- hp att)))(update-in (update-in adv [:inventory] #(disj % obj)) [:hp] - att))
            (<= att 10) (do (println "The monster had used Farencation, your health got reduced to"(str (- hp att)))(update-in (update-in adv [:inventory] #(disj % obj)) [:hp] - att))
            (<= att 15) (do (println "The monster had used Tiunizh, your health is now "(str (- hp att)))(update-in (update-in adv [:inventory] #(disj % obj)) [:hp] - att))
            :else (do (println "You did not expect this coming at all but the monster had casted one strong spell and your health is now "(str (- hp att)))(update-in (update-in adv [:inventory] #(disj % obj)) [:hp] - att))
          ))))))

(defn consume [adv]
    (let [inv (get-in adv :inventory)]
        (if (inv "meal") (do (println "You have eaten the meal")(update-in (update-in adv [:inventory] #(disj % "meal")) [:skill] + 10))
                                     (do (println "Everything you have edible ran out")adv))))


(defn acquire [obj adv]
  (let [currentRoom (adv :location)
      inv (get-in adv [:inventory])
      cont (get-in currentMap [currentRoom :content])
         ]
   (if (cont (name obj))
    (if (inv (name obj)) (do (println "Don't pick this up") adv)
        (do (println (str "Item acquired:: "(name obj)))(update-in adv [:inventory] #(conj % (name obj)))))
     (do (println "Not existent ")
        adv) )))

(defn discard [obj adv]
    (let [inv (get-in adv [:inventory])]
    (if (inv (name obj))(do (println (str "You notice that your pocket feels lighter. Unfortunately, you have dropped" (name obj)))(update-in adv [:inventory] #(disj %(name obj))))
                        (do (println "Not existent")adv))))

(defn print_inventory [adv]
    (do (println (seq (adv :inventory)))
      adv))

;the function ofr "content" which prints out the content in the o_adventurers location
(defn print_location_content [adv]
    (let [location (adv :location)]
    (do (println (str (-> currentMap location :content)))adv)))



;the function for "status" which prints out the o_adventurers current status
(defn adv_status_mon [adv mon]
  (do (println (str "Name: "(-> adv :name)". HP: "(-> adv :hp) ". Skill Point: " (-> adv :skill) ". Social Point: " (-> adv :social)))mon))

;attack_mon
(defn attack [adv mon]
  (let [skill (adv :skill)]
  (println "Attacking the monster" (str (adv :skill))"hp")
  (update-in mon [:hp] - skill)))

;use objects to att
(defn using [adv mon obj]
  (let [inv (adv :inventory)
        monster_hp (mon :hp)]
   (if (inv (name obj))
    (case (name obj)
      "potions" (if (>= monster_hp 50)(do (println "This potion does not seem to be working")mon) (do (println "Got em!")(assoc-in mon [:defeat] 1)))
      "coffee" (do (println "COFFEE!")mon)
      "pillow" (do (println "+10 health")mon)
      "mic" (do (println "The microphone suddenly activated itself and decreased the monster's hp")(update-in mon [:hp] - 25))
      "abacus" (do (println "This device was never seen before by the monster, the monster had lost 25 hp")(update-in mon [:hp] - 25))
      "blanket" (do (println "I knew this was helpful. Wrapping it around yourself, you have recovered 20 health")mon)
      "stone" (do (println "You threw this at the exam, it crouched down in defeat")(update-in mon [:hp] - 50))
      "paper" (do (println "I knew this would be helpful, the monster lost 70 hp")(update-in mon [:hp] - 70))
      "telescop" (do (println "The telescope pierces through the monster")(update-in mon [:hp] - 15))
      (do (println (str (name obj))"didn't seem to be effective")mon))
    (do (println "Hmm you don't have" (str (name obj))) mon)
      ) ) )

      (defn mon_stat [adv mon]
        (let [fight (adv :fight)
             monster_hp (get-in mon [:hp])]
          (if (< monster_hp 1) (assoc-in mon [:defeat] 1)
          (if (= fight 0) mon
        (let [currentRoom (adv :location)
              ]
              (do (println "Let's check up on the monster's health. It seems like the health of it is: "(str monster_hp) "\nPlease type in your next possible move")
              mon)
              )))))

(defn respond_monster [inst adv monster]
  (let [state (adv :fight)
        monster_hp (monster :hp)]
  (if (= state 0)monster
  (if (< monster_hp 1) monster
  (if (contains? inst 1)
    (match [(inst 0)]
      [:use] (using adv monster (inst 1))
      [_] (do
          (println (str "I'm terribly sorry "(-> adv :name)". This action is not allowed currently, please select another one"))
          monster)
    )
    (match inst

      [:s] (adv_status_mon adv monster)
      [:status] (adv_status_mon adv monster)
      [:attack] (attack adv monster)
      [:att] (attack adv monster)
      _ (do
          (println (str "I'm sorry "(-> adv :name)". I cannot allow you to do that."))
          monster) )
    )))))

(defn respond [inst adv monster]
  (let [state (adv :fight)
        inv (adv :inventory)
      ]
  (if (= state 1 )
    (let [monster_hp (monster :hp)
          skill (adv :skill)]
    (cond
    (= (inst 0) :att) (monster_attack adv monster skill "")
    (= (inst 0) :attack) (monster_attack adv monster skill "")
    (= (inst 0) :use)   (if (inv (name (inst 1)))
                            (case (name (inst 1))
                            "coffee" (monster_attack (update-in adv [:skill] + 10) monster 0 "coffee")
                             "abacus" (monster_attack adv monster 25 "abacus")
                             "potions" (monster_attack adv monster 0 "potions")
                             "stone" (monster_attack adv monster 50 "stone")
                             "pillow" (monster_attack (update-in adv [:hp] + 10)monster 0 "pillow")
                             "mic" (monster_attack adv monster 30 "mic")
                             "paper" (monster_attack adv monster 70 "paper")

                             "telescope" (monster_attack adv monster 0 "telescope")
                             "blanket" (monster_attack (update-in adv [:hp] + 30)monster 0 "blanket")

                             adv)
                             adv)
    :else  adv) )
  (if (contains? inst 1)
  (match [(inst 0)]
          [:get] (acquire (inst 1) adv)
          [:drink] (drink (inst 1) adv)
          [:acquire] (acquire (inst 1) adv)
          [:drop] (discard (inst 1) adv)

          [_](do
              (println (str "This move is unexplainable "(-> adv :name)". Please select another one."))
              adv)
          )
  (match inst
         [:up] (go :up adv)
         [:find] (go :find adv)
         [:investigate] (go :find adv)
         [:south] (go :south adv)
         [:slumber] (sleep adv)
         [:north] (go :north adv)
         [:down] (go :down adv)
         [:west] (go :west adv)
         [:east] (go :east adv)
         [:sleep] (sleep adv)
         [:content] (print_location_content adv)
         [:cry] (cry adv)
         [:take] (go :take adv)
         [:consume] (consume adv)
         [:jump] (jump adv)
         [:investigate] (go :find adv)
         [:battle] (battling adv monster)
         [:status] (adv_status adv)
         [:leave] (go :leave adv)
         [:consume] (consume adv)
         [:first] (do (println firstTime) adv)
         [:inventory] (print_inventory adv)
         [:in] (go :in adv)
         [:out] (go :out adv)
         [:puzzle] (puzzles adv)
         [:solve_puzzle] (puzzles adv)
         [:i] (print_inventory adv)
         [:s] (adv_status adv)
         [:puzzle] (puzzles adv)
         [:help] (do (println help)adv)
         [:fight] (battling adv monster)
         [:talk] (talk adv)

         _ (do
             (println (str "I'm sorry "(-> adv :name)". I cannot allow you to do that."))
             adv) )
    ) ) ) )


(defn to-keywords [st]
   (mapv keyword (str/split st #" +")))

(defn -main
  [& args]
  (println "Hello my darling! Looks like it is a beautiful day today. The birds are chirping, the air is crisp. How about we go for a nice walk? Oh wait, have you been here before, if not, our campus is absolutely gorgeous!")
  (println "What is your name?")
  (let [n (read-line)
        adv-n (adventurer :name)
        o_adventurer (assoc-in adventurer [:name] n)]
  (println (str "What a lovely name, "(str (-> o_adventurer :name))"! I hope you're well rested for today! Feel free to explore our campus."))
  (loop [the-m currentMap
         the-mon exam
         the-a o_adventurer
         ]
    (let [location (the-a :location)
          the-a' (status the-a the-mon)
          the-mon' (mon_stat the-a' the-mon)
          _      (if (= (the-a :fight) 0) (println " What would you like to do next?") (print ""))
          inst   (read-line) ]
      (recur the-m (respond_monster (to-keywords inst) the-a' the-mon') (respond (to-keywords inst) the-a' the-mon'))
      ) ) )
  )
