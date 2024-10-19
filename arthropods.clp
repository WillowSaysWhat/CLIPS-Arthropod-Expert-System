;; features of an arthropod help in a template for init into an object.
(deftemplate arthropod
	(slot id (default myArthropod))
   (slot more-than-six-legs (default nil))
   (slot has-tails (default nil)) 
   (slot is-segmented (default nil))
   (slot feathery-tail (default nil))
   (slot two-tails(default nil) )
   (slot wings-overlap-diag (default nil))
   (slot stubby-wings (default nil))
   (slot tails-longer-than-body-width (default nil))
   (slot thin-and-hairy-tails (default nil))
   (slot more-than-eight-legs (default nil))
   (slot eight-legs-four-pairs (default nil))
   (slot big-claws (default nil))
)

;; FUNCTIONS

;; print question function to mininise duplicate code
(deffunction print-string (?str)
   (printout t ?str crlf)
)

;; question and error handling function
(deffunction ask-question (?yes-symbol ?no-symbol ?question-string)
   (bind ?input "")
   
   ;; while loop to ensure valid input
   (while (and (neq ?input ?yes-symbol) (neq ?input ?no-symbol))
      (printout t ?question-string crlf)
      (bind ?input (read))
      
      ;; check for valid input
      (if (and (neq ?input ?yes-symbol) (neq ?input ?no-symbol)) then
         (printout t "Invalid input. Please answer " ?yes-symbol " or " ?no-symbol "." crlf)
      )
   )
   
   ;; Return the valid input
   ?input
)

;; RULES

;; first question
(defrule ask-about-legs
=>
   (print-string "Welcome to the Freshwater Arthropod Identifier!")

   ;; Call the ask-question function with yes, no, and the question string
   (bind ?legs (ask-question yes no "Does the arthropod have more than six legs? (yes/no):"))

   ;; make object and insert answer once valid input is received
   (assert (arthropod (more-than-six-legs ?legs)))
)

;; does it have tails?
(defrule has-tails
   ?myAnswers <- (arthropod (id myArthropod)(more-than-six-legs no)(has-tails nil))
=>
   ;; different error handling
   (bind ?tails -1)
   (while (and (neq ?tails 0) (neq ?tails 1)(neq ?tails 2))
      (printout t "How many tails? (0=none, 1, 2=more): " crlf)
      (bind ?tails (read))
      ;; error handling
      (if (and (neq ?tails 0) (neq ?tails 1)(neq ?tails 2)) then
         (printout t "Invalid input. Please answer 0-2." crlf)))
   (modify ?myAnswers (has-tails ?tails))
)

;; segmented body question (tails 0)
(defrule segmented-body
   ?myAnswers <- (arthropod (id myArthropod) (more-than-six-legs no) (has-tails 0) (is-segmented nil))
   =>
   ;; Using the ask-question function to ask if the arthropod has segments
   (bind ?segments (ask-question yes no "Does it have segments? (yes/no): " ))

   ;; Modifying the fact with the user's response
   (modify ?myAnswers (is-segmented ?segments))
)

;; wings chubby wings question
(defrule wings-stubby-wings
   ?myAnswers <- (arthropod (id myArthropod) (more-than-six-legs no) (has-tails 0) (is-segmented yes) (stubby-wings nil))
=>
   ;; Use the ask-question function to handle the input process
   (bind ?wings-stubby (ask-question yes no "Does it have stubby wings? (yes/no): " ))

   ;; Modify the original object with the user's answer
   (modify ?myAnswers (stubby-wings ?wings-stubby))
)

;; wings diag question
(defrule wings-overlap-diagonally
   ?myAnswers <- (arthropod (id myArthropod) (more-than-six-legs no) (has-tails 0) (is-segmented no)(wings-overlap-diag nil))
=>
   ;; Use the ask-question function to handle the input process
   (bind ?wings-diag (ask-question yes no "Does the wings overlap diagonally? (yes/no): " ))

   ;; Modify the original object with the user's answer
   (modify ?myAnswers (wings-overlap-diag ?wings-diag))
)

;; feathery tail question (tails 1)
(defrule feathery-tail
	?myAnswers <- (arthropod (id myArthropod) (more-than-six-legs no) (has-tails 1) (feathery-tail nil))
=>
	(bind ?feathery (ask-question yes no "Does it have a feathery tail? (yes/no): "))
	
	;; modify object
	(modify ?myAnswers(feathery-tail ?feathery))
)

;; two tails question (tails +1)
(defrule has-two-tails
	?myAnswers <- (arthropod (id myArthropod) (more-than-six-legs no) (has-tails 2) (two-tails nil))
=>
	(bind ?two-tails (ask-question yes no "Does it have 2 tails? (yes/no): "))
	
	;; modify object
	(modify ?myAnswers(two-tails ?two-tails))
)

;; tails longer than body width question
(defrule tails-longer-than-body-width
	?myAnswers <- (arthropod (id myArthropod) (more-than-six-legs no) (has-tails 2) (two-tails no)(tails-longer-than-body-width nil))
=>
	(bind ?longer-than-body (ask-question yes no "Is the tail longer than the body width? (yes/no): "))
	
	;; modify object
	(modify ?myAnswers(tails-longer-than-body-width ?longer-than-body))
)

;; thin and hairy tails question
(defrule thin-hairy-tails
	?myAnswers <- (arthropod (id myArthropod) (more-than-six-legs no) (has-tails 2) (two-tails no)(tails-longer-than-body-width yes)(thin-and-hairy-tails nil))
=>
	(bind ?thin-and-hairy (ask-question yes no "are they thin and heary tails? (yes/no): "))
	
	;; modify object
	(modify ?myAnswers(thin-and-hairy-tails ?thin-and-hairy))
)

;; Rule for asking if it has more than 8 legs
(defrule has-eight-legs
   ?myAnswers <- (arthropod (id myArthropod)(more-than-six-legs yes)(more-than-eight-legs nil))
=>
   ;; Call the ask-question function with yes, no, and the specific question string
   (bind ?eight-legs (ask-question yes no "Does it have more than eight legs? (yes/no):"))
   
   ;; Modify the original fact with the answer to the question
   (modify ?myAnswers (more-than-eight-legs ?eight-legs))
)

;; big claws question
(defrule has-big-claws
   ?myAnswers <- (arthropod (id myArthropod) (more-than-six-legs yes) (more-than-eight-legs yes)(big-claws nil))
=>
   ;; Using the ask-question function to prompt the user
   (bind ?claws (ask-question yes no "Does it have big claws? (yes/no): "))
   
   ;; Modifying the fact with the user's response
   (modify ?myAnswers (big-claws ?claws))
)



;; Answers

;; Water Boatman
(defrule identify-water-batman
   ?myAnswers <- (arthropod (id myArthropod)(more-than-six-legs no)(has-tails 0)(is-segmented no)(wings-overlap-diag yes)) 
=>
   (print-string "")
   (print-string "(more-than-six-legs no)(has-tails 0)(is-segmented no)(wings-overlap-diag yes)")
   (print-string "It is most likely a Water Boatman.")
)

;; Water Beetle
(defrule identify-beetle
   ?myAnswers <- (arthropod (id myArthropod)(more-than-six-legs no)(has-tails 0)(is-segmented no)(wings-overlap-diag no)) 
=>
   (print-string "")
   (print-string "(more-than-six-legs no)(has-tails 0)(is-segmented no)(wings-overlap-diag no)")
   (print-string "It is most likely a Water Beetle.")
)

;; Caseless Caddis Fly or Beetle Lavae
(defrule identify-fly-beetle
   ?myAnswers <- (arthropod (id myArthropod)(more-than-six-legs no)(has-tails 0)(is-segmented yes)(stubby-wings no)) 
=>
   (print-string "")
   (print-string "(more-than-six-legs no)(has-tails 0)(is-segmented yes)(stubby-wings no)")
   (print-string "It is most likely a Caseless Caddis Fly or Beetle Larvae.")
)

;; Dragonfly Nymph
(defrule identify-dragonfly-nymph
   ?myAnswers <- (arthropod (id myArthropod)(more-than-six-legs no)(has-tails 0)(is-segmented yes)(stubby-wings yes)) 
=>
   (print-string "")
   (print-string "(more-than-six-legs no)(has-tails 0)(is-segmented yes)(stubby-wings yes)")
   (print-string "It is most likely a Dragonfly Nymph.")
)

;; stick insect or water scorpion
(defrule identify-stick-insect-scorpion
   ?myAnswers <- (arthropod (id myArthropod)(more-than-six-legs no)(has-tails 1)(feathery-tail no)) 
=>
   (print-string "")
   (print-string "(more-than-six-legs no)(has-tails 1)(feathery-tail no)")
   (print-string "It is most likely a Water Stick Insect or a Water Scorpion." )
)

;; alderly fly lavae
(defrule identify-alderfly-lavae
   ?myAnswers <- (arthropod (id myArthropod)(more-than-six-legs no)(has-tails 1)(feathery-tail yes)) 
=>
   (print-string "")
   (print-string "(more-than-six-legs no)(has-tails 1)(feathery-tail yes)")
   (print-string "It is most likely an Alderfly larvae." )
)

;; CHECK THIS
(defrule identify-dragonfly-nymph-second
   ?myAnswers <- (arthropod (id myArthropod)(more-than-six-legs no)(has-tails 2)(two-tails no)(tails-longer-than-body-width no)) 
=>
   (print-string "")
   (print-string "(more-than-six-legs no)(has-tails 2)(two-tails no)(tails-longer-than-body-width no)")
   (print-string "It is most likely a Dragonfly Nymph." )
)

;; mayfly nymph
(defrule identify-mayfly-nymph
   ?myAnswers <- (arthropod (id myArthropod)(more-than-six-legs no)(has-tails 2)(two-tails no)(tails-longer-than-body-width yes)(thin-and-hairy-tails yes)) 
=>
   (print-string "")
   (print-string "(more-than-six-legs no)(has-tails 2)(two-tails no)(tails-longer-than-body-width yes)(thin-and-hairy-tails yes)")
   (print-string "It is most likely a Mayfly Nymph." )
)

;; damselfly lavae
(defrule identify-damselfly-nymph
   ?myAnswers <- (arthropod (id myArthropod)(more-than-six-legs no)(has-tails 2)(two-tails no)(tails-longer-than-body-width yes)(thin-and-hairy-tails no)) 
=>
   (print-string "")
   (print-string "(more-than-six-legs no)(has-tails 2)(two-tails no)(tails-longer-than-body-width yes)(thin-and-hairy-tails no)")
   (print-string "It is most likely a Damselfly Nymph." )
)

;; stonefly nymph
(defrule identify-stonefly-nymph
   ?myAnswers <- (arthropod (id myArthropod)(more-than-six-legs no)(has-tails 2)(two-tails yes)) 
=>
   (print-string "")
   (print-string "[more-than-six-legs no][has-tails 2][two-tails yes]")
   (print-string "It is most likely a Stonefly Nymph.")
)

;;  water spider or mite
(defrule identify-water-Spider-or-mite
   ?myAnswers <- (arthropod (id myArthropod)(more-than-six-legs yes)(more-than-eight-legs no)) 
=>
   (print-string "")
   (print-string "[more-than-six-legs yes] [more-than-eight-legs no]")
   (print-string "If the legs are paired, it is a")
   (print-string "Water Spider or Mite.")
)

;; crayfish
(defrule identify-freshwater-crayfish
   ?myAnswers <- (arthropod (id myArthropod)(more-than-six-legs yes)(more-than-eight-legs yes)(big-claws yes)) 
=>
   (print-string "")
	(print-string "[more-than-six-legs yes] [more-than-eight-legs yes] [big-claws yes]")
   (print-string "It is most likely a Freshwater Crayfish")
)

;; shrimp or hoglouse
(defrule identify-freshwater-shrimp-or-hoglouse
   ?myAnswers <- (arthropod (id myArthropod)(more-than-six-legs yes)(more-than-eight-legs yes)(big-claws no)) 
=> 
	 (print-string "")
	 (print-string "[more-than-six-legs yes] [more-than-eight-legs yes] [big-claws no]")
   (print-string "It is most likely a Freshwater Shrimp or Hoglouse.")
)