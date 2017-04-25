;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Name: William Siu                 Date:3/30/17
;;;; Course: ICS313        Assignment: Homework 6  
;;;; File: wsiu4_6.lisp

(defconstant +ID+ "William Siu")

;;function to print out name, course name, and assignment number
(defun i-d (course-number assignment-number)
  (format t "Name: ~a~%Course: ICS~d~%Assignment # ~d~%" +ID+ course-number assignment-number))

(defparameter *nodes* '((courtyard (You are currently in the courtyard with your army battling against
                                         the enemy. You should leave them be as you see they can handle themselves.))
                        (inner-courtyard (You are in the inner courtyard  of the castle. You see some enemy forces already inside
                                        but are busy fighting your own men.))
                        (library (You are in the library. There are dead bodies here. Luckily it is the enemy only.))
                        (armory (You are in the armory. This place is heavily guarded and secured. Gear up if you need to. 
                                     There is also a forge here. It could make or repair something perhaps.))
                        (stairs (You are at the stairs. The fighting seems to have extented further into the castle.))
                        (main-hall-north (You are in the northern main hall. The fighting seems to be leading into the upper area.))
                        (main-hall-south (You are in the southern main hall. The fighting seems to be leading into the upper area.))
                        (dining-hall (You are in the dining hall. It is a total mess! There are food all over the place
                                          after a scuffle with the enemy.))
                        (kitchen (You are in the kitchen. It is unpopulated due to evacuations for the peoples safety.))
                        (guest-room (You are in the guest room. It is empty but luckily undamaged.))
                        (guard-house (You are in the guard-house. You see that your men are discussing measures about defeating
                                          the invading forces.))
                        (inner-stairs (You are on top upstairs. There a few dead bodies from the enemy side. You notice
                                           some of your men are fatigued.))
                        (upper-main-hall (You are in the upper-main hall. The fighting is even here.))
                        (throne-room (You're in the throne room. You have view of the battle taking place.))
                        (upper-castle-stairs (You are in the room leading to the upper castle. There are dead guards and the
                                                  magical defenses are destroyed. The path to the upper courtyard is unguarded.))
                        (bed-chamber (You are in your room. Thankfully it is undamaged and nothing is misplaced.))
                        (royal-chamber (You are in the royal chamber. You noticed your people are dead on the floor and the
                                           entrance to the underground is open...))
                        (underground (You are underground now. You hear chattering far below the staircase.))
                        (underground-stairs (You are walking down the path to reach below underground. The chattering is getting louder.))
                        (underground-hall (You are in the hall underground. The chattering seems to be coming from the chapel.))
                        (chapel (You are in the chapel. It is your men discussing plans for a retreat if things do not end well. There is an
                                     altar to help enhance your equipment. Looks like it is still functional.))
                        (escape-room (You are in the escape room. The path to run away is secured in case something goes wrong.))
                        (upper-courtyard (You are in the upper courtyard. There are a bunch of dead bodies. At a closer look it is
                                              your men. There seems to be chattering behind the door.)) 
                        (magic-chamber-room (The crystal remains intact but the enemy leader is making its way to destroy it. 
                                              At a closer look you see that it is the demonic leader Azkarnoft. He notices your presence.))))

(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

; Map of game and its location connections
; wanted to design a way to have random encounters by chance everytime you move room to room but ran out of time to tinker with.

(defparameter *edges* '((courtyard (inner-courtyard north door))
                        (inner-courtyard (courtyard south door)
                                         (library west door)
                                         (armory east door)                                         
                                         (stairs upstairs path))
                        (armory (inner-courtyard west door))
                        (library (inner-courtyard east door))
                        (stairs (inner-courtyard downstairs path)                          
                                (main-hall-north upstairs_left path)
                                (main-hall-north upstairs_right path))
                        (main-hall-north (stairs downstairs_right path)
                                         (stairs downstairs_left path)
                                         (dining-hall west door)                                         
                                         (guest-room east door)
                                         (main-hall-south south door))
                        (guest-room (main-hall-north west door))
                        (dining-hall (main-hall-north east door)
                                     (kitchen west door))
                        (kitchen(dining-hall east door))
                        (main-hall-south (main-hall-north north door)
                                         (guard-house east door)
                                         (inner-stairs upstairs path))
                        (guard-house (main-hall-south west door))
                        (inner-stairs (main-hall-south downstairs path)
                                      (upper-main-hall north door)
                                      (throne-room south door))
                        (throne-room (inner-stairs north door))
                        (upper-main-hall (inner-stairs south door)
                                         (bed-chamber east door)
                                         (royal-chamber north door)
                                         (upper-castle-stairs west door))
                        (bed-chamber (upper-main-hall west door))
                        (royal-chamber (upper-main-hall south door)
                                       (underground north path))
                        (underground (royal-chamber south door)
                                     (underground-stairs downstairs path))
                        (underground-stairs (underground upstairs path)
                                            (underground-hall downstairs path))
                        (underground-hall (underground-stairs upstairs path)
                                          (chapel north door)
                                          (escape-room south door))
                        (chapel (underground-hall south door))
                        (escape-room (underground-hall north door))
                        (upper-castle-stairs (upper-main-hall east door)
                                             (upper-courtyard upstairs path))
                        (upper-courtyard (upper-castle-stairs downstairs path)
                                         (magic-chamber-room north door))
                        (magic-chamber-room (upper-courtyard south door))))

; to tell that there is a walkable path in this direction

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))


; objects
; spell_book required to make the 2 SPELS work.

(defparameter *objects* '(sword broken_shield spell_book crystal))

(defparameter *object-locations* '((sword armory)
                                   (broken_shield inner-stairs)
                                   (spell_book bed-chamber)
                                   (crystal chapel)))


(defun objects-at (loc objs obj-loc)
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'courtyard)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

; command options to use

; walk command. will refer to help commands if need help

(defun walk (direction)
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
        '(you cannot go that way. Refer to help if you need assistance.)))))

; command to pick up object. will refer to help commands if need help

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	  (t '(you cannot get that. Refer to help commands if you need assistance.))))


(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun have (object) 
  (member object (cdr (inventory))))

; Beginning intro of the game. Then the game starts

(defun start-game ()
  (format t "~%Your kingdom is under attack from dark forces. The enchanted magic the castle has is 
repelling most of the invading armies but some have slipped through into the
castle. If they reach to the magic chamber room and destroy the power source then the
magic will wear off and you will be overrun. You must head there and make sure
it is secured.")
(game-repl))

; help commands to guide player

(defun game-repl ()
  (format t "~%:Type help, h, or ? for help:~%")
  (format t "> ")
  (let ((cmd (game-read)))
   (unless (eq (car cmd) 'quit)
     (if (or (eq (car cmd) '?) (eq (car cmd) 'help) (eq (car cmd) 'h))     ;If user type help, h or ? it will print the available commands.
         (progn
           (format t "~%:The following commands below are available:~%")
           (format t "~%~29a Describes your current loaction." "look")
           (format t "~%~29a Walk to a new location. Example - walk upstairs or walk west or walk upstairs_left." "walk direction")
           (format t "~%~26a    Pickup an item in your current location. Example - pickup pie" "pickup (object name)")
           (format t "~%~29a Opens up your inventory bag." "inventory")
           (format t "~%~29a Repairs a broken item in your inventory. Objects must be listed in order and must be in the right area. Example - repair sword broken_shield." "repair (object1 object2)")
           (format t "~%~29a Enchants an item in your inventory. Objects must be listed in order and must be in the right area. Example - enchant crystal broken_shield." "enchant (object1 object2)")
           (format t "~%~29a Attacks a specific name target. Example - attack sword goblin." "attack (object target name)")
           (format t "~%~29a Quits the game.~%~%" "quit")
           (game-repl))
       (progn
         (game-print (game-eval cmd))
         (game-repl))))))

(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
                       (list 'quote x)))
           (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
  (fresh-line))


; SPELS/ACTIONS

(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
            '(i cant ,command like that.)))
     (pushnew ',command *allowed-commands*)))

; repair action

(defparameter *repaired_shield* nil)

(game-action repair broken_shield spell_book armory
             (if (and (have 'spell_book) (not *repaired_shield*))
                 (progn (setf *repaired_shield* 't)
                        '(You use spell book to cast a fire spell. You start repairing the shield. The shield has been repaired.))
               '(There is no fire to use. Perhaps a spell book would help repairing the shield.)))

; enchant action
; also requires the spell_book because you need magic to enchant your weapon :)

(defparameter *enchant-sword* nil)

(game-action enchant sword crystal chapel
             (if (and (and (have 'crystal) (not *enchant-sword*)) (have 'spell_book))
                 (progn (setf *enchant-sword* 't)
                        '(You use your spell book to imbue the crystal into your sword. Your sword is now enchanted!))
               '(There is no crystal to use or you cant remember the spell. Perhaps a spell from your book would help enhancing the sword.)))

; attack action

(game-action attack sword Azkarnoft magic-chamber-room
             (cond ((not *repaired_shield*) '(You charge in to slay the leader. You land a blow but the leader attacks back and deals  ; losing condition - shield is not repaired
                                                  a fatal blow to you. The kingdom falls. You lose! THE END!))
                   ((not *enchant-sword*) '(You charge in to slay the leader. You land a blow to the leader. He attacks back and you     ; losing condition - sword is not enchanted
                                             block a heavy blow to you but the shield breaks. You hit him again and almost got him.
                                             But he strikes you again and it was a fatal blow. The kingdom falls. You lose! THE END!))
                   (t '(You charge in to slay the leader. You land a blow to the leader. He attacks back and you block                    ; win condition true otherwise
                                             a heavy blow to you but the shield breaks. You hit him again and he dies.
                                             The enemy leader falls and the enemy is defeated for good. You win! THE END!))))
