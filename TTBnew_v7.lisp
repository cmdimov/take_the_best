;; In addition to the simplest, naive version of TTB, which follows the original definition of Gigerenzer & Goldstein (1996), this models directly makes a choice if both companies have the same name
;; This *fifth* version of TTB also loads the participant-specific retrieval threshold and inter-trail-intervals, and gets BOLD predictions for the declarative module
;; Also, this fifth version adds a fixation cross lasting 2 seconds at the beginning of each trial, and a 10 minute break between blocks
;; This version, on the other hand, stores company names in imaginal instead of looking back and forth


(clear-all)

;; commands to run exp 1 and exp2
; (run-subjects-times (range 1 (length *exp1-dm*)) *exp1-vpns* *exp1-dm* *exp1-df* *exp1-an* *exp1-rt* *exp1-output* 1)

(defparameter *version* "v7")
(defparameter *current-directory* (translate-logical-pathname "ACT-R6:")) ;absolute path to currect directory
(defparameter *exp1-an* (directory (merge-pathnames "anExp1/*.txt" *current-directory*)))
(defparameter *exp2-an* (directory (merge-pathnames "anExp2/*.txt" *current-directory*)))
(defparameter *exp1-dm* (directory (merge-pathnames "dmExp1/*.txt" *current-directory*)))
(defparameter *exp2-dm* (directory (merge-pathnames "dmExp2/*.txt" *current-directory*)))
(defparameter *exp1-df* (directory (merge-pathnames "itempairsExp1/*.*" *current-directory*)))
(defparameter *exp2-df* (directory (merge-pathnames "itempairsExp2/*.*" *current-directory*)))
(defparameter *exp1-output* (concatenate 'string "outputExp1/TTBnew_" *version* "/"))
(defparameter *exp2-output* (concatenate 'string "outputExp2/TTBnew_" *version* "/"))
(defparameter *exp1-rt* (directory (merge-pathnames "rtExp1/*.txt" *current-directory*)))
(defparameter *exp2-rt* (directory (merge-pathnames "rtExp2/*.txt" *current-directory*)))
(defparameter *exp1-vpns* '(02 03 05 06 07 08 09 10 11 12 14 15 16 17 18 19 20))
(defparameter *exp2-vpns* '(01 02 03 04 05 06 07 08 09 10 11 12 13 15 16))


;; define other parameters
(defparameter *start-time* nil)
(defparameter *subject-start-time* nil)
(defparameter *all-start-time* nil)
(defparameter *end-time* nil)
(defparameter *subject-end-time* nil)
(defparameter *all-end-time* nil)
(defparameter *subject-RT* nil)
(defparameter *all-RT* nil)
(defparameter *subject-responses* nil)
(defparameter *all-responses* nil)
(defparameter *current-subject* nil)
(defparameter *all-subjects* nil)
(defparameter *current-pair-type* nil)
(defparameter *all-pair-types* nil)
(defparameter *current-left-item* nil)
(defparameter *all-left-items* nil)
(defparameter *current-right-item* nil)
(defparameter *all-right-items* nil)
(defparameter *bold-predictions* nil)
(defparameter *all-bold-predictions* nil)
(defparameter *model* (concatenate 'string "TTBnew_" *version*))
(defparameter experimental-window nil)
(defparameter *done* nil)

; a couple of useful small functions
(defun finish-trial () (equal *done* t))
(defun range (min max) (loop for n from min to max collect n))
(defun repeat (item times) (if (equal times 0) '() (cons item (repeat item (- times 1)))))



;; a function to load chunks into declarative memory
(defun populate-dm (file)
  (with-open-file (in file) 
    (progn (eval (read in)) t)
    )
)



;; define a function to present the two alternatives on the screen
(defun load-and-process-display (first-alternative second-alternative)
  ;(close-exp-window) ;close currently open virtual screen
  (setf experimental-window (open-exp-window "Present two alternatives" :visible nil :x 0 :y 0 :width 1280 :height 760)) ;define display
  (clear-exp-window) ;make-sure there is nothing displayed on the virtual screen
  (add-text-to-exp-window :text first-alternative  :x 181 :y 350) ;add name of first alternative on screen
  (add-text-to-exp-window :text second-alternative  :x 981 :y 350) ;add name of second alternative on screen
  (install-device experimental-window) ;set the virtual window as my current device
  (proc-display) ; make model process display
)

(defun present-and-process-cross ()
  (setf experimental-window (open-exp-window "Present two alternatives" :visible nil :x 0 :y 0 :width 1280 :height 760)) ;define display
  (clear-exp-window) ;make-sure there is nothign displayed on the virtual screen
  (add-text-to-exp-window :text "+" :x 640 :y 350) ;add cross to screen
  (install-device experimental-window) ;set the virtual screen as my current device
  (proc-display) ;process display
)

;; a function to run a single participant
(defun run-subject (vpn-index dm-file df an-file rt-file)
  (reset)
  (setf *subject-start-time* nil)
  (setf *subject-end-time* nil)
  (setf *subject-RT* nil)
  (setf *subject-responses* nil)
  (setf *current-subject* nil)
  (setf *current-pair-type* nil)
  (setf *bold-predictions* nil)
  (setf *current-left-item* nil)
  (setf *current-right-item* nil)
  (populate-dm (nth (- vpn-index 1) dm-file)) ;load memory files
  (with-open-file (in (nth (- vpn-index 1) an-file)) (eval (read in))) ;set activation noise
  (with-open-file (in (nth (- vpn-index 1) rt-file)) (eval (read in))) ;set retrieval threshold
  (with-open-file (stream (nth (- vpn-index 1) df)) ;load item pairs
    (do ((line (read stream) (read stream nil 'eof))) ;loop through item pairs
	((eq line 'eof) t)
      (let ((left-item (first line)) 
	    (right-item (second line))
	    (pair-type (third line))
	    (iti (fourth line)) )
	;perceive cross
	(present-and-process-cross)
	(goal-focus see-cross)
	(run-full-time 2)
	;do take-the-best
	(load-and-process-display left-item right-item)
	(setf *done* nil)
	(setf *start-time* (get-time))
	(push *start-time* *subject-start-time*)
	(goal-focus goal-start)
	(run-until-condition 'finish-trial)
	(run-full-time (/ iti 1000))
       	(push *end-time* *subject-end-time*)
	(push left-item *current-left-item*)
	(push right-item *current-right-item*)
	(push vpn-index *current-subject*)
	(push pair-type *current-pair-type*)
	) 
      )
    )
  (push (predict-bold-response) *bold-predictions*)
  )

; a function to write simulation output into a file
(defun write-to-file (vpn repetitions directory i)
; write response and RT data
  (with-open-file (out (ensure-directories-exist (merge-pathnames (concatenate 'string directory *model* "VPN" (write-to-string vpn) "Rep" (write-to-string repetitions) "_v7.csv") *current-directory*)) :direction :output :if-does-not-exist :create :if-exists :append)
    (format out "~&~{~a~t~}~%" (list 'Subject 'Left-Item 'Right-Item 'Pair-Type 'Start-Time 'End-Time 'RT 'responses))
    (dotimes (j (length (nth i *all-subjects*)))
	(format out "~&~{~a~t~}~%" (list (nth j (nth i *all-subjects*)) (nth j (nth i *all-left-items*)) (nth j (nth i *all-right-items*)) (nth j (nth i *all-pair-types*)) (nth j (nth i *all-start-time*)) (nth j (nth i *all-end-time*)) (nth j (nth i *all-RT*)) (nth j (nth i *all-responses*)) ))
	)
    )
  )

(defun write-bold-to-file (vpn repetitions directory i)
; write BOLD predicitons to file
  (with-open-file (out (ensure-directories-exist (merge-pathnames (concatenate 'string directory *model* "VPN" (write-to-string vpn) "Rep" (write-to-string repetitions) "_BOLD_v7.csv") *current-directory*)) :direction :output :if-does-not-exist :create :if-exists :append)
    (format out "~&~{~a~t~}~%" (list 'Subject 'Procedural 'Goal 'Visual 'Manual 'Imaginal 'Retrieval))
    (dotimes (j (- (length (first (first (nth i *all-bold-predictions*)))) 1))
      (format out "~&~{~a~t~}~%" (list (first (nth i *all-subjects*)) (nth (+ j 1) (first (first (nth i *all-bold-predictions*)))) (nth (+ j 1) (second (first (nth i *all-bold-predictions*)))) (nth (+ j 1) (third (first (nth i *all-bold-predictions*)))) (nth (+ j 1) (fourth (first (nth i *all-bold-predictions*)))) (nth (+ j 1) (fifth (first (nth i *all-bold-predictions*)))) (nth (+ j 1) (sixth (first (nth i *all-bold-predictions*)))))
	      )
      )
    )
)


; function to run multiple subjects, each multiple times
(defun run-subjects (vpn-indices vpns dm-file df an rt directory n)
  (setf *all-start-time* nil)
  (setf *all-end-time* nil)
  (setf *all-RT* nil)
  (setf *all-responses* nil)
  (setf *all-subjects* nil)
  (setf *all-pair-types* nil)
  (setf *all-left-items* nil)
  (setf *all-right-items* nil)
  (setf *all-bold-predictions* nil)
  (loop for i in vpn-indices do
	    (run-subject i dm-file df an rt)
	    (push (reverse *subject-RT*) *all-RT*)
	    (push (reverse *subject-start-time*) *all-start-time*)
	    (push (reverse *subject-end-time*) *all-end-time*)
	    (push (reverse *subject-responses*) *all-responses*)
	    (push (reverse *current-subject*) *all-subjects*)
	    (push (reverse *current-pair-type*) *all-pair-types*)
	    (push (reverse *current-right-item*) *all-right-items*)
	    (push (reverse *current-left-item*) *all-left-items*)
	    (push (reverse *bold-predictions*) *all-bold-predictions*)
	    )
  (setf *all-RT* (reverse *all-RT*))
  (setf *all-start-time* (reverse *all-start-time*))
  (setf *all-end-time* (reverse *all-end-time*))
  (setf *all-responses* (reverse *all-responses*))
  (setf *all-subjects* (reverse *all-subjects*))
  (setf *all-pair-types* (reverse *all-pair-types*))
  (setf *all-right-items* (reverse *all-right-items*))
  (setf *all-left-items* (reverse *all-left-items*))
  (setf *all-bold-predictions* (reverse *all-bold-predictions*))
  (dotimes (i (length *all-subjects*))
    (write-to-file (nth (- (nth i vpn-indices) 1) vpns) n directory i)
    (write-bold-to-file (nth (- (nth i vpn-indices) 1) vpns) n directory i) 
    )
)

(defun run-subjects-times (vpn-indices vpns dm-file df an rt directory repetitions)
  (loop for j from 1 to repetitions do
       (run-subjects vpn-indices vpns dm-file df an rt directory j)
   )
)

;;a function to capture response of model through the motor module
(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (setf *end-time* (get-time))
  (push (- *end-time* *start-time*) *subject-RT*)
  (cond ((equalp key #\q) (push 'q *subject-responses*))
        ((equalp key #\p) (push 'p *subject-responses*))
        (t (push 'pnq *subject-responses*)))
  (setf *done* t)
)


(defmethod compute-exec-time ((mtr-mod motor-module) (self punch))
 (setf (exec-time self) (max (init-time mtr-mod)
                             (randomize-time
                              (+ (init-time mtr-mod)
                                 (key-closure-time (current-device-interface)))))))


(defmethod compute-finish-time ((mtr-mod motor-module) (self punch))
 (max (exec-time self) (randomize-time (+ (init-time mtr-mod) (* 2 (burst-time mtr-mod))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; DEFINE MODEL ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-model take-the-best-cjpt
;; define parameters
  (sgp
   ;; general parameters
   :randomize-time t ;enable time randomization (t equals the default of 3), used mainly by perceptual and motor modules
;   :seed (948923123 1) ;set seed for random number generator
   :er t ;enables randomness (specifies whether modules operate deterministically or stochastically)
   :esc t ;enables subsymbolic computation
   :ol t ;enables optimized learning (can be set to nil or an integer as well)
   ;; device paramters
   :pixels-per-inch 72 ;default 72
   :show-focus t ;determine whether a red circle is shown at the position of focus of attention
   :viewing-distance 15 ;distance between model's eyes and display in inches; default 15
   ;; trace parameters
   :buffer-trace nil;t ;enable buffer tracing
;   :buffer-tracing-step 0.025; trace buffer every 25ms
   :traced-buffers (production goal visual manual imaginal retrieval) ;set which buffers to trace
   :save-buffer-trace t ;save buffer trace
   :save-p-history nil ;save production history
   :trace-detail high ;amount of detail shown in process trace
   :v nil;t;display model output; can be set to a file (e.g., "modeloutput.txt")
   ;; bold module parameters
   :bold-inc 1.5 ;period of BOLD scan in seconds
   :bold-param-mode act-r ;can be set to "act-r" and "spm"
   ; :bold-exp ;set exponent of gamma function
   ; :neg-bold-exp ;set negative exponent of gamma function
   ;; procedural module parameters
   :crt t ;show details about conflict resolution
   :use-tree nil ;creates a decision tree at beginning of experiment; usually makes model run faster
   :vpft nil ;randomized production firing time
   :alpha 0.2 ;alpha in utility learning equation; default is 0.2
   :egs 0 ;noise added to utility values; default is 0
   :iu 0 ;initial utility for user-defined productionsl default 0
   :nu 0 ;utility of newly learned productions; default 0
   :ul nil ;enable utility learning
   :ult nil ;trace utility learning
   ;; imaginal module parameters
   :imaginal-delay 0.2 ;set time required for imaginal module to operate; default 0.2
   :vidt nil ;randomize imaginal delay
   ;; declerative module parameters
   :ms 0 ;the maximum similarity between values which are chunk-slot-equal; defaults to 0
   :nsji nil ;allow or not negative values of Sji
   :md -1 ;the maximum difference between itmes which are not chunk-slot-equal; defaults to -1
   ;:ans nil ;add instantaneous activation noise; if set to number, noise is added
   :pas nil ;add permanent noise upon chunk creation
   :declarative-num-finsts 4 ;set number of declerative finsts
   :declarative-finst-span 2 ;set duration during which declerative finsts are remembered/present
   :bll 0.5 ;enable/disable base level learning; default 0.5
   :blc 0.0 ;set a base-level constant; default 0
   ; :chunk-add-hook - specify a function to be called when a chunk is added to DM
   ; :retrieval-request-hook - specify a function to be called when a retrieval request is made
   ; :retrieval-chunk-hook - specify a function to be called when a chunk is retrieved
   ; :retrieval-set-hook - specify a function to be called during the retrieval process
   :le 1 ;latency exponent (f) in equation for retrieval times; defaults to 1
   :lf 1 ;latency factor (F) in equation for retrieval times; defaults to 1
   :mas nil ;enable/disable spreading activation; provies S value in spreading activation equation
;   :rt -1.5 ;set retrieval threshold; defaults to 0
   :sact nil ;save details of activation computation
   :act t ;print details of a chunk's activation computation
   ;; visual module parameters
   :optimize-visual t ;if t, then process each word into one feature
   :visual-finst-span 2 ;duration of visual finsts in seconds
   :visual-num-finsts 4;number of items which the visual module can trace
   ;; timing module parameters
   ; :record-ticks t ;traces ticks
   ; :time-master-start-increment 0.011 ;initial value for timing module; default 0.011
   ; :time-mult 1.1 ;multiplicative factor in timing equation; default 1.1
   ; :time-noise 0.015 ;noise parameter in timing equation; default 0.015
   )

;; Define hand locations on keyboard
(set-hand-location left 1 3)
(set-hand-location right 10 3) 

;; Define chunks and place them in declarative memory
  (add-dm
   (see-cross state look-at-cross)
   (goal-start state start-TTB)
   )

;; Define production for looking at cross
(p look-at-cross
=goal>
	state look-at-cross
==>
+visual-location>
	:attended nil
=goal>
	state see-cross
)

(p see-cross
=goal>
	state see-cross
=visual-location>
?visual>
	state free
==>
+visual>
	cmd move-attention
	screen-pos =visual-location
-goal>
-visual>
)
;; Define productions for take-the-best

(p start-names-comparison
=goal>
	state start-TTB
==>
+visual-location>
	screen-x lowest
=goal>
	state see-item1
)

(p see-item1
=goal>
	state see-item1
=visual-location>
?visual>
	state free
 ==>
+visual>
	cmd move-attention
	screen-pos =visual-location
=goal>
	state store-item1   
)

(p store-item1
=goal>
	state store-item1
=visual>
	value       =item
==>
+imaginal>
	name1 =item
=goal>
	state see-item2
+visual-location>
	screen-x highest
)

(p see-item2
=goal>
     state see-item2
=visual-location>
==>
+visual>
    cmd move-attention
    screen-pos =visual-location
=goal>
    state compare-names
)

(p names-same
=goal>
    state compare-names
=imaginal>
    name1 =item
=visual>
    value =item
?manual>
      state free
==>
=goal>
	state companies-same
+manual>
	cmd         punch
	hand        right 
	finger      thumb
)

(p names-different
=goal>
	state compare-names
=imaginal>
	name1 =item1
=visual>
	- value =item1
	value =item2
==>
=goal>
    state retrieve-item1
+imaginal>
	cue-name cue1
	left-item =item1
	right-item =item2
=goal>
	state item1-start
)

(p select-cue2
=goal>
	state select-cue
=imaginal>
	cue-name cue1
	left-item =li
	right-item =ri
==>
=goal>
	state item1-start
+imaginal>
	cue-name cue2
	left-item =li
	right-item =ri
)

(p select-cue3
=goal>
	state select-cue
=imaginal>
	cue-name cue2
	left-item =li
	right-item =ri
==>
=goal>
	state item1-start
+imaginal>
	cue-name cue3
	left-item =li
	right-item =ri
)

(p select-cue4
=goal>
	state select-cue
=imaginal>
	cue-name cue3
	left-item =li
	right-item =ri
==>
=goal>
	state item1-start
+imaginal>
	cue-name cue4
	left-item =li
	right-item =ri
)

(p item1-start
=goal>
	state item1-start
=imaginal>
	cue-name =c
	left-item =li
==>
+retrieval>
	item-name =li
	cue-name =c
=goal>
	state retrieve-cue-item1
=imaginal>
)

(p store-cue-item1
=goal>
    state retrieve-cue-item1
=retrieval>
    cue-name =c
    cue-value =v
=imaginal>
	cue-name =c
	left-item =li
	right-item =ri
==>
=goal>
    state item2-start
+imaginal>
	cue-name =c
	left-item =li
	right-item =ri
	left-cue-value =v
-retrieval>
)

(p cue-retrieval-failure-item1
=goal>
    state retrieve-cue-item1
?retrieval>
    state error
=imaginal>
	cue-name =c
	left-item =li
	right-item =ri
==>
=goal>
    state item2-start
+imaginal>
	cue-name =c
	left-item =li
	right-item =ri
	left-cue-value 0
)

(p item2-start
=goal>
	state item2-start
=imaginal>
	cue-name =c
	right-item =ri
==>
+retrieval>
	item-name =ri
	cue-name =c
=goal>
	state company-compare
=imaginal>
)


(p company1-bigger1
=goal>
	state company-compare
=imaginal>
	cue-name =cue
	left-cue-value 1
=retrieval>
	cue-value 0
?manual>
	state free

==>
=goal>
	state company1-bigger
+manual>
    ISA         punch
    hand        left 
    finger      index
)

(p company1-bigger2
=goal>
	state company-compare
=imaginal>
	cue-name =cue
	left-cue-value 1
?retrieval>
	state error
?manual>
    state free

==>
=goal>
	state company1-bigger
+manual>
    ISA         punch
    hand        left 
    finger      index
)

(p company2-bigger
=goal>
	state company-compare
=imaginal>
	cue-name =cue
	left-cue-value 0
=retrieval>
	cue-value 1
?manual>
    state free
==>
=goal>
	state company2-bigger
+manual>
    cmd         punch
    hand        right 
    finger      index
)

(p next-cue-1
=goal>
    state company-compare
=imaginal>
    cue-name =cue
    - cue-name cue4
    left-cue-value =v
=retrieval>
    cue-value =v
==>
=goal>
    state select-cue
=imaginal>
)

(p next-cue-2
=goal>
    state company-compare
=imaginal>
    cue-name =cue
    - cue-name cue4
    left-cue-value 0
?retrieval>
    state error
==>
=goal>
    state select-cue
=imaginal>
)


(p guess-1
=goal>
    state company-compare
=imaginal>
    cue-name cue4
    left-cue-value =v
=retrieval>
    cue-value =v
?manual>
    state free
==>
=goal>
    state companies-same
+manual>
    cmd         punch
    hand        right 
    finger      thumb
)

(p guess-2
=goal>
    state company-compare
=imaginal>
    cue-name cue4
    left-cue-value 0
?retrieval>
    state error
?manual>
    state free
==>
=goal>
    state companies-same
+manual>
    cmd         punch
    hand        right 
    finger      thumb
)

)
