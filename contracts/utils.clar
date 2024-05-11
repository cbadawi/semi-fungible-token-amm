(define-constant ERR-PERCENTAGE-INVALID (err u2015))

;; ----------------------------------------------------------------------------------------
;; -------------------------------------------UTILS----------------------------------------
;; ----------------------------------------------------------------------------------------	
(define-public (check-err-reducer (result (response uint uint)) (prior (response uint uint)))
	(ok (match prior 
        ok-value result
        err-value (err err-value)
)))

(define-public (check-err-collateral-debt-reducer (result (response (tuple (collateral-value uint) (debt-value uint) (average-liquidation-threshold-numerator uint) (average-ltv-numerator uint)) uint)) (prior (response (tuple (collateral-value uint) (debt-value uint) (average-liquidation-threshold-numerator uint) (average-ltv-numerator uint)) uint)))
	(ok (match prior 
        ok-value result
        err-value (err err-value)
)))

;; todo this is a real problem but non-blocker. find something better. read this convo https://discord.com/channels/621759717756370964/623217767356694547/1225719071555715072
(define-read-only (get-current-time)
	(unwrap-panic (get-block-info? time (- block-height u1))
))
;; ----------------------------------------------------------------------------------------
;; MATH 
;; ----------------------------------------------------------------------------------------
;; TODO all arithmetic operations need to be revised during testing. create your own RAY library if needed.
;; https://medium.com/dapphub/introducing-ds-math-an-innovative-safe-math-library-d58bc88313da
;; TODO2 CHECK FOR OVERFLOWS SIMILAR TO THE BIGINT LIBRARY
(define-constant SECONDS-PER-YEAR u31536000)
(define-constant DECIMALS u18)
(define-constant UNIT (pow u10 DECIMALS))
;; all percebtages should be expressed as this unit
(define-constant PERCENTAGE_FACTOR u10000) ;; 100% is 10000 

;; TODO VIP make sure you're only using mul-unit and mul-unit when ray multiplactions are needed
;; TODO2 do i need to create similar operations for ray div? the math doesnt need it as 1.1/2.2 = 110000/220000 but it cant be said for multiplications
(define-public (div-unit-down (a uint) (b uint)) 
	(ok	(/ (* a UNIT) b) 
))

(define-public (div-unit-up (a uint) (b uint)) 
	(ok	(ceil-div (* a UNIT) b) 
))

(define-public (mul-unit (a uint) (b uint)) 
	(ok (/ (* a b) UNIT)
))

(define-public (ceil-div (a uint) (b uint))
	(ok (let ((quotient (/ a b)))
    (if (is-eq (mod a b) u0)
        quotient
        (+ quotient u1)
))))

;; @param a the number you want to get a percentage of
;; @param perc the percentage expressed in bps. ex 10.01% is u1001
(define-public (percent-mul (a uint) (perc uint)) 
	(ok	(/ (* a perc) PERCENTAGE_FACTOR)
))

(define-public (percent-div (a uint) (perc uint)) 
	(begin (asserts! (>= perc u0) ERR-PERCENTAGE-INVALID)
				(ok (* (/ a perc) PERCENTAGE_FACTOR))
))

(define-public (get-percentage-a-over-b (a uint) (b uint)) 
	(ok (/ (* a PERCENTAGE_FACTOR) b)
))