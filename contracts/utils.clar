(define-constant ERR-PERCENTAGE-INVALID (err u2015))

;; ----------------------------------------------------------------------------------------
;; -------------------------------------------UTILS----------------------------------------
;; ----------------------------------------------------------------------------------------	
(define-public (check-err-reducer (result (response uint uint)) (prior (response uint uint)))
	(ok (match prior 
        ok-value result
        err-value (err err-value)
)))

(define-read-only (check-err-collateral-debt-reducer (result (response (tuple (collateral-value uint) (debt-value uint) (average-liquidation-threshold-numerator uint) (average-ltv-numerator uint)) uint)) (prior (response (tuple (collateral-value uint) (debt-value uint) (average-liquidation-threshold-numerator uint) (average-ltv-numerator uint)) uint)))
	(match prior 
        ok-value result
        err-value (err err-value)
))

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
(define-read-only (div-unit-down (a uint) (b uint)) 
	(/ (* a UNIT) b) 
)

(define-read-only (div-unit-up (a uint) (b uint)) 
	(ceil-div (* a UNIT) b) 
)

(define-read-only (mul-unit (a uint) (b uint)) 
	(/ (* a b) UNIT)
)

(define-constant one-8 u100000000)
(define-constant fixed-precision u8)
(define-read-only (mul-to-fixed-precision (a uint) (decimals-a uint) (b-fixed uint))
  (if (> decimals-a fixed-precision)
    (mul (/ a (pow u10 (- decimals-a fixed-precision))) b-fixed)
    (mul (* a (pow u10 (- fixed-precision decimals-a))) b-fixed)
  )
)
(define-read-only (mul (x uint) (y uint))
  (/ (+ (* x y) (/ one-8 u2)) one-8))


(define-read-only (ceil-div (a uint) (b uint))
	(let ((quotient (/ a b)))
    (if (is-eq (mod a b) u0)
        quotient
        (+ quotient u1)
)))

;; @param a the number you want to get a percentage of
;; @param perc the percentage expressed in bps. ex 10.01% is u1001
(define-read-only (percent-mul (a uint) (perc uint)) 
	(/ (* a perc) PERCENTAGE_FACTOR)
)

(define-read-only (percent-div (a uint) (perc uint)) 
	(begin (asserts! (>= perc u0) ERR-PERCENTAGE-INVALID)
				(ok (* (/ a perc) PERCENTAGE_FACTOR))
))

(define-read-only (get-percentage-a-over-b (a uint) (b uint)) 
	(/ (* a PERCENTAGE_FACTOR) b)
)