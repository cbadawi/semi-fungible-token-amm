(impl-trait .trait-ownable.ownable-trait)
(impl-trait .trait-m-token.m-token-trait)

(use-trait ft-trait .trait-sip-010.sip-010-trait)


;; todo compare the textx between this wusd c token contract and the sbtc c token contract. they need to be consistent.


;; errors
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-INVALID-AMOUNT (err u1001))
(define-constant ERR-INVALID-LIQUIDITY-INDEX (err u1002))
(define-constant ERR-NOT-AUTHORIZED-TOKEN (err u1003))
(define-constant ERR-PERCENTAGE-INVALID (err u1004))
(define-constant ERR-NON-TRANSFERABLE-TOKEN (err u1005))
(define-constant ERR-NO-TREASURY-SET (err u1006))
(define-constant ERR-USER-NOT-FOUND (err u1007))

;; non-transferable token
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  ERR-NON-TRANSFERABLE-TOKEN
)
(define-public (transfer-underlying-token (amount uint) (recipient principal) (asset-trait <ft-trait>)) 
  ERR-NON-TRANSFERABLE-TOKEN
)
(define-read-only (get-treasury)
  ERR-NO-TREASURY-SET
)
(define-public (liquidate-m-token  (amount uint) (sender principal) (recipient principal)) 
  ERR-NON-TRANSFERABLE-TOKEN
)


(define-fungible-token variable-wusd-debt-token)

(define-data-var token-uri (string-utf8 256) u"")
(define-data-var contract-owner principal tx-sender)
(define-map approved-contracts principal bool)

(define-read-only (get-contract-owner)
  (ok (var-get contract-owner))
)

(define-public (set-contract-owner (owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (var-set contract-owner owner))
  )
)

(define-private (check-is-approved (sender principal))
  (ok (asserts! (or (default-to false (map-get? approved-contracts sender)) (is-eq sender (var-get contract-owner))) ERR-NOT-AUTHORIZED))
)

(define-public (add-approved-contract (new-approved-contract principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (map-set approved-contracts new-approved-contract true)
    (ok true)
  )
)

(define-public (set-approved-contract (owner principal) (approved bool))
	(begin
		(asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
		(ok (map-set approved-contracts owner approved))
	)
)

(set-approved-contract (as-contract tx-sender) true)

;; ---------------------------------------------------------
;; Scaled Token Functions - m-token-trait
;; ---------------------------------------------------------
;; aTokens are 1:1 to the amount deposited. Depositing 10 DAI returns you 10 aDAI. So, how does interest accrue?
;; by integrating the index into the sip 10 functions, the balances grows without any incoming transaction
;; say when a user borrows and variable debt tokens are minted, amount-minted = amount-scaled * liquidity index
;; amount-scaled < amount-minted and is the normalized number without the liquidity index.
;; now, after interest accrued, the balance of the user would reflect the increase in index relative to the time of deposit: amount-scaled * index
(define-map user-state { user: principal } { last-index: uint })

(define-data-var normalized-income uint UNIT)

(define-read-only (get-normalized-income) (var-get normalized-income))

(define-public (set-normalized-income (index uint)) 
  (begin (try! (check-is-approved tx-sender))
        (ok (var-set normalized-income index))
))

(define-public (burn-scaled (amount uint) (who principal) (liquidity-index uint)) 
  (begin
    (try! (check-is-approved tx-sender))
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> liquidity-index u0) ERR-INVALID-LIQUIDITY-INDEX)
    (let ((amount-scaled (div-unit-down amount liquidity-index)))
      (map-set user-state {user: who} {last-index: liquidity-index})
      ;; TODO aave prints some events here
      (print {FUNCTION:"burn-scaled", amount-scaled:amount-scaled, amount:amount, liquidity-index:liquidity-index})
      (try! (ft-burn? variable-wusd-debt-token amount-scaled who))
      (ok true)
)))

(define-public (mint-scaled (amount uint) (who principal) (liquidity-index uint))
  (begin
    (try! (check-is-approved tx-sender))
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> liquidity-index u0) ERR-INVALID-LIQUIDITY-INDEX)
    (let ((amount-scaled (div-unit-down amount liquidity-index)))
      (map-set user-state {user: who} {last-index: liquidity-index})
      (print {Function: "variable-debt-token-mint-scaled", amount-scaled: amount-scaled, liquidity-index:liquidity-index, amount:amount})
      (try! (ft-mint? variable-wusd-debt-token amount-scaled who))
      (ok true)
)))

(define-public (get-scaled-balance (who principal))
  (ok (ft-get-balance variable-wusd-debt-token who))
)

(define-read-only (get-scaled-total-supply)
  (ok (ft-get-supply variable-wusd-debt-token))
)

;; ---------------------------------------------------------
;; SIP-10 Functions
;; ---------------------------------------------------------
;; @desc get-total-supply
;; @returns (response uint)
;; @desc get-total-supply
;; @returns (response uint)
(define-read-only (get-total-supply)
  (ok (mul-unit (ft-get-supply variable-wusd-debt-token) (var-get normalized-income)))
)

;; @desc get-balance
;; @params token-id
;; @params who
;; @returns (response uint)
(define-read-only (get-balance (account principal))
  (ok (mul-unit (ft-get-balance variable-wusd-debt-token account) (var-get normalized-income)))
)

;; @desc get-name
;; @returns (response string-utf8)
(define-read-only (get-name)
  (ok "variable-wusd-debt-token")
)

;; @desc get-symbol
;; @returns (response string-utf8)
(define-read-only (get-symbol)
  (ok "variable-wusd-debt-token")
)

;; @desc get-decimals
;; @returns (response uint)
(define-read-only (get-decimals)
   	(ok u6) ;;NOTE V2 is expected to have 6 decimals
)

(define-public (set-token-uri (value (string-utf8 256)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (var-set token-uri value))
  )
)


(define-read-only (get-token-uri)
  (ok (some (var-get token-uri)))
)

(define-public (mint (amount uint) (recipient principal))
  (begin
    (try! (check-is-approved tx-sender))
    (ft-mint? variable-wusd-debt-token amount recipient)
  )
)

(define-public (burn (amount uint) (sender principal))
  (begin
    (try! (check-is-approved tx-sender))
    (ft-burn? variable-wusd-debt-token amount sender)
  )
)

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
(define-private (div-unit-down (a uint) (b uint)) 
	(/ (* a UNIT) b) 
)

(define-private (div-unit-up (a uint) (b uint)) 
	(ceil-div (* a UNIT) b) 
)

(define-private (mul-unit (a uint) (b uint)) 
    (/ (* a b) UNIT)
)

(define-private (ceil-div (a uint) (b uint))
  (let ((quotient (/ a b)))
    (if (is-eq (mod a b) u0)
        quotient
        (+ quotient u1)
)))

;; @param a the number you want to get a percentage of
;; @param perc the percentage expressed in bps. ex 10.01% is u1001
(define-private (percent-mul (a uint) (perc uint)) 
	(/ (* a perc) PERCENTAGE_FACTOR)
)

(define-private (percent-div (a uint) (perc uint)) 
(begin (asserts! (>= perc u0) ERR-PERCENTAGE-INVALID)
				(ok (* (/ a perc) PERCENTAGE_FACTOR))
))

(define-private (get-percentage-a-over-b (a uint) (b uint)) 
(/ (* a PERCENTAGE_FACTOR) b)
)