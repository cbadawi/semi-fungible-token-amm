(impl-trait .trait-ownable.ownable-trait)
(impl-trait .trait-sip-010.sip-010-trait)
(impl-trait .trait-m-token.m-token-trait)

(use-trait ft-trait .trait-sip-010.sip-010-trait)

(define-fungible-token sbtc-m-token)

(define-constant UNDERLYING_ASSET 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.sbtc)

(define-data-var token-uri (string-utf8 256) u"")
(define-data-var contract-owner principal tx-sender)
(define-data-var treasury-address principal tx-sender)
(define-map approved-contracts principal bool)

;; errors
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-INVALID-AMOUNT (err u1001))
(define-constant ERR-INVALID-LIQUIDITY-INDEX (err u1002))
(define-constant ERR-NOT-AUTHORIZED-TOKEN (err u1003))
(define-constant ERR-PERCENTAGE-INVALID (err u1004))
(define-constant ERR-NON-TRANSFERABLE-TOKEN (err u1005))
(define-constant ERR-NO-TREASURY-SET (err u1006))
(define-constant ERR-USER-NOT-FOUND (err u1007))

(define-read-only (get-contract-owner)
  (ok (var-get contract-owner))
)

(define-public (set-contract-owner (owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (var-set contract-owner owner))
  )
)

(define-read-only (get-treasury)
  (ok (var-get treasury-address))
)

(define-public (set-treasury (address principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (var-set treasury-address address))
  )
)


;; @desc check-is-approved
;; @restricted Contract-Owner
;; @params sender
;; @returns (response bool)
(define-private (check-is-approved (sender principal))
  (ok (asserts! (or (default-to false (map-get? approved-contracts sender)) (is-eq sender (var-get contract-owner))) ERR-NOT-AUTHORIZED))
)

(define-public (add-approved-contract (new-approved-contract principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (map-set approved-contracts new-approved-contract true))
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
      (try! (ft-burn? sbtc-m-token amount-scaled who))
      (ok true)
)))

(define-public (mint-scaled (amount uint) (who principal) (liquidity-index uint))
  (begin
    (try! (check-is-approved tx-sender))
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> liquidity-index u0) ERR-INVALID-LIQUIDITY-INDEX)
    (let ((amount-scaled (div-unit-down amount liquidity-index)))
      (map-set user-state {user: who} {last-index: liquidity-index})
      (print {Function: "sbtc-m-token-mint-scaled", amount-scaled: amount-scaled, liquidity-index:liquidity-index, amount:amount})
      (try! (ft-mint? sbtc-m-token amount-scaled who))
      (ok true)
)))

(define-public (get-scaled-balance (account principal))
  (ok (ft-get-balance sbtc-m-token account))
)

(define-read-only (get-scaled-total-supply)
  (ok (ft-get-supply sbtc-m-token))
)

;; ---------------------------------------------------------
;; SIP-10 Functions
;; ---------------------------------------------------------

;; @desc get-total-supply
;; @returns (response uint)
(define-read-only (get-total-supply)
  (ok (mul-unit (ft-get-supply sbtc-m-token) (var-get normalized-income)))
)

;; @desc get-balance
;; @params token-id
;; @params who
;; @returns (response uint)
(define-read-only (get-balance (account principal))
  (ok (mul-unit (ft-get-balance sbtc-m-token account) (var-get normalized-income)))
)

;; @desc get-name
;; @returns (response string-utf8)
(define-read-only (get-name)
  (ok "sbtc-m-token")
)

;; @desc get-symbol
;; @returns (response string-utf8)
(define-read-only (get-symbol)
  (ok "sbtc-m-token")
)

;; @desc get-decimals
;; @returns (response uint)
(define-read-only (get-decimals)
   	(ok u6) ;;NOTE V2 is expected to have 6 decimals
)

;; @desc set-token-uri
;; @restricted Contract-Owner
;; @params value
;; @returns (response bool)
(define-public (set-token-uri (value (string-utf8 256)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (var-set token-uri value))
  )
)

;; @desc get-token-uri 
;; @params token-id
;; @returns (response none)
(define-read-only (get-token-uri)
  (ok (some (var-get token-uri)))
)

;; @desc transfer
;; @restricted sender
;; @params token-id 
;; @params amount
;; @params sender
;; @params recipient
;; @returns (response boolean)
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq sender tx-sender) ERR-NOT-AUTHORIZED)
    (match (ft-transfer? sbtc-m-token amount sender recipient)
      response (begin
        (print memo)
        (ok response)
      )
      error (err error)
    )
  )
)

(define-public (transfer-underlying-token (amount uint) (recipient principal) (asset-trait <ft-trait>)) 
(begin  
    (try! (check-is-approved tx-sender))
		(asserts! (is-eq UNDERLYING_ASSET (contract-of asset-trait)) ERR-NOT-AUTHORIZED-TOKEN)
    (ok (as-contract (try! (contract-call? asset-trait transfer amount tx-sender recipient none))))
))

(define-public (liquidate-m-token (amount uint) (liquidated-user principal) (recipient principal)) 
  (begin  
    (try! (check-is-approved tx-sender))
    (ft-transfer? sbtc-m-token amount liquidated-user recipient)
))

;; @dev kept for consistency with sip-10. use mint-scaled instead
(define-public (mint (amount uint) (recipient principal))
  (begin
    (try! (check-is-approved tx-sender))
    ;; (ft-mint? sbtc-m-token amount recipient)
    (ok false)
  )
)

;; @desc burn
;; @restricted ContractOwner/Approved Contract
;; @params token-id
;; @params amount
;; @params sender
;; @returns (response boolean)
(define-public (burn (amount uint) (sender principal))
  (begin
    (try! (check-is-approved tx-sender))
    (ft-burn? sbtc-m-token amount sender)
  )
)
;; ----------------------------------------------------------------------------------------
;; MATH 
;; ----------------------------------------------------------------------------------------
;; https://medium.com/dapphub/introducing-ds-math-an-innovative-safe-math-library-d58bc88313da
(define-constant SECONDS-PER-YEAR u31536000)
(define-constant DECIMALS u18)
(define-constant UNIT (pow u10 DECIMALS))
;; all percebtages should be expressed as this unit
(define-constant PERCENTAGE_FACTOR u10000) ;; 100% is 10000 

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