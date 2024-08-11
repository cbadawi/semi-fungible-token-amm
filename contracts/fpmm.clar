(use-trait ft .sip-010-trait.sip-010-trait)
(use-trait sft .sip013-semi-fungible-token-trait.sip013-semi-fungible-token-trait)
;; token definitions
;; There will be no token definitions in the fpmm.
;; LP tokens are created independently and passed and validated in the appropriate functions such as add-to-position

;; constants
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-POOL-ALREADY-EXISTS (err u2000))
(define-constant ERR-INVALID-POOL (err u2001))
(define-constant ERR-BLOCKLISTED (err u2002))
(define-constant ERR-INVALID-LIQUIDITY (err u2003))
(define-constant ERR-PERCENT-GREATER-THAN-ONE (err u2004))
(define-constant ERR-EXCEEDS-MAX-SLIPPAGE (err u2005))
(define-constant ERR-ORACLE-NOT-ENABLED (err u2006))
(define-constant ERR-ORACLE-AVERAGE-BIGGER-THAN-ONE (err u2007))
(define-constant ERR-PAUSED (err u2008))
(define-constant ERR-SWITCH-THRESHOLD-BIGGER-THAN-ONE (err u2009))
(define-constant ERR-NO-LIQUIDITY (err u2010))
(define-constant ERR-MAX-IN-RATIO (err u2011))
(define-constant ERR-MAX-OUT-RATIO (err u2012))

;; data 
(define-data-var paused bool false)

;; getters
(define-read-only (get-pool-details-by-id (pool-id uint))
    (contract-call? .amm-registry get-pool-details-by-id pool-id))
		
(define-read-only (get-pool-details (token-x principal) (token-y principal) (token-y-id uint))
    (contract-call? .amm-registry get-pool-details token-x token-y token-y-id))

(define-read-only (is-paused)
	(var-get paused))

(define-read-only (get-token-given-position (token-x principal) (token-y principal) (token-y-id uint) (dx uint) (max-dy (optional uint)))
	(let (
			(pool (try! (get-pool-details token-x token-y token-y-id)))
			(dy (default-to u340282366920938463463374607431768211455 max-dy))
		)
		(asserts! (and (> dx u0) (> dy u0))  ERR-NO-LIQUIDITY)
		(ok (get-token-given-position-internal (get balance-x pool) (get balance-y pool) (get total-supply pool) dx dy))))

(define-read-only (get-invariant (balance-x uint) (balance-y uint))
        (mul-down balance-x balance-y)
)
(define-read-only (get-position-given-burn (token-x principal) (token-y principal) (factor uint) (token-amount uint))
	(let (
			(pool (try! (get-pool-details token-x token-y factor)))
		)
		(asserts! (> (get total-supply pool) u0) ERR-NO-LIQUIDITY)
		(ok (get-position-given-burn-internal (get balance-x pool) (get balance-y pool) (get total-supply pool) token-amount))))

(define-read-only (check-pool-status (token-x principal) (token-y principal) (token-y-id uint))
	(let (
			(pool (try! (get-pool-details token-x token-y token-y-id)))
		)
		(ok (asserts! (and (>= block-height (get start-block pool)) (<= block-height (get end-block pool))) ERR-NOT-AUTHORIZED))))

(define-read-only (get-y-given-x (token-x principal) (token-y principal) (factor uint) (dx uint))
	(let (
			(pool (try! (get-pool-details token-x token-y factor)))
			(threshold (get threshold-x pool))
			(dy (if (>= dx threshold)
				(get-y-given-x-internal (get balance-x pool) (get balance-y pool) dx)
				(div-down (mul-down dx (get-y-given-x-internal (get balance-x pool) (get balance-y pool) threshold)) threshold)))
		)
		(asserts! (< dx (mul-down (get balance-x pool) (get max-in-ratio pool))) ERR-MAX-IN-RATIO)
		(asserts! (< dy (mul-down (get balance-y pool) (get max-out-ratio pool))) ERR-MAX-OUT-RATIO)
		(ok dy)))
        
(define-read-only (get-x-given-y (token-x principal) (token-y principal) (token-y-id uint) (dy uint))
	(let (
			(pool (try! (get-pool-details token-x token-y token-y-id)))
			(threshold (get threshold-y pool))
			(dx (if (>= dy threshold)
					(get-x-given-y-internal (get balance-x pool) (get balance-y pool) dy)
					(div-down (mul-down dy (get-x-given-y-internal (get balance-x pool) (get balance-y pool) threshold)) threshold)))
		)
		(asserts! (< dy (mul-down (get balance-y pool) (get max-in-ratio pool))) ERR-MAX-IN-RATIO)
		(asserts! (< dx (mul-down (get balance-x pool) (get max-out-ratio pool))) ERR-MAX-OUT-RATIO)
		(ok dx)))

(define-read-only (get-price (token-x principal) (token-y principal) (token-y-id uint))
	(let (
			(pool (try! (get-pool-details token-x token-y token-y-id)))
		)
		(ok (get-price-internal (get balance-x pool) (get balance-y pool)))))

;; public functions: add-to-position, reduce-position, swap
(define-public (add-to-position-10-13 (token-x-trait <ft>) (token-y-trait <sft>) (token-y-id uint) (dx uint) (max-dy (optional uint)))
    (let (
            (token-x (contract-of token-x-trait))
            (token-y (contract-of token-y-trait))
            (pool (try! (get-pool-details token-x token-y token-y-id)))
            (balance-x (get balance-x pool))
            (balance-y (get balance-y pool))
            (total-supply (get total-supply pool))
            (add-data (try! (get-token-given-position token-x token-y token-y-id dx max-dy)))
            (new-supply (get token add-data))
            (dy (get dy add-data))
            (pool-updated (merge pool { total-supply: (+ new-supply total-supply), balance-x: (+ balance-x dx), balance-y: (+ balance-y dy) }))
            (sender tx-sender))
        (asserts! (not (is-paused)) ERR-PAUSED)
        (asserts! (and (> dx u0) (> dy u0)) ERR-INVALID-LIQUIDITY)
        (asserts! (>= (default-to u340282366920938463463374607431768211455 max-dy) dy) ERR-EXCEEDS-MAX-SLIPPAGE)
        (try! (contract-call? token-x-trait transfer dx sender .amm-vault none))
        (try! (contract-call? token-y-trait transfer token-y-id dy sender .amm-vault))
        (as-contract (try! (contract-call? .amm-registry update-pool token-x token-y token-y-id pool-updated)))
        (as-contract (try! (contract-call? .token-amm-pool mint (get pool-id pool) new-supply sender)))
        (print { object: "pool", action: "liquidity-added", data: pool-updated, dx: dx, dy: dy, token-x: token-x, token-y: token-y, sender: sender, new-pool: pool-updated })
        (ok {supply: new-supply, dx: dx, dy: dy})))

(define-public (reduce-position-10-13 (token-x-trait <ft>) (token-y-trait <sft>) (token-y-id uint) (percent uint))
    (let (
            (token-x (contract-of token-x-trait))
            (token-y (contract-of token-y-trait))
            (pool (try! (get-pool-details token-x token-y token-y-id)))
            (balance-x (get balance-x pool))
            (balance-y (get balance-y pool))
            (total-shares (unwrap-panic (contract-call? .token-amm-pool get-balance-fixed (get pool-id pool) tx-sender)))
            (shares (if (is-eq percent ONE_8) total-shares (mul-down total-shares percent)))
            (total-supply (get total-supply pool))
            (reduce-data (try! (get-position-given-burn token-x token-y token-y-id shares)))
            (dx (get dx reduce-data))
            (dy (get dy reduce-data))
            (pool-updated (merge pool { total-supply: (if (<= total-supply shares) u0 (- total-supply shares)), balance-x: (if (<= balance-x dx) u0 (- balance-x dx)), balance-y: (if (<= balance-y dy) u0 (- balance-y dy)) }))
            (sender tx-sender))  
		;; (asserts! (not (is-blocklisted-or-default tx-sender)) ERR-BLOCKLISTED)		
        (asserts! (not (is-paused)) ERR-PAUSED)       
        (asserts! (<= percent ONE_8) ERR-PERCENT-GREATER-THAN-ONE)
        (as-contract (try! (contract-call? .amm-vault transfer-ft token-x-trait dx sender)))
        (as-contract (try! (contract-call? .amm-vault transfer-sft token-y-trait token-y-id dy sender)))
        (as-contract (try! (contract-call? .amm-registry update-pool token-x token-y token-y-id pool-updated)))
        (as-contract (try! (contract-call? .token-amm-pool burn-fixed (get pool-id pool) shares sender)))
        (print { object: "pool", action: "liquidity-removed", data: pool-updated, dx: dx, dy: dy, token-x: token-x, token-y: token-y, sender: sender })
        (ok {dx: dx, dy: dy})))

(define-public (swap-x-for-y (token-x-trait <ft>) (token-y-trait <sft>) (token-y-id uint) (dx uint) (min-dy (optional uint)))
    (let (
            (token-x (contract-of token-x-trait))
            (token-y (contract-of token-y-trait))
            (pool (try! (get-pool-details token-x token-y token-y-id)))
            (balance-x (get balance-x pool))
            (balance-y (get balance-y pool))
            (fee (mul-up dx (get fee-rate-x pool)))
            (dx-net-fees (if (<= dx fee) u0 (- dx fee)))
            (fee-rebate (mul-down fee (get fee-rebate pool)))
            (dy (try! (get-y-given-x token-x token-y token-y-id dx-net-fees)))                
            (pool-updated (merge pool {
                balance-x: (+ balance-x dx-net-fees fee-rebate),
                balance-y: (if (<= balance-y dy) u0 (- balance-y dy)),
                ;; oracle-resilient: (if (get oracle-enabled pool) (try! (get-oracle-resilient token-x token-y factor)) u0)
                }))
            (sender tx-sender))
				;; (asserts! (not (is-blocklisted-or-default tx-sender)) ERR-BLOCKLISTED)
        (asserts! (not (is-paused)) ERR-PAUSED)
        (try! (check-pool-status token-x token-y token-y-id))
        (asserts! (> dx u0) ERR-INVALID-LIQUIDITY)
        (asserts! (<= (div-down dy dx-net-fees) (get-price-internal balance-x balance-y)) ERR-INVALID-LIQUIDITY)
        (asserts! (<= (default-to u0 min-dy) dy) ERR-EXCEEDS-MAX-SLIPPAGE)
        (try! (contract-call? token-x-trait transfer dx sender .amm-vault none))
        (and (> dy u0) (as-contract (try! (contract-call? .amm-vault transfer-sft token-y-trait token-y-id dy sender))))
        (as-contract (try! (contract-call? .amm-vault add-to-reserve token-x (- fee fee-rebate))))
        (as-contract (try! (contract-call? .amm-registry update-pool token-x token-y token-y-id pool-updated)))
        (print { object: "pool", action: "swap-x-for-y", data: pool-updated, dx: dx, dy: dy, token-x: token-x, token-y: token-y, sender: sender, fee: fee, fee-rebate: fee-rebate })
        (ok {dx: dx-net-fees, dy: dy})))

(define-public (swap-y-for-x (token-x-trait <ft>) (token-y-trait <sft>) (token-y-id uint) (dy uint) (min-dx (optional uint)))
    (let (
            (token-x (contract-of token-x-trait))
            (token-y (contract-of token-y-trait))
            (pool (try! (get-pool-details token-x token-y token-y-id)))
            (balance-x (get balance-x pool))
            (balance-y (get balance-y pool))
            (fee (mul-up dy (get fee-rate-y pool)))
            (dy-net-fees (if (<= dy fee) u0 (- dy fee)))
            (fee-rebate (mul-down fee (get fee-rebate pool)))
            (dx (try! (get-x-given-y token-x token-y token-y-id dy-net-fees)))
            (pool-updated (merge pool {
                balance-x: (if (<= balance-x dx) u0 (- balance-x dx)),
                balance-y: (+ balance-y dy-net-fees fee-rebate),
                ;; oracle-resilient: (if (get oracle-enabled pool) (try! (get-oracle-resilient token-x token-y factor)) u0)
                }))
            (sender tx-sender))
				;; (asserts! (not (is-blocklisted-or-default tx-sender)) ERR-BLOCKLISTED)
        (asserts! (not (is-paused)) ERR-PAUSED)
        (try! (check-pool-status token-x token-y token-y-id))
        (asserts! (> dy u0) ERR-INVALID-LIQUIDITY)        
        (asserts! (>= (div-down dy-net-fees dx) (get-price-internal balance-x balance-y)) ERR-INVALID-LIQUIDITY)
        (asserts! (<= (default-to u0 min-dx) dx) ERR-EXCEEDS-MAX-SLIPPAGE)        
        (try! (contract-call? token-y-trait transfer token-y-id dy sender .amm-vault))
        (and (> dx u0) (as-contract (try! (contract-call? .amm-vault transfer-ft token-x-trait dx sender))))            
        (as-contract (try! (contract-call? .amm-vault add-to-reserve token-y (- fee fee-rebate))))
        (as-contract (try! (contract-call? .amm-registry update-pool token-x token-y token-y-id pool-updated)))
        (print { object: "pool", action: "swap-y-for-x", data: pool-updated, dx: dx, dy: dy, token-x: token-x, token-y: token-y, sender: sender, fee: fee, fee-rebate: fee-rebate })
        (ok {dx: dx, dy: dy-net-fees})))
;; private functions
(define-private (get-token-given-position-internal (balance-x uint) (balance-y uint) (total-supply uint) (dx uint) (dy uint))
	(if (is-eq total-supply u0)
		{token: (get-invariant dx dy), dy: dy}
		{token: (div-down (mul-down total-supply dx) balance-x), dy: (div-down (mul-down balance-y dx) balance-x)}))

(define-private (get-position-given-mint-internal (balance-x uint) (balance-y uint) (total-supply uint) (token-amount uint))
	(let (
			(token-div-supply (div-down token-amount total-supply))
		)
		{dx: (mul-down balance-x token-div-supply), dy: (mul-down balance-y token-div-supply)}))

(define-private (get-position-given-burn-internal (balance-x uint) (balance-y uint) (total-supply uint) (token-amount uint))
	(get-position-given-mint-internal balance-x balance-y total-supply token-amount))

(define-private (get-price-internal (balance-x uint) (balance-y uint))
        ;; (div-down (+ (- ONE_8 factor) (mul-down factor balance-y)) (+ (- ONE_8 factor) (mul-down factor balance-x)))
        (div-down balance-y balance-x)
)

(define-private (get-y-given-x-internal (balance-x uint) (balance-y uint) (dx uint))
        (div-down (mul-down dx balance-y) (+ balance-x dx)))

(define-private (get-x-given-y-internal (balance-x uint) (balance-y uint) (dy uint))
    (div-down (mul-down dy balance-x) (+ balance-y dy))  
)
;;--------------------------------------------------------------------------------------------------------
;;------------------------------------------------- Math -------------------------------------------------
;;--------------------------------------------------------------------------------------------------------
;; todo are all sft decimal places? might have to pass decimals variable as a config
(define-constant ONE_8 u100000000) ;; 8 decimal places
(define-constant MAX_POW_RELATIVE_ERROR u4)
(define-private (mul-down (a uint) (b uint))
	(/ (* a b) ONE_8))
(define-private (mul-up (a uint) (b uint))
	(let (
			(product (* a b))
		)
		(if (is-eq product u0) u0 (+ u1 (/ (- product u1) ONE_8)))))
(define-private (div-down (a uint) (b uint))
	(if (is-eq a u0) u0 (/ (* a ONE_8) b)))
(define-private (div-up (a uint) (b uint))
	(if (is-eq a u0) u0 (+ u1 (/ (- (* a ONE_8) u1) b))))