(use-trait ft .sip-010-trait.sip-010-trait)
(use-trait sft .sip013-semi-fungible-token-trait.sip013-semi-fungible-token-trait)

(define-constant ERR-POOL-ALREADY-EXISTS (err u2000))
(define-constant ERR-INVALID-POOL (err u2001))

(define-data-var pool-nonce uint u0)

(define-map pools-id-map uint { token-x: principal, token-y: principal, token-y-id: uint })
(define-map pools-data-map
  {
    token-x: principal,
    token-y: principal,
		token-y-id: uint
  }
  {
    pool-id: uint,
    total-supply: uint,
    balance-x: uint,
    balance-y: uint,
    fee-rate-x: uint,
    fee-rate-y: uint,
    fee-rebate: uint,
    start-block: uint,
    end-block: uint,
    threshold-x: uint,
    threshold-y: uint,
    max-in-ratio: uint,
    max-out-ratio: uint
  }
)

(define-read-only (get-pool-details-by-id (pool-id uint))
    (ok (unwrap! (map-get? pools-id-map pool-id) ERR-INVALID-POOL)))
(define-read-only (get-pool-details (token-x principal) (token-y principal) (token-y-id uint))
    (ok (unwrap! 
		(map-get? pools-data-map { token-x: token-x, token-y: token-y, token-y-id: token-y-id })
		 ERR-INVALID-POOL)))

(define-public (create-pool (token-x-trait <ft>) (token-y-trait <sft>) (token-y-id uint)) 
    (let (
            (pool-id (+ (var-get pool-nonce) u1))
            (token-x (contract-of token-x-trait))
            (token-y (contract-of token-y-trait))
            (pool-data {
                pool-id: pool-id,
                total-supply: u0,
                balance-x: u0,
                balance-y: u0,
                fee-rate-x: u0,
                fee-rate-y: u0,
                fee-rebate: u0,
                start-block: u340282366920938463463374607431768211455,
                end-block: u340282366920938463463374607431768211455,
                threshold-x: u0,
                threshold-y: u0,
                max-in-ratio: u0,
                max-out-ratio: u0
            }))
        (try! (is-approved))
        (asserts! (is-none (map-get? pools-data-map { token-x: token-x, token-y: token-y, token-y-id:token-y-id })) ERR-POOL-ALREADY-EXISTS)             
        (map-set pools-data-map { token-x: token-x, token-y: token-y, token-y-id: token-y-id} pool-data)
        (map-set pools-id-map pool-id { token-x: token-x, token-y: token-y, token-y-id: token-y-id })
        (var-set pool-nonce pool-id)
        (print { object: "pool", action: "created", data: pool-data, token-x: token-x, token-y: token-y})
        (ok true)))  
				      
(define-public (update-pool (token-x principal) (token-y principal) (token-y-id uint)
    (pool-data {
        pool-id: uint,
        total-supply: uint,
        balance-x: uint,
        balance-y: uint,
        fee-rate-x: uint,
        fee-rate-y: uint,
        fee-rebate: uint,
        start-block: uint,
        end-block: uint,
        threshold-x: uint,
        threshold-y: uint,
        max-in-ratio: uint,
        max-out-ratio: uint }))
    (begin
        (try! (is-approved))
        (ok (map-set pools-data-map { token-x: token-x, token-y: token-y, token-y-id: token-y-id} pool-data))))


;; private
(define-private (is-approved) 
(contract-call? .auth check-is-approved))