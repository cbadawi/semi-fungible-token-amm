;; 																----------------------
;; 																-------- AUTH --------
;; 																----------------------

(define-constant ERR-NOT-AUTHORIZED (err u1000))


(define-data-var contract-owner principal tx-sender)
(define-map approved-contracts principal bool)
(map-set approved-contracts (as-contract tx-sender) true)

(define-read-only (check-is-owner)
  (ok (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED))
)
(define-read-only (check-is-approved)
  (ok (asserts! (default-to false (map-get? approved-contracts tx-sender)) ERR-NOT-AUTHORIZED))
)

(define-public (change-owner (new-owner principal)) 
(begin 
	(try! (check-is-owner)) 
	(ok (var-set contract-owner new-owner))
))

(define-public (add-approved-contract (new-approved-contract principal))
	(begin
		(try! (check-is-owner))
		(ok (map-set approved-contracts new-approved-contract true))
))

(define-public (remove-approved-contract (approved-contract principal))
	(begin
		(try! (check-is-owner))
		(ok (map-set approved-contracts approved-contract false))
))