(use-trait ft .sip-010-trait.sip-010-trait)
(use-trait sft .sip013-semi-fungible-token-trait.sip013-semi-fungible-token-trait)
(use-trait flash-loan-trait .trait-flash-loan-user.flash-loan-user-trait)

(define-constant ONE_8 u100000000) ;; 8 decimal places
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-PAUSED (err u1001))
(define-constant ERR-INVALID-BALANCE (err u1002))
(define-constant ERR-INVALID-TOKEN (err u2026))
(define-constant ERR-AMOUNT-EXCEED-RESERVE (err u2024))
(define-map approved-tokens principal bool)
(define-map approved-flash-loan-users principal bool)
(define-map reserve principal uint)
(define-data-var flash-loan-fee-rate uint u0)
(define-data-var flash-loan-enabled bool false)
(define-data-var paused bool false)
(define-read-only (is-dao-or-extension)
	;; (ok (asserts! (or (is-eq tx-sender .executor-dao) (contract-call? .executor-dao is-extension contract-caller)) ERR-NOT-AUTHORIZED)))
(contract-call? .auth check-is-approved )
)
(define-read-only (get-flash-loan-enabled)
	(var-get flash-loan-enabled))
(define-read-only (is-paused)
	(var-get paused))
(define-read-only (get-flash-loan-fee-rate)
	(var-get flash-loan-fee-rate))
(define-read-only (get-reserve (token-trait principal))
	(default-to u0 (map-get? reserve token-trait)))
(define-public (set-flash-loan-enabled (enabled bool))
	(begin
		(try! (is-dao-or-extension))
		(ok (var-set flash-loan-enabled enabled))))
(define-public (pause (new-paused bool))
	(begin
		(try! (is-dao-or-extension))
		(ok (var-set paused new-paused))))
(define-public (set-approved-flash-loan-user (flash-loan-user-trait principal) (approved bool))
	(begin
		(try! (is-dao-or-extension))
		(ok (map-set approved-flash-loan-users flash-loan-user-trait approved))))
(define-public (set-approved-token (token-trait principal) (approved bool))
	(begin
		(try! (is-dao-or-extension))
		(ok (map-set approved-tokens token-trait approved))))
(define-public (set-flash-loan-fee-rate (fee uint))
	(begin
		(try! (is-dao-or-extension))
		(ok (var-set flash-loan-fee-rate fee))))
(define-public (transfer-ft (token-trait <ft>) (amount uint) (recipient principal))
	(begin
		(asserts! (not (is-paused)) ERR-PAUSED)
		(asserts! (and (is-ok (is-dao-or-extension)) (is-ok (check-is-approved-token (contract-of token-trait)))) ERR-NOT-AUTHORIZED)
		(as-contract (contract-call? token-trait transfer amount tx-sender recipient none))))
(define-public (transfer-ft-two (token-x-trait <ft>) (dx uint) (token-y-trait <ft>) (dy uint) (recipient principal))
	(begin
		(try! (transfer-ft token-x-trait dx recipient))
		(transfer-ft token-y-trait dy recipient)))
(define-public (transfer-sft (token-trait <sft>) (token-id uint) (amount uint) (recipient principal))
	(begin
		(asserts! (not (is-paused)) ERR-PAUSED)
		(asserts! (and (is-ok (is-dao-or-extension)) (is-ok (check-is-approved-token (contract-of token-trait)))) ERR-NOT-AUTHORIZED)
		(as-contract (contract-call? token-trait transfer token-id amount tx-sender recipient))))
(define-public (flash-loan (flash-loan-user-trait <flash-loan-trait>) (token-trait <ft>) (amount uint) (memo (optional (buff 16))))
	(let (
			(pre-bal (unwrap-panic (contract-call? token-trait get-balance (as-contract tx-sender))))
			(fee-with-principal (+ ONE_8 (var-get flash-loan-fee-rate)))
			(amount-with-fee (mul-up amount fee-with-principal))
			(recipient tx-sender))
		(asserts! (not (is-paused)) ERR-PAUSED)
		(asserts! (and (is-ok (check-is-approved-flash-loan-user (contract-of flash-loan-user-trait))) (is-ok (check-is-approved-token (contract-of token-trait)))) ERR-NOT-AUTHORIZED)
		;; make sure current balance > loan amount
		(asserts! (> pre-bal amount) ERR-INVALID-BALANCE)
		;; transfer loan to flash-loan-user
		(as-contract (try! (contract-call? token-trait transfer amount tx-sender recipient none)))
		;; flash-loan-user executes with loan received
		(try! (contract-call? flash-loan-user-trait execute token-trait amount memo))
		;; return the loan + fee
		(try! (contract-call? token-trait transfer amount-with-fee recipient (as-contract tx-sender) none))
		(ok amount-with-fee)))
(define-public (add-to-reserve (token-trait principal) (amount uint))
	(begin
		(asserts! (not (is-paused)) ERR-PAUSED)
		(try! (is-dao-or-extension))
		(ok (map-set reserve token-trait (+ amount (get-reserve token-trait))))))
(define-public (remove-from-reserve (token-trait principal) (amount uint))
	(begin
		(asserts! (not (is-paused)) ERR-PAUSED)
		(try! (is-dao-or-extension))
		(asserts! (<= amount (get-reserve token-trait)) ERR-AMOUNT-EXCEED-RESERVE)
		(ok (map-set reserve token-trait (- (get-reserve token-trait) amount)))))
(define-private (check-is-approved-flash-loan-user (flash-loan-user-trait principal))
	(ok (asserts! (default-to false (map-get? approved-flash-loan-users flash-loan-user-trait)) ERR-NOT-AUTHORIZED)))
(define-private (check-is-approved-token (flash-loan-token principal))
	(ok (asserts! (default-to false (map-get? approved-tokens flash-loan-token)) ERR-NOT-AUTHORIZED)))
(define-private (mul-down (a uint) (b uint))
	(/ (* a b) ONE_8))
(define-private (mul-up (a uint) (b uint))
	(let  (
			(product (* a b)))
		(if (is-eq product u0) u0 (+ u1 (/ (- product u1) ONE_8)))))
