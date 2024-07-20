(use-trait ft-trait .trait-sip-010.sip-010-trait)

(define-trait variable-debt-token-trait
	(
		(mint-scaled (uint principal uint) (response bool uint))
		(burn-scaled (uint principal uint) (response bool uint))
    (get-scaled-balance (principal) (response uint uint))
    (get-scaled-total-supply () (response uint uint))
    (set-normalized-debt (uint) (response bool uint))

    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri () (response (optional (string-utf8 256)) uint))
    (mint (uint principal) (response bool uint))
    (burn (uint principal) (response bool uint))   
	)
) 