(use-trait ft-trait .trait-sip-010.sip-010-trait)

(define-trait m-token-trait
	(
		(mint-scaled (uint principal uint) (response bool uint))
		(burn-scaled (uint principal uint) (response bool uint))
    (get-scaled-balance (principal) (response uint uint))
    (get-scaled-total-supply () (response uint uint))
    (get-treasury () (response principal uint))
    (transfer-underlying-token (uint principal <ft-trait>) (response bool uint))
    (liquidate-m-token (uint principal principal) (response bool uint))
    (set-normalized-income (uint) (response bool uint))
		;; sip-10. TODO ask if theres a cleaner way than manually copying the sip 10 methods. I want to pass that trait as an argument through different functions & sometimes call sip-10 methods, while other times call the m-token methods. i get : method 'get-total-supply' unspecified in trait <m-token-trait>clarity
		;; Transfer from the caller to a new principal
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))

    ;; the human readable name of the token
    (get-name () (response (string-ascii 32) uint))

    ;; the ticker symbol, or empty if none
    (get-symbol () (response (string-ascii 32) uint))

    ;; the number of decimals used, e.g. 6 would mean 1_000_000 represents 1 token
    (get-decimals () (response uint uint))

    ;; the balance of the passed principal
    (get-balance (principal) (response uint uint))

    ;; the current total supply (which does not need to be a constant)
    (get-total-supply () (response uint uint))

    ;; an optional URI that represents metadata of this token
    (get-token-uri () (response (optional (string-utf8 256)) uint))

    (mint (uint principal) (response bool uint))
    (burn (uint principal) (response bool uint))   
	)
)