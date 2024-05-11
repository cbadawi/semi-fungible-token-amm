(use-trait ft-trait .trait-sip-010.sip-010-trait)

(define-constant ERR-NOT-AUTHORIZED (err u2000))
(define-constant ERR-NOT-AUTHORIZED-TOKEN (err u2001))
(define-constant ERR-RESERVE-NOT-EXIST (err u2002))
(define-constant ERR-CONFIG-NOT-SET (err u2007))

(define-constant iterator-helper-list (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19))
(define-constant DECIMALS u18)
(define-constant UNIT (pow u10 DECIMALS))


(define-map reserve-data { asset: principal } { 
	id: uint,
	liquidity-index: uint,
	liquidity-rate: uint,
	variable-borrow-index: uint,
	variable-borrow-rate: uint,
	last-updated-timestamp: uint,
	m-token-principal: principal,
	stable-debt-token-principal: principal,
	variable-debt-token-principal: principal,
	interest-rate-strategy-principal: principal,
	accrued-to-treasury: uint,
})

(define-map reserves-map { id: uint } { address: principal })
;; having to update balance and debt in user-assets-balance because I cant loop through all traits for calculateUserAccountData : _getUserBalanceInBaseCurrency & _getUserDebtInBaseCurrency & : https://discord.com/channels/621759717756370964/713087894260023377/1233016424012709918
;; user-assets-balance keeps track of balances per pool used in the HF calculations, while get-user-account-data provides an overall overview.
(define-map user-assets-balance {user: principal, asset: principal} { scaled-collateral-balance: uint, scaled-debt-balance: uint })
(define-data-var reserves-count uint u0)


;; todo probably will have to be rewritten as a bitmap with 1 or 2 uint128s to save on gas
;;  the configs commented out will not be used in the initial iteration v0
;;  see contracts/protocol/pool/PoolConfigurator.sol
;; contracts/protocol/libraries/types/DataTypes.sol#L38
;; contracts/protocol/libraries/configuration/ReserveConfiguration.sol
(define-map reserve-config { asset: principal } {
	ltv: uint, ;;  maximum percentage e.g., at LTV=75%, for every 1 ETH worth of collateral, borrowers will be able to borrow 0.75 ETH worth of the corresponding currency
	liquidation-threshold:uint, ;; percentage at which a position is defined as undercollateralised. For example, a Liquidation threshold of 80% means that if the debt value rises above 80% of the collateral, the position is undercollateralised and could be liquidated.
	liquidation-bonus:uint, ;; equiv to liquidationBonus. percentage of the collateral liquidated. 105% means liquidators will recieve 5% of the collateral
	decimals: uint,
	is-active: bool,
	is-frozen: bool,
	is-borrowing-enabled: bool, ;; todo where is that used?
	;; is-stable-rate-borrowing-enabled: bool,
	is-paused: bool,
	;; is-borrowing-in-isolation-mode-enabled: bool,
	;; is-siloed-borrowing-enabled: bool,
	;; is-flash-loan-enabled: bool,
	reserve-factor: uint, ;; in bps. ex 10.01% is u1001
	;; TODO why do borrow caps and supply caps need to be in whole tokens? save on gas and have them in decimal units to not have to multiplly by 10^decimals
	borrow-cap:uint, ;; in whole tokens, do not include decimals. borrowCap == 0 => no cap ;; reduces insolvency risks
	supply-cap:uint, ;; in whole tokens, do not include decimals.supplyCap == 0 => no cap
	liquidation-fee: uint, ;; percentage
	;; e-mode-category: uint,
	;; debt-ceiling-in-isolation-mode: uint, ;; with (ReserveConfiguration::DEBT_CEILING_DECIMALS) decimals
})

;;-------------------------------------------------------------------------------------
;;-------------------------------------- GETTERS --------------------------------------
;;-------------------------------------------------------------------------------------
(define-read-only (get-reserve (asset principal)) 
	(ok (unwrap! (map-get? reserve-data {asset: asset}) ERR-RESERVE-NOT-EXIST))
)

(define-read-only (get-reserves-count) 
	(var-get reserves-count)
)

(define-read-only (get-reserves-list) 
		(let ((iterator (unwrap-panic (slice? iterator-helper-list u0 (get-reserves-count)))))
				(map get-reserves-list-helper iterator)
))

(define-read-only (get-reserve-principal (id uint))
	(unwrap-panic (map-get? reserves-map {id: id}))
)

(define-private (get-reserves-list-helper (id uint)) 
	(get address (get-reserve-principal id))
)

(define-read-only (get-reserve-config (asset principal)) 
	(ok (unwrap! (map-get? reserve-config {asset: asset}) ERR-CONFIG-NOT-SET))
)

;; default to false and u0 if its the first supply)
(define-read-only (get-user-assets-balance (asset principal) (who principal)) 
	(ok (match (map-get? user-assets-balance {asset: asset, user: who})
			user-assets
			user-assets
			{scaled-collateral-balance: u0, scaled-debt-balance: u0}
)))
;;----------------------------------------------------------------------------------------
;;------------------------------------- SETTERS ------------------------------------------
;;----------------------------------------------------------------------------------------
;; reserves
(define-public (init-reserve (asset principal) 
														 (asset-trait <ft-trait>)
														(m-token-principal principal) 
														(stable-debt-token-principal principal) ;; todo
														(variable-debt-token-principal principal)
														) 
	(let ((reserve-id (var-get reserves-count))
				) 
		(asserts! (or (is-ok (check-is-approved)) (is-ok (check-is-owner))) ERR-NOT-AUTHORIZED)
		(var-set reserves-count (+ reserve-id u1))
		(map-set reserves-map {id: reserve-id} {address: asset })
		;; TODO defaults are my best guess
		(ok (map-insert reserve-data {asset: asset} {
			id:reserve-id,
			liquidity-index: UNIT,
			liquidity-rate: u0,
			variable-borrow-index: UNIT,
			variable-borrow-rate: u0,
			last-updated-timestamp: (get-current-time),
			m-token-principal: m-token-principal,
			stable-debt-token-principal: stable-debt-token-principal,
			variable-debt-token-principal: variable-debt-token-principal,
			interest-rate-strategy-principal:variable-debt-token-principal, ;; TODO seperate concerns and have the reserve strategy callable by its own trait. worth a read https://book.clarity-lang.org/ch13-03-contract-upgradability.html
			accrued-to-treasury: u0,
		}))
))

(define-public (set-reserve (asset principal) 
														(new-reserve (tuple (id uint) (liquidity-index uint) (liquidity-rate uint) (variable-borrow-index uint) (variable-borrow-rate uint) (last-updated-timestamp uint) (m-token-principal principal) (stable-debt-token-principal principal) (variable-debt-token-principal principal) (interest-rate-strategy-principal principal) (accrued-to-treasury uint)))
														)
		(begin 
			(asserts! (or (is-ok (check-is-approved)) (is-ok (check-is-owner))) ERR-NOT-AUTHORIZED)
			(ok (map-set reserve-data { asset: asset } new-reserve))
))

;; TODO what are the repercussions of dropping a reserve? what hapens to the funds?
(define-public (drop-reserve) 
	(begin 
			(asserts! (or (is-ok (check-is-approved)) (is-ok (check-is-owner))) ERR-NOT-AUTHORIZED)
			(ok true)
))
(define-public (set-reserve-interest-rate-strategy-principal (asset principal) (interest-rate-strategy-principal principal)) 
	(begin 
			(asserts! (or (is-ok (check-is-approved)) (is-ok (check-is-owner))) ERR-NOT-AUTHORIZED)
			(let (
					(reserve (try! (get-reserve asset)))
					) 
				(ok (map-set reserve-data {asset: asset} (merge reserve {interest-rate-strategy-principal: interest-rate-strategy-principal}))) ;; todo shouldnt this be in config?
)))
;; configuration
(define-public (set-configuration (asset principal) (config (tuple (ltv uint) (liquidation-threshold uint) (liquidation-bonus uint) (decimals uint) (is-active bool) (is-frozen bool) (is-borrowing-enabled bool)  (is-paused bool) (reserve-factor uint) (borrow-cap uint) (supply-cap uint) (liquidation-fee uint))))
	(begin 
			(asserts! (or (is-ok (check-is-approved)) (is-ok (check-is-owner))) ERR-NOT-AUTHORIZED)
			(ok (map-set reserve-config {asset: asset} config))
))

(define-public (set-user-assets-balance (who principal) (asset principal) (new-balances (tuple (scaled-collateral-balance uint) (scaled-debt-balance uint)))) 
	(begin 
		(asserts! (or (is-ok (check-is-approved)) (is-ok (check-is-owner))) ERR-NOT-AUTHORIZED)
		(ok (map-set user-assets-balance {user: who, asset: asset} new-balances))
))

;;----------------------------------------------------------------------------------------
;;------------------------------------- AUTH ------------------------------------------
;;----------------------------------------------------------------------------------------
(define-data-var contract-owner principal tx-sender)
(define-map approved-contracts principal bool)
(map-set approved-contracts (as-contract tx-sender) true)

(define-private (check-is-owner)
  (ok (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED))
)
(define-private (check-is-approved)
  (ok (asserts! (default-to false (map-get? approved-contracts tx-sender)) ERR-NOT-AUTHORIZED))
)
(define-public (add-approved-contract (new-approved-contract principal))
	(begin
		(try! (check-is-owner))
		(ok (map-set approved-contracts new-approved-contract true))
))
(define-public (drop-approved-contract (approved-contract principal))
	(begin
		(try! (check-is-owner))
		(ok (map-delete approved-contracts approved-contract))
))

(define-private (get-current-time) 
	(contract-call? .utils get-current-time)
)
