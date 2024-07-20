(use-trait ft-trait .trait-sip-010.sip-010-trait)
(use-trait ct-trait .trait-m-token.m-token-trait)
(use-trait vdt-trait .trait-variable-debt-token.variable-debt-token-trait)

(define-constant ERR-YOU-POOR (err u420))
(define-constant ERR-NOT-AUTHORIZED (err u2000))
(define-constant ERR-NOT-AUTHORIZED-TOKEN (err u2001))
(define-constant ERR-RESERVE-NOT-EXIST (err u2002))
(define-constant ERR-RESERVE-INACTIVE (err u2003))
(define-constant ERR-RESERVE-PAUSED (err u2004))
(define-constant ERR-RESERVE-FROZEN (err u2005))
(define-constant ERR-BORROWING-NOT-ENABLED (err u2006))
(define-constant ERR-CONFIG-NOT-SET (err u2007))
(define-constant ERR-INVALID-AMOUNT (err u2008))
(define-constant ERR-SUPPLY-CAP-EXCEEDED (err u2009))
(define-constant ERR-BORROW-CAP-EXCEEDED (err u2010))
(define-constant ERR-HEALTH-FACTOR-LESS-THAN-LIQUIDATION-THRESHOLD (err u2011))
(define-constant ERR-BORROWING-DISABLED (err u2012))
(define-constant ERR-NO-COLLATERAL-FOUND (err u2013))
(define-constant ERR-LTV-INVALID (err u2014))
(define-constant ERR-PERCENTAGE-INVALID (err u2015))
(define-constant ERR-COLLATERAL-TOO-LOW (err u2016))
(define-constant ERR-HEALTH-FACTOR-NOT-BELOW-THRESHOLD (err u2017))
(define-constant ERR-NO-DEBT-FOUND (err u2018))
(define-constant ERR-ZERO-UNDERLYING-BALANCE (err u2019))

(define-constant iterator-helper-list (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19))
(define-constant MAX_UINT u340282366920938463463374607431768211455)

;; unit's use is equivalent to a ray https://medium.com/dapphub/introducing-ds-math-an-innovative-safe-math-library-d58bc88313da
(define-constant UNIT (pow u10 DECIMALS))
(define-constant SECONDS-PER-YEAR u31536000)
;; decimals of indices, rate etc... not tokens
(define-constant DECIMALS u18)
(define-constant PERCENTAGE_FACTOR u10000) ;; 100% is 10000 

;;  these need to be relative to unit since they are being compared to health-factor in get-debt
;; TODO defaults should be added to the reserve config or in default reserve strategy contract
(define-constant MAX_LIQUIDATION_CLOSE_FACTOR  UNIT)
(define-constant DEFAULT_LIQUIDATION_CLOSE_FACTOR (/ UNIT u2)) ;; % of the debt to be liquidated ;; todo this doesnt need to be hardcoded and can be calculated to minimize liquidations
(define-constant HEALTH_FACTOR_LIQUIDATION_THRESHOLD (* (/ UNIT u100) u95)) ;; percentage at which the HF needs to be to trigger a liquidation
(define-constant CLOSE_FACTOR_HF_THRESHOLD (* (/ UNIT u100) u95)) ;; when HF reached 95% of MAX_LIQUIDATION_CLOSE_FACTOR => liquidate
(define-constant OPTIMAL_USAGE_RATIO (/ (* PERCENTAGE_FACTOR u80) u100)) ;; percentage expressed in bps;; todo should be moved in the default reserve startegy contract

;; --------------------------------------------------------------------------------------
;; ----------------------------------------GETTERS---------------------------------------
;; --------------------------------------------------------------------------------------
(define-read-only (get-reserve (asset principal)) 
	(contract-call? .reserve-data get-reserve asset)
)

(define-read-only (get-reserves-list) 
	(contract-call? .reserve-data get-reserves-list)
)

(define-read-only (get-reserve-config (asset principal)) 
	(contract-call? .reserve-data get-reserve-config asset)
)

(define-read-only (get-reserves-count) 
	(contract-call? .reserve-data get-reserves-count)
)

(define-read-only (get-user-assets-balance (asset principal) (who principal)) 
	(contract-call? .reserve-data get-user-assets-balance asset who)
)

(define-private (is-asset-used-as-collateral (who principal) (asset principal)) 
	(let  (
			(user-config (contract-call? .reserve-data get-user-config who))
			(curr-used-as-collateral (get used-as-collateral user-config))
			(reserve-id (get id (contract-call? .reserve-data get-reserve-id asset)))
		)
		(print {Function:"is-asset-used-as-collateral", who:who, asset:asset, reserve-id:reserve-id, curr-used-as-collateral:curr-used-as-collateral})
		(unwrap-panic (element-at? curr-used-as-collateral reserve-id))
))
;; ---------------------------------------------------------------------------------------
;; ----------------------------------------Setters----------------------------------------
;; ---------------------------------------------------------------------------------------
(define-private (set-reserve (asset principal) 
														(new-reserve (tuple (id uint) (liquidity-index uint) (liquidity-rate uint) (variable-borrow-index uint) (variable-borrow-rate uint) (last-updated-timestamp uint) (m-token-principal principal) (stable-debt-token-principal principal) (variable-debt-token-principal principal) (interest-rate-strategy-principal principal) (accrued-to-treasury uint)))
														)
	(as-contract (contract-call? .reserve-data set-reserve asset new-reserve))
)

(define-private (set-user-assets-balance (who principal) (asset principal) (new-balances (tuple (scaled-collateral-balance uint) (scaled-debt-balance uint)))) 
	(as-contract (contract-call? .reserve-data set-user-assets-balance who asset new-balances))
)

(define-public (set-as-collateral (enable bool) (asset-trait <ft-trait>) (m-token-trait <ct-trait>)) 
	(let ((who tx-sender)
				(asset (contract-of asset-trait))
				(reserve (try! (get-reserve asset)))
				(balance (try! (contract-call? m-token-trait get-balance who)))
				(config (try! (get-reserve-config asset)))
				(is-active (get is-active config))
				(is-paused (get is-paused config))
				(ltv (get ltv config))
				(debt-ceiling (get debt-ceiling config))
			)
			(try! (check-set-as-collateral asset balance is-active is-paused)) 
			(if enable 
					(try! (validate-and-set-collateral who asset ltv debt-ceiling) )
					(try! (validate-and-unset-collateral who asset))
					)
			(ok true)
))

(define-private (validate-and-set-collateral (who principal) (asset principal) (ltv uint) (debt-ceiling uint)) 
	(let ((user-config (contract-call? .reserve-data get-user-config who))
				(opt-isolated-asset (get isolated-asset user-config))
				(curr-used-as-collateral (get used-as-collateral user-config))
				(next-used-as-collateral (get-new-used-as-collateral asset curr-used-as-collateral true))
			)
			(if (or (is-eq ltv u0) (not (is-eq debt-ceiling u0))) 
							false 
							;; is isolation mode active?
							(match opt-isolated-asset isolated-asset 
								false 
								(as-contract (try! (contract-call? .reserve-data set-user-config who (merge user-config 
																																											{isolated-asset: (some asset), 
																																											used-as-collateral: next-used-as-collateral }))))
				))
				(ok true)
				))

(define-private (validate-and-unset-collateral (who principal) (asset principal)) 
	(let ((user-config (contract-call? .reserve-data get-user-config who))
				(curr-used-as-collateral (get used-as-collateral user-config))
				(reserve-id (get id (contract-call? .reserve-data get-reserve-id asset)))
			)
		(try! (check-health-and-ltv who))
		(ok (as-contract (try! (contract-call? .reserve-data set-user-config who (merge user-config 
																																{used-as-collateral: (get-new-used-as-collateral asset curr-used-as-collateral false)})))))
))

;; --------------------------------------------------------------------------------------
;; ---------------------------------------CHECKERS---------------------------------------
;; --------------------------------------------------------------------------------------
(define-private (check-set-as-collateral (asset principal) (balance uint) (is-active bool) (is-paused bool)) 
	(begin (asserts! is-active ERR-RESERVE-INACTIVE)
		(asserts! (not is-paused) ERR-RESERVE-PAUSED)
		(asserts! (> balance u0) ERR-ZERO-UNDERLYING-BALANCE)
		(ok true)
))

(define-private (get-new-used-as-collateral (asset principal) (curr-used-as-collateral (list 20 bool)) (enable bool))
	(let ((reserve-id (get id (contract-call? .reserve-data get-reserve-id asset)))
				(len-assets (len curr-used-as-collateral))
				(left-side (unwrap-panic (slice? curr-used-as-collateral u0 reserve-id)))
				(right-side (if (>= reserve-id (len curr-used-as-collateral)) (list ) (unwrap-panic (slice? curr-used-as-collateral (+ reserve-id u1) len-assets))))
				(next-used-as-collateral (concat (concat left-side (list enable)) (unwrap-panic (slice? curr-used-as-collateral (+ reserve-id u1) len-assets))))
			)
			(unwrap-panic (as-max-len? next-used-as-collateral u20))
))

;; --------------------------------------------------------------------------------------
;; ----------------------------------------SUPPLY----------------------------------------
;; --------------------------------------------------------------------------------------								
;; TODO remove asset from arguments and use contract-of asset-trait. no need for verifications, get reserve will fail.
(define-public (supply (asset principal) (amount uint) (asset-trait <ft-trait>) (variable-debt-token-trait <vdt-trait>) (m-token-trait <ct-trait>)) 
	(let (
			(sender tx-sender)
			(reserve (try! (get-reserve asset)))
			(config (try! (get-reserve-config asset)))
			(user-asset-balance (get-user-assets-balance asset sender))
			;; variables below are found as is in the reserveCache variable
			(current-liquidity-rate  (get liquidity-rate reserve))
			(current-liquidity-index (get liquidity-index reserve))
			(last-updated-timestamp  (get last-updated-timestamp reserve))
			(variable-debt-token-principal  (get variable-debt-token-principal reserve))
			(current-variable-borrow-rate  (get variable-borrow-rate reserve))
			(current-variable-borrow-index (get variable-borrow-index reserve))
			(m-token-principal (get m-token-principal reserve))
			(accrued-to-treasury (get accrued-to-treasury reserve))
			(reserve-factor (get reserve-factor config))
			(ltv (get ltv config))
			(debt-ceiling (get debt-ceiling config))
			(curr-scaled-variable-debt (try! (contract-call? variable-debt-token-trait get-scaled-total-supply)))
			(curr-balance (as-contract (try! (contract-call? m-token-trait get-scaled-balance sender)))) 
			(is-first-supply (is-eq curr-balance u0))
			)
		(asserts! (is-eq asset (contract-of asset-trait)) ERR-NOT-AUTHORIZED-TOKEN)
		(asserts! (is-eq m-token-principal (contract-of m-token-trait)) ERR-NOT-AUTHORIZED-TOKEN)
		(asserts! (is-eq variable-debt-token-principal (contract-of variable-debt-token-trait)) ERR-NOT-AUTHORIZED-TOKEN)
		(if is-first-supply (try! (validate-and-set-collateral sender asset ltv debt-ceiling)) false)
		(try! (update-state asset reserve current-liquidity-rate last-updated-timestamp current-variable-borrow-rate curr-scaled-variable-debt current-variable-borrow-index current-liquidity-index reserve-factor))
		(let (
					(next-reserve (try! (get-reserve asset)))
					(next-variable-borrow-index (get variable-borrow-index next-reserve))
					(next-liquidity-index (get liquidity-index next-reserve)) 
					(liquidity-added amount)
					(liquidity-taken u0)
					(total-stable-debt u0) ;; todo
					(average-stable-borrow-rate u0) ;; todo
					(prev-user-balance (get scaled-collateral-balance user-asset-balance))
					(scaled-amount (div-unit-up amount next-liquidity-index))
				)
				(try! (check-supply asset amount m-token-trait accrued-to-treasury next-liquidity-index))
				(as-contract (try! (contract-call? m-token-trait set-normalized-income next-liquidity-index)))
				(as-contract (try! (contract-call? variable-debt-token-trait set-normalized-debt next-variable-borrow-index)))
				(as-contract (try! (update-interest-rates next-reserve curr-scaled-variable-debt liquidity-added liquidity-taken reserve-factor asset asset-trait m-token-principal)))
				(try! (contract-call? asset-trait transfer amount sender m-token-principal none))
				(as-contract (try! (contract-call? m-token-trait mint-scaled amount sender next-liquidity-index)))
				(as-contract (try! (set-user-assets-balance sender asset (merge user-asset-balance {scaled-collateral-balance: (+ prev-user-balance scaled-amount)}))))
				(print { FUNCTION: "supply", reserve: reserve, config: config, curr-scaled-variable-debt: curr-scaled-variable-debt, new-scaled-collateral-balance: (+ prev-user-balance scaled-amount), next-liquidity-index:next-liquidity-index, amount:amount, scaled-amount:scaled-amount})
				(ok amount)
)))

(define-private (check-supply (asset principal) (amount uint) (m-token-trait <ct-trait>) (accrued-to-treasury uint) (next-liquidity-index uint)) 
	(begin  
		(asserts! (> amount u0) ERR-INVALID-AMOUNT)
		(let (
				(config (try! (get-reserve-config asset)))
				(is-active (get is-active config))
				(is-frozen (get is-frozen config))
				(is-paused (get is-paused config))
				(supply-cap (get supply-cap config))
				(decimals (get decimals config))
				(m-token-supply (try! (contract-call? m-token-trait get-scaled-total-supply))) 
				(supply-held (+ m-token-supply amount (mul-unit accrued-to-treasury next-liquidity-index)))
				)
				(print {FUNCTION:"check-supply",  accrued-to-treasury: accrued-to-treasury, next-liquidity-index:next-liquidity-index, m-token-supply:m-token-supply,  amount:amount, supply-held:supply-held, supply-cap:supply-cap, decimals:decimals, supply-cap-inbaseunit:(* supply-cap (pow u10 decimals)) })
				(asserts! is-active ERR-RESERVE-INACTIVE)
				(asserts! (not is-frozen) ERR-RESERVE-FROZEN)
				(asserts! (not is-paused) ERR-RESERVE-PAUSED)
				(asserts! (or (is-eq supply-cap u0)
				 							(< supply-held (* supply-cap (pow u10 decimals)  
										)))
											ERR-SUPPLY-CAP-EXCEEDED) 	
				(ok true)
)))

;; -------------------------------------------------------------------------------------------------
;; ----------------------------------------WITHDRAW-------------------------------------------------
;; -------------------------------------------------------------------------------------------------	
;; users redeem their cTokens for the underlying asset previously supplied in the protocol.
(define-public (withdraw (asset principal) (amount uint) (asset-trait <ft-trait>) (variable-debt-token-trait <vdt-trait>) (m-token-trait <ct-trait>))
	(let (
			(who tx-sender)
			(reserve (try! (get-reserve asset)))
			(config (try! (get-reserve-config asset)))
			(user-assets (get-user-assets-balance asset who))
			(curr-user-collateral (get scaled-collateral-balance user-assets))
			;; variables below are equivalent to the reserveCache variable
			(current-liquidity-rate  (get liquidity-rate reserve))
			(current-liquidity-index (get liquidity-index reserve))
			(last-updated-timestamp  (get last-updated-timestamp reserve))
			(variable-debt-token-principal  (get variable-debt-token-principal reserve))
			(current-variable-borrow-rate  (get variable-borrow-rate reserve))
			(current-variable-borrow-index (get variable-borrow-index reserve))
			(m-token-principal (get m-token-principal reserve))
			(accrued-to-treasury (get accrued-to-treasury reserve))
			(reserve-factor (get reserve-factor config))
			(is-active (get is-active config))
			(is-paused (get is-paused config))
			(curr-scaled-variable-debt (try! (contract-call? variable-debt-token-trait get-total-supply)))
			)
		(asserts! (is-eq asset (contract-of asset-trait)) ERR-NOT-AUTHORIZED-TOKEN)
		(asserts! (is-eq m-token-principal (contract-of m-token-trait)) ERR-NOT-AUTHORIZED-TOKEN)
		(try! (update-state asset reserve current-liquidity-rate last-updated-timestamp current-variable-borrow-rate curr-scaled-variable-debt current-variable-borrow-index current-liquidity-index reserve-factor))
		(let (
				(next-reserve (try! (get-reserve asset)))
				(next-variable-borrow-index (get variable-borrow-index next-reserve))
				(next-liquidity-index (get liquidity-index next-reserve))
				(total-variable-debt (mul-unit curr-scaled-variable-debt next-variable-borrow-index))
				(total-stable-debt u0) 
				(average-stable-borrow-rate u0)
				(scaled-balance (try! (contract-call? m-token-trait get-scaled-balance who)))
				(user-balance (mul-unit scaled-balance next-liquidity-index))
				(amount-to-withdraw (if (> amount user-balance) user-balance amount))
				(liquidity-added u0)
				(liquidity-taken amount-to-withdraw)
			)	
			(try! (check-withdraw amount-to-withdraw user-balance is-active is-paused))
			(as-contract (try! (contract-call? m-token-trait set-normalized-income next-liquidity-index)))
			(as-contract (try! (contract-call? variable-debt-token-trait set-normalized-debt next-variable-borrow-index)))
			(try! (update-interest-rates next-reserve curr-scaled-variable-debt liquidity-added liquidity-taken reserve-factor asset asset-trait m-token-principal))
			(as-contract (try! (contract-call? m-token-trait burn-scaled amount-to-withdraw who current-liquidity-index)))
			(as-contract (try! (contract-call? m-token-trait transfer-underlying-token amount-to-withdraw who asset-trait)))
			;; todo should amount-to-withdraw be scaled (divided by index)
			(try! (set-user-assets-balance who asset (merge user-assets {scaled-collateral-balance: (- curr-user-collateral amount-to-withdraw)})))
			(try! (check-health-and-ltv who))
			(print {FUNCTION:"withdraw", old-reserve:reserve, old-config:config, scaled-balance:scaled-balance, user-balance: user-balance, next-reserve:next-reserve, curr-user-collateral:curr-user-collateral,
					current-liquidity-index:current-liquidity-index, amount-to-withdraw:amount-to-withdraw, amount:amount })	
			(ok amount-to-withdraw)
)))

(define-private (check-withdraw (amount uint) (user-balance uint) (is-active bool) (is-paused bool))
	(begin
		(asserts! (not (is-eq amount u0)) ERR-INVALID-AMOUNT)
		(asserts! (<= amount user-balance) ERR-YOU-POOR)
		(asserts! is-active ERR-RESERVE-INACTIVE)
		(asserts! (not is-paused) ERR-RESERVE-PAUSED)
		(ok true)
))
;; ;; ----------------------------------------------------------------------------------------
;; ;; ----------------------------------------BORROW------------------------------------------
;; ;; ----------------------------------------------------------------------------------------	
(define-public (borrow (asset principal) (amount uint) (asset-trait <ft-trait>) (m-token-trait <ct-trait>) (variable-debt-token-trait <vdt-trait>))
 (let (
			(reserve (try! (get-reserve asset)))
			(sender tx-sender)
			(variable-debt-token-principal (get variable-debt-token-principal reserve))
			(m-token-principal (get m-token-principal reserve))
			(user-asset-balance (get-user-assets-balance asset sender))
			(current-variable-borrow-index (get variable-borrow-index reserve))
			(liquidity-rate (get liquidity-rate reserve))
			(last-updated-timestamp (get last-updated-timestamp reserve))
			(variable-borrow-rate (get variable-borrow-rate reserve))
			(current-liquidity-index (get liquidity-index reserve))
			(config (try! (get-reserve-config asset)))
			(reserve-factor (get reserve-factor config))
			(curr-scaled-variable-debt (try! (contract-call? variable-debt-token-trait get-total-supply)))
		) 
		(asserts! (is-eq asset (contract-of asset-trait)) ERR-NOT-AUTHORIZED-TOKEN)
		(asserts! (is-eq variable-debt-token-principal (contract-of variable-debt-token-trait)) ERR-NOT-AUTHORIZED-TOKEN)
		(asserts! (is-eq m-token-principal (contract-of m-token-trait)) ERR-NOT-AUTHORIZED-TOKEN)
		(try! (update-state asset reserve liquidity-rate last-updated-timestamp variable-borrow-rate curr-scaled-variable-debt current-variable-borrow-index current-liquidity-index reserve-factor))
		(let (
				(next-reserve (try! (get-reserve asset)))
				(next-liquidity-index (get liquidity-index next-reserve))
				(next-variable-borrow-index (get variable-borrow-index next-reserve))
				;; todo remove is-first-borrow logic from mint scaled if unused
				(is-first-borrow (as-contract (try! (contract-call? variable-debt-token-trait mint-scaled amount sender next-variable-borrow-index)))) 
				(next-scaled-variable-debt (try! (contract-call? variable-debt-token-trait get-total-supply)))
				(total-variable-debt (mul-unit next-scaled-variable-debt next-variable-borrow-index))
				(liquidity-added u0)
				(liquidity-taken amount) ;; todo sometimes the underlying isnt released : https://github.com/aave/aave-v3-core/blob/6d6fa53d360b43f492ff5b3c7033f95aee4f1335/contracts/protocol/libraries/logic/BorrowLogic.sol#L149
				(average-stable-borrow-rate u0)
				(total-stable-debt u0)
				(curr-user-debt (get scaled-debt-balance user-asset-balance))
			)
			(as-contract (try! (contract-call? m-token-trait set-normalized-income next-liquidity-index)))
			(as-contract (try! (contract-call? variable-debt-token-trait set-normalized-debt next-variable-borrow-index)))
			(try! (check-borrow sender amount asset variable-debt-token-trait next-variable-borrow-index))
			(print {FUNCTION:"borrow", next-scaled-variable-debt:next-scaled-variable-debt,total-variable-debt:total-variable-debt })
			(try! (set-user-assets-balance sender asset (merge user-asset-balance {scaled-debt-balance: (+ curr-user-debt amount)})))
			(try! (update-interest-rates next-reserve next-scaled-variable-debt liquidity-added liquidity-taken reserve-factor asset asset-trait m-token-principal))
			;; transfer borrowed asset held by the m-token-contract to user
			(as-contract (try! (contract-call? m-token-trait transfer-underlying-token amount sender asset-trait)))
			(ok true) 
)))

(define-private (check-borrow (who principal) (amount uint) (asset principal) (variable-debt-token-trait <vdt-trait>) (next-variable-borrow-index uint))
(let (
			(config (try! (get-reserve-config asset)))
			(decimals (get decimals config))
			(is-active (get is-active config))
			(is-paused (get is-paused config))
			(is-frozen (get is-frozen config))
			(is-borrowing-enabled (get is-borrowing-enabled config))
			(borrow-cap (get borrow-cap config))
		) 
		(asserts! is-active ERR-RESERVE-INACTIVE)
		(asserts! is-borrowing-enabled ERR-BORROWING-DISABLED)
		(asserts! (not is-frozen) ERR-RESERVE-FROZEN)
		(asserts! (not is-paused) ERR-RESERVE-PAUSED)
		(try! (check-borrow-cap borrow-cap amount variable-debt-token-trait next-variable-borrow-index decimals))
		(let ((user-account-data (get user-account-data (try! (check-health-and-ltv who))))
					(collateral (get collateral-value user-account-data))	
					(debt (get debt-value user-account-data))	
					(avg-liq-thresh (get average-liquidation-threshold user-account-data))
					(current-ltv (get average-ltv user-account-data))
					(asset-price (unwrap-panic (get-asset-price asset)))
					(borrow-value (/ (* amount asset-price) (pow u10 decimals)))
					(collateral-needed (percent-div (+ debt borrow-value) current-ltv)) ;; borrowed allowed = collateral * LTV
				)
			(print {FUNCTION:"check-borrow", amount:amount,debt:debt, current-ltv:current-ltv, borrow-value:borrow-value, asset:asset, asset-price:asset-price, user-account-data:user-account-data})
			(asserts! (not (is-eq collateral u0)) ERR-NO-COLLATERAL-FOUND)
			(asserts! (not (is-eq current-ltv u0)) ERR-LTV-INVALID)
			(asserts! (>= collateral collateral-needed) ERR-COLLATERAL-TOO-LOW)
			(ok true)
)))

(define-private (check-borrow-cap (borrow-cap uint) (amount uint) (variable-debt-token-trait <vdt-trait>) (next-variable-borrow-index uint) (decimals uint)) 
(let (
		(curr-scaled-variable-debt (try! (contract-call? variable-debt-token-trait get-total-supply)))
		(total-supplied-variable-debt (mul-unit  curr-scaled-variable-debt next-variable-borrow-index))
		(total-variable-debt (+ total-supplied-variable-debt amount))
		)
		(print {FUNCTION:"check-borrow-cap", total-variable-debt:total-variable-debt, borrow-cap:borrow-cap})
		(asserts! (< total-variable-debt (* borrow-cap (pow u10 decimals))) ERR-BORROW-CAP-EXCEEDED) 
		(ok true)
))

;; ;; ----------------------------------------------------------------------------------------
;; ;; -------------------------------------------REPAY----------------------------------------
;; ;; ----------------------------------------------------------------------------------------	
;; sends back to pool m-tokens or the underlying asset. burns variable debt
(define-public (repay (asset principal) (amount uint) (asset-trait <ft-trait>) (m-token-trait <ct-trait>) (variable-debt-token-trait <vdt-trait>) (use-m-tokens bool))
	(let (
				(sender tx-sender)
				(reserve (try! (get-reserve asset)))
				(liquidity-rate (get liquidity-rate reserve));;
				(last-updated-timestamp  (get last-updated-timestamp reserve));;
				(variable-borrow-index (get variable-borrow-index reserve)) 
				(current-variable-borrow-rate  (get variable-borrow-rate reserve))
				(variable-borrow-rate (get variable-borrow-rate reserve));;
				(current-liquidity-index (get liquidity-index reserve));;
				(m-token-principal (get m-token-principal reserve))
				(variable-debt-token-principal (get variable-debt-token-principal reserve))
				(curr-scaled-variable-debt (try! (contract-call? variable-debt-token-trait get-total-supply))) 
				(config (try! (get-reserve-config asset)))
				(reserve-factor (get reserve-factor config))
			)
			(asserts! (is-eq asset (contract-of asset-trait)) ERR-NOT-AUTHORIZED-TOKEN)
			(asserts! (is-eq m-token-principal (contract-of m-token-trait)) ERR-NOT-AUTHORIZED-TOKEN)
			(asserts! (is-eq variable-debt-token-principal (contract-of variable-debt-token-trait)) ERR-NOT-AUTHORIZED-TOKEN)
			(try! (update-state asset reserve liquidity-rate last-updated-timestamp variable-borrow-rate curr-scaled-variable-debt variable-borrow-index current-liquidity-index reserve-factor))
			(try! (check-repay sender amount asset use-m-tokens))
			(let (
						(next-reserve (try! (get-reserve asset)))
						(next-liquidity-index (get liquidity-index next-reserve));;
						(next-variable-borrow-index (get variable-borrow-index next-reserve));;
					) 
					(as-contract (try! (contract-call? variable-debt-token-trait set-normalized-debt next-variable-borrow-index)))
					(let (
						(user-debt (try! (contract-call? variable-debt-token-trait get-balance sender)))
						(amount-to-repay (if (> amount user-debt) user-debt amount))
						(liquidity-added amount-to-repay)
						(liquidity-taken u0)
						(user-scaled-debt (try! (contract-call? variable-debt-token-trait get-scaled-balance sender)))
						)
						(as-contract (try! (contract-call? m-token-trait set-normalized-income next-liquidity-index)))
						(as-contract (try! (contract-call? variable-debt-token-trait burn-scaled amount-to-repay sender next-variable-borrow-index)))
						(let ((next-scaled-variable-debt (try! (contract-call? variable-debt-token-trait get-scaled-total-supply)))
									(user-assets (get-user-assets-balance asset sender))
									(scaled-debt-balance (get scaled-debt-balance user-assets))
						) 
							(try! (update-interest-rates next-reserve next-scaled-variable-debt liquidity-added liquidity-taken reserve-factor asset asset-trait m-token-principal))
							(print {FUNCTION:"repay", amount:amount, amount-to-repay:amount-to-repay, next-variable-borrow-index:next-variable-borrow-index, user-assets:user-assets, next-scaled-debt:next-scaled-variable-debt, next-liquidity-index:next-liquidity-index, user-scaled-debt:user-scaled-debt, user-debt:user-debt})
							(try! (set-user-assets-balance sender asset (merge user-assets {scaled-debt-balance: next-scaled-variable-debt})))
							(if use-m-tokens (as-contract (try! (contract-call? m-token-trait burn-scaled amount-to-repay sender next-liquidity-index))) 
																(try! (contract-call? asset-trait transfer amount-to-repay sender m-token-principal none)))
							(ok amount-to-repay)
)))))

(define-public (check-repay (sender principal) (amount uint) (asset principal) (use-m-tokens bool)) 
	(begin  
		(asserts! (> amount u0) ERR-INVALID-AMOUNT)
		(let ((reserve (try! (get-reserve asset)))
				(config (unwrap-panic (get-reserve-config asset)))
				(is-active (get is-active config))
				(is-paused (get is-paused config))
			)
			(asserts! is-active ERR-RESERVE-INACTIVE)
			(asserts! (not is-paused) ERR-RESERVE-PAUSED)
		(ok true)
)))
;; ;; ----------------------------------------------------------------------------------------
;; ;; --------------------------------------LIQUIDATE-----------------------------------------
;; ;; ----------------------------------------------------------------------------------------	
;; the liquidated user keeps his debt but a portion of the collateral to be liquidated
(define-public (liquidate (liquidated-user principal) 
													(collateral-asset principal)
													(coll-asset-trait <ft-trait>)
													(coll-m-token-trait <ct-trait>) 
													(coll-variable-debt-token-trait <vdt-trait>) 
													(debt-asset principal) 
													(debt-asset-trait <ft-trait>) 
													(debt-m-token-trait <ct-trait>) 
													(debt-variable-debt-token-trait <vdt-trait>) 
													(debt-to-cover uint) 
													(receive-m-tokens bool)) 
(let ((liquidator tx-sender)
			;; debt reserve cache
			(debt-reserve (try! (get-reserve debt-asset)))
			(debt-m-token-principal (get m-token-principal debt-reserve))
			(debt-current-liquidity-rate  (get liquidity-rate debt-reserve))
			(last-updated-timestamp  (get last-updated-timestamp debt-reserve))
			(debt-current-variable-borrow-rate  (get variable-borrow-rate debt-reserve))
			(debt-current-liquidity-index (get liquidity-index debt-reserve))
			(debt-variable-debt-token-principal  (get variable-debt-token-principal debt-reserve))
			(debt-current-variable-borrow-index (get variable-borrow-index debt-reserve))
			(debt-next-variable-borrow-index debt-current-variable-borrow-index) 
			;; collateral reserve cache
			(coll-reserve (try! (get-reserve collateral-asset)))
			(coll-m-token-principal (get m-token-principal coll-reserve))
			(coll-variable-debt-token-principal (get variable-debt-token-principal coll-reserve))
			(coll-liquidity-rate (get liquidity-rate coll-reserve))
			(coll-last-updated-timestamp (get last-updated-timestamp coll-reserve))
			(coll-variable-borrow-rate (get variable-borrow-rate coll-reserve))
			(coll-variable-borrow-index (get variable-borrow-index coll-reserve))
			(coll-liquidity-index (get liquidity-index coll-reserve))
			(debt-reserve-config (unwrap-panic (get-reserve-config debt-asset))) 
			(coll-reserve-config (unwrap-panic (get-reserve-config collateral-asset))) 
			(debt-reserve-factor (get reserve-factor debt-reserve-config))
		)
		(asserts! (is-eq debt-asset (contract-of debt-asset-trait)) ERR-NOT-AUTHORIZED-TOKEN)
		(asserts! (is-eq debt-m-token-principal (contract-of debt-m-token-trait)) ERR-NOT-AUTHORIZED-TOKEN)
		(asserts! (is-eq debt-variable-debt-token-principal (contract-of debt-variable-debt-token-trait)) ERR-NOT-AUTHORIZED-TOKEN)
		;; todo something i'm not unserdtanding yet is that from the frontend we could have multiple collaterals, while in the code its just one variable. see "the liquidator can claim a single collateral," https://blog.wehodl.finance/how-liquidation-works-on-aave-a-guide-for-defi-lenders-and-borrowers-2e1d933dacd6
		(asserts! (is-eq collateral-asset (contract-of coll-asset-trait)) ERR-NOT-AUTHORIZED-TOKEN)
		(asserts! (is-eq coll-m-token-principal (contract-of coll-m-token-trait)) ERR-NOT-AUTHORIZED-TOKEN)
		(asserts! (is-eq coll-variable-debt-token-principal (contract-of coll-variable-debt-token-trait)) ERR-NOT-AUTHORIZED-TOKEN)
		(let (
				(debt-curr-scaled-variable-debt (try! (contract-call? debt-variable-debt-token-trait get-scaled-total-supply))) ;; todo get-total-supply
				(coll-curr-scaled-variable-debt (try! (contract-call? coll-variable-debt-token-trait get-scaled-total-supply))) ;; todo get-total-supply
				) 
				(try! (update-state debt-asset debt-reserve debt-current-liquidity-rate last-updated-timestamp debt-current-variable-borrow-rate debt-curr-scaled-variable-debt debt-current-variable-borrow-index debt-current-liquidity-index debt-reserve-factor))
				(let (
						(user-account-data (unwrap-panic (get-user-health-metrics liquidated-user)))
						(health-factor (get-health-factor user-account-data))
						(user-stable-debt u0)
						(user-variable-debt (try! (contract-call? debt-variable-debt-token-trait get-balance liquidated-user))) ;; todo should return balance * reserve normalized debt
						;;  why have 2 actual-debt-to-liquidate
						(initial-actual-debt-to-liquidate (get-debt health-factor user-stable-debt user-variable-debt debt-to-cover)) ;; calculateDebt
					)
					(try! (check-liquidation-call collateral-asset debt-asset health-factor user-stable-debt user-variable-debt))
					(let ( 
								(user-coll-balance (try! (contract-call? coll-m-token-trait get-balance liquidated-user)))
								(next-debt-reserve (try! (get-reserve debt-asset)))
								(next-debt-liquidity-index (get liquidity-index next-debt-reserve))
								(next-debt-variable-borrow-index (get variable-borrow-index next-debt-reserve))
								(curr-debt-variable-borrow-rate (get variable-borrow-rate next-debt-reserve))
								(debt-price (unwrap-panic (get-asset-price debt-asset)))
								(coll-price (unwrap-panic (get-asset-price collateral-asset)))
								(coll-reserve-factor (get reserve-factor coll-reserve-config))
								(coll-decimals (get decimals coll-reserve-config))
								(debt-decimals (get decimals debt-reserve-config))
								(liquidation-bonus (get liquidation-bonus coll-reserve-config))
								(liquidation-fee (get liquidation-fee coll-reserve-config))
								(actual-debt-and-coll-to-liquidate (get-final-actual-coll-and-debt-to-liquidate debt-asset coll-price debt-price coll-decimals debt-decimals initial-actual-debt-to-liquidate user-coll-balance liquidation-bonus liquidation-fee user-variable-debt debt-variable-debt-token-trait))
								(actual-coll-to-liquidate (get coll-amount actual-debt-and-coll-to-liquidate)) ;; u_10000000 ;; 0.1 btc
								(actual-debt-to-liquidate (get debt-amount actual-debt-and-coll-to-liquidate)) ;; debt-amount-needed: u571_70860000 ;; whats this? why are we burning this small amount of variable debt tokens
								(liquidation-fee-amount (get liquidation-fee-amount actual-debt-and-coll-to-liquidate))
								(liquidity-added actual-debt-to-liquidate)
								(liquidity-taken u0)
								(total-stable-debt u0)
								(average-stable-borrow-rate u0)
								(user-assets (get-user-assets-balance debt-asset liquidated-user))
							)
							(print {FUNCTION:"liquidate", health-factor:health-factor, user-account-data:user-account-data, user-variable-debt:user-variable-debt, initial-actual-debt-to-liquidate:initial-actual-debt-to-liquidate, actual-debt-to-liquidate:actual-debt-to-liquidate,
											 actual-coll-to-liquidate:actual-coll-to-liquidate, liquidation-fee-amount:liquidation-fee-amount, user-assets: user-assets})
							(as-contract (try! (contract-call? debt-m-token-trait set-normalized-income next-debt-liquidity-index)))
							(as-contract (try! (contract-call? debt-variable-debt-token-trait set-normalized-debt next-debt-variable-borrow-index)))
							(try! (burn-debt-tokens liquidated-user debt-asset user-variable-debt actual-debt-to-liquidate debt-variable-debt-token-trait))
							(let (
									(debt-next-scaled-variable-debt (try! (contract-call? debt-variable-debt-token-trait get-total-supply))) ;; todo these should be scaled total supply https://github.com/aave/aave-v3-core/blob/6d6fa53d360b43f492ff5b3c7033f95aee4f1335/contracts/protocol/libraries/logic/ReserveLogic.sol#L359
								)
								(try! (update-interest-rates next-debt-reserve debt-next-scaled-variable-debt liquidity-added liquidity-taken debt-reserve-factor debt-asset debt-asset-trait debt-m-token-principal))
								(if receive-m-tokens (try! (liquidate-m-tokens collateral-asset actual-coll-to-liquidate liquidated-user liquidator coll-m-token-trait))
																	(try! (burn-coll-tokens liquidated-user liquidator actual-coll-to-liquidate collateral-asset coll-reserve coll-liquidity-rate coll-last-updated-timestamp coll-variable-borrow-rate u0 coll-variable-borrow-index coll-liquidity-index coll-reserve-factor coll-asset-trait coll-m-token-trait coll-m-token-principal coll-variable-debt-token-trait)))
								(let ((next-coll-reserve (try! (get-reserve collateral-asset)))
											(next-coll-liquidity-index (get liquidity-index next-coll-reserve)) ;; updated in burn-coll-tokens
											(next-coll-liquidity-rate (get liquidity-rate next-coll-reserve))
										)
										(try! (transfer-fee-to-treasury liquidated-user coll-m-token-trait liquidation-fee-amount last-updated-timestamp coll-liquidity-index coll-liquidity-rate)) 
										(ok true)
)))))))

;; transfers c tokens from the liquidated user to the liquidator
(define-private (liquidate-m-tokens (coll-asset principal) (actual-coll-to-liquidate uint) (liquidated-user principal) (liquidator principal) (coll-m-token-trait <ct-trait>)) 
(let (
			(liquidated-user-coll-asset-balances (get-user-assets-balance coll-asset liquidated-user))
			(liquidator-coll-asset-balances (get-user-assets-balance coll-asset liquidator))
			(liquidated-user-coll (get scaled-collateral-balance liquidated-user-coll-asset-balances))
			(liquidator-coll (get scaled-collateral-balance liquidator-coll-asset-balances))
		)
		(as-contract (try! (contract-call? coll-m-token-trait liquidate-m-token actual-coll-to-liquidate liquidated-user liquidator)))
		(try! (set-user-assets-balance liquidated-user coll-asset (merge liquidated-user-coll-asset-balances {scaled-collateral-balance: (- liquidated-user-coll actual-coll-to-liquidate)})))
		(ok (try! (set-user-assets-balance liquidator coll-asset (merge liquidator-coll-asset-balances {scaled-collateral-balance: (+ liquidator-coll actual-coll-to-liquidate)}))))
))

;; update states, rates, burns coll-a-token and transfers underlying asset
(define-private (burn-coll-tokens (liquidated-user principal)
															(liquidator principal)
															(actual-collateral-to-liquidate uint)
															(asset principal)
															(reserve (tuple (id uint) (liquidity-index uint) (liquidity-rate uint) (variable-borrow-index uint) (variable-borrow-rate uint) (last-updated-timestamp uint) (m-token-principal principal) (stable-debt-token-principal principal) (variable-debt-token-principal principal) (interest-rate-strategy-principal principal) (accrued-to-treasury uint)))
															(current-liquidity-rate uint)
															(last-updated-timestamp uint)
															(current-variable-borrow-rate uint)
															(curr-scaled-variable-debt uint)
															(current-variable-borrow-index uint)
															(current-liquidity-index uint)
															(reserve-factor uint)
															(asset-trait <ft-trait>)
															(m-token-trait <ct-trait>)
															(m-token-principal principal)
															(variable-debt-token-trait <vdt-trait>)
															)

	(let ((liquidity-added u0)
				(liquidity-taken actual-collateral-to-liquidate)
				(total-stable-debt u0)
				(average-stable-borrow-rate u0)
				(total-variable-debt (mul-unit curr-scaled-variable-debt current-variable-borrow-index))
				)
		(try! (update-state asset reserve current-liquidity-rate last-updated-timestamp current-variable-borrow-rate curr-scaled-variable-debt current-variable-borrow-index current-liquidity-index reserve-factor))
		(let ((next-reserve (try! (get-reserve asset)))
					(next-liquidity-index (get liquidity-index next-reserve))
					(next-variable-borrow-index (get variable-borrow-index next-reserve))
				) 
		(as-contract (try! (contract-call? m-token-trait set-normalized-income next-liquidity-index)))
		(as-contract (try! (contract-call? variable-debt-token-trait set-normalized-debt next-variable-borrow-index)))
		(try! (update-interest-rates next-reserve curr-scaled-variable-debt liquidity-added liquidity-taken reserve-factor asset asset-trait m-token-principal))
		(let ((updated-reserve (try! (get-reserve asset)))
					(liquidated-user-coll-asset-balances (get-user-assets-balance asset liquidated-user))
					(curr-user-coll (get scaled-collateral-balance liquidated-user-coll-asset-balances))
				)
				(as-contract (try! (contract-call? m-token-trait burn-scaled actual-collateral-to-liquidate liquidated-user next-liquidity-index)))
				(as-contract (try! (contract-call? m-token-trait transfer-underlying-token actual-collateral-to-liquidate liquidator asset-trait)))
				(ok (try! (set-user-assets-balance liquidated-user asset (merge liquidated-user-coll-asset-balances {scaled-collateral-balance: (- curr-user-coll actual-collateral-to-liquidate)}))))
))))

(define-private (get-final-actual-coll-and-debt-to-liquidate (debt-asset principal) 
																								(coll-price uint) ;; u6123_00000000
																								(debt-price uint) ;; u1_02000000
																								(coll-decimals uint) 
																								(debt-decimals uint) 
																								(debt-to-cover uint) 
																								(user-coll-balance uint) ;; u_10000000 ;; 0.1 btc
																								(liquidation-bonus uint)  
																								(liquidation-fee uint) 
																								(variable-debt uint) 
																								(debt-variable-debt-token-trait <vdt-trait>)) 
	(let ((coll-asset-unit (pow u10 coll-decimals)) ;; u1_00000000
				(debt-asset-unit (pow u10 debt-decimals)) ;; u1_00000000
				(debt-to-cover-value (/ (* debt-to-cover debt-price) debt-asset-unit))
				(base-coll-to-liquidate (/ (* debt-to-cover-value coll-asset-unit) coll-price)) ;; u_66634002 ;; 0.6 btc
				(max-coll-to-liquidate (percent-mul base-coll-to-liquidate liquidation-bonus)) ;; u69965702
				(coll-and-debt-amounts (get-collateral-and-debt-liquidation-amounts max-coll-to-liquidate user-coll-balance debt-to-cover coll-price debt-price coll-asset-unit debt-asset-unit liquidation-bonus))
				(coll-amount (get coll-amount coll-and-debt-amounts))
				(debt-amount-needed (get debt-amount-needed coll-and-debt-amounts)) ;; 
				(liquidation-fee-amount (get-liquidation-protocol-fee-amount liquidation-fee liquidation-bonus coll-amount))
			)
			(print {FUNCTION:"get-final-actual-coll-and-debt-to-liquidate", base-coll-value-to-liquidate:base-coll-to-liquidate,max-coll-to-liquidate:max-coll-to-liquidate, liquidation-fee-amount:liquidation-fee-amount, 
				coll-and-debt-amounts:coll-and-debt-amounts, debt-asset-unit:debt-asset-unit, coll-price:coll-price, coll-asset-unit:coll-asset-unit, debt-to-cover:debt-to-cover, debt-price:debt-price,debt-to-cover-value:debt-to-cover-value,
				user-coll-balance:user-coll-balance })
			{coll-amount: (- coll-amount liquidation-fee-amount),
				debt-amount: debt-amount-needed,
				liquidation-fee-amount: liquidation-fee-amount}
))

;; Burns the debt tokens of the user up to the amount being repaid by the liquidator.
(define-private (burn-debt-tokens (liquidated-user principal) (debt-asset principal) (variable-debt uint) (actual-debt-to-liquidate uint) (debt-variable-debt-token-trait <vdt-trait>))
	(let ((amount-to-burn (if (>= variable-debt actual-debt-to-liquidate) actual-debt-to-liquidate variable-debt))
			(liquidated-user-debt-asset-balances (get-user-assets-balance debt-asset liquidated-user))
			(curr-user-debt (get scaled-debt-balance liquidated-user-debt-asset-balances))
	)
		(as-contract (try! (contract-call? debt-variable-debt-token-trait burn actual-debt-to-liquidate liquidated-user)))
		(ok (try! (set-user-assets-balance liquidated-user debt-asset (merge liquidated-user-debt-asset-balances {scaled-debt-balance: (- curr-user-debt actual-debt-to-liquidate)}))))
))

(define-private (get-collateral-and-debt-liquidation-amounts (max-coll-to-liquidate uint) (user-coll-balance uint) (debt-to-cover uint) (coll-price uint) (debt-price uint) (coll-asset-unit uint) (debt-asset-unit uint) (liquidation-bonus uint)) 
	(if (> max-coll-to-liquidate user-coll-balance)
			 {coll-amount: user-coll-balance,
				debt-amount-needed: (percent-div (/ (* coll-price user-coll-balance debt-asset-unit) (* debt-price coll-asset-unit)) liquidation-bonus)
				}
			{coll-amount: max-coll-to-liquidate,
				debt-amount-needed: debt-to-cover
				}
))


(define-private (get-liquidation-protocol-fee-amount (liquidation-fee uint) (liquidation-bonus uint) (coll-amount uint))
	(if (is-eq liquidation-fee u0)
			u0
			(let ((bonus-collateral (- coll-amount (percent-div coll-amount liquidation-bonus)))) ;; collateral that should go to the liquidator
					(percent-mul bonus-collateral liquidation-fee)
)))

(define-private (check-liquidation-call (coll-asset principal) (debt-asset principal) (health-factor uint) (stable-debt uint) (variable-debt uint)) 
(let ((coll-config (try! (get-reserve-config coll-asset)))
		(coll-is-active (get is-active coll-config))
		(coll-is-paused (get is-paused coll-config))
		(debt-config (try! (get-reserve-config debt-asset)))
		(debt-is-active (get is-active debt-config))
		(debt-is-paused (get is-paused debt-config))
		(total-debt (+ stable-debt variable-debt))
		)
		(asserts! (and debt-is-active coll-is-active) ERR-RESERVE-INACTIVE)
		(asserts! (not (or debt-is-paused coll-is-paused)) ERR-RESERVE-PAUSED)
		(asserts! (< health-factor HEALTH_FACTOR_LIQUIDATION_THRESHOLD) ERR-HEALTH-FACTOR-NOT-BELOW-THRESHOLD)
		(asserts! (> total-debt u0) ERR-NO-DEBT-FOUND)
		(ok true)
))

(define-private (get-debt (health-factor uint) (stable-debt uint) (variable-debt uint) (debt-to-cover uint)) 
	(let ((total-debt (+ stable-debt variable-debt))
				(close-factor (if (> health-factor CLOSE_FACTOR_HF_THRESHOLD) DEFAULT_LIQUIDATION_CLOSE_FACTOR MAX_LIQUIDATION_CLOSE_FACTOR))
				(max-liquidatable-debt (mul-unit close-factor total-debt))
				)
	(print {FUNCTION:"get-debt", close-factor:close-factor, total-debt:total-debt, max-liquidatable-debt:max-liquidatable-debt }) 
	(if (> debt-to-cover max-liquidatable-debt) max-liquidatable-debt debt-to-cover)
))

(define-private (transfer-fee-to-treasury (liqiudated-user principal) (coll-m-token-trait <ct-trait>) (liquidation-fee-amount uint) (last-updated-timestamp uint) (liquidity-index uint) (current-liquidity-rate uint))  
(if (> liquidation-fee-amount u0) 
	(let (
				(treasury (try! (contract-call? coll-m-token-trait get-treasury)))
				(next-liquidity-index (get-normalized-income last-updated-timestamp liquidity-index current-liquidity-rate))
				(scaled-down-liquidation-fee (/ liquidation-fee-amount liquidity-index))
				(scaled-down-user-balance (try! (contract-call? coll-m-token-trait  get-scaled-balance tx-sender))) ;; todo is this tx-sender meant for the liquidator or the liquidated user
				(sent-fee (if (> scaled-down-liquidation-fee scaled-down-user-balance) 
											(mul-unit scaled-down-user-balance liquidity-index)
											liquidation-fee-amount
			)))
		(ok (as-contract (try! (contract-call? coll-m-token-trait liquidate-m-token sent-fee liqiudated-user treasury))))
		)
		(ok false)
))

;; ----------------------------------------------------------------------------------------
;; RESERVE LOGIC
;; todo add a view similar to this. it canbt be a view because of `(var-set user user-principal)`. helpful to display user stats https://github.com/aave/aave-v3-core/blob/6d6fa53d360b43f492ff5b3c7033f95aee4f1335/contracts/protocol/libraries/logic/PoolLogic.sol#L156
;; ----------------------------------------------------------------------------------------	
(define-constant EMPTY_USER_ACCOUNT_DATA {collateral-value: u0, debt-value: u0, average-liquidation-threshold-numerator: u0, average-ltv-numerator: u0})
;; overview of activity on all pools
(define-read-only (get-user-health-metrics (who principal))
		(let (
					(health-metrics-list (map get-reserve-health-metrics-helper (get-reserves-list) (list who who who who who who who who who who who who who who who who who who who who who who who who who who who who who who) ))
				)
				(try! (fold check-err-collateral-debt-reducer health-metrics-list (ok EMPTY_USER_ACCOUNT_DATA)))
				(let ((user-account-data (unwrap-panic (fold process-and-add-collateral-debt-reducer health-metrics-list (ok EMPTY_USER_ACCOUNT_DATA))))
						(collateral (get collateral-value user-account-data))
					)
					(ok (merge user-account-data {average-liquidation-threshold: (if (is-eq collateral u0) u0 (get-percentage-a-over-b (get average-liquidation-threshold-numerator user-account-data) collateral)),
																	average-ltv: (if (is-eq collateral u0) u0 (get-percentage-a-over-b (get average-ltv-numerator user-account-data) collateral)) 
																	}
)))))

;; called on withdraw or disabling an asset as collateral
(define-public (check-health-and-ltv (who principal)) 
	(let ((user-account-data (unwrap-panic (get-user-health-metrics who)))
				(health-factor (get-health-factor user-account-data))
				)
	(asserts! (> health-factor HEALTH_FACTOR_LIQUIDATION_THRESHOLD) ERR-HEALTH-FACTOR-LESS-THAN-LIQUIDATION-THRESHOLD)
	(print {Function:"check-health-and-ltv", who: who, user-account-data:user-account-data, health-factor: health-factor})
	(ok {health-factor:health-factor, user-account-data: user-account-data })
))

;; cant use tx-sender since this function is being called by the liquidator in the liquidate function, so we'll need to pass the user address as an argument.
(define-data-var user principal 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5)

(define-read-only (get-reserve-health-metrics-helper (asset principal) (who principal))
	(let ((user-assets (get-user-assets-balance asset who))
				(scaled-collateral-balance (get scaled-collateral-balance user-assets))
				(scaled-debt-balance (get scaled-debt-balance user-assets))
				(reserve (try! (get-reserve asset)))
				(config (try! (get-reserve-config asset)))
				(last-updated-timestamp  (get last-updated-timestamp reserve))
				(liquidity-index (get liquidity-index reserve))
				(variable-borrow-index (get variable-borrow-index reserve))
				(current-liquidity-rate (get liquidity-rate reserve))
				(current-variable-borrow-rate (get variable-borrow-rate reserve))
				(ltv (get ltv config))
				(liquidation-threshold (get liquidation-threshold config))
				(decimals (get decimals config))
				(asset-price (unwrap-panic (get-asset-price asset)))
				(normalized-income (get-normalized-income last-updated-timestamp liquidity-index current-liquidity-rate))
				(collateral (if (is-asset-used-as-collateral who asset) (get-user-collateral-in-base-currency asset-price scaled-collateral-balance normalized-income decimals) u0))
				(debt (get-user-debt-in-base-currency asset-price scaled-debt-balance last-updated-timestamp variable-borrow-index current-variable-borrow-rate decimals))
			)
			(print {FUNCTION:"get-reserve-health-metrics-helperwhoo", who:who, scaled-debt-balance:scaled-debt-balance, asset:asset, user-assets:user-assets, collateral:collateral, asset-price:asset-price, scaled-collateral-balance:scaled-collateral-balance,
					normalized-income:normalized-income, liquidation-threshold:liquidation-threshold, ltv:ltv, threshold-numerator:(percent-mul collateral liquidation-threshold), is-asset-used-as-collateral:(is-asset-used-as-collateral who asset), 
					ltv-numerator:(percent-mul collateral ltv), debt:debt})
			(ok {collateral-value: collateral,
						debt-value: debt, 
						average-liquidation-threshold-numerator: (percent-mul collateral liquidation-threshold),
						average-ltv-numerator: (percent-mul collateral ltv)
					}
)))

(define-read-only (process-and-add-collateral-debt-reducer (a (response (tuple (collateral-value uint) (debt-value uint) (average-liquidation-threshold-numerator uint) (average-ltv-numerator uint)) uint)) (b (response (tuple (collateral-value uint) (debt-value uint) (average-liquidation-threshold-numerator uint) (average-ltv-numerator uint)) uint)) )
	(let ((new-health-metrics (unwrap-panic a))
				(new-collateral (get collateral-value new-health-metrics))
				(new-debt (get debt-value new-health-metrics))
				(new-liq-thresh (get average-liquidation-threshold-numerator new-health-metrics))
				(new-ltv (get average-ltv-numerator new-health-metrics))
				(old-health-metrics (unwrap-panic b))
				(old-collateral (get collateral-value old-health-metrics))
				(old-debt (get debt-value old-health-metrics))
				(old-liq-thresh (get average-liquidation-threshold-numerator old-health-metrics))
				(old-ltv (get average-ltv-numerator old-health-metrics))
			)
			(ok {collateral-value: (+ new-collateral old-collateral), debt-value: (+ new-debt old-debt),  average-liquidation-threshold-numerator: (+ new-liq-thresh old-liq-thresh), average-ltv-numerator:(+ new-ltv old-ltv)})
))

(define-private (get-user-debt-in-base-currency (asset-price uint) (scaled-debt-balance uint) (last-updated-timestamp uint) (variable-borrow-index uint) (current-variable-borrow-rate uint) (decimals uint)) 
	(let ((variable-debt  (get-user-variable-debt scaled-debt-balance last-updated-timestamp variable-borrow-index current-variable-borrow-rate))
				(stable-debt u0) ;; todo
				(total-debt (+ variable-debt stable-debt))
	)
	(print {FUNCTION:"get-user-debt-in-base-currency", variable-debt:variable-debt})
	(/ (* total-debt asset-price) (pow u10 decimals))
))
				
;; this is equivalent to  (total-variable-debt (mul-unit next-scaled-variable-debt next-variable-borrow-index)) since get-normalized-debt is just returning the variable borrow index
(define-private (get-user-variable-debt (scaled-debt uint) (last-updated-timestamp uint) (variable-borrow-index uint) (current-variable-borrow-rate uint)) 
	(if (is-eq scaled-debt u0)
			scaled-debt
			(let (
				  (latest-variable-borrow-index (get-normalized-debt last-updated-timestamp variable-borrow-index current-variable-borrow-rate))
					)
				(print {FUNCTION:"get-user-variable-debt", variable-borrow-index:latest-variable-borrow-index, scaled-debt:scaled-debt, last-updated-timestamp:last-updated-timestamp })
				(mul-unit scaled-debt latest-variable-borrow-index)
)))

(define-private (get-user-collateral-in-base-currency (asset-price uint) (scaled-collateral-balance uint) (normalized-income uint) (decimals uint)) 
	(/ (* (mul-unit scaled-collateral-balance normalized-income) asset-price) (pow u10 decimals))
)

;; gets or calculates the latest liquidity-index
(define-read-only (get-normalized-income (last-updated-timestamp uint) (liquidity-index uint) (current-liquidity-rate uint)) 
	(if (is-eq last-updated-timestamp (get-current-time))
		liquidity-index
		(mul-unit (calc-linear-interest current-liquidity-rate last-updated-timestamp) liquidity-index)
	)
)

;; gets or calculates the latest variable-borrow-index
(define-read-only (get-normalized-debt (last-updated-timestamp uint) (variable-borrow-index uint) (current-variable-borrow-rate uint)) 
	(if (is-eq last-updated-timestamp (get-current-time))
		variable-borrow-index
		(mul-unit (calc-compound-interest current-variable-borrow-rate last-updated-timestamp) variable-borrow-index)
	)
)

;; basically collateral over debt. https://docs.aave.com/risk/asset-risk/risk-parameters#health-factor
(define-private (get-health-factor (user-account-data (tuple (collateral-value uint) (debt-value uint) (average-liquidation-threshold-numerator uint) (average-ltv-numerator uint) (average-liquidation-threshold uint) (average-ltv uint))))
	(let (
				(collateral (get collateral-value user-account-data))
				(debt (get debt-value user-account-data))	;; todo interesting here that aave is getting the debt prior to the borrow and not adding the borrowed amount. I kept implemented it like that for consistency
				(avg-liq-thresh (get average-liquidation-threshold user-account-data)) 
			)
	 (if (is-eq debt u0) 
	 			MAX_UINT
				(div-unit-up (percent-mul collateral avg-liq-thresh) debt)
)))
;; ----------------------------------------------------------------------------------------
;; UPDATE STATES
;; ----------------------------------------------------------------------------------------	
(define-private (update-state (asset principal)
															(reserve (tuple (id uint) (liquidity-index uint) (liquidity-rate uint) (variable-borrow-index uint) (variable-borrow-rate uint) (last-updated-timestamp uint) (m-token-principal principal) (stable-debt-token-principal principal) (variable-debt-token-principal principal) (interest-rate-strategy-principal principal) (accrued-to-treasury uint)))
															(current-liquidity-rate uint)
															(last-updated-timestamp uint)
															(current-variable-borrow-rate uint)
															(curr-scaled-variable-debt uint)
															(current-variable-borrow-index uint)
															(current-liquidity-index uint)
															(reserve-factor uint)
															) 
	(let ((reserve-updated-indexes (update-indexes reserve current-liquidity-rate last-updated-timestamp curr-scaled-variable-debt current-variable-borrow-rate current-variable-borrow-index current-liquidity-index))
				(new-reserve (accrue-to-treasury reserve-updated-indexes curr-scaled-variable-debt current-variable-borrow-index reserve-factor) )
				)  
		(print {FUNCTION:"update-state", asset:asset, last-updated-timestamp: last-updated-timestamp, block-height: block-height, curr-timestamp: (get-current-time) })
		(ok (try! (set-reserve asset (merge new-reserve {last-updated-timestamp: (get-current-time)})))
)))

(define-private (update-indexes (reserve (tuple (id uint) (liquidity-index uint) (liquidity-rate uint) (variable-borrow-index uint) (variable-borrow-rate uint) (last-updated-timestamp uint) (m-token-principal principal) (stable-debt-token-principal principal) (variable-debt-token-principal principal) (interest-rate-strategy-principal principal) (accrued-to-treasury uint)))
																(current-liquidity-rate uint)
																(last-updated-timestamp uint)
																(curr-scaled-variable-debt uint)
																(current-variable-borrow-rate uint)
																(current-variable-borrow-index uint)
																(current-liquidity-index uint))
	(let ((reserve-updated-liquidity-index (update-liquidity-index reserve current-liquidity-rate current-liquidity-index last-updated-timestamp))
				(reserve-updated-borrow-index (update-borrow-index reserve-updated-liquidity-index curr-scaled-variable-debt current-liquidity-rate current-variable-borrow-index last-updated-timestamp))
				)  
				reserve-updated-borrow-index
))

(define-private (update-borrow-index (reserve (tuple (id uint) (liquidity-index uint) (liquidity-rate uint) (variable-borrow-index uint) (variable-borrow-rate uint) (last-updated-timestamp uint) (m-token-principal principal) (stable-debt-token-principal principal) (variable-debt-token-principal principal) (interest-rate-strategy-principal principal) (accrued-to-treasury uint)))
																			(curr-scaled-variable-debt uint)
																			(current-variable-borrow-rate uint)
																			(current-variable-borrow-index uint)
																			(last-updated-timestamp uint)) 
			(if (is-eq curr-scaled-variable-debt u0)
				reserve
				(let (
							(cumulated-variable-borrow-interest (calc-compound-interest current-variable-borrow-rate last-updated-timestamp))
							(next-variable-borrow-index (mul-unit cumulated-variable-borrow-interest current-variable-borrow-index))
							)
					(print {FUNCTION:"update-borrow-index", curr-scaled-variable-debt:curr-scaled-variable-debt, current-variable-borrow-index: current-variable-borrow-index, current-variable-borrow-rate: current-variable-borrow-rate,next-variable-borrow-index:next-variable-borrow-index,cumulated-variable-borrow-interest:cumulated-variable-borrow-interest, last-updated-timestamp: last-updated-timestamp })
					(merge reserve {variable-borrow-index: next-variable-borrow-index})
)))

(define-private (update-liquidity-index (reserve (tuple (id uint) (liquidity-index uint) (liquidity-rate uint) (variable-borrow-index uint) (variable-borrow-rate uint) (last-updated-timestamp uint) (m-token-principal principal) (stable-debt-token-principal principal) (variable-debt-token-principal principal) (interest-rate-strategy-principal principal) (accrued-to-treasury uint)))
																				(current-liquidity-rate uint)
																				(current-liquidity-index uint)
																				(last-updated-timestamp uint)) 
	(if (is-eq current-liquidity-rate u0)
		reserve
		(let (
				(cumulated-liquidity-interest (calc-linear-interest current-liquidity-rate last-updated-timestamp))
				(next-liquidity-index (mul-unit cumulated-liquidity-interest current-liquidity-index))
			)
			(print {FUNCTION:"update-liquidity-index", current-liquidity-rate: current-liquidity-rate, cumulated-liquidity-interest: cumulated-liquidity-interest, next-liquidity-index: "replace", last-updated-timestamp: last-updated-timestamp, current-liquidity-index: current-liquidity-index })
			(merge reserve {liquidity-index: next-liquidity-index})
	))
)

;; updates the map which is later read in a seperate function to move funds to treasury, executeMintToTreasury.
;; TODO2 why does it have to be called on every tx? cant I decouple that to a seperate func and use the increases in reserves supply or borrows to get the treasury amount?
;; TODO3 maybe it makes more sense to take fees from borrowers and not suppliers? as we dont want to dis-incentivize the side that is adding liquidity to the platform.
(define-private (accrue-to-treasury (reserve (tuple (id uint) (liquidity-index uint) (liquidity-rate uint) (variable-borrow-index uint) (variable-borrow-rate uint) (last-updated-timestamp uint) (m-token-principal principal) (stable-debt-token-principal principal) (variable-debt-token-principal principal) (interest-rate-strategy-principal principal) (accrued-to-treasury uint)))
																(curr-scaled-variable-debt uint)
																(current-variable-borrow-index uint)
																(reserve-factor uint)
																)
	(begin 
		(if (is-eq reserve-factor u0) 
			reserve
			(let ((next-variable-borrow-index (get variable-borrow-index reserve)) ;; updates in update-indexes
					(next-liquidity-index (get liquidity-index reserve)) ;; updates in update-indexes
					(prev-total-variable-debt (mul-unit curr-scaled-variable-debt current-variable-borrow-index))
					(curr-total-variable-debt (mul-unit curr-scaled-variable-debt next-variable-borrow-index))
					(total-debt-accrued (if (> curr-total-variable-debt prev-total-variable-debt) (- curr-total-variable-debt prev-total-variable-debt) (- prev-total-variable-debt curr-total-variable-debt)))
					(amount (percent-mul total-debt-accrued reserve-factor))
				)
				(print {FUNCTION:"init-accrue-to-treasury", reserve-factor:reserve-factor, amount:amount, total-debt-accrued:total-debt-accrued, curr-total-variable-debt:curr-total-variable-debt, prev-total-variable-debt:prev-total-variable-debt, next-liquidity-index:next-liquidity-index, next-variable-borrow-index:next-variable-borrow-index, curr-scaled-variable-debt:curr-scaled-variable-debt, current-variable-borrow-index:current-variable-borrow-index})
				(if (is-eq amount u0) 
					reserve
					(let (
						(prev-accrued-to-treasury (get accrued-to-treasury reserve))
						(scaled-amount (div-unit-up amount next-liquidity-index))
						(next-accrued-to-treasury (if (> curr-total-variable-debt prev-total-variable-debt) (+ prev-accrued-to-treasury scaled-amount) (- prev-accrued-to-treasury scaled-amount)) )
						) 
						(print {FUNCTION:"accrue-to-treasury", next-accrued-to-treasury: next-accrued-to-treasury})
						(merge reserve {accrued-to-treasury: next-accrued-to-treasury})
))))))

;; todo could be more gas efficient here. multipying by index then dividing by it in mint-scaled 
(define-public (mint-to-treasury (asset principal) (m-token-trait <ct-trait>)) 
	(let ((reserve (try! (get-reserve asset)))
				(accrued-to-treasury (get accrued-to-treasury reserve))
				(last-updated-timestamp (get last-updated-timestamp reserve))
				(liquidity-index (get liquidity-index reserve))
				(current-liquidity-rate (get liquidity-rate reserve))
				(normalized-income (get-normalized-income last-updated-timestamp liquidity-index current-liquidity-rate))
				(amount-to-mint (mul-unit normalized-income accrued-to-treasury))
				(treasury-address (try! (contract-call? m-token-trait get-treasury)))
				)
		;; (asserts! (or (is-ok (check-is-approved)) (is-ok (check-is-owner))) ERR-NOT-AUTHORIZED)
	(as-contract (try! (contract-call? m-token-trait mint-scaled amount-to-mint treasury-address liquidity-index)))
	(print {Function:"mint-to-treasury", amount-to-mint: amount-to-mint, normalized-income: normalized-income})
	(set-reserve asset (merge reserve {accrued-to-treasury: u0}))
	)
)

;; taylor series expansion https://en.wikipedia.org/wiki/Binomial_approximation
(define-private (calc-compound-interest (rate uint) (last-updated-timestamp uint))
	(let ((exp (- (get-current-time) last-updated-timestamp))
				(exp-minus-1 (- exp u1))
				(exp-minus-2 (- exp u2))
				(base-power-2 (/ (mul-unit rate rate) (* SECONDS-PER-YEAR SECONDS-PER-YEAR)))
				(base-power-3 (/ (mul-unit base-power-2 rate) SECONDS-PER-YEAR))
				(first-term (/ (* rate exp) SECONDS-PER-YEAR))
				(second-term (/ (* (* exp exp-minus-1) base-power-2) u2))
				(third-term (/ (* (* (* exp exp-minus-1) exp-minus-2) base-power-3) u6))
				)
			(print {FUNCTION:"calc-compound-interest", rate:rate, first-term:first-term, second-term:second-term, third-term:third-term, exp:exp, base-power-2:base-power-2, base-power-3:base-power-3  })
			(+ UNIT first-term second-term third-term)
))

(define-private (calc-linear-interest (current-liquidity-rate uint) (last-updated-timestamp uint)) 
	(let ((result (* current-liquidity-rate (- (get-current-time) last-updated-timestamp))))
		(print {FUNCTION:"calc-linear-interest", result:result, time-delta: (- (get-current-time) last-updated-timestamp), curr-time:(get-current-time), last-updated-ts:last-updated-timestamp})
		(+ UNIT (/ result SECONDS-PER-YEAR))
))
;; ----------------------------------------------------------------------------------------
;; INTEREST RATE SRTATEGIES 
;; ----------------------------------------------------------------------------------------
;; default startegy

;; https://etherscan.io/address/0x694d4cFdaeE639239df949b6E24Ff8576A00d1f2#readContract
;; Base variable borrow rate when usage rate = 0. 
(define-data-var base-variable-borrow-rate-var uint u0)
(define-public (set-base-variable-borrow-rate (new-rate uint))
	(begin (asserts! (or (is-ok (check-is-approved)) (is-ok (check-is-owner))) ERR-NOT-AUTHORIZED) (ok (var-set base-variable-borrow-rate-var new-rate)))
)
(define-data-var variable-rate-slope-1-var uint (* u4 UNIT))
(define-public (set-variable-rate-slope-1 (new-slope uint))
	(begin (asserts! (or (is-ok (check-is-approved)) (is-ok (check-is-owner))) ERR-NOT-AUTHORIZED) (ok (var-set variable-rate-slope-1-var new-slope)))
)
(define-data-var variable-rate-slope-2-var uint (* u8 UNIT))
(define-public (set-variable-rate-slope-2 (new-slope uint))
	(begin (asserts! (or (is-ok (check-is-approved)) (is-ok (check-is-owner))) ERR-NOT-AUTHORIZED) (ok (var-set variable-rate-slope-2-var new-slope)))
)
(define-data-var max-excess-usage-ratio-var uint u0)
(define-public (set-max-excess-usage-ratio (new-ratio uint))
	(begin (asserts! (or (is-ok (check-is-approved)) (is-ok (check-is-owner))) ERR-NOT-AUTHORIZED) (ok (var-set max-excess-usage-ratio-var new-ratio)))
)

;; TODO BUG `supply` should send the curr-scaled-variable-debt because the scaled-variable-debt is not changing after a deposit but every other method should pass the next-scaled-variable-debt as argument which is not the case
(define-private (update-interest-rates (reserve (tuple (id uint) (liquidity-index uint) (liquidity-rate uint) (variable-borrow-index uint) (variable-borrow-rate uint) (last-updated-timestamp uint) (m-token-principal principal) (stable-debt-token-principal principal) (variable-debt-token-principal principal) (interest-rate-strategy-principal principal) (accrued-to-treasury uint)))
																				(next-scaled-variable-debt uint)
																				(liquidity-added uint)
																				(liquidity-taken uint)
																				(reserve-factor uint) 
																				(asset principal)
																				(asset-trait <ft-trait>)
																				(m-token-principal principal)) 
	(let ((next-liquidity-index (get liquidity-index reserve))
				(next-variable-borrow-index (get variable-borrow-index reserve))
				(total-variable-debt (mul-unit next-scaled-variable-debt next-variable-borrow-index))
				(total-debt total-variable-debt)
				(base-variable-borrow-rate (var-get base-variable-borrow-rate-var))
				)
				(print {FUNCTION: "init-update-interest-rates", liquidity-taken:liquidity-taken, liquidity-added:liquidity-added, total-debt:total-debt, total-variable-debt:total-variable-debt, next-variable-borrow-index:next-variable-borrow-index, next-scaled-variable-debt:next-scaled-variable-debt})
				(let (
							(asset-amount-held (if (is-eq total-debt u0) u0 (try! (contract-call? asset-trait get-balance m-token-principal))))
							(available-liquidity (if (is-eq total-debt u0) u0 (+ (- asset-amount-held liquidity-taken) liquidity-added)))
							(available-liquidity-plus-debt (if (is-eq total-debt u0) u0 (+ available-liquidity total-debt)))
							(borrow-usage-ratio (if (is-eq total-debt u0) u0 (get-percentage-a-over-b total-debt available-liquidity-plus-debt)))
							(current-average-stable-borrow-rate u0) ;; todo
							(variable-rate-slope-1 (var-get variable-rate-slope-1-var))
							(variable-rate-slope-2 (var-get variable-rate-slope-2-var))
							(stable-and-variable-borrow-rates (get-new-stable-and-variable-borrow-rates borrow-usage-ratio base-variable-borrow-rate variable-rate-slope-1 variable-rate-slope-2))
							(current-stable-borrow-rate (unwrap-panic (element-at? stable-and-variable-borrow-rates u0)))
							(variable-borrow-rate (unwrap-panic (element-at? stable-and-variable-borrow-rates u1)))
							(overall-borrow-rate (get-overall-borrow-rate u0 total-variable-debt variable-borrow-rate current-average-stable-borrow-rate))
							(current-liquidity-rate (percent-mul (percent-mul overall-borrow-rate borrow-usage-ratio) (- PERCENTAGE_FACTOR reserve-factor)))
						)
						(print {FUNCTION: "update-interest-rates", asset:asset, asset-amount-held:asset-amount-held, total-debt:total-debt, liquidity-rate: current-liquidity-rate , variable-borrow-rate: variable-borrow-rate, current-stable-borrow-rate: current-stable-borrow-rate,  borrow-usage-ratio:borrow-usage-ratio, available-liquidity-plus-debt:available-liquidity-plus-debt, available-liquidity:available-liquidity, overall-borrow-rate:overall-borrow-rate})
						(set-reserve asset (merge reserve {liquidity-rate: current-liquidity-rate, variable-borrow-rate: variable-borrow-rate}))
)))

(define-private (get-new-stable-and-variable-borrow-rates (borrow-usage-ratio uint) (base-variable-borrow-rate uint) (variable-rate-slope-1 uint) (variable-rate-slope-2 uint)) 
	(if (> borrow-usage-ratio OPTIMAL_USAGE_RATIO) 
			(let (
						(max-excess-usage-ratio (- PERCENTAGE_FACTOR OPTIMAL_USAGE_RATIO))
						(excess-borrow-usage-ratio (/ (- borrow-usage-ratio OPTIMAL_USAGE_RATIO) max-excess-usage-ratio))
						(stable-borrow-rate u0)
						(variable-borrow-rate (+ base-variable-borrow-rate (+ variable-rate-slope-1 (mul-unit variable-rate-slope-2 excess-borrow-usage-ratio))) )
						)
				(list stable-borrow-rate variable-borrow-rate)		
			)
			(let (
						(stable-borrow-rate u0)
						(variable-borrow-rate (+ base-variable-borrow-rate (percent-div (percent-mul variable-rate-slope-1 borrow-usage-ratio) OPTIMAL_USAGE_RATIO)))
					)
				(print {FUNCTION:"get-new-stable-and-variable-borrow-rates", variable-borrow-rate:variable-borrow-rate, base-variable-borrow-rate:base-variable-borrow-rate, variable-rate-slope-1:variable-rate-slope-1,  borrow-usage-ratio:borrow-usage-ratio, OPTIMAL_USAGE_RATIO:OPTIMAL_USAGE_RATIO})
				(list stable-borrow-rate variable-borrow-rate)		
)))

;; calculates the weighted average between stable rate and borrow rate
(define-private (get-overall-borrow-rate (total-stable-debt uint) (total-variable-debt uint) (current-variable-borrow-rate uint) (current-average-stable-borrow-rate uint)) 
	(let ((total-debt (+ total-stable-debt total-variable-debt))) 
			(if (is-eq total-debt u0)
					u0
					(let ((weighted-variable-rate (mul-unit total-variable-debt current-variable-borrow-rate)) 
								(weighted-stable-rate (mul-unit total-stable-debt current-average-stable-borrow-rate)) 
								)
								(print {FUNCTION:"get-overall-borrow-rate",total-debt:total-debt, total-variable-debt:total-variable-debt, current-variable-borrow-rate:current-variable-borrow-rate, weighted-variable-rate:weighted-variable-rate })
								;; overall-borrow-rate
								(div-unit-up (+ weighted-stable-rate weighted-variable-rate) total-debt)
))))
;; ----------------------------------------------------------------------------------------
;; TODO handle PRICES. Ideally prices should be set in another contract that any function needeing the prices can call get-price(asset) on. The price contractmshould be interraproable through a principal var and a trait argument
;; ----------------------------------------------------------------------------------------	
(define-map prices { asset: principal } { price: uint })

(define-private (get-asset-price (asset principal))
	(get price (map-get? prices {asset: asset}))
)

;; price must be include decimals, meaning if bitcoin is 64k, the asset-price is 64,000_00000000
(define-public (set-asset-price (asset principal) (new-price uint)) 
(begin 
		(asserts! (or (is-ok (check-is-approved)) (is-ok (check-is-owner))) ERR-NOT-AUTHORIZED)
		(ok (map-set prices {asset: asset} {price: new-price}))
))


;; ----------------------------------------------------------------------------------------
;; -------------------------------------------UTILS----------------------------------------
;; ----------------------------------------------------------------------------------------	
(define-read-only (check-err-collateral-debt-reducer (result (response (tuple (collateral-value uint) (debt-value uint) (average-liquidation-threshold-numerator uint) (average-ltv-numerator uint)) uint)) (prior (response (tuple (collateral-value uint) (debt-value uint) (average-liquidation-threshold-numerator uint) (average-ltv-numerator uint)) uint)))
    (contract-call? .utils check-err-collateral-debt-reducer result prior)
)

;; todo this is a real problem but non-blocker. find something better. read this convo https://discord.com/channels/621759717756370964/623217767356694547/1225719071555715072
(define-private (get-current-time)
    (contract-call? .utils get-current-time )
)
;; ----------------------------------------------------------------------------------------
;; AUTH 
;; ----------------------------------------------------------------------------------------
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
;; ----------------------------------------------------------------------------------------
;; MATH 
;; ----------------------------------------------------------------------------------------
;; TODO VIP make sure you're only using mul-unit and mul-unit when ray multiplactions are needed
;; TODO2 do i need to create similar operations for ray div? the math doesnt need it as 1.1/2.2 = 110000/220000 but it cant be said for multiplications
;; (define-private (div-unit-down (a uint) (b uint)) 
;; 	(unwrap-panic (contract-call? .utils div-unit-down a b))
;; )

(define-private (div-unit-up (a uint) (b uint)) 
	(contract-call? .utils div-unit-up a b)
)

(define-private (mul-unit (a uint) (b uint)) 
	(contract-call? .utils mul-unit a b)
)

(define-private (ceil-div (a uint) (b uint))
	(contract-call? .utils ceil-div a b)
)

;; @param a the number you want to get a percentage of
;; @param perc the percentage expressed in bps. ex 10.01% is u1001
(define-private (percent-mul (a uint) (perc uint)) 
	(contract-call? .utils percent-mul a perc)
)

(define-private (percent-div (a uint) (perc uint)) 
	(unwrap-panic (contract-call? .utils percent-div a perc))
)

(define-private (get-percentage-a-over-b (a uint) (b uint)) 
	(contract-call? .utils get-percentage-a-over-b a b)
)