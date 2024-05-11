import { Cl, deserializePostCondition } from "@stacks/transactions"
import { initSimnet, tx } from '@hirosystems/clarinet-sdk';
import { describe, expect, it, beforeEach} from "vitest"

import { prettyEvents, ERR_SUPPLY_CAP_EXCEEDED, ERR_BORROW_CAP_EXCEEDED, poolContract, sbtcContract, sBtcCTokenContract, sBtcVariableDebtContract, contractDeployer, beforeSBTCSupply, address1, address2, wUsdContract, wUsdVariableDebtContract, wUsdBorrowAmount, wUsdSupplyCap, beforeWUSDBorrow, sBtcCollateralAmount, ERR_COLLATERAL_TOO_LOW, beforeSupply, sBtcBorrowCap, sBtcSupplyCap, supply, wUsdCTokenContract, wUsdBorrowCap, borrow, addPoolToApproved, initBorrowedReserveAndAddSupply, ERR_HEALTH_FACTOR_LESS_THAN_LIQUIDATION_THRESHOLD, logResponse, setAssetPrice, ERR_HEALTH_FACTOR_NOT_BELOW_THRESHOLD, vaultContract } from "./helpers"

describe("init-reserve", ()=> {
	it("should add a reserve", ()=>{
		const initResponse = simnet.callPublicFn(
      vaultContract,
			"init-reserve",
			[
				Cl.principal(sbtcContract), 
				Cl.principal(sbtcContract), 
				Cl.principal(sBtcCTokenContract),
				Cl.principal(sBtcVariableDebtContract),
				Cl.principal(sBtcVariableDebtContract),
			],
      contractDeployer
		)
		const reserveData = simnet.getMapEntry(
      vaultContract,
      "reserve-data",
      Cl.tuple({ "asset": Cl.principal(sbtcContract) })
    )
		console.log(
			"initResponse",
			Cl.prettyPrint(initResponse.result),
			Cl.prettyPrint(reserveData)
		)
		// @ts-ignore
		expect(reserveData.value.data["m-token-principal"]).toStrictEqual(Cl.principal(sBtcCTokenContract))
	})
})

describe("supply", ()=>{
	beforeEach(()=> {
		beforeSupply(sbtcContract, sBtcVariableDebtContract, sBtcCTokenContract, address1, sBtcCollateralAmount, sBtcBorrowCap, sBtcSupplyCap, Cl.uint(61462_23000000) )
	})
	it("should update last timestamp in reserve data", ()=>{
		const supplyResponse = simnet.callPublicFn(
      poolContract,
			"supply",
			[
				Cl.principal(sbtcContract), 
				sBtcCollateralAmount,
				Cl.principal(sbtcContract), 
				Cl.principal(sBtcVariableDebtContract),
				Cl.principal(sBtcCTokenContract),
			],
      address1
		)
	console.log("supplyResponse",Cl.prettyPrint(supplyResponse.result),prettyEvents(supplyResponse.events, "check-supply"))
	const reserveData = simnet.getMapEntry(
		vaultContract,
		"reserve-data",
		Cl.tuple({ "asset": Cl.principal(sbtcContract) })	
	)
	// @ts-ignore
	expect(Number(reserveData.value.data["last-updated-timestamp"].value)).toBeGreaterThan(0)
	})

	it("should fail with supply cap exceeded", ()=> {
			const supplyResponse = simnet.callPublicFn(
				poolContract,
				"supply",
				[
					Cl.principal(sbtcContract), 
					Cl.uint(Number(sBtcCollateralAmount.value) + Number(sBtcSupplyCap.value)*10**8),
					Cl.principal(sbtcContract), 
					Cl.principal(sBtcVariableDebtContract),
					Cl.principal(sBtcCTokenContract),
				],
				address1
			)
		// console.log("supplyResponse",Cl.prettyPrint(supplyResponse.result),prettyEvents(supplyResponse.events, "check-supply"))
		expect(supplyResponse.result).toBeErr(Cl.uint(ERR_SUPPLY_CAP_EXCEEDED))
	})

	it("should send the collateral token, meaning the asset, from the user to the m-token contract", ()=> {
		const supplyResponse = simnet.callPublicFn(
			poolContract,
			"supply",
			[
				Cl.principal(sbtcContract), 
				Cl.uint(Number(sBtcCollateralAmount.value)),
				Cl.principal(sbtcContract), 
				Cl.principal(sBtcVariableDebtContract),
				Cl.principal(sBtcCTokenContract),
			],
			address1
		)
	// console.log("supplyResponse", Cl.prettyPrint(supplyResponse.result), prettyEvents(supplyResponse.events))
	expect(
		supplyResponse.events.filter(
			e => e.event === "ft_transfer_event"
		)[0]?.data.recipient
	).toBe(sBtcCTokenContract)

	expect(
		supplyResponse.events.filter(
			e => e.event === "ft_transfer_event"
		)[0]?.data.amount
	).toBe(sBtcCollateralAmount.value.toString())
	})

	it("should set the amount in user-config map", ()=>{
		const supplyResponse = simnet.callPublicFn(
			poolContract,
			"supply",
			[
				Cl.principal(sbtcContract), 
				Cl.uint(Number(sBtcCollateralAmount.value)),
				Cl.principal(sbtcContract), 
				Cl.principal(sBtcVariableDebtContract),
				Cl.principal(sBtcCTokenContract),
			],
			address1
		)
	console.log("supplyResponse", Cl.prettyPrint(supplyResponse.result), prettyEvents(supplyResponse.events))
	expect(
		supplyResponse.events.filter(
			e => e.event === "ft_mint_event"
		)[0]?.data.recipient
	).toBe(address1)
	expect(
		supplyResponse.events.filter(
			e => e.event === "ft_mint_event"
		)[0]?.data.amount
	).toBe(sBtcCollateralAmount.value.toString())	

	expect(
		supplyResponse.events.filter(
			e => e.event === "ft_mint_event"
		)[0]?.data.asset_identifier
	).toBe(sBtcCTokenContract + "::sbtc-m-token")	
})
	it("should send m-tokens to the borrower", ()=> {
		const supplyResponse = simnet.callPublicFn(
			poolContract,
			"supply",
			[
				Cl.principal(sbtcContract), 
				Cl.uint(Number(sBtcCollateralAmount.value)),
				Cl.principal(sbtcContract), 
				Cl.principal(sBtcVariableDebtContract),
				Cl.principal(sBtcCTokenContract),
			],
			address1
		)
		// console.log("supplyResponse", prettyEvents(supplyResponse.events))
		const event = supplyResponse.events.find(
				e => e.event === "ft_mint_event"
			)
		expect(event?.data.asset_identifier).toBe(sBtcCTokenContract+"::sbtc-m-token")
		expect(event?.data.amount).toBe(sBtcCollateralAmount.value.toString())
	})
})

describe("borrow", ()=> {
	beforeEach(()=> {
		addPoolToApproved()
		beforeSupply(sbtcContract, sBtcVariableDebtContract, sBtcCTokenContract, address1, sBtcCollateralAmount, sBtcBorrowCap, sBtcSupplyCap, Cl.uint(61462_23000000) )
		supply(sbtcContract, sBtcCollateralAmount, sBtcVariableDebtContract, sBtcCTokenContract, address1)
		initBorrowedReserveAndAddSupply(wUsdContract, wUsdVariableDebtContract, wUsdCTokenContract, address1, wUsdBorrowAmount, wUsdBorrowCap, wUsdSupplyCap, Cl.uint(1_02000000))
	})
	
	it("should throw borrow cap exceeded", ()=> {
		const borrowResponse = simnet.callPublicFn(poolContract, "borrow",[
			Cl.principal(wUsdContract),
			Cl.uint(Number(wUsdBorrowAmount.value) + Number(wUsdSupplyCap.value)*10**8),
			Cl.principal(wUsdContract),
			Cl.principal(wUsdCTokenContract),
			Cl.principal(wUsdVariableDebtContract) 
		],
		address1
	)
	// console.log(prettyEvents(borrowResponse.events))
	expect(borrowResponse.result).toBeErr(Cl.uint(ERR_BORROW_CAP_EXCEEDED))
	})
	
	it("should throw collateral too low", ()=>{
		const borrowResponse = simnet.callPublicFn(poolContract, "borrow",[
			Cl.principal(wUsdContract),
			Cl.uint(15_000_00000000), // 15k with 6k$ in btc collateral
			Cl.principal(wUsdContract),
			Cl.principal(wUsdCTokenContract),
			Cl.principal(wUsdVariableDebtContract) 
		],
		address1
	)
	expect(borrowResponse.result).toBeErr(Cl.uint(ERR_COLLATERAL_TOO_LOW))
	})

	it("should update interest rates", ()=> {

		beforeSupply(sbtcContract, sBtcVariableDebtContract, sBtcCTokenContract, address1, sBtcCollateralAmount, sBtcBorrowCap, sBtcSupplyCap, Cl.uint(61462_23000000) )
		supply(sbtcContract, sBtcCollateralAmount, sBtcVariableDebtContract, sBtcCTokenContract, address1)
		initBorrowedReserveAndAddSupply(wUsdContract, wUsdVariableDebtContract, wUsdCTokenContract, address1, wUsdBorrowAmount, wUsdBorrowCap, wUsdSupplyCap, Cl.uint(1_02000000))

		const borrowUSDResponse = simnet.callPublicFn(poolContract, "borrow",[
				Cl.principal(wUsdContract),
				wUsdBorrowAmount,
				Cl.principal(wUsdContract),
				Cl.principal(wUsdCTokenContract),
				Cl.principal(wUsdVariableDebtContract) 
			],
			address1
		)

		const reserveData = simnet.getMapEntry(
      vaultContract,
      "reserve-data",
      Cl.tuple({ "asset": Cl.principal(wUsdContract)})
    )
		logResponse("borrowUSDResponse", borrowUSDResponse)
		// @ts-ignore
		expect(reserveData.value.data["variable-borrow-rate"].value).toBeGreaterThan(BigInt(100000000000000000))
	}) 
	
	it("should mint variable debt tokens to the borrower", ()=> {
		const borrowResponse = simnet.callPublicFn(poolContract, "borrow",[
				Cl.principal(wUsdContract),
				wUsdBorrowAmount,
				Cl.principal(wUsdContract),
				Cl.principal(wUsdCTokenContract),
				Cl.principal(wUsdVariableDebtContract) 
			],
			address1
		)
	// console.log("borrowResponse", prettyEvents(borrowResponse.events))
		const event = borrowResponse.events.find(
				e => e.event === "ft_mint_event"
			)
		expect(event?.data.asset_identifier).toBe(wUsdVariableDebtContract+"::variable-wusd-debt-token")
		expect(event?.data.amount).toBe(wUsdBorrowAmount.value.toString())
	})

	it("should transfer the borrowed token to the borrower", ()=> {
		const usdBorrowResponse = simnet.callPublicFn(poolContract, "borrow",[
				Cl.principal(wUsdContract),
				wUsdBorrowAmount,
				Cl.principal(wUsdContract),
				Cl.principal(wUsdCTokenContract),
				Cl.principal(wUsdVariableDebtContract) 
			],
			address1
		)
		console.log("usdBorrowResponse", Cl.prettyPrint(usdBorrowResponse.result), prettyEvents(usdBorrowResponse.events))
		const event = usdBorrowResponse.events.find(
				e => e.event === "ft_transfer_event"
			)
		expect(event?.data.asset_identifier).toBe(wUsdContract+"::wusd")
		expect(event?.data.amount).toBe(wUsdBorrowAmount.value.toString())
	})

	it("should throw collateral too low", ()=> {
		const usdBorrowResponse = borrow(wUsdContract, wUsdCTokenContract, wUsdVariableDebtContract, Cl.uint(100000_00000000), address1)		
		expect(usdBorrowResponse.result).toBeErr(Cl.uint(ERR_COLLATERAL_TOO_LOW))
	})
})

describe("withdraw", ()=> {
	beforeEach(()=>{
		addPoolToApproved()
		beforeSupply(sbtcContract, sBtcVariableDebtContract, sBtcCTokenContract, address1, sBtcCollateralAmount, sBtcBorrowCap, sBtcSupplyCap, Cl.uint(61462_23000000) )
		const btcSupplyResponse = supply(sbtcContract, sBtcCollateralAmount, sBtcVariableDebtContract, sBtcCTokenContract, address1)
		initBorrowedReserveAndAddSupply(wUsdContract, wUsdVariableDebtContract, wUsdCTokenContract, address1, wUsdBorrowAmount, wUsdBorrowCap, wUsdSupplyCap, Cl.uint(1_02000000))
		const usdBorrowResponse = borrow(wUsdContract, wUsdCTokenContract, wUsdVariableDebtContract, Cl.uint(200_00000000), address1)		
		const secondUsdBorrowResponse = borrow(wUsdContract, wUsdCTokenContract, wUsdVariableDebtContract, Cl.uint(100_00000000), address1)		
	})
	it("should withdraw",()=>{
		const withdrawAmount = Cl.uint(1_000_000)
		const withdrawResponse = simnet.callPublicFn(poolContract, "withdraw",[
				Cl.principal(sbtcContract),
				withdrawAmount, // 0.01 btc
				Cl.principal(sbtcContract),
				Cl.principal(sBtcVariableDebtContract),
				Cl.principal(sBtcCTokenContract) 
			],
			address1
		)
		expect(withdrawResponse.result).toStrictEqual(Cl.ok(withdrawAmount))
		// shoud burn c tokens
		const burnEvent = withdrawResponse.events.find(e => e.event === "ft_burn_event")
		expect(burnEvent?.data.asset_identifier).toBe(sBtcCTokenContract+"::sbtc-m-token")
		expect(burnEvent?.data.amount).toBe(withdrawAmount.value.toString())
		// should transfer underlying asset to the user
		const transferEvent = withdrawResponse.events.find(e => e.event === "ft_transfer_event")
		expect(transferEvent?.data.asset_identifier).toBe(sbtcContract+"::sbtc")
		expect(transferEvent?.data.amount).toBe(withdrawAmount.value.toString())
	})

	it("should throw health factor less than liquidation threshold",()=>{
		const withdrawAmount = Cl.uint(100_000_000)

		const withdrawResponse = simnet.callPublicFn(poolContract, "withdraw",[
			Cl.principal(sbtcContract),
			withdrawAmount, 
			Cl.principal(sbtcContract),
			Cl.principal(sBtcVariableDebtContract),
			Cl.principal(sBtcCTokenContract) 
		],
		address1
	)
	expect(withdrawResponse.result).toBeErr(Cl.uint(ERR_HEALTH_FACTOR_LESS_THAN_LIQUIDATION_THRESHOLD))
	})
})


describe("repay", ()=> {
	beforeEach(()=>{
		addPoolToApproved()
		beforeSupply(sbtcContract, sBtcVariableDebtContract, sBtcCTokenContract, address1, sBtcCollateralAmount, sBtcBorrowCap, sBtcSupplyCap, Cl.uint(61462_23000000) )
		const suppResponse = supply(sbtcContract, sBtcCollateralAmount, sBtcVariableDebtContract, sBtcCTokenContract, address1)
		logResponse("suppResponse", suppResponse)
		initBorrowedReserveAndAddSupply(wUsdContract, wUsdVariableDebtContract, wUsdCTokenContract, address1, wUsdBorrowAmount, wUsdBorrowCap, wUsdSupplyCap, Cl.uint(1_02000000))
		const bottowResponse = borrow(wUsdContract, wUsdCTokenContract, wUsdVariableDebtContract, Cl.uint(250_00000000), address1)		

	})

	it("should repay the usd borrow with underlying asset", ()=> {
		const amount = 100_00000000
		const repayResponse = simnet.callPublicFn(poolContract, "repay",[
			Cl.principal(wUsdContract),
			Cl.uint(amount), 
			Cl.principal(wUsdContract),
			Cl.principal(wUsdCTokenContract),
			Cl.principal(wUsdVariableDebtContract),
			Cl.bool(false) 
		],
		address1
	)
	logResponse('repayResponse', repayResponse)
	// burns the variable debt token
	const burnEvent = repayResponse.events.find(
				e => e.event === "ft_burn_event"
			)
		expect(burnEvent?.data.asset_identifier).toBe(wUsdVariableDebtContract+"::variable-wusd-debt-token")
		expect(burnEvent?.data.amount).toBe(amount.toString())
	
	// transfer the underlying asset back to the reserve
	const transferEvent = repayResponse.events.find(
					e => e.event === "ft_transfer_event"
				)
		expect(transferEvent?.data.asset_identifier.split("::")[0]).toBe(wUsdContract)
		expect(transferEvent?.data.amount).toBe(amount.toString())
		}) 
		// todo
		it("should repay the usd borrow with burning ctoken", ()=> {
			}) 
	}) 

	describe("liquidate", ()=>{
		const liquidator = address2
		beforeEach(()=>{
			addPoolToApproved()
			beforeSupply(sbtcContract, sBtcVariableDebtContract, sBtcCTokenContract, address1, sBtcCollateralAmount, sBtcBorrowCap, sBtcSupplyCap, Cl.uint(61462_23000000) )
			const supplyResponse = supply(sbtcContract, sBtcCollateralAmount, sBtcVariableDebtContract, sBtcCTokenContract, address1)
			initBorrowedReserveAndAddSupply(wUsdContract, wUsdVariableDebtContract, wUsdCTokenContract, address1, wUsdBorrowAmount, wUsdBorrowCap, wUsdSupplyCap, Cl.uint(1_02000000))
			const borrowResponse = borrow(wUsdContract, wUsdCTokenContract, wUsdVariableDebtContract, Cl.uint(4000_00000000), address1)		
			logResponse("Supply response", supplyResponse)
			logResponse("Borrow response", borrowResponse)
		})
		it("should throw health factor not below threshold", ()=>{
			setAssetPrice(sbtcContract, Cl.uint(61234_23000000))
			const liquidateResponse = simnet.callPublicFn(poolContract, "liquidate",[
				Cl.principal(address1),
				Cl.principal(sbtcContract),
				Cl.principal(sbtcContract),
				Cl.principal(sBtcCTokenContract),
				Cl.principal(sBtcVariableDebtContract),
				Cl.principal(wUsdContract),
				Cl.principal(wUsdContract),
				Cl.principal(wUsdCTokenContract),
				Cl.principal(wUsdVariableDebtContract),
				Cl.uint(60000_00000000),
				Cl.bool(false)
			],
			address2
		)
		expect(liquidateResponse.result).toBeErr(Cl.uint(ERR_HEALTH_FACTOR_NOT_BELOW_THRESHOLD))
		})

		it("should liquidate", ()=>{
			setAssetPrice(sbtcContract, Cl.uint(6123_00000000))
			const liquidateResponse = simnet.callPublicFn(poolContract, "liquidate",[
				Cl.principal(address1),
				Cl.principal(sbtcContract),
				Cl.principal(sbtcContract),
				Cl.principal(sBtcCTokenContract),
				Cl.principal(sBtcVariableDebtContract),
				Cl.principal(wUsdContract),
				Cl.principal(wUsdContract),
				Cl.principal(wUsdCTokenContract),
				Cl.principal(wUsdVariableDebtContract),
				Cl.uint(99999_00000000),
				Cl.bool(false)
			],
			liquidator
		)
		logResponse("liquidation response", liquidateResponse)
		const burnEvents = liquidateResponse.events.filter(
				e => e.event === "ft_burn_event"
			)
			const transferEvents = liquidateResponse.events.filter(
				e => e.event === "ft_transfer_event"
			)
		expect(burnEvents[0]?.data.asset_identifier).toBe(wUsdVariableDebtContract+"::variable-wusd-debt-token")
		expect(burnEvents[1]?.data.asset_identifier).toBe(sBtcCTokenContract+"::sbtc-m-token")
		expect(transferEvents[0]?.data.asset_identifier).toBe(sbtcContract+"::sbtc")
		expect(transferEvents[0]?.data.recipient).toBe(liquidator)
	})
	})
