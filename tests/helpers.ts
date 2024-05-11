import { Cl } from "@stacks/transactions"
import {UIntCV} from "@stacks/transactions"

export const ERR_NOT_AUTHORIZED = 1000
export const ERR_TOO_MANY_OUTCOME_SLOTS = 2001
export const ERR_TOO_FEW_OUTCOME_SLOTS = 2002
export const ERR_CONDITION_ALREADY_PREPARED = 2003
export const ERR_CONDITION_NOT_PREPARED = 2004
export const ERR_WRONG_SLOTS_INPUT = 2005
export const ERR_CONDITION_UNKNOWN = 2006
export const ERR_TOO_FEW_PARTITIONS = 2007
export const ERR_COLLECTIONID_NOT_FOUND = 2008
export const ERR_CANT_SERIALIZE_COLLATERAL = 2009
export const ERR_INDEX_SETS_NOT_VALID = 2010
export const ERR_INDEX_SETS_NOT_DISJOINT = 2011


export const ERR_YOU_POOR=420
// const ERR_NOT_AUTHORIZED = 2000;
export const ERR_NOT_AUTHORIZED_TOKEN = 2001;
export const ERR_RESERVE_NOT_EXIST = 2002;
export const ERR_RESERVE_INACTIVE = 2003;
export const ERR_RESERVE_PAUSED = 2004;
export const ERR_RESERVE_FROZEN = 2005;
export const ERR_BORROWING_NOT_ENABLED = 2006;
export const ERR_CONFIG_NOT_SET = 2007;
export const ERR_INVALID_AMOUNT = 2008;
export const ERR_SUPPLY_CAP_EXCEEDED = 2009;
export const ERR_BORROW_CAP_EXCEEDED = 2010;
export const ERR_HEALTH_FACTOR_LESS_THAN_LIQUIDATION_THRESHOLD = 2011;
export const ERR_BORROWING_DISABLED = 2012;
export const ERR_NO_COLLATERAL_FOUND = 2013;
export const ERR_LTV_INVALID = 2014;
export const ERR_PERCENTAGE_INVALID = 2015;
export const ERR_COLLATERAL_TOO_LOW = 2016;
export const ERR_HEALTH_FACTOR_NOT_BELOW_THRESHOLD = 2017;
export const ERR_NO_DEBT_FOUND = 2018;

export const prettyEvents = (events: any, functionName='') => {
  console.log({ rawEvents: events })
  return events.filter(
    (e:any) => {
      if(!functionName) return true
      return JSON.stringify(e.data.value)?.includes(functionName)
    }
  ).map(
    // @ts_ignore
    (e: any) =>
      e.event == "print_event"
        ? {
            value: Cl.prettyPrint(e.data.value),
            contract_identifier: e.data.contract_identifier
          }
        : e
  )
}

export const logResponse = (name:string, resp:any, ) => {
	console.log(name, Cl.prettyPrint(resp.result), prettyEvents(resp.events))
}

export const buffFromHex = (id: string) =>
  Cl.buffer(Uint8Array.from(Buffer.from(id, "hex")))


export const accounts = simnet.getAccounts()
export const address1 = accounts.get("wallet_1")! // ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5
export const address2 = accounts.get("wallet_2")! // ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG
export const address3 = accounts.get("wallet_3")! 

export const utilsContract = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.utils"
export const vaultContract = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.vault"


export const contractDeployer = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM"
export const collateralName0 = "sbtc"
export const sbtcContract = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.sbtc"
export const sBtcCTokenContract = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.sbtc-m-token"
export const sBtcVariableDebtContract = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.variable-sbtc-debt-token"
export const sBtcCollateralAmount = Cl.uint(10_000_000) // 0.1 btc
export const sBtcBorrowAmount = Cl.uint(1_000_000) // 0.01 btc
export const sBtcBorrowCap = Cl.uint(4)
export const sBtcSupplyCap = Cl.uint(6)

export const collateralName1 = "wusd"
export const wUsdContract = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.wusd"
export const wUsdCTokenContract = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.wusd-m-token"
export const wUsdVariableDebtContract = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.variable-wusd-debt-token"
export const wUsdCollateralAmount = Cl.uint(200_000_00000000) // 200k, 8 decimals
export const wUsdBorrowAmount = Cl.uint(1_500_00000000) // 15k
export const wUsdBorrowCap = Cl.uint(300_000) // 300k
export const wUsdSupplyCap = Cl.uint(600_000)


export const poolName = "pool"
export const poolContract = contractDeployer + "." + poolName
export const PERCENTAGE_FACTOR = 10000


export const beforeSBTCSupply = ()=> {
	// sbtc, supplied by user 1
	simnet.callPublicFn(
		sbtcContract,
      "mint",
      [sBtcCollateralAmount, Cl.principal(address1)],
      contractDeployer
    )
		// approval for mints and burns
		simnet.callPublicFn(
			sBtcCTokenContract,
			"add-approved-contract",
			[Cl.principal(poolContract)],
			contractDeployer
		)

		simnet.callPublicFn(
      poolContract,
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

		simnet.callPublicFn(
			poolContract,
			"set-configuration",
			[
				Cl.principal(sbtcContract),
				Cl.tuple({
					"ltv": Cl.uint(0.8*PERCENTAGE_FACTOR),
					"liquidation-threshold": Cl.uint(0.85*PERCENTAGE_FACTOR),
					"liquidation-bonus": Cl.uint(1.05*PERCENTAGE_FACTOR),
					"decimals": Cl.uint(8), // 8 sats units in 1 btc
					"is-active": Cl.bool(true),
					"is-frozen": Cl.bool(false),
					"is-borrowing-enabled": Cl.bool(true),
					"is-paused": Cl.bool(false),
					"reserve-factor": Cl.uint(0.15*PERCENTAGE_FACTOR),
					"borrow-cap": sBtcBorrowCap,
					"supply-cap": sBtcSupplyCap,
					"liquidation-fee": Cl.uint(0.1*PERCENTAGE_FACTOR)
				}),
			],
			contractDeployer
		)
	}

	export const beforeWUSDBorrow = ()=> { 
	simnet.callPublicFn(
		wUsdContract,
      "mint",
      [wUsdCollateralAmount, Cl.principal(address2)],
      contractDeployer
    )
		simnet.callPublicFn(
			wUsdVariableDebtContract,
			"add-approved-contract",
			[Cl.principal(poolContract)],
			contractDeployer
		)
		simnet.callPublicFn(
			wUsdCTokenContract,
			"add-approved-contract",
			[Cl.principal(poolContract)],
			contractDeployer
		)
		simnet.callPublicFn(
      poolContract,
			"init-reserve",
			[
				Cl.principal(wUsdContract), 
				Cl.principal(wUsdContract), 
				Cl.principal(wUsdCTokenContract),
				Cl.principal(wUsdVariableDebtContract),
				Cl.principal(wUsdVariableDebtContract),
			],
      contractDeployer
		)

		simnet.callPublicFn(
			poolContract,
			"set-configuration",
			[
				Cl.principal(wUsdContract),
				Cl.tuple({
					"ltv": Cl.uint(0.9*PERCENTAGE_FACTOR),
					"liquidation-threshold": Cl.uint(0.95*PERCENTAGE_FACTOR),
					"liquidation-bonus": Cl.uint(1.02*PERCENTAGE_FACTOR),
					"decimals": Cl.uint(8), // 8 sats units in 1 btc
					"is-active": Cl.bool(true),
					"is-frozen": Cl.bool(false),
					"is-borrowing-enabled": Cl.bool(true),
					"is-paused": Cl.bool(false),
					"reserve-factor": Cl.uint(0.15*PERCENTAGE_FACTOR),
					"borrow-cap": wUsdBorrowCap,
					"supply-cap": wUsdSupplyCap,
					"liquidation-fee": Cl.uint(0.1*PERCENTAGE_FACTOR)
				}),
			],
			contractDeployer
		)
		// setasset prices
		const setassetprice1 = simnet.callPublicFn(poolContract, "set-asset-price", [
			Cl.principal(sbtcContract), Cl.uint(61462_23000000)
		], contractDeployer)
		const setassetprice2 = simnet.callPublicFn(poolContract, "set-asset-price", [
			Cl.principal(wUsdContract), Cl.uint(1_02000000)
		], contractDeployer)


		// supply here since we're not testing wusd supply call, only wusd borrow. for user2 to borrow, someone must have deposited wusd in the pool
		simnet.callPublicFn(
			poolContract,
			"supply",
			[
				Cl.principal(wUsdContract), 
				wUsdCollateralAmount, // 100k
				Cl.principal(wUsdContract), 
				Cl.principal(wUsdVariableDebtContract),
				Cl.principal(wUsdCTokenContract),
			], 
			address2
		)
}

export const beforeSupply = (contract:string, variableDebtContract:string, cTokenContract:string, user:string, amount:UIntCV, borrowCap:UIntCV, supplyCap:UIntCV, assetPrice: UIntCV) => {
	mint(contract, amount, user);
	addPoolToApproved();
	initReserveAndConfig(contract, cTokenContract, variableDebtContract, borrowCap, supplyCap)
	setAssetPrice(contract, assetPrice)
}

export const initBorrowedReserveAndAddSupply = (contract:string, variableDebtContract:string, cTokenContract:string, user:string, amount:UIntCV, borrowCap:UIntCV, supplyCap:UIntCV, assetPrice:UIntCV) => {
	if(user == address2) throw new Error("user can't be address2 ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG")
	// supply more than what needs to be borrowd by a different user
	const collateralAmountByUser2 = Cl.uint(Number(amount.value) * 5)
	beforeSupply(contract, variableDebtContract, cTokenContract, address2, collateralAmountByUser2, borrowCap, supplyCap, assetPrice)
	const usdSupplyResponse = supply(contract, collateralAmountByUser2, variableDebtContract, cTokenContract, address2);
	// console.log("usdSupplyResponse", Cl.prettyPrint(usdSupplyResponse.result),prettyEvents(usdSupplyResponse.events))
	setAssetPrice(contract, assetPrice)

}

export const supply = (contract:string, amount:UIntCV, variableDebtContract:string, cTokenContract:string, user:string)=> {
	return simnet.callPublicFn(
		poolContract,
		"supply",
		[
			Cl.principal(contract), 
			amount, 
			Cl.principal(contract), 
			Cl.principal(variableDebtContract),
			Cl.principal(cTokenContract),
		],
		user
	)
}

export const initReserveAndConfig = (contract:string, cTokenContract:string, variableDebtContract:string, borrowCap:UIntCV, supplyCap:UIntCV)=> {
	simnet.callPublicFn(
		vaultContract,
		"init-reserve",
		[
			Cl.principal(contract), 
			Cl.principal(contract), 
			Cl.principal(cTokenContract),
			Cl.principal(variableDebtContract),
			Cl.principal(variableDebtContract),
		],
		contractDeployer
	)

	simnet.callPublicFn(
		vaultContract,
		"set-configuration",
		[
			Cl.principal(contract),
			Cl.tuple({
				"ltv": Cl.uint(0.8*PERCENTAGE_FACTOR),
				"liquidation-threshold": Cl.uint(0.85*PERCENTAGE_FACTOR),
				"liquidation-bonus": Cl.uint(1.05*PERCENTAGE_FACTOR),
				"decimals": Cl.uint(8), // 8 sats units in 1 btc
				"is-active": Cl.bool(true),
				"is-frozen": Cl.bool(false),
				"is-borrowing-enabled": Cl.bool(true),
				"is-paused": Cl.bool(false),
				"reserve-factor": Cl.uint(0.15*PERCENTAGE_FACTOR),
				"borrow-cap": borrowCap,
				"supply-cap": supplyCap,
				"liquidation-fee": Cl.uint(0.1*PERCENTAGE_FACTOR)
			}),
		],
		contractDeployer
	)
}

export const addPoolToApproved = ()=> {
	[sBtcVariableDebtContract,
		sBtcCTokenContract,
		wUsdVariableDebtContract,
		wUsdCTokenContract,
		vaultContract
	].map((contract)=> simnet.callPublicFn(
		contract,
		"add-approved-contract",
		[Cl.principal(poolContract)],
		contractDeployer
	))
}

export const setAssetPrice= (asset:string, price:UIntCV) => {
	return  simnet.callPublicFn(poolContract, "set-asset-price", [
		Cl.principal(asset), price
	], contractDeployer)
}

export const mint = (contract:string, amount:UIntCV, user:string)=> {
	return  simnet.callPublicFn(
		contract,
      "mint",
      [amount, Cl.principal(user)],
      contractDeployer
    )
}

export const borrow = (contract:string, cTokenContract:string, variableDebtContract:string, amount:UIntCV, user:string) => {
	return simnet.callPublicFn(poolContract, "borrow",[
			Cl.principal(contract),
			amount,
			Cl.principal(contract),
			Cl.principal(cTokenContract),
			Cl.principal(variableDebtContract) 
		],
		user
	)
}