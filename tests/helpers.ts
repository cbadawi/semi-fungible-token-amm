import { Cl } from "@stacks/transactions"

// fpmm errors
export const ERR_NOT_AUTHORIZED = 1000;
export const ERR_POOL_ALREADY_EXISTS = 2000;
export const ERR_INVALID_POOL = 2001;
export const ERR_BLOCKLISTED = 2002;
export const ERR_INVALID_LIQUIDITY = 2003;
export const ERR_PERCENT_GREATER_THAN_ONE = 2004;
export const ERR_EXCEEDS_MAX_SLIPPAGE = 2005;
export const ERR_ORACLE_NOT_ENABLED = 2006;
export const ERR_ORACLE_AVERAGE_BIGGER_THAN_ONE = 2007;
export const ERR_PAUSED = 2008;
export const ERR_SWITCH_THRESHOLD_BIGGER_THAN_ONE = 2009;
export const ERR_NO_LIQUIDITY = 2010;
export const ERR_MAX_IN_RATIO = 2011;
export const ERR_MAX_OUT_RATIO = 2012;



export const UNIT = 10**18;

export const prettyEvents = (events: any, functionName='') => {
  // console.log({ rawEvents: events })
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

export const contractDeployer = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM"
export const ammRegistryContract = contractDeployer + ".amm-registry"
export const fpmmContract = contractDeployer + ".fpmm"
export const ammVaultContract = contractDeployer + ".amm-vault"
export const tokenAmmPoolContract = contractDeployer + ".token-amm-pool"
export const authContract = contractDeployer + ".auth"

export const sip10Token = contractDeployer + ".wagmi"
export const sip13Token = contractDeployer + ".semi-fungible-token"


export const mintSFT = (tokenId=0, mintAmount=1000, recipient=address1) => {
	const mintRespone = simnet.callPublicFn(
		sip13Token,
		"mint",
		[
			Cl.uint(tokenId),
			Cl.uint(mintAmount),
			Cl.principal(recipient)
		],
		contractDeployer
	)
return mintRespone
}

export const mintSip10 = (token=sip10Token, mintAmount=100000, recipient=address1) => {
	const mintRespone = simnet.callPublicFn(
		token,
		"mint",
		[
			Cl.uint(mintAmount),
			Cl.principal(recipient)
		],
		contractDeployer
	)
return mintRespone
}

export const addApproved = (address:string) => {
	return simnet.callPublicFn(
		authContract,
		"add-approved-contract",
		[Cl.principal(address)],
		contractDeployer
	)
}

export const createPool = (tokenX=sip10Token, tokenY=sip13Token, tokenYID=0)=> {
	return simnet.callPublicFn(
		ammRegistryContract,
		"create-pool",
		[
			Cl.principal(tokenX), 
			Cl.principal(tokenY), 
			Cl.uint(tokenYID),
		],
		contractDeployer
	)
}

export const initPool = (tokenXAmount:number, tokenYAmount:number, tokenX=sip10Token, tokenY=sip13Token, tokenYID=0)=> {
	// mintSFT(0,tokenYAmount, contractDeployer)
	// mintSip10(sip10Token,tokenXAmount,contractDeployer)

	// simnet.callPublicFn(
	// 	tokenX,
	// 	"transfer",
	// 	[
	// 		Cl.uint(tokenXAmount),
	// 		Cl.principal(contractDeployer),
	// 		Cl.principal(ammVaultContract),
	// 		Cl.none()
	// 	],
	// 	contractDeployer
	// )
	
	// simnet.callPublicFn(
	// 	tokenY,
	// 	"transfer",
	// 	[
	// 		Cl.uint(tokenYID),
	// 		Cl.uint(tokenYAmount),
	// 		Cl.principal(contractDeployer),
	// 		Cl.principal(ammVaultContract)
	// 	],
	// 	contractDeployer
	// )
	
	const poolData = Cl.tuple({
		"pool-id":Cl.uint(0), 
		"total-supply":Cl.uint(0), 
		"balance-x":Cl.uint(tokenXAmount), 
		"balance-y":Cl.uint(tokenYAmount), 
		"fee-rate-x":Cl.uint(0), 
		"fee-rate-y":Cl.uint(0), 
		"fee-rebate":Cl.uint(0), 
		"start-block":Cl.uint(0), 
		"end-block":Cl.uint(999999999), 
		"threshold-x":Cl.uint(0), 
		"threshold-y":Cl.uint(0), 
		"max-in-ratio":Cl.uint(80000000), 
		"max-out-ratio":Cl.uint(80000000)
	})

	simnet.callPublicFn(
		ammVaultContract,
		"set-approved-token",
		[
			Cl.principal(tokenX), 
			Cl.bool(true)
		],
		contractDeployer
	)

	simnet.callPublicFn(
		ammVaultContract,
		"set-approved-token",
		[
			Cl.principal(tokenY), 
			Cl.bool(true)
		],
		contractDeployer
	)
	
	return simnet.callPublicFn(
		ammRegistryContract,
		"update-pool",
		[
			Cl.principal(tokenX), 
			Cl.principal(tokenY), 
			Cl.uint(tokenYID),
			poolData
		],
		contractDeployer
	)
}

export const addToPos = (tokenYID:number, dX:number, maxDy:number) => {
	return simnet.callPublicFn(
		fpmmContract,
		"add-to-position-10-13",
		[
			Cl.principal(sip10Token), 
			Cl.principal(sip13Token), 
			Cl.uint(tokenYID),
			Cl.uint(dX),
			Cl.some(Cl.uint(maxDy)),
		],
		address1
	)
}