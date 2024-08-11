import { beforeEach, describe, expect, it } from "vitest";
import { addApproved, address1, addToPos, contractDeployer, createPool, fpmmContract, initPool, logResponse, mintSFT, mintSip10, sip10Token, sip13Token } from "./helpers";
import { Cl } from "@stacks/transactions";

describe("add position", () => {
	const tokenYID = 0;
	const tokenXAmount = 200_000_000000;
	const tokenYAmount = 1_000_000000;

	beforeEach(()=>{
    mintSip10(sip10Token, tokenXAmount);
		mintSFT(tokenYID, tokenYAmount);
    addApproved(contractDeployer);
    addApproved(fpmmContract);
		createPool();
	})
  it("adds to position", () => {
		const tokenYID = 0
		const dX = tokenXAmount
		const maxDy = tokenYAmount
		const addToPosResp = addToPos(tokenYID, dX, maxDy)
		logResponse("addToPosResp", addToPosResp)
    expect(addToPosResp.result).toBeDefined();
  });
});

describe('get price', () => { 
	const tokenYID = 0;
	const tokenXAmount = 200_000_000000;
	const tokenYAmount = 1_000_000000;
	const dX = tokenXAmount
	const maxDy = tokenYAmount

	beforeEach(()=>{
    mintSip10(sip10Token, tokenXAmount);
		mintSFT(tokenYID, tokenYAmount);
    addApproved(contractDeployer);
    addApproved(fpmmContract);
		createPool();
		addToPos(tokenYID, dX, maxDy)
	})

	it('returns a price', ()=>{
	const priceResp = simnet.callReadOnlyFn(
		fpmmContract,
		"get-price",
		[
			Cl.principal(sip10Token), 
			Cl.principal(sip13Token), 
			Cl.uint(tokenYID),
		],
		address1
	)
	logResponse("price", priceResp)
	expect(priceResp).toBeDefined()
	})
})

describe('swap x for y', () => { 
	const tokenYID = 0;
	const tokenXAmount = 200_000_000000;
	const tokenYAmount = 1_000_000000;
	const dX = tokenXAmount
	const maxDy = tokenYAmount

	beforeEach(()=>{
    mintSip10(sip10Token, tokenXAmount);
		mintSFT(tokenYID, tokenYAmount);
    addApproved(contractDeployer);
    addApproved(fpmmContract);
		createPool();
		addToPos(tokenYID, dX, maxDy)
		initPool(tokenXAmount, tokenYAmount, sip10Token, sip13Token, tokenYID)
	})

	it('swaps', ()=>{
		mintSip10(sip10Token, tokenXAmount);

		const swapResp = simnet.callPublicFn(
		fpmmContract,
		"swap-x-for-y",
		[
			Cl.principal(sip10Token), 
			Cl.principal(sip13Token), 
			Cl.uint(tokenYID),
			Cl.uint(10_000_000000),
			Cl.none()
		],
		address1
	)
	logResponse("swapResp", swapResp)
	expect(swapResp).toBeDefined()
	})
})
