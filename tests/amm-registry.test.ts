import { beforeAll, beforeEach, describe, expect, it } from "vitest";
import { addApproved, contractDeployer, createPool, logResponse, mintSFT } from "./helpers";
import { Cl } from "@stacks/transactions";

describe("create pool", () => {
	beforeEach(()=>{
		mintSFT();
    addApproved(contractDeployer);
	})
  it("creates pool", () => {
    const createPoolResp = createPool();
    console.log('createPoolResp.events', createPoolResp.events)
    logResponse("init-response", createPoolResp);

    expect(createPoolResp.result).toBeOk(Cl.bool(true));
  });
});
