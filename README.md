# MemePool

Contracts that handle *liquid* borrowing and lending any sip-10 token, but focusing on memecoins.  
- The `pool.clar` contract serves as the main entrypoint & contains all the public functions needed to interact with the platform. 
- The `vault.clar` contains the reserve and user configurations, including the interest rates, liquidation thresholds etc ...
- This is a *liquid*  borrowing and lending platform, thus when a user borrows or supplies tokens, he receives tokens in return that represent a share of his positions. When a user supplies, he receives an `m-token`, borrowers receives a non-transferable `variable-debt-token`. Both tokens are implement the `m-token-trait` rebasing and have modified sip-10 functions to account for the change in amounts held due to the accrued interest rates.

## TLDR 
- A user can deposit a collateral by calling `supply`. 
- By supplying his collateral, the users gains : 
	1- The ability to borrow an amount, of any meme or stable coin, thus leveraging his positions.
	2- Receive an APY yield in the same token deposited. The APY comes from the borrowers who pay an interest rate to borrow the user's collateral.
- If the borrowed amount value (in usd terms) increases above the collateral value * liquidation threshold, we say then the health factor of the position is below 1 and is elligible for liquidation by a different user (liquidator).

## Overview

More formal docs are coming soon, but :

### Public Functions
`supply`, `withdraw`, `repay`, `borrow`, `liquidate` all perform an action and behave in a similar manner.
- The current state of the of the reserve (pool) is fetched using `get-reserve`.
- The indexes of the reserve is updated in `update-state`. You can read more about indexes, and how rates are calculated [here](https://medium.com/@kinaumov/back-to-the-basics-compound-aave-436a1887ad94)
- Checks and validates the current parameters (`check-supply`, `check-withdraw` etc...)
- Updates the interest rates in `interest-rates` using the indexes in `next-reserve` which were previously calculated & set in the reserve map in `update-state` call

### Risks

Borrowing and lending platform carry a huge risk, even more so when illiquid, volatile memecoins are listed as collateral. the configurations such as supply/borrow caps need ot be very conservative, liquidation factor and liquidator bonus need to favor the liquidator to compensate for highly volatile markets.