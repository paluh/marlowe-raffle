import { clonePrizes, mkMintingScriptYaml } from "./clonePrizes.js";
import { makePayoutUTxOs } from "./makePayoutUTxO.js";

const main = async () => {
  // await clonePrizes();
  await makePayoutUTxOs();
}

await main();
