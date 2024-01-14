import * as fs from 'fs';
import * as yaml from 'yaml';
import * as child_process from "child_process";
import { tmpdir } from "os";
import { Address, CurrencySymbol, NonLovelaceAssetId, NonLovelaceValueMap, TokenName, TxOutRef } from './cardano.js';
import { toCardanoAssetId } from './blockfrost.js';

const mainnetProjectId = "mainnetAJ7cf0wCJKB4Ma1UydKstGDiLrFxtdmz";

// To fetch
// curl -H "project_id: mainnetAJ7cf0wCJKB4Ma1UydKstGDiLrFxtdmz" https://cardano-mainnet.blockfrost.io/api/v0/txs/{txId}/utxos | jq > 1st-prize.json

export type Prize = "1stPrize" | "2ndPrize" | "3rdPrize";

type ClonningInfo = {
  origTxOutRef: TxOutRef;
  holderAddr: Address;
  valueMap: NonLovelaceValueMap;
  prize: Prize;
}

// This function accepts JSON received from blockfrost and returns a Map of currency to (asset name, quantity)
const fetchPrizeTokensInfo = async (txOutRef:TxOutRef): Promise<NonLovelaceValueMap> => {
  // Fetch from blockfrost using ajax (fetch function) tx out ref info
  const response = await fetch(`https://cardano-mainnet.blockfrost.io/api/v0/txs/${txOutRef.txId}/utxos`, {
    headers: {
      "project_id": mainnetProjectId
    }
  });
  const json = await response.json();
  const result:NonLovelaceValueMap = new Map<NonLovelaceAssetId, bigint>();
  const assets = json.outputs[txOutRef.index].amount;
  assets.forEach((asset:any) => {
    const assetId = toCardanoAssetId(asset.unit);
    if(assetId == "lovelace") return;
    const quantity = BigInt(asset.quantity) || 10n;
    result.set(assetId, quantity);
  });
  return result;
}

const mkCloningInfo = async (txOutRef:TxOutRef, prize: Prize, holderDir:string): Promise<ClonningInfo> => {
  const holderAddr = fs.readFileSync(`${holderDir}/addr_test.pay`, "utf8").trim();
  const valueMap = await fetchPrizeTokensInfo(txOutRef);
  return {
    origTxOutRef: txOutRef,
    holderAddr,
    valueMap,
    prize,
  };
}

// In every wallet dir we have:
// * addr_test.pay
// * addr_test.skey
export const firstPrizeHolderDir = "./wallets/1st-prize-initial-holder/";
export const secondPrizeHolderDir = "./wallets/2nd-prize-initial-holder/";
export const thirdPrizeHolderDir = "./wallets/3rd-prize-initial-holder/";

const firstPrizeMainnetUTxO:TxOutRef = {
  txId: "8d239e97fa5b7e8cee00d7f160aa4f51adaef17386f883de4d3d9e391bbf7bfc",
  index: 1
};

const secondPrizeMainnetUTxO:TxOutRef = {
  txId: "6c01c234aa14bf292c41875c0437cc8657b13570b1b349b64578fd0a212710ab",
  index: 1
};

const thirdPrizeMainnetUTxO:TxOutRef = {
  txId: "143fe58ddd63197f7a7b1f48d6c2a616ce8955873c9c633ea0d39b8a81fef97f",
  index: 1
};


// This is an example result:
//
// # 160a880d9fc45380737cb7e57ff859763230aab28b3ef6a84007bfcc
// #   4d495241: 125000
// - Mint:
//     nickname: 160a880d9fc45380737cb7e57ff859763230aab28b3ef6a84007bfcc
//     minLovelace: 2000000
//     tokenDistribution:
//       - [addr_xvk12mvghppvv3077nc7am4asqzj6fszy3hv6urev8r8ghtxfly3rrmc933sl6vsjyltpnuuf047kwgvsd6g72ffzf3xm2z6wj66jrq00vggvkvvh, "\00x4d495241", 125000]
//
// # 1d779e04b2ee9d64acb9e747129c10760976ad651696d7e406c39acb
// #   4d696c6573746f6e6531434e43416c6131353837: 1
// #   4d696c6573746f6e6532434e43416c6131363238: 1
// #   4d696c6573746f6e6533434e43416c6131323633: 1
// #   4d696c6573746f6e6534434e43416c6130393030: 1
// #   4d696c6573746f6e6535434e43416c6131393537: 1
// #   4d696c6573746f6e6536434e43416c6131353337: 1
// #   4d696c6573746f6e6537434e43416c6131353234: 1
// - Mint:
//     nickname: 1d779e04b2ee9d64acb9e747129c10760976ad651696d7e406c39acb
//     minLovelace: 2000000
//     tokenDistribution:
//       - [ addr_xvk12mvghppvv3077nc7am4asqzj6fszy3hv6urev8r8ghtxfly3rrmc933sl6vsjyltpnuuf047kwgvsd6g72ffzf3xm2z6wj66jrq00vggvkvvh, "\00x4d696c6573746f6e6531434e43416c6131353837", 1]
//       - [ addr_xvk12mvghppvv3077nc7am4asqzj6fszy3hv6urev8r8ghtxfly3rrmc933sl6vsjyltpnuuf047kwgvsd6g72ffzf3xm2z6wj66jrq00vggvkvvh, "\00x4d696c6573746f6e6532434e43416c6131363238", 1]
//       - [ addr_xvk12mvghppvv3077nc7am4asqzj6fszy3hv6urev8r8ghtxfly3rrmc933sl6vsjyltpnuuf047kwgvsd6g72ffzf3xm2z6wj66jrq00vggvkvvh, "\00x4d696c6573746f6e6533434e43416c6131323633", 1]
//       - [ addr_xvk12mvghppvv3077nc7am4asqzj6fszy3hv6urev8r8ghtxfly3rrmc933sl6vsjyltpnuuf047kwgvsd6g72ffzf3xm2z6wj66jrq00vggvkvvh, "\00x4d696c6573746f6e6534434e43416c6130393030", 1]
//       - [ addr_xvk12mvghppvv3077nc7am4asqzj6fszy3hv6urev8r8ghtxfly3rrmc933sl6vsjyltpnuuf047kwgvsd6g72ffzf3xm2z6wj66jrq00vggvkvvh, "\00x4d696c6573746f6e6535434e43416c6131393537", 1]
//       - [ addr_xvk12mvghppvv3077nc7am4asqzj6fszy3hv6urev8r8ghtxfly3rrmc933sl6vsjyltpnuuf047kwgvsd6g72ffzf3xm2z6wj66jrq00vggvkvvh, "\00x4d696c6573746f6e6536434e43416c6131353337", 1]
//       - [ addr_xvk12mvghppvv3077nc7am4asqzj6fszy3hv6urev8r8ghtxfly3rrmc933sl6vsjyltpnuuf047kwgvsd6g72ffzf3xm2z6wj66jrq00vggvkvvh, "\00x4d696c6573746f6e6537434e43416c6131353234", 1]
//
// What we want to do is:
//
// * we want to create a minting script
//    * for every currency in the file we want to create a separate Mint entry in the script
//    * the mint entry sholud use destination prize holder address
// * the final minting script should be something like:
//
type MintOperation = {
  nickname: string;
  minLovelace: bigint;
  tokenDistribution: [Address, string, bigint][];
}

// So the original value map is a map of asset id to quantity.
//
// export type CurrencySymbol = string;
//
// export type TokenName = string;
//
// export type NonLovelaceAssetId = Readonly<{
//   currencySymbol: CurrencySymbol;
//   tokenName: TokenName
// }>;
//
// export type NonLovelaceValueMap = Map<NonLovelaceAssetId, bigint>;
//
// We want to turn it into nested map:

export type NonLovelaceValueNestedMap = Map<CurrencySymbol, Map<TokenName, bigint>>;

const toNonLovelaceValueNestedMap = (valueMap: NonLovelaceValueMap): NonLovelaceValueNestedMap => {
  const result = new Map<CurrencySymbol, Map<TokenName, bigint>>();
  valueMap.forEach((quantity: bigint, assetId: NonLovelaceAssetId) => {
    const tokenQuantity:bigint = result.get(assetId.currencySymbol)?.get(assetId.tokenName) || BigInt(0);
    const tokenMap = result.get(assetId.currencySymbol) || new Map<TokenName, bigint>();
    tokenMap.set(assetId.tokenName, tokenQuantity + quantity);
    result.set(assetId.currencySymbol, tokenMap);
  });
  return result;
}

const mkMintingScript = (cloningInfo:ClonningInfo): MintOperation[] => {
  const result:MintOperation[] = [];
  const nestedValueMap = toNonLovelaceValueNestedMap(cloningInfo.valueMap);
  nestedValueMap.forEach((tokenMap: Map<TokenName, bigint>, currencySymbol: CurrencySymbol) => {
    const tokenDistribution:[Address, string, bigint][] = [];
    tokenMap.forEach((quantity: bigint, tokenName: TokenName) => {
      // Token name should be prefixed with \00x
      const tokenNamePrefixed = "\x000x" + tokenName;
      tokenDistribution.push([cloningInfo.holderAddr, tokenNamePrefixed, quantity]);
    });
    result.push({
      nickname: currencySymbol,
      minLovelace: 2000000n,
      tokenDistribution
    });
  });
  return result;
}


export const mkMintingScriptYaml = async (): Promise<string> => {
  const PrizesCloningInfo = {
    firstPrize: await mkCloningInfo(firstPrizeMainnetUTxO, "1stPrize", firstPrizeHolderDir),
    secondPrize: await mkCloningInfo(secondPrizeMainnetUTxO, "2ndPrize", secondPrizeHolderDir),
    thirdPrize: await mkCloningInfo(thirdPrizeMainnetUTxO, "3rdPrize", thirdPrizeHolderDir),
  };

  const mintingScript = [
    ...mkMintingScript(PrizesCloningInfo.firstPrize),
    ...mkMintingScript(PrizesCloningInfo.secondPrize),
    ...mkMintingScript(PrizesCloningInfo.thirdPrize)
  ];

  // Now we have to wrap every operation in `{ Mint: ... }` and stringify it to yaml
  const mintingScriptYaml = yaml.stringify({
    testName: "Clone prizes",
    operations:
      mintingScript.map((mintOperation:MintOperation) => {
      return { Mint: mintOperation };
    })
  });

  const dir = tmpdir();
  const mintingScriptPath = `${dir}/minting-script.yaml`;
  console.log("Writing minting script to", mintingScriptPath);
  // Let's write yaml to a ./minting/clone-prizes.yaml
  fs.writeFileSync(mintingScriptPath, mintingScriptYaml);
  return mintingScriptPath;
}

type CloningReportFile = string;

export const clonePrizes = async (): Promise<CloningReportFile> => {
  console.log("KSDJFLSKDJFLSKDFJSLDKFJSLDKFJ");
  const walletsDir = "./wallets";
  console.log("Creating minting script");
  const mintingScriptPath = await mkMintingScriptYaml();
  const faucetVKey = fs.readFileSync(`${walletsDir}/faucet/addr_test.pay`, "utf8").trim();
  const faucetSKeyFile = `${walletsDir}/faucet/addr.skey`;
  const cardanoNodeSocketPath = process.env.CARDANO_NODE_SOCKET_PATH;
  if(!cardanoNodeSocketPath) {
    throw new Error("CARDANO_NODE_SOCKET_PATH is not defined");
  }
  const cloningReportFile = `./cloning-report.json`;
  const cmd = "cabal";
  const args = [
    "run",
    "exe:marlowe-cli",
    "--",
    "--babbage-era",
    "test",
    "--testnet-magic",
    "1",
    "--max-concurrent-runners",
    "8",
    "--socket-path",
    cardanoNodeSocketPath,
    "--faucet-skey-file",
    faucetSKeyFile,
    "--faucet-address",
    faucetVKey,
    "--write-to-json-file",
    cloningReportFile,
    "--max-retries",
    "3",
    "--stream-json",
    mintingScriptPath
  ];
  console.log("Running cloning test script");
  const result = child_process.spawnSync(cmd, args);
  if (result.status !== 0) {
    console.log(result.stderr.toString());
    console.log("Failed to run cloning test script");
    console.log("Report file:", cloningReportFile);
    process.exit(1);
  }
  return cloningReportFile;
}

