import * as fs from "fs";
import * as child_process from "child_process";
import { tmpdir } from "os";
import { Address, AssetId, UTxO, ValueMap, appendValueMap, toNonLovelaceValueMap, NonLovelaceValueMap, TxOutRef, pureLovelaceUTxOs, nonPureLovelaceUTxOs, NonLovelaceAssetId, TxId } from './cardano.js';
import { addressUTxOs } from "./blockfrost.js";
import { firstPrizeHolderDir, secondPrizeHolderDir, thirdPrizeHolderDir } from "./clonePrizes.js";

// So this is information about payout script:
// PAYOUT_ADDR=$(marlowe-cli role address --mainnet)
// Script address: THIS WAS SO WRONG - THIS WAS FETCHING MAIN ADDRESS
// const scriptAddress:Address = (() => {
//   console.log("Creating payout script address");
//   const cmd = "cabal";
//   const args = [
//     "run",
//     "marlowe-cli",
//     "--",
//     "role",
//     "address",
//     "--testnet-magic",
//     "1"
//   ];
//   const result = child_process.spawnSync(cmd, args);
//   const scriptAddress = result.stdout.toString().trim();
//   if (result.status !== 0 || scriptAddress.trim() === "") {
//     console.log("Failed to create payout script address.");
//     console.log(result.stderr.toString());
//     process.exit(1);
//   }
//   return scriptAddress;
// })();

const scriptAddresses = {
  "mainnet": "addr1w8sk2cgzxg34hwa7ladenzer8k4wg2temmyj5eezm8x6nzg35nqj2",
  "testNet": "addr_test1wrsk2cgzxg34hwa7ladenzer8k4wg2temmyj5eezm8x6nzg2u8ua0"
}

const formatTxOutRef = (txOutRef: TxOutRef): string => {
  return `${txOutRef.txId}#${txOutRef.index}`;
}

const mkDatumFile = (policyId: string, tokenName: string): string => {
  // Generate system wide temporary file name
  const dir = tmpdir();
  const datumFile = `${dir}/${tokenName}.datum`;
  const cmd = "marlowe-cli";
  const args = [
    "role",
    "datum",
    "--roles-currency",
    policyId,
    "--role-name",
    tokenName,
    "--out-file",
    datumFile
  ];
  const result = child_process.spawnSync(cmd, args);
  if (result.status !== 0) {
    console.log(result.stderr.toString());
    process.exit(1);
  } else {
    console.log("cmd: ", cmd);
    console.log(args);
    console.log("stdout: ");
    console.log(result.stdout.toString());
    console.log("stderr: ");
    console.log(result.stderr.toString());
    console.log("Datum file created: ", datumFile);
  }
  return datumFile;
}

type Prize = "1stPrize" | "2ndPrize" | "3rdPrize";

type WinningTokenInfo = { policyId: string, tokenName: string, prize: Prize, datumFile: string };
type WinningTokensInfo = {
  firstPrize: WinningTokenInfo;
  secondPrize: WinningTokenInfo;
  thirdPrize: WinningTokenInfo;
};

const extractWinningTokensInfo = (mintingWinningTokensReportFile:string): WinningTokensInfo => {
  console.log("Extracting winning tokens info from: ", mintingWinningTokensReportFile);
  let fileContent:string = fs.readFileSync(mintingWinningTokensReportFile, "utf8");
  const log = JSON.parse(fileContent).results[0].logs;
  const policyId = log.find(({ label }: { label: string, info: string }) => label === "Mint-Policy-Id").info.policyId;
  console.log(log.find(({ label }: { label: string }) => label === "Mint-Distribution"));
  const tokenNames = log.find(({ label }: { label: string }) => label === "Mint-Distribution").info.distribution.map((distribution: any) => {;
    let tokenName = distribution.tokens[0][0].unTokenName;
    return tokenName;
  });
  // We expect exactly three token names
  if (tokenNames.length !== 3) {
    throw new Error(`Expected exactly three token names, got ${tokenNames.length}`);
  }
  const [firstTokenName, secondTokenName, thirdTokenName] = tokenNames;
  const firstPrize: WinningTokenInfo = {
    policyId,
    tokenName: firstTokenName,
    prize: "1stPrize",
    datumFile: mkDatumFile(policyId, firstTokenName)
  };
  const secondPrize: WinningTokenInfo = {
    policyId,
    tokenName: secondTokenName,
    prize: "2ndPrize",
    datumFile: mkDatumFile(policyId, secondTokenName)
  };
  const thirdPrize: WinningTokenInfo = {
    policyId,
    tokenName: thirdTokenName,
    prize: "3rdPrize",
    datumFile: mkDatumFile(policyId, thirdTokenName)
  };
  return {
    firstPrize,
    secondPrize,
    thirdPrize
  };
}

const mkRedeemerFile = (): string => {
  // Generate system wide temporary file name
  const dir = tmpdir();
  const redeemerFile = `${dir}/prize.redeemer`;
  const cmd = "marlowe-cli";
  const args = [
    "role",
    "redeemer",
    "--out-file",
    redeemerFile
  ];
  const result = child_process.spawnSync(cmd, args);
  if (result.status !== 0) {
    console.log(result.stderr.toString());
    process.exit(1);
  } else {
    console.log("cmd: ", cmd);
    console.log(args);
    console.log("stdout: ");
    console.log(result.stdout.toString());
    console.log("stderr: ");
    console.log(result.stderr.toString());
    console.log("Redeemer file created: ", redeemerFile);
  }
  return redeemerFile;
}

const mkProtocolParamsFile = (): string => {
  // Generate system wide temporary file name
  const dir = tmpdir();
  const protocolParamsFile = `${dir}/network.protocol`;
  const cmd = "cardano-cli";
  const args = [
    "query",
    "protocol-parameters",
    "--testnet-magic",
    "1",
    "--out-file",
    protocolParamsFile
  ];
  const result = child_process.spawnSync(cmd, args);
  if (result.status !== 0) {
    console.log(result.stderr.toString());
    process.exit(1);
  } else {
    console.log("cmd: ", cmd);
    console.log(args);
    console.log("stdout: ");
    console.log(result.stdout.toString());
    console.log("stderr: ");
    console.log(result.stderr.toString());
    console.log("Protocol params file created: ", protocolParamsFile);
  }
  return protocolParamsFile;
}

class PrizeBundleSource {
  collateralUTxO: UTxO;
  nonPureLovelaceUTxO: UTxO[];
  holderChangeAddr: Address;
  holderSkeyFile: string;
  nonLovelaceValueMap: NonLovelaceValueMap;
  prize: Prize;
  // Constructor should throw if any array or map is empty or if skay file is missing
  constructor(collateralUTxO: UTxO, nonPureLovelaceUTxO: UTxO[], holderChangeAddr: string, holderSkeyFile: string, nonLovelaceValueMap: NonLovelaceValueMap, prize: Prize) {
    if (nonPureLovelaceUTxO.length === 0) {
      throw new Error("nonPureLovelaceUTxO is empty");
    }
    if (nonLovelaceValueMap.size === 0) {
      throw new Error("nonLovelaceValueMap is empty");
    }
    if (!fs.existsSync(holderSkeyFile)) {
      throw new Error(`holderSkeyFile is missing: ${holderSkeyFile}`);
    }
    this.collateralUTxO = collateralUTxO;
    this.nonPureLovelaceUTxO = nonPureLovelaceUTxO;
    this.holderChangeAddr = holderChangeAddr;
    this.holderSkeyFile = holderSkeyFile;
    this.nonLovelaceValueMap = nonLovelaceValueMap;
    this.prize = prize;
  }
}

const totalNonLovelaceValue = (utxos: UTxO[]): NonLovelaceValueMap => {
  const valueMap = utxos.reduce((acc: ValueMap, utxo: UTxO) => {
    return appendValueMap(acc, utxo.assets);
  }, new Map<AssetId, bigint>());
  return toNonLovelaceValueMap(valueMap);
}

const holderPrizeBundleSource = async (holderDir: string, prize: Prize): Promise<PrizeBundleSource> => {
  const addr = fs.readFileSync(`${holderDir}/addr_test.pay`, "utf8").trim();
  console.log("Creating prize bundle source for: ", addr);
  const utxos = await addressUTxOs(addr);
  const holderSkeyFile = `${holderDir}/addr.skey`;
  const possibleCollateralUTxOs = pureLovelaceUTxOs(utxos);
  console.log(possibleCollateralUTxOs);
  if (possibleCollateralUTxOs.length === 0) {
    throw new Error(`Expected at least one pure lovelace utxo.`);
  }
  const collateralUTxO = possibleCollateralUTxOs[0];
  return new PrizeBundleSource(collateralUTxO, nonPureLovelaceUTxOs(utxos), addr, holderSkeyFile, totalNonLovelaceValue(utxos), prize);
}

const formatTxOut = (changeAddress: Address, nonLovelaceValueMap: NonLovelaceValueMap, lovelace: bigint): string => {
  const lovelaceAmount = lovelace.toString();
  const nonLovelaceAmounts = [...nonLovelaceValueMap.entries()].map(([assetId, quantity]: [NonLovelaceAssetId, bigint]) => {
    const { currencySymbol, tokenName } = assetId;
    const amount = quantity.toString();
    return `${amount} ${currencySymbol}.${tokenName}`;
  });
  const amounts = [...nonLovelaceAmounts];
  return `${changeAddress}+${lovelaceAmount}+${amounts.join("+")}`;
}

const mkUnsignedPayoutTransaction = (prizeBundleSource: PrizeBundleSource, winningTokenInfo: WinningTokenInfo): string => {
  console.log("Creating unsigned payout transaction for: ", prizeBundleSource, winningTokenInfo);
  const { collateralUTxO, nonPureLovelaceUTxO, nonLovelaceValueMap, prize } = prizeBundleSource;
  mkRedeemerFile();
  const protocolParamsFile = mkProtocolParamsFile();
  const otherTxIns = nonPureLovelaceUTxO.map(formatTxOutRef).flatMap((txOutRef: string) => ["--tx-in", txOutRef]);
  const tmpDir = tmpdir();
  const txOut = formatTxOut(scriptAddresses.testNet, nonLovelaceValueMap, 7000000n); // collateralUTxO.assets.get("lovelace") || BigInt(0));
  const txFile = `${tmpDir}/${prize}.unsigned`;
  const cmd = "cardano-cli";
  const args = [
    "transaction",
    "build",
    "--testnet-magic",
    "1",
    "--babbage-era",
    "--protocol-params-file",
    protocolParamsFile,
    "--tx-in-collateral",
    formatTxOutRef(collateralUTxO),
    "--tx-in",
    formatTxOutRef(collateralUTxO),
    ...otherTxIns,
    "--tx-out",
    txOut,
    "--tx-out-datum-embed-file",
    winningTokenInfo.datumFile,
    "--change-address",
    prizeBundleSource.holderChangeAddr,
    "--out-file",
    txFile
  ];
  // now let's add other tx-ins
  // args.push(...otherTxIns);
  console.log("cmd: ", cmd);
  console.log(args);

  const result = child_process.spawnSync(cmd, args);
  if (result.status !== 0) {
    console.log(result.stderr.toString());
    process.exit(1);
  }
  return txFile;
}

const mkSignedPayoutTransaction = (prizeBundleSource: PrizeBundleSource, winningTokenInfo: WinningTokenInfo): string => {
  const unsignedTxFile = mkUnsignedPayoutTransaction(prizeBundleSource, winningTokenInfo);
  console.log("Signing payout transaction: ", unsignedTxFile);
  const { holderSkeyFile } = prizeBundleSource;
  const cmd = "cardano-cli";
  const signedTxFile = unsignedTxFile.replace(".unsigned", ".signed");
  const args = [
    "transaction",
    "sign",
    "--testnet-magic",
    "1",
    "--signing-key-file",
    holderSkeyFile,
    "--tx-body-file",
    unsignedTxFile,
    "--out-file",
    signedTxFile
  ];

  const result = child_process.spawnSync(cmd, args);
  if (result.status !== 0) {
    console.log(result.stderr.toString());
    process.exit(1);
  }
  return signedTxFile;
}

const submitPayoutTransaction = (prizeBundleSource: PrizeBundleSource, winningTokenInfo: WinningTokenInfo): string => {
  const signedTxFile = mkSignedPayoutTransaction(prizeBundleSource, winningTokenInfo);
  console.log("Submitting payout transaction: ", signedTxFile);
  const cmd = "cardano-cli";
  const args = [
    "transaction",
    "submit",
    "--testnet-magic",
    "1",
    "--tx-file",
    signedTxFile
  ];
  const result = child_process.spawnSync(cmd, args);
  if (result.status !== 0) {
    console.log(result.stderr.toString());
    process.exit(1);
  }
  const txIdArgs = [
    "transaction",
    "txid",
    "--tx-file",
    signedTxFile
  ];
  const txIdResult = child_process.spawnSync(cmd, txIdArgs);
  if (txIdResult.status !== 0) {
    console.log(txIdResult.stderr.toString());
    process.exit(1);
  }
  const txId = txIdResult.stdout.toString().trim();
  return txId;
}

type MintingReportFile = string;

const mintWinningTokens = (): MintingReportFile => {
  const mintingScriptPath = "./minting/mint-winning-tokens.yaml";
  const mintingWinningTokensReportFile:MintingReportFile = "./minting-report.json";
  const cardanoNodeSocketPath = process.env.CARDANO_NODE_SOCKET_PATH;
  if(!cardanoNodeSocketPath) {
    throw new Error("CARDANO_NODE_SOCKET_PATH is not defined");
  }
  const walletsDir = "./wallets";
  const faucetVKey = fs.readFileSync(`${walletsDir}/faucet/addr_test.pay`, "utf8").trim();
  const faucetSKeyFile = `${walletsDir}/faucet/addr.skey`;
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
    mintingWinningTokensReportFile,
    "--max-retries",
    "3",
    "--stream-json",
    mintingScriptPath
  ];
  console.log("Running minting test script");
  const result = child_process.spawnSync(cmd, args);
  if (result.status !== 0) {
    console.log(result.stderr.toString());
    console.log("Failed to run minting test script");
    console.log("Report file:", mintingWinningTokensReportFile);
    process.exit(1);
  }
  return mintingWinningTokensReportFile;
}

export const makePayoutUTxOs = async (): Promise<TxId[]> => {
  console.log("Running makePayoutUTxOs");
  const mintingReportFile:MintingReportFile = mintWinningTokens();
  const mintingWinningTokensReportFile = extractWinningTokensInfo(mintingReportFile);

  const firstPrizeBundleSource = await holderPrizeBundleSource(firstPrizeHolderDir, "1stPrize");
  console.log("firstPrizeBundleSource: ", firstPrizeBundleSource);
  console.log("Submitting payout transaction for: ", mintingWinningTokensReportFile.firstPrize);
  const firstPrizeTxId = submitPayoutTransaction(firstPrizeBundleSource, mintingWinningTokensReportFile.firstPrize);
  console.log("1st prize tx id: ", firstPrizeTxId);

  const secondPrizeBundleSource = await holderPrizeBundleSource(secondPrizeHolderDir, "2ndPrize");
  console.log("Submitting payout transaction for: ", mintingWinningTokensReportFile.secondPrize);
  const secondPrizeTxId = submitPayoutTransaction(secondPrizeBundleSource, mintingWinningTokensReportFile.secondPrize);
  console.log("2nd prize tx id: ", secondPrizeTxId);

  const thirdPrizeBundleSource = await holderPrizeBundleSource(thirdPrizeHolderDir, "3rdPrize");
  console.log("Submitting payout transaction for: ", mintingWinningTokensReportFile.thirdPrize);
  const thirdPrizeTxId = submitPayoutTransaction(thirdPrizeBundleSource, mintingWinningTokensReportFile.thirdPrize);
  console.log("3rd prize tx id: ", thirdPrizeTxId);
  return [firstPrizeTxId, secondPrizeTxId, thirdPrizeTxId];
}
