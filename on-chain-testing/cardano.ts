export type Address = string;

export type CurrencySymbol = string;

export type TokenName = string;

export type NonLovelaceAssetId = Readonly<{
  currencySymbol: CurrencySymbol;
  tokenName: TokenName
}>;

export type AssetId = "lovelace" | NonLovelaceAssetId;

export type NonLovelaceValueMap = Map<NonLovelaceAssetId, bigint>;

export type ValueFlatMap = Map<AssetId, bigint>;

export type ValueMap = ValueFlatMap;

export const appendValueMap = (valueMap1: ValueMap, valueMap2: ValueMap): ValueMap => {
  const result = new Map<AssetId, bigint>(valueMap1);
  valueMap2.forEach((quantity: bigint, assetId: AssetId) => {
    const tokenQuantity:bigint = result.get(assetId) || BigInt(0);
    result.set(assetId, tokenQuantity + quantity);
  });
  return result;
}

export const toNonLovelaceValueMap = (valueMap: ValueMap): NonLovelaceValueMap => {
  const result = new Map<NonLovelaceAssetId, bigint>();
  valueMap.forEach((quantity: bigint, assetId: AssetId) => {
    if(assetId === "lovelace") return;
    const tokenQuantity:bigint = result.get(assetId) || BigInt(0);
    result.set(assetId, tokenQuantity + quantity);
  });
  return result;
}

export const appendNonLovelaceValueMap = (valueMap1: NonLovelaceValueMap, valueMap2: NonLovelaceValueMap): NonLovelaceValueMap => {
  const result = new Map<NonLovelaceAssetId, bigint>(valueMap1);
  valueMap2.forEach((quantity: bigint, assetId: NonLovelaceAssetId) => {
    const tokenQuantity:bigint = result.get(assetId) || BigInt(0);
    result.set(assetId, tokenQuantity + quantity);
  });
  return result;
}

export type Asset = { assetId: AssetId; quantity: bigint };

export type TxId = string;

export type UTxO = { txId: TxId; index: number; assets: ValueMap };

export type TxOutRef = { txId: TxId; index: number };

export const pureLovelaceUTxOs = (utxos: UTxO[]): UTxO[] => {
  return utxos.filter((utxo: UTxO) => {
    return [...utxo.assets.entries()].every(([assetId, ]:[AssetId, bigint]) => assetId == "lovelace")
  });
}

export const nonPureLovelaceUTxOs = (utxos: UTxO[]): UTxO[] => {
  return utxos.filter((utxo: UTxO) => {
    return ![...utxo.assets.entries()].every(([assetId, ]:[AssetId, bigint]) => assetId == "lovelace")
  });
}
