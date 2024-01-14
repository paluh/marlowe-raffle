import { Address, AssetId, UTxO, ValueFlatMap } from './cardano.js';

// Blockfrost combines currency symbol and token name into a single string.
export type BlockFrostAssetId = string;

export const toCardanoAssetId = (blockFrostAssetId: BlockFrostAssetId): AssetId => {
  if(blockFrostAssetId === "lovelace") {
    return "lovelace";
  } else {
    const currencySymbol = blockFrostAssetId.slice(0, 56);
    const tokenName = blockFrostAssetId.slice(56);
    return { currencySymbol, tokenName };
  }
}

export const addressUTxOs = async (addrs: Address): Promise<UTxO[]> => {
  const response = await fetch(`https://cardano-preprod.blockfrost.io/api/v0/addresses/${addrs}/utxos/`, {
    headers: {
      "project_id": "preprodD9cONxVqzHYtFEL4RObOZ46y4begqNHc"
    }
  });
  const utxos = await response.json();
  const utxoList: UTxO[] = [];
  utxos.forEach((utxo: any) => {
    const assets: ValueFlatMap = new Map<AssetId, bigint>();
    utxo.amount.forEach((amount: any) => {
      const assetId = toCardanoAssetId(amount.unit);
      assets.set(
        assetId,
        amount.quantity
      );
    });

    utxoList.push({
      txId: utxo.tx_hash,
      index: utxo.output_index,
      assets
    });
  });
  return utxoList;
}

