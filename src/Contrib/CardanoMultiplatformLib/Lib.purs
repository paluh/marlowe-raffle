module CardanoMultiplatformLib.Lib
  ( props
  , Lib
  , Props
  , Hash_script_data
  , Calc_script_data_hash
  ) where

import CardanoMultiplatformLib.Address as Address
import CardanoMultiplatformLib.CostModel as CostModel
import CardanoMultiplatformLib.Ed25519KeyHash as Ed25519KeyHash
import CardanoMultiplatformLib.Transaction as Transaction
import Data.Undefined.NoProblem as NoProblem
import Effect.Uncurried (EffectFn3, EffectFn4)

type Hash_script_data =
  EffectFn3
    Transaction.RedeemersObject
    CostModel.CostmdlsObject
    (NoProblem.Opt Transaction.PlutusListObject)
    Transaction.ScriptDataHashObject

type Calc_script_data_hash =
  EffectFn4
    Transaction.RedeemersObject
    Transaction.PlutusListObject
    CostModel.CostmdlsObject
    CostModel.LanguagesObject
    (NoProblem.Opt Transaction.ScriptDataHashObject)

type Props =
  { "Address" :: Address.Address
  , "BigInt" :: Transaction.BigInt
  , "BigIntObject" :: Transaction.BigIntObject
  , "BigNum" :: Transaction.BigNum
  , "BigNumObject" :: Transaction.BigNumObject
  , "CostModel" :: CostModel.CostModel
  , "CostModelObject" :: CostModel.CostModelObject
  , "Costmdls" :: CostModel.Costmdls
  , "CostmdlsObject" :: CostModel.CostmdlsObject
  , "Ed25519KeyHash" :: Ed25519KeyHash.Ed25519KeyHash
  , "Ed25519KeyHashObject" :: Ed25519KeyHash.Ed25519KeyHashObject
  , "Value" :: Transaction.Value
  , "Transaction" :: Transaction.Transaction
  , "TransactionWitnessSet" :: Transaction.TransactionWitnessSet
  , "TransactionBody" :: Transaction.TransactionBody
  , "TransactionBodyObject" :: Transaction.TransactionBodyObject
  , "TransactionHash" :: Transaction.TransactionHash
  , "TransactionHashObject" :: Transaction.TransactionHashObject
  , "TransactionUnspentOutput" :: Transaction.TransactionUnspentOutput
  , "TransactionUnspentOutputObject" :: Transaction.TransactionUnspentOutputObject
  , "TransactionOutput" :: Transaction.TransactionOutput
  , "TransactionOutputObject" :: Transaction.TransactionOutputObject
  , "TransactionInput" :: Transaction.TransactionInput
  , "TransactionInputObject" :: Transaction.TransactionInputObject
  , "hash_script_data" :: Hash_script_data
  , "calc_script_data_hash" :: Calc_script_data_hash
  }

newtype Lib = Lib Props

props :: Lib -> Props
props (Lib r) = r

