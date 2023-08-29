type TxId = string;

export interface TxOutRef {
  txId: TxId;
  txIx: number;
}

type Network = "preprod" | "preview" | "mainnet";

type ProjectId = string;

type WalletApi = any;

export interface Props {
  wallet: WalletApi;
  network: Network;
  txOutRef: TxOutRef;
  blockfrostProjectId: ProjectId;
}

type HookError<tag> = {
  tag: tag
  msg: string;
  // This is a JSON which can help debugging;
  info: any;
};

type InitializationError = HookError<"Setup" | "FetchPayoutUTxOError" | "FindRoleTokenUTxOError" | "PayoutUTxOStatusCheckError" | "PayoutUTxOAlreadySpentError">;

type WithdrawalError = HookError<"GrabCollateralUTxOsError" | "BuildTxError" | "SignTxOperationError" | "UserAbortedError" | "WalletSubmitTxError" | "WitnessKeySetupFailed" | "BlockfrostSubmitTxError">;

type FatalError = HookError<"FatalError">;

type HookStatus = {
  status: "Initializing";
  step: string;
} | {
  status: "InitializationFailed";
  error: InitializationError;
} | {
  status: "AwaitingWithdrawalTrigger";
  trigger: () => void;
} | {
  status: "ProcessingWithdrawal";
  msg: string;
} | {
  status: "WithdrawalFailed";
  error: WithdrawalError;
  retry: () => void;
} | {
  status: "WithdrawalSucceeded";
  txId: string;
} | {
  status: "FatalError";
  error: FatalError;
};

export function useWithdrawal (props: Props): { status: HookStatus, reset: (props: Props) => void };

