{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Web3.Types
  ( Int64
  , Text
  , HexData
  , HexData256
  , HexQuantity
  , HexHash64
  , HexHash160
  , HexHash256
  , HexHash512
  , HexHash2048
  , HexEthAddr(..)
  , HexEthSign
  , isHexStrHash
  , isHexStrAddr
  , RpcMaybeBool(..)
  , fromRpcMaybeBool
  , RpcMaybeObj(..)
  , fromRpcMaybeObj
  , RpcEthSyncing(..)
  , ParamBlock(..)
  , RpcParamBlock(..)
  , RpcParamObject(..)
  , RpcEthTx(..)
  , RpcEthMsgCall(..)
  , RpcEthBlkTx(..)
  , RpcEthBlock(..)
  , isPendingBlock
  , RpcEthLog(..)
  , RpcEthTxReceipt(..)
  , RpcEthStorage(..)
  , RpcEthAccount(..)
  , RpcEthState(..)
  , RpcTraceOptions(..)
  , defaultTraceOptions
  , RpcTraceLog(..)
  , RpcEthTraceTx(..)
  , RpcEthFilterTopicValue(..)
  , RpcEthFilterTopic(..)
  , RpcEthFilter(..)
  , RpcFilterLog(..)
  , Web3Rpc(..)
  ) where

import Control.Monad (mapM)
import Data.Aeson.JsonRpc
import Data.Aeson.JsonUtils
import Data.Aeson.Types
import Data.Int
import Data.Maybe (fromJust,isJust,isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Network.JsonRpcConn
import Network.Web3.HexText

data Web3Rpc =
    Web3_web3_clientVersion
  | Web3_web3_sha3 HexData
  | Web3_net_version
  | Web3_net_listening
  | Web3_net_peerCount
  | Web3_eth_protocolVersion
  | Web3_eth_syncing
  | Web3_eth_coinbase
  | Web3_eth_mining
  | Web3_eth_hashrate
  | Web3_eth_gasPrice
  | Web3_eth_accounts
  | Web3_eth_blockNumber
  | Web3_eth_getBalance HexEthAddr RpcParamBlock
  | Web3_eth_getStorageAt HexEthAddr HexQuantity RpcParamBlock
  | Web3_eth_getTransactionCount HexEthAddr RpcParamBlock
  | Web3_eth_getBlockTransactionCountByHash HexHash256
  | Web3_eth_getBlockTransactionCountByNumber RpcParamBlock
  | Web3_eth_getUncleCountByBlockHash HexHash256
  | Web3_eth_getUncleCountByBlockNumber RpcParamBlock
  | Web3_eth_getCode HexEthAddr RpcParamBlock
  | Web3_eth_sign HexEthAddr HexData
  | Web3_eth_sendTransaction RpcEthTx
  | Web3_eth_sendRawTransaction HexData
  | Web3_eth_call RpcEthMsgCall RpcParamBlock
  | Web3_eth_estimateGas RpcEthMsgCall
  | Web3_eth_getBlockByHash HexHash256 Bool
  | Web3_eth_getBlockByNumber RpcParamBlock Bool
  | Web3_eth_getTransactionByHash HexHash256
  | Web3_eth_getTransactionByBlockHashAndIndex HexHash256 HexQuantity
  | Web3_eth_getTransactionByBlockNumberAndIndex RpcParamBlock HexQuantity
  | Web3_eth_getTransactionReceipt HexHash256
  | Web3_eth_getUncleByBlockHashAndIndex HexHash256 HexQuantity
  | Web3_eth_getUncleByBlockNumberAndIndex RpcParamBlock HexQuantity
  | Web3_eth_newFilter RpcEthFilter
  | Web3_eth_newBlockFilter
  | Web3_eth_newPendingTransactionFilter
  | Web3_eth_uninstallFilter HexQuantity
  | Web3_eth_getFilterChanges HexQuantity
  | Web3_eth_getLogs RpcEthFilter
  | Web3_debug_dumpBlock HexQuantity
  | Web3_debug_traceTransaction HexHash256 RpcTraceOptions
  deriving (Show)

instance FromResponse Bool where
  parseResult _ = Just parseJSON

instance FromResponse [Text] where
  parseResult _ = Just parseJSON

instance FromResponse Text where
  parseResult _ = Just parseJSON

instance FromResponse HexEthAddr where
  parseResult _ = Just parseJSON

instance FromJSON a => FromResponse (RpcMaybeBool a) where
  parseResult _ = Just parseJSON

instance FromJSON a => FromResponse (RpcMaybeObj a) where
  parseResult _ = Just parseJSON

instance FromResponse RpcEthSyncing where
  parseResult _ = Just parseJSON

instance FromResponse RpcParamBlock where
  parseResult _ = Just parseJSON

instance FromJSON a => FromResponse (RpcParamObject a) where
  parseResult _ = Just parseJSON

instance FromResponse RpcEthBlkTx where
  parseResult _ = Just parseJSON

instance FromResponse RpcEthBlock where
  parseResult _ = Just parseJSON

instance FromResponse RpcEthLog where
  parseResult _ = Just parseJSON

instance FromResponse RpcEthTxReceipt where
  parseResult _ = Just parseJSON

instance FromResponse RpcEthState where
  parseResult _ = Just parseJSON

instance FromResponse RpcTraceLog where
  parseResult _ = Just parseJSON

instance FromResponse RpcEthTraceTx where
  parseResult _ = Just parseJSON

type HexData = HexStr
type HexData256 = HexData
type HexQuantity = HexStr
type HexHash64 = HexStr
type HexHash160 = HexStr
type HexHash256 = HexStr
type HexHash512 = HexStr
type HexHash2048 = HexStr
type HexEthSign = HexHash512

newtype HexEthAddr = HexEthAddr { getHexAddr :: HexHash160 } deriving (Eq, Ord)

instance Show HexEthAddr where
  show = T.unpack . getHexAddr

instance ToJSON HexEthAddr where
  toJSON = String . getHexAddr

instance FromJSON HexEthAddr where
  parseJSON (String h) = return $ HexEthAddr h

isHexStrLength :: Int -> HexStr -> Bool
isHexStrLength l = (l+2 ==) . T.length

isHexStrHash :: HexStr -> Bool
isHexStrHash = isHexStrLength 64

isHexStrAddr :: HexStr -> Bool
isHexStrAddr = isHexStrLength 40

data RpcMaybeBool a = MBFalse | MBJust a deriving (Show)

instance (FromJSON a) => FromJSON (RpcMaybeBool a) where
  parseJSON (Bool False) = return MBFalse
  parseJSON v = MBJust <$> parseJSON v

fromRpcMaybeBool :: RpcMaybeBool a -> Maybe a
fromRpcMaybeBool mb = case mb of
  MBFalse -> Nothing
  MBJust a -> Just a

data RpcMaybeObj a = MONull | MOJust a deriving (Show)

instance (FromJSON a) => FromJSON (RpcMaybeObj a) where
  parseJSON Null = return MONull
  parseJSON v = MOJust <$> parseJSON v

fromRpcMaybeObj :: (FromJSON a) => RpcMaybeObj a -> Maybe a
fromRpcMaybeObj mo = case mo of
  MONull -> Nothing
  MOJust a -> Just a

fromMaybeRpcMaybeObj :: (FromJSON a) => Maybe (RpcMaybeObj a) -> Maybe a
fromMaybeRpcMaybeObj mmo = mmo >>= fromRpcMaybeObj

data RpcEthSyncing = RpcEthSyncing
                   { synStartingBlock :: Int64  -- ^ El nº de bloque donde se ha iniciado la importación (se resetea despues que la sincronización lo alcance)
                   , synCurrentBlock :: Int64   -- ^ El bloque actual
                   , synHighestBlock :: Int64   -- ^ El último bloque estimado
                   } deriving (Show)

instance FromJSON RpcEthSyncing where
  parseJSON (Object o) = RpcEthSyncing
                     <$> (fromHex <$> o .: "startingBlock")
                     <*> (fromHex <$> o .: "currentBlock")
                     <*> (fromHex <$> o .: "highestBlock")

data ParamBlock = PBHex HexHash256      -- ^ Hash del bloque
                | PBNum Int64           -- ^ Nº de bloque
                | PBEarliest            -- ^ Bloque inicial
                | PBLatest              -- ^ Último bloque minado
                | PBPending             -- ^ Bloque de las transacciones/estado pendientes
                deriving (Show)

data RpcParamBlock = RPBNum Int64            -- ^ Nº de bloque
                   | RPBEarliest             -- ^ Bloque inicial
                   | RPBLatest               -- ^ Último bloque minado
                   | RPBPending              -- ^ Bloque de las transacciones/estado pendientes
                   deriving (Show)

instance ToJSON RpcParamBlock where
  toJSON RPBEarliest = String "earliest"
  toJSON RPBLatest = String "latest"
  toJSON RPBPending = String "pending"
  toJSON (RPBNum n) = String $ toHex n

instance FromJSON RpcParamBlock where
  parseJSON (String "earliest") = return RPBEarliest
  parseJSON (String "latest") = return RPBLatest
  parseJSON (String "pending") = return RPBPending
  parseJSON (String h) = return $ RPBNum $ fromHex h

data RpcParamObject a = POHash HexHash256 | POObject a deriving (Show)

instance (FromJSON a) => FromJSON (RpcParamObject a) where
  parseJSON (String h) = return $ POHash h
  parseJSON v@(Object o) = POObject <$> parseJSON v

toJSONListMaybe :: ToJSON a => [(Text,Maybe a)] -> [Pair]
toJSONListMaybe = foldl (\r (t,ma) -> t .= fromJust ma:r) []
                . filter (isJust . snd)

data RpcEthTx = RpcEthTx
  { txFrom :: HexEthAddr            -- ^ Dirección del emisor
  , txTo :: Maybe HexEthAddr        -- ^ Dirección del destinatario de la transacción (opcional al crear un nuevo contract)
  , txGas :: Maybe Int64            -- ^ Gas provisto para la ejecución de la transacción. Devolverá el gas no usado. opcional, por defecto 90000
  , txGasPrice :: Maybe Integer     -- ^ Precio del gas usado en cada pago de gas. Opcional, por defecto 'pendiente de determinar'
  , txValue :: Maybe Integer        -- ^ Valor enviado junto con la transacción. Opcional.
  , txData :: Maybe HexData         -- ^ El código compilado de un contract, o el hash de la firma del método llamado y los parámetros codificados
  , txNonce :: Maybe Int64          -- ^ Nonce. Permite reemplazar transacciones pendientes que usan el mismo nonce. Opcional
  } deriving (Show)

instance ToJSON RpcEthTx where
  toJSON (RpcEthTx f t g gp v d n) =
    object $ toJSONListMaybe
      [ ("from", Just $ getHexAddr f)
      , ("to", t >>= Just . getHexAddr)
      , ("gas", g >>= Just . toHex)
      , ("gasPrice", gp >>= Just . toHex)
      , ("value", v >>= Just . toHex)
      , ("data", d >>= Just)
      , ("nonce", n >>= Just . toHex)
      ]

data RpcEthMsgCall = RpcEthMsgCall
  { msgFrom :: Maybe HexEthAddr         -- ^ Dirección del emisor
  , msgTo :: Maybe HexEthAddr           -- ^ Dirección del destinatario
  , msgGas :: Maybe Int64               -- ^ Gas provisto para la ejecución de la transacción
  , msgGasPrice :: Maybe Integer        -- ^ Precio del gas usado en cada pago de gas
  , msgValue :: Maybe Integer           -- ^ Valor enviado en la transacción
  , msgData :: Maybe HexData            -- ^ Hash de la firma del método y parámetros codificados
  } deriving (Show)

instance ToJSON RpcEthMsgCall where
  toJSON (RpcEthMsgCall f t g gp v d) =
    object $ toJSONListMaybe
      [ ("from", f >>= Just . getHexAddr)
      , ("to", t >>= Just . getHexAddr)
      , ("gas", g >>= Just . toHex)
      , ("gasPrice", gp >>= Just . toHex)
      , ("value", v >>= Just . toHex)
      , ("data", d)
      ]

data RpcEthBlkTx = RpcEthBlkTx
  { btxHash :: HexHash256               -- ^ Hash de la transacción
  , btxNonce :: Int64                   -- ^ Nº de transacciones hechas por el emisor previas a esta
  , btxBlockHash :: Maybe HexHash256    -- ^ Hash del bloque que la contiene, a no ser que el bloque este pendiente
  , btxBlockNumber :: Maybe Int64       -- ^ Nº de bloque que la contiene
  , btxTransactionIndex :: Maybe Int    -- ^ Indice de la transacción en el bloque
  , btxFrom :: HexEthAddr               -- ^ Dirección del emisor
  , btxTo :: Maybe HexEthAddr           -- ^ Dirección del receptor
  , btxValue :: Integer                 -- ^ Valor transferido en *wei*s
  , btxGasPrice :: Integer              -- ^ Precio del gas del emisor en *wei*s
  , btxGas :: Int64                     -- ^ Gas provisto por el emisor
  , btxInput :: HexData                 -- ^ Datos enviados en la transacción
  , btxV :: Maybe Integer
  , btxR :: Maybe Integer
  , btxS :: Maybe Integer
  } deriving (Show)

instance FromJSON RpcEthBlkTx where
  parseJSON (Object o) = RpcEthBlkTx
                     <$> o .: "hash"
                     <*> (fromHex <$> o .: "nonce")
                     <*> o .:? "blockHash"
                     <*> (fromMaybeHex <$> o .:? "blockNumber")
                     <*> (fromMaybeHex <$> o .:? "transactionIndex")
                     <*> (HexEthAddr <$> o .: "from")
                     <*> (fmap HexEthAddr <$> o .:? "to")
                     <*> (fromHex <$> o .: "value")
                     <*> (fromHex <$> o .: "gasPrice")
                     <*> (fromHex <$> o .: "gas")
                     <*> o .: "input"
                     <*> (fromMaybeHex <$> o .:? "v")
                     <*> (fromMaybeHex <$> o .:? "r")
                     <*> (fromMaybeHex <$> o .:? "s")

fromMaybeHex :: HexText a => Maybe HexStr -> Maybe a
fromMaybeHex = fmap fromHex

isPendingBlock :: RpcEthBlock -> Bool
isPendingBlock blk = isNothing (rebHash blk) &&
                     isNothing (rebMiner blk) &&
                     isNothing (rebNonce blk)

-- | TODO: Usar dos constructores, para bloque existente y pendiente
data RpcEthBlock = RpcEthBlock
  { rebNumber :: Maybe Int64            -- ^ Nº de bloque
  , rebHash :: Maybe HexHash256         -- ^ Hash del bloque
  , rebParentHash :: Maybe HexHash256   -- ^ Hash del bloque padre
  , rebNonce :: Maybe HexHash64         -- ^ Hash del POW
  , rebMixHash :: Maybe HexHash256
  , rebSha3Uncles :: HexHash256         -- ^ SHA3 de los datos uncles del bloque
  , rebLogsBloom :: Maybe HexHash2048   -- ^ El *bloom filter* de los logs del bloque
  , rebTransactionsRoot :: HexHash256   -- ^ El root del trie de la transacción del bloque
  , rebStateRoot :: HexHash256          -- ^ El root del trie del estado final del bloque
  , rebReceiptsRoot :: HexHash256       -- ^ El root del trie de receptores del bloque
  , rebMiner :: Maybe HexEthAddr        -- ^ Dirección del beneficiario que recibe la recompensa del minado
  , rebDifficulty :: Integer            -- ^ Difficulty
  , rebTotalDifficulty :: Maybe Integer -- ^ Difficulty total del chain hasta este bloque
  , rebExtraData :: HexData
  , rebSize :: Int                      -- ^ Tamaño del bloque en bytes
  , rebGasLimit :: Int64                -- ^ Máximo gas permitido en el bloque
  , rebGasUsed :: Int64                 -- ^ El gas total usado por todas las transacciones del bloque
  , rebTimestamp :: Int64               -- ^ Fecha de incorporación del bloque
  , rebTransactions :: [RpcParamObject RpcEthBlkTx]     -- ^ Array de transacciones, compuesto de objetos transacción o sus hashes, según parámetro de llamada
  , rebUncles :: [HexHash256]           -- ^ Array de uncle hashes
  } deriving (Show)

instance FromJSON RpcEthBlock where
  parseJSON (Object o) = RpcEthBlock
                     <$> (fromMaybeHex <$> o .:? "number")
                     <*> (fromMaybeRpcMaybeObj <$> o .:? "hash")
                     <*> o .:? "parentHash"
                     <*> (fromMaybeRpcMaybeObj <$> o .:? "nonce")
                     <*> o .:? "mixHash"
                     <*> o .: "sha3Uncles"
                     <*> o .:? "logsBloom"
                     <*> o .: "transactionsRoot"
                     <*> o .: "stateRoot"
                     <*> o .: "receiptsRoot"
                     <*> (fmap HexEthAddr . fromMaybeRpcMaybeObj <$> o .:? "miner")
                     <*> (fromHex <$> o .: "difficulty")
                     <*> (fromMaybeHex . fromMaybeRpcMaybeObj <$> o .:? "totalDifficulty")
                     <*> o .: "extraData"
                     <*> (fromHex <$> o .: "size")
                     <*> (fromHex <$> o .: "gasLimit")
                     <*> (fromHex <$> o .: "gasUsed")
                     <*> (fromHex <$> o .: "timestamp")
                     <*> o .: "transactions"
                     <*> o .: "uncles"

data RpcEthLog = RpcEthLog
               { logRemoved :: Maybe Bool
               , logLogIndex :: Maybe Int
               , logTransactionIndex :: Maybe Int
               , logTransactionHash :: Maybe HexHash256
               , logBlockHash :: Maybe HexHash256
               , logBlockNumber :: Maybe Int64
               , logAddress :: HexEthAddr
               , logData :: HexData
               , logTopics :: [HexData]
               } deriving (Show)

instance FromJSON RpcEthLog where
  parseJSON (Object o) = RpcEthLog
                     <$> o .:? "removed"
                     <*> (fromMaybeHex . fromMaybeRpcMaybeObj <$> o .:? "logIndex")
                     <*> (fromMaybeHex . fromMaybeRpcMaybeObj <$> o .:? "transactionIndex")
                     <*> (fromMaybeRpcMaybeObj <$> o .:? "transactionHash")
                     <*> (fromMaybeRpcMaybeObj <$> o .:? "blockHash")
                     <*> (fromMaybeHex . fromMaybeRpcMaybeObj <$> o .:? "blockNumber")
                     <*> (HexEthAddr <$> o .: "address")
                     <*> o .: "data"
                     <*> o .: "topics"

data RpcEthTxReceipt = RpcEthTxReceipt
                     { txrTransactionHash :: HexHash256     -- ^ Hash de la transacción
                     , txrTransactionIndex :: Int           -- ^ Indice de la transacción en el bloque
                     , txrBlockHash :: HexHash256           -- ^ Hash del bloque donde se encuentra la transacción
                     , txrBlockNumber :: Int64              -- ^ Nº del bloque
                     , txrCumulativeGasUsed :: Int64        -- ^ Cantidad total de gas usado cuando se ejecutó la transacción en el bloque
                     , txrGasUsed :: Int64                  -- ^ La cantidad de gas usado por esta transacción
                     , txrContractAddress :: Maybe HexEthAddr -- ^ La dirección del contract creado, si es el caso
                     , txrLogs :: [RpcEthLog]               -- ^ Array de objetos log, que ha generado esta transación
                     , txrStatus :: Maybe Bool              -- ^ Just False indica que la transacción ha fallado con el opcode REVERT, y Just True indica que la transacción ha finalizado correctamente (Desde Byzantium)
                     } deriving (Show)

instance FromJSON RpcEthTxReceipt where
  parseJSON (Object o) = RpcEthTxReceipt
                     <$> o .: "transactionHash"
                     <*> (fromHex <$> o .: "transactionIndex")
                     <*> o .: "blockHash"
                     <*> (fromHex <$> o .: "blockNumber")
                     <*> (fromHex <$> o .: "cumulativeGasUsed")
                     <*> (fromHex <$> o .: "gasUsed")
                     <*> (fmap HexEthAddr . fromMaybeRpcMaybeObj <$> o .:? "contractAddress")
                     <*> o .: "logs"
                     <*> (fmap fromHex <$> o .:? "status")

data RpcEthStorage = RpcEthStorage
    { storeAddr :: HexQuantity  -- ^ Dirección de memoria
    , storeVal :: HexData256    -- ^ Valor almacenado
    } deriving (Show)

data RpcEthAccount = RpcEthAccount
    { accAddr :: HexEthAddr         -- ^ Dirección de la cuenta
    , accNonce :: Int64             -- ^ Número de transacciones enviadas
    , accBalance :: Integer         -- ^ Balance de la cuenta en el estado del bloque
    , accCode :: HexData            -- ^ Código del contrato (si lo es)
    , accCodeHash :: HexHash256
    , accStorage :: [RpcEthStorage] -- ^ Almacenamiento del contrato (si lo es)
    , accRoot :: HexHash256
    } deriving (Show)

data RpcEthState = RpcEthState
    { stateRoot :: HexHash256
    , stateAccounts :: [RpcEthAccount]
    } deriving (Show)

parseJsonStorage :: Text -> Value -> Parser RpcEthStorage
parseJsonStorage addr (String s) = return $ RpcEthStorage (joinHex addr) (joinHex s)

parseJsonAccount :: Text -> Value -> Parser RpcEthAccount
parseJsonAccount addr (Object o) = RpcEthAccount (HexEthAddr $ joinHex addr)
                      <$> o .: "nonce"
                      <*> (read <$> o .: "balance")
                      <*> (joinHex <$> o .: "code")
                      <*> (joinHex <$> o .: "codeHash")
                      <*> parseListOfKeyObject o "storage" parseJsonStorage
                      <*> (joinHex <$> o .: "root")

instance FromJSON RpcEthState where
  parseJSON (Object o) = RpcEthState
                     <$> (joinHex <$> o .: "root")
                     <*> parseListOfKeyObject o "accounts" parseJsonAccount

data RpcTraceOptions = RpcTraceOptions
    { traceOpDisableStorage :: Bool     -- ^ Si es `True` desactiva la captura del almacenamiento
    , traceOpDisableMemory :: Bool      -- ^ Si es `True` desactiva la captura de la memoria
    , traceOpDisableStack :: Bool       -- ^ Si es `True` desactiva la captura de la pila de ejecución
    , traceOpFullStorage :: Bool        -- ^ Si es `True` devuelve todo el almacenamiento, incluyendo lo que no ha cambiado. Es un proceso lento, por lo que el valor por defecto es `False`. Si es `False` (por defecto) devuelve solo el almacenamiento que ha sido modifiado
    , traceOpTracer :: Text             -- ^ Ver documentación online en <https://github.com/ethereum/go-ethereum/wiki/Management-APIs#debug_tracetransaction>
    , traceOpTimeout :: Text
    } deriving (Show)

-- | Devuelve las opciones por defecto
defaultTraceOptions :: RpcTraceOptions
defaultTraceOptions = RpcTraceOptions False False False False "" "5s"

instance ToJSON RpcTraceOptions where
  toJSON (RpcTraceOptions dstor dmem dstack fstor tracer timeout) =
    object $ []
      ++ ["disableStorage" .= dstor | dstor]
      ++ ["disableMemory" .= dmem | dmem]
      ++ ["disableStack" .= dstack | dstack]
      ++ ["fullStorage" .= fstor | fstor]
      ++ (if T.null tracer then [] else ["tracer" .= tracer])
      ++ (if timeout=="5s" then [] else ["timeout" .= timeout])

data RpcTraceLog = RpcTraceLog
    { traceLogDepth :: Int                  -- ^ Profundidad de ejecución
    , traceLogError :: Maybe Value          -- ^ Si es `Nothing` ignorar el resto de campos
    , traceLogGas :: Int64                  -- ^ Cantidad de gas restante
    , traceLogGasCost :: Int64              -- ^ Coste de la operación
    , traceLogMemory :: [HexData256]        -- ^ Espacio de memoria del contrato
    , traceLogOp :: Text                    -- ^ OpCode
    , traceLogPc :: Int                     -- ^ Proggram counter
    , traceLogStack :: [HexData256]         -- ^ Pila de ejecución
    , traceLogStorage :: [RpcEthStorage]    -- ^ Almacenamiento
    } deriving (Show)

instance FromJSON RpcTraceLog where
  parseJSON (Object o) = RpcTraceLog
                     <$> o .: "depth"
                     <*> (maybe Nothing fromRpcMaybeObj <$> o .:? "error")
                     <*> o .: "gas"
                     <*> o .: "gasCost"
                     <*> (maybe [] (map joinHex) . fromRpcMaybeObj <$> o .: "memory")
                     <*> o .: "op"
                     <*> o .: "pc"
                     <*> (map joinHex <$> o .: "stack")
                     <*> parseListOfKeyObject o "storage" parseJsonStorage

data RpcEthTraceTx = RpcEthTraceTx
    { traceTxGas :: Int64
    , traceTxReturnValue :: HexData
    , traceTxLogs :: [RpcTraceLog]
    } deriving (Show)

instance FromJSON RpcEthTraceTx where
  parseJSON (Object o) = RpcEthTraceTx
                     <$> o .: "gas"
                     <*> o .: "returnValue"
                     <*> o .: "structLogs"

data RpcEthFilterTopicValue =
    EthFilterTopicNull
  | EthFilterTopic HexData
  deriving (Show)

instance ToJSON RpcEthFilterTopicValue where
  toJSON EthFilterTopicNull = Null
  toJSON (EthFilterTopic h) = String h

data RpcEthFilterTopic =
    EthFilterTopicValue RpcEthFilterTopicValue
  | EthFilterOrTopics [RpcEthFilterTopicValue]
  deriving (Show)

instance ToJSON RpcEthFilterTopic where
  toJSON (EthFilterTopicValue tv) = toJSON tv
  toJSON (EthFilterOrTopics tvs) = toJSON tvs

data RpcEthFilter = RpcEthFilter
  { filterFromBlock :: Maybe RpcParamBlock
  , filterToBlock :: Maybe RpcParamBlock
  , filterAddress :: Maybe [HexEthAddr]
  , filterTopics :: Maybe [RpcEthFilterTopic]
  } deriving (Show)

instance ToJSON RpcEthFilter where
  toJSON (RpcEthFilter fb tb mas ts) =
    object $ toJSONListMaybe
      [ ("fromBlock", Just $ maybe (toJSON RPBLatest) toJSON fb)
      , ("toBlock", Just $ maybe (toJSON RPBLatest) toJSON tb)
      , ("address", mas >>= \as -> Just $ if length as == 1
                                            then toJSON (head as)
                                            else toJSON as )
      , ("topics", ts >>= Just . toJSON)
      ]

data RpcFilterLog = EthHashFilterLog HexHash256
                  | EthFilterLog RpcEthLog
                  deriving (Show)

instance FromJSON RpcFilterLog where
  parseJSON (String h) = return (EthHashFilterLog h)
  parseJSON v = EthFilterLog <$> parseJSON v

instance ToJSON Web3Rpc where
  toJSON Web3_web3_clientVersion = toArrayValue []
  toJSON (Web3_web3_sha3 dat) = toArrayValue [String dat]
  toJSON Web3_net_version = toArrayValue []
  toJSON Web3_net_listening = toArrayValue []
  toJSON Web3_net_peerCount = toArrayValue []
  toJSON Web3_eth_protocolVersion = toArrayValue []
  toJSON Web3_eth_syncing = toArrayValue []
  toJSON Web3_eth_coinbase = toArrayValue []
  toJSON Web3_eth_mining = toArrayValue []
  toJSON Web3_eth_hashrate = toArrayValue []
  toJSON Web3_eth_gasPrice = toArrayValue []
  toJSON Web3_eth_accounts = toArrayValue []
  toJSON Web3_eth_blockNumber = toArrayValue []
  toJSON (Web3_eth_getBalance addr blk) = toArrayValue [String $ getHexAddr addr, toJSON blk]
  toJSON (Web3_eth_getStorageAt addr idx blk) = toArrayValue [String $ getHexAddr addr, String idx, toJSON blk]
  toJSON (Web3_eth_getTransactionCount addr blk) = toArrayValue [String $ getHexAddr addr, toJSON blk]
  toJSON (Web3_eth_getBlockTransactionCountByHash h) = toArrayValue [String h]
  toJSON (Web3_eth_getBlockTransactionCountByNumber blk) = toArrayValue [toJSON blk]
  toJSON (Web3_eth_getUncleCountByBlockHash h) = toArrayValue [String h]
  toJSON (Web3_eth_getUncleCountByBlockNumber blk) = toArrayValue [toJSON blk]
  toJSON (Web3_eth_getCode addr blk) = toArrayValue [String $ getHexAddr addr, toJSON blk]
  toJSON (Web3_eth_sign addr dat) = toArrayValue [String $ getHexAddr addr, String dat]
  toJSON (Web3_eth_sendTransaction tx) = toArrayValue [toJSON tx]
  toJSON (Web3_eth_sendRawTransaction h) = toArrayValue [String h]
  toJSON (Web3_eth_call cTx blk) = toArrayValue [toJSON cTx, toJSON blk]
  toJSON (Web3_eth_estimateGas cTx) = toArrayValue [toJSON cTx]
  toJSON (Web3_eth_getBlockByHash h fullt) = toArrayValue [String h, Bool fullt]
  toJSON (Web3_eth_getBlockByNumber blk fullt) = toArrayValue [toJSON blk, Bool fullt]
  toJSON (Web3_eth_getTransactionByHash h) = toArrayValue [String h]
  toJSON (Web3_eth_getTransactionByBlockHashAndIndex h idx) = toArrayValue [String h, String idx]
  toJSON (Web3_eth_getTransactionByBlockNumberAndIndex blk idx) = toArrayValue [toJSON blk, String idx]
  toJSON (Web3_eth_getTransactionReceipt h) = toArrayValue [String h]
  toJSON (Web3_eth_getUncleByBlockHashAndIndex h idx) = toArrayValue [String h, String idx]
  toJSON (Web3_eth_getUncleByBlockNumberAndIndex blk idx) = toArrayValue [toJSON blk, String idx]
  toJSON (Web3_eth_newFilter f) = toArrayValue [toJSON f]
  toJSON Web3_eth_newBlockFilter = toArrayValue []
  toJSON Web3_eth_newPendingTransactionFilter = toArrayValue []
  toJSON (Web3_eth_uninstallFilter fi) = toArrayValue [String fi]
  toJSON (Web3_eth_getFilterChanges fi) = toArrayValue [String fi]
  toJSON (Web3_eth_getLogs f) = toArrayValue [toJSON f]
  toJSON (Web3_debug_dumpBlock blk) = toArrayValue [String blk]
  toJSON (Web3_debug_traceTransaction h ops) = toArrayValue [String h, toJSON ops]

instance ToRequest Web3Rpc where
  requestMethod Web3_web3_clientVersion = "web3_clientVersion"
  requestMethod (Web3_web3_sha3 _) = "web3_sha3"
  requestMethod Web3_net_version = "net_version"
  requestMethod Web3_net_listening = "net_listening"
  requestMethod Web3_net_peerCount = "net_peerCount"
  requestMethod Web3_eth_protocolVersion = "eth_protocolVersion"
  requestMethod Web3_eth_syncing = "eth_syncing"
  requestMethod Web3_eth_coinbase = "eth_coinbase"
  requestMethod Web3_eth_mining = "eth_mining"
  requestMethod Web3_eth_hashrate = "eth_hashrate"
  requestMethod Web3_eth_gasPrice = "eth_gasPrice"
  requestMethod Web3_eth_accounts = "eth_accounts"
  requestMethod Web3_eth_blockNumber = "eth_blockNumber"
  requestMethod (Web3_eth_getBalance _ _) = "eth_getBalance"
  requestMethod (Web3_eth_getStorageAt addr _ _) = "eth_getStorageAt"
  requestMethod (Web3_eth_getTransactionCount _ _) = "eth_getTransactionCount"
  requestMethod (Web3_eth_getBlockTransactionCountByHash _) = "eth_getBlockTransactionCountByHash"
  requestMethod (Web3_eth_getBlockTransactionCountByNumber _) = "eth_getBlockTransactionCountByNumber"
  requestMethod (Web3_eth_getUncleCountByBlockHash _) = "eth_getUncleCountByBlockHash"
  requestMethod (Web3_eth_getUncleCountByBlockNumber _) = "eth_getUncleCountByBlockNumber"
  requestMethod (Web3_eth_getCode _ _) = "eth_getCode"
  requestMethod (Web3_eth_sign _ _) = "eth_sign"
  requestMethod (Web3_eth_sendTransaction _) = "eth_sendTransaction"
  requestMethod (Web3_eth_sendRawTransaction _) = "eth_sendRawTransaction"
  requestMethod (Web3_eth_call _ _) = "eth_call"
  requestMethod (Web3_eth_estimateGas _) = "eth_estimateGas"
  requestMethod (Web3_eth_getBlockByHash _ _) = "eth_getBlockByHash"
  requestMethod (Web3_eth_getBlockByNumber _ _) = "eth_getBlockByNumber"
  requestMethod (Web3_eth_getTransactionByHash _) = "eth_getTransactionByHash"
  requestMethod (Web3_eth_getTransactionByBlockHashAndIndex _ _) = "eth_getTransactionByBlockHashAndIndex"
  requestMethod (Web3_eth_getTransactionByBlockNumberAndIndex _ _) = "eth_getTransactionByBlockNumberAndIndex"
  requestMethod (Web3_eth_getTransactionReceipt _) = "eth_getTransactionReceipt"
  requestMethod (Web3_eth_getUncleByBlockHashAndIndex _ _) = "eth_getUncleByBlockHashAndIndex"
  requestMethod (Web3_eth_getUncleByBlockNumberAndIndex _ _) = "eth_getUncleByBlockNumberAndIndex"
  requestMethod (Web3_eth_newFilter _) = "eth_newFilter"
  requestMethod Web3_eth_newBlockFilter = "eth_newBlockFilter"
  requestMethod Web3_eth_newPendingTransactionFilter = "eth_newPendingTransactionFilter"
  requestMethod (Web3_eth_uninstallFilter _) = "eth_uninstallFilter"
  requestMethod (Web3_eth_getFilterChanges _) = "eth_getFilterChanges"
  requestMethod (Web3_eth_getLogs _) = "eth_getLogs"
  requestMethod (Web3_debug_dumpBlock _) = "debug_dumpBlock"
  requestMethod (Web3_debug_traceTransaction _ _) = "debug_traceTransaction"

  requestIsNotif _ = False
