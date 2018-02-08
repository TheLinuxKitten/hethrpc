{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.Web3
  (
  -- * Web3 Monad
    Web3T
  , showWeb3Session
  , web3SessionPollDelay
  , runWeb3IpcT
  , runWeb3HttpT
  , web3ThrowE
  , web3FromE
  -- ** Async
  , web3Fork
  , web3ForkCompleted
  , web3ForkWait
  , web3ForkCancel
  , Web3Async
  , web3Async
  , web3Wait
  , web3Poll
  , web3Cancel
  -- ** Filters
  , FilterId
  , web3FilterNew
  , web3FilterUninstall
  , web3FilterGetChanges
  -- * JSON-RPC API
  -- ** web3
  , web3_clientVersion
  , web3_sha3
  -- ** net
  , net_version
  , net_listening
  , net_peerCount
  -- ** eth
  , eth_protocolVersion
  , eth_syncing
  , eth_coinbase
  , eth_mining
  , eth_hashrate
  , eth_gasPrice
  , eth_accounts
  , eth_blockNumber
  , eth_getBalance
  , eth_getStorageAt
  , eth_getTransactionCount
  , eth_getBlockTransactionCountByHash
  , eth_getBlockTransactionCountByNumber
  , eth_getBlockTransactionCount
  , eth_getUncleCountByBlockHash
  , eth_getUncleCountByBlockNumber
  , eth_getUncleCount
  , eth_getCode
  , eth_sign
  , defParamTxContractNew
  , defParamTxContractCall
  , eth_sendTransaction
  , eth_sendRawTransaction
  , defParamCall
  , eth_call
  , defParamEstimateGasContractNew
  , eth_estimateGas
  , eth_getBlockByHash
  , eth_getBlockByNumber
  , eth_getBlock
  , eth_getTransactionByHash
  , eth_getTransactionByBlockHashAndIndex
  , eth_getTransactionByBlockNumberAndIndex
  , eth_getTransactionByBlockAndIndex
  , eth_getTransactionReceipt
  , eth_getUncleByBlockHashAndIndex
  , eth_getUncleByBlockNumberAndIndex
  , eth_getUncleByBlockAndIndex
  , eth_newFilter
  , eth_newBlockFilter
  , eth_newPendingTransactionFilter
  , eth_uninstallFilter
  , eth_getFilterChanges
  , eth_getFilterLogs
  , eth_getLogs
  -- ** debug
  , debug_dumpBlock
  , debug_traceTransaction
  -- * Web3 Types
  , module Network.Web3.HexText
  , module Network.Web3.Types
  -- * JSON-RPC Connection
  , module Network.JsonRpcConn
  -- * Aeson
  , module Data.Aeson.Types
  -- * Logger
  , module Control.Monad.Logger
  , module Control.Monad.Trans.Control
  ) where

import Data.Aeson.JsonRpc (JsonRpcVersion(..))
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.Logger
import Control.Monad.Trans.Control

import Network.JsonRpcCliHttp
import Network.JsonRpcCliIpc
import Network.JsonRpcConn
import Network.Web3.HexText
import Network.Web3.Types

import Control.Concurrent (ThreadId, threadDelay)
import Control.Concurrent.Async.Lifted
import Control.Concurrent.MVar.Lifted
import Control.Exception.Base (SomeException, AsyncException)
import Control.Exception.Lifted
import Control.Monad (void, when)
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid ((<>))
import Data.UUID (toText)
import Data.UUID.V4
import qualified Data.Vector as V

type Web3Async c m a = Async (StM (Web3T c m) a)

type FilterId = Text

data Web3Session c m = Web3Session
  { sessionForks :: MVar (HM.HashMap ThreadId (Web3Async c m ()))
  , sessionFilters :: MVar (HM.HashMap FilterId (Integer, ThreadId, MVar (V.Vector RpcFilterLog)))
  , sessionPollDelay :: Int
  , sessionGetFilterChangesDelay :: Int
  }

showWeb3Session :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                => Text -> Web3T c m ()
showWeb3Session titulo = do
  ses <- ask
  logInfoN $ "======================= " <> titulo <> " ========================"
  let sfrsv = sessionForks ses
  sfrs <- takeMVar sfrsv
  logInfoN $ T.pack $ "Session Forks: " ++ show (HM.keys sfrs)
  putMVar sfrsv sfrs
  let sflsv = sessionFilters ses
  sfls <- takeMVar sflsv
  mapM_ (\sfl -> do
      let (fi,ti,_) = sfls HM.! sfl
      logInfoN $ T.pack $ "Session Filter: [" ++ show sfl ++ ":"
                       ++ show fi ++ "," ++ show ti ++ "]") (HM.keys sfls)
  putMVar sflsv sfls

web3SessionPollDelay :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                     => Web3T c m Int
web3SessionPollDelay = sessionPollDelay <$> ask

-- | Monad para la ejecución de métodos RPC
type Web3T c m = ReaderT (Web3Session c m) (JsonRpcConnT c m)

web3ThrowE :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m, FromJSON a)
           => Text -> Web3T c m a
web3ThrowE = lift . rpcThrowE

web3FromE :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m, FromJSON a)
          => Either Text a -> Web3T c m a
web3FromE = lift . fromRpcE

-- | Ejecuta la acción Web3T asíncronamente.
web3Async :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
          => Web3T c m a -> Web3T c m (Web3Async c m a)
web3Async = asyncBound

-- | Espera que la acción asíncrona termine, y devuelve `Left` e si la
-- acción a lanzado una excepción e, o `Right` a si ha devuelto un valor a.
-- Ver `waitCatch`.
web3Wait :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
         => Web3Async c m a -> Web3T c m (Either SomeException a)
web3Wait asy = do
  let ti = asyncThreadId asy
  logDebugN $ T.pack $ "waitCatch thread id " ++ show ti
  waitCatch asy

-- | Versión Web3T de `poll`.
web3Poll :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
         => Web3Async c m a -> Web3T c m (Maybe (Either SomeException a))
web3Poll = poll

-- | Versión Web3T de `cancel`.
web3Cancel :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
           => Web3Async c m () -> Web3T c m ()
web3Cancel asy = do
  let ti = asyncThreadId asy
  logDebugN $ T.pack $ "cancel thread id " ++ show ti
  cancel asy

-- | Ejecuta la acción Web3T asíncronamente. Devuelve el `ThreadId`.
-- Al finalizar la conexión con el nodo Ethereum se cancelarán
-- (ver `cancel`) los threads que continuen en ejecución.
web3Fork :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
          => Web3T c m () -> Web3T c m ThreadId
web3Fork f = do
  asy <- web3Async f
  let ti = asyncThreadId asy
  logDebugN $ T.pack $ "Forked thread id " ++ show ti
  sfsv <- sessionForks <$> ask
  takeMVar sfsv >>= putMVar sfsv . HM.insert ti asy
  return ti

web3ForkAction :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m, FromJSON a)
               => Bool
               -> Text
               -> (Web3Async c m () -> Web3T c m a)
               -> ThreadId -> Web3T c m a
web3ForkAction delFork nf f ti = do
  sfsv <- sessionForks <$> ask
  sfs <- readMVar sfsv
  logDebugN $ T.pack $ "web3ForkAction: thread id " ++ show ti
  case HM.lookup ti sfs of
    Just sf -> do
      logDebugN $ (nf <>) $ T.pack $ ": thread id " ++ show ti
      f sf <* when delFork (takeMVar sfsv >>= putMVar sfsv . HM.delete ti)
    Nothing -> web3ThrowE $ nf <> ": ThreadId not found: " <> T.pack (show ti)

-- | Consulta al monad `Web3T` si el thread ha finalizado.
web3ForkCompleted :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                  => ThreadId -> Web3T c m Bool
web3ForkCompleted = web3ForkAction False "web3ForkCompleted" web3Poll'
  where
    web3Poll' :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
              => Web3Async c m () -> Web3T c m Bool
    web3Poll' asy = isJust <$> liftBase (poll asy)

-- | Espera la finalización del thread administrado por el monad `Web3T`.
-- Si el thread ha finalizado no hace nada.
web3ForkWait :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
             => ThreadId -> Web3T c m ()
web3ForkWait = web3ForkAction True "web3ForkWait" web3Wait'
  where
    web3Wait' :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
               => Web3Async c m () -> Web3T c m ()
    web3Wait' asy = do
      let ti = asyncThreadId asy
      logDebugN $ T.pack $ "wait thread id " ++ show ti
      void $ liftBase $ waitCatch asy

-- | Finaliza el thread administrado por el monad `Web3T`.
-- Si el thread ha finalizado no hace nada.
web3ForkCancel :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
               => ThreadId -> Web3T c m ()
web3ForkCancel = web3ForkAction True "web3ForkCancel" web3Cancel'
  where
    web3Cancel' :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                => Web3Async c m () -> Web3T c m ()
    web3Cancel' = web3Cancel

terminateForks :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
               => Web3T c m ()
terminateForks = ask >>= takeMVar . sessionForks >>= mapM_ terminate' . HM.elems
  where
    terminate' :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
               => Web3Async c m () -> Web3T c m ()
    terminate' asy = do
      logDebugN $ T.pack $ "Terminate thread id " ++ show (asyncThreadId asy)
      mr <- liftBase $ poll asy
      when (isNothing mr) $ web3Cancel asy

-- | Crea un nuevo filtro, en el nodo, que es administrado por el monad
-- `Web3T`. Devuelve el filter id.
web3FilterNew :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
              => RpcEthFilter   -- ^ Parámetros del filtro
              -> Web3T c m FilterId
web3FilterNew flt = do
  sfsv <- sessionFilters <$> ask
  sfs <- takeMVar sfsv
  uuid_ <- toText <$> liftIO nextRandom
  logDebugN $ T.pack
            $ "web3FilterInstall: installing filter id " ++ show uuid_
  fi <- eth_newFilter flt
  ti <- web3Fork (filterUpdater uuid_)
  flv <- liftBase (newMVar V.empty)
  putMVar sfsv (HM.insert uuid_ (fi,ti,flv) sfs)
  return uuid_

filterUpdate :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
             => FilterId -> Web3T c m ()
filterUpdate fi = do
  sfsv <- sessionFilters <$> ask
  sfs <- takeMVar sfsv
  case HM.lookup fi sfs of
    Nothing -> web3ThrowE $ "filterUpdate"
                         <> ": filter id not found: " <> T.pack (show fi)
    Just (fi',_,flsv) -> do
      fls <- takeMVar flsv
      fls2 <- eth_getFilterChanges fi'
      let fls' = if null fls2 then fls else fls V.++ V.fromList fls2
      logDebugN $ T.pack
                $ "filterUpdate: updating filter id " ++ show fi
      putMVar flsv fls'
  putMVar sfsv sfs
  return ()

filterUpdater :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
              => FilterId -> Web3T c m ()
filterUpdater fi = handle handleE filterUpdater'
  where
    filterUpdater' = do
      delay <- sessionGetFilterChangesDelay <$> ask
      liftIO (threadDelay $ delay*10^6)
      filterUpdate fi
      filterUpdater'
    handleE :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
            => AsyncException -> Web3T c m ()
    handleE e = do
      logDebugN $ T.pack
                $ "handle filterUpdater: uninstall filter id " ++ show fi
      filterUninstall False fi

filterUninstall :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                => Bool -> FilterId -> Web3T c m ()
filterUninstall cf fi = do
  sfsv <- sessionFilters <$> ask
  sfs <- takeMVar sfsv
  case HM.lookup fi sfs of
    Nothing -> web3ThrowE $ "filterUninstall"
                         <> ": filter id not found: " <> T.pack (show fi)
    Just (fi',ti,_) -> do
      logDebugN $ T.pack  $ "filterUninstall: uninstall filter id " ++ show fi
      void $ eth_uninstallFilter fi'
      when cf $ web3ForkCancel ti
  putMVar sfsv (HM.delete fi sfs)

-- | Desinstala el filtro del nodo y su administración del monad `Web3T`.
web3FilterUninstall :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                    => FilterId    -- ^ Filter id
                    -> Web3T c m ()
web3FilterUninstall = filterUninstall True

-- | Devuelve los `RpcFilterLog` producidos desde la última consulta al
-- monad `Web3T`.
web3FilterGetChanges :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                     => FilterId     -- ^ Filter id
                     -> Web3T c m [RpcFilterLog]
web3FilterGetChanges fi = do
  filterUpdate fi
  sfsv <- sessionFilters <$> ask
  sfs <- takeMVar sfsv
  fls <- case HM.lookup fi sfs of
          Nothing -> web3ThrowE $ "web3FilterGetChanges"
                               <> ": filter id not found: " <> T.pack (show fi)
          Just (_,_,flsv) -> do
            logDebugN $ T.pack
              $ "web3FilterGetChanges: getting changes of filter id " ++ show fi
            fls <- takeMVar flsv
            putMVar flsv V.empty
            return $ V.toList fls
  putMVar sfsv sfs
  return fls

runWeb3TSession :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                => Web3Session c m -> Web3T c m a -> JsonRpcConnT c m a
runWeb3TSession s f = runReaderT f s

runWeb3TNewSession :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                   => Int -> Int -> Web3T c m a -> JsonRpcConnT c m a
runWeb3TNewSession pollD filterD f = do
  vfork <- liftBase (newMVar HM.empty)
  vflt <- liftBase (newMVar HM.empty)
  runWeb3TSession (Web3Session vfork vflt pollD filterD) (procWeb3 f)
  where
    procWeb3 :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
             => Web3T c m a -> Web3T c m a
    procWeb3 f = f >>= \r -> terminateForks >> return r

-- | Ejecuta el monad sobre el socket IPC
runWeb3IpcT :: (MonadLoggerIO m, MonadBaseControl IO m, FromJSON a)
            => Int        -- ^ Poll delay (en segundos)
            -> Int        -- ^ Get filter changes delay (en segundos)
            -> FilePath   -- ^ Path del fichero geth.ipc
            -> Web3T AppDataUnix m a -> m (Either Text a)
runWeb3IpcT pollD filterD fp = runJsonRpcIpcT fp
                             . runWeb3TNewSession pollD filterD

-- | Ejecuta el monad sobre la interfaz HTTP
runWeb3HttpT :: (MonadLoggerIO m, MonadBaseControl IO m, FromJSON a)
             => Int        -- ^ Poll delay (en segundos)
             -> Int        -- ^ Get filter changes delay (en segundos)
             -> String     -- ^ URL
             -> Web3T Request m a -> m (Either Text a)
runWeb3HttpT pollD filterD url = runJsonRpcHttpT (parseRequest_ url)
                               . runWeb3TNewSession pollD filterD

web3SendJsonRpc :: ( JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m
                   , ToJSON r, ToRequest r, FromJSON a)
                => r -> Web3T c m a
web3SendJsonRpc = lift . sendJsonRpc JsonRpcV2

web3SendJsonRpc' :: ( JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m
                    , ToJSON r, ToRequest r, FromJSON a)
                 => r -> Web3T c m (Either Text a)
web3SendJsonRpc' = lift . sendJsonRpc' JsonRpcV2

-- | Devuelve la versión actual del cliente web3
web3_clientVersion :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                   => Web3T c m Text
web3_clientVersion = web3SendJsonRpc Web3_web3_clientVersion

-- | Devuelve el Keccak-256 (no el SHA3-256) de los datos
web3_sha3 :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
          => HexData    -- ^ Los datos a convertir
          -> Web3T c m HexHash256
web3_sha3 = web3SendJsonRpc . Web3_web3_sha3

-- | Devuelve el ID de red actual
net_version :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
            => Web3T c m Text
net_version = web3SendJsonRpc Web3_net_version

-- | Consulta si el cliente está escuchando conexiones de red
net_listening :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
              => Web3T c m Bool
net_listening = web3SendJsonRpc Web3_net_listening

-- | Devuelve el número de peers atualmente conectados al cliente
net_peerCount :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
              => Web3T c m Int
net_peerCount = fromHex <$> web3SendJsonRpc Web3_net_peerCount

-- | Devuelve la versión actual del protocolo ethereum
eth_protocolVersion :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                    => Web3T c m Text
eth_protocolVersion = web3SendJsonRpc Web3_eth_protocolVersion

-- | Devuelve información sobre el estado de la sincronización si está en curso
eth_syncing :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
            => Web3T c m (Maybe RpcEthSyncing)
eth_syncing = fromRpcMaybeBool <$> web3SendJsonRpc Web3_eth_syncing

-- | Devuelve la dirección coinbase del cliente
-- TODO: Debería devolver un `Maybe`
eth_coinbase :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
             => Web3T c m HexEthAddr
eth_coinbase = web3SendJsonRpc Web3_eth_coinbase

-- | Devuelve `True` si el cliente está minando nuevos bloques
eth_mining :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
           => Web3T c m Bool
eth_mining = web3SendJsonRpc Web3_eth_mining

-- | Devuelve el nº de hashes por segundo que está minando el nodo
eth_hashrate :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
             => Web3T c m Integer
eth_hashrate = fromHex <$> web3SendJsonRpc Web3_eth_hashrate

-- | Devuelve el precio actual del __gas__ en __wei__s
eth_gasPrice :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
             => Web3T c m Integer
eth_gasPrice = fromHex <$> web3SendJsonRpc Web3_eth_gasPrice

-- | Devuelve la lista de direcciones del cliente
eth_accounts :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
             => Web3T c m [HexEthAddr]
eth_accounts = web3SendJsonRpc Web3_eth_accounts

-- | Devuelve el número del bloque más reciente
eth_blockNumber :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                => Web3T c m Int64
eth_blockNumber = fromHex <$> web3SendJsonRpc Web3_eth_blockNumber

-- | Devuelve el balance, en __wei__s, de la cuenta de la dirección dada
eth_getBalance :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
               => HexEthAddr    -- ^ Dirección de la cuenta a comprobar
               -> RpcParamBlock -- ^ Bloque (según el estado de ethereum almacenado en ese bloque)
               -> Web3T c m Integer
eth_getBalance addr blk = fromHex
                      <$> web3SendJsonRpc (Web3_eth_getBalance addr blk)

-- | Devuelve el valor de una posición del almacenamiento de una dirección
-- TODO: Documentar el ejemplo usando esta librería
eth_getStorageAt :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                 => HexEthAddr      -- ^ Dirección del almacenamiento
                 -> Integer         -- ^ Posición en el almacenamiento
                 -> RpcParamBlock   -- ^ Bloque (estado) a consultar
                 -> Web3T c m HexData256
eth_getStorageAt addr num blk = web3SendJsonRpc $ Web3_eth_getStorageAt addr (toHex num) blk

-- | Devuelve el nº de transacciones enviadas desde una dirección (cuenta)
eth_getTransactionCount :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                        => HexEthAddr       -- ^ Dirección (cuenta)
                        -> RpcParamBlock    -- ^ Bloque
                        -> Web3T c m Int
eth_getTransactionCount addr blk =
  fromHex <$> web3SendJsonRpc (Web3_eth_getTransactionCount addr blk)

-- | Devuelve el nº de transacciones de un bloque
eth_getBlockTransactionCountByHash :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                                   => HexHash256    -- ^ Hash del bloque
                                   -> Web3T c m Int
eth_getBlockTransactionCountByHash h = fromMaybe 0 . fromRpcMaybeObj
                                   <$> web3SendJsonRpc (Web3_eth_getBlockTransactionCountByHash h)

-- | Devuelve el nº de transacciones de un bloque
eth_getBlockTransactionCountByNumber :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                                     => RpcParamBlock   -- ^ Bloque
                                     -> Web3T c m Int
eth_getBlockTransactionCountByNumber blk =
  fromHex <$> web3SendJsonRpc (Web3_eth_getBlockTransactionCountByNumber blk)

-- | Devuelve el nº de transacciones de un bloque
eth_getBlockTransactionCount :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                             => ParamBlock   -- ^ Bloque
                             -> Web3T c m Int
eth_getBlockTransactionCount blk = case blk of
    PBHex h -> eth_getBlockTransactionCountByHash h
    PBNum n -> eth_getBlockTransactionCountByNumber (RPBNum n)
    PBEarliest -> eth_getBlockTransactionCountByNumber RPBEarliest
    PBLatest -> eth_getBlockTransactionCountByNumber RPBLatest
    PBPending -> eth_getBlockTransactionCountByNumber RPBPending

-- | Devuelve el nº de __uncle__s de un bloque
eth_getUncleCountByBlockHash :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                             => HexHash256      -- ^ Hash del bloque
                             -> Web3T c m Int
eth_getUncleCountByBlockHash h = fromMaybe 0 . fromRpcMaybeObj
                             <$> web3SendJsonRpc (Web3_eth_getUncleCountByBlockHash h)

-- | Devuelve el nº de __uncle__s de un bloque
eth_getUncleCountByBlockNumber :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                               => RpcParamBlock     -- ^ Bloque
                               -> Web3T c m Int
eth_getUncleCountByBlockNumber blk =
  fromHex <$> web3SendJsonRpc (Web3_eth_getUncleCountByBlockNumber blk)

-- | Devuelve el nº de *uncle*s de un bloque
eth_getUncleCount :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                  => ParamBlock     -- ^ Bloque
                  -> Web3T c m Int
eth_getUncleCount blk = case blk of
    PBHex h -> eth_getUncleCountByBlockHash h
    PBNum n -> eth_getUncleCountByBlockNumber (RPBNum n)
    PBEarliest -> eth_getUncleCountByBlockNumber RPBEarliest
    PBLatest -> eth_getUncleCountByBlockNumber RPBLatest
    PBPending -> eth_getUncleCountByBlockNumber RPBPending

-- | Devuelve código dada una dirección (cuenta)
eth_getCode :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
            => HexEthAddr       -- ^ Dirección
            -> RpcParamBlock    -- ^ Bloque
            -> Web3T c m HexData
eth_getCode addr blk = web3SendJsonRpc $ Web3_eth_getCode addr blk

-- | Obtiene la firma de un mensaje. La dirección debe estar /unlocked/
eth_sign :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m, HexText a)
         => HexEthAddr      -- ^ Dirección
         -> a               -- ^ Mensaje a firmar
         -> Web3T c m HexEthSign     -- ^ Firma
eth_sign addr dat = web3SendJsonRpc $ Web3_eth_sign addr $ toHex dat

-- | Parámetros por defecto para la creación de un contract: /emisor/, /gas/ a usar y /datos/ del contract.
defParamTxContractNew f g d = RpcEthTx f Nothing (Just g) Nothing Nothing (Just d) Nothing

-- | Parámetros por defecto para la llamada a un método de un contract: /emisor/, /contract/ y /datos/ de la llamada.
defParamTxContractCall f t d = RpcEthTx f (Just t) Nothing Nothing Nothing (Just d) Nothing

-- | Crea un nuevo mensaje, una transacción o contract creation (si el campo /data/ contiene código). Devuelve el hash de la transacción, o el hash cero (/0x/) si la transacción no está disponible. Usar `web3_eth_getTransactionReceipt` para obtener la dirección del contract, despues de que la transacción sea minada.
eth_sendTransaction :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                    => RpcEthTx -> Web3T c m HexHash256
eth_sendTransaction = web3SendJsonRpc . Web3_eth_sendTransaction

-- | Crea un nuevo mensaje, una transacción o contract creation para transacciones firmadas
eth_sendRawTransaction :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                       => HexData       -- ^ La transacción firmada
                       -> Web3T c m HexHash256
eth_sendRawTransaction = web3SendJsonRpc . Web3_eth_sendRawTransaction

-- | Parámetros por defecto para la llamada a un método de un contract: /emisor/, /contract/ y /datos/ de la llamada.
defParamCall f t d = RpcEthMsgCall (Just f) (Just t) Nothing Nothing Nothing (Just d)

-- | Ejecuta un nuevo mensaje sin crear una transacción en el blockchain
eth_call :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
         => RpcEthMsgCall       -- ^ Transaction call
         -> RpcParamBlock       -- ^ Bloque
         -> Web3T c m HexData    -- ^ Valor devuelto por el contract ejecutado
eth_call msgCall blk = web3SendJsonRpc $ Web3_eth_call msgCall blk

-- | Parámetros por defecto para la estimación del gas necesario para la creación de un contract: /emisor/ y /datos/ del contract.
defParamEstimateGasContractNew f d = RpcEthMsgCall (Just f) Nothing Nothing Nothing Nothing (Just d)

-- | Realiza un /call/ o /transaction/, que no será añadida al blockchain, y
-- devuelve la cantidad de gas usado, que puede ser usada como cantidad estimada
-- de gas usado.
-- Si no se especifica un `msgGas` geth usa el gas limit del bloque pendiente
-- como límite máximo, por lo que puede no ser suficiente para ejecutar nuestra
-- call/transaction.
eth_estimateGas :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                => RpcEthMsgCall       -- ^ Transaction call
                -> Web3T c m (Either Text Int64)   -- ^ Cantidad de gas usado
eth_estimateGas msgCall = either Left (Right . fromHex)
                      <$> web3SendJsonRpc' (Web3_eth_estimateGas msgCall)

-- | Devuelve información sobre un bloque. Devuelve `Nothing` si el bloque no existe
eth_getBlockByHash :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                   => HexHash256        -- ^ Hash del bloque
                   -> Bool              -- ^ Si es `True` devuelve las transacciones completas, sino solo los hashes de las transacciones
                   -> Web3T c m (Maybe RpcEthBlock)
eth_getBlockByHash hex b = fromRpcMaybeObj
                       <$> web3SendJsonRpc (Web3_eth_getBlockByHash hex b)

-- | Devuelve información de un bloque
eth_getBlockByNumber :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                     => RpcParamBlock   -- ^ Bloque
                     -> Bool            -- ^ Devolver transacciones completas o sus hashes
                     -> Web3T c m (Maybe RpcEthBlock)
eth_getBlockByNumber blk b = fromRpcMaybeObj
                         <$> web3SendJsonRpc (Web3_eth_getBlockByNumber blk b)

-- | Devuelve información de un bloque
eth_getBlock :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
             => ParamBlock      -- ^ Bloque
             -> Bool            -- ^ Devolver transacciones completas o sus hashes
             -> Web3T c m (Maybe RpcEthBlock)
eth_getBlock blk b = case blk of
    PBHex h -> eth_getBlockByHash h b
    PBNum n -> eth_getBlockByNumber (RPBNum n) b
    PBEarliest -> eth_getBlockByNumber RPBEarliest b
    PBLatest -> eth_getBlockByNumber RPBLatest b
    PBPending -> eth_getBlockByNumber RPBPending b

-- | Devuelve información de la transacción solicitada
eth_getTransactionByHash :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                         => HexHash256      -- ^ Hash de la transacción
                         -> Web3T c m (Maybe RpcEthBlkTx)
eth_getTransactionByHash h = fromRpcMaybeObj
                         <$> web3SendJsonRpc (Web3_eth_getTransactionByHash h)

-- | Devuelve información de una transacción
eth_getTransactionByBlockHashAndIndex :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                                      => HexHash256     -- ^ Hash del bloque
                                      -> Int            -- ^ Indice de la transacción
                                      -> Web3T c m (Maybe RpcEthBlkTx)
eth_getTransactionByBlockHashAndIndex h i = fromRpcMaybeObj <$> web3SendJsonRpc (Web3_eth_getTransactionByBlockHashAndIndex h $ toHex i)

-- | Devuelve información de una transacción
eth_getTransactionByBlockNumberAndIndex :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                                        => RpcParamBlock    -- ^ Bloque
                                        -> Int              -- ^ Indice
                                        -> Web3T c m (Maybe RpcEthBlkTx)
eth_getTransactionByBlockNumberAndIndex blk i = fromRpcMaybeObj <$> web3SendJsonRpc (Web3_eth_getTransactionByBlockNumberAndIndex blk $ toHex i)

-- | Devuelve información de una transacción
eth_getTransactionByBlockAndIndex :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                                  => ParamBlock       -- ^ Bloque
                                  -> Int              -- ^ Indice
                                  -> Web3T c m (Maybe RpcEthBlkTx)
eth_getTransactionByBlockAndIndex blk i = case blk of
    PBHex h -> eth_getTransactionByBlockHashAndIndex h i
    PBNum n -> eth_getTransactionByBlockNumberAndIndex (RPBNum n) i
    PBEarliest -> eth_getTransactionByBlockNumberAndIndex RPBEarliest i
    PBLatest -> eth_getTransactionByBlockNumberAndIndex RPBLatest i
    PBPending -> eth_getTransactionByBlockNumberAndIndex RPBPending i

-- | Devuelve el receptor de una transacción
eth_getTransactionReceipt :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                          => HexHash256     -- ^ Hash de la transacción
                          -> Web3T c m (Maybe RpcEthTxReceipt)
eth_getTransactionReceipt h = fromRpcMaybeObj
                          <$> web3SendJsonRpc (Web3_eth_getTransactionReceipt h)

-- | Devuelve información de un uncle de un bloque
eth_getUncleByBlockHashAndIndex :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                                => HexHash256   -- ^ Hash del bloque
                                -> Int          -- ^ Indice del uncle
                                -> Web3T c m (Maybe RpcEthBlock)
eth_getUncleByBlockHashAndIndex h i = fromRpcMaybeObj <$> web3SendJsonRpc (Web3_eth_getUncleByBlockHashAndIndex h $ toHex i)

-- | Devuelve información de un uncle de un bloque
eth_getUncleByBlockNumberAndIndex :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                                  => RpcParamBlock  -- ^ Bloque
                                  -> Int            -- ^ Indice
                                  -> Web3T c m (Maybe RpcEthBlock)
eth_getUncleByBlockNumberAndIndex blk i = fromRpcMaybeObj <$> web3SendJsonRpc (Web3_eth_getUncleByBlockNumberAndIndex blk $ toHex i)

-- | Devuelve información de un uncle de un bloque
eth_getUncleByBlockAndIndex :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                            => ParamBlock  -- ^ Bloque
                            -> Int            -- ^ Indice
                            -> Web3T c m (Maybe RpcEthBlock)
eth_getUncleByBlockAndIndex blk i = case blk of
    PBHex h -> eth_getUncleByBlockHashAndIndex h i
    PBNum n -> eth_getUncleByBlockNumberAndIndex (RPBNum n) i
    PBEarliest -> eth_getUncleByBlockNumberAndIndex RPBEarliest i
    PBLatest -> eth_getUncleByBlockNumberAndIndex RPBLatest i
    PBPending -> eth_getUncleByBlockNumberAndIndex RPBPending i

-- | Crea un objeto filter en el nodo para notificar cuando cambia el estado (logs).
eth_newFilter :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
              => RpcEthFilter   -- ^ Opciones del filter
              -> Web3T c m Integer
eth_newFilter fo = fromHex <$> web3SendJsonRpc (Web3_eth_newFilter fo)

-- | Crea un objeto filter en el nodo para notificar cuando llega un nuevo bloque.
eth_newBlockFilter :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                   => Web3T c m Integer
eth_newBlockFilter = fromHex <$> web3SendJsonRpc Web3_eth_newBlockFilter

-- | Crea un objeto filter en el nodo para notificar cuando llega una nueva transacción pendiente.
eth_newPendingTransactionFilter :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                                => Web3T c m Integer
eth_newPendingTransactionFilter = fromHex <$> web3SendJsonRpc Web3_eth_newBlockFilter

-- | Desinstala un filter dado el id. Debe ser llamado cuando ya no sea necesario filtrar cambios de estado. Tener en cuenta que los filters tienen un timeout que se reinicia con las llamadas a `eth_getFilterChanges`. Devuelve `True` si se ha desinstalado, sino `False`.
eth_uninstallFilter :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                    => Integer  -- ^ Filter id
                    -> Web3T c m Bool
eth_uninstallFilter = web3SendJsonRpc . Web3_eth_uninstallFilter . toHex

-- | Método de consulta de un filter. Devuelve un array con los logs producidos desde la última consulta. Para filtros creados con `eth_newBlockFilter` devuelve los hashes de los bloques; para filtros creados con `eth_newPendingTransactionFilter` devuelve los hashes de las transacciones, y para filtros creados con `eth_newFilter` devuelve objetos log.
eth_getFilterChanges :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                     => Integer   -- ^ Filter id
                     -> Web3T c m [RpcFilterLog]
eth_getFilterChanges = web3SendJsonRpc . Web3_eth_getFilterChanges . toHex

eth_getFilterLogs :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                  => Integer   -- ^ Filter id
                  -> Web3T c m [RpcFilterLog]
eth_getFilterLogs = eth_getFilterChanges

-- | Devuelve los logs, ya emitidos, que cumplen con los parámetros del filtro.
eth_getLogs :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
            => RpcEthFilter   -- ^ Opciones del filter
            -> Web3T c m [RpcFilterLog]
eth_getLogs = web3SendJsonRpc . Web3_eth_getLogs

-- | Recupera el estado de Ethereum correspondiente al bloque y devuelve
-- una lista de accounts que incluyen storage y code.
debug_dumpBlock :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                => Int64   -- ^ Bloque
                -> Web3T c m RpcEthState
debug_dumpBlock = web3SendJsonRpc . Web3_debug_dumpBlock . toHex

-- | Devuelve el trace o ejecución de una transacción
debug_traceTransaction :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                       => HexHash256        -- ^ Hash de la transacción
                       -> RpcTraceOptions   -- ^ Opciones
                       -> Web3T c m RpcEthTraceTx
debug_traceTransaction h ops = web3SendJsonRpc $ Web3_debug_traceTransaction h ops

{-
eth_getLogs
eth_getWork
eth_submitWork
eth_submitHashrate
db_putString
db_getString
db_putHex
db_getHex
shh_version
shh_post
shh_newIdentity
shh_hasIdentity
shh_newGroup
shh_addToGroup
shh_newFilter
shh_uninstallFilter
shh_getFilterChanges
shh_getMessages
-}

{-
Web3_web3_clientVersion
(Web3_web3_sha3 dat)
Web3_net_version
Web3_net_listening
Web3_net_peerCount
Web3_eth_protocolVersion
Web3_eth_syncing
Web3_eth_coinbase
Web3_eth_mining
Web3_eth_hashrate
Web3_eth_gasPrice
Web3_eth_accounts
Web3_eth_blockNumber
(Web3_eth_getBalance addr blk)
(Web3_eth_getStorageAt addr idx blk)
(Web3_eth_getTransactionCount addr blk)
(Web3_eth_getBlockTransactionCountByHash h)
(Web3_eth_getBlockTransactionCountByNumber blk)
(Web3_eth_getUncleCountByBlockHash h)
(Web3_eth_getUncleCountByBlockNumber blk)
(Web3_eth_getCode addr blk)
(Web3_eth_sign addr dat)
(Web3_eth_sendTransaction tx)
(Web3_eth_sendRawTransaction h)
(Web3_eth_call cTx blk)
(Web3_eth_getBlockByHash h fullt)
(Web3_eth_getBlockByNumber blk fullt)
(Web3_eth_getTransactionByHash h)
(Web3_eth_getTransactionByBlockHashAndIndex h idx)
(Web3_eth_getTransactionByBlockNumberAndIndex blk idx)
(Web3_eth_getTransactionReceipt h)
(Web3_eth_getUncleByBlockHashAndIndex h idx)
(Web3_eth_getUncleByBlockNumberAndIndex blk idx)
-}
