{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Web3.Extra
  ( guardMining
  , web3_sendTx
  , web3_estimateAndSendTx
  , web3_estimateAndSendTx'
  , web3_estimateAndSendTxs
  , web3_call
  , web3_callPure
  , web3_guardBin
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception.Base
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.Web3

-- | Ejecuta acción web3 comprobando antes que el nodo está minando
guardMining :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
            => Web3T c m (Either Text a)   -- ^ Acción web3
            -> Web3T c m (Either Text a)
guardMining f = do
  mining <- eth_mining
  if mining then f else return (Left "Node is not mining!!!")

sendTx :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
       => HexEthAddr -> Maybe HexEthAddr -> Maybe Int64
       -> Maybe Integer -> Maybe Integer -> Maybe HexData
       -> Web3T c m (Int64, HexHash256)
sendTx addrFrom mAddrTo mGas mGasPrice mValue mData = do
  let tx = RpcEthTx addrFrom mAddrTo mGas mGasPrice mValue mData Nothing
  txh <- eth_sendTransaction tx
  blkNumIni <- eth_blockNumber
  return (blkNumIni, txh)

pollTxr :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
        => (Int64, HexHash256)
        -> Web3T c m (Either Text RpcEthTxReceipt)
pollTxr p@(blkNumIni, txh) = do
  delay <- web3SessionPollDelay
  liftIO (threadDelay $ delay * 10^6)
  mtxr <- eth_getTransactionReceipt txh
  case mtxr of
    Nothing -> do
      mining <- eth_mining
      blkNum <- eth_blockNumber
      if mining
        then do
          if blkNum > (blkNumIni + 2)
            then return $ Left "pollTxr: Transaction not included!!!"
            else pollTxr p
        else do
          logDebugN "pollTxr: Node is not mining!!! Trying again..."
          pollTxr (blkNum, txh)
    Just txr -> return $ Right txr

estimateSendTx addrFrom mAddrTo mValue mData = do
  let msg = RpcEthMsgCall (Just addrFrom) mAddrTo Nothing Nothing mValue mData
  eth_estimateGas msg

sendError :: String -> HexEthAddr -> Maybe HexEthAddr -> Maybe Integer -> Maybe HexData -> Text -> Text
sendError nom addrFrom mAddrTo mValue mData msgErr =
  T.pack $ nom ++ " " ++ show addrFrom ++ " " ++ show mAddrTo ++ " "
        ++ show mValue ++ " " ++ show mData ++ ": " ++ T.unpack msgErr

-- | Envia una transacción, espera a que sea minado el bloque que la incluye,
-- y devuelve el receipt de la transacción.
web3_sendTx :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
            => HexEthAddr         -- ^ Dirección del emisor
            -> Maybe HexEthAddr   -- ^ Dirección del destinatario
            -> Maybe Int64        -- ^ Gas para la ejecución de la transacción
            -> Maybe Integer      -- ^ Precio del gas usado
            -> Maybe Integer      -- ^ Valor enviado junto a la transacción
            -> Maybe HexData      -- ^ Datos
            -> Web3T c m (Either Text RpcEthTxReceipt)
web3_sendTx addrFrom mAddrTo mGas mGasPrice mValue mData = guardMining $
  sendTx addrFrom mAddrTo mGas mGasPrice mValue mData >>= pollTxr

-- | Estima el coste de envio de la transacción y la envia (ver `web3_sendTx`).
web3_estimateAndSendTx :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                       => HexEthAddr         -- ^ Dirección del emisor
                       -> Maybe HexEthAddr   -- ^ Dirección del destinatario
                       -> Maybe Integer -- ^ Valor enviado junto a la transacción
                       -> Maybe HexData -- ^ Datos
                       -> Web3T c m (Either Text RpcEthTxReceipt)
web3_estimateAndSendTx addrFrom mAddrTo mValue mData = do
  egas <- estimateSendTx addrFrom mAddrTo mValue mData
  case egas of
    Left e -> return $ Left $ sendError "web3_estimateAndSendTx" addrFrom mAddrTo mValue mData e
    Right gas -> web3_sendTx addrFrom mAddrTo (Just gas) Nothing mValue mData

web3_estimateAndSendTx' :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                        => (HexEthAddr, Maybe HexEthAddr, Maybe Integer, Maybe HexData)   -- ^ Dirección del emisor, del receptor, valor enviado y datos
                       -> Web3T c m (Either Text RpcEthTxReceipt)
web3_estimateAndSendTx' (addrFrom, mAddrTo, mValue, mData) =
  web3_estimateAndSendTx addrFrom mAddrTo mValue mData

-- | Envia una lista de transacciones y espera a que finalizen, devolviendo
-- la lista de resultados. Las transacciones se envian secuencialmente; lo
-- que no asegura que los nodos las incluyan en ese orden en el bloque a minar.
web3_estimateAndSendTxs :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                        => [(HexEthAddr, Maybe HexEthAddr, Maybe Integer, Maybe HexData)]   -- ^ Dirección del emisor, del receptor, valor enviado y datos
                        -> Web3T c m [Either Text RpcEthTxReceipt]
web3_estimateAndSendTxs txds = mapM mySendTx txds
                           >>= mapM myAsync
                           >>= mapM myWait'
  where
    mySendTx :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
             => (HexEthAddr, Maybe HexEthAddr, Maybe Integer, Maybe HexData)
             -> Web3T c m (Either Text (Int64,HexHash256))
    mySendTx txd@(f,t,v,d) = guardMining $ do
      egas <- estimateSendTx f t v d
      case egas of
        Left e -> return $ Left $ sendError "web3_estimateAndSendTxs" f t v d e
        Right gas -> do
          !txh <- sendTx f t (Just gas) Nothing v d
          return (Right txh)
    myAsync :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
            => Either Text (Int64,HexHash256)
            -> Web3T c m (Either Text (Web3Async c m (Either Text RpcEthTxReceipt)))
    myAsync = either (return . Left) (\a -> Right <$> web3Async (pollTxr a))
    myWait' :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
            => Either Text (Web3Async c m (Either Text RpcEthTxReceipt))
            -> Web3T c m (Either Text RpcEthTxReceipt)
    myWait' = either (return . Left) myWait
    myWait :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
           => Web3Async c m (Either Text RpcEthTxReceipt)
           -> Web3T c m (Either Text RpcEthTxReceipt)
    myWait asy = either (Left . T.pack . displayException) id <$> web3Wait asy

-- | LLama a una función (de tipo view) del contract
web3_call :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
          => HexEthAddr   -- ^ Dirección del emisor
          -> HexEthAddr   -- ^ Dirección del contrato
          -> HexData      -- ^ Datos
          -> Web3T c m HexData
web3_call addrFrom addrTo dat =
  eth_call (RpcEthMsgCall (Just addrFrom) (Just addrTo) Nothing Nothing Nothing (Just dat)) RPBLatest

-- | LLama a una función (de tipo pure) del contract
web3_callPure :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
              => HexEthAddr   -- ^ Dirección del contrato
              -> HexData      -- ^ Datos
              -> Web3T c m HexData
web3_callPure addrTo dat =
  eth_call (RpcEthMsgCall Nothing (Just addrTo) Nothing Nothing Nothing (Just dat)) RPBLatest

web3_guardBin :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
              => HexData -> HexEthAddr -> Web3T c m ()
web3_guardBin binCode addr = do
  code <- eth_getCode addr RPBLatest
  unless (code == binCode)
    $ web3ThrowE $ "Node's contract code ("
                <> code <> ") doesn't match object's contract code ("
                <> binCode <> ")"

