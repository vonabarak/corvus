{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Capnp.Rpc.Untyped
  ( -- * Connections to other vats
    Conn
  , ConnConfig (..)
  , handleConn
  , withConn
  , acquireConn
  , requestBootstrap

    -- * Clients for capabilities
  , Client
  , call
  , nullClient
  , newPromiseClient
  , IsClient (..)

    -- * Promise pipelining
  , Pipeline
  , walkPipelinePtr
  , pipelineClient
  , waitPipeline

    -- * Exporting local objects
  , export

    -- * Unwrapping local clients
  , unwrapServer

    -- * Waiting for resolution

    -- * Errors
  , RpcError (..)

    -- * Shutting down the connection
  )
where

import Internal.Rpc.Client
