{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Examples.Rpc.CalculatorClient (main) where

import qualified Capnp as C
import Capnp.Gen.Calculator
import Capnp.Rpc
  ( ConnConfig (..)
  , fromClient
  , requestBootstrap
  , socketTransport
  , withConn
  )
import Control.Monad (when)
import Data.Function ((&))
import Network.Simple.TCP (connect)

main :: IO ()
main = connect "localhost" "4000" $ \(sock, _addr) ->
  withConn (socketTransport sock C.defaultLimit) C.def {debugMode = True} $ \conn -> do
    client <- requestBootstrap conn
    let calc :: C.Client Calculator
        calc = fromClient client

    literalValue <-
      ( ( calc
            & C.callP
              #evaluate
              C.def {expression = Expression $ Expression'literal 123}
        )
          >>= C.callR #read C.def . C.pipe #value
        )
        >>= C.waitPipeline
        >>= C.evalLimitT C.defaultLimit . C.parseField #value
    assertEq literalValue 123

    let getOp op =
          (calc & C.callP #getOperator C.def {op})
            >>= C.asClient . C.pipe #func

    add <- getOp Operator'add
    subtractOp <- getOp Operator'subtract

    value <-
      ( ( calc
            & C.callP
              #evaluate
              C.def
                { expression =
                    Expression $
                      Expression'call
                        Expression'call'
                          { function = subtractOp
                          , params =
                              [ Expression $
                                  Expression'call
                                    Expression'call'
                                      { function = add
                                      , params =
                                          [ Expression $ Expression'literal 123
                                          , Expression $ Expression'literal 45
                                          ]
                                      }
                              , Expression $ Expression'literal 67
                              ]
                          }
                }
        )
          >>= C.callR #read C.def . C.pipe #value
        )
        >>= C.waitPipeline
        >>= C.evalLimitT C.defaultLimit . C.parseField #value
    assertEq value 101
    putStrLn "PASS"

assertEq :: (Show a, Eq a) => a -> a -> IO ()
assertEq got want =
  when (got /= want) $
    error $
      "Got " ++ show got ++ " but wanted " ++ show want
