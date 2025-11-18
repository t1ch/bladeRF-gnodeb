{-# language BangPatterns #-}
{-# language QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Domains where
import Clash.Explicit.Prelude

-- Create TxDom domain
createDomain vSystem{vName="TxDom"}

-- Create RxDom domain
createDomain vSystem{vName="RxDom"}
