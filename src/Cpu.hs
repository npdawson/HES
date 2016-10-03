module Cpu where

-- import Instruction
import Control.Monad.Reader
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Unboxed as V
import Data.Word

data CpuState s = CpuState
  {
    regA :: STRef s Word8
  , regX :: STRef s Word8
  , regY :: STRef s Word8
  , pc   :: STRef s Word16
  , sp   :: STRef s Word8
  , regP :: STRef s Word8
  , rom  :: V.Vector Word8
  , ram  :: VM.MVector s Word8
  }

type CPU s = ReaderT () (ST s)
