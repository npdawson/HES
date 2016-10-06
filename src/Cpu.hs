{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cpu where

-- import Instruction
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Word

data CpuConfig = CpuConfig
  { rom  :: B.ByteString -- TODO create Cart type with mapper functionality
    -- ram is only a reference to a mutable vector, so it can be
    -- passed around in the Reader monad
  , ram  :: VM.IOVector Word8
  }

data CpuState = CpuState
  { regA :: Word8
  , regX :: Word8
  , regY :: Word8
  , regP :: Word8
  , pc   :: Word16
  , sp   :: Word8
  }

newtype CPU a = CPU { runC :: ReaderT CpuConfig (StateT CpuState IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader CpuConfig, MonadState CpuState )

runCPU :: CPU a -> B.ByteString -> IO (a, CpuState)
runCPU cpu rom0 = do
  ram0 <- VM.replicate 65536 0
  let config = CpuConfig
        { rom = rom0
        , ram = ram0
        }
      initState = CpuState
        { regA = 0
        , regX = 0
        , regY = 0
        , regP = 0
        , pc = 0
        , sp = 0
        }
    in runStateT (runReaderT (runC cpu) config) initState
