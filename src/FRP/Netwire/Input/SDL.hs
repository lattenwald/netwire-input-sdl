{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module FRP.Netwire.Input.SDL (
    SDLInput, SDLInputT
  , SDLInputControl, SDLInputState
  , getInput, mkInputControl, pollSDL
) where


import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Trans.Resource
import Foreign
import Foreign.C
import qualified Data.Set as Set
import qualified Graphics.UI.SDL as SDL

import FRP.Netwire.Input

data SDLInputState = SDLInputState
                     { keysPressed  :: Set.Set SDL.Scancode
                     , keysReleased :: Set.Set SDL.Scancode
                     } deriving Show

emptyInput :: SDLInputState
emptyInput = SDLInputState { keysPressed  = Set.empty
                           , keysReleased = Set.empty }

instance Key SDL.Scancode

type SDLInput = State SDLInputState
type SDLInputT m = StateT SDLInputState m

instance Monad m => MonadKeyboard SDL.Scancode (StateT SDLInputState m) where
  keyIsPressed :: SDL.Scancode -> StateT SDLInputState m Bool
  keyIsPressed key = get >>= (return . isKeyPressed key)

  releaseKey :: SDL.Scancode -> StateT SDLInputState m ()
  releaseKey key = get >>= (put . debounceKey key)

isKeyPressed :: SDL.Scancode -> SDLInputState -> Bool
isKeyPressed key = (Set.member key) . keysPressed

debounceKey :: SDL.Scancode -> SDLInputState -> SDLInputState
debounceKey key input = input { keysPressed = Set.delete key (keysPressed input) }

data SDLInputControl = IptCtl (TVar SDLInputState) SDL.Window

getInput :: SDLInputControl -> IO (SDLInputState)
getInput (IptCtl var _) = readTVarIO var

setInput :: SDLInputControl -> SDLInputState -> IO ()
setInput (IptCtl var win) ipt = do
  atomically $ writeTVar var ipt

foreign import ccall "wrapper"
  wrap :: (Ptr () -> Ptr SDL.Event -> IO CInt) -> IO SDL.EventFilter

keyboardEventFilter :: SDLInputControl -> Ptr () -> Ptr SDL.Event -> IO CInt
keyboardEventFilter ctl _ ptrE = do
  e <- peek ptrE
  inp <- getInput ctl
  case SDL.eventType e of
   SDL.SDL_KEYDOWN -> do
     let
       key = SDL.keysymScancode $ SDL.keyboardEventKeysym e
       inp' = inp { keysPressed = Set.union (keysPressed inp) (Set.singleton key) }
     setInput ctl inp'
     return 1
   SDL.SDL_KEYUP -> do
     let
       key = SDL.keysymScancode $ SDL.keyboardEventKeysym e
       inp' = inp { keysPressed = Set.delete key (keysPressed inp) }
     setInput ctl inp'
     return 1
   _ -> return 0

mkInputControl :: SDL.Window -> IO (SDLInputControl)
mkInputControl win = do
  ctlvar <- newTVarIO emptyInput
  let ctl = IptCtl ctlvar win
  kbdFilter <- wrap (keyboardEventFilter ctl)
  SDL.setEventFilter kbdFilter nullPtr
  return ctl

pollSDL :: SDLInputControl -> IO SDLInputState
pollSDL ctl = do
  runResourceT $ snd <$> allocate malloc free >>= SDL.pollEvent
  getInput ctl
