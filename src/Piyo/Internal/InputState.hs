module Piyo.Internal.InputState
    ( updateInputs
    , mkAction
    , extractPayload
    , payloadToAction
    , getKey
    )
where

import qualified SDL
import           Data.List
import           Piyo.Internal.Common
import           Piyo.Internal.Types


updateInputs :: KeyMotion -> [Input] -> [Input]
updateInputs (Pressed  i) is = union is [i]
updateInputs (Released i) is = is \\ [i]

mkAction :: Maybe SDL.Event -> Action
mkAction = maybe Idle (payloadToAction . extractPayload)

extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _ p) = p

payloadToAction :: SDL.EventPayload -> Action
payloadToAction SDL.QuitEvent         = Quit
payloadToAction (SDL.KeyboardEvent k) = getKey k
payloadToAction _                     = Idle

getKey :: SDL.KeyboardEventData -> Action
getKey (SDL.KeyboardEventData _ SDL.Pressed False keysym) =
    case SDL.keysymKeycode keysym of
        SDL.KeycodeEscape -> Quit
        SDL.KeycodeUp     -> KeyInput (Pressed BtnUp)
        SDL.KeycodeDown   -> KeyInput (Pressed BtnDown)
        SDL.KeycodeLeft   -> KeyInput (Pressed BtnLeft)
        SDL.KeycodeRight  -> KeyInput (Pressed BtnRight)
        SDL.KeycodeZ      -> KeyInput (Pressed BtnA)
        SDL.KeycodeX      -> KeyInput (Pressed BtnB)
        _                 -> Idle
getKey (SDL.KeyboardEventData _ SDL.Released False keysym) =
    case SDL.keysymKeycode keysym of
        SDL.KeycodeEscape -> Quit
        SDL.KeycodeUp     -> KeyInput (Released BtnUp)
        SDL.KeycodeDown   -> KeyInput (Released BtnDown)
        SDL.KeycodeLeft   -> KeyInput (Released BtnLeft)
        SDL.KeycodeRight  -> KeyInput (Released BtnRight)
        SDL.KeycodeZ      -> KeyInput (Pressed BtnA)
        SDL.KeycodeX      -> KeyInput (Pressed BtnB)
        _                 -> Idle
getKey _ = Idle
