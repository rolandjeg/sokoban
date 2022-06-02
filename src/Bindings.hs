module Bindings where

import SDL

bindings :: String -> Keycode
bindings "q" = SDL.KeycodeQ
bindings "reset" = SDL.KeycodeR
