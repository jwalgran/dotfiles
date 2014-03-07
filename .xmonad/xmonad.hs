import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
  xmproc <- spawnPipe "/home/likewise-open/AZVA-INT/jwalgran/.cabal/bin/xmobar /home/likewise-open/AZVA-INT/jwalgran/.xmobarrc"
  xmonad $ defaultConfig {
         modMask = mod4Mask,
         manageHook = manageDocks <+> manageHook defaultConfig,
         layoutHook = avoidStruts  $  layoutHook defaultConfig,
         logHook = dynamicLogWithPP xmobarPP
                       { ppOutput = hPutStrLn xmproc,
                         ppTitle = xmobarColor "green" "" . shorten 80
                       }
         }
