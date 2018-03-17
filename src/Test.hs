{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test
where

import Graphics.Wayland.Scanner
import Graphics.Wayland.Scanner.WLS

-- runScanner (protocolFromFile "/usr/share/wayland-protocols/stable/xdg-shell/xdg-shell.xml") $ ScannerEnv mempty

runScanner (protocolFromFile "/tmp/test.xml") $ ScannerEnv mempty
