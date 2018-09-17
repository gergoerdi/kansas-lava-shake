{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Development.KansasLava.Shake.Xilinx
       ( XilinxConfig(..), XilinxTarget(..)
       , xilinxRules
       ) where

import           Development.Shake          hiding ((~>))
import           Development.Shake.FilePath

import qualified Data.Text                  as T
import           System.Environment         (lookupEnv)
import           Text.Mustache
import           Text.Mustache.Types

import           Paths_kansas_lava_shake

data XilinxTarget = XilinxTarget{ targetFamily, targetDevice, targetSpeed, targetPackage :: String }

data XilinxConfig = XilinxConfig{ xilinxRoot   :: FilePath
                                , xilinxTarget :: XilinxTarget
                                }

xilinxRules :: XilinxConfig -> FilePath -> String -> [FilePath] -> [FilePath] -> Rules ()
xilinxRules XilinxConfig{..} outDir projName srcs ipcores = do
    outDir </> projName <.> "bit" %> \_out -> do
        need . concat $ [ [ outDir </> src | src <- srcs ]
                        , [ outDir </> "ipcore_dir" </> xco | xco <- ipcores ]
                        , [ outDir </> projName <.> "tcl" ]
                        ]

        xilinx "xtclsh" [projName <.> "tcl", "rebuild_project"]

    outDir </> "*.tcl" %> do
        mustache projCtxt
  where
    xilinx tool args = liftIO $ do
      xilinxRootEnv <- lookupEnv "KANSAS_LAVA_XILINX_ROOT" -- optional xtclsh dir
      cmd (Cwd outDir) ((maybe xilinxRoot id xilinxRootEnv) </> tool) args

    projCtxt = object $ [
        "project" ~=  projName,
        "targetFamily" ~= targetFamily,
        "targetDevice" ~= targetDevice,
        "targetSpeed" ~= targetSpeed,
        "targetPackage" ~= targetPackage,
        "ipcores" ~> (map (\n -> object ["name" ~> n]) $ map dropExtension ipcores),
        "srcs" ~> (map (\s -> object ["fileName" ~> s]) srcs)
        ]
      where
        XilinxTarget{..} = xilinxTarget

mustache :: Value -> FilePath -> Action ()
mustache ctxt target = do
    alwaysRerun
    rSrc <- liftIO $ getDataFileName ("ise.template" </> templateName)
    withTempDir $ \tempDir -> do
      copyFileChanged rSrc (tempDir </> templateName)
      tE <- liftIO $ automaticCompile [tempDir] templateName
      case tE of
        Left err ->
          liftIO $ error $ "KansasLava.Shake.Xilinx - mustache template problem: " ++ show err
        Right template -> do
          let st = substitute template ctxt
          writeFileChanged target $ T.unpack st
  where
    ext = drop 1 . takeExtension $ target
    templateName = ext <.> "mustache"
