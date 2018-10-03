{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Development.KansasLava.Shake.Xilinx
       ( XilinxTarget(..)
       , papilioOne, papilioPro
       , xilinxRules
       ) where

import           Development.Shake          hiding ((~>))
import           Development.Shake.FilePath
import           Development.Shake.Config

import qualified Data.Text                  as T
import           Text.Mustache
import           Text.Mustache.Types
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)

import           Paths_kansas_lava_shake

data XilinxTarget = XilinxTarget{ targetFamily, targetDevice, targetPackage, targetSpeed :: String }

papilioPro :: XilinxTarget
papilioPro = XilinxTarget "Spartan6" "xc6slx9" "tqg144" "-2"

papilioOne :: XilinxTarget
papilioOne = XilinxTarget "Spartan3E" "xc3s500e" "vq100" "-5"

boards :: M.Map String XilinxTarget
boards = M.fromList
    [ ("papilio-pro", papilioPro)
    , ("papilio-one", papilioOne)
    ]

xilinxRules :: Maybe XilinxTarget -> FilePath -> String -> [FilePath] -> [FilePath] -> Rules ()
xilinxRules mtarget outDir projName srcs ipcores = do
    usingConfigFile "build.mk"

    outDir </> projName <.> "bit" %> \_out -> do
        need . concat $ [ [ outDir </> src | src <- srcs ]
                        , [ outDir </> "ipcore_dir" </> xco | xco <- ipcores ]
                        , [ outDir </> projName <.> "tcl" ]
                        ]

        xilinx "xtclsh" [projName <.> "tcl", "rebuild_project"]

    outDir </> "*.tcl" %> \out -> do
        target <- case mtarget of
            Just target -> return target
            Nothing -> do
                board <- fromMaybe (error "Please set BOARD in build.mk") <$> getConfig "BOARD"
                return $ fromMaybe (error $ unwords ["Unknown board:", board]) $ M.lookup board boards
        mustache (projCtxt target) out
  where
    xilinx tool args = do
        root <- getConfig "XILINX_ROOT"
        wrap <- getConfig "XILINX_WRAPPER"
        let exe = case (wrap, root) of
                (Just wrap, _) -> [wrap, tool]
                (Nothing, Just root) -> [root </> tool]
                (Nothing, Nothing) -> error "XILINX_ROOT or XILINX_WRAPPER must be set"
        cmd (Cwd outDir) exe args

    projCtxt XilinxTarget{..} = object $ [
        "project" ~=  projName,
        "targetFamily" ~= targetFamily,
        "targetDevice" ~= targetDevice,
        "targetSpeed" ~= targetSpeed,
        "targetPackage" ~= targetPackage,
        "ipcores" ~> (map (\n -> object ["name" ~> n]) $ map dropExtension ipcores),
        "srcs" ~> (map (\s -> object ["fileName" ~> s]) srcs)
        ]

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
