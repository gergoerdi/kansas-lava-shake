{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Development.KansasLava.Shake.Xilinx
       ( XilinxConfig(..)
       , xilinxRules
       ) where

import Development.Shake
import Development.Shake.FilePath
import System.Directory

import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Data.String (fromString)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.Hastache

import Paths_kansas_lava_shake

data XilinxConfig = XilinxConfig{ xilinxRoot :: FilePath
                                , xilinxPlatform :: String
                                }

xilinxRules :: XilinxConfig
            -> String
            -> [String]
            -> Rules ()
xilinxRules XilinxConfig{..} mod xaws = do
    "*.ut" %>
        textTemplate []

    "*.xst" %>
        textTemplate [ ("MAIN", fromString mod)
                     , ("TOP", fromString mod)
                     , ("PLATFORM", fromString xilinxPlatform)
                     ]

    "*.xise" %>
        listTemplate "components" xiseFiles

    "*.prj" %> \target -> do
        let vhdlWork baseName = mconcat ["vhdl work \"", baseName <.> "vhdl", "\""]
        liftIO $ writeFile target . unlines $
          map (vhdlWork . gensrc) vhdls ++ map (vhdlWork . xawsrc) xaws

    xawsrc "*.vhdl" %> \target -> do
        let xaw = ".." </> "xaw" </> takeFileName target -<.> "xaw"
        need [xaw]
        removeFilesAfter "." ["xaw2vhdl.log"]
        xilinx "xaw2vhdl" [xaw, "-st", "XST", target]
    xawsrc "*.ucf" %> \target -> need [target -<.> "vhdl"]

    "*.ngc" %> \target -> do
        liftIO $ createDirectoryIfMissing True "xst/projnav.tmp"
        need $
          (target -<.> "prj"):
          (target -<.> "xst"):
          [gensrc $ f <.> "vhdl" | f <- vhdls] ++
          [xawsrc $ f <.> "vhdl" | f <- xaws]

        removeFilesAfter "."
          [ "xst//*"
          , "_xmsgs//*"
          , target -<.> "lso"
          , target -<.> "ngr"
          , target -<.> "syr"
          , "*.xrpt"
          ]
        xilinx "xst" [ "-ifn", target -<.> "xst"
                     , "-ofn", target -<.> "syr"
                     ]

    "*.ngd" %> \target -> do
        liftIO $ createDirectoryIfMissing True "xst/projnav.tmp"
        let ucf = gensrc $ target -<.> "ucf"
        need [ target -<.> "ngc"
             , ucf
             ]
        removeFilesAfter "."
          [ target -<.> "bld"
          , "ngo//*"
          , "xlnx_auto_0_xdb//*"
          , "*.xrpt"
          ]
        xilinx "ngdbuild" [ "-dd", "ngo"
                          , "-nt", "timestamp"
                          , "-uc", ucf
                          , "-p", xilinxPlatform
                          , target -<.> "ngc"
                          , target
                          ]

    "*.pcf" %> \target -> do
        liftIO $ createDirectoryIfMissing True "xst/projnav.tmp"
        need [ target -<.> "ngc"
             , target -<.> "ngd"
             ]
        removeFilesAfter "."
          [ "*_summary.xml"
          , "*_usage.xml"
          , "xilinx_device_details.xml"
          , target -<.> "ngm"
          , target -<.> "mrp"
          , target -<.> "map"
          ]
        xilinx "map" [ "-p", xilinxPlatform
                     , "-ir", "off"
                     , "-pr", "off"
                     , "-c", "100"
                     , "-w"
                     , "-o", mapFileName (<> "_map") target -<.> "ncd"
                     , target -<.> "ngd"
                     , target -<.> "pcf"
                     ]

    alternatives $ do
        "*_map.ncd" %> \target -> need [mapFileName (fromJust . stripSuffix "_map") target -<.> "pcf"]

        "*.ncd" %> \target -> do
            liftIO $ createDirectoryIfMissing True "xst/projnav.tmp"
            need [target -<.> "pcf"]
            removeFilesAfter "."
              [ "*_pad.txt"
              , "*_pad.xrpt"
              , "*_pad.csv"
              , "_xmsgs//*"
              , "par_usage_statistics.html"
              , target -<.> "pad"
              , target -<.> "par"
              , target -<.> "xpi"
              , target -<.> "unroutes"
              , target -<.> "ptwx"
              ]

            xilinx "par" [ "-w"
                         , "-ol", "high"
                         , "-mt", "off"
                         , mapFileName (<> "_map") target -<.> "ncd"
                         , target -<.> "ncd"
                         , target -<.> "pcf"
                         ]

    "*.bit" %> \target -> do
        liftIO $ createDirectoryIfMissing True "xst/projnav.tmp"
        need [ target -<.> "ut"
             , target -<.> "ncd"
             ]
        removeFilesAfter "."
          [ "*_bitgen.xwbt"
          , "usage_statistics_webtalk.html"
          , "webtalk.log"
          , target -<.> "bgn"
          , target -<.> "drc"
          ]
        xilinx "bitgen" [ "-f", target -<.> "ut"
                        , target -<.> "ncd"
                        ]
  where
    xilinx tool args = cmd (xilinxRoot </> tool) args

    gensrc f = "gensrc" </> f
    xawsrc f = gensrc $ "xaw" </> f

    vhdls = [mod, "lava-prelude"]

    xiseFiles = map (return .) $ ucf : map vhdl vhdls ++ map xaw xaws
      where
        vhdl componentName = \key -> case key of
            "type" -> MuVariable ("FILE_VHDL" :: String)
            "fileName" -> MuVariable $ componentName <.> "vhdl"
            "behavior" -> MuBool True
        ucf = \key -> case key of
            "type" -> MuVariable ("FILE_UCF" :: String)
            "fileName" -> MuVariable $ mod <.> "ucf"
            "behavior" -> MuBool False
        xaw componentName = \key -> case key of
            "type" -> MuVariable ("FILE_XAW" :: String)
            "fileName" -> MuVariable $ ".." </> "xaw" </> componentName <.> "xaw"
            "behavior" -> MuBool True

hastache :: MuContext IO -> FilePath -> Action ()
hastache ctxt target = do
    alwaysRerun
    templateFile <- liftIO $ getDataFileName ("ise.template" </> templateName)
    t <- liftIO $ hastacheFile hastacheConfig templateFile ctxt
    writeFileChanged target $ BL.unpack t
  where
    hastacheConfig = MuConfig{ muEscapeFunc = emptyEscape
                             , muTemplateFileDir = Nothing
                             , muTemplateFileExt = Just "mustache"
                             , muTemplateRead = const $ return Nothing
                             }

    ext = drop 1 . takeExtension $ target
    templateName = ext <.> "mustache"

listTemplate :: BS.ByteString -> [MuContext IO] -> FilePath -> Action ()
listTemplate key0 entities = hastache ctxt
  where
    ctxt key = return $ if key == key0 then MuList entities else MuNothing

textTemplate :: [(BS.ByteString, BL.ByteString)] -> FilePath -> Action ()
textTemplate replacements = hastache ctxt
  where
    ctxt key = return $ case lookup key replacements of
        Just value -> MuVariable value
        Nothing -> MuNothing

mapFileName :: (String -> String) -> FilePath -> FilePath
mapFileName f fp = replaceFileName fp (f (takeFileName fp))

stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix suffix = fmap reverse . stripPrefix (reverse suffix) . reverse
