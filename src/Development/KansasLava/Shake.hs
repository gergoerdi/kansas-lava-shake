module Development.KansasLava.Shake
       ( lavaRules
       ) where

import Development.Shake
import Development.Shake.FilePath
import Language.KansasLava.VHDL (writeVhdlPrelude)

lavaRules :: FilePath -> String -> String -> Rules ()
lavaRules modName vhdl ucf = do
    "gensrc" </> modName <.> "vhdl" %> \target -> do
        alwaysRerun
        writeFileChanged target vhdl
    "gensrc" </> modName <.> "ucf" %> \target -> do
        alwaysRerun
        writeFileChanged target ucf
    "gensrc/lava-prelude.vhdl" %> \target -> do
        alwaysRerun
        withTempFile $ \tempFile -> do
            liftIO $ writeVhdlPrelude tempFile
            copyFileChanged tempFile target
