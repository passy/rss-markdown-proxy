{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}

import           BasicPrelude      hiding (fromString)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import           System.Directory  (getCurrentDirectory)
import           System.IO         (Handle (), IOMode (ReadMode), openFile,
                                    stdin)
import           Lib
import           Test.Hspec
import           Text.XML.HXT.Core
import           Debug.Trace

openFixture :: forall s b. FilePath -> IO (IOStateArrow s b XmlTree)
openFixture path = do
    dir <- getCurrentDirectory
    openXMLFile $ dir </> "test" </> "fixtures" </> path

openXMLFile :: forall s b. FilePath -> IO (IOStateArrow s b XmlTree)
openXMLFile = (readXMLFileHandle =<<) . getHandle
  where
    getHandle path = openFile path ReadMode

readXMLFileHandle :: forall s b. Handle -> IO (IOStateArrow s b XmlTree)
readXMLFileHandle h = do
    contents <- TIO.hGetContents h
    return $ readString [withWarnings yes] $ T.unpack contents

main :: IO ()
main = hspec $
  describe "XML Parser" $
    it "reads descriptions" $ do
    doc <- liftIO $ openFixture "sounds.rss"
    descs <- liftIO $ runX $
      doc >>> propagateNamespaces >>> deep selectDescriptions /> getText

    length (traceShowId descs) `shouldBe` 5
