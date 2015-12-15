{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Lib
import           System.Directory       (getCurrentDirectory)
import           System.FilePath        ((</>))
import           System.IO              (Handle (), IOMode (ReadMode), openFile)
import           Test.Hspec
import           Text.XML.HXT.Core

openFixture :: forall a. (FilePath -> IO a) -> FilePath -> IO a
openFixture f path = do
    dir <- getCurrentDirectory
    f $ dir </> "test" </> "fixtures" </> path

openXMLFixture :: forall s b. FilePath -> IO (IOStateArrow s b XmlTree)
openXMLFixture = openFixture openXMLFile

openStringFixture :: FilePath -> IO String
openStringFixture = openFixture readFile

openXMLFile :: forall s b. FilePath -> IO (IOStateArrow s b XmlTree)
openXMLFile = (readXMLFileHandle =<<) . getHandle
  where
    getHandle path = openFile path ReadMode

readXMLFileHandle :: forall s b. Handle -> IO (IOStateArrow s b XmlTree)
readXMLFileHandle h = do
    contents <- TIO.hGetContents h
    return $ readString [withWarnings yes] $ T.unpack contents

main :: IO ()
main = hspec $ do
  describe "XML Parser" $
    it "reads descriptions" $ do
    doc <- liftIO $ openXMLFixture "sounds.rss"
    descs <- liftIO $ runX $
      doc >>> propagateNamespaces >>> deep selectDescriptions /> getText

    length descs `shouldBe` 5

  describe "RSS Transformer" $
    it "transforms correctly" $ do
    input <- liftIO $ openStringFixture "sounds.rss"
    expected <- liftIO $ openStringFixture "output.rss"

    output <- liftIO $ transformRSS input

    output `shouldBe` expected
