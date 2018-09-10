{-# LANGUAGE OverloadedStrings #-}

import Data.Warc.Body
import Data.Warc.WarcEntry
import Data.Warc.Header
import Data.Warc.HeaderLine
import Data.Warc.Key
import Data.Warc.Value 

import Control.Monad.IO.Class             (liftIO)
import Control.Monad.Trans.Resource       (MonadResource, runResourceT)
import Data.ByteString.Char8        as C8 (ByteString, pack, length, readFile)
import Data.Conduit                       (ConduitM, awaitForever, mapOutput, runConduit, yield, (.|))
import Data.Conduit.Combinators           (sourceDirectoryDeep, sinkFile)
import System.Environment                 (getArgs)

main :: IO ()
main = do
    args <- getArgs
    print args
    let [destFile, startPath] = args
    let followSymlinks = False
    runResourceT $
        runConduit $ sourceDirectoryDeep followSymlinks startPath
                  .| withContents
                  .| mapOutput toByteString toWarcEntry
                  .| sinkFile destFile

withContents :: MonadResource m => ConduitM FilePath (ByteString, ByteString) m ()
withContents = awaitForever $ \fp -> do
    contents <- liftIO $ C8.readFile fp
    yield (pack fp, contents)

toWarcEntry :: MonadResource m => ConduitM (ByteString, ByteString) WarcEntry m ()
toWarcEntry = awaitForever $ \(fp, bs) ->
    let header = WarcHeader (WarcVersion "1.0")
                            [ HeaderLine (MandatoryKey WarcType) (StringValue "response")
                            , HeaderLine (MandatoryKey WarcRecordId) (StringValue fp)
                            , HeaderLine (OptionalKey WarcTargetURI) (StringValue fp)
                            , HeaderLine (MandatoryKey ContentLength) (IntValue $! C8.length bs)
                            , HeaderLine (CustomKey UncompressedContentLength) (IntValue $! C8.length bs)
                            ]
    in yield (WarcEntry header (UncompressedBody bs))
