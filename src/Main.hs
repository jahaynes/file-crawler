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
import Data.Conduit.List            as CL (filter)
import Data.Conduit.Combinators           (sourceDirectoryDeep, sinkFile)
import Data.List                          (isSuffixOf)
import System.Environment                 (getArgs)

usage :: String
usage = "usage: file-crawler destFile startPath [filesuffix]"

filterEnd :: Maybe String -> String -> Bool
filterEnd   Nothing     _ = True
filterEnd (Just fe) fname = fe `isSuffixOf` fname

main :: IO ()
main = do
    args <- getArgs
    let (destFile, startPath, fe) = case args of
                                        [a,b,c] -> (a,b,Just c)
                                        [a,b]   -> (a,b,Nothing)
                                        _       -> error usage
    let followSymlinks = False
    runResourceT $
        runConduit $ sourceDirectoryDeep followSymlinks startPath
                  .| CL.filter (filterEnd fe)
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
