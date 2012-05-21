module Backend.Embedded where

type Embedded = Map.Map FilePath EmbeddedEntry

data EmbeddedEntry = EEFile S8.ByteString | EEFolder Embedded

embeddedLookup :: Embedded -> Pieces -> IO FileLookup
embeddedLookup root pieces =
    return $ elookup "<root>" (map F.fromText pieces) root
  where
    elookup  :: FilePath -> [FilePath] -> Embedded -> FileLookup
    elookup p [] x = Just $ Left $ Folder (unFilePath p) $ map toEntry $ Map.toList x
    elookup p [""] x = elookup p [] x
    elookup _ (p:ps) x =
        case Map.lookup p x of
            Nothing -> Nothing
            Just (EEFile f) ->
                case ps of
                    [] -> Just $ Right $ bsToFile p f
                    _ -> Nothing
            Just (EEFolder y) -> elookup p ps y

toEntry :: (FilePath, EmbeddedEntry) -> Either Folder File
toEntry = error "toEntry"
{-
toEntry (name, EEFolder{}) = Left $ Folder name []
toEntry (name, EEFile bs) = Right $ File
    { fileGetSize = S8.length bs
    , fileToResponse = \s h -> W.ResponseBuilder s h $ fromByteString bs
    , fileName = name
    , fileGetHash = return $ Just $ runHash bs
    , fileGetModified = Nothing
    }
-}

toEmbedded :: [(Prelude.FilePath, S8.ByteString)] -> Embedded
toEmbedded fps =
    go texts
  where
    texts = map (\(x, y) -> (filter (not . T.null . unFilePath) $ toPieces x, y)) fps
    toPieces "" = []
    toPieces x =
        let (y, z) = break (== '/') x
         in toFilePath y : toPieces (drop 1 z)
    go :: [([FilePath], S8.ByteString)] -> Embedded
    go orig =
        Map.fromList $ map (second go') hoisted
      where
        next = map (\(x, y) -> (head x, (tail x, y))) orig
        grouped :: [[(FilePath, ([FilePath], S8.ByteString))]]
        grouped = groupBy ((==) `on` fst) $ sortBy (comparing fst) next
        hoisted :: [(FilePath, [([FilePath], S8.ByteString)])]
        hoisted = map (fst . head &&& map snd) grouped
    go' :: [([FilePath], S8.ByteString)] -> EmbeddedEntry
    go' [([], content)] = EEFile content
    go' x = EEFolder $ go $ filter (\y -> not $ null $ fst y) x

bsToFile :: FilePath -> S8.ByteString -> File
bsToFile name bs = File
    { fileGetSize = S8.length bs
    , fileToResponse = \s h -> W.ResponseBuilder s h $ fromByteString bs
    , fileName = name
    , fileGetHash = return $ Just $ runHash bs
    , fileGetModified = Nothing
    }
