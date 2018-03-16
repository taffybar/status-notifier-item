module StatusNotifier.Util where

import           Control.Lens
import           DBus.Client
import qualified DBus.Internal.Types as T
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as VS
import           Data.Vector.Storable.ByteString
import           Language.Haskell.TH
import           Network.Socket (ntohl)
import           Paths_status_notifier_item ( getDataDir )
import           System.FilePath

getXMLDataFile :: String -> IO FilePath
getXMLDataFile filename = (</> filename) . (</> "xml") <$> getDataDir

infixl 4 ??
(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab
{-# INLINE (??) #-}

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond whenTrue whenFalse =
  cond >>= (\bool -> if bool then whenTrue else whenFalse)

makeLensesWithLSuffix =
  makeLensesWith $
  lensRules & lensField .~ \_ _ name ->
    [TopName (mkName $ nameBase name ++ "L")]

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe $ return ()

networkToSystemByteOrder :: BS.ByteString -> BS.ByteString
networkToSystemByteOrder original =
  vectorToByteString $ VS.map ntohl $ byteStringToVector original

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

makeErrorReply :: ErrorName -> String -> Reply
makeErrorReply e message = ReplyError e [T.toVariant message]
