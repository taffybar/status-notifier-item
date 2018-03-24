module StatusNotifier.Util where

import           Control.Arrow
import           Control.Lens
import           DBus.Client
import qualified DBus.Internal.Message as M
import qualified DBus.Internal.Types as T
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as VS
import           Data.Vector.Storable.ByteString
import           Language.Haskell.TH
import           Network.Socket (ntohl)
import           Paths_status_notifier_item ( getDataDir )
import           System.FilePath
import           System.IO
import           System.IO.Unsafe
import           System.Log.Handler.Simple
import           System.Log.Logger

getXMLDataFile :: String -> IO FilePath
getXMLDataFile filename = (</> filename) . (</> "xml") <$> getDataDir

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

{-# NOINLINE defaultHandler #-}
defaultHandler :: GenericHandler Handle
defaultHandler = unsafePerformIO $ streamHandler stdout INFO

{-# NOINLINE makeDefaultLogger #-}
makeDefaultLogger :: String -> Logger
makeDefaultLogger name =
  setLevel INFO $ unsafePerformIO $ getLogger name

logErrorWithDefault ::
  Show a => Logger -> b -> [Char] -> Either a b -> IO b
logErrorWithDefault logger def message =
  either (\err -> logL logger ERROR (message ++ show err) >> return def) return

exemptUnknownMethod ::
  b -> Either M.MethodError b -> Either M.MethodError b
exemptUnknownMethod def eitherV =
  case eitherV of
    Right _ -> eitherV
    Left M.MethodError { M.methodErrorName = errorName } ->
      if errorName == errorUnknownMethod
      then Right def
      else eitherV

exemptAll ::
  b -> Either M.MethodError b -> Either M.MethodError b
exemptAll def eitherV =
  case eitherV of
    Right _ -> eitherV
    Left _ -> Right def

infixl 4 <..>
(<..>) :: Functor f => (a -> b) -> f (f a) -> f (f b)
(<..>) = fmap . fmap

infixl 4 <<$>>
(<<$>>) :: (a -> IO b) -> Maybe a -> IO (Maybe b)
fn <<$>> m = sequenceA $ fn <$> m

forkM :: Monad m => (i -> m a) -> (i -> m b) -> i -> m (a, b)
forkM a b i =
  do
    r1 <- a i
    r2 <- b i
    return (r1, r2)

tee :: Monad m => (i -> m a) -> (i -> m b) -> i -> m a
tee = (fmap . fmap . fmap) (fmap fst) forkM

(>>=/) :: Monad m => m a -> (a -> m b) -> m a
(>>=/) a = (a >>=) . tee return
