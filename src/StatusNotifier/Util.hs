{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.Util where

import           Control.Arrow
import           Control.Lens
import           DBus.Client
import qualified DBus.Internal.Message as M
import qualified DBus.Internal.Types as T
import qualified DBus.Introspection as I
import qualified DBus.Generation as G
import qualified Data.ByteString as BS
import           Data.Maybe
import qualified Data.Vector.Storable as VS
import           Data.Vector.Storable.ByteString
import           Language.Haskell.TH
import           Network.Socket (ntohl)
import           StatusNotifier.TH
import           System.IO
import           System.IO.Unsafe
import           System.Log.Handler.Simple
import           System.Log.Logger

getIntrospectionObjectFromFile :: FilePath -> T.ObjectPath -> Q I.Object
getIntrospectionObjectFromFile filepath nodePath = runIO $
  head . maybeToList . I.parseXML nodePath <$> readFile filepath

generateClientFromFile :: G.GenerationParams -> Bool -> FilePath -> Q [Dec]
generateClientFromFile params useObjectPath filepath = do
  object <- getIntrospectionObjectFromFile filepath "/"
  let interface = head $ I.objectInterfaces object
      actualObjectPath = I.objectPath object
      realParams =
        if useObjectPath
        then params { G.genObjectPath = Just actualObjectPath }
        else params
  (++) <$> G.generateClient realParams interface <*>
           G.generateSignalsFromInterface realParams interface

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond whenTrue whenFalse =
  cond >>= (\bool -> if bool then whenTrue else whenFalse)

makeLensesWithLSuffix :: Name -> DecsQ
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

logErrorWithDefault ::
  Show a => Logger -> b -> String -> Either a b -> IO b
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

getInterfaceAt
  :: Client
  -> T.BusName
  -> T.ObjectPath
  -> IO (Either M.MethodError (Maybe I.Object))
getInterfaceAt client bus path =
  right (I.parseXML "/") <$> introspect client bus path
