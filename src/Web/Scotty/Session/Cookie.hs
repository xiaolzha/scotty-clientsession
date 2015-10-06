{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.Session.Cookie (
        Session
      , getSession
      , writeSession
    ) where


import           Control.Monad.Reader (MonadIO, liftIO)
import           Data.Time.Clock (secondsToDiffTime)
import qualified Data.HashMap.Strict as M
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Web.Scotty.Trans as S
import qualified Web.ClientSession as CS
import qualified Web.Cookie as CK
import qualified Data.ByteString as B
import qualified Blaze.ByteString.Builder as BB


type Session = M.HashMap B.ByteString B.ByteString

sessionCookieName :: B.ByteString
sessionCookieName = "CLIENT_SESSION"

getSession :: (S.ScottyError e, MonadIO m) => CS.Key -> S.ActionT e m Session
getSession key = do
    cookieHeader <- S.header "Cookie"
    session <- liftIO $ deserializeCookie cookieHeader key
    return session

writeSession :: (S.ScottyError e, MonadIO m) => CS.Key -> Session -> Maybe Integer -> S.ActionT e m ()
writeSession key session expiration = do
    encrypted <- liftIO $ CS.encryptIO key $ sessionMapToString session
    S.addHeader "Set-Cookie" $ mkSetCookieText $ mkSetCookie encrypted expiration
    where
        sessionMapToString :: Session -> B.ByteString
        sessionMapToString sessionMap =  BB.toByteString $ CK.renderCookies $ M.toList sessionMap
        mkSetCookie :: B.ByteString -> Maybe Integer -> CK.SetCookie
        mkSetCookie encrypted Nothing = CK.def {CK.setCookieName = sessionCookieName, CK.setCookieValue = encrypted, CK.setCookieHttpOnly = True }
        mkSetCookie encrypted (Just maxAge) = CK.def {CK.setCookieName = sessionCookieName, CK.setCookieValue = encrypted, CK.setCookieHttpOnly = True, CK.setCookieMaxAge = (Just $ secondsToDiffTime maxAge) }
        mkSetCookieText :: CK.SetCookie -> TL.Text
        mkSetCookieText setCookie = TL.fromStrict $ TE.decodeUtf8 $ BB.toByteString $ CK.renderSetCookie setCookie 

deserializeCookie :: Maybe TL.Text -> CS.Key -> IO Session
deserializeCookie Nothing _ = return M.empty
deserializeCookie (Just cookieHeader) key = do
    let decryptedCookie = CS.decrypt key $ parseCookieValue cookieHeader
    return $ toSessionMap decryptedCookie
    where
        parseCookieValue :: TL.Text -> B.ByteString
        parseCookieValue fullCookie = CK.setCookieValue $ CK.parseSetCookie $ TE.encodeUtf8 $ TL.toStrict fullCookie
        toSessionMap :: Maybe B.ByteString -> Session
        toSessionMap Nothing = M.empty
        toSessionMap (Just val) = M.fromList $ CK.parseCookies val



