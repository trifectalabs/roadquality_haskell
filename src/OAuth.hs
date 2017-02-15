module OAuth where

import           Data.Maybe (fromJust)
import           Network.OAuth.OAuth2
import           Data.ByteString.Internal
import qualified Data.ByteString.Char8 as C

fetchCode :: OAuth2 -> String
fetchCode fbOAuth = C.unpack $ oauthClientId fbOAuth
