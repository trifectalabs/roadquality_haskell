module OAuth where

import Data.Maybe (fromJust)
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.HttpClient
import Network.OAuth.Http.PercentEncoding

facebookAuthUrl = fromJust . parseURL $ "https://www.facebook.com/v2.8/dialog/oauth?client_id={app-id} &redirect_uri={redirect-uri}"
facebookAccessTokenUrl = fromJust . parseURL $ "https://graph.facebook.com/v2.8/oauth/access_token?
   client_id={app-id}
   &redirect_uri={redirect-uri}
   &client_secret={app-secret}
   &code={code-parameter}"
