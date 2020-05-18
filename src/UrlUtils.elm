module UrlUtils exposing (baseUrl)

import Url

baseUrl : Url.Url -> Url.Url
baseUrl url =
  { url | path = "", query = Nothing, fragment = Nothing }
