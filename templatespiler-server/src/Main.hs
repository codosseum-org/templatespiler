module Main where

import Network.Wai.Handler.Warp (run)
import Servant (Handler, Server, serve, (:<|>) (..))
import Templatespiler.Server

main :: IO ()
main = do
  run 8080 $ serve (Proxy @TemplatespilerAPI) templatespilerServer
  putStrLn ""

templatespilerServer :: Server TemplatespilerAPI
templatespilerServer =
  parse :<|> generate :<|> compile
  where
    parse :: TemplateParseRequest -> Handler ParsedTemplate
    parse = undefined
    generate :: TemplateID -> Maybe Int -> Handler GenerateResponse
    generate = undefined
    compile :: TemplateID -> Maybe Language -> Handler CompiledTemplateResponse
    compile = undefined
