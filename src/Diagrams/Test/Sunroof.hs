module Diagrams.Test.Sunroof (sunroofTester) where


import Data.Default
import Language.Sunroof hiding ( (!) )
import Language.Sunroof.JS.Browser as SR
import Language.Sunroof.JS.Canvas as SR
import Diagrams.Backend.Sunroof
import Diagrams.Prelude hiding (D, (<.>), (#))
import Diagrams.Tests
import System.FilePath ((</>),(<.>))
import Text.Html as H hiding ((</>))

sunroofTester :: (String, Test -> IO Html)
sunroofTester =
  ( "Sunroof"
  , \ (Test nm dig) -> do
      let ident = "sunroof_" ++ nm
      let jsCode = renderDia SunroofBackend (SunroofOptions (Dims 200 200)) dig
      strCode <- sunroofCompileJSA def ident $ do
                    renderF <- function jsCode
                    canvasE <- SR.document # SR.getElementById (js ident)
                    context <- canvasE # SR.getContext (js "2d")
                    renderF $$ context
      writeFile (name nm) $ show $ H.thehtml $ 
        H.tag "head" noHtml
        +++ (H.body $ concatHtml $
        [ H.tag "canvas" ! 
            [ H.width "200"
            , H.height 200
            , identifier ident 
            ] << noHtml
        , H.tag "script" ! 
            [ strAttr "type" "text/javascript" 
            ] << H.primHtml strCode
        ])
      return $ H.tag "iframe" ! 
        [ H.src $ name nm
        , H.height 210
        , H.width "210"
        , intAttr "frameborder"  0
        , intAttr "marginheight" 0
        , intAttr "scrolling"    0
        ] << noHtml
  )
  where
    name nm = prefix </> nm <.> "html"
    prefix = "sunroof"