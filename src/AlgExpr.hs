module AlgExpr where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import CubeParser
import Text.Megaparsec (runParser)
import qualified Data.Text as T
import Language.Haskell.TH.Syntax (liftData)

algExpr :: QuasiQuoter
algExpr = QuasiQuoter { quoteExp  = parseAlgExprExp
                      , quotePat  = undefined
                      , quoteType = undefined
                      , quoteDec  = undefined
                      }

parseAlgExprExp :: String -> TH.Q TH.Exp
parseAlgExprExp input = case runParser parseScramble "" (T.pack input) of
    Left _ -> error "failed"
    Right alg -> liftData alg
