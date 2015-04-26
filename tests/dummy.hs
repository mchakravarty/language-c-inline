
-- This program does nothing. Its only here to force the building of the
-- library object files for use with the testsuite.

import Language.C.Quote.C ()
import Language.C.Inline.C ()
import Language.Haskell.TH ()

main :: IO ()
main = putStrLn "Dummy program."
