import TestInlineObjC

main :: IO ()
main
  = do
    { objc_initialise
    ; dumpURL "http://www.cse.unsw.edu.au/~chak/"
    }
