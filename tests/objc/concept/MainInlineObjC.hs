import TestInlineObjC

main :: IO ()
main
  = do
    { objc_initialise
    ; dumpURL "https://raw.githubusercontent.com/mchakravarty/language-c-inline/master/tests/objc/concept/TestInlineObjC.hs"
    }
