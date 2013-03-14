import TestInlineObjC

main :: IO ()
main
  = do
    { objc_initialise
    ; dumpURL "https://raw.github.com/mchakravarty/language-c-inline/master/tests/objc/TestInlineObjC.hs"
    }
