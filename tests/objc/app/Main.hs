import qualified App         as App
import qualified AppDelegate as Delegate

main :: IO ()
main 
  = do
    { App.objc_initialise
    ; Delegate.objc_initialise
    ; App.main
    }
