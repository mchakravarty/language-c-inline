import qualified App         as App
import qualified AppDelegate as Delegate

main :: IO ()
main 
  = do
    { App.objc_initialise
    ; Deletage.objc_initialise
    ; App.main
    }
