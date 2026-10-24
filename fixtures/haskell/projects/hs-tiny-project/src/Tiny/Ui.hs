module Tiny.Ui where { import System.IO (hFlush, stdout)
; import qualified Tiny.Core as Core
; render :: Core.Name -> IO ()
; render name = print (Core.greet name) >> hFlush stdout }
