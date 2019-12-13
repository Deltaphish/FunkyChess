{-- Remove comments if running on windows --}
{-- Otherwise unicode errors will occur   --}

import ChessAI
import Board
--import GHC.IO.Encoding
--import System.Win32.Console

main = do 
    --setLocaleEncoding utf8
    --setConsoleOutputCP 65001
    print $ makeAiMove initBoard
