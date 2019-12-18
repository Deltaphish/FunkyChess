main = do
  gameloop initBoard

gameloop board =
  showBoard
  (board',state) <- makeMove board white
  printState state
  if state == checkMate
    then print "GameOver"
    else do 
       (board'' state) <- makeMove board' black
       print state
       if state == checkMate
          then print "GameOver"
          else gameloop board''


makeMove board color = do
  case input of
    Nothing -> makeMove board color
    Just move -> case movePieace board move of
                 Nothing -> makeMove board color
                 Just res -> res
