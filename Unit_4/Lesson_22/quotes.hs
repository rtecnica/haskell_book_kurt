main :: IO ()
main = do
  print "Enter a number between 1 and 5:"
  userInput <- getContents
  selectQuote userInput

selectQuote :: String -> IO ()
selectQuote (x : xs) = do
  case x of
    '1' -> print "Workers of the world! Unite!"
    '2' -> print "No Gods, No Masters"
    '3' -> print "I am CornHolio"
    '4' -> print "I Will not Fear, Fear is the Mind-Killer"
    '5' -> print "Congratulations, you played yourself"
    _ -> return ()
  if x == 'n'
    then return ()
    else do
      if x == '\n'
        then return ()
        else print "Would you like another quote?"
      selectQuote xs
