module GreetIfCool3 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True ->
      putStrLn "eyyy"
    False ->
      putStrLn "pshhh"
  where cool =
            coolness == "down"
        