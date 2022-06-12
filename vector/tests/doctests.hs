import Test.DocTest (doctest)

main :: IO ()
main = doctest [ "-Iinclude" , "-Iinternal" , "-XHaskell2010" , "src/Data" ]
