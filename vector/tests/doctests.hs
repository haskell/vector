import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-Iinclude", "-Iinternal", "src/Data"]
