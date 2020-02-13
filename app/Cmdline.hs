module Cmdline where

import System.Console.ANSI

version = "2.1"

showHelp :: IO ()
showHelp = do
    printSplashScreen
    putStrLn "\n"
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn "USAGE: fscript <file> [options]"
    putStrLn "     | fscript --repl [options]\n\n"
    setSGR [SetColor Foreground Vivid Cyan]
    putStrLn "OPTIONS:"
    putStrLn " -h | --help             | Show this help"
    putStrLn " -r | --repl             | Run in interactive REPL mode"
    putStrLn "    | --debug-no-stdlib  | Do not import stdlib/Base.fscript; When using --debug-expr-only, use 'put' and 'show', because 'print' is defined in stdlib"
    putStrLn "    | --debug-parse      | REPL ONLY; Show the parser results"
    putStrLn "    | --debug-expr-only  | REPL ONLY; Only evaluate and print out expressions"
    putStrLn "    | --debug-stmnt-only | REPL ONLY; Only evaluate statements"
    putStrLn ""
    setSGR [SetColor Foreground Vivid Magenta]
    putStrLn "Github/Documentation: https://github.com/Innf107/FScript"
    setSGR [Reset]

printSplashScreen :: IO ()
printSplashScreen = do
                  setSGR [SetColor Foreground Vivid Green]
                  putStrLn $ " ______   ______   ______   ______     __   ______    __________"
                  putStrLn $ "|   ___| |   ___| |   ___| |   __  \\  |  | |   __  \\ |___    ___|"
                  putStrLn $ "|  |___  |  |___  |  |     |  |__|  | |  | |  |__|  |    |  |"
                  putStrLn $ "|   ___| |___   | |  |     |       /  |  | |   ____/     |  |"
                  putStrLn $ "|  |      ___|  | |  |___  |  |\\  \\   |  | |  |          |  |"
                  putStrLn $ "|__|     |______| |______| |__| \\__\\  |__| |__|          |__|    v" ++ version
                  setSGR [Reset]
