module Main where

import System.Console.Haskeline
import Control.Monad
import Control.Monad.Catch (catch, SomeException)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Map as M
import Data.List (intercalate, isInfixOf)

data ToyDebuggerCommand = Stop
    | Print String
    | Breakpoint
    deriving (Show)

type Program = [ToyDebuggerCommand]

data Mode = RunAll | Step | Debug  | Undefined deriving (Eq, Enum, Show)

data ProgramState = ProgramState {
        mode :: Mode,
        program :: Program,
        env :: Environment
    } deriving (Show)

data Environment = Environment {
    line :: Int
    } deriving (Show)

type Arguments = [String]

type Command = Arguments -> ProgramState -> IO (Either (String, ProgramState) ProgramState)

type HelpString = String

data CommandDef = CommandDef Command HelpString

type CommandMap = M.Map String CommandDef

testProgram :: Program
testProgram = [
        Print "Line 0",
        Print "Line 1",
        Breakpoint,
        Print "Line 2",
        Stop,
        Print "Line 3, after stop"
    ]

initialProgramState :: ProgramState
initialProgramState = ProgramState {mode = Undefined, program = testProgram, env = Environment 0}

commands :: CommandMap
commands = M.fromList [
        ("run", CommandDef (cmdRun RunAll) "Run all, ignoring breakpoints."),
        ("step", CommandDef (cmdRun Step) "Run step by step."),
        ("debug", CommandDef (cmdRun Debug) "Run until breakpoint."),
        ("reset", CommandDef (cmdReset) "Reset program state."),
        ("help", CommandDef (cmdHelp commands) "This help.")
    ]

runCommand :: CommandMap -> Command
runCommand cmds cmd state =
    case cmd of
        [] -> return $ Left ("Empty command", state)
        (cmdName: args) ->
            case M.lookup cmdName cmds of
                Nothing -> return $ Left ("Command '" ++ cmdName ++ "' not found. Possible variants: " ++ findMostSimilar cmdName, state)
                Just (CommandDef fn _) -> fn args state
    where
        findMostSimilar cmdName = intercalate " " $ (filter . isInfixOf) cmdName $ M.keys cmds

cmdHelp :: CommandMap -> Command
cmdHelp cmds args state = return $ Left (helpStr, state)
    where
        addCommandHelp names key (CommandDef _ def) acc = if names == [] || elem key names then acc ++ "\n" ++ key ++ ":\n\t" ++ def else ""
        helpStr = M.foldrWithKey (addCommandHelp args) "Commands:\nType <command> help for command help\n" cmds

cmdReset :: Command
cmdReset _ state  = return $ Right $ state {mode = Undefined, env = Environment 0}

cmdRun :: Mode -> Command
cmdRun mode _ state  = runAllStep $ state {mode = mode}

runAllStep :: ProgramState -> IO (Either (String, ProgramState) ProgramState)
runAllStep state = runAllStep' state $ drop (line $ env state) $ program state
    where
        runAllStep' state [] = return $ Right state
        runAllStep' state (c:cmds) = do
            res <- runToyDebuggerCommand (incLine state) c
            case res of
                Right newState -> runAllStep' newState cmds
                res -> return res

runToyDebuggerCommand :: ProgramState -> ToyDebuggerCommand -> IO (Either (String, ProgramState) ProgramState)
runToyDebuggerCommand state Stop = return $ Left ("Stopped...", state)
runToyDebuggerCommand state Breakpoint = if (mode state == RunAll)
    then return $ Right state
    else return $ Left ("Breakpoint...", state)
runToyDebuggerCommand state (Print s) = do
    putStrLn s
    if (mode state == Step)
        then return $ Left ("Step...", state)
        else return $ Right state

incLine state = state {env = incLine' $ env state}
    where
        incLine' (Environment line) = Environment (line + 1)

main :: IO ()
main = do
        let state = initialProgramState in do
            runInputTWithPrefs defaultPrefs defaultSettings $ loop state
    where
        loop state = do
            minput <- getInputLine "ToyDebugger> "
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just input -> (flip catch) exceptonHandler $ do
                    res <- liftIO $ runCommand commands (words input) state
                    case res of
                        Left (errorMessage, state') -> outputStrLn errorMessage >> loop state'
                        Right state' -> loop state'

        exceptonHandler :: SomeException -> InputT IO ()
        exceptonHandler ex = outputStrLn $ "Exception" ++ show ex