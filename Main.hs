module Main where

import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import Control.Monad.State

import PID

readArgs :: [String] -> [Double]
readArgs = map read

main :: IO ()
main = do
	args <- getArgs
	let [kp, ki, kd] = readArgs args
	let settings = PIDSettings kp ki kd
	evalPID $ forever $ do
		input <- liftIO $ getLine
		let [objective, value] = map read $ words input
		action <- pid settings objective value
		state <- get
		liftIO $ print action
		liftIO $ print state
