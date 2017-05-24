module Main where

import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import System.IO

import PID

readArgs :: [String] -> [Double]
readArgs = map read

main :: IO ()
main = do
	hSetBuffering stdin LineBuffering
	hSetBuffering stdout LineBuffering
	args <- getArgs
	let ([kp, ki, kd], pid) = case length args of
		3 -> (readArgs args, simplePID)
		4 -> (init $ readArgs args, \s o v -> boundIntegralPID s (last $ readArgs args) o v)
		_ -> error "incorrect number of args"
	let settings = PIDSettings kp ki kd
	evalPID $ forever $ do
		input <- liftIO $ getLine
		let [objective, value] = map read $ words input
		action <- pid settings objective value
		liftIO $ print action
