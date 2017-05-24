module PID where

import Control.Monad.State

data Fractional b => PIDSettings b = PIDSettings
	{ kp :: b
	, ki :: b
	, kd :: b
	} deriving Show
data Fractional b => PIDState b = PIDState
	{ integral :: b
	, lastError :: b
	} deriving Show

pid :: (Fractional a, Monad m) => PIDSettings a -> a -> a -> StateT (PIDState a) m a
pid (PIDSettings kp ki kd) objective value = do
	let error = objective - value
	PIDState integral_ lastError_ <- get
	let newIntegral = integral_ + error
	let derivative = error - lastError_
	put $ PIDState newIntegral error
	return $ kp * error + ki * newIntegral + kd * derivative

evalPID p = evalStateT p (PIDState 0 0)
