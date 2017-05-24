module PID
	( PIDSettings(..)
	, PIDState(..)
	, evalPID
	, simplePID
	, boundIntegralPID
	) where

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

evalPID p = evalStateT p (PIDState 0 0)

simplePID :: (Fractional a, Monad m) => PIDSettings a -> a -> a -> StateT (PIDState a) m a
simplePID (PIDSettings kp ki kd) objective value = do
	let error = objective - value
	PIDState integral_ lastError_ <- get
	let newIntegral = integral_ + error
	let derivative = error - lastError_
	put $ PIDState newIntegral error
	return $ kp * error + ki * newIntegral + kd * derivative

bind max val = if (abs val) > max then max * signum val else val

boundIntegralPID :: (Fractional a, Ord a, Monad m) => PIDSettings a -> a -> a -> a -> StateT (PIDState a) m a
boundIntegralPID (PIDSettings kp ki kd) intmax objective value = do
	let error = objective - value
	PIDState integral_ lastError_ <- get
	let newIntegral = bind intmax $ integral_ + error
	let derivative = error - lastError_
	put $ PIDState newIntegral error
	return $ kp * error + ki * newIntegral + kd * derivative
