module Data.Default where

class Default a where
	default ∷ a

class PartialDefault a r | a → r where
	withRequired :: r → a