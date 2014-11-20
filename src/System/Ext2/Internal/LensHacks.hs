{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Ext2.Internal.LensHacks
-- Copyright   : (C) 2014 Ricky Elrod
-- License     : BSD2 (see LICENSE file)
-- Maintainer  : Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : lens, template-haskell
----------------------------------------------------------------------------
module System.Ext2.Internal.LensHacks where

import Data.Char
import Data.List
import Data.Maybe
import Control.Lens
import Control.Lens.Internal.FieldTH
import Language.Haskell.TH

namespaceLensRules :: LensRules
namespaceLensRules = LensRules
  { _simpleLenses    = False
  , _generateSigs    = True
  , _generateClasses = False
  , _allowIsos       = True
  , _classyLenses    = const Nothing
  , _fieldToDef      = abbreviatedNamer
  }

-- | This is taken straight out of 'Control.Lens.TH' but but modified to give
-- a 'TopName' back instead of a 'MethodName'. This means we can
-- 'makeLensesWith'out classes using abbreviated fields.
abbreviatedNamer :: Name -> [Name] -> Name -> [DefName]
abbreviatedNamer _ fields field = maybeToList $ do
  fieldPart <- stripMaxLc (nameBase field)
  method    <- computeMethod fieldPart
  return (TopName (mkName method))

  where
  stripMaxLc f = do x <- stripPrefix optUnderscore f
                    case break isUpper x of
                      (p,s) | null p || null s -> Nothing
                            | otherwise                  -> Just s
  optUnderscore  = ['_' | any (isPrefixOf "_" . nameBase) fields ]

  computeMethod (x:xs) | isUpper x = Just (toLower x : xs)
  computeMethod _                  = Nothing
