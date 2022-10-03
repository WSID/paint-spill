{-|
Module      : Graphics.PaintSpill
Description : Vector Graphics with acceleraed graphics.
Copyright   : Wissle Lim, 2022
License     : MIT
Maintainer  : jongsome@gmail.com
Stability   : unstable

Work in progress!
-}

{-# LANGUAGE FlexibleContexts #-}

module Graphics.PaintSpill where

import Control.Monad.Reader.Class
import Control.Monad.Writer.Class

answerOfEverything :: IO Int
answerOfEverything = do
    return (6 * 7)


-- | A single drawing operation.
data Draw
    = Fill Shape !Pattern
    | Stroke Shape !StrokeOption !Pattern

-- | Group of drawing operations.
type Draws = [Draw]

fill :: Shape -> Pattern -> Draws
fill shape pattern = [Fill shape pattern]

stroke :: Shape -> StrokeOption -> Pattern -> Draws
stroke shape stroke pattern = [Stroke shape stroke pattern]

data Shape = Shape
data Pattern = Pattern
data StrokeOption = StrokeOption

data DrawEnv = DrawEnv
    { envStroke :: !StrokeOption
    , envPattern :: !Pattern
    }

askStroke :: (MonadReader DrawEnv m) => m StrokeOption
askStroke = asks envStroke

askPattern :: (MonadReader DrawEnv m) => m Pattern
askPattern = asks envPattern

fillw :: (MonadReader DrawEnv m, MonadWriter Draws m) => Shape -> m ()
fillw shape = tell . fill shape =<< askPattern

strokew :: (MonadReader DrawEnv m, MonadWriter Draws m) => Shape -> m ()
strokew shape = tell =<< (stroke shape <$> askStroke <*> askPattern)

withPattern :: (MonadReader DrawEnv m) => Pattern -> m a -> m a
withPattern pattern = local (\e -> e {envPattern = pattern})

withStroke :: (MonadReader DrawEnv m) => StrokeOption -> m a -> m a
withStroke stroke = local (\e -> e {envStroke = stroke})