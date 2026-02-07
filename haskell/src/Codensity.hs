{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Codensity where

import Freer (Freer(..), Talk(..), send, interpretPure)

-- ============================================================
-- Codensity モナド
-- ============================================================

-- Freer モナドの >>= は継続を合成で連鎖させる:
--   Bind fx k >>= f = Bind fx ((>>= f) . k)
--
-- 左結合の >>= チェーン (((m >>= f) >>= g) >>= h) は
-- 継続が入れ子になり O(n^2) になり得る。
--
-- Codensity モナドはこの問題を解決する。

-- | Codensity モナド
-- CPS 変換されたモナドとも見なせる
newtype Codensity m a = Codensity
  { runCodensity :: forall r. (a -> m r) -> m r }

instance Functor (Codensity m) where
  fmap f (Codensity g) = Codensity (\k -> g (k . f))

instance Applicative (Codensity m) where
  pure a = Codensity (\k -> k a)
  Codensity f <*> Codensity x = Codensity (\k -> f (\g -> x (k . g)))

instance Monad (Codensity m) where
  Codensity m >>= f = Codensity (\k -> m (\a -> runCodensity (f a) k))

-- >>= が常に O(1) になる!
-- 関数合成の結合方向が右結合に矯正されるため。

-- | Codensity に持ち上げる
liftCodensity :: Monad m => m a -> Codensity m a
liftCodensity m = Codensity (m >>=)

-- | Codensity から降ろす
lowerCodensity :: Monad m => Codensity m a -> m a
lowerCodensity (Codensity f) = f pure

-- ============================================================
-- 実行例
-- ============================================================

exampleProgram :: Freer Talk String
exampleProgram = do
  name <- send (Ask "名前を入力してください")
  send (Tell ("こんにちは、" ++ name ++ "さん！"))
  age <- send (Ask "年齢を入力してください")
  send (Tell (age ++ "歳ですね。"))
  pure name

example :: IO ()
example = do
  putStrLn "=== Codensity & Step Execution ==="
  let (name, msgs) = interpretPure ["太郎", "25"] exampleProgram
  putStrLn $ "結果: " ++ name
  mapM_ (\msg -> putStrLn $ "  出力: " ++ msg) msgs
