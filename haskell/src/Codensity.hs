{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Codensity where

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
-- Freer + Codensity = 効率的な実行
-- ============================================================

-- Freer モナド（再掲）
data Freer f a where
  Pure :: a -> Freer f a
  Bind :: f x -> (x -> Freer f a) -> Freer f a

instance Functor (Freer f) where
  fmap g (Pure a) = Pure (g a)
  fmap g (Bind fx k) = Bind fx (fmap g . k)

instance Applicative (Freer f) where
  pure = Pure
  Pure g <*> x = fmap g x
  Bind fx k <*> x = Bind fx ((<*> x) . k)

instance Monad (Freer f) where
  Pure a >>= f = f a
  Bind fx k >>= f = Bind fx ((>>= f) . k)

send :: f a -> Freer f a
send fa = Bind fa Pure

-- ============================================================
-- 限定継続と Generator パターン
-- ============================================================

-- Freer の Bind パターンマッチは「限定継続（delimited continuation）」を
-- 提供する。パターンマッチで:
--   Bind (Ask prompt) k -> ...
-- k は「Ask の結果を受け取って残りの計算を続ける」限定継続。
--
-- これは Generator の yield に相当する:
--   const name = yield Ask("名前は？")
-- yield で一旦制御を返し、外側が値を供給して再開する。

-- | 命令セット
data Talk a where
  Ask  :: String -> Talk String
  Tell :: String -> Talk ()

-- | ステップ実行: 一命令ずつ処理する
data Step f a where
  Done :: a -> Step f a
  Await :: f x -> (x -> Freer f a) -> Step f a

-- | Freer をステップに分解する
viewFreer :: Freer f a -> Step f a
viewFreer (Pure a)    = Done a
viewFreer (Bind fx k) = Await fx k

-- | ステップ実行のインタプリタ
runStepIO :: Freer Talk a -> IO a
runStepIO m = case viewFreer m of
  Done a -> pure a
  Await (Ask prompt) k -> do
    putStrLn prompt
    input <- getLine
    runStepIO (k input)
  Await (Tell msg) k -> do
    putStrLn msg
    runStepIO (k ())

-- 純粋インタプリタ
interpretPure :: [String] -> Freer Talk a -> (a, [String])
interpretPure inputs m = case viewFreer m of
  Done a -> (a, [])
  Await (Ask _) k -> case inputs of
    (x:xs) -> interpretPure xs (k x)
    []     -> error "入力が足りません"
  Await (Tell msg) k ->
    let (a, msgs) = interpretPure inputs (k ())
     in (a, msg : msgs)

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
