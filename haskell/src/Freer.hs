{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Freer where

-- ============================================================
-- Free モナドの Functor 制約の問題
-- ============================================================

-- Free モナドでは命令セットに Functor が必要だった:
--
--   data TalkF next
--     = Ask String (String -> next)   ← Functor のための関数
--     | Tell String next
--
-- Ask の (String -> next) は fmap を可能にするためだけに存在する。
-- これを取り除きたい。

-- ============================================================
-- Free モナドの bind を観察する
-- ============================================================

-- Free モナドの構造を思い出す:
--   data Free f a = Pure a | Free (f (Free f a))
--
-- (>>=) :: Free f a -> (a -> Free f b) -> Free f b
-- Pure a >>= f = f a
-- Free fx >>= f = Free (fmap (>>= f) fx)
--
-- ここで fmap (>>= f) は「f (Free f a) の中の Free f a に (>>= f) を適用する」。
-- Functor が必要なのはまさにこの fmap のため。
-- >>= のたびに木構造全体を走査するため、左結合チェーンで O(n^2) になる。
--
-- 動機: fmap を実際に適用するのではなく、
-- 命令と継続を分離して保持し、>>= では継続を合成するだけにしたい。
-- この直観を厳密に導出するのが Coyoneda。

-- ============================================================
-- 米田の補題（直観的理解）
-- ============================================================

-- 米田の補題: 任意の Functor f に対して
--   f a ≅ forall r. (a -> r) -> f r    (Yoneda / CPS 側)
--   f a ≅ exists x. (x -> a, f x)     (Coyoneda / データ側)
--
-- Freer の導出に直接関わるのは Coyoneda のほう:
--   data Coyoneda f a where
--     Coyoneda :: (x -> a) -> f x -> Coyoneda f a
--
-- 直観: f x の値と変換関数 (x -> a) をペアで保持する。
-- fmap を実際に適用するのではなく、変換関数を蓄積しておく。
--
-- Coyoneda f は f が Functor でなくても Functor になる!
--   fmap g (Coyoneda h fx) = Coyoneda (g . h) fx
-- g の適用を関数合成で蓄積するだけ。実際の fmap は不要。

-- ============================================================
-- Freer モナド
-- ============================================================

-- Free モナドで Coyoneda を適用すると:
--   Free (Coyoneda f) a
-- ≅ Pure a | Free (Coyoneda f (Free (Coyoneda f) a))
-- ≅ Pure a | Free (exists x. (x -> Free (Coyoneda f) a, f x))
--
-- exists x について f x（命令）と x -> Free ... a（継続）のペアを持つ。
-- これはまさに Bind :: f x -> (x -> Freer f a) -> Freer f a
-- 鍵は「f は Functor でなくてよい」ということ。

-- | Freer モナド
data Freer f a where
  Pure   :: a -> Freer f a
  Bind   :: f x -> (x -> Freer f a) -> Freer f a
  -- f x: 命令（結果の型は x）
  -- (x -> Freer f a): 結果 x を受け取って次の計算を行う継続

instance Functor (Freer f) where
  fmap g (Pure a)     = Pure (g a)
  fmap g (Bind fx k)  = Bind fx (fmap g . k)

instance Applicative (Freer f) where
  pure = Pure
  Pure g <*> x     = fmap g x
  Bind fx k <*> x  = Bind fx ((<*> x) . k)

instance Monad (Freer f) where
  Pure a    >>= f = f a
  Bind fx k >>= f = Bind fx ((>>= f) . k)
  -- 継続 k の後ろに f を合成するだけ。
  -- fmap は不要！

-- | 命令を Freer に持ち上げる
send :: f a -> Freer f a
send fa = Bind fa Pure

-- ============================================================
-- 具体例: Functor なしの命令セット
-- ============================================================

-- | 入出力の命令セット（Functor インスタンス不要！）
data Talk a where
  Ask  :: String -> Talk String   -- 質問して文字列を得る
  Tell :: String -> Talk ()       -- メッセージを表示する

-- GADT で命令の「結果の型」を直接指定。
-- (String -> next) のような継続パラメータは不要。

-- 便利な命令
ask :: String -> Freer Talk String
ask prompt = send (Ask prompt)

tell :: String -> Freer Talk ()
tell msg = send (Tell msg)

-- | プログラムの例
talkProgram :: Freer Talk String
talkProgram = do
  name <- ask "名前を入力してください"
  tell ("こんにちは、" ++ name ++ "さん！")
  age <- ask "年齢を入力してください"
  tell (age ++ "歳ですね。")
  pure name

-- ============================================================
-- インタプリタ
-- ============================================================

-- | IO で実行
runTalkIO :: Freer Talk a -> IO a
runTalkIO (Pure a) = pure a
runTalkIO (Bind (Ask prompt) k) = do
  putStrLn prompt
  input <- getLine
  runTalkIO (k input)
runTalkIO (Bind (Tell msg) k) = do
  putStrLn msg
  runTalkIO (k ())

-- | 純粋に実行（テスト用）
runTalkPure :: [String] -> Freer Talk a -> (a, [String])
runTalkPure _ (Pure a) = (a, [])
runTalkPure inputs (Bind (Ask _) k) =
  case inputs of
    (x:xs) -> runTalkPure xs (k x)
    []     -> error "入力が足りません"
runTalkPure inputs (Bind (Tell msg) k) =
  let (a, msgs) = runTalkPure inputs (k ())
   in (a, msg : msgs)

-- ============================================================
-- ステップ実行
-- ============================================================

-- | ステップ実行: 一命令ずつ処理する
data Step f a where
  Done  :: a -> Step f a
  Await :: f x -> (x -> Freer f a) -> Step f a

-- | Freer をステップに分解する
viewFreer :: Freer f a -> Step f a
viewFreer (Pure a)    = Done a
viewFreer (Bind fx k) = Await fx k

-- | ステップ実行のインタプリタ（IO 版）
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

-- | ステップ実行のインタプリタ（純粋版）
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

example :: IO ()
example = do
  putStrLn "=== Freer Monad ==="
  let (name, msgs) = runTalkPure ["太郎", "25"] talkProgram
  putStrLn $ "結果: " ++ name
  mapM_ (\msg -> putStrLn $ "  出力: " ++ msg) msgs
