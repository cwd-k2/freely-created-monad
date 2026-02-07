{-# LANGUAGE GADTs #-}

module Free where

-- ============================================================
-- 自由モナド（Free Monad）
-- ============================================================

-- 前章で「継続のチェーンをデータ化したい」という課題が残った。
-- そこで (a -> b) の部分を Functor f に抽象化する。

-- | Free モナド: Functor f の上に構築されるモナド
data Free f a
  = Pure a                  -- 計算が完了して値 a を持つ
  | Free (f (Free f a))    -- 命令 f を一つ実行し、残りの計算が続く

-- Functor f が必要
instance Functor f => Functor (Free f) where
  fmap g (Pure a) = Pure (g a)
  fmap g (Free fx) = Free (fmap (fmap g) fx)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure g <*> x = fmap g x
  Free fg <*> x = Free (fmap (<*> x) fg)

instance Functor f => Monad (Free f) where
  Pure a >>= f = f a
  Free fx >>= f = Free (fmap (>>= f) fx)

-- ============================================================
-- 具体例: 簡単な命令セット
-- ============================================================

-- | 入出力の命令セット
data TalkF next
  = Ask String (String -> next)   -- 質問して、答えを受け取って次へ
  | Tell String next              -- メッセージを表示して次へ
  deriving Functor

-- | Free モナドに持ち上げるヘルパー
liftFree :: Functor f => f a -> Free f a
liftFree fx = Free (fmap Pure fx)

-- 便利な命令
ask :: String -> Free TalkF String
ask prompt = liftFree (Ask prompt id)

tell :: String -> Free TalkF ()
tell msg = liftFree (Tell msg ())

-- | プログラムの例
talkProgram :: Free TalkF String
talkProgram = do
  name <- ask "名前を入力してください"
  tell ("こんにちは、" ++ name ++ "さん！")
  age <- ask "年齢を入力してください"
  tell (age ++ "歳ですね。")
  pure name

-- ============================================================
-- インタプリタ: Free モナドを実際に実行する
-- ============================================================

-- | IO で対話的に実行するインタプリタ
runTalkIO :: Free TalkF a -> IO a
runTalkIO (Pure a) = pure a
runTalkIO (Free (Ask prompt next)) = do
  putStrLn prompt
  input <- getLine
  runTalkIO (next input)
runTalkIO (Free (Tell msg next)) = do
  putStrLn msg
  runTalkIO next

-- | 純粋なインタプリタ（テスト用）
runTalkPure :: [String] -> Free TalkF a -> (a, [String])
runTalkPure _inputs (Pure a) = (a, [])
runTalkPure inputs (Free (Ask _ next)) =
  case inputs of
    (x:xs) -> runTalkPure xs (next x)
    []     -> error "入力が足りません"
runTalkPure inputs (Free (Tell msg next)) =
  let (a, msgs) = runTalkPure inputs next
   in (a, msg : msgs)

-- ============================================================
-- 実行例
-- ============================================================

example :: IO ()
example = do
  putStrLn "=== Free Monad ==="
  let (name, msgs) = runTalkPure ["太郎", "25"] talkProgram
  putStrLn $ "結果: " ++ name
  mapM_ (\msg -> putStrLn $ "  出力: " ++ msg) msgs
