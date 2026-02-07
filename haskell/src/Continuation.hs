module Continuation where

-- ============================================================
-- 直接スタイル（Direct Style）
-- ============================================================

-- 普通の関数合成
add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

-- (3 + 4)^2 を直接スタイルで
directExample :: Int
directExample = square (add 3 4)

-- ============================================================
-- CPS（Continuation-Passing Style）
-- ============================================================

-- 「残りの計算」を引数として受け取る
addCPS :: Int -> Int -> (Int -> r) -> r
addCPS x y k = k (x + y)

squareCPS :: Int -> (Int -> r) -> r
squareCPS x k = k (x * x)

-- (3 + 4)^2 を CPS で
cpsExample :: Int
cpsExample = addCPS 3 4 (\n -> squareCPS n (\result -> result))

-- ============================================================
-- CPS の合成
-- ============================================================

-- CPS 計算の型を名前付けする
type Cont r a = (a -> r) -> r

-- CPS 計算を合成する関数
-- 「最初の計算を行い、その結果を使って次の計算を行う」
bindCont :: Cont r a -> (a -> Cont r b) -> Cont r b
bindCont m f = \k -> m (\a -> f a k)

-- bindCont を使って書き直す
cpsComposed :: Int
cpsComposed = (addCPS 3 4 `bindCont` squareCPS) id

-- ============================================================
-- 継続で「中断と再開」を表現する
-- ============================================================

-- 継続を保存して「中断」をシミュレート
data Suspended a = Suspended
  { value       :: a
  , continuation :: a -> a
  }

-- 中断して途中の値を見せる
suspendedExample :: Suspended Int
suspendedExample = addCPS 3 4 (\n -> Suspended n (\x -> x * x))

-- 再開する
resumeExample :: Int
resumeExample =
  let s = suspendedExample
   in continuation s (value s)

-- ============================================================
-- 実行例
-- ============================================================

example :: IO ()
example = do
  putStrLn "=== Continuations ==="
  putStrLn $ "直接スタイル: " ++ show directExample
  putStrLn $ "CPS:         " ++ show cpsExample
  putStrLn $ "CPS (合成):  " ++ show cpsComposed
  putStrLn $ "中断値:      " ++ show (value suspendedExample)
  putStrLn $ "再開後:      " ++ show resumeExample
