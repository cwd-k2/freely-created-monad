module Defunctionalization where

-- ============================================================
-- 高階関数のデータ化（Defunctionalization）
-- ============================================================

-- 高階関数版: 関数を受け取って適用する
applyTwiceHO :: (a -> a) -> a -> a
applyTwiceHO f x = f (f x)

hoExample :: Int
hoExample = applyTwiceHO (+3) 10  -- 16

-- Defunctionalization: 関数をデータに変換する

-- Step 1: 使われる関数をデータで表現する
data IntFun
  = AddN Int       -- (+n)
  | MulN Int       -- (*n)
  | Square         -- (^2)

-- Step 2: データを関数に戻す（apply 関数）
applyIntFun :: IntFun -> Int -> Int
applyIntFun (AddN n) x = x + n
applyIntFun (MulN n) x = x * n
applyIntFun Square   x = x * x

-- Step 3: 高階関数をデータ版で書き直す
applyTwiceDefunc :: IntFun -> Int -> Int
applyTwiceDefunc fun x = applyIntFun fun (applyIntFun fun x)

defuncExample :: Int
defuncExample = applyTwiceDefunc (AddN 3) 10  -- 16

-- ============================================================
-- 継続の Defunctionalization
-- ============================================================

-- CPS 版の計算（前章から）
type Cont r a = (a -> r) -> r

addCPS :: Int -> Int -> Cont r Int
addCPS x y k = k (x + y)

squareCPS :: Int -> Cont r Int
squareCPS x k = k (x * x)

-- 継続もまた「関数」なので、データ化できる

-- 「addCPS 3 4 の後に何をするか」をデータで表す
data ContData
  = ThenSquare          -- 次に二乗する
  | ThenAdd Int         -- 次に n を足す
  | Done                -- もう何もしない（恒等継続）

-- 継続データを適用する
applyCont :: ContData -> Int -> Int
applyCont ThenSquare x = x * x
applyCont (ThenAdd n) x = x + n
applyCont Done       x = x

-- データ化された継続を使って計算
defuncCPSExample :: Int
defuncCPSExample = addCPS 3 4 (applyCont ThenSquare)  -- 49

-- ============================================================
-- 継続のチェーン（合成）をデータで表す
-- ============================================================

-- 継続の連鎖もデータで表現できる
data ContChain a
  = DoneChain                                -- 終了
  | forall b. StepChain (a -> b) (ContChain b)  -- 一歩進んで続ける

-- ……しかし、ここで (a -> b) がまだ関数のまま。
-- この関数もデータ化するには？ → 次章で、命令セット（Functor）として抽象化する。

-- ============================================================
-- 実行例
-- ============================================================

example :: IO ()
example = do
  putStrLn "=== Defunctionalization ==="
  putStrLn $ "高階関数版:         " ++ show hoExample
  putStrLn $ "Defunctionalized版: " ++ show defuncExample
  putStrLn $ "継続のデータ化:     " ++ show defuncCPSExample
