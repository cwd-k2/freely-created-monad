# Defunctionalization

## 関数値をデータにする

**Defunctionalization** は、プログラム中の**関数値**（ラムダ式やクロージャなど、値として渡される関数）をデータ型に変換する手法です。

変換対象は関数値そのものです。高階関数（関数を引数に取る関数）は関数値の*消費者*であり、Defunctionalization は消費者ではなく、消費される側の関数値をデータに置き換えます。

元の高階関数版：

```haskell
applyTwiceHO :: (a -> a) -> a -> a
applyTwiceHO f x = f (f x)

-- 使用例
applyTwiceHO (+3) 10  -- 16
```

Defunctionalization の手順：

**1. 使われる関数をデータで表現する**

```haskell
data IntFun
  = AddN Int       -- (+n)
  | MulN Int       -- (*n)
  | Square         -- (^2)
```

**2. データを関数に戻す `apply` を定義する**

```haskell
applyIntFun :: IntFun -> Int -> Int
applyIntFun (AddN n) x = x + n
applyIntFun (MulN n) x = x * n
applyIntFun Square   x = x * x
```

**3. 高階関数をデータ版で書き直す**

```haskell
applyTwiceDefunc :: IntFun -> Int -> Int
applyTwiceDefunc fun x = applyIntFun fun (applyIntFun fun x)

applyTwiceDefunc (AddN 3) 10  -- 16
```

関数がデータになったことで、**パターンマッチによる識別、シリアライズ、比較、変換、最適化** などが可能になります。DSL の文脈では、インタプリタが命令をパターンマッチで識別し、命令ごとに異なる処理を行えるようになることが最も重要な利点です。

## TypeScript で考える

TypeScript プログラマにとって、Defunctionalization は馴染みのあるパターンです。

```typescript
// 高階関数版
const applyTwice = (f: (x: number) => number, x: number) => f(f(x));

// Defunctionalized 版 — まさに union type + switch
type IntFun = { tag: "addN"; n: number } | { tag: "mulN"; n: number } | { tag: "square" };

const applyIntFun = (fun: IntFun, x: number): number => {
  switch (fun.tag) {
    case "addN":  return x + fun.n;
    case "mulN":  return x * fun.n;
    case "square": return x * x;
  }
};
```

コールバック関数を tagged union に置き換える——これが Defunctionalization です。

## 継続の Defunctionalization

前章で見た CPS の継続も関数です。DSL のインタプリタが命令を識別して処理を分岐するには、命令がデータとして見える必要があります。ここでは、継続に含まれる「何をするか」の部分をデータ化していきます。

```haskell
-- 「何をするか」をデータで表す
data ContData
  = ThenSquare          -- 次に二乗する
  | ThenAdd Int         -- 次に n を足す
  | Done                -- 何もしない

-- データを関数に戻す
applyCont :: ContData -> Int -> Int
applyCont ThenSquare x = x * x
applyCont (ThenAdd n) x = x + n
applyCont Done       x = x
```

しかし、継続を**連鎖**させたい場合、途中のステップにはまだ関数が残ります。

```haskell
data ContChain a
  = DoneChain
  | forall b. StepChain (a -> b) (ContChain b)
  -- forall b. はデータ構築子の中では「存在量化」として働く
  -- （Haskell では ExistentialQuantification または GADTs 拡張が必要）
```

`(a -> b)` をどうデータ化するか？——次章で、この部分を**命令セット（Functor）** として抽象化し、自由モナドを導出します。

> 📖 対応コード: [`haskell/src/Defunctionalization.hs`](../haskell/src/Defunctionalization.hs)
