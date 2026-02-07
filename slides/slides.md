---
marp: true
theme: default
class: invert
paginate: true
style: |
  :root {
    --color-bg: #1b1d2a;
    --color-fg: #d4d6e4;
    --color-accent: #7cacf8;
    --color-accent-dim: #4a6fa5;
    --color-surface: #252840;
    --color-border: #363850;
    --color-highlight: #f0c674;
  }

  section {
    background: var(--color-bg);
    color: var(--color-fg);
    font-family: 'Helvetica Neue', Arial, 'Hiragino Kaku Gothic ProN', 'Hiragino Sans', sans-serif;
    font-size: 28px;
    letter-spacing: 0.01em;
    padding: 50px 60px;
    text-align: left;
  }

  h1 {
    color: var(--color-accent);
    font-size: 1.5em;
    font-weight: 600;
    border-bottom: 2px solid var(--color-border);
    padding-bottom: 0.25em;
    margin-bottom: 0.6em;
  }

  h2 { color: var(--color-accent-dim); font-weight: 400; }

  strong { color: var(--color-highlight); font-weight: 600; }

  a { color: var(--color-accent); }

  /* ── Code ── */

  code {
    font-family: 'JetBrains Mono', 'Fira Code', 'SF Mono', monospace;
    font-size: 0.85em;
    background: var(--color-surface) !important;
    color: var(--color-fg) !important;
    padding: 0.1em 0.35em;
    border-radius: 4px;
  }

  pre {
    background: var(--color-surface) !important;
    border: 1px solid var(--color-border);
    border-radius: 8px;
    padding: 0.8em 1em !important;
    margin: 0.5em 0;
    line-height: 1.45;
    text-align: left;
  }

  pre code {
    background: transparent !important;
    color: var(--color-fg) !important;
    padding: 0;
    font-size: 0.82em;
    text-align: left;
  }

  /* ── Table ── */

  table {
    font-size: 0.82em;
    border-collapse: collapse;
    width: 100%;
  }

  table th {
    background: var(--color-surface);
    color: var(--color-accent);
    font-weight: 600;
    border-bottom: 2px solid var(--color-accent-dim);
    padding: 0.5em 0.8em;
    text-align: left;
  }

  table td {
    border-bottom: 1px solid var(--color-border);
    padding: 0.45em 0.8em;
    text-align: left;
  }

  /* ── Blockquote ── */

  blockquote {
    font-size: 0.88em;
    border-left: 3px solid var(--color-accent-dim);
    background: var(--color-surface);
    padding: 0.5em 1em;
    border-radius: 0 6px 6px 0;
    color: #a8abbe;
  }

  blockquote strong { color: var(--color-highlight); }

  /* ── List ── */

  ul, ol { margin: 0.3em 0; }
  li { margin: 0.2em 0; line-height: 1.5; }
  li::marker { color: var(--color-accent-dim); }

  /* ── Lead slides ── */

  section.lead {
    text-align: center;
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
  }

  section.lead h1 {
    font-size: 2.2em;
    border-bottom: none;
    padding-bottom: 0;
    margin-bottom: 0.3em;
    letter-spacing: 0.02em;
  }

  section.lead h2 {
    font-size: 1.05em;
    color: #7a7d94;
    font-weight: 300;
    margin-top: 0;
  }

  section.lead p {
    color: #8a8da6;
    font-size: 0.95em;
  }

  /* ── Dense slides ── */

  section.dense {
    font-size: 22px;
    padding: 35px 50px;
  }

  section.dense h1 {
    font-size: 1.4em;
    margin-bottom: 0.35em;
  }

  section.dense pre {
    padding: 0.6em 0.9em !important;
    margin: 0.35em 0;
  }

  section.dense pre code { font-size: 0.8em; }
  section.dense table { font-size: 0.88em; }
  section.dense blockquote { font-size: 0.9em; }

  /* ── Page number ── */

  section::after {
    color: #4a4d64;
    font-size: 0.6em;
  }
---

<!-- _class: lead -->

# 自由モナドから Freer モナドへ

## 継続・Defunctionalization・自由モナドの段階的導出

---

# 今日のゴール

最終的にこう書ける DSL を **導出** する:

```typescript
const program = Do(function* () {
  const name = yield* ask("名前を入力してください");
  yield* tell(`こんにちは、${name}さん！`);
  return name;
});
```

- プログラムの構造が **データ** として存在する
- 実行は **後から決める**（IO / テスト用 pure / ログ出力 …）

> Freer を天下りで与えず、**なぜそうなるか** を段階的に導出する

---

# ロードマップ

| #   | テーマ                  | キーワード                |
| --- | ----------------------- | ------------------------- |
| 1   | **継続**                | CPS, `Cont r a`           |
| 2   | **Defunctionalization** | 関数→データ, `apply`      |
| 3   | **Free モナド**         | 命令の構造化, `Functor`   |
| 4   | **Freer モナド**        | Coyoneda, Functor 不要    |
| 5   | **実行基盤**            | Step, 限定継続, Codensity |
| 6   | **TypeScript DSL**      | Generator, one-shot       |

各ステップで **前章の課題 → 本章の解決** を繰り返す

---

<!-- _class: lead -->

# Ch.2 — 継続

「計算の残り」を値にする

---

# Direct Style vs CPS

```haskell
-- Direct Style: 値を返す
add :: Int -> Int -> Int
add x y = x + y

-- CPS: 「次にやること」を受け取る
addCPS :: Int -> Int -> (Int -> r) -> r
addCPS x y k = k (x + y)
```

**継続 `k`** = 「この値を受け取って、残りの計算を行う関数」

- 計算を **中断・再開** できる
- 制御の流れを **プログラマブル** にする

---

# Cont 型と bindCont

```haskell
type Cont r a = (a -> r) -> r

bindCont :: Cont r a -> (a -> Cont r b) -> Cont r b
bindCont m f = \k -> m (\a -> f a k)
```

- `m`: 値 `a` を生む計算
- `f`: `a` を受けて次の計算を作る
- `bindCont` で継続を **体系的に合成** できる

---

# 継続の壁

継続は **関数** → 中身を見られない

```
bindCont m f = \k -> m (\a -> f a k)
                        ^^^^^^^^^^^^^
                        ブラックボックス
```

DSL のインタプリタは **命令の種類を判別** して分岐したい

→ 継続を **データ** にする必要がある

---

<!-- _class: lead -->

# Ch.3 — Defunctionalization

関数値をデータに変換する

---

# Defunctionalization の手順

**関数値** → **データ型 + apply 関数**

```haskell
-- 1. 関数をデータで表現
data IntFun = AddN Int | MulN Int | Square

-- 2. apply で関数に戻す
applyIntFun :: IntFun -> Int -> Int
applyIntFun (AddN n) x = x + n
applyIntFun (MulN n) x = x * n
applyIntFun Square   x = x * x
```

関数の **「何をするか」** がデータとして見える

---

# 継続の Defunctionalization

継続チェーンをデータ化してみる:

```haskell
data ContChain a
  = DoneChain
  | forall b. StepChain (a -> b) (ContChain b)
```

命令列は構造化できたが…

```
StepChain  (a -> b)         (ContChain b)
            ^^^^^^^^         ^^^^^^^^^^^^
            まだ関数のまま！  残りのチェーン
```

**`(a -> b)` をどうデータ化する？**

---

# ContChain の壁と突破口

一般の `(a -> b)` は Defunc できない — 可能な関数が無限にある

しかし **DSL** の文脈では仮定を置ける:

> 「プログラムは **離散的な命令の列** である」
> → `(a -> b)` = 命令を実行 → 結果を受け取る → 次へ進む

ここで「結果を受け取って**次を選ぶ**」がポイント:

- 単なるリストでは不十分 — 後の命令が前の **結果に依存** する
- この **依存的逐次実行** を扱う仕組みが必要 → **モナド**

命令セットを構造化しつつ、モナドとして合成する → **Free モナド** へ

---

<!-- _class: lead -->

# Ch.4 — Free モナド

命令セットを Functor で構造化する

---

# なぜモナドが必要か

単なる命令リスト `[Instruction]` では不十分:

```typescript
const name = yield * ask("名前は？");
yield * tell(`こんにちは、${name}さん！`);
//                       ^^^^ 前の結果に依存！
```

後の命令が前の **結果に依存** する → **依存的逐次実行**

これには命令間の **継続** が必要 → まさに `>>=` の仕事

```
命令 → 結果 → (結果に基づいて次の命令を選ぶ) → 結果 → ...
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
              この関数が >>= の第二引数
```

---

# Free モナドの定義

```haskell
data Free f a
  = Pure a                -- 計算完了、結果は a
  | Free (f (Free f a))   -- 命令を1つ実行し、残りが続く
```

命令セットは **Functor** で表現:

```haskell
data TalkF next
  = Ask String (String -> next)  -- 質問 + 継続
  | Tell String next             -- 出力 + 継続
  deriving Functor
```

`next` = 「この命令の後に何が続くか」

---

# ContChain → Free

```
ContChain:   StepChain  (a -> b)        (ContChain b)
                         ^^^^^^^^        ^^^^^^^^^^^^
                         不透明な関数      残りのチェーン

Free:        Free       (f next)
                         ^^^^^^
                         構造化された命令 + 残りのチェーン
```

ContChain で不透明だった `(a -> b)` のうち、
**命令** (Ask, Tell) がデータとして見えるようになった

> Free = ContChain の命令部分を Defunctionalization したもの

---

# Free の bind と DSL プログラム

```haskell
instance Functor f => Monad (Free f) where
  Pure a >>= f = f a
  Free fx >>= f = Free (fmap (>>= f) fx)
--                       ^^^^
--                       Functor 制約が必要！
```

`>>=` は `fmap` で命令の中に潜る → `f` が **Functor** でなければならない

`TalkF` の `(String -> next)` は、この `fmap` を通すためだけに存在する

```haskell
talkProgram :: Free TalkF String
talkProgram = do
  name <- ask "名前を入力してください"
  tell ("こんにちは、" ++ name ++ "さん！")
  pure name
```

---

# Free の課題

```haskell
Free fx >>= f = Free (fmap (>>= f) fx)
```

**2つの問題:**

1. **Functor 制約**: `fmap` のために `f` が Functor でなければならない
   - `(String -> next)` は `fmap` のためだけに存在
   - 命令の本質には不要

2. **性能**: `>>=` のたびに構造全体を走査 → O(n²)

→ 命令と継続を **分離** できないか？

---

<!-- _class: lead -->

# Ch.5 — Free から Freer へ

Functor 制約を取り除く

---

# アイデア: 命令と継続の分離

```haskell
-- Free: fmap で命令の中に潜る
Free fx >>= f = Free (fmap (>>= f) fx)

-- Freer: 継続を合成するだけ
Bind fx k >>= f = Bind fx ((>>= f) . k)
```

命令 `fx` には触らず、継続 `k` を合成するだけ

→ `fmap` 不要 → **Functor 制約が消える**

---

# Coyoneda — 正式な導出経路

```haskell
data Coyoneda f a where
  Coyoneda :: (x -> a) -> f x -> Coyoneda f a
```

**核心的性質**: `f` が Functor でなくても `Coyoneda f` は **常に Functor**

```haskell
instance Functor (Coyoneda f) where
  fmap g (Coyoneda h fx) = Coyoneda (g . h) fx
  -- f の fmap を呼ばず、関数合成だけ
```

米田の補題に由来するが、使うのは「常に Functor になる」構成的事実

---

# 導出: Free (Coyoneda f) → Freer f

```
Free (Coyoneda f (Free (Coyoneda f) a))
= Free (∃x. (x → Free (Coyoneda f) a, f x))
```

存在量化された `x` + ペア `(f x, x → ...)` → まさに分離

```haskell
data Freer f a where
  Pure :: a -> Freer f a
  Bind :: f x -> (x -> Freer f a) -> Freer f a
```

`f` に **Functor 制約なし！**

---

# Freer の Monad インスタンス

```haskell
instance Monad (Freer f) where
  Pure a    >>= f = f a
  Bind fx k >>= f = Bind fx ((>>= f) . k)
  -- 継続の合成だけ、fmap 不要
```

命令セットもシンプルに:

```haskell
data Talk a where
  Ask  :: String -> Talk String  -- 質問 → 文字列を返す
  Tell :: String -> Talk ()      -- 出力 → () を返す
```

`(String -> next)` が消え、**本質的な情報だけ** が残る

---

# send ヘルパー

```haskell
send :: f a -> Freer f a
send fa = Bind fa Pure
```

命令を Freer に持ち上げる — 「命令を発行して結果をそのまま返す」

```haskell
ask :: String -> Freer Talk String
ask prompt = send (Ask prompt)

tell :: String -> Freer Talk ()
tell msg = send (Tell msg)
```

Freer の構造は完成 → 次は **どう実行するか**

---

<!-- _class: lead -->

# Ch.6 — 実行基盤

Step 実行・限定継続・Codensity

---

# Step 型と viewFreer

```haskell
data Step f a where
  Done  :: a -> Step f a
  Await :: f x -> (x -> Freer f a) -> Step f a

viewFreer :: Freer f a -> Step f a
viewFreer (Pure a)    = Done a
viewFreer (Bind fx k) = Await fx k
```

インタプリタに必要な情報だけを公開する抽象化層

現在の実装では自明だが、**内部表現を変えたときに非自明になる**

---

# Step ベースのインタプリタ

```haskell
runStepIO :: Freer Talk a -> IO a
runStepIO m = case viewFreer m of
  Done a             -> pure a
  Await (Ask prompt) k -> do
    putStrLn prompt
    input <- getLine
    runStepIO (k input)   -- 値を継続に渡して再開
  Await (Tell msg) k -> do
    putStrLn msg
    runStepIO (k ())
```

ループ構造: 命令を覗く → 処理 → 結果を `k` に渡す → 再帰

---

# 限定継続としての Freer

Step 実行は **限定継続** の shift/reset 構造そのもの

| 限定継続           | Freer                                   |
| ------------------ | --------------------------------------- |
| `shift`            | `send` — 命令発行、残りを継続として捕捉 |
| `reset`            | インタプリタループ — 捕捉範囲を区切る   |
| 捕捉された継続 `k` | `Bind` の第2引数 `(x -> Freer f a)`     |
| 継続に値を渡す     | `k value`                               |

```haskell
send (Ask "名前は？") >>= \name -> send (Tell ("こんにちは、" ++ name))
-- = Bind (Ask "名前は？") (\name -> send (Tell (...)))
--                          ^^^^^^^^^^^^^^^^^^^^^^^^^
--                          限定継続
```

---

# Codensity — O(n²) の解決

```haskell
Bind fx k >>= f = Bind fx ((>>= f) . k)
-- 左結合チェーンで継続がネスト → 実行時 O(n²)
```

```haskell
newtype Codensity m a = Codensity
  { runCodensity :: forall r. (a -> m r) -> m r }
```

- `>>=` が常に **O(1)** — 差分リストと同じアイデア
- `forall r.` のパラメトリシティが結合順序の入れ替えを保証

> do 記法は **右結合** にデシュガーされるため O(n²) は発生しない
> → DSL 実装では Codensity は **省略**

---

<!-- _class: lead -->

# Ch.7 — TypeScript DSL

Generator = 言語レベルの限定継続

---

# Generator ↔ Freer の対応

| Freer (Haskell)      | Generator (TypeScript)         |
| -------------------- | ------------------------------ |
| `Bind fx k`          | `yield instruction`            |
| `fx` — 命令          | `instruction` — yield された値 |
| `k` — 限定継続       | yield 後の関数本体の残り       |
| パターンマッチで処理 | `gen.next(value)` で値を渡す   |

**重要な違い**: one-shot vs multi-shot

- 素の Freer: `k` は通常の関数 → 何度でも呼べる (multi-shot)
- Codensity で最適化: 継続の再結合が起きる → **one-shot 前提**
- Generator: 内部状態あり → 一度しか再開できない (one-shot)

---

# 命令定義

```typescript
type Ask = { readonly tag: "ask"; readonly prompt: string };
type Tell = { readonly tag: "tell"; readonly message: string };

type TalkInstruction = Ask | Tell;
```

Defunc された命令セット（Haskell の GADT に対応）

```typescript
function* ask(prompt: string): Generator<Ask, string, string> {
  return (yield { tag: "ask", prompt }) as string;
}

function* tell(message: string): Generator<Tell, void, void> {
  return (yield { tag: "tell", message }) as void;
}
```

---

# DSL プログラム

```typescript
const greetProgram = Do(function* () {
  const name = yield* ask("名前を入力してください");
  yield* tell(`こんにちは、${name}さん！`);
  const age = yield* ask("年齢を入力してください");
  yield* tell(`${age}歳ですね。`);
  return name;
});
```

Haskell の do 記法とほぼ同じ見た目

**サンク化が重要**: `Program<A> = () => Generator<...>`

- Generator は one-shot → 使い切り
- サンクで毎回新しい Generator を生成 → 同じプログラムを複数インタプリタで実行可能

---

<!-- _class: dense -->

# インタプリタ

```typescript
function runPure<A>(
  program: Program<A>,
  inputs: string[],
): { result: A; outputs: string[] } {
  const gen = program(); // サンクから Generator を生成
  const outputs: string[] = [];
  let inputIndex = 0;
  let step = gen.next();

  while (!step.done) {
    // reset（ループ境界）
    switch (step.value.tag) {
      case "ask":
        step = gen.next(inputs[inputIndex++]); // k(value)
        break;
      case "tell":
        outputs.push(step.value.message);
        step = gen.next(undefined);
        break;
    }
  }
  return { result: step.value, outputs };
}
```

---

<!-- _class: lead -->

# まとめ

---

<!-- _class: dense -->

# 導出の全体像

```
継続 (CPS)
  │  「計算の残り」を値にする
  │  課題: 関数は中身が見えない
  ▼
Defunctionalization
  │  関数→データに変換
  │  課題: 継続の (a→b) がまだ関数
  ▼
Free モナド
  │  命令セットを Functor で構造化
  │  課題: Functor 制約が不要に重い
  ▼
Freer モナド (Coyoneda)
  │  命令と継続を分離、Functor 不要
  │  実行基盤: Step + 限定継続
  ▼
TypeScript DSL
    Generator = 限定継続の言語サポート
```

---

# Haskell ↔ TypeScript 対応表

| 概念             | Haskell                              | TypeScript            |
| ---------------- | ------------------------------------ | --------------------- |
| 命令セット       | GADT                                 | Tagged union          |
| 継続             | `(x -> Freer f a)`                   | Generator の残り      |
| 継続の性質       | 素: multi-shot / Codensity: one-shot | one-shot              |
| モナド合成       | `>>=` / do 記法                      | `yield*` / Generator  |
| プログラム再利用 | 純粋データ                           | サンク                |
| インタプリタ     | パターンマッチ                       | switch + `gen.next()` |

> Freer の本質 — **プログラムをデータとして記述し、実行を後から決める** — は言語を超えて変わらない
