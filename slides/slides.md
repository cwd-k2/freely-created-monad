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

# Generator で DSL

## 自由に手に入るモナド

---

# 問題: 定義と実行の密結合

ビジネスロジックに `console.log` を書けば、そのコードはコンソールでしか動かない

- テストするにはモック
- Web API に転用するには書き直し

→ **プログラムの定義と実行を分離したい**

---

# 今日のゴール

最終的にこう書ける DSL を **導出** する:

```typescript
const program: Program<string> = function* () {
  const name = yield* ask("名前を入力してください");
  yield* tell(`こんにちは、${name}さん！`);
  return name;
};
```

- プログラムの構造が **データ** として存在する
- 実行は **後から決める**（IO / テスト用 pure / ログ出力 …）

> Freer を天下りで与えず、**なぜそうなるか** を段階的に導出する

---

<!-- _class: lead -->

# Ch.2 — 計算の具象化

実行を差し替えるには、インタプリタが命令を識別できる必要がある
→ 制御フローを **データとして見える形** にする

---

# CPS: 具象化の第一段階

```haskell
-- Direct Style: 値を返す
add :: Int -> Int -> Int
add x y = x + y

-- CPS: 「次にやること」を受け取る
addCPS :: Int -> Int -> (Int -> r) -> r
addCPS x y k = k (x + y)
```

**継続 `k`** = 「この値を受け取って、残りの計算を行う関数」

- 「計算の残り」が **関数として明示化** される — 具象化の第一段階
- しかし関数は **中身が見えない** → DSL のインタプリタは命令を判別できない

→ 関数を **データ** にする必要がある

---

# Defunctionalization

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

関数がデータになった → パターンマッチで **識別** できる

---

# TypeScript で見る Defunctionalization

tagged union + switch がまさに Defunctionalization:

```typescript
type IntFun = { tag: "addN"; n: number } | { tag: "mulN"; n: number } | { tag: "square" };

const applyIntFun = (fun: IntFun, x: number): number => {
  switch (fun.tag) {
    case "addN":  return x + fun.n;
    case "mulN":  return x * fun.n;
    case "square": return x * x;
  }
};
```

- `tag` によるパターンマッチ = Haskell のコンストラクタに対するパターンマッチ
- コールバック関数を tagged union に置き換える — これが Defunctionalization

---

# 継続の Defunctionalization

継続チェーンをデータ化してみる:

```haskell
data ContChain a
  = DoneChain
  | forall b. StepChain (a -> b) (ContChain b)
```

> `forall b.` はデータ構築子内では**存在量化**（∃b）— 構築側は `b` を選べるが、受取側には不透明

命令列は構造化できたが…

```
StepChain  (a -> b)         (ContChain b)
            ^^^^^^^^         ^^^^^^^^^^^^
            まだ関数のまま！  残りのチェーン
```

---

# `(a -> b)` をどうデータ化する？

→ 一般の `(a -> b)` は Defunc できないが、**DSL の命令** はもっと構造が単純なはず

---

<!-- _class: lead -->

# Ch.3 — 自由モナドとステップ実行

命令を構造化してモナドにし、ステップ実行のパターンを認識する

---

# ContChain の壁と突破口

一般の `(a -> b)` は Defunc できない — 可能な関数が無限にある

しかし **DSL** の文脈では仮定を置ける:

> 「プログラムは **離散的な命令の列**（ないし木構造）である」
> → `(a -> b)` = 命令を実行 → 結果を受け取る → 次へ進む

ここで「結果を受け取って**次を選ぶ**」がポイント:

- 単なるリストでは不十分 — 後の命令が前の **結果に依存** する
- この **依存的逐次実行** こそが `>>=`（bind）の仕事 → **モナド** が必要

---

# (a → b) の分解

DSL のステップの構造:

1. **命令を実行する**（`ask "名前は？"`、`tell msg`）
2. **命令の結果を受け取る**（`String`、`()`）
3. **結果を使って次のステップへ進む**

これを型で表す:

```haskell
data TalkF next
  = Ask String (String -> next)  -- 命令 + 結果を受け取る継続
  | Tell String next             -- 命令 + 次のステップ
```

`next` = 「このステップの後に何が続くか」

---

# ContChain → TalkF: 何が見えるようになったか

```
ContChain:  StepChain  (a -> b)              (ContChain b)
                        ^^^^^^^^              ^^^^^^^^^^^^
                        不透明な関数            残りのチェーン

TalkF:      Ask "名前？" (String -> next)
            ^^^^^^^^^^   ^^^^^^^^^^^^^^^
            命令データ    結果を受け取る継続
            (識別可能)    (next が「残り」を表す)
```

- **命令部分** (`Ask "名前？"`) → データとして識別可能になった
- **継続部分** (`String -> next`) → 関数のまま残っている

> **命令の Defunctionalization + 継続の構造化**

---

# なぜ next を差し替える必要があるか — Functor

命令をチェーンにするには、`next`（続き先）を差し替える操作が必要

例: `ask` の後に `tell` を繋ぐ → `ask` の `next` を差し替える

```haskell
fmap f (Ask prompt k) = Ask prompt (f . k)
-- 命令はそのまま、継続の先に f を合成

fmap f (Tell msg next) = Tell msg (f next)
-- 命令はそのまま、次を差し替え
```

この「命令の内容を変えずに `next` だけを差し替える」操作が `fmap`

→ `TalkF` は **Functor**

---

# 自由モナドの定義

`TalkF` の `next` に「残りの計算」を再帰的に入れると:

```haskell
data Free f a
  = Pure a                -- 計算完了、結果は a
  | Free (f (Free f a))   -- 命令を1つ実行し、残りが続く
```

```haskell
instance Functor f => Monad (Free f) where
  Pure a >>= f = f a
  Free fx >>= f = Free (fmap (>>= f) fx)
--                       ^^^^
--                       Functor 制約が必要！
```

`>>=` は `fmap` で命令の中に潜り、末尾に新しい計算を接ぎ木する

---

# DSL プログラムと Free

```haskell
talkProgram :: Free TalkF String
talkProgram = do
  name <- ask "名前を入力してください"
  tell ("こんにちは、" ++ name ++ "さん！")
  pure name
```

この `talkProgram` は **データ構造** — 実行はまだ起きていない

- `do` 記法は `>>=` に脱糖される → Free のツリーが組み上がるだけ
- IO は一切発生していない — **プログラムの記述と実行が分離** されている

---

# Free のインタプリタ

```haskell
runTalkIO :: Free TalkF a -> IO a
runTalkIO (Pure a) = pure a
runTalkIO (Free (Ask prompt next)) = do
  putStrLn prompt; input <- getLine
  runTalkIO (next input)
runTalkIO (Free (Tell msg next)) = do
  putStrLn msg; runTalkIO next
```

同じプログラムに対して **異なるインタプリタ** を差し替えられる
→ テスト用の純粋実行、ログ出力など

---

# Free の課題

```haskell
Free fx >>= f = Free (fmap (>>= f) fx)
```

**2つの問題:**

1. **Functor 制約**: `fmap` のために `f` が Functor でなければならない
   - `Ask` の `(String -> next)` は `fmap` のためだけに存在
   - 命令の本質には不要
2. **性能**: `>>=` のたびに構造全体を走査 → O(n²)

どちらも `fmap` で**命令の内部に潜る**構造に起因している

→ 内部に潜らずに済む構造はないか？ — これは次章で

---

# ステップ実行の認識

`runTalkIO` をもう一度見ると — 先頭を覗き、命令を処理し、継続に結果を渡す

これは **ステップ実行** — プログラムを一命令ずつ処理するパターン

> このパターンが可能なのは、プログラムが Free で**データとして具象化**されているから
> 一般のモナド（`IO`、`Reader`）の `>>=` では中を覗けない

`bindCont` で「計算を鎖のように繋げる」と言ったその構造が
→ データのパターンマッチとして目に見える形で動いている

---

<!-- _class: lead -->

# Ch.4 — Freer と限定継続

Functor 制約を取り除き、ステップ実行を整理すると限定継続が現れる
→ Generator の `yield`/`next` と**同じ構造**であることが見える

---

# アイデア: 命令と継続の分離

```haskell
-- Free: fmap で命令の中に潜る
Free fx >>= f = Free (fmap (>>= f) fx)

-- もし分離できたら: 継続を合成するだけ
Bind fx k >>= f = Bind fx ((>>= f) . k)
```

命令 `fx` には触らず、継続 `k` を合成するだけ

→ `fmap` 不要 → **Functor 制約が消える**

しかし `Free (f (Free f a))` がどうしてこの形になるのか？
→ 存在量化された型変数 `x` はどこから来る？

---

# Coyoneda — 正式な導出経路

```haskell
data Coyoneda f a where
  Coyoneda :: (x -> a) -> f x -> Coyoneda f a
```

`f x`（素材）と `x -> a`（変換関数）のペア。`fmap` を `f` に適用せず **蓄積** する

**核心的性質**: `f` が Functor でなくても `Coyoneda f` は **常に Functor**

```haskell
instance Functor (Coyoneda f) where
  fmap g (Coyoneda h fx) = Coyoneda (g . h) fx
  -- f の fmap を呼ばず、関数合成だけ
```

---

# 導出: Free (Coyoneda f) → Freer f

```
Free (Coyoneda f (Free (Coyoneda f) a))
= Free (∃x. (x → Free (Coyoneda f) a, f x))
```

存在量化された `x` + ペア `(f x, x → ...)` → 命令と継続の分離

```haskell
data Freer f a where
  Pure :: a -> Freer f a
  Bind :: f x -> (x -> Freer f a) -> Freer f a

instance Monad (Freer f) where
  Pure a    >>= f = f a
  Bind fx k >>= f = Bind fx ((>>= f) . k)
```

`f` に **Functor 制約なし！**

---

# 命令セットの簡素化

```haskell
-- Free 版: Functor のための (String -> next) が必要
data TalkF next
  = Ask String (String -> next)
  | Tell String next

-- Freer 版: 本質だけが残る
data Talk a where
  Ask  :: String -> Talk String
  Tell :: String -> Talk ()
```

```haskell
send :: f a -> Freer f a
send fa = Bind fa Pure
-- 「命令を発行して結果をそのまま返す」最小のプログラム
```

---

# Freer の命令 = TypeScript の tagged union

Freer の GADT は TypeScript の tagged union にそのまま対応する:

```typescript
type Ask  = { readonly tag: "ask";  readonly prompt: string };  // → string
type Tell = { readonly tag: "tell"; readonly message: string }; // → void
type TalkInstruction = Ask | Tell;
```

- Free 版で必要だった `(String -> next)` のような継続フィールドは**不要**
- 命令の本質（種類 + 引数）だけが残っている
- ch05 の Generator DSL は、この tagged union をそのまま使う

---

# Step — ステップ実行の型

命令と継続が分離されたことで、ステップ実行を型として表現できる:

```haskell
data Step f a where
  Done  :: a -> Step f a                        -- 計算完了
  Await :: f x -> (x -> Freer f a) -> Step f a  -- 命令 + 継続

viewFreer :: Freer f a -> Step f a
viewFreer (Pure a)    = Done a
viewFreer (Bind fx k) = Await fx k
```

インタプリタは `Freer` の内部構造に依存せず、**Done か Await か** だけを見る

---

# Step によるインタプリタ

```haskell
runStepIO m = case viewFreer m of
  Done a             -> pure a
  Await (Ask prompt) k -> do
    putStrLn prompt; input <- getLine
    runStepIO (k input)   -- 継続に値を渡して再開
  Await (Tell msg) k -> do
    putStrLn msg; runStepIO (k ())
```

Free 版と同じ構造——**先頭を覗き、命令を処理し、継続に値を渡す**

---

# Step = TypeScript の IteratorResult

`Step` は TypeScript の `IteratorResult` にそのまま対応する:

```typescript
const step = gen.next();    // viewFreer に対応
```

| Step (Haskell)   | IteratorResult (TypeScript) |
| ---------------- | --------------------------- |
| `Done a`         | `{ done: true, value: a }`  |
| `Await fx k`     | `{ done: false, value: fx }`|
| `viewFreer m`    | `gen.next()`                |
| `k value`        | `gen.next(value)`           |

---

# 限定継続としての Freer

ステップ実行は **限定継続** の shift/reset に概念的に対応する

| 限定継続           | Freer                                   |
| ------------------ | --------------------------------------- |
| `shift`            | `send` — 命令発行、残りを継続として捕捉 |
| `reset`            | インタプリタループ — 捕捉範囲を区切る   |
| 捕捉された継続 `k` | `Bind` の第2引数 `(x -> Freer f a)`     |
| 継続に値を渡す     | `k value`                               |

```haskell
send (Ask "名前は？") >>= \name -> send (Tell ("こんにちは、" ++ name))
-- = Bind (Ask "名前は？") (\name -> ...)
--                          ^^^^^^^^^^^^^ 限定継続
```

---

# Generator への橋渡し

- **shift** = 命令を発行し、残りの計算を継続として捕捉。制御はインタプリタへ
- **`yield`** = 値を外に渡して中断。制御は呼び出し元へ。再開時に値を受け取る

どちらも「値を外に渡して中断し、外側から値を供給されて再開する」

→ `yield` = shift、`while` ループ = reset — **同じプロトコル**

---

<!-- _class: lead -->

# Ch.5 — Generator と DSL

限定継続を TypeScript の Generator で実現する

---

# Generator ↔ Freer の対応

| Freer (Haskell)      | Generator (TypeScript)         |
| -------------------- | ------------------------------ |
| `Bind fx k`          | `yield instruction`            |
| `fx` — 命令          | `instruction` — yield された値 |
| `k` — 限定継続       | yield 後の関数本体の残り       |
| パターンマッチで処理 | `gen.next(value)` で値を渡す   |
| `send` = shift       | `yield` = shift                |
| インタプリタ = reset | `while` ループ = reset         |

**重要な違い**: Freer の `k` は何度でも呼べる (multi-shot)
Generator は一度しか再開できない (one-shot)

---

# 命令定義と DSL プログラム

```typescript
type Ask = { readonly tag: "ask"; readonly prompt: string };
type Tell = { readonly tag: "tell"; readonly message: string };

function* ask(prompt: string): Generator<Ask, string, string> {
  return (yield { tag: "ask", prompt }) as string;
}

const greetProgram: Program<string> = function* () {
  const name = yield* ask("名前を入力してください");
  yield* tell(`こんにちは、${name}さん！`);
  return name;
};
```

`Program<A> = () => Generator<...>` — サンク化で不変性を担保

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

# 補遺: Codensity

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
> → Generator では `yield*` が本質的に右結合で左結合問題が発生しない

---

<!-- _class: lead -->

# まとめ

---

# Haskell ↔ TypeScript 対応表

| 概念             | Haskell                              | TypeScript            |
| ---------------- | ------------------------------------ | --------------------- |
| 命令セット       | GADT                                 | Tagged union          |
| 継続             | `(x -> Freer f a)`                   | Generator の残り      |
| 継続の性質       | multi-shot（通常の関数）             | one-shot（内部状態）  |
| モナド合成       | `>>=` / do 記法                      | `yield*` / Generator  |
| プログラム再利用 | 純粋データ                           | サンク                |
| インタプリタ     | パターンマッチ                       | switch + `gen.next()` |

> Freer の本質 — **プログラムをデータとして記述し、実行を後から決める** — は言語を超えて変わらない
