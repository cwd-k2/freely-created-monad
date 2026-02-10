# 自由モナドとステップ実行

継続のチェーン `ContChain` には不透明な関数 `(a -> b)` が残っていました。この関数の中身はどうなっているのか？——DSL という文脈に立ち返ると、その構造を明らかにでき、自由モナドが自然に導出されます。そして、自由モナドのインタプリタには**ステップ実行**（「先頭を覗き、命令を処理し、継続に値を渡す」）というパターンが潜んでいます。

## なぜモナドが必要か

`ContChain` の `(a -> b)` の構造化に進む前に、ひとつ確認すべきことがあります。命令をデータにするだけで DSL は書けるのでしょうか？

```typescript
// 命令のリスト——これだけで十分？
const program: TalkInstruction[] = [
  { tag: "ask", prompt: "名前は？" },
  { tag: "tell", message: "こんにちは" },  // ← 固定文字列しか書けない
];
```

これは「プログラムがデータである」「実行を後から決められる」という性質を満たしますが、決定的に足りないものがあります。**後の命令が前の命令の結果に依存する**ことを表現できません。

```typescript
const name = yield* ask("名前は？");
yield* tell(`こんにちは、${name}さん！`);
//                       ^^^^ ask の結果に依存
```

`tell` の内容が `ask` の結果に依存しています。命令列が静的に決まるなら `Instruction[]` で十分ですが、この**依存的な逐次実行**を表現するには、命令の間に「結果を受け取って次のプログラムを決める関数」——つまり**継続**——が必要です。

```
命令 → 結果 → (結果に基づいて次の命令を選ぶ) → 結果 → ...
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
              この関数が >>=（bind）の第二引数
```

これはモナドの `>>=`（bind）の型そのものであり、モナド構造が必要な理由です。前章の `bindCont` が「計算を鎖のように繋げる」操作だったことを思い出してください。`>>=` はその一般化です。

```
m >>= f
  m: 値 a を生み出す計算
  f: a を受け取って次の計算を決める関数（= bindCont の第二引数）
```

前章で `ContChain` の `(a -> b)` を「まだ関数が残っている」と指摘しました。この関数は消せません。**依存的な逐次実行の本質**だからです。消すべきなのは関数の不透明さであり、関数の存在そのものではありません。

## 継続のチェーンに残った関数

前章で、継続のチェーンをデータ化しようとしたとき、次のような型に辿り着きました。

```haskell
data ContChain a
  = DoneChain
  | forall b. StepChain (a -> b) (ContChain b)
```

`StepChain` の `(a -> b)` は「今の値 `a` を受け取り、何かをして次の値 `b` を生み出す」関数です。この関数は完全に不透明で、Defunctionalization できていません。

### (a -> b) の中身を分解する

この `(a -> b)` は実際には何をしているのでしょうか？一般の関数 `(a -> b)` は任意の計算を表せるので、これ以上分解する方法はありません。ただ、ここで思い出すべきは、私たちが構築しようとしているのは **DSL** だということです。

DSL のプログラムでは、各ステップは「何らかの離散的な命令を実行し、その結果を使って次に進む」という構造を持ちます。ContChain の `(a -> b)`（「`a` を受け取り何かをして `b` を返す」）の「何かをする」は、DSL では命令の実行に限定されます。

```
ask "名前は？"  →  (\name -> tell ("こんにちは、" ++ name) → (\() -> ...))
```

`ask "名前は？"` が命令の実行、`\name -> ...` が結果を受け取って次へ進む部分です。ContChain はこの全体を不透明な `(a -> b)` に閉じ込めていましたが、DSL の各ステップには共通の構造があります。

1. **何らかの命令を実行する**（`ask "名前は？"`、`tell "こんにちは"`）
2. **命令の結果を受け取る**（`ask` なら `String`、`tell` なら `()`）
3. **その結果を使って次のステップへ進む**

つまり DSL の文脈では、`(a -> b)` は「**命令データ + 結果を受け取る継続**」に分解できます。これは一般的な定理ではなく、「プログラムは離散的な命令の列である」というドメインの仮定に基づく設計上の選択です。

### 命令セットを型で表す

この分解を型で表しましょう。各命令は「命令の種類と引数」「結果の型」「結果を受け取って次に進む継続」を持ちます。

```haskell
data TalkF next
  = Ask String (String -> next)   -- 命令: Ask prompt
                                  -- 結果型: String
                                  -- 継続: String -> next
  | Tell String next              -- 命令: Tell msg
                                  -- 結果型: ()
                                  -- 継続: () -> next （簡略化して直接 next）
```

型パラメータ `next` が「このステップの後に何が続くか」を表しています。これは `ContChain` の `(a -> b)` を「命令データ + 結果を受け取る継続」に分解したものです。同時に、`ContChain` では `StepChain` が並列に並んでいた構造が、`TalkF` では `next` を介して入れ子になっています。

```
ContChain:   StepChain  (a -> b)            (ContChain b)
                         ^^^^^^^^            ^^^^^^^^^^^^
                         不透明な関数          残りのチェーン

TalkF:       Ask "名前？" (String -> next)
             ^^^^^^^^^^   ^^^^^^^^^^^^^^^
             命令データ    結果を受け取る継続
             (識別可能)    (next が「残り」を表す)
```

不透明だった `(a -> b)` のうち、命令部分（`Ask "名前？"`）がデータとして識別可能になりました。ただし、継続部分（`String -> next`）は関数のまま残っています。これは「**命令の Defunctionalization + 継続の構造化**」です。

### なぜ next を差し替える必要があるか — Functor

`TalkF next` の型パラメータ `next` は「この命令の後に何が続くか」を表しています。ただしコンストラクタを一つ作った時点では、後に何が続くかは未定です。`Ask "名前？" id` の `id` は「結果をそのまま返す」仮の継続にすぎません。

命令をチェーンにするとは、この仮の `next` を実際の後続計算に差し替えることです。たとえば `Ask` の後に `Tell` を繋ぐには、`Ask` の `next` を「`Tell` を実行してから終了する計算」に差し替えます。

ところが `next` は `TalkF` の**中**に閉じ込められています。`Ask prompt (String -> next)` の `next` は継続関数の返り値の位置にあり、外から直接書き換えることができません。そこで必要になるのが、命令の内容を変えずに `next` だけを差し替える操作——`fmap` です。

```haskell
fmap f (Ask prompt k) = Ask prompt (f . k)  -- 命令はそのまま、継続の先に f を合成
fmap f (Tell msg next) = Tell msg (f next)   -- 命令はそのまま、次を差し替え
```

`fmap` が定義できる（すなわち `TalkF` が **Functor** である）ことで、命令を自由にチェーンに組み込めるようになります。

## 自由モナドの定義

前節で命令セットを Functor `f` として構造化しました。`f` の型パラメータ `next` に「残りの計算」を入れることで、命令をチェーン状に繋げられます。この構造を再帰的に定義すると、自由モナド（Free Monad）が得られます。

```haskell
data Free f a
  = Pure a                  -- 計算完了、値 a を持つ
  | Free (f (Free f a))    -- 命令 f を一つ実行し、残りの計算が続く
```

- `Pure a` — 「もう何もしなくていい、結果は `a`」
- `Free (f (Free f a))` — 「命令 `f` の `next` に `Free f a`（残りの計算）を入れたもの」

`TalkF` で具体化すると、`Free (Ask "名前？" (\name -> Free (Tell name (Pure ()))))` のように、命令が入れ子のデータ構造として表現されます。`f` が Functor であれば、この構造は自動的に Monad になります。

## Monad インスタンス

```haskell
instance Functor f => Monad (Free f) where
  Pure a >>= f = f a                      -- 値があればそのまま関数を適用
  Free fx >>= f = Free (fmap (>>= f) fx)  -- 命令の「中」に潜って再帰的に >>= を適用
```

`>>=`（bind）の動作：

- `Pure a` の場合：単に `f a` を返す。継続を直接適用。
- `Free fx` の場合：命令 `fx` の中にある「残りの計算」に対して再帰的に `>>= f` を適用する。つまり、命令チェーンの**末尾に新しい計算を接ぎ木**する。この「接ぎ木」は毎回チェーンの先頭から末端までを辿り直すため、チェーンが長くなると性能問題に繋がります（補遺で詳しく扱います）。

## 具体例: 入出力の DSL

命令セットを Functor として定義します。

```haskell
data TalkF next
  = Ask String (String -> next)   -- 質問して、答えを使って次へ
  | Tell String next              -- メッセージを表示して次へ
  deriving Functor
```

`TalkF` の型パラメータ `next` が「次の計算」を表しています。`deriving Functor` で、`next` の部分に関数を適用できるようになります。

個々の命令を Free モナドに持ち上げるヘルパーを定義します。単一の命令だけからなるプログラムを作りたいので、`next` を `Pure`（= 計算完了）で埋めます。前節で見たように `next` は `f` の内部にあるため、差し替えには `fmap` を使います。

```haskell
liftFree :: Functor f => f a -> Free f a
liftFree fx = Free (fmap Pure fx)
-- fmap で命令 fx の内部の next を Pure に差し替える
```

これを使って便利な命令関数を定義します。

```haskell
ask :: String -> Free TalkF String
ask prompt = liftFree (Ask prompt id)

tell :: String -> Free TalkF ()
tell msg = liftFree (Tell msg ())
```

do 記法でプログラムが書けます。

```haskell
talkProgram :: Free TalkF String
talkProgram = do
  name <- ask "名前を入力してください"
  tell ("こんにちは、" ++ name ++ "さん！")
  age <- ask "年齢を入力してください"
  tell (age ++ "歳ですね。")
  pure name
```

この `talkProgram` は **データ構造** です。実行はまだ行われていません。

## インタプリタ: データから実行へ

同じプログラムに対して異なるインタプリタを差し替えられるのが Free モナドの強みです。

```haskell
-- IO で対話的に実行
runTalkIO :: Free TalkF a -> IO a
runTalkIO (Pure a) = pure a
runTalkIO (Free (Ask prompt next)) = do
  putStrLn prompt
  input <- getLine
  runTalkIO (next input)
runTalkIO (Free (Tell msg next)) = do
  putStrLn msg
  runTalkIO next

-- 純粋なインタプリタ（テスト用）
runTalkPure :: [String] -> Free TalkF a -> (a, [String])
```

プログラムの定義と実行が分離されました。ただし Free モナドには課題があり、またこのインタプリタには重要な構造が隠れています。順に見ていきましょう。

## Free モナドの課題

Free モナドは強力ですが、命令セット `f` に **Functor 制約が必要** です。

```haskell
data TalkF next
  = Ask String (String -> next)   -- ← この (String -> next) が Functor のために必要
  | Tell String next
```

`fmap` は「命令の内容を変えずに、`next`（続き先）だけを差し替える」操作です。`Tell msg next` のように `next` が直接フィールドにあれば差し替えは簡単ですが、`Ask` の場合、結果（`String`）は継続関数の返り値の中に閉じ込められています。`next` を差し替えるには、継続を引数として持ち `(String -> next)` という形にする必要があります。

この `(String -> next)` は Functor の `fmap` を実現するためだけに存在しています。構造化されたまま残っている継続の部分であり、定型的で、命令の本質的な情報を持っていません。この Functor 制約を取り除く方法は次章で扱います。

## ステップ実行の認識

Functor 制約の問題は次章に送るとして、ここでは `runTalkIO` をもう一度見てみましょう。このインタプリタは何をしているでしょうか？

```haskell
runTalkIO (Pure a) = pure a
runTalkIO (Free (Ask prompt next)) = do
  putStrLn prompt
  input <- getLine
  runTalkIO (next input)     -- 継続に値を渡して再開
runTalkIO (Free (Tell msg next)) = do
  putStrLn msg
  runTalkIO next             -- 次のステップへ
```

先頭をパターンマッチで覗き、命令を処理し、継続に結果を渡して次のステップへ進む。これは**ステップ実行**（プログラムを一命令ずつ処理するインタプリタのパターン）です。

このパターンが成立するのは、プログラムが Free モナドによって**データとして具象化（reify）**されているからです。`runTalkIO` は Free の構造をパターンマッチで覗き、命令の種類に応じて処理を分岐しています。

一般のモナド（たとえば `Reader` や `IO`）の `>>=` では、こうした「覗き見」はできません。`IO a >>= f` は内部で何が起きているかが不透明であり、外から命令を一つ取り出して処理を分岐する、という操作は原理的に不可能です。Free モナドが**データ**であるからこそ、中を覗けるのです。

前章で `bindCont`（「計算の結果を受け取って次の計算に渡す」合成操作）を「本資料全体を貫く中心的なアイデア」と呼びました。`runTalkIO` のステップ実行はその reification にほかなりません。命令を処理して得た値を継続（`next`）に渡し、次のステップへ進む。`bindCont` の構造が、データのパターンマッチとして目に見える形で動いています。

次章では Functor 制約を取り除いた Freer モナドを導出し、このステップ実行の構造をさらに整理します。

> 📖 対応コード: [`haskell/src/Free.hs`](../haskell/src/Free.hs)
