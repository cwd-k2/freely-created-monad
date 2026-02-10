# Freer と限定継続

前章で、Free モナドのインタプリタがステップ実行のパターンであることを認識しました。しかし Free モナドには Functor 制約という荷物が残っています。この章では、まず Functor 制約を取り除いた **Freer モナド**を導出します。

次に、Freer のステップ実行を整理すると、そこに**限定継続**（shift/reset）の構造が潜んでいることを見出します。限定継続は「値を外に渡して中断し、外側から値を供給されて再開する」というプロトコルであり、これはまさに Generator の `yield`/`next` そのものです。つまり、限定継続を認識することで、Haskell の Freer モナドと TypeScript の Generator が**同じ構造の別表現**であることが明らかになり、次章での TypeScript 実装への道が開けます。

## Free モナドの bind を観察する

前章末尾で指摘した Functor 制約の問題を解決するために、Free モナドの `>>=` がどう動くかを詳しく追ってみます。

```haskell
data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Monad (Free f) where
  return = Pure
  Pure a >>= f = f a
  Free fx >>= f = Free (fmap (>>= f) fx)
```

`>>=` の動きを具体的に追ってみます。

```
Free (Ask "名前" (\name -> Free (Tell name (Pure ()))))  >>= g

-- Free fx >>= g = Free (fmap (>>= g) fx) を適用:
= Free (Ask "名前" (\name -> Free (Tell name (Pure ())) >>= g))
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                              (>>= g) が継続の返り値に合成される

-- 内側の Free (Tell ...) >>= g にも同じ規則を適用:
= Free (Ask "名前" (\name -> Free (Tell name (Pure () >>= g))))

-- Pure a >>= g = g a:
= Free (Ask "名前" (\name -> Free (Tell name (g ()))))
```

`Free fx >>= f = Free (fmap (>>= f) fx)` を見ると、`>>=` は `fmap` を通じて**命令 `fx` の内部に潜り込み**、末尾の `Pure` を書き換えています。ここに2つの問題があります。

1. **Functor 制約**: `fmap` を使うため、命令セット `f` が Functor でなければならない
2. **性能**: `>>=` のたびに構造全体を走査するため、左結合チェーンで O(n²) になる

どちらの問題も、`fmap` で命令の内部に潜る構造に起因しています。もし命令 `fx` と継続を分離して保持し、`>>=` では継続を合成するだけにできれば：

```haskell
Bind fx k >>= f = Bind fx ((>>= f) . k)
-- 命令の内部に潜らず、継続を関数合成で延長するだけ
```

`fmap` が不要になり、Functor 制約が消えます。

> **性能について**: この構造では個々の `>>=` は O(1) ですが、左結合チェーン全体の O(n²) 問題は残ります（蓄積された継続の実行時に再び現れるため）。なお、do 記法は `>>=` を右結合にデシュガーするため、do 記法でプログラムを組み立てる限りこの問題は発生しません。左結合チェーンへの対処は補遺で **Codensity モナド** として扱います。

しかし、この直観だけでは `Free (f (Free f a))` がどうして `Bind :: f x -> (x -> Freer f a) -> Freer f a` という形になるのか——特に存在量化された型変数 `x` がどこから来るのか——が明らかではありません。これを厳密に導出するのが、次節の Coyoneda です。

## Coyoneda

前節の直観を厳密に導くために、**Coyoneda** という構成を導入します。

```haskell
data Coyoneda f a where
  Coyoneda :: (x -> a) -> f x -> Coyoneda f a
```

`Coyoneda f a` は「`f x` の値と、結果を `x` から `a` に変換する関数のペア」です。`fmap` を実際に `f` に適用するのではなく、変換関数を蓄積しておきます。

この構成の鍵は、**`f` が Functor でなくても `Coyoneda f` は Functor になる**ことです。

```haskell
instance Functor (Coyoneda f) where
  fmap g (Coyoneda h fx) = Coyoneda (g . h) fx
  -- g の適用を関数合成で蓄積するだけ。f の fmap は一切使わない。
```

`Coyoneda` は任意の型構成子 `f :: * -> *` から Functor を生み出す普遍的な構成であり、圏論における**余米田の補題（co-Yoneda lemma）**——米田の補題の双対——に由来する名前です。"Co-" 接頭辞がこの双対性を示しています。`f` が Functor である場合には `Coyoneda f a ≅ f a` が成り立ちますが、Freer の導出で使うのは同型性ではなく、**`Coyoneda f` が常に Functor になる**という構成的事実のほうです。

## Freer モナドの導出

この Coyoneda を使って、Freer モナドを厳密に導出します。

出発点は `Free (Coyoneda f) a` です。`Coyoneda f` は `f` が Functor でなくても Functor になるので、この型は任意の `f` に対して Free モナドとして成立します。

`Free` コンストラクタを展開してみましょう。

```
Free (Coyoneda f (Free (Coyoneda f) a))
= Free (exists x. (x -> Free (Coyoneda f) a, f x))
```

存在量化された `x` について、`f x`（命令）と `x -> Free (Coyoneda f) a`（継続）のペアを持つ——これはまさに前節で動機として描いた「命令と継続の分離」の構造です。

この展開結果をそのまま GADT として定義すれば、Freer モナドが得られます。

```haskell
data Freer f a where
  Pure :: a -> Freer f a
  Bind :: f x -> (x -> Freer f a) -> Freer f a
```

各構成子の意味：

- `Pure a` — 計算完了、値 `a` を持つ（Free の `Pure` と同じ）
- `Bind fx k` — 命令 `fx`（結果の型は `x`）を実行し、結果を継続 `k` に渡す（Coyoneda 分解で得られた構造）

**`f` に Functor 制約がない** ことに注目してください。Coyoneda が `fmap` の役割を吸収したため、`f` は任意の `* -> *` 型で構いません。命令 `f x` はデータとして保持され、継続 `k` は関数のまま残ります——命令セットの Defunctionalization（ch02）と、Functor 層の Coyoneda 分解が組み合わさった結果です。

## Monad インスタンス

```haskell
instance Monad (Freer f) where
  Pure a    >>= f = f a
  Bind fx k >>= f = Bind fx ((>>= f) . k)
```

`Bind fx k >>= f` は、継続 `k` の後ろに `f` を合成するだけです。`fmap` は一切使いません。

## 命令セットの簡素化

Functor 制約が不要になったことで、命令セットが格段にシンプルになります。

```haskell
-- Freer 版: GADT で結果の型を直接指定
data Talk a where
  Ask  :: String -> Talk String   -- 質問して文字列を得る
  Tell :: String -> Talk ()       -- メッセージを表示する
```

`(String -> next)` のような継続パラメータは完全に消えました。命令の**本質だけ**が残っています。この GADT は TypeScript の tagged union にそのまま対応します。

```typescript
type Ask = { readonly tag: "ask"; readonly prompt: string };   // 結果: string
type Tell = { readonly tag: "tell"; readonly message: string }; // 結果: void
type TalkInstruction = Ask | Tell;
```

Free 版の `TalkF` では Functor のために `(next: string) => next` のような継続フィールドが必要でしたが、Freer 版では命令データだけが残ります。TypeScript の union type で自然に表現できる形になったことは、次章で Generator による DSL を実装する際の土台になります。

```haskell
-- 使い方は Free 版と同じ
talkProgram :: Freer Talk String
talkProgram = do
  name <- send (Ask "名前を入力してください")
  send (Tell ("こんにちは、" ++ name ++ "さん！"))
  age <- send (Ask "年齢を入力してください")
  send (Tell (age ++ "歳ですね。"))
  pure name
```

`send` は命令を Freer に持ち上げるだけの関数です。

```haskell
send :: f a -> Freer f a
send fa = Bind fa Pure
```

---

ここまでで Freer モナドの定義を得ました。命令と継続が型レベルで分離され、Functor 制約が消えました。ここからは、この分離がインタプリタの構造——ステップ実行と限定継続——にどう反映されるかを見ていきます。

---

## ステップ実行の整理

前章で Free モナドのインタプリタがステップ実行のパターンであることを認識しました。Freer では命令と継続が分離されたことで、このパターンをより明確に表現できます。

インタプリタに**必要な情報だけを公開**する `Step` 型を導入します。

```haskell
data Step f a where
  Done  :: a -> Step f a                        -- 計算完了
  Await :: f x -> (x -> Freer f a) -> Step f a  -- 命令 + 継続

viewFreer :: Freer f a -> Step f a
viewFreer (Pure a)    = Done a
viewFreer (Bind fx k) = Await fx k
```

インタプリタは `Freer` のコンストラクタに依存せず、「計算完了（`Done`）か、命令待ち（`Await`）か」だけを見ます。現在の実装では `viewFreer` は単純なコンストラクタの読み替えですが、内部表現を変更してもインタプリタのコードが変わらないという利点があります（Codensity 最適化を内部化した実装ではここが非自明な処理になります）。

```haskell
runStepIO :: Freer Talk a -> IO a
runStepIO m = case viewFreer m of
  Done a -> pure a
  Await (Ask prompt) k -> do
    putStrLn prompt
    input <- getLine
    runStepIO (k input)       -- 継続に値を供給して再開
  Await (Tell msg) k -> do
    putStrLn msg
    runStepIO (k ())          -- 継続に値を供給して再開
```

前章の Free 版インタプリタと比較してみましょう。

```
Free 版:   runTalkIO (Free (Ask prompt next)) = ... runTalkIO (next input)
Freer 版:  Await (Ask prompt) k              -> ... runStepIO (k input)
```

構造は同じ——**先頭を覗き、命令を処理し、継続に値を渡す**——ですが、Freer 版では命令 `Ask prompt` と継続 `k` が型レベルで分離されています。この分離が、次節の限定継続という見方を自然にします。

この `Step` は TypeScript の `IteratorResult` にそのまま対応します。

```typescript
const step: IteratorResult<TalkInstruction, string> = gen.next();

// step.done === true  → Done a     （計算完了、step.value が結果）
// step.done === false → Await fx k （命令待ち、step.value が命令）
//   gen.next(value) で継続 k に値を供給して再開
```

Haskell の `viewFreer` でパターンマッチする操作が、TypeScript では `gen.next()` の返り値の `.done` を見る操作に対応しています。

## 限定継続

### 継続と限定継続の違い

通常の継続（continuation）は**プログラム全体の残り**を表します。

```
1 + (2 * □)    -- □ の継続は「2を掛けて1を足す」= プログラム全体の残り
```

**限定継続（delimited continuation）** は**特定の境界までの残り**を表します。

```
reset (1 + (2 * shift k => ...))
       ^^^^^^^^^^^^^^^^^
       k が捕捉するのはここだけ（reset までの範囲）
```

限定継続の枠組みでは、2つの演算子が基本になります。

- **`reset`**（デリミタ）— 継続の捕捉範囲の境界を設定する
- **`shift`**（捕捉）— 現在の位置から最も近い `reset` までの継続を捕捉する

### Freer における限定継続

前節のステップ実行は、この shift/reset の構造に概念的に対応します（形式的な同型ではなく、構造の類似性に基づくアナロジーです）。同様の構造は**代数的エフェクト**（algebraic effects）の perform/handler にも見られ、Freer モナドは代数的エフェクトの Haskell における実現と見なすこともできます。

```haskell
data Freer f a where
  Pure :: a -> Freer f a
  Bind :: f x -> (x -> Freer f a) -> Freer f a
```

対応関係：

| 限定継続 | Freer |
|---|---|
| `shift` | `send` — 命令を発行し、残りの計算を継続として捕捉 |
| `reset` | インタプリタのループ — 継続の捕捉範囲を区切る |
| 捕捉された継続 `k` | `Bind` の第2引数 `(x -> Freer f a)` |
| 継続に値を供給 | `k value` — パターンマッチで取り出した `k` に値を渡す |

`send` の定義を思い出してみましょう。

```haskell
send :: f a -> Freer f a
send fa = Bind fa Pure
--        ^^^^ ^^  ^^^^
--        命令  結果  「結果をそのまま返す」継続
```

`send fa` は「命令 `fa` を発行し、その結果をそのまま返す」という最小のプログラムです。これが `do` 記法の `>>=` で他の計算と結合されると：

```haskell
send (Ask "名前は？") >>= \name -> send (Tell ("こんにちは、" ++ name))

-- >>= の定義により:
= Bind (Ask "名前は？") (\name -> send (Tell ("こんにちは、" ++ name)))
```

`Bind` の第2引数 `(\name -> ...)` が限定継続です。「`Ask` の結果 `name` を受け取って、残りの計算全体を続ける」関数であり、インタプリタ（= `reset`）の境界まで——すなわちプログラムの末尾の `Pure` まで——を捕捉しています。インタプリタが `k` を呼ぶと次の `Bind`（次の命令）が返ってくるため、一命令ずつステップ実行できますが、`k` 自体が保持しているのは「次の命令まで」ではなく残りの計算全体です。

前節のインタプリタを改めて読むと、`case viewFreer m of` が `reset`（境界の設定）、`Await` のパターンマッチが `shift` で中断された地点の処理、`k input` が限定継続への値の供給です。

## Generator への橋渡し

次章では、限定継続を**言語レベルで直接サポート**する仕組み——Generator の `yield`/`next`——を使って、TypeScript で DSL を実装します。

Generator の `yield` は shift に対応します。両者の動作を並べてみましょう。

- **shift（= `send`）**: 命令を発行し、自分自身の残りの計算を継続として捕捉する。制御は最も近い `reset`（インタプリタ）に移る
- **`yield`**: 値（命令）を外に渡して中断する。制御は呼び出し元（インタプリタの `while` ループ）に移る。再開時に `gen.next(value)` で値を受け取る

どちらも「値を外に渡して中断し、外側から値を供給されて再開する」——同じプロトコルです。インタプリタの `while` ループが `reset`（継続の捕捉範囲の境界）に対応します。

> 📖 対応コード: [`haskell/src/Freer.hs`](../haskell/src/Freer.hs)
