# Generator と DSL

「値を外に渡して中断し、外側から値を供給されて再開する」——この限定継続のプロトコルを、言語レベルで直接サポートしているのが TypeScript の Generator です。この章では Generator で DSL を実装し、Freer モナドとの対応を確認します。

## Generator = 限定継続の言語サポート

TypeScript（JavaScript）の Generator は、限定継続を**言語レベルでサポート**する仕組みです。

```typescript
function* myGen(): Generator<string, number, boolean> {
  const flag: boolean = yield "命令1";  // yield で中断、boolean を受け取って再開
  const flag2: boolean = yield "命令2";
  return 42;
}
```

Freer との対応：

| Freer (Haskell) | Generator (TypeScript) |
|---|---|
| `Bind fx k` | `yield instruction` |
| `fx` — 命令 | `instruction` — yield する値 |
| `k` — 限定継続 | yield 後の残りの関数本体 |
| パターンマッチで命令を処理 | `gen.next(value)` で値を供給 |
| `send` = shift | `yield` = shift |
| インタプリタのループ = reset | `while` ループ = reset |

ただし重要な違いがあります。Haskell の Freer における継続 `k :: x -> Freer f a` は**通常の関数**なので、同じ `k` に異なる値を何度でも渡せます（multi-shot）。一方、Generator の継続は内部状態を持ち、**一度しか再開できません**（one-shot）。このため、非決定性（バックトラック）のような同じ継続を複数回呼ぶインタプリタは Generator では直接表現できません。本資料の DSL（対話プログラム）では継続を一度しか使わないため、この制約は問題になりません。

## TypeScript での DSL 実装

### 命令の定義

```typescript
type Ask = { readonly tag: "ask"; readonly prompt: string };
type Tell = { readonly tag: "tell"; readonly message: string };

type TalkInstruction = Ask | Tell;
```

Defunctionalization された命令セットそのものです。Haskell の GADT に相当します。

### 命令の yield ラッパー

```typescript
function* ask(prompt: string): Generator<Ask, string, string> {
  return (yield { tag: "ask", prompt }) as string;
}

function* tell(message: string): Generator<Tell, void, void> {
  return (yield { tag: "tell", message }) as void;
}
```

`ask` や `tell` は `yield` で命令を外に渡し、自身は中断します。`as string` は TypeScript の Generator 型が `yield` の返り値型を単一の `TNext` パラメータで管理するための制約に対する型アサーションで、インタプリタが正しい型の値を供給する前提のもとで安全です。プログラム側ではこれらを `yield*` で呼びます。`yield*` は**サブジェネレータへの委譲**——呼び出し先の Generator が `yield` した値をそのまま外側に伝播し、外側から供給された値を呼び出し先に戻します。つまり `yield*` が Haskell の do 記法（`>>=` による合成）に対応するモナド合成の仕組みです。

### プログラム型: `() => Generator` の意味

プログラムの型を定義します。

```typescript
type Program<A> = () => Generator<TalkInstruction, A, any>;
```

この `() => Generator<...>` という型が本質的です。Generator は one-shot——一度イテレートすると内部状態が進み、巻き戻せません。つまり `Generator` そのものは**可変で使い捨て**のオブジェクトです。

しかし Generator **関数**（`function*`）は、呼ぶたびに新しい Generator を返します。関数自体は何度呼んでも同じ振る舞いをし、状態を持ちません。つまり `() => Generator<...>` は**不変なプログラム表現**です。

| | Haskell (Freer) | TypeScript (Generator) |
|---|---|---|
| プログラム | 純粋なデータ（自由に共有可能） | `() => Generator`（呼ぶたびに新規生成） |
| 不変性の担保 | 言語レベル（純粋性） | サンク化（関数で包むこと） |

`function*` のリテラルがそのまま `() => Generator` 型を持つため、特別なラッパー関数は不要です。

### プログラムの記述

```typescript
const greetProgram: Program<string> = function* () {
  const name = yield* ask("名前を入力してください");
  yield* tell(`こんにちは、${name}さん！`);
  const age = yield* ask("年齢を入力してください");
  yield* tell(`${age}歳ですね。`);
  return name;
};
```

Haskell の do 記法と同じ見た目で DSL が書けます。`greetProgram` はまだ実行されていません——Generator 関数として保持されているだけです。インタプリタが `greetProgram()` を呼んで初めて Generator が生成され、実行が始まります。そして別のインタプリタがもう一度 `greetProgram()` を呼べば、また新しい Generator で最初から実行できます。

### インタプリタの実装

同じプログラムに対して、異なるインタプリタを差し替えられます。

**純粋インタプリタ（テスト用）:**

```typescript
function runPure<A>(program: Program<A>, inputs: string[]): { result: A; outputs: string[] } {
  const gen = program();
  const outputs: string[] = [];
  let inputIndex = 0;
  let step = gen.next();

  while (!step.done) {
    switch (step.value.tag) {
      case "ask":
        step = gen.next(inputs[inputIndex++]);  // 限定継続に値を供給
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

インタプリタのループは Freer のステップ実行と同じ構造です。

- `step.done === false` → `Await fx k`（命令がある）
- `step.done === true` → `Done a`（計算完了）
- `gen.next(value)` → `k value`（限定継続に値を供給）

**IO インタプリタ、ログ収集インタプリタ** なども同じプログラムに対して適用できます。プログラムの定義と実行が完全に分離されています。

## まとめ

| 概念 | Haskell | TypeScript |
|---|---|---|
| 命令セット | GADT | Tagged union |
| 継続 | `(x -> Freer f a)` | Generator の残り |
| 継続の性質 | multi-shot（通常の関数） | one-shot（内部状態を持つ） |
| モナド合成 | `>>=` / do 記法 | `yield*` / Generator |
| プログラムの再利用 | 純粋なデータ（自由に共有可能） | サンク（呼び出すたびに新規生成） |
| インタプリタ | パターンマッチ | switch + `gen.next()` |

継続の性質（multi-shot vs one-shot）が最も大きな違いです。Haskell の `k` は通常の関数なので同じ値を何度でも渡せますが、Generator の継続は内部状態を持ち一度しか再開できません。本資料の DSL のように継続を一度しか使わないユースケースでは、この違いは問題になりません。

> 📖 対応コード: [`typescript/src/dsl-example.ts`](../typescript/src/dsl-example.ts)
