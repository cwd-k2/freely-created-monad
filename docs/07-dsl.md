# DSL を書こう！

## モナドであることの担保

ここまでの章で見てきた通り、Freer モナドは以下を保証します。

1. **プログラムはデータである** — 命令の列がデータ構造として表現される
2. **合成可能である** — `>>=`（bind）によってプログラムを自由に連結できる
3. **実行は後から決められる** — インタプリタを差し替えることで同じプログラムに異なる意味を与えられる

これは「モナドである」ことの実用的な意味です。モナド則が満たされるからこそ、do 記法や `>>=` の連鎖が直観通りに動きます。

## Generator = 限定継続の言語サポート

TypeScript（JavaScript）の Generator は、`yield` で計算を中断し、外側から値を供給して再開できます。

```typescript
function* myGen(): Generator<string, number, boolean> {
  const flag: boolean = yield "命令1";  // yield で中断、boolean を受け取って再開
  const flag2: boolean = yield "命令2";
  return 42;
}
```

これは Freer の構造に対応します。

| Freer (Haskell) | Generator (TypeScript) |
|---|---|
| `Bind fx k` | `yield instruction` |
| `fx` — 命令 | `instruction` — yield する値 |
| `k` — 限定継続 | yield 後の残りの関数本体 |
| パターンマッチで命令を処理 | `gen.next(value)` で値を供給 |

ただし重要な違いがあります。Haskell の Freer における継続 `k :: x -> Freer f a` は**通常の関数**なので、同じ `k` に異なる値を何度でも渡せます（multi-shot）。一方、Generator の継続は内部状態を持ち、**一度しか再開できません**（one-shot）。このため、非決定性（バックトラック）のような同じ継続を複数回呼ぶインタプリタは Generator では直接表現できません。本資料の DSL（対話プログラム）では継続を一度しか使わないため、この制約は問題になりません。

この one-shot 性は、前章で触れた Codensity とも関係しています。Codensity は継続を関数として自由に合成・再結合できることを前提とした最適化ですが、one-shot な Generator ではそもそもこの再結合が成立しません。前章で Codensity を先に扱ったのは、multi-shot な継続が可能にする最適化を理解した上で、Generator がその一部を引き換えに言語レベルの `yield`/`next` という簡潔さを得ている、というトレードオフを明確にするためです。

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

> **`as string` について**: TypeScript の Generator 型は `yield` の返り値型を単一の `TNext` パラメータで管理するため、`yield*` で合成する際に型情報が `any` に落ちることがあります。`as string` はインタプリタが正しい型の値を供給する前提での型アサーションです。

`yield*` で呼ぶことで、命令を発行して結果を受け取れます。

### プログラムの記述

```typescript
const greetProgram = Do(function* () {
  const name = yield* ask("名前を入力してください");
  yield* tell(`こんにちは、${name}さん！`);
  const age = yield* ask("年齢を入力してください");
  yield* tell(`${age}歳ですね。`);
  return name;
});
```

Haskell の do 記法と同じ見た目で DSL が書けます。`Do` は Generator 関数をそのまま `Program` 型（= サンク）として返す関数です。`function*` を直接代入しても同じですが、`Do` で包むことで DSL のプログラム定義であることを明示しています。

`Program<A>` がサンク（`() => Generator<...>`）であることは本質的です。Generator は one-shot なので、一度イテレートすると使い切りになります。サンクにしておくことで、呼び出すたびに新しい Generator を生成でき、**同じプログラムを複数のインタプリタで実行**できます。Haskell の Freer 値は純粋なデータなので再利用に問題はありませんが、Generator ではサンク化がそれに代わる役割を果たしています。

`greetProgram` はまだ実行されていません——関数（サンク）として保持されています。

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
| 継続 | `(x -> Freer f a)`（multi-shot） | Generator の残り（one-shot） |
| モナド合成 | `>>=` / do 記法 | `yield*` / Generator |
| プログラムの再利用 | 純粋なデータ（自由に共有可能） | サンク（呼び出すたびに新規生成） |
| インタプリタ | パターンマッチ | switch + `gen.next()` |

Freer モナドの本質——**プログラムをデータとして記述し、実行を後から決める**——は、言語が変わっても同じです。Generator による実装は one-shot 制約などの違いがありますが、「命令の発行 → 中断 → インタプリタが値を供給して再開」というプロトコルは共通しています。

> 📖 対応コード: [`typescript/src/dsl-example.ts`](../typescript/src/dsl-example.ts)
