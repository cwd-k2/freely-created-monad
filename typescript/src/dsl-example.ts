// ============================================================
// Freer モナド DSL — TypeScript 実装
// ============================================================

// ------------------------------------------------------------
// 命令の定義（Defunctionalized な命令セット）
// ------------------------------------------------------------

type Ask = { readonly tag: "ask"; readonly prompt: string };
type Tell = { readonly tag: "tell"; readonly message: string };

// 命令の union type
type TalkInstruction = Ask | Tell;

// 命令の結果型を型レベルで対応付ける（Haskell の GADT に相当）
// eslint-disable-next-line @typescript-eslint/no-unused-vars
type ResultOf<I extends TalkInstruction> = I extends Ask ? string : void;

// ------------------------------------------------------------
// Freer の構造: 命令 + 限定継続 = Generator
// ------------------------------------------------------------

// Generator<Yield, Return, Next> で:
//   Yield  = 命令 (yield で外に渡す)
//   Return = プログラムの最終結果
//   Next   = yield の返り値（インタプリタから供給される値）

// 命令を yield するラッパー
// yield* で呼ぶことで型安全に命令の結果を受け取れる
function* ask(prompt: string): Generator<Ask, string, string> {
  return (yield { tag: "ask", prompt }) as string;
}

function* tell(message: string): Generator<Tell, void, void> {
  return (yield { tag: "tell", message }) as void;
}

// ------------------------------------------------------------
// プログラム型: Generator 関数 = 不変なプログラム表現
// ------------------------------------------------------------

// Generator は one-shot（一度イテレートすると使い切り）だが、
// Generator *関数* は呼ぶたびに新しい Generator を返す。
// この () => Generator こそが「不変なプログラム表現」であり、
// Haskell の純粋な Freer 値に対応するサンクである。
type Program<A> = () => Generator<TalkInstruction, A, any>;

// ------------------------------------------------------------
// プログラムの例
// ------------------------------------------------------------

const greetProgram: Program<string> = function* () {
  const name: string = yield* ask("名前を入力してください");
  yield* tell(`こんにちは、${name}さん！`);
  const age: string = yield* ask("年齢を入力してください");
  yield* tell(`${age}歳ですね。`);
  return name;
};

// ------------------------------------------------------------
// インタプリタ
// ------------------------------------------------------------

// IO インタプリタ（Node.js readline を使用）
async function runIO<A>(program: Program<A>): Promise<A> {
  const readline = await import("readline");
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  const question = (prompt: string): Promise<string> =>
    new Promise((resolve) => rl.question(prompt + " > ", resolve));

  const gen = program();
  let result = gen.next();

  while (!result.done) {
    const instruction = result.value;
    switch (instruction.tag) {
      case "ask": {
        const answer = await question(instruction.prompt);
        result = gen.next(answer);
        break;
      }
      case "tell": {
        console.log(instruction.message);
        result = gen.next(undefined);
        break;
      }
    }
  }

  rl.close();
  return result.value;
}

// 純粋インタプリタ（テスト用）
function runPure<A>(
  program: Program<A>,
  inputs: string[],
): { result: A; outputs: string[] } {
  const gen = program();
  const outputs: string[] = [];
  let inputIndex = 0;
  let step = gen.next();

  while (!step.done) {
    const instruction = step.value;
    switch (instruction.tag) {
      case "ask": {
        if (inputIndex >= inputs.length) {
          throw new Error("入力が足りません");
        }
        step = gen.next(inputs[inputIndex++]);
        break;
      }
      case "tell": {
        outputs.push(instruction.message);
        step = gen.next(undefined);
        break;
      }
    }
  }

  return { result: step.value, outputs };
}

// ログ収集インタプリタ
function runWithLog<A>(
  program: Program<A>,
  inputs: string[],
): { result: A; log: Array<{ instruction: string; detail: string }> } {
  const gen = program();
  const log: Array<{ instruction: string; detail: string }> = [];
  let inputIndex = 0;
  let step = gen.next();

  while (!step.done) {
    const instruction = step.value;
    switch (instruction.tag) {
      case "ask": {
        const answer = inputs[inputIndex++] ?? "";
        log.push({ instruction: "ask", detail: `"${instruction.prompt}" → "${answer}"` });
        step = gen.next(answer);
        break;
      }
      case "tell": {
        log.push({ instruction: "tell", detail: instruction.message });
        step = gen.next(undefined);
        break;
      }
    }
  }

  return { result: step.value, log };
}

// ------------------------------------------------------------
// 実行
// ------------------------------------------------------------

// 純粋インタプリタでテスト
const pureResult = runPure(greetProgram, ["太郎", "25"]);
console.log("=== 純粋インタプリタ ===");
console.log("結果:", pureResult.result);
pureResult.outputs.forEach((msg) => console.log("  出力:", msg));

// ログ収集インタプリタ
const logResult = runWithLog(greetProgram, ["太郎", "25"]);
console.log("\n=== ログ収集インタプリタ ===");
console.log("結果:", logResult.result);
logResult.log.forEach((entry) =>
  console.log(`  [${entry.instruction}] ${entry.detail}`),
);
