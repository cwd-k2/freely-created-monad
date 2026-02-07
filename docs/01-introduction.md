# はじめに

## この勉強会のゴール

最終的に、TypeScript で **DSL（ドメイン特化言語）** を書けるようになることを目指します。

```typescript
// 最終形のイメージ
const program = Do(function* () {
  const name = yield* ask("名前を入力してください");
  yield* tell(`こんにちは、${name}さん！`);
  const age = yield* ask("年齢を入力してください");
  yield* tell(`${age}歳ですね。`);
  return name;
});
```

この `program` はまだ**実行されていません**。プログラムの「構造」がデータとして記述されているだけです。実行方法は後から自由に差し替えられます——コンソール対話、テスト用のモック、ログ収集など。

この柔軟性を支えている仕組みが **Freer モナド** であり、そこに至る道筋を一歩ずつ辿るのがこの勉強会のテーマです。

## 道筋

Freer モナドを「天下り」で与えるのではなく、次のステップで自然に導出します。

1. **継続（Continuation）** — プログラムの「残りの計算」を明示化する
2. **Defunctionalization** — 関数値をデータに変換する
3. **自由モナド（Free Monad）** — 命令を Defunctionalization し、継続を構造化してモナドにする
4. **Freer モナド** — Functor 制約を取り除き、より自由に
5. **実行基盤** — ステップ実行、限定継続、Codensity モナド
6. **DSL** — TypeScript で実際に書いてみる

Haskell で理論を確認し、最後に TypeScript で実用的な実装に落とし込みます。
