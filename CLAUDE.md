# freely-created-monad

社内勉強会資料。自由モナドから Freer モナドへの導出を段階的に解説する。

## 4つの視点

1. DSL のプログラムは個々の命令の列として**具象化 (reify)** できる
2. 具象化されたプログラムは**ステップ実行**に分解できる
3. ステップ実行を Freer で整理すると**限定継続**が浮かび上がる
4. 限定継続は **Generator** の `yield`/`next` に対応する

## ディレクトリ構成

```
docs/           Markdown ドキュメント（01-06）
haskell/        各章に対応する Haskell コード（cabal プロジェクト）
typescript/     DSL の TypeScript 実装（Generator ベース）
slides/         Marp スライド（slides.md → slides.html）
```

## 章構成

| ch | ドキュメント | タイトル | Haskell モジュール |
|---|---|---|---|
| 01 | `docs/01-introduction.md` | はじめに | — |
| 02 | `docs/02-reification.md` | 計算の具象化 | Continuation.hs, Defunctionalization.hs |
| 03 | `docs/03-free-monad.md` | 自由モナドとステップ実行 | Free.hs |
| 04 | `docs/04-freer-monad.md` | Freer と限定継続 | Freer.hs |
| 05 | `docs/05-dsl.md` | Generator と DSL | — (TypeScript: `src/dsl-example.ts`) |
| 06 | `docs/06-codensity.md` | 補遺: Codensity モナド | Codensity.hs |

## ビルド

```bash
# Haskell
cd haskell && cabal build && cabal run freely-created-monad

# TypeScript
cd typescript && npm install && npx tsc --noEmit

# スライド
npx @marp-team/marp-cli slides/slides.md -o slides/slides.html --html
```

## 設計方針

- Freer を天下りで与えず、継続 → Defunctionalization → Free → Freer と自然に導出する
- Haskell コードは `-Wall` でクリーンビルド、TypeScript は `strict: true`
- cabal は `GHC2021`、GADT と RankNTypes を明示的に有効化
- 各 Haskell モジュールのデモ用エントリポイントは `example`（Main から qualified で呼ぶ）
- Codensity.hs は `import Freer` して Freer の型を再利用（再定義しない）
- Step / viewFreer / runStepIO / interpretPure は Freer.hs に配置

## ドキュメントの論理構造

- ch03 冒頭: モナドの必要性（依存的逐次実行）を明示的に論じる節がある
- ch03: ContChain → Free は厳密な導出ではなく、DSL ドメインの仮定に基づく設計選択
- ch03: `(a -> b)` の分解は2段階 —「命令セットを型で表す」→「なぜ next を差し替える必要があるか — Functor」
- ch03「ステップ実行の認識」節: runTalkIO がステップ実行パターンであることを明示（軸 1-2 の核心）
- ch04 の Free → Freer 導出は **Coyoneda** が正式な導出経路
- ch04: ステップ実行の整理（Step / viewFreer）→ 限定継続 → Generator への橋渡し
- ch06 は補遺として Codensity のみ扱う
