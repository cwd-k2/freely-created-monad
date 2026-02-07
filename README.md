# 自由に作られたモナド — 自由モナドから Freer へ

社内向け勉強会の資料です。Freer モナドを「天下り」で与えるのではなく、継続・Defunctionalization・自由モナドというステップを経て自然に導出します。

## 目次

1. **[はじめに](docs/01-introduction.md)** — ゴールの提示
2. **[プログラムは継続で書ける](docs/02-continuations.md)** — CPS 変換の基本
3. **[Defunctionalization](docs/03-defunctionalization.md)** — 高階関数をデータにする
4. **[自由モナド](docs/04-free-monad.md)** — Defunctionalization された継続からモナドへ
5. **[Free から Freer へ](docs/05-free-to-freer.md)** — Functor 制約の除去と米田の補題
6. **[Freer の実行基盤](docs/06-freer-execution.md)** — Codensity モナドと限定継続
7. **[DSL を書こう！](docs/07-dsl.md)** — TypeScript での実装

## コード

### Haskell

```sh
cd haskell && cabal build
cabal run freely-created-monad
```

| モジュール | 対応セクション |
|---|---|
| `Continuations` | 02: 継続 |
| `Defunctionalization` | 03: Defunctionalization |
| `FreeMonad` | 04: 自由モナド |
| `Freer` | 05: Freer モナド |
| `Codensity` | 06: 実行基盤 |

### TypeScript

```sh
cd typescript && npm install
npx tsc --noEmit        # 型チェック
npx tsx src/dsl-example.ts  # 実行
```
