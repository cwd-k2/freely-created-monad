# Generator で DSL — 自由に手に入るモナド

社内向け勉強会の資料です。Freer モナドを「天下り」で与えるのではなく、継続・Defunctionalization・自由モナドというステップを経て自然に導出し、最終的に TypeScript の Generator で DSL を実装するところまでを扱います。

## 4 つのリフレーミング軸

1. DSL のプログラムは離散的な命令の列として**具象化**できる
2. 具象化されたプログラムは**ステップ実行**に分解できる
3. ステップ実行を Freer で整理すると**限定継続**が浮かび上がる
4. 限定継続は **Generator** の `yield`/`next` に対応する

## 目次

1. **[はじめに](docs/01-introduction.md)** — ゴールの提示
2. **[計算の具象化](docs/02-reification.md)** — CPS と Defunctionalization
3. **[自由モナドとステップ実行](docs/03-free-monad.md)** — 具象化からモナドへ
4. **[Freer と限定継続](docs/04-freer-monad.md)** — Coyoneda による Functor 制約の除去
5. **[Generator と DSL](docs/05-dsl.md)** — TypeScript での実装
6. **[補遺: Codensity モナド](docs/06-codensity.md)** — 左結合ボトルネックの解消

## コード

### Haskell

```sh
cd haskell && cabal build
cabal run freely-created-monad
```

| モジュール | 対応章 |
|---|---|
| `Continuation` | 02: 計算の具象化 |
| `Defunctionalization` | 02: 計算の具象化 |
| `Free` | 03: 自由モナドとステップ実行 |
| `Freer` | 04: Freer と限定継続 |
| `Codensity` | 06: 補遺: Codensity モナド |

### TypeScript

```sh
cd typescript && npm install
npx tsc --noEmit        # 型チェック
npx tsx src/dsl-example.ts  # 実行
```
