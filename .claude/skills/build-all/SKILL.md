---
name: build-all
description: Haskell と TypeScript を一括ビルドし、整合性を確認する。
allowed-tools: Bash(cabal build *), Bash(cabal run *), Bash(cd *), Bash(npx tsc *), Bash(npm install *)
---

# 一括ビルドチェック

Haskell と TypeScript の両方をビルドし、エラーがないことを確認する。

## 手順

1. `cd haskell && cabal build` を実行
2. `cd haskell && cabal run freely-created-monad` を実行して各モジュールの `example` が動くことを確認
3. `cd typescript && npm install && npx tsc --noEmit` を実行
4. 結果をまとめて報告する

## 報告形式

```
## ビルド結果

| ターゲット | 状態 | 備考 |
|-----------|------|------|
| Haskell (cabal build) | OK / NG | [エラーがあれば要約] |
| Haskell (cabal run)   | OK / NG | [出力の要約] |
| TypeScript (tsc)      | OK / NG | [エラーがあれば要約] |
```

エラーがある場合は該当箇所を引用し、修正案を提示する。
