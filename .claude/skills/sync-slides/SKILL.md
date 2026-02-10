---
name: sync-slides
description: ドキュメント変更をスライドに反映し、Marp でレンダリング確認する。
allowed-tools: Read, Grep, Glob, Edit, Write, Bash(npx @marp-team/marp-cli *)
---

# スライド同期

`docs/` の変更内容を `slides/slides.md` に反映し、レンダリングを確認する。

## 手順

1. `git diff docs/` で最近のドキュメント変更を確認する（未コミットの変更がない場合は直近のコミットを参照）
2. 変更内容に対応するスライドの箇所を `slides/slides.md` から特定する
3. スライドを修正する
4. `npx @marp-team/marp-cli slides/slides.md --images png --image-scale 1 -o /tmp/marp-check/` でレンダリングし、はみ出しがないか画像で確認する
5. はみ出しがある場合はスライド分割または `dense` クラスの適用で対処する
6. `npx @marp-team/marp-cli slides/slides.md -o slides/slides.html --html` で最終 HTML をビルドする

## 注意事項

- スライドはドキュメントの要約であり、全文を転記しない
- コード例はスライドに収まるよう簡略化してよい
- `dense` クラスは情報量の多いスライドに適用する（フォントサイズが小さくなる）
