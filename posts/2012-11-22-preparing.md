---
title: Haskell を動かす
tags: haskell, ghc
---

## Haskell を動かす

「Haskell のプログラムを書いてみたんだけど、どうやったら動かせるの？」に答えます。

### インストール

[Haskell Platform](http://www.haskell.org/platform/) をインストールしましょう。

インストールの詳細については公式ドキュメントや他の記事に任せます。

### 実行

Haskell のコンパイラはいくつもありますが、もっとも多くの人が使っている ghc の場合を例に3種類の方法を紹介します。

1. ghci (interactive shell)

1行毎にコンパイルして動作確認できます。hs ファイルやライブラリのモジュールを呼び出すこともできます。

<pre>
$ ghci
<code>&gt putStrLn "Hello World"</code>
<code>Hello World</code>
</pre>

2. runhaskell (interpreter)

hs ファイルをコンパイルして main 関数を実行します。main が適切に書かれていないとエラーが出ます。

<pre>
$ vi hoge.hs
<code>main = print "Hello World"</code>
$ runhaskell hoge.hs
<code>Hello World</code>
</pre>
 

3. ghc (compiler)

hs ファイルをコンパイルして実行ファイルまたはモジュールファイルを生成します。

<pre>
$ ghc -o hoge hoge.hs
$ ./hoge
<code>Hello World</code>
</pre>

同じコードであれば、後者の方がより高速に実行できます。

私の場合、以下のように使い分けています。開発中は主に ghci で小さな関数単位で型チェックや動作確認を行い、runhaskell でプログラム全体の動作を確認します。ghc コマンドでビルドするのは本番系で運用する時、もしくは速度が必要とされる計算などを行うときだけです。
