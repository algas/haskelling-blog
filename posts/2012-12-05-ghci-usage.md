---
title: ghci を使う
tags: haskell, ghci
---

## ghci を使う

ここでは ghci の2種類の使い方について説明します。私が使い方を知らなくて困ったことのみを中心に書き、細かいことは他の書籍やWebサイトに任せます。

### hs ファイルを呼び出す

Hoge.hs というファイル名で以下の2行を保存しましょう。
<pre>
add :: Int -> Int -> Int
add x y = x + y
</pre>
ghci からこのファイルを呼び出すことができます。
<pre>
$ ghci Hoge.hs 
> add 1 3
4
</pre>
もしファイルに変更があったり他のファイルを呼び出したいときには ghci プロンプトで以下のコマンドを実行します。
<pre>
> :load Hoge.hs
</pre>
ghci を終了するには
<pre>
> :quit
</pre>
です。


### 対話的にコードを書く

以下のコマンドで ghci プロンプトを立ち上げます。
<pre>
$ ghci
Prelude>　1 + 2
3
</pre>
「結果を表示する」命令を書いていないのにどうして結果が表示されたのでしょうか？ 実は ghci のプロンプトには以下のルールがあります。  
「戻り値が Show のインスタンスで、かつ () ではないときに結果を表示する」  
プロンプトで評価するのは IO a という型を持つ何かと決められています。
次は関数を定義してみましょう。
<pre>
Prelude> add x y = x + y
<interactive> :8:7: parse error on input `='
</pre>
エラーが出てしまいました。
ghci プロンプトで関数を定義したい場合は以下のようにします。
<pre>
Prelude> let add x y = x + y
Prelude> add 2 3
5
</pre>
ここで書いた以外にも便利なコマンドや機能がたくさんあります。  
[http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html](http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html)
