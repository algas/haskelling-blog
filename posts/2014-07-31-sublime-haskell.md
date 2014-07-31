---
title: Setup Sublime Text as Haskell IDE
tags: haskell, sublime
---

## Sublime Text を IDE として GHC 7.8 のコードを書く

Sublime Text というテキストエディタを使って GHC 7.8 系のコードを書くための手順をまとめました。  
cabal sandbox を使って開発ができます。

### Setup Haskell

ghc 7.8.3 (執筆時点での最新) と cabal-install (1.20.0.3) をインストールします。  
執筆時点では対応している Haskell-Platform の安定版はリリースされていません。  
拙作のインストーラ haskell-playbook (ansible) などを使ってインストールしましょう。  
[https://github.com/algas/haskell-playbook](https://github.com/algas/haskell-playbook)  
もしこの記事を読んでいる時点で Haskell-Platform (2014.2?) が出ていればそちらを使ってください。

### Setup IDE

Sublime Text (以下、ST) と関連プラグイン(SublimeHaskell, SublimeREPL)をセットアップします。

#### Install Sublime Text

ST3 とパッケージ管理ツール(Package Control)をインストールします。

1. ST3 をインストールする  
(ST2 の場合にはファイルパスなどが異なる場合があります)
2. ST3 に Package Control をインストールする

#### Install SublimeHaskell Plugin

SublimeHaskell をインストールします。  
[https://github.com/SublimeHaskell/SublimeHaskell](https://github.com/SublimeHaskell/SublimeHaskell)  
名前の補完やファイル保存時の自動ビルドができるようになります。

1. cabal パッケージリストの更新と cabal の更新  
cabal update && cabal install cabal-install
2. 基本依存ライブラリのインストール  
cabal install aeson haskell-src-exts haddock
もし haddock のインストールに失敗したら以下を試そう
cabal install haddock --constraint=haddock==2.13.2.1
3. 追加依存ライブラリのインストール(オプション)  
cabal install ghc-mod stylish-haskell haskell-docs hdevtools
(cabal sandbox を使うので cabal-dev は使いません)
4. SublimeHaskell のインストール  
5. ST3 の再起動  

#### Install SublimeREPL Plugin

SublimeREPL をインストールします。  
[https://github.com/wuub/SublimeREPL](https://github.com/wuub/SublimeREPL)  
SublimeText 上で repl (インタラクティブシェル) が使えるようになります。

1. SublimeREPL のインストール  
2. パッケージ内の以下のファイルを変更する  
Linux: $HOME/.config/sublime-text-3/Packages/SublimeREPL/config/Haskell/Main.sublime-menu  
Mac: $HOME/Library/Application Support/Sublime Text 3/Packages/SublimeREPL/config/Haskell/Main.sublime-menu  
    * before:
<pre>
"cmd": ["ghci"],
</pre>
    * after:
<pre>
"cmd": ["cabal", "repl"],
</pre>
3. ST3 の再起動

### Development

以下の proj にはプロジェクトの名前を適切につけてください。

#### Setup Your Cabal

Cabal (Haskell のパッケージ) を設定します。
ターミナルから以下を実行します。

1. ディレクトリの作成  
mkdir proj && cd proj
2. パッケージリストの更新  
cabal update
3. sandbox の作成  
cabal sandbox init && cabal update
4. Cabal の初期設定  
cabal init
5. cabal ファイルの編集  
Edit proj.cabal  
main-is のコメントを外す(コメントイン)
6. main ファイルの作成  
Edit main.hs
<pre>
module Main where
main :: IO ()
main = print "Hello world!"
</pre>
7. 実行  
cabal run

#### Setup Your Sublime Project

Sublime Text 上で以下の設定を行います。

1. File > "Open Folder..." > proj
2. Project > "Save Project As..." > proj.sublime-project

#### Usage SublimeREPL

SublimeREPL の使い方というか起動方法は以下の通りです。

1. Tools > SublimeREPL > Haskell
2. 開いたウィンドウの中でコマンドを実行できます。  
<pre>
<interactive> :main
</pre>

repl (ghci) の使い方は他の記事や書籍などを参考にしてください。

#### Work Flow

私は以下のような流れで開発を進めています。

1. コードを書く
2. 依存関係のエラーが出る
3. .cabal に依存ライブラリを追加する
4. cabal install --only-dependencies
5. エラー治った
6. repl で動作を確認しながら開発を進める
7. ひと通りできたら cabal run で main の動作を確認する
8. 最初に戻る

#### Commands

* 依存ライブラリのインストール  
cabal install --only-dependencies
* ビルド  
cabal build
* インタープリタ  
cabal run
* インタラクティブシェル  
cabal repl

### Tips

* Tab to spaces  
タブをスペースに変換する設定をします。
設定する場所は影響範囲に応じて以下の2箇所から選択できます。  
    * Haskell のみ  
(Haskell ファイルを開いている状態で)  
Preference > "Settings - More" > "Syntax Specific - User"  
    * Sublime Text 全体  
Preference > "Settings - User"  
<pre>
{
    "draw_white_space": "all", // スペース記号を表示する
    "tab_size": 4, // 1タブで4スペースになる
    "translate_tabs_to_spaces": true, // タブをスペースに変更する
}
</pre>
* 画面分割  
REPL の画面をソースコードと同時に表示できるように画面分割すると作業が捗ります。  
View > Layout > Rows: 2  
横長モニタでは Rows: 2 の代わりに Columns: 2 もオススメです。

### ToDo

* SublimeREPL の修正を下位互換性を保ちつつフィードバックしたい  
Haskell 対応した修正について本体に取り込んでもらえるように設定ファイルに分離できればいいな、と。
