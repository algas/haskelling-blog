---
title: Install GHC and cabal-install in Ubuntu 13.04
tags: haskell, ghc, cabal, ubuntu
---

## Haskell の実行環境の構築 in Ubuntu 13.04 (raring)

Ubuntu 13.04 (raring) で Haskell (GHC) の実行環境を構築してみました。
多くの環境においては haskell-platform をインストールするのが、haskell の実行環境を作るのに最も簡単な方法です。
しかし、raring には haskell-platform パッケージが本記事の執筆時点(2013/05/06)にはリリースされていません。

### OSパッケージを使う

<pre>
sudo apt-get update && sudo apt-get install ghc cabal-install
</pre>
これで ghc, cabal コマンドを使えるようになります

### OSパッケージを使わない

OSパッケージを使わずに Haskell (GHC) の実行環境を構築してみます。

手順は大きくわけて以下の3つ。
1. 依存パッケージのインストール
2. ghc のインストール
3. cabal-install のインストール 

各手順の詳細を説明します。

1. 依存パッケージのインストール
ghc と cabal-install は "build-essential","libgmp-dev","libgmp3-dev","libgmp3c2","zlib1g-dev" のパッケージに依存しています。
<pre>
sudo apt-get update && sudo apt-get install build-essential libgmp-dev libgmp3-dev zlib1g-dev
</pre>
執筆時点において libgmp3c2 の raring 向けパッケージが公式アーカイブにありません。
しかし Ubuntu 12.10 (quantal) のパッケージを入れれば問題ありません。
sources.list に quantal を追加するか deb ファイルを取ってきてインストールしましょう。
<pre>
wget -o libgmp3c2.deb http://jp.archive.ubuntu.com/ubuntu/pool/universe/g/gmp4/libgmp3c2_4.3.2+dfsg-2ubuntu1_amd64.deb && sudo dpkg -i libgmp3c2.deb
</pre>

2. ghc のインストール
以下では ghc 7.6.3 をインストールする手順を説明します。
prefix オプションで指定した先(/path/to/ghc)にユーザ権限でインストールするとします。
<pre>
wget http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-x86_64-unknown-linux.tar.bz2
tar -xvf ghc-7.6.3-x86_64-unknown-linux.tar.bz2 && cd ghc-7.6.3 
mkdir /path/to/ghc
./configure --prefix=/path/to/ghc && make install
</pre>
インストールできたら /path/to/ghc/bin を PATH に追加してください。
<pre>
export PATH=/path/to/ghc/bin:$PATH
</pre>

3. cabal-install のインストール
以下では cabal-install 1.16.0.2 のインストール手順を説明します。
<pre>
wget http://hackage.haskell.org/packages/archive/cabal-install/1.16.0.2/cabal-install-1.16.0.2.tar.gz
tar -xvf cabal-install-1.16.0.2.tar.gz && cd  cabal-install-1.16.0.2/cabal-install
GHC=/path/to/ghc/bin/ghc GHC_PKG=/path/to/ghc/bin/ghc-pkg sh bootstrap.sh
</pre>
これで ~/.cabal/bin に cabal がインストールされたはずです。
PATH に追加しましょう。
<pre>
export PATH=$HOME/.cabal/bin:$PATH
</pre>

以上でインストールの手順は完了です。
