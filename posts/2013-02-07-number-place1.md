---
title: Number Place Solver
tags: haskell, game
---

## NumberPlace を解いてみた

数独やナンバープレイスと呼ばれるパズルをHaskellで解くコードを書いてみました。
総当りではなく人間が解いていくやり方でマスを埋めていきます。
難しい問題は解けないかもしれません。
ゲームのルールの知っているものとして説明を省きます。

### 基本方針

1. 初期状態のデータを入力する
2. 全部のマスが埋まっているかどうかをチェックする（埋まってたら終了）
3. 縦、横、ブロックを調べて入力可能なマスを1から9までについて探す
4. もし入力可能なマスがあったら埋める
5. 手順2に戻って繰り返す

### データ構造

* 各マスには[1-9]の数値(Int)を入れます。何も入っていないマスには0を入れます。
* マスはData.Arrayを使って表現することにします。

上記の条件を踏まえて、骨格を実装していきます。
データ型とデータを作成するための関数を定義します。
Data.Array を使って 9x9 の Int 型の array を作成しようと思います。

今回使用する Data.Array の関数を紹介します。
<pre>
listArray :: Ix i => (i,i) -> [e] -> Array i e | リストから array を作成する
(!) :: Ix i => Array i e -> i -> e | インデックスで指定した値を取得する
indices :: Ix i => Array i e -> [i] | インデックスのリストを取得する
elems :: Ix i => Array i e -> [e] | 値のリストを取得する
assocs :: Ix i => Array i e -> [(i,e)] | インデックスと値のリストを取得する
(//) :: Ix i => Array i e -> [(i,e)] -> Array i e | array を更新する
</pre>
詳しくは Data.List のリファレンスを参照してください。
<pre>
import Data.List
import Data.Array
import Control.Applicative

type Index = (Int,Int)
type Board = Array Index Int

boardBounds :: (Index,Index)
boardBounds = ((1,1),(9,9))

createBoard :: [[Int]] -> Board
createBoard = listArray boardBounds . concat
</pre>
createBoard に 9x9 の2次元リストを渡すと Int 型の Data.Array を生成します。
listArray に境界インデックスとデータ（リスト）を渡すとN次元のArrayが作れます。
縦、横、ブロックのそれぞれにおいて要素を見る必要があるので、indices と elems をそれらについて作成します。
<pre>
horizontalIndices :: Index -> [Index]
horizontalIndices (i,_) = [(i,j) | j <- [1..9]]

verticalIndices :: Index -> [Index]
verticalIndices (_,i) = [(j,i) | j <- [1..9]]

blockIndices :: Index -> [Index]
blockIndices (i,j) = [(f i + x, f j + y) | x <- [1..3], y <- [1..3]]
    where f n = 3 * ((n - 1) `div` 3)

horizontalElems :: Board -> [[Int]]
horizontalElems = splits 9 . elems

verticalElems :: Board -> [[Int]]
verticalElems = transpose . horizontalElems

blockElems :: Board -> [[Int]]
blockElems b = concat [ sp xss | xss <- (splits 27 . elems) b]
    where
        sp :: [Int] -> [[Int]]
        sp xss = map concat $ transpose [splitAt3 (3,6) xs | xs <- splits 9 xss]
</pre>
ここで上記の関数を定義するために以下の補助関数を作成しました。
<pre>
isFinished :: Board -> Bool
isFinished b = all check (horizontalElems b ++ verticalElems b ++ blockElems b)
    where
        check :: [[Int]] -> Bool
        check xs = all (==[1..9]) $ map sort xs

solve :: Board -> Board
solve b
    | isFinished b = b
    | s == b = b
    | otherwise = solve s
    where
        s = solveStep b
</pre>
solve では isFinished が True もしくは solveStep b == b すなわち、それ以上解けない場合にループを抜けるように定義しています。
isFinished は縦、横、ブロックの全要素を見て [1-9] を含んでいたら True を返すという数独の定義そのものになっています。

長くなったので solveStep の説明以降は次回にすることにします。
