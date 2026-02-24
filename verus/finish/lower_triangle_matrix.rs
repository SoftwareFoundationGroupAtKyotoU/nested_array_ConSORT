use vstd::prelude::*;

verus! {
    // 1. 1次元配列（行）を生成し、最初の l 個を 1、残りを 0 で初期化する関数
    fn init_row(n: usize, l: usize) -> (res: Vec<i32>)
        requires
            n <= 1000,
            l <= n, // 1 にする個数(l)は、配列全体のサイズ(n)以下であること
        ensures
            res@.len() == n as int,
            // 最初の l 個は 1 になる
            forall|j: int| #![auto] 0 <= j && j < l ==> res@[j] == 1,
            // l 以降の要素は 0 になる
            forall|j: int| #![auto] l <= j && j < n ==> res@[j] == 0,
    {
        let mut vec = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                n <= 1000,
                l <= n,
                vec@.len() == i as int,
                // 追加済みの要素のうち、l より前は 1、l 以降は 0 であることを保つ
                forall|j: int| #![auto] 0 <= j && j < l && j < i ==> vec@[j] == 1,
                forall|j: int| #![auto] l <= j && j < i ==> vec@[j] == 0,
            decreases n - i
        {
            if i < l {
                vec.push(1);
            } else {
                vec.push(0);
            }
            i = i + 1;
        }
        vec
    }

    // 2. 三角行列を生成する関数 (擬似コードの easyMatrix に相当)
    fn easy_matrix(n: usize) -> (res: Vec<Vec<i32>>)
        requires
            n <= 1000,
        ensures
            res@.len() == n as int,
            forall|i: int| #![auto] 0 <= i && i < n ==> res@[i]@.len() == n as int,
            // 【下三角行列の証明 1】対角成分および左下 (j <= i) は 1
            forall|i: int, j: int| #![auto] 0 <= i && i < n && 0 <= j && j <= i ==> 
                res@[i]@[j] == 1,
            // 【下三角行列の証明 2】対角成分より右上 (j > i) は 0
            forall|i: int, j: int| #![auto] 0 <= i && i < n && i < j && j < n ==> 
                res@[i]@[j] == 0,
    {
        let mut matrix: Vec<Vec<i32>> = Vec::new();
        let mut i = 0;
        
        while i < n
            invariant
                i <= n,
                n <= 1000,
                matrix@.len() == i as int,
                forall|k: int| #![auto] 0 <= k && k < i ==> matrix@[k]@.len() == n as int,
                // ループの途中でも、下三角行列の性質を保っていることを保証
                forall|k: int, j: int| #![auto] 0 <= k && k < i && 0 <= j && j <= k ==> 
                    matrix@[k]@[j] == 1,
                forall|k: int, j: int| #![auto] 0 <= k && k < i && k < j && j < n ==> 
                    matrix@[k]@[j] == 0,
            decreases n - i
        {
            // 擬似コードの initlength = l - k + 1 のロジック。
            // 0行目は 1個、1行目は 2個... つまり i 行目は i + 1 個の `1` を持つ行を作る
            let row = init_row(n, i + 1);
            matrix.push(row);
            i = i + 1;
        }
        matrix
    }

    // 3. メインの検証関数
    fn main_verify(undet: usize, ind1: usize, ind2: usize)
        requires
            undet > 0,
            undet <= 1000,
            ind1 < undet,
            ind2 < undet,
            // 擬似コードの let ind2 = _ : (ind2 > ind1) に相当
            ind1 < ind2, 
    {
        // 三角行列を生成
        let kp = easy_matrix(undet);
        
        // アサーション！
        // 列(ind2) の方が 行(ind1) より大きい、つまり「対角線より右上」の要素を指定しているため、
        // easy_matrix 関数の ensures に従って、確実に 0 になっていることが証明されます！
        assert(kp[ind1 as int][ind2 as int] == 0);
    }
}