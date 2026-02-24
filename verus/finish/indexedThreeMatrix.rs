use vstd::prelude::*;

verus! {
    // 1. 1次元配列（行）を初期化する関数
    fn init_row(n: usize) -> (res: Vec<i32>)
        ensures
            res@.len() == n as int,
            forall|j: int| #![auto] 0 <= j && j < n ==> res@[j] == 1,
    {
        let mut vec = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                vec@.len() == i as int,
                forall|j: int| #![auto] 0 <= j && j < i ==> vec@[j] == 1,
            decreases n - i
        {
            vec.push(1);
            i = i + 1;
        }
        vec
    }

    // 2. 2次元配列（面）を初期化する関数 (擬似コードの iM に相当)
    // 3次元関数から呼び出しやすいよう、引数に &mut を取らず Vec を直接返す設計にしています
    fn init_matrix(k: usize) -> (res: Vec<Vec<i32>>)
        ensures
            res@.len() == k as int,
            forall|x: int| #![auto] 0 <= x && x < k ==> res@[x]@.len() == k as int - x,
            forall|x: int, y: int| #![auto] 0 <= x && x < k && 0 <= y && y < (k as int - x) ==> res@[x]@[y] == 1,
    {
        // ここを修正！
        let mut matrix: Vec<Vec<i32>> = Vec::new(); 
        let mut i = 0;
        
        while i < k
            invariant
                i <= k,
                matrix@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> matrix@[x]@.len() == k as int - x,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < (k as int - x) ==> matrix@[x]@[y] == 1,
            decreases k - i
        {
            matrix.push(init_row(k - i));
            i = i + 1;
        }
        matrix
    }

    // 3. 3次元配列（立体）を初期化する関数 (擬似コードの iTm に相当)
    fn init_tensor(m: usize, ppp: &mut Vec<Vec<Vec<i32>>>)
        requires
            old(ppp)@.len() == 0,
        ensures
            // 1次元目の長さ（高さ）
            ppp@.len() == m as int,
            // 2次元目の長さ（縦）
            forall|x: int| #![auto] 0 <= x && x < m ==> ppp@[x]@.len() == m as int - x,
            // 3次元目の長さ（横）: ここから「m - x - y」と短くなっていきます
            forall|x: int, y: int| #![auto] 0 <= x && x < m && 0 <= y && y < (m as int - x) ==> 
                ppp@[x]@[y]@.len() == m as int - x - y,
            // 【重要】すべての要素が 1 であること（x, y, z の3変数！）
            forall|x: int, y: int, z: int| #![auto] 
                0 <= x && x < m && 
                0 <= y && y < (m as int - x) && 
                0 <= z && z < (m as int - x - y) ==> 
                ppp@[x]@[y]@[z] == 1,
    {
        let mut i = 0;
        while i < m
            invariant
                i <= m,
                ppp@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> ppp@[x]@.len() == m as int - x,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < (m as int - x) ==> 
                    ppp@[x]@[y]@.len() == m as int - x - y,
                forall|x: int, y: int, z: int| #![auto] 
                    0 <= x && x < i && 
                    0 <= y && y < (m as int - x) && 
                    0 <= z && z < (m as int - x - y) ==> 
                    ppp@[x]@[y]@[z] == 1,
            decreases m - i
        {
            ppp.push(init_matrix(m - i));
            i = i + 1;
        }
    }

    // 4. 検証用メイン関数
    fn main_verify(ind1: usize, ind2: usize, ind3: usize)
        requires
            ind1 < 10,
            ind2 < 10 - ind1,        // 擬似コードの 9 - ind1 に対応（上限は配列長に揃えています）
            ind3 < 10 - ind1 - ind2, // 擬似コードの 9 - ind1 - ind2 に対応
    {
        // 3次元の空ベクタを生成
        let mut p: Vec<Vec<Vec<i32>>> = Vec::new();
        
        init_tensor(10, &mut p);

        // 3つのインデックスを使って値をアサート
        assert(p[ind1 as int][ind2 as int][ind3 as int] == 1);
    }
}