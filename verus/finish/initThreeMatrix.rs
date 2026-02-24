use vstd::prelude::*;

verus! {
    // 1. 1次元配列を 1 で初期化する関数 (擬似コードの init)
    fn init_1d(n: usize) -> (res: Vec<i32>)
        requires
            n <= 1000,
        ensures
            res@.len() == n as int,
            forall|i: int| #![auto] 0 <= i && i < n ==> res@[i] == 1,
    {
        let mut vec = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                n <= 1000,
                vec@.len() == i as int,
                forall|j: int| #![auto] 0 <= j && j < i ==> vec@[j] == 1,
            decreases n - i
        {
            vec.push(1);
            i = i + 1;
        }
        vec
    }

    // 2. 2次元行列を 1 で初期化する関数 (擬似コードの iM)
    fn init_2d(l: usize, k: usize) -> (res: Vec<Vec<i32>>)
        requires
            l <= 1000,
            k <= 1000,
        ensures
            res@.len() == k as int,
            forall|x: int| #![auto] 0 <= x && x < k ==> res@[x]@.len() == l as int,
            forall|x: int, y: int| #![auto] 0 <= x && x < k && 0 <= y && y < l ==> 
                res@[x]@[y] == 1,
    {
        let mut matrix: Vec<Vec<i32>> = Vec::new(); // 明示的な型推論の補助は省略可能(戻り値で推論される場合もありますが、書いておくと安全です)
        let mut i = 0;
        while i < k
            invariant
                i <= k,
                l <= 1000,
                k <= 1000,
                matrix@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> matrix@[x]@.len() == l as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < l ==> 
                    matrix@[x]@[y] == 1,
            decreases k - i
        {
            matrix.push(init_1d(l));
            i = i + 1;
        }
        matrix
    }

    // 3. 3次元配列（テンソル）を 1 で初期化する関数 (擬似コードの iTm)
    // 擬似コードに合わせて、kk 個の「ll × ll」の2次元行列を作成して積み重ねます。
    fn init_3d(ll: usize, kk: usize) -> (res: Vec<Vec<Vec<i32>>>)
        requires
            ll <= 1000,
            kk <= 1000,
        ensures
            // 1次元目のサイズは kk
            res@.len() == kk as int,
            // 2次元目のサイズは ll
            forall|x: int| #![auto] 0 <= x && x < kk ==> res@[x]@.len() == ll as int,
            // 3次元目のサイズも ll
            forall|x: int, y: int| #![auto] 0 <= x && x < kk && 0 <= y && y < ll ==> 
                res@[x]@[y]@.len() == ll as int,
            // すべての要素が 1 であること！
            forall|x: int, y: int, z: int| #![auto] 
                0 <= x && x < kk && 0 <= y && y < ll && 0 <= z && z < ll ==> 
                res@[x]@[y]@[z] == 1,
    {
        // 【重要】3次元配列であることを明示して、Rustのコンパイラを助ける！
        let mut tensor: Vec<Vec<Vec<i32>>> = Vec::new();
        let mut i = 0;

        while i < kk
            invariant
                i <= kk,
                ll <= 1000,
                kk <= 1000,
                tensor@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> tensor@[x]@.len() == ll as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < ll ==> 
                    tensor@[x]@[y]@.len() == ll as int,
                // ループ中でも、追加された 3次元の全要素が 1 であることを保証
                forall|x: int, y: int, z: int| #![auto] 
                    0 <= x && x < i && 0 <= y && y < ll && 0 <= z && z < ll ==> 
                    tensor@[x]@[y]@[z] == 1,
            decreases kk - i
        {
            // ll × ll の2次元行列を作成して、テンソル（3次元目）に押し込む
            tensor.push(init_2d(ll, ll));
            i = i + 1;
        }
        tensor
    }

    // 4. メインの検証関数
    fn main_verify(x: usize, ind1: usize, ind2: usize, ind3: usize)
        requires
            x > 0,
            x <= 1000,
            ind1 < x,
            ind2 < x,
            ind3 < x,
    {
        // 1. 各次元が x サイズの 3次元配列を作成
        let p = init_3d(x, x);
        
        // 2. アサーション！
        // 3つのインデックスを使って奥底のデータにアクセスしても、確実に 1 になっている！
        assert(p[ind1 as int][ind2 as int][ind3 as int] == 1);
    }
}