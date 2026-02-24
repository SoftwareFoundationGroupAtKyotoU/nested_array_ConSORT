use vstd::prelude::*;

verus! {
    // 1. 1次元配列を val, val-1, val-2... と初期化する関数
    fn init_row(n: usize, val: i32) -> (res: Vec<i32>)
        requires
            n <= 1000,
            // オーバーフローを防ぐための現実的な上限と下限
            -1000000 <= val && val <= 1000000,
        ensures
            res@.len() == n as int,
            // j 番目の要素は val - j になることを保証
            forall|j: int| #![auto] 0 <= j && j < n ==> res@[j] == val - j,
    {
        let mut vec = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                n <= 1000,
                -1000000 <= val && val <= 1000000,
                vec@.len() == i as int,
                // ループ途中でも、追加した要素は val - j になっている
                forall|j: int| #![auto] 0 <= j && j < i ==> vec@[j] == val - j,
            decreases n - i
        {
            // Rustの実行コード。val から i を引いて push する
            vec.push(val - i as i32);
            i = i + 1;
        }
        vec
    }

    // 2. 行列を初期化する関数（すべての行が同じになる）
    fn init_matrix(l: usize, k: usize, val: i32) -> (res: Vec<Vec<i32>>)
        requires
            l <= 1000,
            k <= 1000,
            -1000000 <= val && val <= 1000000,
        ensures
            res@.len() == k as int,
            forall|x: int| #![auto] 0 <= x && x < k ==> res@[x]@.len() == l as int,
            // 行(x)に依存せず、列(y)だけで値が決まる (val - y)
            forall|x: int, y: int| #![auto] 0 <= x && x < k && 0 <= y && y < l ==> 
                res@[x]@[y] == val - y,
    {
        let mut matrix: Vec<Vec<i32>> = Vec::new();
        let mut i = 0;
        while i < k
            invariant
                i <= k,
                l <= 1000,
                k <= 1000,
                -1000000 <= val && val <= 1000000,
                matrix@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> matrix@[x]@.len() == l as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < l ==> 
                    matrix@[x]@[y] == val - y,
            decreases k - i
        {
            matrix.push(init_row(l, val));
            i = i + 1;
        }
        matrix
    }

    // 3. メインの検証関数
    fn main_verify(ind1: usize, ind2: usize)
        requires
            // 擬似コードの条件 (ind1 >= 0 は usize なら自動的に満たされるため省略)
            ind1 <= 2,
            ind2 < 3,
    {
        let three: usize = 3;
        let val_p: i32 = 3;
        let val_q: i32 = 4;

        // 1. 行列の生成
        let p1 = init_matrix(three, three, val_p);
        let q1 = init_matrix(three, three, val_q);

        // 2. アサーション！
        // Z3ソルバは以下のように推論します。
        // 「q1 の要素は val_q - ind2、つまり 4 - ind2 だ！」 (1つ目のアサートが成功)
        assert(q1[ind1 as int][ind2 as int] == val_q - ind2 as int);
        
        // 「p1 の要素は 3 - ind2、q1 は 4 - ind2。
        //   ind2 が何であっても 3 - ind2 < 4 - ind2 だ！」 (2つ目のアサートが成功)
        assert(p1[ind1 as int][ind2 as int] < q1[ind1 as int][ind2 as int]);
    }
}