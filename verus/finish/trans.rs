use vstd::prelude::*;

verus! {
    // 1. 元の行列から特定の「列（column）」を抽出して1次元配列にする関数
    // 擬似コードの init 関数に相当します
    fn extract_column(n: usize, col_idx: usize, matrix: &Vec<Vec<i32>>) -> (res: Vec<i32>)
        requires
            matrix@.len() == n as int,
            col_idx < n,
            forall|i: int| #![auto] 0 <= i && i < n ==> matrix@[i]@.len() == n as int,
        ensures
            res@.len() == n as int,
            // 抽出された配列の i 番目の要素は、元の行列の i 行目・col_idx 列目の要素になる
            forall|i: int| #![auto] 0 <= i && i < n ==> res@[i] == matrix@[i]@[col_idx as int],
    {
        let mut res: Vec<i32> = Vec::new(); // 型推論エラー回避
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                matrix@.len() == n as int,
                col_idx < n,
                forall|k: int| #![auto] 0 <= k && k < n ==> matrix@[k]@.len() == n as int,
                res@.len() == i as int,
                forall|k: int| #![auto] 0 <= k && k < i ==> res@[k] == matrix@[k]@[col_idx as int],
            decreases n - i
        {
            // 行を i で進めながら、列は col_idx に固定して縦に拾っていく
            res.push(matrix[i][col_idx]);
            i = i + 1;
        }
        res
    }

    // 2. 行列全体を転置する関数
    // 擬似コードの trans 関数に相当します
    fn transpose_matrix(n: usize, pp: &Vec<Vec<i32>>, qq: &mut Vec<Vec<i32>>)
        requires
            old(qq)@.len() == 0,
            pp@.len() == n as int,
            forall|i: int| #![auto] 0 <= i && i < n ==> pp@[i]@.len() == n as int,
        ensures
            qq@.len() == n as int,
            forall|i: int| #![auto] 0 <= i && i < n ==> qq@[i]@.len() == n as int,
            // 【重要】転置の性質：新しい行列 qq の (i, j) 成分は、元の行列 pp の (j, i) 成分に等しい！
            forall|i: int, j: int| #![auto] 0 <= i && i < n && 0 <= j && j < n ==> 
                qq@[i]@[j] == pp@[j]@[i],
    {
        let mut col_idx = 0;
        while col_idx < n
            invariant
                col_idx <= n,
                pp@.len() == n as int,
                forall|i: int| #![auto] 0 <= i && i < n ==> pp@[i]@.len() == n as int,
                qq@.len() == col_idx as int,
                forall|i: int| #![auto] 0 <= i && i < col_idx ==> qq@[i]@.len() == n as int,
                // ループの途中でも、これまで追加した行について転置の性質が成り立っていることを保証
                forall|i: int, j: int| #![auto] 0 <= i && i < col_idx && 0 <= j && j < n ==> 
                    qq@[i]@[j] == pp@[j]@[i],
            decreases n - col_idx
        {
            // pp の col_idx 列目を抽出して、qq の新しい行として追加する
            let col = extract_column(n, col_idx, pp);
            qq.push(col);
            col_idx = col_idx + 1;
        }
    }

    // 3. テスト用の行列を初期化する関数
    // 擬似コードの {v:int| (v = i2)} の通り、値が「列のインデックス j に等しい」行列を作ります
    fn init_mat1(n: usize) -> (res: Vec<Vec<i32>>)
        // 【追加】n が大きすぎないことを要求（i32へのキャストを安全にするため）
        requires
            n <= 10000,
        ensures
            res@.len() == n as int,
            forall|i: int| #![auto] 0 <= i && i < n ==> res@[i]@.len() == n as int,
            forall|i: int, j: int| #![auto] 0 <= i && i < n && 0 <= j && j < n ==> 
                res@[i]@[j] == j as int,
    {
        let mut matrix: Vec<Vec<i32>> = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                // 【追加】外側のループにも n の上限を教える
                n <= 10000, 
                matrix@.len() == i as int,
                forall|k: int| #![auto] 0 <= k && k < i ==> matrix@[k]@.len() == n as int,
                forall|k: int, j: int| #![auto] 0 <= k && k < i && 0 <= j && j < n ==> 
                    matrix@[k]@[j] == j as int,
            decreases n - i
        {
            let mut row: Vec<i32> = Vec::new();
            let mut j = 0;
            while j < n
                invariant
                    j <= n,
                    // 【追加】内側のループにも n の上限を教える（これが一番重要！）
                    n <= 10000, 
                    row@.len() == j as int,
                    forall|k: int| #![auto] 0 <= k && k < j ==> row@[k] == k as int,
                decreases n - j
            {
                // n <= 10000 なので、j も最大 10000。
                // したがって j as i32 は絶対にオーバーフローしないとVerusが確信する！
                row.push(j as i32); 
                j = j + 1;
            }
            matrix.push(row);
            i = i + 1;
        }
        matrix
    }

    // 4. メインの検証関数
    fn main_verify(unde: usize, ind1: usize, ind2: usize)
        requires
            unde > 0,
            unde <= 1000,
            ind1 < unde,
            ind2 < unde,
    {
        // 1. mat1 を初期化
        let mat1 = init_mat1(unde);
        let mut mat2: Vec<Vec<i32>> = Vec::new();
        
        // 2. 転置を実行！
        transpose_matrix(unde, &mat1, &mut mat2);
        
        // 3. 転置の性質が成り立っていることをアサート！
        assert(mat1[ind1 as int][ind2 as int] == mat2[ind2 as int][ind1 as int]);
    }
}