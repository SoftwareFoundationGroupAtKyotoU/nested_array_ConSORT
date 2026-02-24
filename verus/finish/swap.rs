use vstd::prelude::*;

verus! {
    // 1. 行（1次元配列）を丸ごとディープコピーする関数
    // 擬似コードの copyarray に相当します
    fn copy_row(l: usize, row: &Vec<i32>) -> (res: Vec<i32>)
        requires
            row@.len() == l as int,
        ensures
            res@.len() == l as int,
            // 中身が全く同じであることを保証
            forall|j: int| #![auto] 0 <= j && j < l ==> res@[j] == row@[j],
    {
        let mut res = Vec::new();
        let mut i = 0;
        while i < l
            invariant
                i <= l,
                row@.len() == l as int,
                res@.len() == i as int,
                forall|k: int| #![auto] 0 <= k && k < i ==> res@[k] == row@[k],
            decreases l - i
        {
            res.push(row[i]);
            i = i + 1;
        }
        res
    }

    // 2. 行列の特定の2行をスワップする関数
    fn swap_row(l: usize, n1: usize, n2: usize, matrix: &mut Vec<Vec<i32>>)
        requires
            old(matrix)@.len() >= 2,
            n1 < old(matrix)@.len(),
            n2 < old(matrix)@.len(),
            // すべての行の長さが l であること
            forall|i: int| #![auto] 0 <= i && i < old(matrix)@.len() ==> old(matrix)@[i]@.len() == l as int,
        ensures
            matrix@.len() == old(matrix)@.len(),
            forall|i: int| #![auto] 0 <= i && i < matrix@.len() ==> matrix@[i]@.len() == l as int,
            
            // 【修正ポイント】行全体の比較(==)ではなく、要素(j)ごとの比較に分解する！
            forall|j: int| #![auto] 0 <= j && j < l ==> 
                matrix@[n1 as int]@[j] == old(matrix)@[n2 as int]@[j],
            forall|j: int| #![auto] 0 <= j && j < l ==> 
                matrix@[n2 as int]@[j] == old(matrix)@[n1 as int]@[j],
            
            // それ以外の行も、要素ごとに元のままであることを保証
            forall|i: int, j: int| #![auto] 
                0 <= i && i < matrix@.len() && 
                i != n1 as int && i != n2 as int && 
                0 <= j && j < l ==>
                matrix@[i]@[j] == old(matrix)@[i]@[j],
    {
        if n1 == n2 {
            return; // 同じ行なら何もしない
        }

        // 擬似コードの通り、行のコピー（ダミー）を作成する
        let dummy1 = copy_row(l, &matrix[n1]);
        let dummy2 = copy_row(l, &matrix[n2]);

        // Verusの `set` メソッドを用いて、元の行列の行を丸ごと差し替える
        matrix.set(n1, dummy2);
        matrix.set(n2, dummy1);
    }

    // 3. テスト用の行列を初期化する関数
    // 擬似コードの (=> (i2 = 1) (v = 1)) and (=> (i2 = 0) (v = 0)) に従い、
    // 0行目を 0、1行目を 1 で初期化します。
    fn init_mat1(n: usize) -> (res: Vec<Vec<i32>>)
        requires
            n >= 2, // 0行目と1行目を作るので、サイズは2以上必要
        ensures
            res@.len() == n as int,
            forall|i: int| #![auto] 0 <= i && i < n ==> res@[i]@.len() == n as int,
            // 0行目はすべて 0
            forall|j: int| #![auto] 0 <= j && j < n ==> res@[0]@[j] == 0,
            // 1行目はすべて 1
            forall|j: int| #![auto] 0 <= j && j < n ==> res@[1]@[j] == 1,
    {
        let mut matrix: Vec<Vec<i32>> = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                matrix@.len() == i as int,
                forall|k: int| #![auto] 0 <= k && k < i ==> matrix@[k]@.len() == n as int,
                i > 0 ==> forall|j: int| #![auto] 0 <= j && j < n ==> matrix@[0]@[j] == 0,
                i > 1 ==> forall|j: int| #![auto] 0 <= j && j < n ==> matrix@[1]@[j] == 1,
            decreases n - i
        {
            let mut row: Vec<i32> = Vec::new();
            let mut j = 0;
            while j < n
                invariant
                    j <= n,
                    row@.len() == j as int,
                    i == 0 ==> forall|k: int| #![auto] 0 <= k && k < j ==> row@[k] == 0,
                    i == 1 ==> forall|k: int| #![auto] 0 <= k && k < j ==> row@[k] == 1,
                decreases n - j
            {
                if i == 0 {
                    row.push(0);
                } else if i == 1 {
                    row.push(1);
                } else {
                    row.push(-1); // 0, 1 行目以外は使わないので適当な値
                }
                j = j + 1;
            }
            matrix.push(row);
            i = i + 1;
        }
        matrix
    }

    // 4. メインの検証関数
    fn main_verify(unde: usize, ind1: usize)
        requires
            unde >= 2,
            ind1 < unde,
    {
        // 1. mat1 を条件通りに初期化
        let mut mat1 = init_mat1(unde);

        // 2. 0行目と1行目をスワップする
        swap_row(unde, 0, 1, &mut mat1);

        // 3. アサーション！
        // スワップされたので、1行目が 0、0行目が 1 になっていることを証明！
        assert(mat1[1][ind1 as int] == 0);
        assert(mat1[0][ind1 as int] == 1);
    }
}