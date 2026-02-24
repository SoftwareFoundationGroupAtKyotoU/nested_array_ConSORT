use vstd::prelude::*;

verus! {
    // 1. 1次元配列を 1 で初期化する関数
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

    // 2. 2次元行列を 1 で初期化する関数
    fn init_matrix(l: usize, k: usize, matrix: &mut Vec<Vec<i32>>)
        requires
            old(matrix)@.len() == 0,
        ensures
            matrix@.len() == k as int,
            forall|x: int| #![auto] 0 <= x && x < k ==> matrix@[x]@.len() == l as int,
            forall|x: int, y: int| #![auto] 0 <= x && x < k && 0 <= y && y < l ==> matrix@[x]@[y] == 1,
    {
        let mut i = 0;
        while i < k
            invariant
                i <= k,
                matrix@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> matrix@[x]@.len() == l as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < l ==> matrix@[x]@[y] == 1,
            decreases k - i
        {
            matrix.push(init_row(l));
            i = i + 1;
        }
    }

    // 3. 行列のトレース（対角成分の和）を求める関数
    fn matrix_trace(n: usize, matrix: &Vec<Vec<i32>>) -> (sum: i32)
        requires
            n > 0,
            // 【重要】足し合わせる回数が多すぎるとオーバーフローするため、上限を設けます
            n <= 10000, 
            matrix@.len() == n as int,
            forall|x: int| #![auto] 0 <= x && x < n ==> matrix@[x]@.len() == n as int,
            // 【重要】対角成分が 0 以上であり、かつ大きすぎないことを要求します
            forall|i: int| #![auto] 0 <= i && i < n ==> 
                0 <= matrix@[i]@[i] && matrix@[i]@[i] <= 10000,
        ensures
            // 擬似コードの assert(d2 >= 0) を保証するため、戻り値が 0 以上であることを約束します
            sum >= 0,
    {
        let mut sum: i32 = 0;
        let mut i = 0;

        while i < n
            invariant
                i <= n,
                n <= 10000,
                matrix@.len() == n as int,
                forall|x: int| #![auto] 0 <= x && x < n ==> matrix@[x]@.len() == n as int,
                forall|k: int| #![auto] 0 <= k && k < n ==> 
                    0 <= matrix@[k]@[k] && matrix@[k]@[k] <= 10000,
                // sum は 0 以上の要素だけを足すため、常に 0 以上になる
                sum >= 0,
                // 【オーバーフロー防止の証明】sum の最大値は i * 10000 を超えないことを Verus に伝える
                sum as int <= (i as int) * 10000,
            decreases n - i
        {
            // 擬似コードの「ポインタをずらしながら再帰」する処理は、
            // 単純に matrix[i][i] を取得する処理と同じ意味になります！
            sum = sum + matrix[i][i];
            i = i + 1;
        }
        sum
    }

    // 4. メインの検証関数
    fn main_verify(ten: usize)
        requires
            // 擬似コードの let ten = _ : (ten > 0) を引数に置き換え、上限も付けます
            ten > 0,
            ten <= 10000,
    {
        let mut mat1: Vec<Vec<i32>> = Vec::new();
        
        // 1. 行列を 1 で初期化する
        init_matrix(ten, ten, &mut mat1);
        
        // 2. トレースを計算する
        // Verus は「mat1 はすべて 1」であることを知っているので、
        // matrix_trace が要求する「0 <= 要素 <= 10000」の条件を満たすと自動で判断します。
        let d2 = matrix_trace(ten, &mat1);
        
        // 3. 擬似コードの最後にあるアサート。
        // matrix_trace の ensures (sum >= 0) のおかげで、一発で証明完了します！
        assert(d2 >= 0);
    }
}