use vstd::prelude::*;

verus! {
    // 1. 1次元配列を 0 で初期化する関数
    fn init_row(n: usize) -> (res: Vec<i32>)
        requires
            n <= 10000,
        ensures
            res@.len() == n as int,
            forall|j: int| #![auto] 0 <= j && j < n ==> res@[j] == 0,
    {
        let mut vec = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                n <= 10000,
                vec@.len() == i as int,
                forall|j: int| #![auto] 0 <= j && j < i ==> vec@[j] == 0,
            decreases n - i
        {
            vec.push(0);
            i = i + 1;
        }
        vec
    }

    // 2. 行列を 0 で初期化する関数
    fn init_matrix(l: usize, k: usize, matrix: &mut Vec<Vec<i32>>)
        requires
            old(matrix)@.len() == 0,
            l <= 10000,
            k <= 10000,
        ensures
            matrix@.len() == k as int,
            forall|x: int| #![auto] 0 <= x && x < k ==> matrix@[x]@.len() == l as int,
            forall|x: int, y: int| #![auto] 0 <= x && x < k && 0 <= y && y < l ==> matrix@[x]@[y] == 0,
    {
        let mut i = 0;
        while i < k
            invariant
                i <= k,
                l <= 10000,
                k <= 10000,
                matrix@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> matrix@[x]@.len() == l as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < l ==> matrix@[x]@[y] == 0,
            decreases k - i
        {
            matrix.push(init_row(l));
            i = i + 1;
        }
    }

    // 3. 1次元配列のコピーを行う関数 (擬似コードの copyarray に相当)
    fn copy_array(n: usize, p: &Vec<i32>) -> (q: Vec<i32>)
        requires
            p@.len() == n as int,
            n <= 10000,
        ensures
            q@.len() == n as int,
            // 【重要】コピー先(q)の中身がコピー元(p)と完全に同じになることを保証
            forall|j: int| #![auto] 0 <= j && j < n ==> q@[j] == p@[j],
    {
        let mut q = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                n <= 10000,
                p@.len() == n as int,
                q@.len() == i as int,
                // ループの途中でも、コピーされた部分までは完全に一致している
                forall|j: int| #![auto] 0 <= j && j < i ==> q@[j] == p@[j],
            decreases n - i
        {
            q.push(p[i]);
            i = i + 1;
        }
        q
    }

    // 4. 2次元行列のコピーを行う関数 (擬似コードの copymatrix に相当)
    fn copy_matrix(n1: usize, n2: usize, pp: &Vec<Vec<i32>>, qq: &mut Vec<Vec<i32>>)
        requires
            old(qq)@.len() == 0,
            pp@.len() == n2 as int,
            n1 <= 10000,
            n2 <= 10000,
            forall|x: int| #![auto] 0 <= x && x < n2 ==> pp@[x]@.len() == n1 as int,
        ensures
            qq@.len() == n2 as int,
            forall|x: int| #![auto] 0 <= x && x < n2 ==> qq@[x]@.len() == n1 as int,
            // 【重要】行列全体で要素が完全に一致することを保証
            forall|x: int, y: int| #![auto] 0 <= x && x < n2 && 0 <= y && y < n1 ==> 
                qq@[x]@[y] == pp@[x]@[y],
    {
        let mut i = 0;
        while i < n2
            invariant
                i <= n2,
                n1 <= 10000,
                n2 <= 10000,
                pp@.len() == n2 as int,
                qq@.len() == i as int,
                forall|k: int| #![auto] 0 <= k && k < n2 ==> pp@[k]@.len() == n1 as int,
                forall|k: int| #![auto] 0 <= k && k < i ==> qq@[k]@.len() == n1 as int,
                // コピー済みの行について、要素の完全一致を保証
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < n1 ==> 
                    qq@[x]@[y] == pp@[x]@[y],
            decreases n2 - i
        {
            // pp の i 行目をコピーして、qq の新しい行として追加する
            let row_copy = copy_array(n1, &pp[i]);
            qq.push(row_copy);
            i = i + 1;
        }
    }

    // 5. メインの検証関数
    fn main_verify(ten: usize, ind1: usize, ind2: usize)
        requires
            ten > 0,
            ten <= 10000,
            ind1 < ten,
            ind2 < ten,
    {
        let mut mat1: Vec<Vec<i32>> = Vec::new();
        let mut mat2: Vec<Vec<i32>> = Vec::new();

        // 1. mat1 をすべて 0 で初期化
        init_matrix(ten, ten, &mut mat1);

        // 2. mat1 を mat2 にコピー
        copy_matrix(ten, ten, &mat1, &mut mat2);

        // 3. アサーションの検証
        // Verus は「mat1 はすべて 0」「mat2 は mat1 と同じ」という2つの契約を繋ぎ合わせ、
        // 「mat2 もすべて 0 だ！」と自動的に数学的結論を導き出します。
        assert(mat2[ind2 as int][ind1 as int] == 0);
    }
}