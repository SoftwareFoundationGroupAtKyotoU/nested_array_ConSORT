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

    // 3. 1次元配列同士を加算する関数
    // p と q は読み取り専用の参照(&)として受け取り、新しい配列 r を返します
    fn add_array(n: usize, p: &Vec<i32>, q: &Vec<i32>) -> (r: Vec<i32>)
        requires
            p@.len() == n as int,
            q@.len() == n as int,
            // 【追加】足し算の結果が i32 の最小値〜最大値の範囲に収まるという約束
            forall|j: int| #![auto] 0 <= j && j < n ==> 
                -2147483648 <= p@[j] + q@[j] && p@[j] + q@[j] <= 2147483647,
        ensures
            r@.len() == n as int,
            forall|j: int| #![auto] 0 <= j && j < n ==> r@[j] == p@[j] + q@[j],
    {
        let mut r = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                p@.len() == n as int,
                q@.len() == n as int,
                r@.len() == i as int,
                // 【追加】ループ内でも同じ約束を引き継ぐ
                forall|j: int| #![auto] 0 <= j && j < n ==> 
                    -2147483648 <= p@[j] + q@[j] && p@[j] + q@[j] <= 2147483647,
                forall|j: int| #![auto] 0 <= j && j < i ==> r@[j] == p@[j] + q@[j],
            decreases n - i
        {
            r.push(p[i] + q[i]);
            i = i + 1;
        }
        r
    }

    // 4. 2次元行列同士を加算する関数
    // 4. 2次元行列同士を加算する関数
    fn add_matrix(n1: usize, n2: usize, pp: &Vec<Vec<i32>>, qq: &Vec<Vec<i32>>, rr: &mut Vec<Vec<i32>>)
        requires
            old(rr)@.len() == 0,
            pp@.len() == n2 as int,
            qq@.len() == n2 as int,
            forall|x: int| #![auto] 0 <= x && x < n2 ==> pp@[x]@.len() == n1 as int,
            forall|x: int| #![auto] 0 <= x && x < n2 ==> qq@[x]@.len() == n1 as int,
            forall|x: int, y: int| #![auto] 0 <= x && x < n2 && 0 <= y && y < n1 ==> 
                -2147483648 <= pp@[x]@[y] + qq@[x]@[y] && pp@[x]@[y] + qq@[x]@[y] <= 2147483647,
        ensures
            rr@.len() == n2 as int,
            forall|x: int| #![auto] 0 <= x && x < n2 ==> rr@[x]@.len() == n1 as int,
            forall|x: int, y: int| #![auto] 0 <= x && x < n2 && 0 <= y && y < n1 ==> 
                rr@[x]@[y] == pp@[x]@[y] + qq@[x]@[y],
    {
        let mut i = 0;
        while i < n2
            invariant
                i <= n2,
                pp@.len() == n2 as int,
                qq@.len() == n2 as int,
                forall|x: int| #![auto] 0 <= x && x < n2 ==> pp@[x]@.len() == n1 as int,
                forall|x: int| #![auto] 0 <= x && x < n2 ==> qq@[x]@.len() == n1 as int,
                rr@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> rr@[x]@.len() == n1 as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < n1 ==> 
                    rr@[x]@[y] == pp@[x]@[y] + qq@[x]@[y],
                // 【ここに追加！】全体の約束から「i行目の」約束を明示的に引き出す
                forall|x: int, y: int| #![auto] 0 <= x && x < n2 && 0 <= y && y < n1 ==> 
                    -2147483648 <= pp@[x]@[y] + qq@[x]@[y] && pp@[x]@[y] + qq@[x]@[y] <= 2147483647,
            decreases n2 - i
        {
            let z = add_array(n1, &pp[i], &qq[i]);
            rr.push(z);
            i = i + 1;
        }
    }

    // 5. メインの検証関数
    fn main_verify(ten: usize, ind1: usize, ind2: usize)
        requires
            ten > 0,
            ind1 < ten,
            ind2 < ten,
    {
        let mut mat1: Vec<Vec<i32>> = Vec::new();
        let mut mat2: Vec<Vec<i32>> = Vec::new();
        let mut mat3: Vec<Vec<i32>> = Vec::new();

        // 1. mat1 と mat2 をそれぞれ 1 で初期化
        init_matrix(ten, ten, &mut mat1);
        init_matrix(ten, ten, &mut mat2);

        // 2. mat1 と mat2 を加算して mat3 に格納
        add_matrix(ten, ten, &mat1, &mat2, &mut mat3);

        // 3. アサーション！
        // Verusは自動的に「mat1=1」「mat2=1」「mat3=mat1+mat2」という事実から、
        // 「mat3=2」であることを数学的に導き出し、このアサートを通過させます！
        assert(mat1[ind1 as int][ind2 as int] == 1);
        assert(mat2[ind1 as int][ind2 as int] == 1);
        assert(mat3[ind1 as int][ind2 as int] == 2);
    }
}