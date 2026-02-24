use vstd::prelude::*;

verus! {
    // 1. テスト用の行列を `1` で初期化する関数
    fn init_row(n: usize) -> (res: Vec<i32>)
        requires n <= 10000,
        ensures
            res@.len() == n as int,
            forall|j: int| #![auto] 0 <= j && j < n ==> res@[j] == 1,
    {
        let mut vec = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                n <= 10000,
                vec@.len() == i as int,
                forall|j: int| #![auto] 0 <= j && j < i ==> vec@[j] == 1,
            decreases n - i
        {
            vec.push(1);
            i = i + 1;
        }
        vec
    }

    fn init_matrix(l: usize, k: usize) -> (res: Vec<Vec<i32>>)
        requires
            l <= 10000,
            k <= 10000,
        ensures
            res@.len() == k as int,
            forall|x: int| #![auto] 0 <= x && x < k ==> res@[x]@.len() == l as int,
            forall|x: int, y: int| #![auto] 0 <= x && x < k && 0 <= y && y < l ==> res@[x]@[y] == 1,
    {
        let mut matrix: Vec<Vec<i32>> = Vec::new();
        let mut i = 0;
        while i < k
            invariant
                i <= k,
                l <= 10000,
                k <= 10000,
                matrix@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> matrix@[x]@.len() == l as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < l ==> matrix@[x]@[y] == 1,
            decreases k - i
        {
            matrix.push(init_row(l));
            i = i + 1;
        }
        matrix
    }

    // 2. 配列を「前から」順番に足す関数 (擬似コードの sum)
    fn sum_forward(n: usize, p: &Vec<i32>) -> (res: i32)
        requires
            p@.len() == n as int,
            n <= 10000,
            forall|j: int| #![auto] 0 <= j && j < n ==> p@[j] == 1,
        ensures
            res as int == n as int,
    {
        let mut sum: i32 = 0;
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                n <= 10000,
                p@.len() == n as int,
                forall|j: int| #![auto] 0 <= j && j < n ==> p@[j] == 1,
                sum as int == i as int,
            decreases n - i
        {
            sum = sum + p[i];
            i = i + 1;
        }
        sum
    }

    // 3. 配列を「後ろから」順番に足す関数 (擬似コードの sumback)
    fn sum_backward(n: usize, p: &Vec<i32>) -> (res: i32)
        requires
            p@.len() == n as int,
            n <= 10000,
            forall|j: int| #![auto] 0 <= j && j < n ==> p@[j] == 1,
        ensures
            res as int == n as int,
    {
        let mut sum: i32 = 0;
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                n <= 10000,
                p@.len() == n as int,
                forall|j: int| #![auto] 0 <= j && j < n ==> p@[j] == 1,
                sum as int == i as int,
            decreases n - i
        {
            sum = sum + p[n - 1 - i];
            i = i + 1;
        }
        sum
    }

    // 4. 行列の合計を求める関数
    fn sum_matrix(n1: usize, n2: usize, q: &Vec<Vec<i32>>) -> (res: i32)
        requires
            q@.len() == n2 as int,
            n1 <= 10000,
            n2 <= 10000,
            forall|x: int| #![auto] 0 <= x && x < n2 ==> q@[x]@.len() == n1 as int,
            forall|x: int, y: int| #![auto] 0 <= x && x < n2 && 0 <= y && y < n1 ==> q@[x]@[y] == 1,
        ensures
            res as int == (n1 as int) * (n2 as int),
    {
        let mut sum: i32 = 0;
        let mut i = 0;
        while i < n2
            invariant
                i <= n2,
                n1 <= 10000,
                n2 <= 10000,
                q@.len() == n2 as int,
                forall|x: int| #![auto] 0 <= x && x < n2 ==> q@[x]@.len() == n1 as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < n2 && 0 <= y && y < n1 ==> q@[x]@[y] == 1,
                sum as int == (n1 as int) * (i as int),
                sum >= 0,
                sum <= 100000000,
            decreases n2 - i
        {
            let s = sum_forward(n1, &q[i]);
            
            // proof ブロックで証明だけを行う
            proof {
                assert(s as int == n1 as int);
                
                // 【魔法のキーワード】分配法則を非線形ソルバで証明させる！
                assert((n1 as int) * (i as int) + (n1 as int) == (n1 as int) * (i as int + 1)) by(nonlinear_arith);
                
                // 【魔法のキーワード】1万×1万が1億以下に収まることも非線形ソルバで証明させる！
                assert(i + 1 <= 10000); // i < n2 かつ n2 <= 10000 なので
                assert((n1 as int) * (i as int + 1) <= 100000000) by(nonlinear_arith) 
                    requires n1 <= 10000, i + 1 <= 10000;
                
                assert(sum as int + s as int == (n1 as int) * (i as int + 1));
            }
            
            sum = sum + s;
            i = i + 1;
        }
        sum
    }

    // 5. 行列の合計を求める別の関数
    fn sum_back_matrix(n1: usize, n2: usize, q: &Vec<Vec<i32>>) -> (res: i32)
        requires
            q@.len() == n2 as int,
            n1 <= 10000,
            n2 <= 10000,
            forall|x: int| #![auto] 0 <= x && x < n2 ==> q@[x]@.len() == n1 as int,
            forall|x: int, y: int| #![auto] 0 <= x && x < n2 && 0 <= y && y < n1 ==> q@[x]@[y] == 1,
        ensures
            res as int == (n1 as int) * (n2 as int),
    {
        if n1 > 0 && n2 > 0 {
            let s = sum_backward(n1, &q[0]);
            
            proof {
                assert(s as int == n1 as int);
            }
            
            let mut sum: i32 = 0;
            let mut i = 1;
            while i < n2
                invariant
                    1 <= i && i <= n2,
                    n1 <= 10000,
                    n2 <= 10000,
                    q@.len() == n2 as int,
                    forall|x: int| #![auto] 0 <= x && x < n2 ==> q@[x]@.len() == n1 as int,
                    forall|x: int, y: int| #![auto] 0 <= x && x < n2 && 0 <= y && y < n1 ==> q@[x]@[y] == 1,
                    sum as int == (n1 as int) * ((i - 1) as int),
                    sum >= 0,
                    sum <= 100000000,
                decreases n2 - i
            {
                let y = sum_forward(n1, &q[i]);
                
                proof {
                    assert(y as int == n1 as int);
                    
                    // 分配法則
                    assert((n1 as int) * ((i - 1) as int) + (n1 as int) == (n1 as int) * (i as int)) by(nonlinear_arith);
                    
                    // オーバーフロー限界
                    assert((n1 as int) * (i as int) <= 100000000) by(nonlinear_arith) 
                        requires n1 <= 10000, i <= 10000;
                    
                    assert(sum as int + y as int == (n1 as int) * (i as int));
                }
                
                sum = sum + y;
                i = i + 1;
            }
            
            // 最後のまとめの計算も非線形ソルバに任せる
            proof {
                assert(s as int == n1 as int);
                assert(sum as int == (n1 as int) * ((n2 - 1) as int));
                assert((n1 as int) + (n1 as int) * ((n2 - 1) as int) == (n1 as int) * (n2 as int)) by(nonlinear_arith);
            }
            
            s + sum
        } else {
            0
        }
    }

    // 6. メインの検証関数
    fn main_verify(unde: usize)
        requires
            unde > 0,
            unde <= 10000,
    {
        let mat1 = init_matrix(unde, unde);
        
        // 異なるアルゴリズムで計算
        let d2 = sum_matrix(unde, unde, &mat1);
        let d3 = sum_back_matrix(unde, unde, &mat1);
        
        // Verus は、両方の関数が ensures で `res == n1 * n2` を保証しているのを見て、
        // 瞬時に d2 == d3 であると判断します！
        assert(d2 == d3);
    }
}