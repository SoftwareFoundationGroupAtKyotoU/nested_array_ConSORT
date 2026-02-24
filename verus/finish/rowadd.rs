use vstd::prelude::*;

verus! {
    // 1. 1次元配列を 1 で初期化する関数
    fn init_row_one(n: usize) -> (res: Vec<i32>)
        requires n <= 1000,
        ensures
            res@.len() == n as int,
            forall|j: int| #![auto] 0 <= j && j < n ==> res@[j] == 1,
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

    // 2. 行列を 1 で初期化する関数
    fn init_matrix_one(l: usize, k: usize) -> (res: Vec<Vec<i32>>)
        requires l <= 1000, k <= 1000,
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
                l <= 1000,
                k <= 1000,
                matrix@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> matrix@[x]@.len() == l as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < l ==> matrix@[x]@[y] == 1,
            decreases k - i
        {
            matrix.push(init_row_one(l));
            i = i + 1;
        }
        matrix
    }

    // 3. 1次元配列を 0 で初期化する関数（集約先の配列用）
    fn init_row_zero(n: usize) -> (res: Vec<i32>)
        requires n <= 1000,
        ensures
            res@.len() == n as int,
            forall|j: int| #![auto] 0 <= j && j < n ==> res@[j] == 0,
    {
        let mut vec = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                n <= 1000,
                vec@.len() == i as int,
                forall|j: int| #![auto] 0 <= j && j < i ==> vec@[j] == 0,
            decreases n - i
        {
            vec.push(0);
            i = i + 1;
        }
        vec
    }

    // 4. 1次元配列同士を足し合わせ、結果を p に書き込む関数
    fn add_array(n: usize, p: &mut Vec<i32>, q: &Vec<i32>)
        requires
            old(p)@.len() == n as int,
            q@.len() == n as int,
            // オーバーフロー防止：p も q も最大 1000 以下であること
            forall|j: int| #![auto] 0 <= j && j < n ==> 0 <= old(p)@[j] && old(p)@[j] <= 1000,
            forall|j: int| #![auto] 0 <= j && j < n ==> 0 <= q@[j] && q@[j] <= 1000,
        ensures
            p@.len() == n as int,
            // 書き換え後の p は、書き換え前の p (old) と q を足したものになる！
            forall|j: int| #![auto] 0 <= j && j < n ==> p@[j] == old(p)@[j] + q@[j],
    {
        // ループが始まる前の p の状態をゴーストとしてスナップショット
        let ghost p_old = p@;
        
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                q@.len() == n as int,
                p@.len() == n as int,
                p_old.len() == n as int,
                forall|j: int| #![auto] 0 <= j && j < n ==> 0 <= p_old[j] && p_old[j] <= 1000,
                forall|j: int| #![auto] 0 <= j && j < n ==> 0 <= q@[j] && q@[j] <= 1000,
                // i 未満（処理済み）の要素は足し算が終わっている
                forall|j: int| #![auto] 0 <= j && j < i ==> p@[j] == p_old[j] + q@[j],
                // i 以降（未処理）の要素はまだ p_old のままである
                forall|j: int| #![auto] i <= j && j < n ==> p@[j] == p_old[j],
            
            // 【重要】無限ループにならないことの証明（これが抜けていました！）
            decreases n - i
        {
            let x = p[i];
            let y = q[i];
            
            // 【追加したヒント】Z3ソルバにオーバーフローしないことを念押し
            assert(x == p_old[i as int]); 
            assert(0 <= x && x <= 1000);  
            assert(0 <= y && y <= 1000);  
            
            p.set(i, x + y); // p の i 番目を直接書き換える
            i = i + 1;
        }
    }

    // 5. 行列のすべての行を足し合わせる関数
    fn sum_row(n1: usize, n2: usize, pp: &Vec<Vec<i32>>, q: &mut Vec<i32>)
        requires
            n1 <= 1000,
            n2 <= 1000,
            old(q)@.len() == n1 as int,
            pp@.len() == n2 as int,
            forall|k: int| #![auto] 0 <= k && k < n2 ==> pp@[k]@.len() == n1 as int,
            // 行列の要素はすべて 1、集約先 q の初期値はすべて 0 であることを前提とします
            forall|j: int| #![auto] 0 <= j && j < n1 ==> old(q)@[j] == 0,
            forall|k: int, j: int| #![auto] 0 <= k && k < n2 && 0 <= j && j < n1 ==> pp@[k]@[j] == 1,
        ensures
            q@.len() == n1 as int,
            // 【証明の核】最終的に、q のすべての要素は足し合わせた行数（n2）に等しくなる！
            forall|j: int| #![auto] 0 <= j && j < n1 ==> q@[j] == n2 as int,
    {
        let mut i = 0;
        while i < n2
            invariant
                i <= n2,
                n1 <= 1000,
                n2 <= 1000,
                q@.len() == n1 as int,
                pp@.len() == n2 as int,
                forall|k: int| #![auto] 0 <= k && k < n2 ==> pp@[k]@.len() == n1 as int,
                forall|k: int, j: int| #![auto] 0 <= k && k < n2 && 0 <= j && j < n1 ==> pp@[k]@[j] == 1,
                // i行目まで足し終わった時点では、q の要素は i になっている
                forall|j: int| #![auto] 0 <= j && j < n1 ==> q@[j] == i as int,
            decreases n2 - i
        {
            // add_array を呼ぶ前に、「現在の q はオーバーフローしない安全な値だよ」とZ3に念押しヒントを与える
            assert(forall|j: int| #![auto] 0 <= j && j < n1 ==> 0 <= q@[j] && q@[j] <= 1000);
            
            // q に pp の i 行目を足し込む（q が上書きされていく）
            add_array(n1, q, &pp[i]);
            
            i = i + 1;
        }
    }

    // 6. メインの検証関数
    fn main_verify(unde: usize, ind1: usize)
        requires
            unde >= 1,
            unde <= 1000,
            ind1 < unde,
    {
        // mat1 を 1 で初期化
        let mat1 = init_matrix_one(unde, unde);
        
        // 擬似コードの alloc unde に相当。集約先の mat2 を 0 で初期化する
        let mut mat2 = init_row_zero(unde);
        
        // 足し合わせを実行！
        sum_row(unde, unde, &mat1, &mut mat2);
        
        // アサーション！
        // 要素は 1 が unde 回足されるため、mat2[ind1] が unde になることが一瞬で証明されます
        assert(mat2[ind1 as int] == unde as int);
    }
}