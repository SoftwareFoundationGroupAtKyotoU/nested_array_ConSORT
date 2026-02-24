use vstd::prelude::*;

verus! {
    // 1. 1次元配列を 0 で初期化する関数
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

    // 2. 行列を 0 で初期化する関数
    fn init_matrix_zero(n: usize) -> (res: Vec<Vec<i32>>)
        requires n <= 1000,
        ensures
            res@.len() == n as int,
            forall|x: int| #![auto] 0 <= x && x < n ==> res@[x]@.len() == n as int,
            forall|x: int, y: int| #![auto] 0 <= x && x < n && 0 <= y && y < n ==> res@[x]@[y] == 0,
    {
        let mut matrix: Vec<Vec<i32>> = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                n <= 1000,
                matrix@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> matrix@[x]@.len() == n as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < n ==> matrix@[x]@[y] == 0,
            decreases n - i
        {
            matrix.push(init_row_zero(n));
            i = i + 1;
        }
        matrix
    }

    // 3. 行列のトレースを「左上から右下へ（前から）」計算する関数
    fn trace_forward(n: usize, q: &Vec<Vec<i32>>) -> (res: i32)
        requires
            q@.len() == n as int,
            n <= 1000,
            forall|i: int| #![auto] 0 <= i && i < n ==> q@[i]@.len() == n as int,
            // 【前提条件】行列の要素はすべて 0 であること
            forall|i: int, j: int| #![auto] 0 <= i && i < n && 0 <= j && j < n ==> q@[i]@[j] == 0,
        ensures
            // すべて 0 を足すので、結果も必ず 0 になる
            res == 0,
    {
        let mut sum: i32 = 0;
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                n <= 1000,
                q@.len() == n as int,
                forall|x: int| #![auto] 0 <= x && x < n ==> q@[x]@.len() == n as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < n && 0 <= y && y < n ==> q@[x]@[y] == 0,
                // 0 を何回足しても 0
                sum == 0,
            decreases n - i
        {
            // 擬似コードの let m = n1 - n2 に相当 (i が増えていく)
            let s = q[i][i];
            
            // 擬似コードの assert(s>=0) を再現
            assert(s >= 0); 
            
            sum = sum + s;
            i = i + 1;
        }
        sum
    }

    // 4. 行列のトレースを「右下から左上へ（後ろから）」計算する関数
    fn trace_backward(n: usize, q: &Vec<Vec<i32>>) -> (res: i32)
        requires
            q@.len() == n as int,
            n <= 1000,
            forall|i: int| #![auto] 0 <= i && i < n ==> q@[i]@.len() == n as int,
            // 【前提条件】行列の要素はすべて 0 であること
            forall|i: int, j: int| #![auto] 0 <= i && i < n && 0 <= j && j < n ==> q@[i]@[j] == 0,
        ensures
            // 後ろから足しても、すべて 0 なので結果は必ず 0 になる
            res == 0,
    {
        let mut sum: i32 = 0;
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                n <= 1000,
                q@.len() == n as int,
                forall|x: int| #![auto] 0 <= x && x < n ==> q@[x]@.len() == n as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < n && 0 <= y && y < n ==> q@[x]@[y] == 0,
                // 0 を何回足しても 0
                sum == 0,
            decreases n - i
        {
            // 擬似コードの let shift = n2 - 1 に相当 (右下から拾っていく)
            let shift = n - 1 - i;
            let s = q[shift][shift];
            
            // 擬似コードの assert(s>=0) を再現
            assert(s >= 0);
            
            sum = sum + s;
            i = i + 1;
        }
        sum
    }

    // 5. メインの検証関数
    fn main_verify(ten: usize)
        requires
            ten > 0,
            ten <= 1000,
    {
        // 1. mat1 を alloc ten : {v:int | (v = 0)} に従って初期化
        let mat1 = init_matrix_zero(ten);
        
        // 2. 読み取り専用のエイリアス (immut mat2 = mat1 + 0) を不変参照で表現
        let mat2: &Vec<Vec<i32>> = &mat1;
        
        // 3. 前からのトレースと、後ろからのトレースを計算
        let d1 = trace_forward(ten, &mat1);
        let d2 = trace_backward(ten, mat2);
        
        // 4. アサーション！
        // Verusは「d1 == 0」かつ「d2 == 0」であることを知っているので、
        // d1 == d2 であることを瞬時に数学的に証明します！
        assert(d1 == d2);
    }
}