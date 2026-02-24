use vstd::prelude::*;

verus! {
    // 1. 1次元（線）
    fn init_row(n: usize) -> (res: Vec<i32>)
        ensures
            res@.len() == n as int,
            forall|x: int| #![auto] 0 <= x && x < n ==> res@[x] == 1,
    {
        let mut vec = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                vec@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> vec@[x] == 1,
            decreases n - i
        {
            vec.push(1);
            i = i + 1;
        }
        vec
    }

    // 2. 2次元（面）
    fn init_matrix(k: usize) -> (res: Vec<Vec<i32>>)
        ensures
            res@.len() == k as int,
            forall|x: int| #![auto] 0 <= x && x < k ==> res@[x]@.len() == k as int - x,
            forall|x: int, y: int| #![auto] 0 <= x && x < k && 0 <= y && y < (k as int - x) ==> res@[x]@[y] == 1,
    {
        let mut matrix: Vec<Vec<i32>> = Vec::new(); // 型アノテーション
        let mut i = 0;
        while i < k
            invariant
                i <= k,
                matrix@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> matrix@[x]@.len() == k as int - x,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < (k as int - x) ==> matrix@[x]@[y] == 1,
            decreases k - i
        {
            matrix.push(init_row(k - i));
            i = i + 1;
        }
        matrix
    }

    // 3. 3次元（立体）- 4次元から呼び出しやすくするため、直接 Vec を返すように設計変更
    fn init_tensor(m: usize) -> (res: Vec<Vec<Vec<i32>>>)
        ensures
            res@.len() == m as int,
            forall|x: int| #![auto] 0 <= x && x < m ==> res@[x]@.len() == m as int - x,
            forall|x: int, y: int| #![auto] 0 <= x && x < m && 0 <= y && y < (m as int - x) ==> res@[x]@[y]@.len() == m as int - x - y,
            forall|x: int, y: int, z: int| #![auto] 
                0 <= x && x < m && 
                0 <= y && y < (m as int - x) && 
                0 <= z && z < (m as int - x - y) ==> res@[x]@[y]@[z] == 1,
    {
        let mut tensor: Vec<Vec<Vec<i32>>> = Vec::new(); // 型アノテーション
        let mut i = 0;
        while i < m
            invariant
                i <= m,
                tensor@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> tensor@[x]@.len() == m as int - x,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < (m as int - x) ==> tensor@[x]@[y]@.len() == m as int - x - y,
                forall|x: int, y: int, z: int| #![auto] 
                    0 <= x && x < i && 
                    0 <= y && y < (m as int - x) && 
                    0 <= z && z < (m as int - x - y) ==> tensor@[x]@[y]@[z] == 1,
            decreases m - i
        {
            tensor.push(init_matrix(m - i));
            i = i + 1;
        }
        tensor
    }

    // 4. 4次元（ハイパーテンソル）- 擬似コードの iFm に相当
    fn init_hypertensor(j: usize, pppp: &mut Vec<Vec<Vec<Vec<i32>>>>)
        requires
            old(pppp)@.len() == 0,
        ensures
            pppp@.len() == j as int,
            forall|x: int| #![auto] 0 <= x && x < j ==> pppp@[x]@.len() == j as int - x,
            forall|x: int, y: int| #![auto] 0 <= x && x < j && 0 <= y && y < (j as int - x) ==> pppp@[x]@[y]@.len() == j as int - x - y,
            forall|x: int, y: int, z: int| #![auto] 
                0 <= x && x < j && 
                0 <= y && y < (j as int - x) && 
                0 <= z && z < (j as int - x - y) ==> pppp@[x]@[y]@[z]@.len() == j as int - x - y - z,
            // 4変数のforall！すべての要素が1であることを証明
            forall|x: int, y: int, z: int, w: int| #![auto] 
                0 <= x && x < j && 
                0 <= y && y < (j as int - x) && 
                0 <= z && z < (j as int - x - y) &&
                0 <= w && w < (j as int - x - y - z) ==> pppp@[x]@[y]@[z]@[w] == 1,
    {
        let mut i = 0;
        while i < j
            invariant
                i <= j,
                pppp@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> pppp@[x]@.len() == j as int - x,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < (j as int - x) ==> pppp@[x]@[y]@.len() == j as int - x - y,
                forall|x: int, y: int, z: int| #![auto] 
                    0 <= x && x < i && 
                    0 <= y && y < (j as int - x) && 
                    0 <= z && z < (j as int - x - y) ==> pppp@[x]@[y]@[z]@.len() == j as int - x - y - z,
                forall|x: int, y: int, z: int, w: int| #![auto] 
                    0 <= x && x < i && 
                    0 <= y && y < (j as int - x) && 
                    0 <= z && z < (j as int - x - y) &&
                    0 <= w && w < (j as int - x - y - z) ==> pppp@[x]@[y]@[z]@[w] == 1,
            decreases j - i
        {
            // 次の次元である3次元配列を作成してpush
            pppp.push(init_tensor(j - i));
            i = i + 1;
        }
    }

    // 5. 検証用メイン関数
    fn main_verify(ind1: usize, ind2: usize, ind3: usize, ind4: usize)
        requires
            ind1 < 10,
            ind2 < 10 - ind1,
            ind3 < 10 - ind1 - ind2,
            ind4 < 10 - ind1 - ind2 - ind3,
    {
        let mut p: Vec<Vec<Vec<Vec<i32>>>> = Vec::new();
        
        init_hypertensor(10, &mut p);

        // 4次元のインデックスを使ってアサート！
        assert(p[ind1 as int][ind2 as int][ind3 as int][ind4 as int] == 1);
    }
}