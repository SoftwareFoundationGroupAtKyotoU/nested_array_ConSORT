use vstd::prelude::*;

verus! {
    // 1. 行列の初期化関数群
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

    // 使いやすいように直接 Vec を返すようにしています
    fn init_matrix(l: usize, k: usize) -> (res: Vec<Vec<i32>>)
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

    // 2. 1次元配列の加算（今回は入力が 1 であることを要求します）
    fn add_array(n: usize, p: &Vec<i32>, q: &Vec<i32>) -> (r: Vec<i32>)
        requires
            p@.len() == n as int,
            q@.len() == n as int,
            // 擬似コードのアサートに合わせるため、入力がすべて 1 であることを前提とします
            forall|j: int| #![auto] 0 <= j && j < n ==> p@[j] == 1,
            forall|j: int| #![auto] 0 <= j && j < n ==> q@[j] == 1,
        ensures
            r@.len() == n as int,
            forall|j: int| #![auto] 0 <= j && j < n ==> r@[j] == 2,
    {
        let mut r = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                p@.len() == n as int,
                q@.len() == n as int,
                forall|j: int| #![auto] 0 <= j && j < n ==> p@[j] == 1,
                forall|j: int| #![auto] 0 <= j && j < n ==> q@[j] == 1,
                r@.len() == i as int,
                forall|j: int| #![auto] 0 <= j && j < i ==> r@[j] == 2,
            decreases n - i
        {
            // 1 + 1 なのでオーバーフローの心配はありません！
            r.push(p[i] + q[i]);
            i = i + 1;
        }
        r
    }

    // 3. 2次元行列の加算
    fn add_matrix(n1: usize, n2: usize, pp: &Vec<Vec<i32>>, qq: &Vec<Vec<i32>>, rr: &mut Vec<Vec<i32>>)
        requires
            old(rr)@.len() == 0,
            pp@.len() == n2 as int,
            qq@.len() == n2 as int,
            forall|x: int| #![auto] 0 <= x && x < n2 ==> pp@[x]@.len() == n1 as int,
            forall|x: int| #![auto] 0 <= x && x < n2 ==> qq@[x]@.len() == n1 as int,
            // pp と qq がすべて 1 であることを要求
            forall|x: int, y: int| #![auto] 0 <= x && x < n2 && 0 <= y && y < n1 ==> pp@[x]@[y] == 1,
            forall|x: int, y: int| #![auto] 0 <= x && x < n2 && 0 <= y && y < n1 ==> qq@[x]@[y] == 1,
        ensures
            rr@.len() == n2 as int,
            forall|x: int| #![auto] 0 <= x && x < n2 ==> rr@[x]@.len() == n1 as int,
            forall|x: int, y: int| #![auto] 0 <= x && x < n2 && 0 <= y && y < n1 ==> rr@[x]@[y] == 2,
    {
        let mut i = 0;
        while i < n2
            invariant
                i <= n2,
                pp@.len() == n2 as int,
                qq@.len() == n2 as int,
                forall|x: int| #![auto] 0 <= x && x < n2 ==> pp@[x]@.len() == n1 as int,
                forall|x: int| #![auto] 0 <= x && x < n2 ==> qq@[x]@.len() == n1 as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < n2 && 0 <= y && y < n1 ==> pp@[x]@[y] == 1,
                forall|x: int, y: int| #![auto] 0 <= x && x < n2 && 0 <= y && y < n1 ==> qq@[x]@[y] == 1,
                rr@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> rr@[x]@.len() == n1 as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < n1 ==> rr@[x]@[y] == 2,
            decreases n2 - i
        {
            let z = add_array(n1, &pp[i], &qq[i]);
            rr.push(z);
            i = i + 1;
        }

        // 擬似コードの「addmatrix内のアサート」を Verus で表現
        // ループ終了時点で rr はすべて 2 になっているので、当然アサートは成功します
        assert(forall|in2: int, in1: int| #![auto] 0 <= in2 && in2 < n2 && 0 <= in1 && in1 < n1 ==> pp@[in2]@[in1] == 1);
        assert(forall|in2: int, in1: int| #![auto] 0 <= in2 && in2 < n2 && 0 <= in1 && in1 < n1 ==> qq@[in2]@[in1] == 1);
        assert(forall|in2: int, in1: int| #![auto] 0 <= in2 && in2 < n2 && 0 <= in1 && in1 < n1 ==> rr@[in2]@[in1] == 2);
    }

    // 4. メインの検証関数
    fn main_verify(undet: usize, ind1: usize, ind2: usize)
        requires
            undet > 0,
            ind1 < undet,
            ind2 < undet,
    {
        // let mat1 = alloc undet : {v:int| (v = 1)} に相当
        let mat1 = init_matrix(undet, undet);
        
        // 【超重要】let immut mat2 = mat1 + 0 に相当
        // Rustでは「不変参照(&)」をとることで、安全にエイリアスを作成できます
        let mat2: &Vec<Vec<i32>> = &mat1;
        
        let mut mat3: Vec<Vec<i32>> = Vec::new();

        // 同じ行列に対する2つの参照(&mat1 と mat2)を同時に渡す！
        // 両方とも「読み取り専用」なので、RustのボローチェッカーもVerusもこれを許可します。
        add_matrix(undet, undet, &mat1, mat2, &mut mat3);

        // 結果のアサート
        assert(mat3[ind1 as int][ind2 as int] == 2);
    }
}