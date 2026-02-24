use vstd::prelude::*;

verus! {
    // 1. 1次元配列を指定された値 val で初期化する
    fn init_row(n: usize, val: i32) -> (res: Vec<i32>)
        // 【修正】安全のため上限を 1000 に
        requires
            n <= 1000,
        ensures
            res@.len() == n as int,
            forall|j: int| #![auto] 0 <= j && j < n ==> res@[j] == val,
    {
        let mut vec = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                n <= 1000,
                vec@.len() == i as int,
                forall|j: int| #![auto] 0 <= j && j < i ==> vec@[j] == val,
            decreases n - i
        {
            vec.push(val);
            i = i + 1;
        }
        vec
    }

    // 2. 行列を指定された値 val で初期化する
    fn init_matrix(k: usize, n: usize, val: i32, matrix: &mut Vec<Vec<i32>>)
        requires
            old(matrix)@.len() == 0,
            k <= 1000,
            n <= 1000,
        ensures
            matrix@.len() == k as int,
            forall|x: int| #![auto] 0 <= x && x < k ==> matrix@[x]@.len() == n as int,
            forall|x: int, y: int| #![auto] 0 <= x && x < k && 0 <= y && y < n ==> matrix@[x]@[y] == val,
    {
        let mut i = 0;
        while i < k
            invariant
                i <= k,
                k <= 1000,
                n <= 1000,
                matrix@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> matrix@[x]@.len() == n as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < n ==> matrix@[x]@[y] == val,
            decreases k - i
        {
            matrix.push(init_row(n, val));
            i = i + 1;
        }
    }

    // 3. 1次元配列の要素の和を求める
    fn sum_array(n: usize, p: &Vec<i32>) -> (res: i32)
        requires
            p@.len() == n as int,
            n <= 1000,
            // 【修正】要素の上限も 1000 に
            forall|j: int| #![auto] 0 <= j && j < n ==> 0 <= p@[j] && p@[j] <= 1000,
        ensures
            res >= 0,
            res <= 1000000, // 1000要素 × 最大1000 = 最大 1,000,000 になる
    {
        let mut sum: i32 = 0;
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                n <= 1000,
                p@.len() == n as int,
                forall|j: int| #![auto] 0 <= j && j < n ==> 0 <= p@[j] && p@[j] <= 1000,
                sum >= 0,
                // i回足したときの最大値は i * 1000
                sum as int <= (i as int) * 1000, 
            decreases n - i
        {
            sum = sum + p[i];
            i = i + 1;
        }
        sum
    }

    // 4. 行列の要素の和を求める
    fn sum_matrix(n1: usize, n2: usize, q: &Vec<Vec<i32>>) -> (res: i32)
        requires
            q@.len() == n2 as int,
            n1 <= 1000,
            n2 <= 1000,
            forall|x: int| #![auto] 0 <= x && x < n2 ==> q@[x]@.len() == n1 as int,
            forall|x: int, y: int| #![auto] 0 <= x && x < n2 && 0 <= y && y < n1 ==> 
                0 <= q@[x]@[y] && q@[x]@[y] <= 1000,
        ensures
            res >= 0,
    {
        let mut sum: i32 = 0;
        let mut i = 0;
        while i < n2
            invariant
                i <= n2,
                n1 <= 1000,
                n2 <= 1000,
                q@.len() == n2 as int,
                forall|x: int| #![auto] 0 <= x && x < n2 ==> q@[x]@.len() == n1 as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < n2 && 0 <= y && y < n1 ==> 
                    0 <= q@[x]@[y] && q@[x]@[y] <= 1000,
                sum >= 0,
                // i行足したときの最大値は i * 1,000,000 (最大10億なのでi32に収まる)
                sum as int <= (i as int) * 1000000, 
            decreases n2 - i
        {
            let s = sum_array(n1, &q[i]);
            sum = sum + s;
            i = i + 1;
        }
        sum
    }

    // 5. メインの検証関数
    fn main_verify(undet: usize)
        requires
            undet > 0,
            undet <= 1000, // ここも合わせて 1000 に
    {
        let mut mat1: Vec<Vec<i32>> = Vec::new();
        let one: i32 = 1;

        init_matrix(undet, undet, one, &mut mat1);
        let d2 = sum_matrix(undet, undet, &mat1);

        assert(d2 >= 0);
    }
}