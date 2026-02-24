use vstd::prelude::*;

verus! {
    // 1. 絶対値を求める関数
    fn abs_val(m: i32) -> (res: i32)
        requires
            // i32の最小値(-2147483648)の符号を反転するとオーバーフローするため、範囲を制限します
            -1000000000 <= m && m <= 1000000000,
        ensures
            res >= 0,
            res == m || res == -m,
    {
        if m >= 0 {
            m
        } else {
            -m
        }
    }

    // 2. 配列を指定された値 x で初期化する関数 (initx)
    fn initx(n: usize, x: i32, p: &mut Vec<i32>)
        requires
            old(p)@.len() == 0,
        ensures
            p@.len() == n as int,
            forall|i: int| #![auto] 0 <= i && i < n ==> p@[i] == x,
    {
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                p@.len() == i as int,
                forall|j: int| #![auto] 0 <= j && j < i ==> p@[j] == x,
            decreases n - i
        {
            p.push(x);
            i = i + 1;
        }
    }

    // 3. 1次元配列同士を加算する関数 (addarray)
    fn addarray(n: usize, p: &Vec<i32>, q: &Vec<i32>, r: &mut Vec<i32>)
        requires
            old(r)@.len() == 0,
            p@.len() == n as int,
            q@.len() == n as int,
            // 【重要】オーバーフローを防ぐための契約
            forall|j: int| #![auto] 0 <= j && j < n ==> -1000000000 <= p@[j] && p@[j] <= 1000000000,
            forall|j: int| #![auto] 0 <= j && j < n ==> -1000000000 <= q@[j] && q@[j] <= 1000000000,
        ensures
            r@.len() == n as int,
            forall|j: int| #![auto] 0 <= j && j < n ==> r@[j] == p@[j] + q@[j],
    {
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                p@.len() == n as int,
                q@.len() == n as int,
                r@.len() == i as int,
                forall|j: int| #![auto] 0 <= j && j < n ==> -1000000000 <= p@[j] && p@[j] <= 1000000000,
                forall|j: int| #![auto] 0 <= j && j < n ==> -1000000000 <= q@[j] && q@[j] <= 1000000000,
                forall|j: int| #![auto] 0 <= j && j < i ==> r@[j] == p@[j] + q@[j],
            decreases n - i
        {
            r.push(p[i] + q[i]);
            i = i + 1;
        }
    }

    // 4. 検証関数 (verify)
    fn verify_array(n: usize, p: &Vec<i32>)
        requires
            p@.len() == n as int,
            // 呼び出し元に対して「中身が 0 以上であること」を要求します
            forall|j: int| #![auto] 0 <= j && j < n ==> p@[j] >= 0,
    {
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                p@.len() == n as int,
                forall|j: int| #![auto] 0 <= j && j < n ==> p@[j] >= 0,
            decreases n - i
        {
            let y = p[i];
            // 事前条件のおかげで、このアサートは100%成功します
            assert(y >= 0);
            i = i + 1;
        }
    }

    // 5. メイン関数
    fn main_verify(rand: i32)
        requires
            // 任意のランダム値ですが、i32のオーバーフローを防ぐために常識的な範囲に制限します
            -1000000000 <= rand && rand <= 1000000000,
    {
        let m: usize = 1000;
        
        let mut p: Vec<i32> = Vec::new();
        let mut q: Vec<i32> = Vec::new();
        let mut r: Vec<i32> = Vec::new();

        // 1. ランダム値の絶対値を取得 (z は必ず 0 以上 1000000000 以下になる)
        let z = abs_val(rand);

        // 2. 配列 p と q を z で初期化
        initx(m, z, &mut p);
        initx(m, z, &mut q);
        
        // 3. 配列を加算して r に格納
        // Verus は p と q が z (10億以下) であることを知っているため、addarray の条件を自動クリア！
        addarray(m, &p, &q, &mut r);

        // 4. 結果の検証
        // Verus は「r は z + z である」「z >= 0 である」ことを知っているので、
        // 「r は 0 以上である」という verify_array の要求を自動的に理解します！
        verify_array(m, &r);
    }
}