use vstd::prelude::*;

verus! {
    // 1. 配列を指定された値 x で初期化する関数
    fn init(n: usize, x: i32, p: &mut Vec<i32>)
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

    // 2. 読み出した値をそのまま書き戻す関数（ブーメラン）
    fn boomerang(b2: usize, r: &mut Vec<i32>)
        requires
            old(r)@.len() == b2 as int,
        ensures
            r@.len() == b2 as int,
            // 【重要】関数が終わった後、すべての要素が「関数に入る前 (old(r))」と同じであること
            forall|i: int| #![auto] 0 <= i && i < b2 ==> r@[i] == old(r)@[i],
    {
        let mut i = 0;
        while i < b2
            invariant
                i <= b2,
                r@.len() == b2 as int,
                // 【重要】ループの途中でも、常に元の配列と同じ状態を保っていること
                forall|j: int| #![auto] 0 <= j && j < b2 ==> r@[j] == old(r)@[j],
            decreases b2 - i
        {
            let t = r[i];     // 値を読み出す
            r.set(i, t);      // そのまま書き戻す（Verusの安全な配列更新メソッド）
            i = i + 1;
        }
    }

    // 3. 配列を読み出して舐めるだけの関数（verify）
    // 擬似コードにアサートがないため、単に全要素をリードする処理として表現します
    fn verify_array(a: usize, p: &Vec<i32>)
        requires
            p@.len() == a as int,
    {
        let mut i = 0;
        while i < a
            invariant
                i <= a,
                p@.len() == a as int,
            decreases a - i
        {
            let y = p[i]; // 読み出し
            i = i + 1;
        }
    }

    // 4. メインの検証関数
    fn main_verify() {
        let mut kp: Vec<i32> = Vec::new();
        let ten: usize = 10;
        let zero: i32 = 0;

        // 1. 初期化
        init(ten, zero, &mut kp);
        assert(kp[0] == 0); // 初期化直後なので当然 0

        // 2. ブーメランを実行！
        boomerang(ten, &mut kp);
        
        // 3. ブーメラン後も値が保たれていることをアサート！
        // boomerang 関数の ensures のおかげで、一瞬で証明されます。
        assert(kp[0] == 0);

        // 4. 最後に verify_array を呼び出す
        verify_array(ten, &kp);
    }
}