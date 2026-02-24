use vstd::prelude::*;

verus! {
    // 1. 初期化関数
    // 修正ポイント：引数を &mut [i32] から &mut Vec<i32> に変更しました
    fn init_array(vec: &mut Vec<i32>) -> (res: i32)
        ensures
            vec@.len() == old(vec)@.len(),
            forall|i: int| #![auto] 0 <= i && i < vec@.len() ==> vec@[i] == 0,
            res == 1,
    {
        let mut idx = 0;
        while idx < vec.len()
            invariant
                idx <= vec@.len(),
                vec@.len() == old(vec)@.len(),
                forall|i: int| #![auto] 0 <= i && i < idx ==> vec@[i] == 0,
            decreases vec@.len() - idx
        {
            vec[idx] = 0;
            idx = idx + 1;
        }
        1
    }

    // 2. 検証関数
    // 修正ポイント：こちらも念のため &Vec<i32> に変更しています
    fn verify_array(vec: &Vec<i32>) -> (res: i32)
        requires
            forall|i: int| #![auto] 0 <= i && i < vec@.len() ==> vec@[i] == 0,
        ensures
            res == 1,
    {
        let mut idx = 0;
        while idx < vec.len()
            invariant
                idx <= vec@.len(),
                forall|i: int| #![auto] 0 <= i && i < vec@.len() ==> vec@[i] == 0,
            decreases vec@.len() - idx
        {
            let y = vec[idx];
            assert(y == 0);
            idx = idx + 1;
        }
        1
    }

    // 3. メイン関数
    fn main_verify() -> (res: i32)
        ensures
            res == 1,
    {
        let mut p: Vec<i32> = Vec::new();
        let mut i = 0;
        while i < 1000
            invariant
                i <= 1000,
                p@.len() == i as int,
            decreases 1000 - i
        {
            p.push(-1); 
            i = i + 1;
        }

        // ここで暗黙の型変換が発生せず、純粋な参照渡しになるため Verus が納得します！
        let d = init_array(&mut p);
        let d2 = verify_array(&p);

        1
    }
}