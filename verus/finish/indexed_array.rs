use vstd::prelude::*;

verus! {
    fn init_row(n: usize) -> (res: Vec<i32>)
        ensures
            res@.len() == n as int,
            forall|j: int| 0 <= j && j < n ==> res@[j] == 1,
    {
        let mut vec = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                vec@.len() == i as int,
                forall|j: int| 0 <= j && j < i ==> vec@[j] == 1,
            decreases n - i
        {
            vec.push(1);
            i = i + 1;
        }
        vec
    }

    fn easy_matrix(k: usize, matrix: &mut Vec<Vec<i32>>)
        requires
            old(matrix)@.len() == 0,
        ensures
            matrix@.len() == k as int,
            forall|x: int| 0 <= x && x < k ==> matrix@[x]@.len() == k as int - x,
            forall|x: int, y: int| 0 <= x && x < k && 0 <= y && y < (k  - x) ==> matrix@[x]@[y] == 1,
    {
        let mut i = 0;
        while i < k
            invariant
                i <= k,
                matrix@.len() == i as int,
                forall|x: int| 0 <= x && x < i ==> matrix@[x]@.len() == k - x,
                forall|x: int, y: int| 0 <= x && x < i && 0 <= y && y < k  - x ==> matrix@[x]@[y] == 1,
            decreases k - i
        {
            let row = init_row(k - i);
            matrix.push(row);
            i = i + 1;
        }
    }

    fn main_verify(ind1: usize)
        requires
            ind1 < 10,
    {
        let mut p: Vec<Vec<i32>> = Vec::new();
        easy_matrix(10, &mut p);
        assert(p[ind1 as int][9 - ind1] == 1);
    }
}