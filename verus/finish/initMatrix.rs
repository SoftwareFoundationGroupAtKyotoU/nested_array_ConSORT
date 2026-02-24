use vstd::prelude::*;

verus! {
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

    fn init_matrix(l: usize, k: usize, matrix: &mut Vec<Vec<i32>>)
        requires
            old(matrix)@.len() == 0,
        ensures
            matrix@.len() == k as int,
            forall|x: int| #![auto] 0 <= x && x < k ==> matrix@[x]@.len() == l as int,
            forall|x: int, y: int| #![auto] 0 <= x && x < k && 0 <= y && y < l ==> matrix@[x]@[y] == 1,
    {
        let mut i = 0;
        while i < k
            invariant
                i <= k,
                matrix@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> matrix@[x]@.len() == l as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < l ==> matrix@[x]@[y] == 1,
            decreases k - i
        {
            let row = init_row(l);
            matrix.push(row);
            i = i + 1;
        }
    }

    fn main_verify(x: usize, ind1: usize, ind2: usize) -> (res: i32)
        requires
            x > 0,
            ind1 < x,
            ind2 < x,
        ensures
            res == 1,
    {
        let mut p: Vec<Vec<i32>> = Vec::new();
        
        init_matrix(x, x, &mut p);
        assert(p[ind1 as int][ind2 as int] == 1);
        
        1
    }
}