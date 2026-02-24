use vstd::prelude::*;

verus! {
    fn init_row(n: usize, val: usize) -> (res: Vec<usize>)
        ensures
            res@.len() == n as int,
            forall|j: int| #![auto] 0 <= j && j < n ==> res@[j] == val,
    {
        let mut vec = Vec::new();
        let mut i = 0;
        while i < n
            invariant
                i <= n,
                vec@.len() == i as int,
                forall|j: int| #![auto] 0 <= j && j < i ==> vec@[j] == val,
            decreases n - i
        {
            vec.push(val);
            i = i + 1;
        }
        vec
    }

    fn easy_matrix(l: usize, k: usize, matrix: &mut Vec<Vec<usize>>)
        requires
            old(matrix)@.len() == 0,
        ensures
            matrix@.len() == k as int,
            forall|x: int| #![auto] 0 <= x && x < k ==> matrix@[x]@.len() == l as int,
            forall|x: int, y: int| #![auto] 0 <= x && x < k && 0 <= y && y < l ==> 
                matrix@[x]@[y] as int == k as int - x,
    {
        let mut i = 0;
        while i < k
            invariant
                i <= k,
                matrix@.len() == i as int,
                forall|x: int| #![auto] 0 <= x && x < i ==> matrix@[x]@.len() == l as int,
                forall|x: int, y: int| #![auto] 0 <= x && x < i && 0 <= y && y < l ==> 
                    matrix@[x]@[y] as int == k as int - x,
            decreases k - i
        {
            let row = init_row(l, k - i);
            matrix.push(row);
            i = i + 1;
        }
    }

    fn main_verify(ind1: usize, ind2: usize)
        requires
            ind1 < 10,
            ind2 < 10,
    {
        let mut pp: Vec<Vec<usize>> = Vec::new();
        
        easy_matrix(10, 10, &mut pp);

        assert(pp[ind2 as int][ind1 as int] as int == 10 - ind2 as int);
    }
}