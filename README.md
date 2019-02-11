# sudoku

My first Haskell program!

The algorithm of the "rule of eights" is my own invention.

The "rule of eights" alone does not solve all sudokus, so I added branching when the logic alone reaches a dead end.

*February 2019*

Most sudokus solve in under a second. This one takes the longest to solve (of the difficult sudokus that I've tried), the "hardest sudoku in the world"...

```
$ time stack exec sudoku-exe
puzzle:
8    |     |     
    3|6    |     
  7  |  9  |2    
-----+-----+-----
  5  |    7|     
     |  4 5|7    
     |1    |  3  
-----+-----+-----
    1|     |  6 8
    8|5    |  1  
  9  |     |4    
1 solution in 11906 misses with 6431 hits and 87 deadends
solution 1:
8 1 2|7 5 3|6 4 9
9 4 3|6 8 2|1 7 5
6 7 5|4 9 1|2 8 3
-----+-----+-----
1 5 4|2 3 7|8 9 6
3 6 9|8 4 5|7 2 1
2 8 7|1 6 9|5 3 4
-----+-----+-----
5 2 1|9 7 4|3 6 8
4 3 8|5 2 6|9 1 7
7 9 6|3 1 8|4 5 2

real    0m8.163s
user    0m15.509s
sys     0m3.247s
```

