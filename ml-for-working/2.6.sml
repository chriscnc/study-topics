
type time = int * int * string

fun precedes ((h1:int,m1:int,ap1:string),(h2:int,m2:int,ap2:string))  =
  (ap1 < ap2) orelse
  (h1 < h2) orelse
  (m1 < m2)
