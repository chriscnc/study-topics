
fun upto(m,n) = 
  if m > n then [] 
  else m :: upto(m + 1, n);

upto(2,5);

fun prodof3 [i,j,k] : int = i * j * k;

prodof3 [1,2,3];

fun prodof3t (i,j,k) : int = i * j * k;

prodof3t (1,2,3);

fun prod [] = 1
  | prod (n::ns) = n * (prod ns);

prod [1,2,3,4];

fun maxl [m] : int = m
  | maxl (m::n::ns) = if m > n then maxl(m::ns)
                               else maxl(n::ns);

maxl [7,1,10];

fun factl (n) = prod(upto(1, n));

factl 7;

fun null [] = true
  | null (_::_) = false;

null [];
null [2];

fun hd (n::_) = n;

hd [1,2];

fun tl (_::ns) = ns;

tl [1,2];

fun maxl lst : int = 
  if null lst then ~1
  else if null (tl lst) then hd lst
       else let val f = (hd lst)
                val s = (hd (tl lst))
                val r = (tl (tl lst))
            in if f > s then maxl(f::r) else maxl(s::r)
            end;
                    
maxl [1,5,3,2];

fun lastel [x] = x
  | lastel (_::xs) = lastel xs;

lastel [1,5,3,2];
                   
fun nlength [] = 0
  | nlength (x::xs) = 1 + nlength xs;

nlength [[1,2,3], [4,5,6]];

local
  fun addlen (n, []) = n
    | addlen (n, x::l) = addlen(n+1, l)
in
  fun length l = addlen (0,l)
end;

length (explode "Throw physic to the dogs!");




