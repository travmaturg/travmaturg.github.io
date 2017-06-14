open Num

type box = {x1: num; x2: num; y1: num; y2: num}

type corn =
{
  x1y1: (num * num) list; x1y2: (num * num) list;
  x2y1: (num * num) list; x2y2: (num * num) list
}

let nmax = int_of_string Sys.argv.(1);;

let write blks fn =
  let f = open_out fn in
  let out x = output_string f x in
  let line x1 x2 y1 y2 =
    out x1; out y1; out x2; out y2;
    out "newpath moveto lineto stroke\n"
  in
  let box_write b =
    let n2s x = (string_of_float (float_of_num x)) ^ " " in
    let sx1 = n2s b.x1 and sx2 = n2s b.x2
    and sy1 = n2s b.y1 and sy2 = n2s b.y2
    in
    line sx1 sx1 sy1 sy2; line sx2 sx2 sy1 sy2;
    line sx1 sx2 sy1 sy1; line sx1 sx2 sy2 sy2;
    line sx1 sx2 sy1 sy2; line sx1 sx2 sy2 sy1
  in
  out "%!\n[450 0 0 450 70 300] concat 0 setlinewidth\n";
  out "0 0 1 0 newpath moveto lineto stroke\n";
  out "0 0 0 1 newpath moveto lineto stroke\n";
  out "0 1 1 1 newpath moveto lineto stroke\n";
  out "1 0 1 1 newpath moveto lineto stroke\n";
  List.iter box_write blks;
  out "showpage";;

let intersect b1 b2 =
  (b1.x1 </ b2.x2) && (b1.x2 >/ b2.x1) && (b1.y1 </ b2.y2) && (b1.y2 >/ b2.y1)

let b_eq b1 b2 =
  (b1.x1 =/ b2.x1) && (b1.x2 =/ b2.x2) && (b1.y1 =/ b2.y1) && (b1.y2 =/ b2.y2)

let p0 = Int 0 and p1 = Int 1 and p12 = Int 1 // Int 2
and p13 = Int 1 // Int 3 and p23 = Int 2 // Int 3 and p34 = Int 3 // Int 4

let num_found = ref 1

let rec fill b_done c n =
  if n > !num_found then (
    num_found := n;
    let s = "searching " ^ (string_of_int n) ^ " of " ^
      (string_of_int nmax) ^ "...\n"
    in
    print_string s; flush_all ()
  );
  let rec fill_one bl =
    match bl with
      | [] -> raise Exit
      | bl1 :: bl2 -> (
	  try
	    let new_corn =
	      let inside x y1 y2 = x >/ y1 && x </ y2 in
	      let cx1y1 =
		let cl =
		  let out (x, y) = not (x =/ bl1.x1 && y =/ bl1.y1) in
		  ref (List.filter out c.x1y1)
		in
		let add (cx, cy) =
		  let b_corn b = b.x1 =/ cx && b.y1 =/ cy in
		  let c_eq (x, y) = x =/ cx && y =/ cy in
		  if not ((List.exists b_corn b_done) ||
		    (List.exists c_eq !cl))
		  then cl := (cx, cy) :: !cl
		in
		if bl1.x1 =/ p0 && bl1.y2 </ p1 then add (p0, bl1.y2);
		if bl1.x2 </ p1 && bl1.y1 =/ p0 then add (bl1.x2, p0);
		let f b =
		  let g b1 b2 =
		    if
		      (b1.x1 =/ b2.x2 && inside b1.y2 b2.y1 b2.y2) ||
			(b1.y2 =/ b2.y1 && inside b2.x2 b1.x1 b1.x2)
		    then add (b2.x2, b1.y2)
		  in
		  g b bl1; g bl1 b
		in
		List.iter f b_done;
		!cl
	      in
	      let cx1y2 =
		let cl =
		  let out (x, y) = not (x =/ bl1.x1 && y =/ bl1.y2) in
		  ref (List.filter out c.x1y2)
		in
		let add (cx, cy) =
		  let b_corn b = b.x1 =/ cx && b.y2 =/ cy in
		  let c_eq (x, y) = x =/ cx && y =/ cy in
		  if not ((List.exists b_corn b_done) ||
		    (List.exists c_eq !cl))
		  then cl := (cx, cy) :: !cl
		in
		if bl1.x1 =/ p0 && bl1.y1 >/ p0 then add (p0, bl1.y1);
		if bl1.x2 </ p1 && bl1.y2 =/ p1 then add (bl1.x2, p1);
		let f b =
		  let g b1 b2 =
		    if
		      (b1.x1 =/ b2.x2 && inside b1.y1 b2.y1 b2.y2) ||
			(b1.y1 =/ b2.y2 && inside b2.x2 b1.x1 b1.x2)
		    then add (b2.x2, b1.y1)
		  in
		  g b bl1; g bl1 b
		in
		List.iter f b_done;
		!cl
	      in
	      let cx2y1 =
		let cl =
		  let out (x, y) = not (x =/ bl1.x2 && y =/ bl1.y1) in
		  ref (List.filter out c.x2y1)
		in
		let add (cx, cy) =
		  let b_corn b = b.x2 =/ cx && b.y1 =/ cy in
		  let c_eq (x, y) = x =/ cx && y =/ cy in
		  if not ((List.exists b_corn b_done) ||
		    (List.exists c_eq !cl))
		  then cl := (cx, cy) :: !cl
		in
		if bl1.x2 =/ p1 && bl1.y2 </ p1 then add (p1, bl1.y2);
		if bl1.x1 >/ p0 && bl1.y1 =/ p0 then add (bl1.x1, p0);
		let f b =
		  let g b1 b2 =
		    if
		      (b1.x1 =/ b2.x2 && inside b2.y2 b1.y1 b1.y2) ||
			(b1.y1 =/ b2.y2 && inside b1.x1 b2.x1 b2.x2)
		    then add (b1.x1, b2.y2)
		  in
		  g b bl1; g bl1 b
		in
		List.iter f b_done;
		!cl
	      in
	      let cx2y2 =
		let cl =
		  let out (x, y) = not (x =/ bl1.x2 && y =/ bl1.y2) in
		  ref (List.filter out c.x2y2)
		in
		let add (cx, cy) =
		  let b_corn b = b.x2 =/ cx && b.y2 =/ cy in
		  let c_eq (x, y) = x =/ cx && y =/ cy in
		  if not ((List.exists b_corn b_done) ||
		    (List.exists c_eq !cl))
		  then cl := (cx, cy) :: !cl
		in
		if bl1.x2 =/ p1 && bl1.y1 >/ p0 then add (p1, bl1.y1);
		if bl1.x1 >/ p0 && bl1.y2 =/ p1 then add (bl1.x1, p1);
		let f b =
		  let g b1 b2 =
		    if
		      (b1.x1 =/ b2.x2 && inside b2.y2 b1.y1 b1.y2) ||
			(b1.y2 =/ b2.y1 && inside b1.x1 b2.x1 b2.x2)
		    then add (b1.x1, b2.y1)
		  in
		  g b bl1; g bl1 b
		in
		List.iter f b_done;
		!cl
	      in
	      {x1y1 = cx1y1; x1y2 = cx1y2; x2y1 = cx2y1; x2y2 = cx2y2}
	    in
	    fill (bl1 :: b_done) new_corn (n + 1)
	  with Exit -> fill_one bl2
	)
  in
  let new_blks =
    let nb = ref [] in
    let add b =
      if
	b.x1 >=/ p0 && b.x2 <=/ p1 && b.y1 >=/ p0 && b.y2 <=/ p1 &&
	  not ((List.exists (intersect b) b_done) ||
	    (List.exists (b_eq b) !nb))
      then nb := b :: !nb
    in
    let ln = (Int 1 // Int n) and ln1 = (Int 1 // Int (n + 1)) in
    let dx, dy = if n mod 2 = 1 then ln, ln1 else ln1, ln in
    Arith_status.set_normalize_ratio true;
    let fx1y1 (x, y) = add {x1 = x; x2 = x +/ dx; y1 = y; y2 = y +/ dy} in
    let fx1y2 (x, y) = add {x1 = x; x2 = x +/ dx; y1 = y -/ dy; y2 = y} in
    let fx2y1 (x, y) = add {x1 = x -/ dx; x2 = x; y1 = y; y2 = y +/ dy} in
    let fx2y2 (x, y) = add {x1 = x -/ dx; x2 = x; y1 = y -/ dy; y2 = y} in
    Arith_status.set_normalize_ratio false;
    List.iter fx1y1 c.x1y1;
    List.iter fx1y2 c.x1y2;
    List.iter fx2y1 c.x2y1;
    List.iter fx2y2 c.x2y2;
    !nb
  in
  if n > nmax then b_done else fill_one new_blks;;

let blks =
  let c0 = {
    x1y1 = [(p13, p34); (p23, p12)]; x1y2 = [p13, p1];
    x2y1 = [p1, p12]; x2y2 = [p1, p1]
  }
  in
  let b12 = {x1 = p0; x2 = p1; y1 = p0; y2 = p12}
  and b23 = {x1 = p0; x2 = p13; y1 = p12; y2 = p1}
  and b34 = {x1 = p13; x2 = p23; y1 = p12; y2 = p34}
  in
  try
(*    fill [] {x1y1=[p0,p0];x1y2=[p0,p1];x2y1=[p1,p0];x2y2=[p1,p1]} 1 *)
    fill [b12; b23; b34] c0 4
  with Exit -> []
in
(* Arith_status.set_normalize_ratio true; *)
write blks ("sq_" ^ (string_of_int nmax) ^ ".ps")

