(* Jimmy Zheng *)
(* 11577623 *)
(* HW1 *)
Control.Print.printDepth := 100;

fun fold f base [] = base      
|   fold f base (x::rest) = f x (fold f base rest)

fun filter pred [] = []
|   filter pred (x::rest) = 
        if (pred x) then x::(filter pred rest)
        else (filter pred rest)

(* 1. merge2, merge2Tail, and mergeN *)
(* (a) merge2 *)
fun merge2 [] [] = []
|   merge2 [] L = L
|   merge2 L [] = L
|   merge2 (x::rest) (y::rest2) = 
        if x < y then x::(merge2 rest (y::rest2) )
        else y::(merge2 (x::rest) rest2 )

(* (b) merge2Tail *)

fun merge2Tail [] [] = []
|   merge2Tail L1 L2 = 
        let 
                fun revAppend [] L = L
                |   revAppend (x::rest) L = revAppend rest (x::L)
                fun merge2TailHelper accum L1 [] = revAppend L1 accum 
                |   merge2TailHelper accum [] L2 = revAppend L2 accum 
                |   merge2TailHelper accum (x::rest) (y::rest2) =
                        if (x < y) then merge2TailHelper (x::accum) (rest) (y::rest2)
                        else merge2TailHelper (y::accum) (x::rest) (rest2)
        in
                rev (merge2TailHelper [] L1 L2)
        end

(* (c) mergeN *)
fun mergeN L = fold merge2 [] L

(* 2. getInRange and countInRange *)
(* (a) getInRange *)
fun getInRange v1 v2 [] = []
|   getInRange v1 v2 L =
        let 
                fun checkLower x = if (v1 < x) then true
                                   else false
                fun checkUpper x = if (v2 > x) then true 
                                   else false
        in
                filter checkUpper (filter checkLower L)
        end


(* (b) countInRange *)
fun countInRange v1 v2 [[]] = 0
|   countInRange v1 v2 Lists = 
        let 
                fun add x y = x + y
                fun addup L = fold add 0 L
                fun turn1 i = 1
                fun inRangeInner L = addup (map turn1 (getInRange v1 v2 L))
        in 
                addup (map inRangeInner Lists)
        end



(* 3. addLengths and addAllLengths *)
datatype lengthUnit = INCH of int | FOOT of int | YARD of int

(* (a) addLengths *)
fun addLengths l1 l2 = 
        let                 
                fun length (INCH inches) = (inches)*1
                |   length (FOOT feets) = (feets)*12
                |   length (YARD yards) = (yards)*36

                fun lengthINCH (inches) = INCH inches
        in 
                lengthINCH((length l1) + (length l2))
        end

(* (b) addAllLengths *)
fun addAllLengths Lists = 
        let 
                fun length (INCH inches) = (inches)*1
                |   length (FOOT feets) = (feets)*12
                |   length (YARD yards) = (yards)*36

                fun inAddLength L =  fold addLengths (INCH 0) L
        in 
                fold addLengths (INCH 0) (map inAddLength Lists)
        end


(* 4. sumTree and createSumTree *)
datatype 'a Tree = LEAF of 'a | NODE of 'a * ('a Tree) * ('a Tree)

(* (a) sumTree *)
fun sumTree (LEAF v) = v
|   sumTree (NODE (v,t1,t2)) = (sumTree t1) + (sumTree t2)

val t1 = NODE (1, NODE(2, NODE (3,LEAF 4, LEAF 5), LEAF 6), NODE(7, LEAF 8, LEAF 9));

sumTree t1;

val t2 = NODE (0, NODE(0, LEAF 4, NODE (0,LEAF 8, LEAF 9)), NODE(0, NODE(0,LEAF 10, NODE (0, LEAF 12, LEAF 13)), LEAF 7));

sumTree t2;

sumTree (LEAF 3);

(* (b) createSumTree *)
fun createSumTree (LEAF v) = (LEAF v)
|   createSumTree (NODE (v,t1,t2)) = (NODE ((sumTree t1) + (sumTree t2),createSumTree(t1),createSumTree(t2))) 

val t3 = NODE (0, NODE(0, NODE (0,LEAF 4, LEAF 5), LEAF 6), NODE(0, LEAF 8, LEAF 9));

createSumTree t3;

(* 5. foldListTree *)
datatype 'a listTree = listLEAF of ('a list) | listNODE of ('a listTree list)

fun add x y = x + y

fun foldListTree f base (listLEAF Ll) = fold f base Ll
|   foldListTree f base (listNODE Ln) = 
        let 
                fun foldTreeHelper (listNODE Ln) = 
                        fold f base (map foldTreeHelper Ln)
                |   foldTreeHelper (listLEAF Ll) = 
                        fold f base Ll
                
        in
               fold f base (map foldTreeHelper Ln)
        end

val t4 = listNODE(
 [ listNODE ([ listLEAF [1,2,3],listLEAF [4,5],listNODE([listLEAF [6], listLEAF []]) ]),
   listNODE([]),listLEAF [7,8],listNODE([listLEAF [], listLEAF []]) ]);

foldListTree add 0 t4;

val L1 = listLEAF ["School","-","of","-","Electrical"] 
val L2 = listLEAF ["-","Engineering","-"]
val L3 = listLEAF ["and","-","Computer","-"]
val L4 = listLEAF ["Science"]
val L5 = listLEAF ["-WSU"]
val N1 = listNODE [L1,L2]
val N2 = listNODE [N1,L3]
val t5 = listNODE [N2,L4,L5]

fun concat a b = a^b;

foldListTree concat "" t5;

(* (c) Testing your tree functions *)
val mytree1 = NODE (1, NODE(2, LEAF 3, NODE (4,LEAF 5, LEAF 6)), NODE(7, NODE(8,LEAF 9, NODE (10, LEAF 11, LEAF 12)), LEAF 13));

sumTree mytree1;

val mytree2 = NODE (0, NODE(0, NODE (0,LEAF 1, LEAF 2), LEAF 3), NODE(0, LEAF 4, LEAF 5));

createSumTree mytree2;

val mL1 = listLEAF ["Hello"] 
val mL2 = listLEAF [" ","World","!"]
val mL3 = listLEAF [" - ","Jimmy Zheng"," Computer"," "]
val mL4 = listLEAF ["Science "]
val mL5 = listLEAF ["355"]
val mN1 = listNODE [mL1,mL2]
val mN2 = listNODE [mN1,mL3]
val mytree3 = listNODE [mN2,mL4,mL5]

fun concat a b = a^b;

foldListTree concat "" mytree3;

(* merge2 *)
fun merge2Test () = let
  val merge2T1 = ((merge2 [2,5,6,8,9] [1,3,4,5,7,8,10])=([1,2,3,4,5,5,6,7,8,8,9,10]))
  val merge2T2 = ((merge2 [1,2] [0,10,12]) = ([0,1,2,10,12]))
  val merge2T3 = ((merge2 [1,3,3,5,5] [~1,2,4]) = ([~1,1,2,3,3,4,5,5]))
  val merge2T4 = ((merge2 [1,2,3] [])=([1,2,3]))
  val merge2T5 = ((merge2 [1,3,5,7,9] [2,4,6,8,10]) = ([1,2,3,4,5,6,7,8,9,10]))
  val merge2T6 = ((merge2 [~2,~1,0,2,5] [~1,2,4]) = ([~2,~1,~1,0,2,2,4,5]));
 in
     print ("merge2:-------------------- \n"   ^
            "   test1: " ^ Bool.toString(merge2T1) ^ "\n" ^
            "   test2: " ^ Bool.toString(merge2T2) ^ "\n" ^
            "   test3: " ^ Bool.toString(merge2T3) ^ "\n" ^
            "   test4: " ^ Bool.toString(merge2T4) ^ "\n" ^
            "   test5: " ^ Bool.toString(merge2T5) ^ "\n" ^
            "   test6: " ^ Bool.toString(merge2T6) ^ "\n")
end
val _ = merge2Test()

(* merge2Tail *)
fun merge2TailTest () = let
  val merge2TailT1 = ((merge2Tail [2,5,6,8,9] [1,3,4,5,7,8,10])=([1,2,3,4,5,5,6,7,8,8,9,10]))
  val merge2TailT2 = ((merge2Tail [1,2] [0,10,12]) = ([0,1,2,10,12]))
  val merge2TailT3 = ((merge2Tail [1,3,3,5,5] [~1,2,4]) = ([~1,1,2,3,3,4,5,5]))
  val merge2TailT4 = ((merge2Tail [1,2,3] [])=([1,2,3]))
  val merge2TailT5 = ((merge2Tail [1,3,5,7,9] [2,4,6,8,10]) = ([1,2,3,4,5,6,7,8,9,10]))
  val merge2TailT6 = ((merge2Tail [~2,~1,0,2,5] [~1,2,4]) = ([~2,~1,~1,0,2,2,4,5]));
 in
     print ("merge2Tail:-------------------- \n"   ^
            "   test1: " ^ Bool.toString(merge2TailT1) ^ "\n" ^
            "   test2: " ^ Bool.toString(merge2TailT2) ^ "\n" ^
            "   test3: " ^ Bool.toString(merge2TailT3) ^ "\n" ^
            "   test4: " ^ Bool.toString(merge2TailT4) ^ "\n" ^
            "   test5: " ^ Bool.toString(merge2TailT5) ^ "\n" ^
            "   test6: " ^ Bool.toString(merge2TailT6) ^ "\n")
end
val _ = merge2TailTest()

(* mergeN *)
fun mergeNTest () = let
  val mergeNT1 = ((mergeN [[1,2],[10,12],[2,5,6,8,9]])=([1,2,2,5,6,8,9,10,12]))
  val mergeNT2 = ((mergeN [[3,4],[~3,~2,~1],[1,2,5,8,9]]) = ([~3,~2,~1,1,2,3,4,5,8,9]))
  val mergeNT3 = ((mergeN [[1,2],[3,4],[5,6,7,8,9],[10]])=([1,2,3,4,5,6,7,8,9,10]))
  val mergeNT4 = ((mergeN [[],[~1,0],[1,2]])=([~1,0,1,2]))
  val mergeNT5 = ((mergeN [[~1],[~3,~2,~1],[0]]) = ([~3,~2,~1,~1,0]))
  val mergeNT6 = ((mergeN [[~2,~1,0,2,5],[~1,2,4]]) = ([~2,~1,~1,0,2,2,4,5]));
 in
     print ("mergeN:-------------------- \n"   ^
            "   test1: " ^ Bool.toString(mergeNT1) ^ "\n" ^
            "   test2: " ^ Bool.toString(mergeNT2) ^ "\n" ^
            "   test3: " ^ Bool.toString(mergeNT3) ^ "\n" ^
            "   test4: " ^ Bool.toString(mergeNT4) ^ "\n" ^
            "   test5: " ^ Bool.toString(mergeNT5) ^ "\n" ^
            "   test6: " ^ Bool.toString(mergeNT6) ^ "\n")
end
val _ = mergeNTest()

(* getInRange *)
fun getInRangeTest () = let
  val getInRangeT1 = ((getInRange 3 10 [1,2,3,4,5,6,7,8,9,10,11])=([4,5,6,7,8,9]))
  val getInRangeT2 = ((getInRange ~5 5 [~10,~5,0,5,10]) = ([0]))
  val getInRangeT3 = ((getInRange ~1 1 [~2,2,3,4,5])=([]))
  val getInRangeT4 = ((getInRange ~1 5 [0,1,2,3,4,5,6,7])=([0,1,2,3,4]))
  val getInRangeT5 = ((getInRange 0 1 [~1,0,1,2]) = ([]))
  val getInRangeT6 = ((getInRange ~4 4 [~6,~4,~2,2,4,6]) = ([~2,2]));
 in
     print ("getInRange:-------------------- \n"   ^
            "   test1: " ^ Bool.toString(getInRangeT1) ^ "\n" ^
            "   test2: " ^ Bool.toString(getInRangeT2) ^ "\n" ^
            "   test3: " ^ Bool.toString(getInRangeT3) ^ "\n" ^
            "   test4: " ^ Bool.toString(getInRangeT4) ^ "\n" ^
            "   test5: " ^ Bool.toString(getInRangeT5) ^ "\n" ^
            "   test6: " ^ Bool.toString(getInRangeT6) ^ "\n")
end
val _ = getInRangeTest()

(* countInRange *)
fun countInRangeTest () = let
  val countInRangeT1 = ((countInRange 3 10 [[1,2,3,4],[5,6,7,8,9],[10,11]])=(6))
  val countInRangeT2 = ((countInRange ~5 5 [[~10,~5,~4],[0,4,5],[],[10]]) = (3))
  val countInRangeT3 = ((countInRange 1 5 [[1,5],[1],[5],[]])=(0))
  val countInRangeT4 = ((countInRange ~1 5 [[0,1],[2,3,4],[5,6,7]])=(5))
  val countInRangeT5 = ((countInRange 0 1 [[~1],[0],[1,2]]) = (0))
  val countInRangeT6 = ((countInRange ~4 4 [[~6,~4],[~2,2],[4,6]]) = (2));
 in
     print ("countInRange:-------------------- \n"   ^
            "   test1: " ^ Bool.toString(countInRangeT1) ^ "\n" ^
            "   test2: " ^ Bool.toString(countInRangeT2) ^ "\n" ^
            "   test3: " ^ Bool.toString(countInRangeT3) ^ "\n" ^
            "   test4: " ^ Bool.toString(countInRangeT4) ^ "\n" ^
            "   test5: " ^ Bool.toString(countInRangeT5) ^ "\n" ^
            "   test6: " ^ Bool.toString(countInRangeT6) ^ "\n")
end
val _ = countInRangeTest()

(* addLengths *)
fun addLengthsTest () = let
  val addLengthsT1 = ((addLengths (FOOT 2) (INCH 5)) = (INCH 29))
  val addLengthsT2 = ((addLengths (YARD 3) (INCH 3)) = (INCH 111))  
  val addLengthsT3 = ((addLengths (FOOT 3) (FOOT 5)) = (INCH 96))
  val addLengthsT4 = ((addLengths (INCH 0) (FOOT 5)) = (INCH 60))
  val addLengthsT5 = ((addLengths (INCH 4) (FOOT 0)) = (INCH 4))
  val addLengthsT6 = ((addLengths (YARD 2) (FOOT 1)) = (INCH 84));
 in
     print ("addLengths:-------------------- \n"   ^
            "   test1: " ^ Bool.toString(addLengthsT1) ^ "\n" ^
            "   test2: " ^ Bool.toString(addLengthsT2) ^ "\n" ^
            "   test3: " ^ Bool.toString(addLengthsT3) ^ "\n" ^
            "   test4: " ^ Bool.toString(addLengthsT4) ^ "\n" ^
            "   test5: " ^ Bool.toString(addLengthsT5) ^ "\n" ^
            "   test6: " ^ Bool.toString(addLengthsT6) ^ "\n")
end
val _ = addLengthsTest()

(* addAllLengths *)
fun addAllLengthsTest () = let
  val addAllLengthsT1 = ((addAllLengths [[YARD 2,FOOT 1],[YARD 1,FOOT 2,INCH 10],[YARD 3]])=(INCH 262))
  val addAllLengthsT2 = ((addAllLengths [[FOOT 2], [FOOT 2, INCH 2],[]]) = (INCH 50))
  val addAllLengthsT3 = ((addAllLengths []) = (INCH 0))
  val addAllLengthsT4 = ((addAllLengths [[INCH 2,FOOT 1],[FOOT 12,FOOT 2,YARD 10],[YARD 3,FOOT 4]])=(INCH 698))
  val addAllLengthsT5 = ((addAllLengths [[], [FOOT 3, INCH 0],[YARD 0]]) = (INCH 36))
  val addAllLengthsT6 = ((addAllLengths [[FOOT 6], [FOOT 9, INCH 2],[YARD 3]]) = (INCH 290));
 in
     print ("addAllLengths:-------------------- \n"   ^
            "   test1: " ^ Bool.toString(addAllLengthsT1) ^ "\n" ^
            "   test2: " ^ Bool.toString(addAllLengthsT2) ^ "\n" ^
            "   test3: " ^ Bool.toString(addAllLengthsT3) ^ "\n" ^
            "   test4: " ^ Bool.toString(addAllLengthsT4) ^ "\n" ^
            "   test5: " ^ Bool.toString(addAllLengthsT5) ^ "\n" ^
            "   test6: " ^ Bool.toString(addAllLengthsT6) ^ "\n")
end
val _ = addAllLengthsTest()