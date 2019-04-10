(* Jimmy Zheng *)
(* 11577623 *)
(* HW1 *)

(* 1. exists *) 
(* a *)
fun exists (a,[]) = false (* if list is empty then false *)
  | exists (a,x::rest) =  (* list is not empty so we check if a exist inside the list *)
        if a = x then true
        else exists (a,rest)

(* b *)
(* ''a is a comparable type and 'a is not becuause the two are comparable types that need to be comparable we use ''a instead of 'a. *)

(* 2. listUnion *) 

(******************************************************)
fun listUnion L1 L2 = 
  let
    fun combine [] [] = [] (* if both list are empty then return empty list *)
      | combine [] L = L  
      | combine (x::rest) L = x::(combine rest L) (* if list is not empty combine them *)
    fun filter [] = [] (* filter througth the single list to find duplicates*)
      | filter (x::rest) = 
         if exists(x,rest) = true then filter (rest)
         else x::filter (rest)
  in
    filter (combine L1 L2) (* call both for listUnion *)
  end
(******************************************************)

(* 3. replace *)
fun replace n v [] = [] 
  | replace 0 v (x::rest) = v::rest (* once n reaches 0 replae v with n *)
  | replace n v (x::rest) = x::(replace (n-1) v rest) (* continue to cons x to the begining until n reaches 0 *)

(* 4. prereqFor *)
(* a *)
val prereqsList = [
("Cpts122" , ["CptS121"]),
("CptS132" , ["CptS131"]),
("CptS223" , ["CptS122", "MATH216"]),
("CptS233" , ["CptS132", "MATH216"]),
("CptS260" , ["CptS223", "CptS233"]),
("CptS315" , ["CptS223", "CptS233"]),
("CptS317" , ["CptS122", "CptS132", "MATH216"]),
("CptS321" , ["CptS223", "CptS233"]),
("CptS322" , ["CptS223", "CptS233"]),
("CptS350" , ["CptS223", "CptS233", "CptS317"]),
("CptS355" , ["CptS223"]),
("CptS360" , ["CptS223", "CptS260"]),
("CptS370" , ["CptS233", "CptS260"]),
("CptS427" , ["CptS223", "CptS360", "CptS370", "MATH216", "EE234"]) ]

fun prereqFor ([],Course) = [] 
  | prereqFor (((Class, L)::rest),Course) = (* (Class, L) is a tuple inside the list *)
      if exists (Course, L) = true then Class::(prereqFor (rest, Course)) (* we use the existing function exists to find it the course is a prereq for any class *)
      else prereqFor (rest, Course) (* if the class is a prereq we cons it to the front and continue to look through the list to find if it is a prereq for any other class *)

(* b *)
(* 'a is used instead of ''a because it is not a comparable type like ''b, we use ''b to compare to the ''b list to retrive a list of 'a. *)

(* 5. isPalindrome *)

(* delete spaces / make all to upper / reverse and compare the 2 strings *)

fun isPalindrome string = 
  let 
    fun spaceRemover [] = [] (* first we delete the spaces*)
      | spaceRemover (x::rest) = 
          if x = #" " then spaceRemover (rest) 
          else Char.toUpper(x)::spaceRemover (rest)
  in 
    (spaceRemover (String.explode(string))) = rev (spaceRemover (String.explode(string)))
  end (* then we make every charater to upper so it is not case sensetive then we get the revserse and compare *)

(* 6. groupSumtoN *)

fun groupSumtoN N [] = [[]]
  | groupSumtoN N L =
  let
    val originalN = N (* the original N because N would be changing *)
    fun groupRest N [] = []
      | groupRest N (x::rest) = 
      if x >= originalN then rest
      else if N-x >= 0 then (groupRest (N-x) rest)
      else x::rest (* counting the "x" that was left out if combined was smaller than N *)
    fun groupSum N [] = [] (* branch for when N is greater than x *)
      | groupSum N (x::rest) =
      if x > originalN then (groupSum N rest)
      else if N-x >= 0 then x::(groupSum (N-x) rest) (* N-x > 0 will cover the edge cases *)
      else (groupSum N rest)
    fun groupXisGreater N [] = [] (* branch for when x is greater than N *)
      | groupXisGreater N (x::rest) = 
      if (x - originalN) >= 0 then [x] 
      else (groupXisGreater N rest)
    fun groupSumAll N [] = []
      | groupSumAll N (x::rest) = (* serparte into 2 different branches one with N > x and one that is the opposite *)
      if x < originalN then (groupSum N (x::rest))::(groupSumAll N (groupRest N (x::rest)))
      else (groupXisGreater N (x::rest))::(groupSumAll N (groupRest N (x::rest)))
  in
    (groupSumAll N L)
  end

(*************************************)
(*Assignment 1 - Tests*)
(*************************************)
fun existsTest () =
   let 
     val existsT1 = (exists(8,[7]) = false )
     val existsT2 = (exists("one",["two","one"]) = true )
     val existsT3 = (exists(true,[false,false]) = false )
     val existsT4 = (exists("JIMMY",["notJimmy", "jimmy"]) = false )
     val existsT5 = (exists(1,[1,2,3]) = true )
     val existsT6 = (exists(1,[]) = false )
   in 
     print ("\n------------- \n1. exists:\n" ^ 
            "  test1: " ^ Bool.toString(existsT1) ^ "\n" ^
            "  test2: " ^ Bool.toString(existsT2) ^ "\n" ^
 	          "  test3: " ^ Bool.toString(existsT3) ^ "\n" ^
            "  test4: " ^ Bool.toString(existsT4) ^ "\n" ^
            "  test5: " ^ Bool.toString(existsT5) ^ "\n" ^
            "  test6: " ^ Bool.toString(existsT6) ^ "\n")		
   end
val _ = existsTest()

fun listUnionTest () =
   let 
     val listUnionT1 = (listUnion [1,3,4] [2,3,4,5] = [1,2,3,4,5] )
     val listUnionT2 = (listUnion [1,1,2,3,3,3] [1,3,4,5] = [2,1,3,4,5] )
     val listUnionT3 = (listUnion ["a","b","c"] ["b","b","d"] = ["a","c","b","d"] )
     val listUnionT4 = (listUnion [[1,2],[2,3]] [[1],[2,3],[2,3]] = [[1,2],[1],[2,3]] )
     val listUnionT5 = (listUnion ["j","i","m","m","y"] [] = ["j","i","m","y"] )
     val listUnionT6 = (listUnion [] [] = [] )
   in 
     print ("\n------------- \n2. listUnion:\n" ^ 
            "  test1: " ^ Bool.toString(listUnionT1) ^ "\n" ^
            "  test2: " ^ Bool.toString(listUnionT2) ^ "\n" ^
 	          "  test3: " ^ Bool.toString(listUnionT3) ^ "\n" ^
            "  test4: " ^ Bool.toString(listUnionT4) ^ "\n" ^
            "  test5: " ^ Bool.toString(listUnionT5) ^ "\n" ^
            "  test6: " ^ Bool.toString(listUnionT6) ^ "\n")		
   end
val _ = listUnionTest()


fun replaceTest () =
   let 
     val replaceT1 = ((replace 3 40 [1, 2, 3, 4, 5, 6]) = [1,2,3,40,5,6] )
     val replaceT2 = ((replace 0 "X" ["a", "b", "c", "d"]) = ["X","b","c","d"] )
     val replaceT3 = ((replace 6 7 [1,2,3,4,5]) = [1,2,3,4,5] )
     val replaceT4 = ((replace 2 1 [1,2,3,4,5]) = [1,2,1,4,5] )
     val replaceT5 = ((replace 4 "Y" ["J", "I", "M", "M", "y"]) = ["J","I","M","M","Y"] )
     val replaceT6 = ((replace 0 " " ["H", "e", "l", "l", "o"]) = [" ","e","l","l","o"] )
   in 
     print ("\n------------- \n3. replace:\n" ^ 
            "  test1: " ^ Bool.toString(replaceT1) ^ "\n" ^
            "  test2: " ^ Bool.toString(replaceT2) ^ "\n" ^
 	          "  test3: " ^ Bool.toString(replaceT3) ^ "\n" ^	
            "  test4: " ^ Bool.toString(replaceT4) ^ "\n" ^
            "  test5: " ^ Bool.toString(replaceT5) ^ "\n" ^
 	          "  test6: " ^ Bool.toString(replaceT6) ^ "\n")	
   end
val _ = replaceTest()

fun prereqForTest () =
   let 
    val prereqsList = [("Cpts122",["CptS121"]), ("CptS132",["CptS131"]), ("CptS223",["CptS122", "MATH216"]), ("CptS233",["CptS132", "MATH216"]), ("CptS260",["CptS223", "CptS233"]), 
                       ("CptS315",["CptS223", "CptS233"]), ("CptS317",["CptS122", "CptS132", "MATH216"]), ("CptS321",["CptS223", "CptS233"]), ("CptS322",["CptS223","CptS233"]), 
                       ("CptS350",["CptS223","CptS233", "CptS317"]), ("CptS355",["CptS223"]), ("CptS360",["CptS223","CptS260"]),("CptS370",["CptS233","CptS260"]),
                       ("CptS427",["CptS223","CptS360", "CptS370", "MATH216", "EE234"])]

     val prereqForT1 = (prereqFor (prereqsList,"CptS260") = ["CptS360","CptS370"] )
     val prereqForT2 = (prereqFor (prereqsList,"CptS223") = ["CptS260","CptS315","CptS321","CptS322","CptS350","CptS355","CptS360","CptS427"] )
     val prereqForT3 = (prereqFor (prereqsList,"CptS355") = [] )
     val prereqForT4 = (prereqFor (prereqsList,"MATH216") = ["CptS223","CptS233","CptS317","CptS427"] )
     val prereqForT5 = (prereqFor ([],"NO CLASS") = [] )
     val prereqForT6 = (prereqFor (prereqsList,"EE234") = ["CptS427"] )
   in 
     print ("\n------------- \n4. prereqFor:\n" ^ 
            "  test1: " ^ Bool.toString(prereqForT1) ^ "\n" ^
            "  test2: " ^ Bool.toString(prereqForT2) ^ "\n" ^
 	          "  test3: " ^ Bool.toString(prereqForT3) ^ "\n" ^
            "  test4: " ^ Bool.toString(prereqForT4) ^ "\n" ^
 	          "  test5: " ^ Bool.toString(prereqForT5) ^ "\n" ^
            "  test6: " ^ Bool.toString(prereqForT6) ^ "\n")		
   end
val _ = prereqForTest()

fun isPalindromeTest () =
   let 
     val isPalindromeT1 = (isPalindrome "a01 02 2010A" = true )
     val isPalindromeT2 = (isPalindrome "Doc note I dissent a fast never prevents a fatness I diet on cod" = true )
     val isPalindromeT3 = (isPalindrome "Yreka Bakery" = true )
     val isPalindromeT4 = (isPalindrome "top cart pop tracPOT" = true )
     val isPalindromeT5 = (isPalindrome " " = true )
     val isPalindromeT6 = (isPalindrome "isPalindrome" = false )
   in 
     print ("\n------------- \n5. isPalindrome:\n" ^ 
            "  test1: " ^ Bool.toString(isPalindromeT1) ^ "\n" ^
            "  test2: " ^ Bool.toString(isPalindromeT2) ^ "\n" ^
 	          "  test3: " ^ Bool.toString(isPalindromeT3) ^ "\n" ^
            "  test4: " ^ Bool.toString(isPalindromeT4) ^ "\n" ^
 	          "  test5: " ^ Bool.toString(isPalindromeT5) ^ "\n" ^
            "  test6: " ^ Bool.toString(isPalindromeT6) ^ "\n")		
   end
val _ = isPalindromeTest()

fun groupSumtoNTest () =
   let 
     val groupSumtoNT1 = (groupSumtoN 15 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] = [[1,2,3,4,5],[6,7],[8],[9],[10]] )
     val groupSumtoNT2 = (groupSumtoN 11 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] = [[1,2,3,4],[5,6],[7],[8],[9],[10]] )
     val groupSumtoNT3 = (groupSumtoN 1 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] = [[1],[2],[3],[4],[5],[6],[7],[8],[9],[10]] )
     val groupSumtoNT4 = (groupSumtoN 5 [] = [[]] )
     val groupSumtoNT5 = (groupSumtoN 0 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] = [[1],[2],[3],[4],[5],[6],[7],[8],[9],[10]] )
     val groupSumtoNT6 = (groupSumtoN 5 [1, 2, 2, 5, 3, 2, 1, 1, 1, 1, 1, 2] = [[1,2,2],[3,2],[1,1,1,1,1],[2]] )
   in 
     print ("\n------------- \n6. groupSumtoN:\n" ^ 
            "  test1: " ^ Bool.toString(groupSumtoNT1) ^ "\n" ^
            "  test2: " ^ Bool.toString(groupSumtoNT2) ^ "\n" ^
 	          "  test3: " ^ Bool.toString(groupSumtoNT3) ^ "\n" ^
            "  test4: " ^ Bool.toString(groupSumtoNT4) ^ "\n" ^
 	          "  test5: " ^ Bool.toString(groupSumtoNT5) ^ "\n" ^
            "  test6: " ^ Bool.toString(groupSumtoNT6) ^ "\n")		
   end
val _ = groupSumtoNTest()