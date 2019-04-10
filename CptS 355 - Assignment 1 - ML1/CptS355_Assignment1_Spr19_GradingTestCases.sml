(*CptS 355 - Spring 2019 Grading Test Cases : 1/28/2019*)

(*1. exists*)
fun exists (n,[]) = false |  exists(n,x::rest) = if n=x then true else exists(n,rest)

(*2. listUnion*)
fun listUnion L1 L2 = 
let
    fun unionhelper [] L = L
     |  unionhelper (x::rest) L = if not (exists (x,L)) then (unionhelper rest (x::L))
                                  else (unionhelper rest L) 
in 
    rev((unionhelper (unionhelper L1 L2) []))
end

(*3. replace*)
fun replace n v [] = []
 |  replace n v (x::rest) =  if (n=0) then v::rest
                                    else x::(replace (n-1) v rest)

(*4. prereqFor*)
fun prereqFor  ([],mycourse) = []
 |  prereqFor  ((course, prereqs)::rest,mycourse) = if exists(mycourse,prereqs)=true then course::(prereqFor (rest,mycourse))
                                            else (prereqFor (rest,mycourse));
(*5. isPalindrome*)

fun isPalindrome s = let
   fun removeSpaces [] = []
    |  removeSpaces (x::rest) = if x = #" " then (removeSpaces rest) else x::(removeSpaces rest)

   fun palindrome_helper [] [] = true 
    |  palindrome_helper [] (x2::rest2) = false
    |  palindrome_helper (x1::rest1) [] = false
    |  palindrome_helper (x1::rest1) (x2::rest2) =  if (Char.toUpper(x1) = Char.toUpper(x2)) then  (palindrome_helper rest1 rest2) else false
in 
    palindrome_helper  (removeSpaces(String.explode(s)))  (removeSpaces(rev(String.explode(s))))
end

fun isPalindrome1 s = let
    fun palindrome_helper1 [] [] = true 
    | palindrome_helper1 [] (x2::rest2) = if x2 = #" " then  (palindrome_helper1 [] rest2) else false
    | palindrome_helper1 (x1::rest1) [] =  if x1 = #" " then  (palindrome_helper1 [] rest1) else false
    | palindrome_helper1 (#" "::rest1) (#" "::rest2) = palindrome_helper1 rest1 rest2
    | palindrome_helper1 (#" "::rest1) (x2::rest2) =  palindrome_helper1 rest1 (x2::rest2)
    | palindrome_helper1 (x1::rest1) (#" " ::rest2) =  palindrome_helper1 (x1::rest1) rest2
    | palindrome_helper1 (x1::rest1) (x2::rest2) =  if (Char.toUpper(x1) = Char.toUpper(x2)) then  (palindrome_helper1 rest1 rest2) else false
in 
     palindrome_helper1  (String.explode(s))  (rev(String.explode(s)))
end
(*6. groupSumtoN*)
fun groupSumtoN n L = 
let 
	fun sumL [] = 0 | sumL(x::rest) = (sumL rest) + x
	fun groupSumtoN_helper n acc [] = [rev(acc)] 
    | groupSumtoN_helper n acc (x::rest) = if (n < sumL(x::acc)) then rev(acc)::(groupSumtoN_helper n [x] (rest)) 
	                                         else groupSumtoN_helper n (x::acc) rest
in 
	groupSumtoN_helper n [] L
end

(*================================*)
(*------------TEST CASES----------*)
(*================================*)

(***********************   COPY FROM HERE  **************************************************)

(*auxilary functions for tests*)
fun diffLists (L1,L2) = 
let
	fun exists (n,[]) = false |  exists(n,x::rest) = if n=x then true else exists(n,rest)
	fun allExists [] L = true 
			|  allExists (x::rest) L = if exists(x,L) then (allExists rest L) else false
in
	(allExists L1 L2) andalso (allExists L2 L1)
end
 
 
fun areAllUnique  L =
let
	fun filter pred [] = [] 
	  | filter pred (x::rest) = if pred x then x::(filter pred rest) else (filter pred rest)
	fun countE  [] e = 0
	  | countE (x::rest) e = if e=x then 1+(countE rest e) else countE rest e
	
	fun isOne e = if e>1 then true else false
in
	null (filter isOne (map (countE L) L))
end

fun score eval points = if eval = true then 0 
						else ~points


fun passFail result = if result = true then "PASS" else "-FAIL-"						

fun checkAB testA testB = if testA = true andalso testB = true then true else false						


(* We print out points to deduct, test case, expected output, actual output*)

(************* Assignment 1 Tests *********************)

(*exists*)
(* (part a) each 2pts  =  total 8 points + (part b) 2pts*)
(*Example solution for part (b)*)
(*exists function should compare the given value with each element of the list for equality. Therefore the type of list elements and the given value should be a comparable type. In ML comparable types are represented by the type variable ''a.*)
fun existsTest () =
    let 
		val inputT1 = (1,[])
		val inputT2 = ([1],[[1,2],[1],[3,4]])
		val inputT3 = ("a",["a","b","c"])
		val inputT4 = ("d",["a","b","c"])
		val showInputT1 = "exists (1,[])"
		val showInputT2 = "exists ([1],[[1,2],[1],[3,4]])"
		val showInputT3 = "exists (\"a\",[\"a\",\"b\",\"c\"])"
		val showInputT4 = "exists (\"d\",[\"a\",\"b\",\"c\"])"
		val outputT1 = false
		val outputT2 = true
		val outputT3 = true
		val outputT4 = false
		val existsT1 = exists inputT1 = outputT1
		val existsT2 = exists inputT2 = outputT2
		val existsT3 = exists inputT3 = outputT3
		val existsT4 = exists inputT4 = outputT4
	in 
		print ("\n--------------------exists:-------------------- \n"
			^ passFail(existsT1) ^ "\t deduct: " ^ Int.toString(score existsT1 2) ^ "\t input :  " ^ showInputT1 ^ "\t expected :  " ^ Bool.toString(outputT1) ^ "\n"
			^ passFail(existsT2) ^ "\t deduct: " ^ Int.toString(score existsT2 2) ^ "\t input :  " ^ showInputT2 ^ "\t expected :  " ^ Bool.toString(outputT2) ^ "\n"
			^ passFail(existsT3) ^ "\t deduct: " ^ Int.toString(score existsT3 2) ^ "\t input :  " ^ showInputT3 ^ "\t expected :  " ^ Bool.toString(outputT3) ^ "\n"
			^ passFail(existsT4) ^ "\t deduct: " ^ Int.toString(score existsT4 2) ^ "\t input :  " ^ showInputT4 ^ "\t expected :  " ^ Bool.toString(outputT4) ^ "\n")
    end
val _ = existsTest()


(* each 3pts = total 15 points*)
fun listUnionTest () =
    let 
		val showInputT1 = "listUnion [1,3,4] [2,3,4,5]"
		val showInputT2 = "listUnion [[2,3],[1,2],[2,3]]  [[1],[2,3]]"
		val showInputT3 = "listUnion [[2,3],[2,3],[],[],[1]]  [[1],[2,3],[2,3],[],[2]] "
		val showInputT4 = "listUnion [1,2,3,4,2,3,4,3,4,4]  [1,2,3,4,5,6]"
		val showInputT5 = "listUnion [\"1\",\"2\",\"3\"]  [\"2\",\"2\",\"2\"]"
		
		val showOutputT1 = "[1,2,3,4,5]"
		val showOutputT2 = "[[1,2],[1],[2,3]]"
		val showOutputT3 = "[[1],[2,3],[],[2]]"
		val showOutputT4 = "[1,2,3,4,5,6]"
		val showOutputT5 = "[\"3\",\"1\",\"2\"]"

		val listUnionT1 = diffLists ( (listUnion [1,3,4] [2,3,4,5]) , [1,2,3,4,5])
		val listUnionT2 = diffLists ( (listUnion [[2,3],[1,2],[2,3]]  [[1],[2,3]]) , [[1,2],[1],[2,3]])
		val listUnionT3 = diffLists ( (listUnion [[2,3],[2,3],[],[],[1]]  [[1],[2,3],[2,3],[],[2]]) , [[1],[2,3],[],[2]])		
		val listUnionT4 = diffLists ( (listUnion [1,2,3,4,2,3,4,3,4,4]  [1,2,3,2,3,4,4,5,6]) , [1,2,3,4,5,6])
		val listUnionT5 = diffLists ( (listUnion ["1","2","3"]  ["2","2","2"]) , ["3","1","2"])
		
	in 
		print ("\n--------------------listUnion:-------------------- \n"
			^ passFail( listUnionT1 ) ^ "\t deduct: " ^ Int.toString(score ( listUnionT1 ) 3) ^ "\t input: " ^ showInputT1 ^ "\t expected: " ^ showOutputT1 ^ "\n"
			^ passFail( listUnionT2 ) ^ "\t deduct: " ^ Int.toString(score ( listUnionT2 ) 3) ^ "\t input: " ^ showInputT2 ^ "\t expected: " ^ showOutputT2 ^ "\n"
			^ passFail( listUnionT3 ) ^ "\t deduct: " ^ Int.toString(score ( listUnionT3 ) 3) ^ "\t input: " ^ showInputT3 ^ "\t expected: " ^ showOutputT3 ^ "\n"
			^ passFail( listUnionT4 ) ^ "\t deduct: " ^ Int.toString(score ( listUnionT4 ) 3) ^ "\t input: " ^ showInputT4 ^ "\t expected: " ^ showOutputT4 ^ "\n"
			^ passFail( listUnionT5 ) ^ "\t deduct: " ^ Int.toString(score ( listUnionT5 ) 3) ^ "\t input: " ^ showInputT5 ^ "\t expected: " ^ showOutputT5 ^ "\n")	
	end
 val _ = listUnionTest()


(* each test is 3pts  = total 15 points*)
fun replaceTest () =
    let 
		val showInputT1 = "replace 3 40 [1, 2, 3, 4, 5, 6]"
		val showInputT2 = "replace 0 \"0\" [\"1\", \"1\", \"2\", \"3\", \"4\", \"5\"]"
		val showInputT3 = "replace 4 (5,\"e\")  [(1,\"a\"),(2,\"b\"),(3,\"c\"),(4,\"d\"),(2,\"f\")]"
		val showInputT4 = "replace 3 [6,7,8] [[1,2],[3],[4,5],[],[9,10]]"
        val showInputT5 = "replace 5 6 [1,2,3,4,5]"
				
		val showOutputT1 = "[1,2,3,40,5,6]"
		val showOutputT2 = "[\"0\",\"1\",\"2\",\"3\",\"4\",\"5\"]"
		val showOutputT3 = "[(1,\"a\"),(2,\"b\"),(3,\"c\"),(4,\"d\"),(5,\"e\")]"
		val showOutputT4 = "[[1,2],[3],[4,5],[6,7,8],[9,10]]"
        val showOutputT5 = "[1,2,3,4,5]"
		
		val replaceT1 = ( (replace 3 40 [1, 2, 3, 4, 5, 6]) = [1,2,3,40,5,6] )
		val replaceT2 = ( (replace 0 "0" ["1", "1", "2", "3", "4", "5"]) = ["0","1","2","3","4","5"] )
		val replaceT3 = ( (replace 4 (5,"e")  [(1,"a"),(2,"b"),(3,"c"),(4,"d"),(2,"f")]) = [(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e")] )
		val replaceT4 = ( (replace 3 [6,7,8] [[1,2],[3],[4,5],[],[9,10]]) = [[1,2],[3],[4,5],[6,7,8],[9,10]] )
        val replaceT5 = ( (replace 5 6 [1,2,3,4,5]) = [1,2,3,4,5] )

	in 
  
		print ("\n-------------------- replace: -------------------- \n"
			^ passFail( replaceT1 ) ^ "\t deduct: " ^ Int.toString(score replaceT1 3) ^ "\t input : " ^ showInputT1 ^ "\t expected : " ^ showOutputT1 ^ "\n"
			^ passFail( replaceT2 ) ^ "\t deduct: " ^ Int.toString(score replaceT2 3) ^ "\t input : " ^ showInputT2 ^ "\t expected : " ^ showOutputT2 ^ "\n"
			^ passFail( replaceT3 ) ^ "\t deduct: " ^ Int.toString(score replaceT3 3) ^ "\t input : " ^ showInputT3 ^ "\t expected : " ^ showOutputT3 ^ "\n"
            ^ passFail( replaceT4 ) ^ "\t deduct: " ^ Int.toString(score replaceT4 3) ^ "\t input : " ^ showInputT4 ^ "\t expected : " ^ showOutputT4 ^ "\n"
			^ passFail( replaceT5 ) ^ "\t deduct: " ^ Int.toString(score replaceT5 3) ^ "\t input : " ^ showInputT5 ^ "\t expected : " ^ showOutputT5 ^ "\n")		
	end
val _ = replaceTest()


 (* (part-a: each 3 points = total 18 )  + (part-b:  2 pts) ->  total 20 points*)
 (*Example solution for part (b)*)
(* "('a * ''b list) list"  is the type of the first argument, where 'a is the type of the "course" and ''b is the type of the "prerequistes". 
In the "prereqs" function the courses and prerequisites are not correlated (they are not compared) therefore their types doesn't need to match. *)
fun prereqForTest () =
    let 
		val prereqsList = [("Cpts122",["CptS121"]), ("CptS132",["CptS131"]), ("CptS223",["CptS122", "MATH216"]), ("CptS233",["CptS132", "MATH216"]), ("CptS260",["CptS223", "CptS233"]), 
                       ("CptS315",["CptS223", "CptS233"]), ("CptS317",["CptS122", "CptS132", "MATH216"]), ("CptS321",["CptS223", "CptS233"]), ("CptS322",["CptS223","CptS233"]), 
                       ("CptS350",["CptS223","CptS233", "CptS317"]), ("CptS355",["CptS223"]), ("CptS360",["CptS223","CptS260"]),("CptS370",["CptS233","CptS260"]),
                       ("CptS421",["CptS322","CptS223"]),("CptS422",["CptS322","CptS321"]),("CptS423",["CptS421","CptS422","CptS360"]),("CptS427",["CptS223","CptS360", "CptS370", "MATH216", "EE234"])
    ]
	    val prereqText = "[(\"Cpts122\",[\"CptS121\"]), (\"CptS132\",[\"CptS131\"]), (\"CptS223\",[\"CptS122\", \"MATH216\"]), (\"CptS233\",[\"CptS132\", \"MATH216\"]), (\"CptS260\",[\"CptS223\", \"CptS233\"])," ^ 
		                 "(\"CptS315\",[\"CptS223\", \"CptS233\"]), (\"CptS317\",[\"CptS122\", \"CptS132\", \"MATH216\"]), (\"CptS321\",[\"CptS223\", \"CptS233\"]), (\"CptS322\",[\"CptS223\",\"CptS233\"])," ^ 
                         "(\"CptS350\",[\"CptS223\",\"CptS233\", \"CptS317\"]), (\"CptS355\",[\"CptS223\"]), (\"CptS360\",[\"CptS223\",\"CptS260\"]),(\"CptS370\",[\"CptS233\",\"CptS260\"])," ^
                         "(\"CptS421\",[\"CptS322\",\"CptS223\"]),(\"CptS422\",[\"CptS322\",\"CptS321\"]),(\"CptS423\",[\"CptS421\",\"CptS422\",\"CptS360\"]),(\"CptS427\",[\"CptS223\",\"CptS360\", \"CptS370\", \"MATH216\", \"EE234\"])]"

		val showInputT1 = "prereqFor (prereqsList , \"CptS223\")"
		val showInputT2 = "prereqFor (prereqsList , \"CptS322\")"
		val showInputT3 = "prereqFor (prereqsList , \"CptS421\")"
		val showInputT4 = "prereqFor (prereqsList , \"CptS360\")"
		val showInputT5 = "prereqFor (prereqsList , \"CptS333\")"
    	val showInputT6 = "prereqFor (prereqsList , \"CptS233\")"
		
		val showOutputT1 = "[\"CptS260\",\"CptS315\",\"CptS321\",\"CptS322\",\"CptS350\",\"CptS355\",\"CptS360\",\"CptS421\",\"CptS427\"]"
		val showOutputT2 = "[\"CptS421\",\"CptS422\"]"
		val showOutputT3 = "[\"CptS423\"]"
		val showOutputT4 = "[\"CptS423\",\"CptS427\"]"
		val showOutputT5 = "[]"
		val showOutputT6 = "[\"CptS260\",\"CptS315\",\"CptS321\",\"CptS322\",\"CptS350\",\"CptS370\"]"

		val prereqForT1 = (diffLists (prereqFor (prereqsList , "CptS223") , ["CptS260","CptS315","CptS321","CptS322","CptS350","CptS355","CptS360","CptS421","CptS427"]))
		val prereqForT2 = (diffLists (prereqFor (prereqsList , "CptS322") , ["CptS421","CptS422"]))
		val prereqForT3 = (diffLists (prereqFor (prereqsList , "CptS421") ,   ["CptS423"]))
		val prereqForT4 = (diffLists (prereqFor (prereqsList , "CptS360") , ["CptS423","CptS427"]))
		val prereqForT5 = (diffLists (prereqFor (prereqsList , "CptS333") , []))
     	val prereqForT6 = (diffLists (prereqFor (prereqsList , "CptS233") , ["CptS260","CptS315","CptS321","CptS322","CptS350","CptS370"]))
	in 
		print ("\n--------------------prereqFor:-------------------- \n"
		    ^ prereqText ^ "\n"
			^ passFail(prereqForT1) ^ "\t deduct: " ^ Int.toString(score (prereqForT1) 3) ^ "\t input :  " ^ showInputT1 ^ "\t expected :  " ^ showOutputT1 ^ "\n"
			^ passFail(prereqForT2) ^ "\t deduct: " ^ Int.toString(score (prereqForT2) 3) ^ "\t input :  " ^ showInputT2 ^ "\t expected :  " ^ showOutputT2 ^ "\n"
			^ passFail(prereqForT3) ^ "\t deduct: " ^ Int.toString(score (prereqForT3) 3) ^ "\t input :  " ^ showInputT3 ^ "\t expected :  " ^ showOutputT3 ^ "\n"
			^ passFail(prereqForT4) ^ "\t deduct: " ^ Int.toString(score (prereqForT4) 3) ^ "\t input :  " ^ showInputT4 ^ "\t expected :  " ^ showOutputT4 ^ "\n"		
			^ passFail(prereqForT5) ^ "\t deduct: " ^ Int.toString(score (prereqForT5) 3) ^ "\t input :  " ^ showInputT5 ^ "\t expected :  " ^ showOutputT5 ^ "\n"		
			^ passFail(prereqForT6) ^ "\t deduct: " ^ Int.toString(score (prereqForT6) 3) ^ "\t input :  " ^ showInputT6 ^ "\t expected :  " ^ showOutputT6 ^ "\n")		
	end
val _ = prereqForTest() 

 (* each 4 pts = total 20 points*)
fun isPalindromeTest () =
    let 
		val showInputT1 = "(isPalindrome \"Doc note I dissent a fast never prevents a fatness I diet on cod\")"
		val showInputT2 = "(isPalindrome \"a  bCd  e F ghiJkLMNOp    on mlK jiH GfED cBA\")"
		val showInputT3 = "(isPalindrome \"1 Yreka-B-akery1\")"
		val showInputT4 = "(isPalindrome \"A Santa Lived As a Devil At NASA\")"
		val showInputT5 = "(isPalindrome \"abcdefg g f e d c b aa\" )"
				
		val showOutputT1 = "true"
		val showOutputT2 = "true"
		val showOutputT3 = "true"
		val showOutputT4 = "true"
		val showOutputT5 = "false"
		
		val isPalindromeT1 = ((isPalindrome "Doc note I dissent a fast never prevents a fatness I diet on cod") = true)
		val isPalindromeT2 = ((isPalindrome "a  bCd  e F ghiJkLMNOp    on mlK jiH GfED cBA") = true)
		val isPalindromeT3 = ((isPalindrome "1 Yreka-B-akery1") = true)
		val isPalindromeT4 = ((isPalindrome "A Santa Lived As a Devil At NASA") = true)
		val isPalindromeT5 = ((isPalindrome "abcdefg g f e d c b aa") = false)
	in 
		print ("\n--------------------isPalindrome:-------------------- \n"
			^ passFail(isPalindromeT1) ^ "\t deduct: " ^ Int.toString(score (isPalindromeT1) 4) ^ "\t input: " ^ showInputT1 ^ "\t expected: " ^ showOutputT1 ^ "\n"
			^ passFail(isPalindromeT2) ^ "\t deduct: " ^ Int.toString(score (isPalindromeT2) 4) ^ "\t input: " ^ showInputT2 ^ "\t expected: " ^ showOutputT2 ^ "\n"
			^ passFail(isPalindromeT3) ^ "\t deduct: " ^ Int.toString(score (isPalindromeT3) 4) ^ "\t input: " ^ showInputT3 ^ "\t expected: " ^ showOutputT3 ^ "\n"
			^ passFail(isPalindromeT4) ^ "\t deduct: " ^ Int.toString(score (isPalindromeT4) 4) ^ "\t input: " ^ showInputT4 ^ "\t expected: " ^ showOutputT4 ^ "\n"		
			^ passFail(isPalindromeT5) ^ "\t deduct: " ^ Int.toString(score (isPalindromeT5) 4) ^ "\t input: " ^ showInputT5 ^ "\t expected: " ^ showOutputT5 ^ "\n")		
	end
val _ = isPalindromeTest()

(* each 4 points = total 20 points*)
fun groupSumtoNTest () =
    let 
		val showInputT1 = "(groupSumtoN 21 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])"
		val showInputT2 = "(groupSumtoN 14 [1, 2, 3, 4, 5, ~6, 7, ~8, 9, 10])"
		val showInputT3 = "(groupSumtoN 54 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])"
		val showInputT4 = "(groupSumtoN 2 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])"
		val showInputT5 = "(groupSumtoN 1 [~1,~2,~3,4,5,6,7])"
				
		val showOutputT1 = "[[1,2,3,4,5,6],[7,8],[9,10]]"
		val showOutputT2 = "[[1,2,3,4],[5,~6,7,~8,9],[10]]"
		val showOutputT3 = "[[1,2,3,4,5,6,7,8,9],[10]]"
		val showOutputT4 = "[[1],[2],[3],[4],[5],[6],[7],[8],[9],[10]]"
		val showOutputT5 = "[[~1,~2,~3,4],[5],[6],[7]]"
			
		val groupSumtoNT1 = ((groupSumtoN 21 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) = [[1,2,3,4,5,6],[7,8],[9,10]] )
		val groupSumtoNT2 = ((groupSumtoN 14 [1, 2, 3, 4, 5, ~6, 7, ~8, 9, 10]) = [[1,2,3,4],[5,~6,7,~8,9],[10]] )
		val groupSumtoNT3 = ((groupSumtoN 54 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) = [[1,2,3,4,5,6,7,8,9],[10]] )
		val groupSumtoNT4 = ((groupSumtoN 2 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) = [[1],[2],[3],[4],[5],[6],[7],[8],[9],[10]] )
		val groupSumtoNT5 = ((groupSumtoN 1 [~1,~2,~3,4,5,6,7]) = [[~1,~2,~3,4],[5],[6],[7]] )
	in 
		print ("\n--------------------groupSumtoN:-------------------- \n"
			^ passFail(groupSumtoNT1) ^ "\t deduct: " ^ Int.toString(score (groupSumtoNT1) 4) ^ "\t input :  " ^ showInputT1 ^ "\t expected :  " ^ showOutputT1 ^ "\n"
			^ passFail(groupSumtoNT2) ^ "\t deduct: " ^ Int.toString(score (groupSumtoNT2) 4) ^ "\t input :  " ^ showInputT2 ^ "\t expected :  " ^ showOutputT2 ^ "\n"
			^ passFail(groupSumtoNT3) ^ "\t deduct: " ^ Int.toString(score (groupSumtoNT3) 4) ^ "\t input :  " ^ showInputT3 ^ "\t expected :  " ^ showOutputT3 ^ "\n"
			^ passFail(groupSumtoNT4) ^ "\t deduct: " ^ Int.toString(score (groupSumtoNT4) 4) ^ "\t input :  " ^ showInputT4 ^ "\t expected :  " ^ showOutputT4 ^ "\n"		
			^ passFail(groupSumtoNT5) ^ "\t deduct: " ^ Int.toString(score (groupSumtoNT5) 4) ^ "\t input :  " ^ showInputT5 ^ "\t expected :  " ^ showOutputT5 ^ "\n")		
	end
val _ = groupSumtoNTest()