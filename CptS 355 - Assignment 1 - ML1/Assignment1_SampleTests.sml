(*************************************)
(*Assignment 1 - Tests*)
(*************************************)
fun existsTest () =
   let 
     val existsT1 = (exists(8,[7]) = false )
     val existsT2 = (exists("one",["two","one"]) = true )
     val existsT3 = (exists(true,[false,false]) = false )
   in 
     print ("\n------------- \nexists:\n" ^ 
            "  test1: " ^ Bool.toString(existsT1) ^ "\n" ^
            "  test2: " ^ Bool.toString(existsT2) ^ "\n" ^
 	        "  test3: " ^ Bool.toString(existsT3) ^ "\n")		
   end
val _ = existsTest()

fun replaceTest () =
   let 
     val replaceT1 = ((replace 3 40 [1, 2, 3, 4, 5, 6]) = [1,2,3,40,5,6] )
     val replaceT2 = ((replace 0 "X" ["a", "b", "c", "d"]) = ["X","b","c","d"] )
     val replaceT3 = ((replace 6 7 [1,2,3,4,5]) = [1,2,3,4,5] )
   in 
     print ("\n------------- \nreplace:\n" ^ 
            "  test1: " ^ Bool.toString(replaceT1) ^ "\n" ^
            "  test2: " ^ Bool.toString(replaceT2) ^ "\n" ^
 	        "  test3: " ^ Bool.toString(replaceT3) ^ "\n")		
   end
val _ = replaceTest()


fun prereqForTest () =
   let 
    val prereqsList = [("Cpts122",["CptS121"]), ("CptS132",["CptS131"]), ("CptS223",["CptS122", "MATH216"]), ("CptS233",["CptS132", "MATH216"]), ("CptS260",["CptS223", "CptS233"]), 
                       ("CptS315",["CptS223", "CptS233"]), ("CptS317",["CptS122", "CptS132", "MATH216"]), ("CptS321",["CptS223", "CptS233"]), ("CptS322",["CptS223","CptS233"]), 
                       ("CptS350",["CptS223","CptS233", "CptS317"]), ("CptS355",["CptS223"]), ("CptS360",["CptS223","CptS260"]),("CptS370",["CptS233","CptS260"]),
                       ("CptS427",["CptS223","CptS360", "CptS370", "MATH216", "EE234"])
]
     val prereqForT1 = (prereqFor (prereqsList,"CptS260") = ["CptS360","CptS370"] )
     val prereqForT2 = (prereqFor (prereqsList,"CptS223") = ["CptS260","CptS315","CptS321","CptS322","CptS350","CptS355","CptS360","CptS427"] )
     val prereqForT3 = (prereqFor (prereqsList,"CptS355") = [] )
   in 
     print ("\n------------- \nprereqFor:\n" ^ 
            "  test1: " ^ Bool.toString(prereqForT1) ^ "\n" ^
            "  test2: " ^ Bool.toString(prereqForT2) ^ "\n" ^
 	        "  test3: " ^ Bool.toString(prereqForT3) ^ "\n")		
   end
val _ = prereqForTest()
