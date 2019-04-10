# Jimmy Zheng
# 11577623
# CPTS 355
# Unix/Linux
import re

#------------------------- 10% -------------------------------------
# The operand stack: define the operand stack and its operations
opstack = []  #assuming top of the stack is the end of the list

# Now define the helper functions to push and pop values on the opstack (i.e, add/remove elements to/from the end of the Python list)
# Remember that there is a Postscript operator called "pop" so we choose different names for these functions.
# Recall that `pass` in python is a no-op: replace it with your code.

def opPop():
    if opstack == []:
        return opstack
    else:
        # op = opstack.pop()
        # if type(op) == str:
        #     newstr = "(" + op + ")"
        #     return newstr
        return opstack.pop()

    # opPop should return the popped value.
    # The pop() function should call opPop to pop the top value from the opstack, but it will ignore the popped value.

def opPush(value):
    if type(value) == str:
        # value = value.replace("(", "")
        # value = value.replace(")", "")
        # if value[0] == '4' or value[0] == '1' or value[0] == '6': #check for numbers
        #     opstack.append(int(value))
        opstack.append(value)
        # if type(int(value)) == int:
        #     opstack.append(int(value))
        # else:
        #     opstack.append(value)
    else:
        opstack.append(value)

#-------------------------- 20% -------------------------------------
# The dictionary stack: define the dictionary stack and its operations
dictstack = []  #assuming top of the stack is the end of the list

# now define functions to push and pop dictionaries on the dictstack, to define name, and to lookup a name

def dictPop():
    dictstack.pop()
    # dictPop pops the top dictionary from the dictionary stack.

def dictPush(d):
    dictstack.append(d)
    #dictPush pushes the dictionary ‘d’ to the dictstack. Note that, your interpreter will call dictPush only when Postscript “begin” operator is called. “begin” should pop the empty dictionary from the opstack and push it onto the dictstack by calling dictPush.

def define(name, value):
    if len(dictstack) == 0:
        newDict = {}
        newDict[name] = value
        dictstack.append(newDict)
    else:
        dictstack[-1][name] = value
    #add name:value pair to the top dictionary in the dictionary stack. Keep the '/' in the name constant.
    # Your psDef function should pop the name and value from operand stack and call the “define” function.

def lookup(name):
    for i in range(len(dictstack)):
        dic = dictstack[-1-i]  # does this work?
        if name in dic.keys():
            return dic[name]
        if "/"+name in dic.keys():
            return dic["/"+name]
        # else:
        #     return None
    # return the value associated with name
    # What is your design decision about what to do when there is no definition for “name”? If “name” is not defined, your program should not break, but should give an appropriate error message.

#--------------------------- 10% -------------------------------------
# Arithmetic and comparison operators: add, sub, mul, div, mod, eq, lt, gt
#Make sure to check the operand stack has the correct number of parameters and types of the parameters are correct.

def add():
    if len(opstack) > 1:
        op2 = opPop()
        op1 = opPop()
        if (type(op1) == int and type(op2) == int) or (type(op1) == float and type(op2) == float):
            opPush(op1 + op2)
    else:
        print("add ERROR")

def sub():
    if len(opstack) > 1:
        op2 = opPop()
        op1 = opPop()
        if (type(op1) == int and type(op2) == int) or (type(op1) == float and type(op2) == float):
            opPush(op1 - op2)
    else:
        print("sub ERROR")

def mul():
    if len(opstack) > 1:
        op2 = opPop()
        op1 = opPop()
        if (type(op1) == int and type(op2) == int) or (type(op1) == float and type(op2) == float):
            opPush(op1 * op2)
    else:
        print("mul ERROR")

def div():
    if len(opstack) > 1:
        op2 = opPop()
        op1 = opPop()
        if (type(op1) == int and type(op2) == int) or (type(op1) == float and type(op2) == float and op2 != 0):  # divide by zero error
            opPush(op1 / op2)
    else:
        print("div ERROR")

def mod():
    if len(opstack) > 1:
        op2 = opPop()
        op1 = opPop()
        if (type(op1) == int and type(op2) == int) or (type(op1) == float and type(op2) == float):
            opPush(op1 % op2)
    else:
        print("mod ERROR")

def eq():
    if len(opstack) > 1:
        op2 = opPop()
        op1 = opPop()
        # if (type(op1) == int and type(op2) == int) or (type(op1) == float and type(op2) == float) or (type(op1) == chr and type(op2) == chr):
        if op1 == op2:
            opPush(True)
        else:
            opPush(False)
    else:
        print("eq ERROR")

def lt():
    if len(opstack) > 1:
        op2 = opPop()
        op1 = opPop()
        if (type(op1) == int and type(op2) == int) or (type(op1) == float and type(op2) == float):
            if op1 < op2:
                opPush(True)
            else:
                opPush(False)
    else:
        print("lt ERROR")

def gt():
    if len(opstack) > 1:
        op2 = opPop()
        op1 = opPop()
        if (type(op1) == int and type(op2) == int) or (type(op1) == float and type(op2) == float):
            if op1 > op2:
                opPush(True)
            else:
                opPush(False)
    else:
        print("gt ERROR")

#--------------------------- 15% -------------------------------------
# String operators: define the string operators length, get, getinterval, put
def length():
    if len(opstack) > 0:
        string = opPop()
        if type(string) == str:
            opPush(len(string) - 2)
    else:
        print("length ERROR")

def get(): #fix to ascii value
    if len(opstack) > 1:
        i = opPop()
        i += 1
        string = opPop()
        if type(string) == str and type(i) == int:
            opPush(ord(string[i]))
    else:
        print("get ERROR")

def getinterval(): #fix
    if len(opstack) > 2:
        x = opPop()
        i = opPop()
        i += 1  # to account for the ()
        string = opPop()
        if type(string) == str and type(i) == int and type(x) == int:
            # opPush(string[i:(i+x)])
            opstack.append("(" + string[i:(i+x)] + ")")
            # print(string[i:(i+x)]) testing
    else:
        print("getinterval ERROR")

def put(): # revise for part 2
    if len(opstack) > 0:
        char = opPop()
        i = opPop()
        string = opPop()
        if type(string) == str and type(i) == int and type(char) == int:
            copstack = []
            newstring = string[:i+1] + chr(char) + string[i+2:]
            #print(newstring) #test
            for i in range(len(opstack)):
                op = opPop()
                if isinstance(op,str) and id(op) == id(string):
                    # if id(op) == id(string):
                    op = newstring
                copstack.append(op)
            for i in range(len(copstack)):
                opPush(copstack.pop())
            #opPush(newstring)
        if len(dictstack) > 0:
            #cdstack = []
            for i in range(len(dictstack)):
                dic = dictstack[-1-i]
                for key,value in dic.items():
                    if id(value) == id(string):
                        dic[key] = newstring
                # if id(string) in dic.keys():
                #     dic[id(string)] = newstring
            #     d = dictstack.pop
            #     for key,value in d.items():
            #         if id(value) == id(string):
            #             d[key] = newstring
            #     cdstack.append(d)
            # for i in range(len(cdstack)):
            #     dic = cdstack.pop()
            # dictPush(dic)
            #opstack = copstack
            #string[i] = char
            #return newString
    else:
        print("put ERROR")
#--------------------------- 25% -------------------------------------
# Define the stack manipulation and print operators: dup, copy, pop, clear, exch, roll, stack
def dup():
    if len(opstack) > 0:
        opPush(opstack[-1])
    else:
        print("dup ERROR")

def copy():
    if len(opstack) > 0:
        x = opPop()
        opcopy = []
        for i in range(x):
            opcopy.insert(0, opstack[-1-i])
        for i in range(len(opcopy)):
            opPush(opcopy[i])
    else:
        print("copy ERROR")

def pop():
    if len(opstack) > 0:
        return opPop()
    else:
        print("pop ERROR")

# def clear():
#     if len(opstack) > 0:
#         while len(opstack) != 0:
#             opPop()
#     else:
#         print("clear ERROR")

def clear():
    del opstack[:]
    del dictstack[:]


def exch():
    if len(opstack) > 1:
        op2 = opPop()
        op1 = opPop()
        opPush(op2)
        opPush(op1)
    else:
        print("exch ERROR")

def roll(): # check again
    if len(opstack) > 0:
        i = opPop()
        x = opPop()
        if type(x) == int and type(i) == int and x < len(opstack) and x > 0:
            opcopy = opstack[-x:]
            opstack[-x:] = []
            if i > 0:
                op2copy = opcopy[:i]
                opcopy[:i] = []
                for i in op2copy:
                    opcopy.append(i)
            else:
                op2copy = opcopy[:-i]
                opcopy[:-i] = []
                #reversed(op2copy)
                for i in op2copy:
                    opcopy.append(i)
            for i in opcopy:
                opstack.append(i)
        else:
            print("roll ERROR")

            # opcopy = opstack[-x:]
            # opstack[-x:] = []
            # index = i - x
            # for i in opcopy:
            #     opstack.insert(-index, i)


def stack():
    if len(opstack) > 0:
        oplist = []
        copstack = reversed(opstack)
        for i in copstack:
            oplist.append(i)
        for j in oplist:
            print(j)
    else:
        print("stack ERROR")

#--------------------------- 20% -------------------------------------
# Define the dictionary manipulation operators: psDict, begin, end, psDef
# name the function for the def operator psDef because def is reserved in Python. Similarly, call the function for dict operator as psDict.
# Note: The psDef operator will pop the value and name from the opstack and call your own "define" operator (pass those values as parameters).
# Note that psDef()won't have any parameters.

def psDict(): #fix
    if len(opstack) > 0:
        #
        # name = opPop()
        size = opPop()
        if type(size) == int:  # fix (make it ignore)
            opPush({})
        #
        # dictPush(opPop())
    else:
        print("psDict ERROR")

def begin():
    if len(opstack) > 0:
        opPop() # added recently
        dictPush({})
    else:
        print("begin ERROR")

def end():
    if len(dictstack) > 0:
        dictPop()
    else:
        print("end ERROR")

def psDef():
    if len(opstack) > 1:
        value = opPop()
        name = opPop()
        if name[0] == "/":
            define(name, value)
        else:
            opPush(name)
            opPush(value)
    else:
        print("psDef ERROR")

def psFor():
    if len(opstack) > 3:
        op = opPop()
        stop = opPop()
        step = opPop()
        start = opPop()
        if isinstance(op,list) and isinstance(stop, int) and isinstance(step, int) and isinstance(start, int):
            if step > 0 and start <= stop:
                for i in range(start, stop + 1, step):
                    opPush(i)
                    interpretSPS(op)  # double check this again...
            elif step < 0 and start > stop:
                for i in range(start, stop - 1, step):
                    opPush(i)
                    interpretSPS(op)
        else:
            opPush(start)
            opPush(step)
            opPush(stop)
            opPush(op)
    else:
        print("psFor ERROR")

def psIf():
    if len(opstack) > 1:
        op2 = opPop()
        op1 = opPop()
        if isinstance(op2,list) and op1:  # check this again
            interpretSPS(op2)
        else:
            opPush(op1)
            opPush(op2)
    else:
        print("psIf ERROR")

def psIfelse():
    if len(opstack) > 2:
        op3 = opPop()
        op2 = opPop()
        op1 = opPop()
        if isinstance(op3,list) and isinstance(op2,list):
            if op1:
                interpretSPS(op2)
            else:
                interpretSPS(op3)
        else:
            opPush(op1)
            opPush(op2)
            opPush(op3)
    else:
        print("psIfelse ERROR")

def tokenize(s):
    return re.findall("/?[a-zA-Z()][a-zA-Z0-9_()]*|[-]?[0-9]+|[}{]+|%.*|[^ \t\n]", s)

def isInt(c):
    try:
        int(c)
        return True
    except:
        return False

# complete this function
# The it argument is an iterator.
# The sequence of return characters should represent a list of properly nested
# tokens, where the tokens between '{' and '}' is included as a sublist. If the
# parenteses in the input iterator is not properly nested, returns False.
def groupMatching2(it):
    res = []
    for c in it:
        if c == '}':
            return res
        elif c=='{':
            # Note how we use a recursive call to group the tokens inside the
            # inner matching parenthesis.
            # Once the recursive call returns the code array for the inner
            # paranthesis, it will be appended to the list we are constructing
            # as a whole.
            res.append(groupMatching2(it))
        elif isInt(c):
            res.append(int(c))
        elif c == 'true':
            res.append(True)
        elif c == 'false':
            res.append(False)
        else:
            res.append(c)
    return False




# Complete this function
# Function to parse a list of tokens and arrange the tokens between { and } braces
# as code-arrays.
# Properly nested parentheses are arranged into a list of properly nested lists.
def parse(L):
    res = []
    it = iter(L)
    for c in it:
        if c == '}':  #non matching closing paranthesis; return false since there is
                    # a syntax error in the Postscript code.
            return False
        elif c == '{':
            res.append(groupMatching2(it))
        elif isInt(c):
            res.append(int(c))
        elif c == 'true':
            res.append(True)
        elif c == 'false':
            res.append(False)
        else:
            res.append(c)
    return res

# Write the necessary code here; again write
# auxiliary functions if you need them. This will probably be the largest
# function of the whole project, but it will have a very regular and obvious
# structure if you've followed the plan of the assignment.
#

opFun = {"add":add,"sub":sub,"mul":mul,"div":div,"mod":mod,"eq":eq,"lt":lt,"gt":gt,"length":length,"get":get,"getinterval":getinterval,"put":put,"dup":dup,"copy":copy,"pop":pop,"clear":clear,"exch":exch,"roll":roll,"stack":stack,"dict":psDict,"begin":begin,"end":end,"def":psDef,"for":psFor,"if":psIf,"ifelse":psIfelse}
def interpretSPS(code): # code is a code array
    for c in code:
        if isinstance(c, list): # should be the same as [type(c) == list]
            opPush(c)
        elif isinstance(c, int):
            opPush(c)
        elif isinstance(c, bool):
            opPush(c)
        elif c in opFun.keys():   # i think its missing pushing str (fixed)
            opFun[c]()
        elif isinstance(c, str):
            if c[0] == '/':
                opPush(c)
            else: # in dict use lookup...
                op = lookup(c)
                if op == None: # fix this part...
                    opPush(c) # changed from op/ changed from c
                elif isinstance(op, list):
                    interpretSPS(op) # changed from interpreter
                else:
                    opPush(op)

# Copy this to your HW4_part2.py file>
def interpreter(s):  # s is a string
    interpretSPS(parse(tokenize(s)))


#clear opstack and dictstack
# def clear():
#     del opstack[:]
#     del dictstack[:]


#testing

input1 = """
        /square {
               dup mul
        } def 
        (square)
        4 square 
        dup 16 eq 
        {(pass)} {(fail)} ifelse
        stack 
        """

input2 ="""
    (facto) dup length /n exch def
    /fact {
        0 dict begin
           /n exch def
           n 2 lt
           { 1}
           {n 1 sub fact n mul }
           ifelse
        end 
    } def
    n fact stack
    """

input3 = """
        /fact{
        0 dict
                begin
                        /n exch def
                        1
                        n -1 1 {mul} for
                end
        } def
        6
        fact
        stack
    """

input4 = """
        /lt6 { 6 lt } def 
        1 2 3 4 5 6 4 -3 roll    
        dup dup lt6 {mul mul mul} if
        stack 
        clear
    """

input5 = """
        (CptS355_HW5) 4 3 getinterval 
        (355) eq 
        {(You_are_in_CptS355)} if
         stack 
        """

input6 = """
        /pow2 {/n exch def 
               (pow2_of_n_is) dup 8 n 48 add put 
                1 n -1 1 {pop 2 mul} for  
              } def
        (Calculating_pow2_of_9) dup 20 get 48 sub pow2
        stack
        """


# --------------------------------------------------------------------
## Sample tests #
# --------------------------------------------------------------------

def testPut():
    opPush("(This is a test _)")
    dup()
    opPush("/s")
    exch()
    psDef()
    dup()
    opPush(15)
    opPush(48)
    put()
    if lookup("s") != "(This is a test 0)" or opPop()!= "(This is a test 0)":
        return False
    return True


##########################################
#------- Part 1 TEST CASES--------------
def printTestInput(funcName, input):
    print("\n-------------------------")
    print(funcName,":")
    print("input:", input)

def clearStacks():
    del opstack[:]
    del dictstack[:]

# 2 pts
def testDefine():
    clearStacks()
    printTestInput("define-1",'/n1 4 def n1')
    dictPush({})
    define("/n1", 4)
    if lookup("n1") != 4:
        print("FAIL - deduct:",-2)
        return False
    print("PASS")
    return True

# 3 pts
def testDefine2():
    clearStacks()
    printTestInput("define-2", '/n1 4 def /n1 5 def n1')
    dictPush({})
    define("/n1", 4)
    define("/n1", 5)
    if lookup("n1") != 5:
        print("FAIL - deduct:", -3)
        return False
    print("PASS")
    return True

# 3 pts
def testLookup():
    clearStacks()
    printTestInput("lookup-1", '/v 3 def /v 4 def /v 5 def v ')
    dictPush({'/v':3})
    dictPush({'/v':4})
    dictPush({'/v':5})
    if lookup("v") != 5:
        print("FAIL - deduct:", -3)
        return False
    print("PASS")
    return True

#2 pts
def testLookup2():
    clearStacks()
    printTestInput("lookup-2", '/a 355 def /a (355) def a')
    dictPush({'/a':355})
    dictPush({'/a':'(355)'})
    if lookup("a") != '(355)':
        print("FAIL - deduct:", -2)
        return False
    print("PASS")
    return True

#Arithmatic operator tests
#2 pts
def testAdd():
    clearStacks()
    printTestInput("add", '9 -2 add')
    opPush(9)
    opPush(-2)
    add()
    if opPop() != 7:
        print("FAIL - deduct:", -2)
        return False
    print("PASS")
    return True

#2 pts
def testSub():
    clearStacks()
    printTestInput("sub", '10 2 sub')
    opPush(10)
    opPush(2)
    sub()
    x = opPop()
    if x == -8:
        print("FAIL - The order of the arguments for 'sub' are reversed - deduct:", -1)
        return False
    elif x != 8:
        print("FAIL - deduct:", -2)
        return False
    print("PASS")
    return True


#2 pts
def testMul():
    clearStacks()
    printTestInput("mul", '2 40 mul')
    opPush(2)
    opPush(40)
    mul()
    if opPop() != 80:
        print("FAIL - deduct:", -2)
        return False
    print("PASS")
    return True

#2 pts
def testDiv():
    clearStacks()
    printTestInput("div", '12 3 div')
    opPush(12)
    opPush(3)
    div()
    x = opPop()
    if x == 0.25:
        print("FAIL - The order of the arguments for 'div' are reversed - deduct:", -1)
        return False
    elif x != 4:
        print("FAIL - deduct:", -2)
        return False
    print("PASS")
    return True

#2 pts
def testMod():
    clearStacks()
    printTestInput("mod", '10 4 mod')
    opPush(10)
    opPush(4)
    mod()
    x = opPop()
    if x == 0:
        print("FAIL - The order of the arguments for 'mod' are reversed - deduct:", -1)
        return False
    elif x != 2:
        print("FAIL - deduct:", -2)
        return False
    print("PASS")
    return True

#Comparison operators tests
# 2 pts
def testEq1():
    clearStacks()
    printTestInput("eq", '6 6 eq')
    opPush(6)
    opPush(6)
    eq()
    if opPop() != True:
        print("FAIL - deduct:", -2)
        return False
    print("PASS")
    return True

# 1 pts
def testEq2():
    clearStacks()
    printTestInput("eq", '(6) (6) eq')
    opPush('(6)')
    opPush('(6)')
    eq()
    if opPop() != True:
        print("FAIL - deduct:", -1)
        return False
    print("PASS")
    return True

# 3 pts
def testLt():
    clearStacks()
    printTestInput("lt", '3 6 lt')
    opPush(3)
    opPush(6)
    lt()
    if opPop() != True:
        print("FAIL - deduct:", -3)
        return False
    print("PASS")
    return True
# 3 pts
def testGt():
    clearStacks()
    printTestInput("gt", '4 5 gt')
    opPush(4)
    opPush(5)
    gt()
    if opPop() != False:
        print("FAIL - deduct:", -3)
        return False
    print("PASS")
    return True

# 4 pts
#String operator tests
def testLength():
    clearStacks()
    printTestInput("length", '(CptS355 HW4) length')
    opPush("(CptS355 HW4)")
    length()
    l = opPop()
    if l == 13:
        print("FAIL - (should not count paranthesis as part of the string) deduct:", -2)
        return False
    elif l != 11:
        print("FAIL - deduct:", -4)
        return False
    print("PASS")
    return True

# 5 pts
def testGet():
    clearStacks()
    printTestInput("get", '(CptS355 HW4) 10 get')
    opPush("(CptS355 HW4)")
    opPush(10)
    get()
    c = opPop()
    if c == 87:
        print("FAIL - (delimiter paranthesis are not part of the string; the 10th character is 4 ; not W) deduct:", -3)
        return False
    elif c== 'W' or c =='4':
        print("FAIL - (get should push the ASCII value of the character onto the stack) deduct:", -3)
        return False
    elif c != 52:
        print("FAIL - deduct:", -5)
        return False
    print("PASS")
    return True

# 5 pts
def testGetinterval():
    clearStacks()
    printTestInput("getinterval", '(CptS355 HW4) 8 3 getinterval')
    opPush("(CptS355 HW4)")
    opPush(8)
    opPush(3)
    getinterval()
    c = opPop()
    if c == '( HW)':
        print("FAIL - (delimiter paranthesis are not part of the string; getinterval returns (HW4) ; not ( HW);  deduct:", -3)
        return False
    elif c == 'HW4':
        print("FAIL - (the returned substring should be enclosed in paranthesis;  deduct:", -3)
        return False
    elif c != '(HW4)':
        print("FAIL - (In this test case 8 is the starting index for the substring and 3 is the length of the substring. deduct:", -5)
        return False
    print("PASS")
    return True

#stack manipulation functions
# 3 pts
def testDup():
    clearStacks()
    printTestInput("dup", '(CptS355 HW4) dup')
    opPush("(CptS355 HW4)")
    dup()
    if opPop()!=opPop():
        print("FAIL - deduct:", -3)
        return False
    print("PASS")
    return True
# 5 pts
def testExch():
    clearStacks()
    printTestInput("exch", '"/x" 5 exch')
    opPush("/x")
    opPush(5)
    exch()
    if opPop()!="/x" and opPop()!=5:
        print("FAIL - deduct:", -5)
        return False
    print("PASS")
    return True
# 2 pts
def testPop():
    clearStacks()
    printTestInput("pop", '10 pop')
    l1 = len(opstack)
    opPush(10)
    pop()
    l2 = len(opstack)
    if l1!=l2:
        print("FAIL - deduct:", -2)
        return False
    print("PASS")
    return True

# 5 pts
def testCopy():
    clearStacks()
    printTestInput("copy", 'true 1 (12) 3 4 3 copy')
    opPush(True)
    opPush(1)
    opPush('(12)')
    opPush(3)
    opPush(4)
    opPush(3)
    copy()
    if opPop()!=4 and opPop()!=3 and opPop()!='(12)' and opPop()!=4 and opPop()!=3 and opPop()!='(12)' and opPop()!=1:
        print("FAIL - deduct:", -5)
        return False
    print("PASS")
    return True

# 3 pts
def testClear():
    clearStacks()
    printTestInput("clear", '10 clear')
    opPush(10)
    opPush("/x")
    clear()
    if len(opstack)!=0:
        print("FAIL - deduct:", -3)
        return False
    print("PASS")
    return True

#dictionary stack operators
# 3 pts
def testDict():
    clearStacks()
    printTestInput("dict", '1 dict')
    opPush(1)
    psDict()
    if opPop()!={}:
        print("FAIL - deduct:", -3)
        return False
    elif len(opstack)>0:
        print("FAIL - psDict should pop the size argumen from the stack ; deduct:", -2)
        return False
    print("PASS")
    return True

# 5 pts
def testBeginEnd():
    clearStacks()
    printTestInput("begin-end", '/x 3 def 1 dict begin /x 4 end x')
    dictPush({})
    opPush("/x")
    opPush(3)
    psDef()
    opPush(1)
    psDict()
    begin()
    opPush("/x")
    opPush(4)
    psDef()
    end() #SUT
    if lookup("x")!=3:
        print("FAIL - deduct:", -5)
        return False
    print("PASS")
    return True

# 5 pts
def testpsDef():
    clearStacks()
    printTestInput("psDef", '/x 10 def /x 20 def x end')
    dictPush({})
    opPush("/x")
    opPush(10)
    psDef()
    opPush("/x")
    opPush(20)
    psDef()
    if lookup("x")==10:
        print("FAIL - deduct:", -4, "(psDef should overwrite the existing definition : -5 +1 partial points)")
        return False
    elif lookup("x")!=20:
        print("FAIL - deduct:", -5)
        return False

    print("PASS")
    return True

# 5 pts
def testpsDef2():
    clearStacks()
    printTestInput("psDef-2", '/x 10 def 2 dict begin /y 20 def x')
    dictPush({})
    opPush("/x")
    opPush(10)
    psDef()
    opPush(2)
    psDict()
    begin()
    opPush("/y")
    opPush(20)
    psDef()
    if lookup("x")!=10:
        end()
        print("FAIL - deduct:", -5)
        return False
    end()
    print("PASS")
    return True

# 6 pts
def testpsDef3():
    clearStacks()
    printTestInput("psDef3", '/x 3 def 1 dict begin /x 30 def 1 dict begin /x 300 def end x')
    # define x in the bottom dictionary
    opPush("/x")
    opPush(3)
    psDef()
    opPush(1)
    psDict()
    begin()
    # define x in the second dictionary
    opPush("/x")
    opPush(30)
    psDef()
    opPush(1)
    psDict()
    begin()
    # define x in the third dictionary
    opPush("/x")
    opPush(300)
    psDef()
    end()
    if lookup("x")!=30:
        end()
        print("FAIL - deduct:", -6)
        return False
    end()
    print("PASS")
    return True



def main_part1():
    testCases = [('define',testDefine),('define2',testDefine2),('lookup',testLookup),('lookup2',testLookup2),('add', testAdd), ('sub', testSub),('mul', testMul),('div', testDiv), \
                 ('mod', testMod),('eq-1',testEq1),('eq-2',testEq2),('lt',testLt),('gt', testGt),  ('length', testLength),('get', testGet),('getinterval', testGetinterval),
                 ('dup', testDup), ('exch', testExch), ('pop', testPop), ('copy', testCopy), \
                 ('clear', testClear), ('dict', testDict), ('begin_end', testBeginEnd), ('psDef', testpsDef), ('psDef2', testpsDef2),('psDef3', testpsDef3),('put',testPut)]
    # add you test functions to this list along with suitable names
    failedTests = [testName for (testName, testProc) in testCases if not testProc()]
    if failedTests:
        return ('Some tests failed\n', failedTests)
    else:
        return ('All part-1 tests OK\n')


if __name__ == '__main__':
    print(main_part1())

    # testing ....
print("\n-------------------------")
clear()
print("input1")
print(tokenize(input1))
print(parse(tokenize(input1)))
interpreter(input1)


print("\n-------------------------")
clear()
print("input2")
print(tokenize(input2))
print(parse(tokenize(input2)))
interpreter(input2)

print("\n-------------------------")
clear()
print("input3")
print(tokenize(input3))
print(parse(tokenize(input3)))
interpreter(input3)
#
print("\n-------------------------")
clear()
print("input4")
print(tokenize(input4))
print(parse(tokenize(input4)))
interpreter(input4)
#
print("\n-------------------------")
clear()
print("input5")
print(tokenize(input5))
print(parse(tokenize(input5)))
interpreter(input5)
#
print("\n-------------------------")
clear()
print("input6")
print(tokenize(input6))
print(parse(tokenize(input6)))
interpreter(input6)

# additional test

input7 = """
        1 2 sub 5 add -1 -4 mul eq 
        {(My_name_is_Jimmy)} {(My_name_is_not_Jimmy)} ifelse
        dup length stack 
        """

print("\n-------------------------")
clear()
print("input7")
print(tokenize(input7))
print(parse(tokenize(input7)))
interpreter(input7)

input8 = """1 -1 -5 {6 mul} for """

print("\n-------------------------")
clear()
print("input8")
print(tokenize(input8))
print(parse(tokenize(input8)))
interpreter(input8)
stack()