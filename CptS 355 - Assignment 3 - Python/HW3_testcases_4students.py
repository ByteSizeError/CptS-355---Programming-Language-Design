#######################################
#############  TEST CASES##############
#######################################

def printTestOutput1(testNum,_input,_output,func2Test,deductScore):
    print('Test-' + str(testNum) + ':')
    if func2Test(_input) == _output:
        print('PASS\t', 'input:', _input, '\n actual output:', func2Test(_input))
    else:  
        print('FAIL\t','(DEDUCT:', deductScore, ') input:', _input , '\n actual output:', func2Test(_input), '\n expected output:', _output)

def printTestOutput2(testNum,_input1,_input2,_output,func2Test,deductScore):
    print('Test-' + str(testNum) + ':')
    if func2Test(_input1,_input2) == _output:
        print('PASS\t', 'input:(', _input1, _input2,')', '\n actual output:', func2Test(_input1,_input2))
    else:  
        print('FAIL\t','(DEDUCT:', deductScore, ') input:(', _input1, _input2, ')' , '\n actual output:', func2Test(_input1,_input2), '\n expected output:', _output)


#3 tests - each 5pts  (TOTAL : 15 pts)
def testbusStops():
    input1 = {"Lentil": ["Chinook", "Orchard", "Valley", "Emerald","Providence", "Stadium", "Main", "Arbor", "Sunnyside", "Fountain", "Crestview", "Wheatland", "Walmart", "Bishop", "Derby", "Dilke"],"Wheat": ["Chinook", "Orchard", "Valley", "Maple","Aspen", "TerreView", "Clay", "Dismores", "Martin", "Bishop", "Walmart", "PorchLight", "Campus"],"Silver": ["TransferStation", "PorchLight", "Stadium", "Bishop","Walmart", "Shopco", "RockeyWay"],"Blue": ["TransferStation", "State", "Larry", "TerreView","Grand", "TacoBell", "Chinook", "Library"],"Gray": ["TransferStation", "Wawawai", "Main", "Sunnyside","Crestview", "CityHall", "Stadium", "Colorado"]}
    input2 = {"Green": ["Emerald","Providence", "MiddleSchool", "Crestview", "Wheat", "Shopco", "Bishop", "Derby"],
              "Lentil": ["Chinook", "Orchard", "Valley", "Maple","Aspen", "TerreView", "Dismores", "Martin", "Bishop", "Walmart", "Campus"],
              "Gold": ["TransferStation", "PorchLight", "Stadium", "Bishop", "Shopco", "RockeyWay"],
              "Blue": ["TransferStation", "State", "Larry", "TerreView","Grand", "TacoBell", "Chinook", "Library"],
              "Bold": ["TransferStation", "Wawawai", "Main", "Sunnyside","PostOffice","Stadium", "Colorado"]}
    input3 = {"Wheat": ["Chinook", "Orchard", "Valley", "Emerald","Providence", "Stadium", "Main", "Harbor", "Walmart", "Bishop", "Derby", "Dilke"],"Gold": ["RecCenter", "TerreView", "Clay", "Dismores", "Martin", "Bishop", "Walmart", "Shopco","PorchLight", "Campus"],"Green": ["TransferStation", "Shopco", "RockeyWay"],"Blue": ["TransferStation", "State", "Larry", "TerreView","Grand", "TacoBell", "Chinook", "Library"]}
    
    output1 = {'Chinook': ['Blue', 'Lentil', 'Wheat'], 'Orchard': ['Lentil', 'Wheat'], 'Valley': ['Lentil', 'Wheat'], 'Emerald': ['Lentil'], 'Providence': ['Lentil'], 'Stadium': ['Gray', 'Lentil', 'Silver'], 'Main': ['Gray', 'Lentil'], 'Arbor': ['Lentil'], 'Sunnyside': ['Gray', 'Lentil'], 'Fountain': ['Lentil'], 'Crestview': ['Gray', 'Lentil'], 'Wheatland': ['Lentil'], 'Walmart': ['Lentil', 'Silver', 'Wheat'], 'Bishop': ['Lentil', 'Silver', 'Wheat'], 'Derby': ['Lentil'], 'Dilke': ['Lentil'], 'Maple': ['Wheat'], 'Aspen': ['Wheat'], 'TerreView': ['Blue', 'Wheat'], 'Clay': ['Wheat'], 'Dismores': ['Wheat'], 'Martin': ['Wheat'], 'PorchLight': ['Silver', 'Wheat'], 'Campus': ['Wheat'], 'TransferStation': ['Blue', 'Gray', 'Silver'], 'Shopco': ['Silver'], 'RockeyWay': ['Silver'], 'State': ['Blue'], 'Larry': ['Blue'], 'Grand': ['Blue'], 'TacoBell': ['Blue'], 'Library': ['Blue'], 'Wawawai': ['Gray'], 'CityHall': ['Gray'], 'Colorado': ['Gray']}
    output2 = {'Crestview': ['Green'], 'Emerald': ['Green'], 'Providence': ['Green'], 'MiddleSchool': ['Green'], 'Wheat': ['Green'], 'Shopco': ['Gold', 'Green'], 'Bishop': ['Gold', 'Green', 'Lentil'], 'Derby': ['Green'], 'Chinook': ['Blue', 'Lentil'], 'Orchard': ['Lentil'], 'Valley': ['Lentil'], 'Maple': ['Lentil'], 'Aspen': ['Lentil'], 'TerreView': ['Blue', 'Lentil'], 'Dismores': ['Lentil'], 'Martin': ['Lentil'], 'Walmart': ['Lentil'], 'Campus': ['Lentil'], 'TransferStation': ['Blue', 'Bold', 'Gold'], 'PorchLight': ['Gold'], 'Stadium': ['Bold', 'Gold'], 'RockeyWay': ['Gold'], 'State': ['Blue'], 'Larry': ['Blue'], 'Grand': ['Blue'], 'TacoBell': ['Blue'], 'Library': ['Blue'], 'Wawawai': ['Bold'], 'Main': ['Bold'], 'Sunnyside': ['Bold'], 'PostOffice': ['Bold'], 'Colorado': ['Bold']}
    output3 = {'Chinook': ['Blue', 'Wheat'], 'Emerald': ['Wheat'], 'Orchard': ['Wheat'], 'Valley': ['Wheat'], 'Providence': ['Wheat'], 'Stadium': ['Wheat'], 'Main': ['Wheat'], 'Harbor': ['Wheat'], 'Walmart': ['Gold', 'Wheat'], 'Bishop': ['Gold', 'Wheat'], 'Derby': ['Wheat'], 'Dilke': ['Wheat'], 'RecCenter': ['Gold'], 'TerreView': ['Blue', 'Gold'], 'Clay': ['Gold'], 'Dismores': ['Gold'], 'Martin': ['Gold'], 'Shopco': ['Gold', 'Green'], 'PorchLight': ['Gold'], 'Campus': ['Gold'], 'TransferStation': ['Blue', 'Green'], 'RockeyWay': ['Green'], 'State': ['Blue'], 'Larry': ['Blue'], 'Grand': ['Blue'], 'TacoBell': ['Blue'], 'Library': ['Blue']}

    print('----------------------\nbusStops Tests: ')
    printTestOutput1(1,input1,output1,busStops,5)
    printTestOutput1(2,input2,output2,busStops,5)
    printTestOutput1(3,input3,output3,busStops,5)
    return


# 3 tests - 3pts, 3pts, 4pts (TOTAL 10pts)
def testaddDict():
    input1 = {'Mon':{'355':2,'451':1,'360':2},'Tue':{'451':2,'360':3}, 'Thu':{'355':3,'451':2,'360':3}, 'Fri':{'355':2}, 'Sun':{'355':1,'451':3,'360':1}}
    input2 = {'Tue':{'427':2,'455':5,'411':2},'Wed':{'411':2,'360':3}, 'Fri':{'455':3,'411':2,'360':8}, 'Sat':{'455':2,'360':2}, 'Sun':{'411':3,'427':3}}
    input3 = {'Mon':{'322':2,'302':1},'Tue':{'302':2,'355':3}, 'Thu':{'322':4,'355':2,'317':3}, 'Fri':{'317':4, '322':3,'355':3}, 'Sun':{'355':4,'322':3,'302':1}}

    output1 = {'355': 8, '451': 8, '360': 9}
    output2 = {'427': 5, '455': 10, '411': 9, '360': 13}
    output3 = {'322': 12, '302': 4, '355': 12, '317': 7}
    print('----------------------\naddDict Tests: ')

    printTestOutput1(1,input1,output1,addDict,3)
    printTestOutput1(2,input2,output2,addDict,3)
    printTestOutput1(3,input3,output3,addDict,4)
    return


#total 2 tests - 5pts, 5pts (total 10pts)
def testaddDictN():
    input1 = [{'Mon':{'355':2,'360':2},'Tue':{'451':2,'360':3},'Thu':{'360':3}, 'Fri':{'355':2, '302':4}, 'Sun':{'355':1}},\
              {'Tue':{'360':2},'Wed':{'355':2},'Fri':{'360':3, '355':1}},\
              {'Mon':{'360':5},'Wed':{'451':4},'Thu':{'355':3},'Fri':{'360':6}, 'Sun':{'355':5, '302':3}}]
    
    input2 = [{'Tue':{'421':2,'455':5,'411':2},'Wed':{'411':2,'460':3}, 'Fri':{'455':3,'411':2,'360':8}, 'Sat':{'455':2,'360':2}, 'Sun':{'411':3,'421':3}},\
              {'Mon':{'421':2,'411':1},'Tue':{'460':2,'455':3}, 'Thu':{'421':4,'455':2,'411':3}, 'Fri':{'460':4, '421':3,'455':3}, 'Sun':{'455':4,'421':3,'411':1}},\
              {'Wed':{'460':5},'Thu':{'411':4},'Fri':{'455':3},'Sat':{'460':6}, 'Sun':{'455':5}}]

    output1 = {'355': 16, '360': 24, '451': 6, '302': 7}
    output2 = {'421': 17, '455': 30, '411': 18, '460': 20, '360': 10}
    print('----------------------\naddDictN Tests: \n')

    printTestOutput1(1,input1,output1,addDictN,5)
    printTestOutput1(2,input2,output2,addDictN,5)
    return

#5 tests - each 1 pts (TOTAL 5pts)
def testsearchDicts():
    L = [{"x":1,"y":True,"z":"found"},{"z":"here","t":5},{"y":False},{"t":9}]
    print('----------------------\nsearchDicts Tests: ')

    printTestOutput2 (1,L,'x',1,searchDicts,1)
    printTestOutput2 (2,L,'y',False,searchDicts,1)
    printTestOutput2 (3,L,'z','here',searchDicts,1)
    printTestOutput2 (4,L,'t',9,searchDicts,1)
    printTestOutput2 (5,L,'k',None,searchDicts,1)
    return

#5 tests - each 2 pts (TOTAL 10pts)
def testsearchDicts2 ():
    L = [(0,{"x":0,"y":False,"z":"zero"}), (0,{"x":1, "z":"two"}), (1,{"y":True}), (1,{"x":3}), (2,{"t":30, "k":20}), (3,{}), (5,{"t":10})]
    print('----------------------\nsearchDicts2 Tests: ')

    printTestOutput2 (1,L,'x',3,searchDicts2 ,2)
    printTestOutput2 (2,L,'y',False,searchDicts2 ,2)
    printTestOutput2 (3,L,'z','two',searchDicts2 ,2)
    printTestOutput2 (4,L,'t',10,searchDicts2 ,2)
    printTestOutput2 (5,L,'k',None,searchDicts2 ,2)
    return

def compareLists (L1,L2):
    if len(L1)!=len(L2):
        return False
    else:
        for item in L1:
            if not (item  in L2):
                return False
    return True

def printsubsetsTestOutput(testNum,_input,_output,deductScore):
    print('Test-' + str(testNum) + ':')
    if compareLists(subsets(_input) , _output):
        print('PASS\t', 'input:', _input, '\n actual output:', subsets(_input))
    else:
        print('FAIL\t','(DEDUCT:', deductScore, ') input:', _input , '\n expected output:', _output, '\n actual output:', subsets(_input))


#4 tests - each 5pts(TOTAL 20pts)
def testsubsets():
    input1 = [1,2,3]
    input2 = ['a',1,'b',(2,3,4)]
    input3 = [(1),(2,3),(4,5,6),(7,8,9,10)]
    input4 = [(1,'a'),(2,'b'),True,'d']

    output1 = [[], [1], [2], [3], [1, 2], [1, 3], [2, 3], [1, 2, 3]]
    output2 = [[], ['a'], [1], ['b'], [(2, 3, 4)], ['a', 1], ['a', 'b'], [1, 'b'], ['a', (2, 3, 4)], [1, (2, 3, 4)], ['b', (2, 3, 4)], ['a', 1, 'b'], ['a', 1, (2, 3, 4)], ['a', 'b', (2, 3, 4)], [1, 'b', (2, 3, 4)], ['a', 1, 'b', (2, 3, 4)]]
    output3 = [[], [1], [(2, 3)], [(4, 5, 6)], [(7, 8, 9, 10)], [1, (2, 3)], [1, (4, 5, 6)], [(2, 3), (4, 5, 6)], [1, (7, 8, 9, 10)], [(2, 3), (7, 8, 9, 10)], [(4, 5, 6), (7, 8, 9, 10)], [1, (2, 3), (4, 5, 6)], [1, (2, 3), (7, 8, 9, 10)], [1, (4, 5, 6), (7, 8, 9, 10)], [(2, 3), (4, 5, 6), (7, 8, 9, 10)], [1, (2, 3), (4, 5, 6), (7, 8, 9, 10)]]
    output4 = [[], [(1, 'a')], [(2, 'b')], [True], ['d'], [(1, 'a'), (2, 'b')], [(1, 'a'), True], [(2, 'b'), True], [(1, 'a'), 'd'], [(2, 'b'), 'd'], [True, 'd'], [(1, 'a'), (2, 'b'), True], [(1, 'a'), (2, 'b'), 'd'], [(1, 'a'), True, 'd'], [(2, 'b'), True, 'd'], [(1, 'a'), (2, 'b'), True, 'd']]


    print('----------------------\nsubsets Tests: \n')

    printsubsetsTestOutput (1,input1,output1,5)
    printsubsetsTestOutput (2,input2,output2,5)
    printsubsetsTestOutput (3,input3,output3,5)
    printsubsetsTestOutput (4,input4,output4,5)
    return


#3 tests - 3pts, 3pts, 4pts (TOTAL 10pts)
def testnumPaths():
    print('----------------------\nnumPaths Tests: ')

    printTestOutput2 (1,10,3,55,numPaths,3)
    printTestOutput2 (3,5,4,35,numPaths,3)
    printTestOutput2 (2,9,8,6435,numPaths,4)
    return

def getIterValues(it,num):
    outL = []
    for item in it:
        if item>=num:
            break
        outL.append(item)
    return outL

def printiterPrimesTestOutput(testNum,_input,_output,deductScore):
    print('Test-' + str(testNum) + ':')

    if _input == _output:
        print('PASS\t', '\n actual output:',_input )
    else:
        print('FAIL\t','(DEDUCT:', deductScore, ') ','\n actual output:', _input, '\n expected output:', _output)

#3 tests -  3pts, 3pts, 4pts (total 10pts)
def testiterPrimes():
    iter1 = iterPrimes()
    iter2 = iterPrimes()
    iter3 = iterPrimes()

    input1 = getIterValues(iter1,50)
    input2 = getIterValues(iter2,70)
    input3 = getIterValues(iter3,150)

    output1 = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
    output2 = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67]
    output3 = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149]


    print('----------------------\niterPrimes Tests: \n')

    printiterPrimesTestOutput (1,input1,output1,3)
    printiterPrimesTestOutput (2,input2,output2,3)
    printiterPrimesTestOutput (3,input3,output3,4)

    return


def printnumbersToSumTestOutput(testNum,iter,_input,_output,deductScore):
    print('Test-' + str(testNum) + ':')
    actual_output = numbersToSum(iter,_input)
    if actual_output == _output:
        print('PASS\t', 'input: iterPrimes(),', _input, '\n actual output:', actual_output)
    else:
        print('FAIL\t','(DEDUCT:', deductScore, ') input:iterPrimes(),', _input , '\n actual output:', actual_output, '\n expected output:', _output)

# 5 tests -  2pts each (total 10pts)
def testnumbersToSum():
    iter1 = iterPrimes()
    iter2 = iterPrimes()

    output1 = [2, 3, 5, 7, 11, 13, 17]
    output2 = [19, 23, 29, 31]
    output3 = [2, 3, 5, 7, 11, 13, 17, 19]
    output4 = [23, 29, 31, 37]
    output5 = [41, 43, 47, 53, 59]

    print('----------------------\nnumbersToSum Tests: \n')

    printnumbersToSumTestOutput (1,iter1,77,output1,2)
    printnumbersToSumTestOutput (2,iter1,120,output2,2)
    printnumbersToSumTestOutput (3,iter2,100,output3,2)
    printnumbersToSumTestOutput (4,iter2,161,output4,2)
    printnumbersToSumTestOutput (5,iter2,300,output5,2)


# Store all test functions in a dictionary and call them by calling execute.
testFunctions = [('Test cases for "busStops": ', testbusStops),
                 ('Test cases for "addDict": ',testaddDict), ('Test cases for "addDictN": ',testaddDictN),
                 ('Test cases for "searchDicts": ',testsearchDicts), ('Test cases for "searchDicts2": ',testsearchDicts2),
                 ('Test cases for "subsets": ', testsubsets),('Test cases for "numPaths": ', testnumPaths),
                 ('Test cases for "iterPrimes:', testiterPrimes), ('Test cases for "numbersToSum:', testnumbersToSum)]


for name,testFunc in testFunctions:
    testFunc()
