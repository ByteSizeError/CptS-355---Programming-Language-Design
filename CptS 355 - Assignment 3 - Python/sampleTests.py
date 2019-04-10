
#CptS 355 - Spring 2019
#Assignment 3 - Sample tests


def testaddDict():
    d = {'Mon':{'355':2,'451':1,'360':2},'Tue':{'451':2, '360':3}, 'Thu':{'355':3,'451':2,'360':3}, 'Fri':{'355':2}, 'Sun':{'355':1,'451':3,'360':1}}
    if addDict({}) != {}:
        return False
    if addDict(d) != {'355': 8, '360': 9, '451': 8}:
        return False
    return True

def testnumbersToSum():
    primes = iterPrimes()
    if numbersToSum(primes, 58) != [2, 3, 5, 7, 11, 13]:
        return False
    if numbersToSum(primes, 100) != [17, 19, 23, 29]:
         return False
    return True


testFunctions = {"busStops":testbusStops,  "addDict": testaddDict, "addDictN": testaddDictN, "searchDicts": testsearchDicts, "searchDicts2": testsearchDicts2, "subsets":testsubsets, "numPaths": testnumPaths, "numbersToSum":testnumbersToSum  }
if __name__ == '__main__':
    for testName,testFunc in testFunctions.items():
        print(testName,':  ',testFunc())
        print('---------------------')


#--------------------------------------------------------------------------------------------
#-- Below are the test inputs given in the assignment description
#--------------------------------------------------------------------------------------------

buses = {
"Lentil": ["Chinook", "Orchard", "Valley", "Emerald","Providence", "Stadium", "Main", "Arbor", "Sunnyside", "Fountain", "Crestview", "Wheatland", "Walmart", "Bishop", "Derby", "Dilke"],
"Wheat": ["Chinook", "Orchard", "Valley", "Maple","Aspen", "TerreView", "Clay", "Dismores", "Martin", "Bishop", "Walmart", "PorchLight", "Campus"],
"Silver": ["TransferStation", "PorchLight", "Stadium", "Bishop","Walmart", "Shopco", "RockeyWay"],
"Blue": ["TransferStation", "State", "Larry", "TerreView","Grand", "TacoBell", "Chinook", "Library"],
"Gray": ["TransferStation", "Wawawai", "Main", "Sunnyside","Crestview", "CityHall", "Stadium", "Colorado"]
}
busStops(buses)

weeklylog = {'Mon':{'355':2,'451':1,'360':2},'Tue':{'451':2,'360':3}, 'Thu':{'355':3,'451':2,'360':3}, 'Fri':{'355':2}, 'Sun':{'355':1,'451':3,'360':1}}
addDict(weeklylog)

log = [{'Mon':{'355':2,'360':2},'Tue':{'451':2,'360':3},'Thu':{'360':3}, 'Fri':{'355':2}, 'Sun':{'355':1}},
{'Tue':{'360':2},'Wed':{'355':2},'Fri':{'360':3, '355':1}},
{'Mon':{'360':5},'Wed':{'451':4},'Thu':{'355':3},'Fri':{'360':6}, 'Sun':{'355':5}}]

addDictN(log)

L1 = [{"x":1,"y":True,"z":"found"},{"x":2},{"y":False}]

searchDicts(L1,"x")
searchDicts(L1,"y")
searchDicts(L1,"z")
searchDicts(L1,"t")

L2 = [(0,{"x":0,"y":True,"z":"zero"}),
     (0,{"x":1}),
     (1,{"y":False}),
     (1,{"x":3, "z":"three"}),
     (2,{})]

searchDicts2 (L2,"x")
searchDicts2 (L2,"y")
searchDicts2 (L2,"z")
searchDicts2 (L2,"t")

subsets ([1,2,3])
subsets([(1,"one"),(2,"two")])
subsets([])

numPaths(2,2)
numPaths(3,3)
numPaths(4,5)

primes = iterPrimes()
primes.__next__()
primes.__next__()
primes.__next__()

primes = iterPrimes()
numbersToSum(primes,58)
numbersToSum(primes,100)
