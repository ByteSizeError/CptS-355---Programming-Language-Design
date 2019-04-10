# Jimmy Zheng 11577623
# CPTS 355 Spring 2019
# Assignment 3 Python
# discussed with Osman Bakari

from functools import reduce
from itertools import combinations


debugging = True


def debug(*s):
    if debugging:
        print(*s)


# 1. Dictionaries - busStops(b)
buses = {
    "Lentil": ["Chinook", "Orchard", "Valley", "Emerald","Providence", "Stadium", "Main", "Arbor", "Sunnyside", "Fountain", "Crestview", "Wheatland", "Walmart", "Bishop", "Derby", "Dilke"],
    "Wheat": ["Chinook", "Orchard", "Valley", "Maple","Aspen", "TerreView", "Clay", "Dismores", "Martin", "Bishop", "Walmart", "PorchLight", "Campus"],
    "Silver": ["TransferStation", "PorchLight", "Stadium", "Bishop","Walmart", "Shopco", "RockeyWay"],
    "Blue": ["TransferStation", "State", "Larry", "TerreView","Grand", "TacoBell", "Chinook", "Library"],
    "Gray": ["TransferStation", "Wawawai", "Main", "Sunnyside","Crestview", "CityHall", "Stadium", "Colorado"]
}


def busStops(b):
    busRoutes = { }
    for bus,stops in b.items():
        for stop in stops:
            busRoutes[stop] = busRoutes.get(stop,[]) + [bus]
    for key,value in busRoutes.items():
        value.sort()
    return busRoutes

# debug(”This is my debugging output”)
# 2. (Dictionary)
study = {'Mon':{'355':2,'451':1,'360':2},'Tue':{'451':2,'360':3}, 'Thu':{'355':3,'451':2,'360':3}, 'Fri':{'355':2}, 'Sun':{'355':1,'451':3,'360':1}}


# a) addDict(d)
def addDict(d):
    studyHours = { }
    for days, classes in d.items():
        for course, hours in classes.items():
            studyHours[course] = studyHours.get(course, 0) + hours
    return studyHours


study2 = [{'Mon':{'355':2,'360':2},'Tue':{'451':2,'360':3},'Thu':{'360':3}, 'Fri':{'355':2}, 'Sun':{'355':1}}, {'Tue':{'360':2},'Wed':{'355':2},'Fri':{'360':3, '355':1}}, {'Mon':{'360':5},'Wed':{'451':4},'Thu':{'355':3},'Fri':{'360':6}, 'Sun':{'355':5}}]


# b) addDictN(L)
def addDictHelper(d, d2):
    for course, hours in d.items():
        d[course] = d2.get(course, 0) + hours
    return d


def addDictN(L):
    return reduce(addDictHelper,list(map(addDict,L)))


# 3. Dictionaries and lists
L1 = [{"x": 1, "y": True, "z": "found"}, {"x": 2}, {"y": False}]


# a) searchDicts(L,k)
def searchDicts(L,k):
    for dicts in reversed(L):
        if k in dicts:
            return dicts[k]


L2 = [(0,{"x":0,"y":True,"z":"zero"}),
        (0,{"x":1}),
        (1,{"y":False}),
        (1,{"x":3, "z":"three"}),
        (2,{})]


# b) searchDicts2(tL,k)
def searchDicts2(tL,k):
    def searchDictsHelper(t,index):
        if k in t[1]:
            return t[1][k]
        if index == tL[t[0]][0]:
            return None
        return searchDictsHelper(tL[t[0]],t[0])
    return searchDictsHelper(tL[-1],len(tL)-1)


# 4. (Lists) subsets
def subsets(L):
    temp = []
    for i in range(len(L) + 1):
        temp = temp + [list(map(list, combinations(L, i)))]
    return sum(temp, [])


# 5. (Recursion) numPaths(m,n)
def numPaths(m,n):
    if m < 0 or n < 0:
        return 0
    elif m == 1 or n == 1:
        return 1
    else:
        return numPaths(m-1,n) + numPaths(m,n-1)


# 6. Iterators
# a) iterPrimes()
class iterPrimes():
    def __init__(self):
        self.current = 1
        self.n = 0

    def getNumber(self,number):
        self.n = number

    def isPrime(self):
        if self.n <= 1:
            return False
        if self.n <= 3:
            return True
        if self.n % 2 == 0 or self.n % 3 == 0:
            return False
        x = 5
        while x*x <= self.n:
            if self.n % x == 0 or self.n % (x+2) == 0:
                return False
            x += 6
        return True

    def __next__(self):
        self.n = self.current + 1
        while not self.isPrime():
            self.n += 1
        self.current = self.n
        return self.n

    def __prev__(self):
        self.n = self.current - 1
        while not self.isPrime():
            self.n -= 1
        self.current = self.n
        return self.n

    def __iter__(self):
        return self


# b) numbersToSum(iNumbers,sum)
def numbersToSum(iNumbers,sum):
    temp = iNumbers.__next__()
    sum = sum - temp
    if sum <= 0:
        iNumbers.__prev__()
        return []
    else:
        return [temp] + numbersToSum(iNumbers, sum)


# Test your code:
# 1. Dictionaries - busStops(b)
def testbusStops():
    if busStops(buses) != {'Chinook': ['Blue', 'Lentil', 'Wheat'], 'Orchard': ['Lentil', 'Wheat'], 'Valley': ['Lentil', 'Wheat'], 'Emerald': ['Lentil'], 'Providence': ['Lentil'], 'Stadium': ['Gray', 'Lentil', 'Silver'], 'Main': ['Gray', 'Lentil'], 'Arbor': ['Lentil'], 'Sunnyside': ['Gray', 'Lentil'], 'Fountain': ['Lentil'], 'Crestview': ['Gray', 'Lentil'], 'Wheatland': ['Lentil'], 'Walmart': ['Lentil', 'Silver', 'Wheat'], 'Bishop': ['Lentil', 'Silver', 'Wheat'], 'Derby': ['Lentil'], 'Dilke': ['Lentil'], 'Maple': ['Wheat'], 'Aspen': ['Wheat'], 'TerreView': ['Blue', 'Wheat'], 'Clay': ['Wheat'], 'Dismores': ['Wheat'], 'Martin': ['Wheat'], 'PorchLight': ['Silver', 'Wheat'], 'Campus': ['Wheat'], 'TransferStation': ['Blue', 'Gray', 'Silver'], 'Shopco': ['Silver'], 'RockeyWay': ['Silver'], 'State': ['Blue'], 'Larry': ['Blue'], 'Grand': ['Blue'], 'TacoBell': ['Blue'], 'Library': ['Blue'], 'Wawawai': ['Gray'], 'CityHall': ['Gray'], 'Colorado': ['Gray']}:
        return False
    return True


# 2. (Dictionaries)
# a) addDict(d)
def testaddDict():
    if addDict({})!= {}:
        return False
    if addDict(study) != {'355': 8, '451': 8, '360': 9}:
        return False
    return True


study3 = [{'Mon':{'355':2,'360':2,'317':1},'Tue':{'451':2,'360':3,'220':2},'Thu':{'360':3,'220':2}, 'Fri':{'355':2}, 'Sun':{'355':1}}, {'Tue':{'360':2,'317':2},'Wed':{'355':2},'Fri':{'360':3, '355':1}}, {'Mon':{'360':5},'Wed':{'451':4},'Thu':{'355':3},'Fri':{'360':6}, 'Sun':{'355':5,'317':2}}]


# b) addDictN(L)
def testaddDictN():
    if addDictN(study2) != {'355': 16, '360': 24, '451': 6}:
        return False
    if addDictN(study3) != {'355': 16, '360': 24, '317': 5, '451': 6, '220': 4}:
        return False
    return True


# 3. Dictionaries and lists
L11 = []


# a) searchDicts(L,k)
def testsearchDicts():
    if searchDicts(L1, "x") != 2:
        return False
    if searchDicts(L1,"y") != False:
        return False
    if searchDicts(L1,"z") != "found":
        return False
    if searchDicts(L1,"t") != None:
        return False
    if searchDicts(L11,"x") != None:
        return False
    return True


# b) searchDicts2(tL,k)
def testsearchDicts2():
    if searchDicts2(L2,"x") != 1:
        return False
    if searchDicts2(L2,"y") != False:
        return False
    if searchDicts2(L2,"z") != "zero":
        return False
    if searchDicts2(L2,"t") != None:
        return False
    return True


# 4. (Lists) subsets
def testsubsets():
    if subsets([1,2,3]) != [[],[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]]:
        return False
    if subsets([(1,"one"),(2,"two")]) != [[],[(1,"one")],[(2,"two")],[(1,"one"),(2,"two")]]:
        return False
    if subsets([]) != [[]]:
        return False
    return True


# 5. (Recursion)
def testnumPaths():
    if numPaths(2,2) != 2:
        return False
    if numPaths(3,3) != 6:
        return False
    if numPaths(4,5) != 35:
        return False
    if numPaths(0,0) != 0:
        return False
    return True


# 6. Iterators
# b) numbersToSum(iNumbers,sum)
def testnumbersToSum():
    primes = iterPrimes()
    if numbersToSum(primes, 58) != [2, 3, 5, 7, 11, 13]:
        return False
    if numbersToSum(primes, 100) != [17, 19, 23, 29]:
        return False
    return True


testFunctions = {"busStops": testbusStops, "addDict": testaddDict, "addDictN": testaddDictN,
                 "searchDicts": testsearchDicts, "searchDicts2": testsearchDicts2, "subsets": testsubsets,
                 "numPaths": testnumPaths, "numbersToSum": testnumbersToSum}
if __name__ == '__main__':
    for testName, testFunc in testFunctions.items():
        print(testName, ': ', testFunc())
        print('---------------------')
