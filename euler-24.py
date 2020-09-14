# for first approach i check solution with this program that i download from:
# http://shriphani.com/blog/2008/08/13/factoradic/
# Then i write the lisp version.

def genFact(decimal_num, position):
    for i in xrange(position, -1, -1):
        fact = factorial(position) * i
        if fact < decimal_num:
            return i, decimal_num - fact
        else:
            continue
        
def factorial(n):
    if n==0 or n==1:
        return 1
    else:
        return n * factorial(n-1)

def getFactoradic(decimal_num, length):
    factoradic = []
    num = decimal_num
    for i in xrange(length-1, -1, -1):
        bag = genFact(num, i)
        factoradic.append(bag[0])
        num = bag[1]
    return factoradic


getFactoradic(1000000,10)
#then check wikipedia http://en.wikipedia.org/wiki/Factoradic#Examples
