def fibonacci():
    a,b = 0,1
    while True:
        yield a
        a,b = b, a + b

def digits(x):
    return len(str(x))


def euler25():
    count = 0
    for i in fibonacci():
        count += 1
        if digits(i) >= 1000:
            return count

print (euler25())
