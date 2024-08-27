#lang dssl2

# nat -> nat
# Compute the nth fibonacci value
def fib(n: nat?) -> nat?:
    if not (nat? (n - 2)):
        return n
    else:
        let n1 = fib(n - 1)
        let n2 = fib(n - 2)
        return (n1 + n2)
        
let ans = fib(5)


test 'fib works':
    assert fib(5) == 5
    assert fib(1) == 1
    assert fib(10) == 55