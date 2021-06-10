# -*- coding: utf-8 -*-
"""
Created on Thu Jun 10 13:47:03 2021

@author: suyeon
"""

def f1(x):
    return (x + 1, str(x) + "+1")
def f2(x):
    return (x + 2, str(x) + "+2")
def f3(x):
    return (x + 3, str(x) + "+3")
x = int(input("정수 입력 : "))
log = "Ops:"
res, log1 = f1(x)
log += log1 + ";"
res, log2 = f2(res)
log += log2 + ";"
res, log3 = f3(res)
log += log3 + ";"
print(res, log)