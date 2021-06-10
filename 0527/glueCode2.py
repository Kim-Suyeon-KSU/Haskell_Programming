# -*- coding: utf-8 -*-
"""
Created on Thu Jun 10 13:48:14 2021

@author: suyeon
"""

def f1(x): return x + 1
def f2(x): return x + 2
def f3(x): return x + 3
x = int(input("정수 입력 : "))
lst = [x]
res = f1(x)
lst.append(res)
res = f2(res)
lst.append(res)
res = f3(res)
lst.append(res)
print(res, lst)