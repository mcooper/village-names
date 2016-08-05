# -*- coding: utf-8 -*-
"""
Created on Sat Apr 11 16:40:16 2015

@author: matthewcooper
"""

with open('/Users/matthewcooper/R workspace/completevills.txt') as f:
    vills = f.read().splitlines()
    
with open('/Users/matthewcooper/R workspace/threegramsforbinmat.txt') as f:
    gram3 = f.read().splitlines()

with open('/Users/matthewcooper/R workspace/binmattest2.txt', 'w') as f:
    f.write('vills,')
    for g in gram3:
        f.write('%s,' % g)
    f.write('\n')
    for v in vills:
        f.write('%s,' % v)
        for g in gram3:
            if g in v:
                f.write('1,')
            else:
                f.write('0,')
        f.write('\n')
            