# -*- coding: utf-8 -*-
"""
Created on Thu Jan 15 17:03:47 2015

@author: matthewcooper
"""
villfile = '/Users/matthewcooper/R workspace/villages.txt'
vills = []

with open(villfile) as f:
    vills = [line.rstrip('\n').strip('"') for line in open(villfile)]

vills.remove('x')

vowels = ['A','E','I','O','U']
consonants = ['B','C','D','F','G','H','J','K','L','M','N','P','Q','R','S','T','V','W','X','Y','Z']    

newvills = []
count2 = 0
for item in vills:
    name = item
    print(name)
    for letter in name:
        if letter in "'6521437()98&`0":
            name = name.replace(letter,'')
        if letter in "-/_&":
            name = name.replace(letter,' ')
        if letter==".":
            name = ''
        if letter==",":
            name = name[:name.index(',')]
            
    #Change a C to K or S
    for i in range(0,len(name)-1):
        if name[i]=='C' and name[i+1] in 'AOU':
            if i==0:
                name = 'K'+name[1:]
            if i>0:
                name = name[:i]+'K'+name[i+1:]
        if name[i]=='C' and name[i+1] in 'EI':
            if i==0:
                name = 'S'+name[1:]
            if i>0:
                name = name[:i]+'S'+name[i+1:]
    
    #Change all OU at beginnging followed by consonant to U, vowel to W
    if name[:2]=='OU' and name[2] in vowels:
        name = 'W'+name[2:]
    if name[:2]=='OU' and name[2] in consonants:
        name = 'W'+name[2:]
    
    #Change all OU at end of word to U
    if name[-2:]=='OU' and name[-3] in consonants:
        name = name[:-2]+'U'
    
    #Change all OU between two consonants to U
    #First count all OUs
    count = 0
    for i in range(0,len(name)-1):
        if name[i]=='O' and name[i+1]=='U':
            count +=1
    index = []
    accumulator = 0
    lookup = 0
    while accumulator < count:
        index.append(name.index('OU',lookup,len(name)))     
        lookup = index[accumulator]+1
        accumulator += 1
    for ind in index:
        if name[ind-1] in consonants and name[ind+2] in consonants:
            name = name[:ind]+'U$'+name[ind+2:]
    name = name.replace('$','')
    
    name = name.replace('SS','S')
    
    #C-'OU'-V to C-'W'-V
    for i in range(1,len(name)-2):
        if name[i-1] in consonants and name[i:i+2]=='OU' and name[i+2] in vowels:
            name = name[:i]+'W$'+name[i+2:]
    name = name.replace('$','')
    
    #Fix Ni Ny GN
    for i in range(0,len(name)-2):   
        if name[i:i+2]=='NI' and name[i+2] in vowels:
            name = name[:i+1]+'Y'+name[i+2:]
            
    for i in range(0,len(name)-2):   
        if name[i:i+2]=='GN' and name[i+2] in vowels:
            name = name[:i]+'NY'+name[i+2:]
    
    if len(name)>2:
        if name[-1]=='Y' and name[-2] in consonants:
            name = name[:-1]+'I'
    
    #Fix Dia Ja
    name = name.replace('DY','DI')
    for i in range(0,len(name)-2):
        if name[i:i+2]=='DI' and name[i+2] in vowels:
            name = name[:i]+'J$'+name[i+2:]
    name = name.replace('$','')
    name = name.replace('DJI','JI')

    #Fix Tia cha
    #Change first and last of word to upper

    name = name.lower()

    for i in range(1,len(name)-2):
        if name[i] == ' ':
            name = name[:i-1]+name[i-1:i+2].upper()+name[i+2:]
    if len(name)>2:
        name = name[0].upper()+name[1:-1]+name[-1].upper()
    
    if count2%1000==0:
        print("We're "+str((count2/153454)*100)+" percent there!")
    count2+=1

    print(name)

    newvills.append(name)
    
lowest = 5
for vill in vills:     
    if len(vill)==2:
        print(vill)

for vill in vills:     
    for i in range(1,len(vill)-2):
           if vill[i:i+2]=='TY' and (vill[i+2] in consonants):
               print(vill)

write = open('/Users/matthewcooper/R workspace/fixvills3.txt', 'w')
for item in newvills:
    write.write(item+'$\n')
write.close()


#read in clean villages and generate ngrams
cleanvills = []
with open('/Users/matthewcooper/R workspace/fixvills.txt') as f:
    cleanvills = [line.rstrip('\n').strip('"').strip('$') for line in open('/Users/matthewcooper/R workspace/fixvills.txt')]
    

#create string of all villages
villstr2 = ''
for item in cleanvills:
    villstr2 = villstr2+'.'+item

letters = consonants

for item in vowels:
    letters.append(item)
    
letterslow = []
for item in letters:
    letterslow.append(item.lower())

lettersall = letterslow
for item in letters:
    lettersall.append(item)

##generate all 
letters2 = letterslow
letters3 = lettersall

threegrams = []
for item in letters2:
    for item2 in letters:
        for item3 in letters3:
            threegrams.append(item+item2.lower()+item3)

goodstrs = []
for item in threegrams:
    if item in villstr2:
        goodstrs.append(item)
    print(item)

write = open('/Users/matthewcooper/R workspace/threegrams.txt', 'w')
for item in goodstrs:
    write.write(item+'$\n')
write.close()

#Find all punctuation etc
collect = ''
for word in vills:
    for letter in word:
        if letter not in consonants and letter not in vowels and letter not in collect:
            collect = collect + letter
            
            
### Generate n-grams
## This way doesnt work
ngrams = []
count2 = 0
for vill in newvills:
    count = 0
    while count < len(vills)-3:
        if vill[count:count+3] not in ngrams and ' ' not in vill[count:count+3] and len(vill[count:count+3])==3:
            ngrams.append(vill[count:count+3])
        count += 1
    if count2%1000==0:
        print("We're "+str((count2/153454)*100)+" percent there!")
    count2+=1
        