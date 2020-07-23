#!/bin/python

def pygrep(filename, expr):
    inputfile_fid = checkVars(filename, expr)
    inputtext = inputfile_fid.read()
    for line in inputtext:
        print(line)
    #print("You Chose"+str(filename))

def checkVars(filename, expr):
    if (type(filename)!=str or type(expr)!=str ):
        print("All variables should be strings")
        return
    else:
        try:
            return open(filename)
        except:
            print("Could not open File")

print(33)


pygrep("./scemunits.txt", 'blah')
pygrep("./scemunlkjits.txt", 'blah')
