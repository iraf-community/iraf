# CL scripts

This is based on [An Introductory User’s Guide to IRAF
Scripts](https://iraf-community.github.io/doc/script.pdf) by Ed Anderson.

You will recall that several tasks may be called in sequence on a
single command line, using a semicolon `;` to separate each command.
For example:

```
cl> mkdir database; dir; cd database; dir
database
no files found
```

If the command sequence is too long to fit on a single line, one could
construct a compound statement by using the curly braces, `{}`:

```
cl> {
>>> dir
database
>>> cd database
>>> dir
no files found
>>> }
```

The `>>>` prompt indicates that the CL requires more input (in this
case, the CL is waiting for a `}`) before executing the task or
sequence of tasks.  A Terminal Script is essentially a compound
statement, but uses some of the simple programming tools provided by
the CL.

Now we check some control structures. First create a list file with a
`for` loop

File: `loop.cl`
```
for (i = 1; i < 5; i+=1) {
    j = i*i
    print (i, j, >> "sqr.lis")
}
```

Execute this script and check the content of the list:


```
cl> cl < loop.cl
cl> type "sqr.lis"
1 1 
2 4 
3 9 
4 16 
```

Read the file just created with a `while` loop with this script:

File: `while.cl`
```
list = "sqr.lis"
while (fscan(list, i, j) != EOF) print(i, j)
```



```
cl> cl < while.cl
1 1 
2 4 
3 9 
4 16 
```

Test intrinsic mathematical CL functions

Test options: `decimals=7`
```
cl> =sin(1.2)
0.9320390859672263
cl> =cos(1.2)
0.3623577544766736
cl> =tan(1.2)
2.5721516221263188
cl> =atan2(2.,3.)
0.5880026035475675
cl> =exp(1.2)
3.3201169227365472
cl> =frac(1.2)
0.2
cl> =int(-1.2)
-1
cl> =log(1.2)
0.1823215567939546
cl> =log10(1.2)
0.07918124604762482
cl> =max(2,3,4,2)
4
cl> =min(2,3,4,2)
2
cl> =mod(123, 7)
4
cl> =nint(1.5)
2
cl> =nint(2.5)
3
cl> =sqrt(1.2)
1.0954451150103321
```

Intrinsic logic functions

```
cl> =real("1.23")
1.23
cl> =isindef(0)
no
cl> =isindef(INDEF)
yes
cl> =radix(123, 10)
123
cl> =radix(123, 16)
7B
cl> =radix(-123, 10)
4294967173
cl> =radix(-123, 16)
FFFFFF85
```

Intrinsic string functions

```
cl> =stridx("fd", "abcdefg")
4
cl> =strldx("fd", "abcdefg")
6
cl> =strlen("abcdefg")
7
cl> =strlstr("def", "abcdefg")
4
cl> =strlwr("aBcDeF")
abcdef
cl> =strstr("def", "abcdefg")
4
cl> =strupr("aBcDeF")
ABCDEF
cl> =substr("abcdefg", 2, 5)
bcde
```

