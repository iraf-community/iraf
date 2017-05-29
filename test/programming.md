# Writing own procedures

## CL scripts

This is based on [An Introductory User’s Guide to IRAF
Scripts](http://iraf.noao.edu/iraf/ftp/iraf/docs/script.pdf) by Ed Anderson.

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

# SPP tasks

This example is taken from a the [slides of a talk by Rob
Seaman](http://iraf.noao.edu/ftp/docs/spp_intro.pdf).

Take a simple program from the test directory:

File: `hello.x`
```
# HELLO -- Sample program introducing SPP.
task hello = t_hello_world
procedure t_hello_world ()
begin
    call printf ("Hello,world!\n")
end
```

Compile it:

```
cl> softools
cl> xc hello.x
hello.x:
   sys_runtask:
   t_hello_world:
hello.f:
   sysruk:
   thelld:
link:
```

Declare and run an IRAF task

```
cl> task $hello = hello.e
cl> hello
Hello, world!
```
