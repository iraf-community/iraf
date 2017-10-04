# Lists

These tests are taken from the examples

## average - Compute the mean and standard deviation of a list

Task average computes the average and standard deviation of a list of
numbers.

File: `numbers`
```
0.7087 
 0.425 
0.9906 
0.5149 
0.4219 
0.6486 
0.7075 
0.8334 
```

```
cl> type numbers | average
0.656325 0.19892008266064 8
cl> print 1 | average add
0.69451111111111 0.21851009384262 9
```

## columns - Convert multicolumn file to separate files

Task columns is used to reformat a multicolumn list file into separate
files, such that one output file is created for each column in the
input file.

File: `dtable`
```
0.7087  0.425 0.9906 
0.5149 0.4219 0.6486 
0.7075 0.8334 0.8039 
 0.571 0.6779 0.3024 
0.6091 0.4976 0.5332 
0.3596 0.1426 0.3051 
0.4146 0.5085 0.0547 
0.4568 0.1392 0.3042 
0.1508 0.4498 0.7418 
```

```
cl> columns dtable 3 outroot=datacol.
cl> type datacol.1
0.7087
0.5149
0.7075
0.571
0.6091
0.3596
0.4146
0.4568
0.1508
cl> type datacol.2
0.425
0.4219
0.8334
0.6779
0.4976
0.1426
0.5085
0.1392
0.4498
cl> type datacol.3
0.9906
0.6486
0.8039
0.3024
0.5332
0.3051
0.0547
0.3042
0.7418
```

## lintran - Perform linear transformation of a list

Specified fields from the input list can be scaled, rotated and
shifted.

File: `frame1`
```
0.7087  0.424 
0.9906 0.5149 
0.4219 0.6486 
0.7075 0.8334 
0.8039  0.571 
0.6779 0.3024 
0.6091 0.4976 
0.5332 0.3596 
```

```
cl> lintran frame1 x1=35.7 y1=389.2 x2=36.9 y2=400.0
1.9087  11.22 
2.1906 11.315 
1.6219 11.449 
1.9075 11.633 
2.0039  11.37 
1.8779 11.102 
1.8091 11.298 
1.7332  11.16 
cl> lintran frame1 angle=90
-0.424 0.7087 
-0.515 0.9906 
-0.649 0.4219 
-0.833 0.7075 
-0.571 0.8039 
-0.302 0.6779 
-0.498 0.6091 
 -0.36 0.5332
```

## table - Format a list of words into a table

Task table reads a list of strings from the standard input or a list
of files and assembles a nicely formatted table.

```
cl> sort numbers numeric_sort=yes | table ncol=2 first_col=1 last_col=20
0.4219    0.7075 
 0.425    0.7087 
0.5149    0.8334 
0.6486    0.9906 
```

## tokens - Break a file up into a stream of tokens

Task tokens breaks the input up into a series of tokens.

File: `hello.x`
```
# HELLO -- Sample program introducing SPP.
procedure t_hello_world ()
begin
    call printf ("Hello,world!")
end
```

```
cl> tokens hello.x newline=no
procedure
t_hello_world
(
)
begin
call
printf
(
"Hello,world!"
)
end
```

## unique - Delete redundant elements from a list

Task unique reads the input comparing adjacent lines. The second and
successive copies of a line are removed; the remainder is passed on to
the standard output.

File: `names`
```
what
is
important
about
object
is
something
```

```
cl> sort names | unique
about
important
is
object
something
what
```

## words - Break a file up into a stream of words

Task words is used to break the input up into a series of words or
strings.

File: `text`
```
Task words is used to break the input up into a series of  words  or
strings.
```

```
cl> words text
Task
words
is
used
to
break
the
input
up
into
a
series
of
words
or
strings.
```

