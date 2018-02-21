# Unit tests for IRAF

This subdirectory contains a simple test script, `run_tests`, and a
collection of non-interactive tests for various aspectes of IRAF. 

## Introduction

All tests are given in
[Markdown](https://guides.github.com/features/mastering-markdown/)
format, where the code blocks are usually executed. The code block
contains CL commands as well as the expected output. In principle, one
can just cut and paste a sample session into the code blocks.

With this example code block, one IRAF command is executed and the
result is compared with the given output:

```
cl> imstat dev$pix
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
              dev$pix    262144     108.3     131.3       -1.    19936.
```

Each Markdown file starts the execution within an empty directory that
is temporaily created, and destroyed after the test. If test data is
needed, they are usually taken from the IRAF source code.

### Additional text files

Sometimes, additional (text) files are required to run the test. These
may be given as a special code block that is preceded with `File:` and
the file name in backticks:

File: `sample.txt`
```
 7.311
 6.123
24.829
16.696
29.923
```

This file is then created within the working directory and can be used for
tests:

```
cl> dir
sample.txt
cl> type sample.txt
 7.311
 6.123
24.829
16.696
29.923
```

### Test options

Options for individual tests can be given by preceding the code block
with a line

Test options: `opt1 opt2=arg`

The individual options are separated by space. Options may have
additional parameters, as `opt2` in the line above.


#### Handling floating point output

Due to limited processing accuracy, the result of IRAF tasks may
differ slightly between different platforms, which would break a
direct string comparison of expected and actual output. To deal with
this, the required accuracy can be set by preceding the code block
with a line containing the required accuracy. For example:

Test options: `decimals=7`
```
cl> =1.0/3.0
0.33333331
```

When this option is used, the expected and the actual output are
parsed for floating point numbers, which are reformatted for the
specified accuracy.

#### Expected failures

Sometimes, it is expected for an individual test to fail. This can be
switched on with the `xfail` option:

Test options: `xfail`
```
cl> =2*2
6
```

#### Skipping tests

One can also skip individual code blocks, f.e. if they don't contain a
test, of if the test is known to never return.

Test options: `skip`
```
cl> while (1==1) i=1
```

#### Testing only specified archs

To run a test only on a subset of architectures, the `arch` option may
be used:

Test options: `arch=linux64`
```
cl> !uname -m
x86_64
```

More than one architecture can be specified by separating them with
comma.

### Running tests

To run, just the script `test/run_test` needs to be executed. It has the
following options:

 * `-h` this help
 * `-v` verbose output
 * `-c CL.e` use `CL.e` as IRAF shell (may be given several times)
 * `TEST.md` run test script `TEST.md` (may be given several times)

If no further options are given, all markdown files in `test` are executed
with the `ecl.e` IRAF shell. For each Markdown file, it will print the file
name, and the result of each test as one character per executed code block:

  * `.` test passed,
  * `F` test failed,
  * `x` test failed and was expected to fail
  * `s` test skipped,
  * `a` architecture specific test skipped.

As an example, the output for this file is

File: `printed on stdout`
```
$ ./test/run_tests test/README.md
ecl.e: README.md ...xs.
```

Without errors, the return status is 0. If there is an error, then the
expected, the actual output and their difference are printed:

File: `printed on stdout`
```
$ ./test/run_tests test/README.md
ecl.e: README.md ..Fxs.

=================== Failure in README.md:66 with ecl.e ===================

Expected
========
cl> =1.0/3.0
0.33333331

Output
======
cl> =1.0/3.0
0.33333333333333

Diff
====
@@ -1,2 +1,2 @@
 cl> =1.0/3.0
-0.33333331
+0.33333333333333
```

In this case, the exit status of the test script is 1.

## Available tests

The following tests are available so far:

 * This file, containing the self-test for the basic unit test functionality,
 * [Check the IRAF tree for generated files](files.md)
 * [Preliminary Test Procedure for IRAF](testproc.md): basic functionality,
   mainly from the test procedure document written by Jeanette Barnes and 
   her "Beginner's Guide to Using IRAF",
 * [Programming interface](programming.md) for basic CL and SPP functionality,
 * Test from the examples in several packages:
    - [`lists`](lists.md),
    - [`images.imcoords`](images.imcoords.md)
    - [`images.imfilter`](images.imfilter.md)
    - [`images.imfit`](images.imfit.md)
    - [`images.imgeom`](images.imgeom.md)
    - [`images.immatch`](images.immatch.md)
    - [`utilities.nttools`](utilities.nttools.md)
    - [`noao.digiphot.photcal`](noao.digiphot.photcal.md)
    - [`noao.astutil`](noao.astutil.md)
 * [Regression test](numerical-recipes.md) for the replacement of Numerical
   Recipes code
