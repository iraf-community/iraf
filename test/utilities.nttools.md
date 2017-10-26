# nttools - NOAO version of TTOOLS package

File: `startable`
```
#c STARno i %5d 
#c X d %6.2f pixels
#c Y d %6.2f pixels
#c MAG d %15.7g magnitude
#c SHARP d %15.7g 
#c ROUND d %15.7g 
#c STARNAME ch*15 %-15s 
#k HISTORY  = 'Created Thu 08:48:13 26-Oct-2017'
1   3.00   4.00              5.              6.              7. HD12345
2  10.00  11.00             12.             13.             14. "HD 122"
3  20.00  21.00             22.             23.             24. ""
```

## keypar - Copy an image or table header keyword to an IRAF parameter.

```
cl> keypar dev$pix airmass
cl> print(keypar.value)
1.08015632629395
```

## keytab - Copy an image or table header keyword to a table element.

## parkey - Put an IRAF parameter into an image or table header keyword.

## partab - Transfer an IRAF parameter to a table element.

## tabkey - Copy a table element to an image or table header keyword.

## tabpar - Transfer a table element to an IRAF parameter.

## gtedit - Graphically edit a table.

## gtpar - Pset to specify graph parameters for 'gtedit' task.

## imtab - Copy an image to a table column.

```
cl> imtab dev$pix pixtab data
cl> head pixtab
#c data i %11d 
#k HISTORY  = 'Column data from dev$pix.imh'
         38
         43
         35
         43
         39
         39
         41
         41
         41
         37
```

## tabim - Copy a table column to an image.

```
cl> tabim pixtab pixtabimg data 2 512
cl> imstat pixtabimg
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
            pixtabimg    262144     108.3     131.3       -1.    19936.
cl> imarith dev$pix - pixtabimg zero
cl> imstat zero
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
                 zero    262144        0.        0.        0.        0.
```

## taextract - Copy an array entry to a column of scalars in another table.

## tainsert - Copy a column of scalars to an array entry in another table.

## tcalc - Perform arithmetic operations on table columns.

## tchcol - Change column name, print format, or units.

## tcheck - Check STSDAS table element values.

## tchsize - Change allocated sizes of various sections of a table.

## tcopy - Copy tables.

Test options: `decimals=7`
```
cl> tcopy startable startable.fits
# startable -> startable.fits
cl> tdump startable.fits
STARno           I             %5d  ""
X                D           %6.2f  pixels
Y                D           %6.2f  pixels
MAG              D          %15.7g  magnitude
SHARP            D          %15.7g  ""
ROUND            D          %15.7g  ""
STARNAME         CH*15       %-15s  ""
XTENSION t 'BINTABLE'  binary table extension
BITPIX   i 8  8-bit bytes
NAXIS    i 2  2-dimensional binary table
NAXIS1   i 59  width of table in bytes
NAXIS2   i 3
PCOUNT   i 0  size of special data area
GCOUNT   i 1  one data group (required keyword)
TFIELDS  i 7
TTYPE1   t 'STARno'  label for field   1
TFORM1   t '1J'  data format of the field: 4-byte INTEGER
TTYPE2   t 'X'  label for field   2
TFORM2   t '1D'  data format of the field: 8-byte DOUBLE
TUNIT2   t 'pixels'  physical unit of field
TTYPE3   t 'Y'  label for field   3
TFORM3   t '1D'  data format of the field: 8-byte DOUBLE
TUNIT3   t 'pixels'  physical unit of field
TTYPE4   t 'MAG'  label for field   4
TFORM4   t '1D'  data format of the field: 8-byte DOUBLE
TUNIT4   t 'magnitude'  physical unit of field
TTYPE5   t 'SHARP'  label for field   5
TFORM5   t '1D'  data format of the field: 8-byte DOUBLE
TTYPE6   t 'ROUND'  label for field   6
TFORM6   t '1D'  data format of the field: 8-byte DOUBLE
TTYPE7   t 'STARNAME'  label for field   7
TFORM7   t '15A'  data format of the field: ASCII Character
EXTNAME  t 'startable'  name of this binary table extension
TDISP1   t 'I5'  display format
TNULL1   i -2147483647  undefined value for column
TDISP2   t 'F6.2'  display format
TDISP3   t 'F6.2'  display format
TDISP4   t 'G15.7'  display format
TDISP5   t 'G15.7'  display format
TDISP6   t 'G15.7'  display format
TDISP7   t 'A15'  display format
HISTORY  t   Created Thu 08:48:13 26-Oct-2017
  1    3.     4.    5.     6.     7. HD12345
  2   10.     11.  12.    13.    14. "HD 122"
  3   20.     21.  22.    23.    24. ""
```

Test options: `decimals=7`
```
cl> tcopy startable.fits startable2
# startable.fits -> startable2
cl> tdump startable2
STARno           I             %5d  ""
X                D           %6.2f  pixels
Y                D           %6.2f  pixels
MAG              D          %15.7g  magnitude
SHARP            D          %15.7g  ""
ROUND            D          %15.7g  ""
STARNAME         CH*15       %-15s  ""
EXTNAME  t 'startable'  name of this binary table extension
HISTORY  t   Created Thu 08:48:13 26-Oct-2017
  1   3.   4.   5.    6.    7. HD12345
  2  10.  11.  12.   13.   14. "HD 122"
  3  20.  21.  22.   23.   24. ""
```


## tcreate - Create a STSDAS table from an ASCII descriptor table.

`columns.cd` may contain just the following:

File: `columns.cd`
```
STARno I  i5
X       r      "F6.2"  pixels
Y       R    F6.2     "pixels"
MAG R   ""   magnitude
                SHARP     R
                                ROUND           r
STARNAME   ch*15
```

Note the free format of, and embedded tabs in, the column definitions
file itself.  The format for display of MAG is not specified, but the
unit is given as magnitude, so adjacent quotes are used to mark the
position where the display format is expected.

The file `data.dat` may contain (if `nskip=3`, `nlines=2`):

File: `data.dat`
```
this is a header
      header2
       header3
 1      3.0     4.0     
           5.0  6.0     7.0 HD12345
   2 10.0 11.0 12.0 13.0
14.0 "HD 122"
3 20.0    21.0        22.0         23.0     24.0  ""
dummy line
```

Note the tabbed and free format of the data file and the specification
of the character strings.  If the character data contain embedded
blanks then the whole string should be quoted, otherwise this is not
necessary.  The final entry is the null character string.

```
cl> tcreate outfile columns.cd data.dat nskip=3
out of synch or extra data in line 9
cl> match "#k HISTORY" outfile stop=yes
#c STARno i %5d 
#c X d %6.2f pixels
#c Y d %6.2f pixels
#c MAG d %15.7g magnitude
#c SHARP d %15.7g 
#c ROUND d %15.7g 
#c STARNAME ch*15 %-15s 
    1   3.00   4.00              5.              6.              7. HD12345
    2  10.00  11.00             12.             13.             14. "HD 122"
    3  20.00  21.00             22.             23.             24. ""
```

## tdelete - Delete tables.

## tdiffer - Form a table which is the difference of two tables.

## tdump - Dump the contents of a table to an ASCII file.

Test options: `decimals=7`
```
cl> tdump startable
STARno           I             %5d  ""
X                D           %6.2f  pixels
Y                D           %6.2f  pixels
MAG              D          %15.7g  magnitude
SHARP            D          %15.7g  ""
ROUND            D          %15.7g  ""
STARNAME         CH*15       %-15s  ""
HISTORY  t Created Thu 08:48:13 26-Oct-2017
  1    3.     4.   5.     6.   7. HD12345
  2   10.    11.  12.    13.  14. "HD 122"
  3   20.    21.  22.    23.  24. ""
```

## tedit - Edit a table.

## texpand - Expand tables according to a set of rules.

## thedit - Edit or print table header keywords.

```
cl> thedit startable i_ncols .
startable,i_ncols = 7
cl> thedit startable.fits history .
startable.fits,HISTORY = "Created Thu 08:48:13 26-Oct-2017"
```

## thistogram - Make a histogram of a column in a table.

## thselect - Select tables satisfying an expression; print keywords.

## tinfo - Display table size information.

```
cl> tinfo startable
# startable
   3 rows written to table
   7 columns defined
   1 header parameters written to table
   5 records allocated for header parameters
  11 space allocated for column descriptors
table type:  text  explicit column definitions
cl> tinfo startable.fits
# startable.fits[1]
   3 rows written to table
   7 columns defined
  35 header parameters written to table
  35 records allocated for header parameters
   7 space allocated for column descriptors
table type:  fits
```

## tintegrate - Numerically integrate one column with respect to another.

## tjoin - Perform a relational join of two tables.

## tlcol - List column information for a table.

## tlinear - Use linear regression to fit one or two table columns.

## tmatch - Find closest match between rows in two tables

## tmerge - Either merge or append tables.

## tprint - Print tables--both headers and data.

## tproduct - Form the Cartesian product of two tables.

## tproject - Create new table from selected columns in a table.

## tquery - Create a new table from selected rows and columns in a table.

## tread - Browse through a table.

## trebin - Resample a table to uniform spacing.

This task resamples tables. The grid on which to interpolate an input
table may be specified either by a table giving explicit values or by
start, end, and step values for uniform spacing. The column names in
the output table will be the same as in the input table.

This is out input file. The numbers are just randomly chosen.

File: `rebin.tbl`
```
#c lambda d %6.2f nm
#c Y d %6.3f pixels
453.02   5.873
464.60  17.939
603.04  39.843
625.08  68.326
647.27  44.617
723.45  68.226
730.31  36.557
764.82  42.797
784.33   2.650
862.67  38.502
```

There are four different functions for interpolation: nearest, linear,
poly3, spline.

```
cl> trebin rebin.tbl rebnr.tbl lambda 460. 860. 50. function=nearest
rebin.tbl --> rebnr.tbl
cl> type rebnr.tbl
#c lambda d %6.2f nm
#c Y d %6.3f pixels
460.00 17.939
510.00 17.939
560.00 39.843
610.00 39.843
660.00 44.617
710.00 68.226
760.00 42.797
810.00  2.650
860.00 38.502
```

```
cl> trebin rebin.tbl reblin.tbl lambda 460. 860. 50. function=linear
rebin.tbl --> reblin.tbl
cl> type reblin.tbl
#c lambda d %6.2f nm
#c Y d %6.3f pixels
460.00 13.146
510.00 25.122
560.00 33.033
610.00 48.838
660.00 48.562
710.00 130.32
760.00 52.715
810.00 14.398
860.00 37.280
```

```
cl> trebin rebin.tbl rebspl.tbl lambda 460. 860. 50. function=spline
rebin.tbl --> rebspl.tbl
cl> type rebspl.tbl
#c lambda d %6.2f nm
#c Y d %6.3f pixels
460.00 13.514
510.00 24.524
560.00  8.351
610.00 52.234
660.00 49.286
710.00 106.62
760.00 41.834
810.00 -26.03
860.00 33.545
```

```
cl> trebin rebin.tbl rebpol.tbl lambda 460. 860. 50. function=poly3
rebin.tbl --> rebpol.tbl
cl> type rebpol.tbl
#c lambda d %6.2f nm
#c Y d %6.3f pixels
460.00 13.681
510.00 31.879
560.00 24.140
610.00 53.145
660.00 56.902
710.00 102.17
760.00 38.854
810.00 -51.35
860.00 22.331
```

## tselect - Create a new table from selected rows of a table.

## tsort - Sort a table.

## tstat - Get mean, standard deviation, min, and max for a column.

## ttranspose - Transpose or flip a table.

File: `ttranspose.in`
```
one     two     three
four    five    six
seven   eight   nine
ten     eleven  twelve
```

The input is the text file "in", and the output is to be displayed on
the screen.  Each of the three operations ("t", "h", "v") and some
combinations are illustrated.

```
cl> ttranspose ttranspose.in STDOUT t
ttranspose.in --> STDOUT
one    four   seven  ten
two    five   eight  eleven
three  six    nine   twelve
cl> ttranspose ttranspose.in STDOUT h
ttranspose.in --> STDOUT
three  two    one
six    five   four
nine   eight  seven
twelve eleven ten
cl> ttranspose ttranspose.in STDOUT v
ttranspose.in --> STDOUT
ten   eleven twelve
seven eight  nine
four  five   six
one   two    three
cl> ttranspose ttranspose.in STDOUT hv
ttranspose.in --> STDOUT
twelve eleven ten
nine   eight  seven
six    five   four
three  two    one
cl> ttranspose ttranspose.in STDOUT th
ttranspose.in --> STDOUT
ten    seven  four   one
eleven eight  five   two
twelve nine   six    three
```

## tunits - Convert table column from one set of units to another

## tupar - Edit table header keywords.

## tiimage - Insert images into rows of a 3-D table.

## titable - Insert 2-D tables into rows of a 3-D table.

## tximage - Extract images from rows of 3-D tables.

## txtable - Extract 2-D tables from rows of 3-D tables.

## tscopy - Copy row/column subsets of tables using selectors.

