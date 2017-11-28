This is compiled from

 * [Preliminary Test Procedure for IRAF](http://iraf.noao.edu/iraf/ftp/iraf/docs/testproc.ps.Z),
   IRAF Version V2.11, Jeannette Barnes, Central Computer Services,
   National Optical Astronomy Observatories, P.O. Box 26732, Tucson, AZ
   85726, Revised September 23, 1997,

 * [A Beginner's Guide to Using IRAF](http://iraf.noao.edu/iraf/ftp/pub/beguide.ps.Z),
   IRAF Version 2.10, Jeannette Barnes, Central Computer Services, August 1993.

***

The following pages describe a short test procedure that sites can
execute to test some basic image functions within IRAF for a new
installation. This process will help verify that everything is working
correctly and also help the rst time user gain familiarity with the
system. The commands you need to type and the expected terminal output
are given below.

# The basics

We will assume that you have started IRAF and are residing in an empty
directory from which you wish to work.

## Packages and tasks

All of the IRAF core packages are loaded when you log into IRAF. You
can list what packages are currently loaded by typing the word
`package`. The following should be displayed, but the packages may not
be listed in the same order:

```
cl> package
    clpackage
    language
    system
    lists
    noao
    nttools
    utilities
    proto
    imutil
    immatch
    imgeom
    imfit
    imfilter
    imcoords
    images
```

New packages can be loaded by simply typing the packages name. Not the
change of the prompt. The last package loaded can be unloaded by
typing `bye`. Try the following. Note that in our example the top
level packages listed may be different than yours.

```
cl> digiphot
di> ?
      apphot   daophot  photcal  ptools
di> bye
cl> ?
      dataio      images      lists       obsolete    proto       system
      dbms        language    noao        plot        softools    utilities
```

## Image files

An IRAF image exists in the `dev$` directory. Lets make a copy of this
image into the current working directory.

```
cl> imcopy dev$pix image.short
dev$pix -> image.short    
```

Let's look at the header information for this image with `imhead`.

```
cl> imhead dev$pix long+
dev$pix[512,512][short]: m51  B  600s
No bad pixels, min=-1., max=19936.
Line storage mode, physdim [512,512], length of user area 1621 s.u.
Created Mon 23:54:13 31-Mar-1997, Last modified Sun 16:37:53 12-Mar-2006
Pixel file "HDR$pix.pix" [ok]
'KPNO-IRAF'           /
'31-03-97'            /
IRAF-MAX=           1.993600E4  /  DATA MAX
IRAF-MIN=          -1.000000E0  /  DATA MIN
IRAF-BPX=                   16  /  DATA BITS/PIXEL
IRAFTYPE= 'SHORT   '            /  PIXEL TYPE
CCDPICNO=                   53  /  ORIGINAL CCD PICTURE NUMBER
ITIME   =                  600  /  REQUESTED INTEGRATION TIME (SECS)
TTIME   =                  600  /  TOTAL ELAPSED TIME (SECS)
OTIME   =                  600  /  ACTUAL INTEGRATION TIME (SECS)
DATA-TYP= 'OBJECT (0)'          /  OBJECT,DARK,BIAS,ETC.
DATE-OBS= '05/04/87'            /  DATE DD/MM/YY
RA      = '13:29:24.00'         /  RIGHT ASCENSION
DEC     = '47:15:34.00'         /  DECLINATION
EPOCH   =                 0.00  /  EPOCH OF RA AND DEC
ZD      = '22:14:00.00'         /  ZENITH DISTANCE
UT      = ' 9:27:27.00'         /  UNIVERSAL TIME
ST      = '14:53:42.00'         /  SIDEREAL TIME
CAM-ID  =                    1  /  CAMERA HEAD ID
CAM-TEMP=              -106.22  /  CAMERA TEMPERATURE, DEG C
DEW-TEMP=              -180.95  /  DEWAR TEMPRATURE, DEG C
F1POS   =                    2  /  FILTER BOLT I POSITION
F2POS   =                    0  /  FILTER BOLT II POSITION
TVFILT  =                    0  /  TV FILTER
CMP-LAMP=                    0  /  COMPARISON LAMP
TILT-POS=                    0  /  TILT POSITION
BIAS-PIX=                    0  /
BI-FLAG =                    0  /  BIAS SUBTRACT FLAG
BP-FLAG =                    0  /  BAD PIXEL FLAG
CR-FLAG =                    0  /  BAD PIXEL FLAG
DK-FLAG =                    0  /  DARK SUBTRACT FLAG
FR-FLAG =                    0  /  FRINGE FLAG
FR-SCALE=                 0.00  /  FRINGE SCALING PARAMETER
TRIM    = 'Apr 22 14:11 Trim image section is [3:510,3:510]'
BT-FLAG = 'Apr 22 14:11 Overscan correction strip is [515:544,3:510]'
FF-FLAG = 'Apr 22 14:11 Flat field image is Flat1.imh with scale=183.9447'
CCDPROC = 'Apr 22 14:11 CCD processing done'
AIRMASS =    1.08015632629395   / AIRMASS
HISTORY 'KPNO-IRAF'
HISTORY '24-04-87'
HISTORY 'KPNO-IRAF'           /
HISTORY '08-04-92'            /
```

Note that the pixels are short integers (=16 bits).

Print the same, but without the user fields:

```
cl> imhead dev$pix l+ u-
dev$pix[512,512][short]: m51  B  600s
No bad pixels, min=-1., max=19936.
Line storage mode, physdim [512,512], length of user area 1621 s.u.
Created Mon 23:54:13 31-Mar-1997, Last modified Sun 16:37:53 12-Mar-2006
Pixel file "HDR$pix.pix" [ok]
'KPNO-IRAF'           /
'31-03-97'            /
```

Check the parameter settings for `imhead`:

```
cl> lpar imhead
       images =                 image names
      (imlist = "*.imh,*.fits,*.pl,*.qp,*.hhh") default image names
  (longheader = no)             print header in multi-line format
  (userfields = yes)            print the user fields (instrument parameters)
        (mode = "ql")           
```

It would be useful to generate two more copies of this image but with
different pixel types - one with 32-bit floating point pixels (called
`real`s) and one with 64-bit double precision floating point pixels
(called `double`). Note that IRAF also supports other pixel data types -
32-bit integers called `long`, 16-bit unsigned integers called
`ushort`, and complex numbers. Execute the following:

```
cl> imarith image.short / 1 image.real pixtype=r
cl> imarith image.short / 1 image.dbl pixtype=d
cl> imhead image.*
image.dbl.fits[512,512][double]: m51  B  600s
image.real.fits[512,512][real]: m51  B  600s
image.short.fits[512,512][short]: m51  B  600s
```

Let's execute a couple of more tasks that will exercise some image
operators. Typing

```
cl> minmax image.dbl,image.real,image.short
image.dbl [77,4] -1. [348,189] 19936.
image.real [77,4] -1. [348,189] 19936.
image.short [77,4] -1. [348,189] 19936.
```

Now display a table with pixel values.

```
cl> listpix image.short[300:305,200:205] formats="%4s %4s" | table
   1.   1.  145.     4.   2.  141.     1.   4.  149.     4.   5.  144.
   2.   1.  143.     5.   2.  132.     2.   4.  149.     5.   5.  145.
   3.   1.  141.     6.   2.  130.     3.   4.  146.     6.   5.  144.
   4.   1.  142.     1.   3.  162.     4.   4.  143.     1.   6.  138.
   5.   1.  135.     2.   3.  145.     5.   4.  145.     2.   6.  139.
   6.   1.  138.     3.   3.  146.     6.   4.  140.     3.   6.  145.
   1.   2.  147.     4.   3.  144.     1.   5.  144.     4.   6.  141.
   2.   2.  147.     5.   3.  135.     2.   5.  145.     5.   6.  141.
   3.   2.  145.     6.   3.  141.     3.   5.  133.     6.   6.  149.
```

## Image sections

Now let's test the use of image sections. Type and observe the
following terminal interactions:

```
cl> imcopy image.real[200:300,200:300] image.sect
image.real[200:300,200:300] -> image.sect
cl> imhead image.sect
image.sect[101,101][real]: m51  B  600s
```

## Modifying images

At this time, let's modify a couple of image titles.

```
cl> hedit image.real title "m51 real" verify=no
image.real,i_title: "m51  B  600s" -> "m51 real"
image.real updated
cl> hedit image.dbl title "m51 double" verify=no
image.dbl,i_title: "m51  B  600s" -> "m51 double"
image.dbl updated
```

We can verify the new title with the `imheader` task.

```
cl> imhead image*
image.dbl.fits[512,512][double]: m51 double
image.real.fits[512,512][real]: m51 real
image.sect.fits[101,101][real]: m51  B  600s
image.short.fits[512,512][short]: m51  B  600s
```
## Accessing a VOTable

Just checking a simple one:

File: `messier.xml`
```
<?xml version="1.0"?>
<VOTABLE version="1.1">
  <RESOURCE>
    <TABLE>
      <DESCRIPTION>A few Messier objects</DESCRIPTION>
      <PARAM datatype="char" arraysize="*" ucd="meta.bib.author"
	     name="Author" value="Mark Taylor"/>
      <FIELD name="Identifier" datatype="char" arraysize="*"
	     ucd="meta.id;meta.main">
	<DESCRIPTION>Messier Identifier for the object</DESCRIPTION>
      </FIELD>
      <FIELD name="RA" datatype="double" unit="deg" ucd="pos.eq.ra;meta.main"/>
      <FIELD name="Dec" datatype="double" unit="deg" ucd="pos.eq.dec;meta.main"/>
      <DATA>
	<TABLEDATA>
	  <TR><TD>M31</TD><TD> 10.502</TD><TD>41.266</TD></TR>
	  <TR><TD>M57</TD><TD>283.253</TD><TD>33.033</TD></TR>
	  <TR><TD>M82</TD><TD>148.753</TD><TD>69.683</TD></TR>
	</TABLEDATA>
      </DATA>
    </TABLE>
  </RESOURCE>
</VOTABLE>
```

VOTables are internally converted to FITS binary tables. 


Test options: `decimals=7`
```
cl> tinfo messier.xml
# messier.xml
   3 rows written to table
   3 columns defined
  20 header parameters written to table
  35 records allocated for header parameters
   3 space allocated for column descriptors
table type:  fits
cl> tlcol messier.xml
# messier.xml
Identifier       CH*3         %-3s ""
RA               D         %25.16g deg
Dec              D         %25.16g deg
cl> tprint messier.xml showhdr-
    1 M31                    10.502         41.26599999999999
    2 M57                   283.253                    33.033
    3 M82                   148.753         69.68300000000001
```

# Plotting data

## One-dimensional (vector) plotting tasks

Now let's check some plotting options. Type

```
cl> plot
cl> pcol image.short 256 >G image.short.meta
cl> !ls  image.short.meta
image.short.meta
cl> del image.short.meta
```

The redirected output graphics file should have the md5 sum
`0806e05ccea335a1ad4962282905c830` on 64 bit. Unfortunately, this is
machine dependent, so on 32 bit, the md5 sum is
`32e139a609d9d50c8f108d0023820c11`.

## Two-dimensional plotting tasks

Let's make a contour plot of the section image. We do it as IRAF graphics.

```
cl> plot
cl> contour image.sect >G image.sect.meta
Image will be block averaged by 1 in x and 1 in y
cl> !ls  image.sect.meta
image.sect.meta
cl> del image.sect.meta
```

Unfortunately, the file contains local information and therefore the
content can't be strictly compared.


## Tasks that manipulate graphics metacode files

Graphics output can be stored in files. These files are called
"metacode" files. Graphics metacode files can be generated by the task
itself, by the user by redirecting the graphics output to a metacode
file on the command line with the `>G` syntax, or by writing the plot
directly to a metacode file with the `:.write` option in interactive
graphics mode.

A metacode file is distributed with your IRAF system and we will use
it to demonstrate these tasks.

```
cl> plot
cl> gkidir dev$vdm.gki

METAFILE 'dev$vdm.gki':
    [1]  (3517 words)    The SINC Function
    [2]  (2855 words)    The SINC Function
    [3]  (5701 words)    .2
    [4]  (2525 words)    Line 250 of dev$pix[200:300,*]
    [5]  (7637 words)    Log Scaling
    [6]  (97781 words)   NOAO/IRAF V2.3 tody@lyra Fri 23:30:27 08-Aug-86
    [7]  (2501 words)    The Sinc Function
    [8]  (11719 words)   Line 250 of dev$pix[200:300,*]
```

# Using the image display

In the workstation environment the user displays an image into a frame
buffer and then uses the display server, either IMTOOL or SAOimage,
for panning, zooming, blinking, changing the lookup tables (greyscale
and color), and so on. These functions will be done differently
depending on the server you are using.

Before doing anything involving image display the environment variable
stdimage must be set to the correct frame buffer size for the display
servers (as described in the `dev$graphcap` file under the section
"STDIMAGE devices") or to the correct image display device. The task
`GDEVICES` is helpful for determining this information for the display
servers.

```
cl> show stdimage
imt512
cl> set stdimage=imt800
cl> set stdimage=iism70v
```

## Displaying IRAF images

The `DISPLAY` task is the main task used for displaying images, but
cannot be tested here. Running `DISPLAY` without an image server will
result in a failure.

# Coordinate systems within IRAF

IRAF has support for three coordinate systems. The "logical"
coordinate system is defined by pixel coordinates relative to the
current image or image section.

```
cl> listpix dev$wpix[16:20,5:6] wcs=logical
1. 1.  41.
2. 1.  38.
3. 1.  41.
4. 1.  42.
5. 1.  40.
1. 2.  38.
2. 2.  38.
3. 2.  38.
4. 2.  39.
5. 2.  38.
```

The "physical" coordinate system is also in pixel coordinates but
relative to the original or parent image. 

```
cl> listpix dev$wpix[16:20,5:6] wcs=physical
16. 5.  41.
17. 5.  38.
18. 5.  41.
19. 5.  42.
20. 5.  40.
16. 6.  38.
17. 6.  38.
18. 6.  38.
19. 6.  39.
20. 6.  38.
```

The "world" coordinates can be in any general world coordinate system
such as right ascension and declination or wavelength.

```
cl> listpix dev$wpix[16:20,5:6] wcs=world format="%H %h"
13:28:05.1 47:24:01.4  41.
13:28:05.1 47:24:01.4  38.
13:28:05.0 47:24:01.4  41.
13:28:04.9 47:24:01.4  42.
13:28:04.8 47:24:01.4  40.
13:28:05.1 47:24:02.1  38.
13:28:05.1 47:24:02.2  38.
13:28:05.0 47:24:02.2  38.
13:28:04.9 47:24:02.2  39.
13:28:04.8 47:24:02.2  38.
```

# Additional interesting topics

## File and image name templates

Templates are character strings including some
metacharacters. Templates can be used as input to IRAF tasks; those
file names matching the template are used as input.

The following are a few examples of the more commonly used file templates.

```
cl> dir *.fits
image.dbl.fits      image.real.fits     image.sect.fits     image.short.fits
```

## The @file

The @file (pronounced "at file") can be used for handling large lists
of images for input and output. The @file is a text file containing a
list of images. The easiest way to generate an @file is with the `FILES`
or `SECTIONS` task. This is often the preferred input/output to tasks
rather than using templates directly. Since the @file is a text file
it can also be edited.

```
cl> files *.fits > inlist
cl> imstat @inlist
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
       image.dbl.fits    262144     108.3     131.3       -1.    19936.
      image.real.fits    262144     108.3     131.3       -1.    19936.
      image.sect.fits     10201     363.9      346.      108.     7734.
     image.short.fits    262144     108.3     131.3       -1.    19936.
```

## Using the CL as a calculator

The CL has a built-in calculator capability. Some variables that may
be used are defined in the parameter file for the CL which includes
the booleans b1, b2, and b3; the integer variables, i, j, and k; the
real variables, x, y, and z; and the string variables, s1, s2, and
s3. There are a variety of built-in functions that are also available
including sin, cos, abs, exp, log, log10, max, min, sqrt, and tan.

For more complex examples see the document An Introductory User's
Guide to IRAF Scripts, mentioned in §9.1.

```
cl> i=1;j=2;x=5;=i+x**j
26.
cl> =x
5.
cl> =sqrt(x/10)
0.70710678118655
cl> =(sin(0.5)**2+cos(0.5)**2)
1.
```

# Cleaning up

Hopefully all went well to this point. Let's clean things up a bit.

```
cl> dir
image.dbl.fits      image.sect.fits     inlist              uparmimlminmax.par
image.real.fits     image.short.fits    messier.xml         uparmntstinfo.par
cl> imdelete image.*
cl> dir
inlist              messier.xml         uparmimlminmax.par  uparmntstinfo.par
```

Remember that if you want to delete any images you just use the task
`imdelete`. The task `delete` will delete your text files. If the
wrong task is used to delete images a warning message is printed and
no images are deleted.

If discrepancies occur during any of these steps, please look at the
examples closely. It might be advisable to backtrack a few steps and
verify things again. If the discrepancies are repeatable there could
indeed be a problem. Please document the discrepancy and feel free to
contact us if some advice or help is needed (iraf@noao.edu).
