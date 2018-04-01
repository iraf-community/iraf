# CCD1: A look at CCD reductions for direct images

This exercise is designed to show you how IRAF deals with the preliminary
reductions of CCD data, including the overscan subtraction, the bias or
zero level subtraction, the dark subtraction, and the flat fielding.  The
images for this exercise are direct imaging data taken at the Kitt Peak
National Observatory by Dr. George Jacoby.

We will need to first unpack the images using the RFITS task.

```
cl> dataio
cl> rfits fm92* "" junk old+
File:  fm920001   -> junk0001.fits  ave of 25 bias        size=352x512
    bitpix=16  scaling=none  pixtype=short
    Image junk0001.fits renamed to m92001.imh
File:  fm920002   -> junk0002.fits  ave of  5 Harris B S  size=352x512
    bitpix=16  scaling=none  pixtype=short
    Image junk0002.fits renamed to m92006.imh
File:  fm920003   -> junk0003.fits  ave of  5 V filter d  size=352x512
    bitpix=16  scaling=none  pixtype=short
    Image junk0003.fits renamed to m92007.imh
File:  fm920004   -> junk0004.fits  M-92 V                size=352x512
    bitpix=16  scaling=none  pixtype=short
    Image junk0004.fits renamed to m92010.imh
File:  fm920005   -> junk0005.fits  M-92 V                size=352x512
    bitpix=16  scaling=none  pixtype=short
    Image junk0005.fits renamed to m92011.imh
File:  fm920006   -> junk0006.fits  M-92 B                size=352x512
    bitpix=16  scaling=none  pixtype=short
    Image junk0006.fits renamed to m92014.imh
File:  fm920007   -> junk0007.fits  M-92 B                size=352x512
    bitpix=16  scaling=none  pixtype=short
    Image junk0007.fits renamed to m92015.imh
```

There should be some IRAF images in this directory called m92*.imh.  There
should be a bias frame as well as two flat fields and four images all taken
through either V or B filters.

```
cl> imhead m92*.fits
m92001.fits[352,512][short]: ave of 25 bias
m92006.fits[352,512][short]: ave of  5 Harris B Sept 1 1987
m92007.fits[352,512][short]: ave of  5 V filter dome flat  1-sept-87
m92010.fits[352,512][short]: M-92 V
m92011.fits[352,512][short]: M-92 V
m92014.fits[352,512][short]: M-92 B
m92015.fits[352,512][short]: M-92 B
```

The bias frame is an average of 25 frames.  This is done to minimize the
noise.  Each flat is an average of 5 frames to improve the signal to noise.
Notice that the pixel type is "short" or 16-bit data.

## CCD reduction with IMRED

The first step would be to average the bias frames.  This can be done
with the task IMCOMBINE in the IMAGES.IMMATCH package.  We would then do the 
same for the flats.  Some type of pixel rejection could be used during this 
step to eliminate bad pixels or cosmic rays.  Since these steps have already 
been done for us we can continue on to the overscan subtraction.

Do you understand the output image names?  Try the following to see what
the actual names on output will be.  The task SECTIONS can be used to test
image templates. In this case, the % sign brackets that part of the image
name we wish to replace (m) and what we wish to replace it with (tr).

```
cl> sections %m%tr%92*.fits
tr92001.fits
tr92006.fits
tr92007.fits
tr92010.fits
tr92011.fits
tr92014.fits
tr92015.fits
```

We need to determine two things at this point:  the overscan region to 
subtract and the trimming parameters to determine the output image size.  For
this chip the overscan region is 32 columns but we often do not use all of the
columns.  The overscan region and any bad rows or columns along the edges of 
the frame are then trimmed from the image to produce our output image.  We
determine these parameters with IMPLOT, using one of the flat field frames.

The values that I determined for the trimming parameters are 1-318 for the
columns, and 2-510 for the rows.  Do you agree?

Once we have this information we are ready to do the overscan subtraction
and trimming.  Load the packages.  And then edit the task COLBIAS to reflect
the values that we determined.

Notice that the overscan and trim values are entered as "image sections",
the x-range and y-range in square brackets.  The trim section is that part
of the image we wish to keep.

So, I think we are ready to execute COLBIAS - this task will subtract the 
overscan from each image and then trim the image according to our 
specifications.

```
cl> noao
cl> imred
cl> bias
cl> colbias.bias="[335:350,2:510]"
cl> colbias.trim="[1:318,2:510]"
cl> colbias.interactive=no
cl> colbias.input="m92*.fits"
cl> colbias.output="%m%tr%92*.fits"
cl> lpar colbias
        input = "m92*.fits"     Input images
       output = "%m%tr%92*.fits" Output images
        (bias = "[335:350,2:510]") Bias section
        (trim = "[1:318,2:510]") Trim section
      (median = no)             Use median instead of average in column bias?
 (interactive = no)             Interactive?
    (function = "spline3")      Fitting function
       (order = 1)              Order of fitting function
  (low_reject = 3.)             Low sigma rejection factor
 (high_reject = 3.)             High sigma rejection factor
    (niterate = 1)              Number of rejection iterations
    (logfiles = "")             Log files
    (graphics = "stdgraph")     Graphics output device
      (cursor = "")             Graphics cursor input
        (mode = "ql")           
cl> colbias "m92*.fits" "%m%tr%92*.fits"
cl> dir
fm920001            fstds.dat           m92015.fits         tr92011.fits
fm920002            m92001.fits         m92fig              tr92014.fits
fm920003            m92006.fits         stds                tr92015.fits
fm920004            m92007.fits         tr92001.fits        uparmimlsectis.par
fm920005            m92010.fits         tr92006.fits        
fm920006            m92011.fits         tr92007.fits        
fm920007            m92014.fits         tr92010.fits        
cl> imhead tr*.fits
tr92001.fits[318,509][real]: ave of 25 bias
tr92006.fits[318,509][real]: ave of  5 Harris B Sept 1 1987
tr92007.fits[318,509][real]: ave of  5 V filter dome flat  1-sept-87
tr92010.fits[318,509][real]: M-92 V
tr92011.fits[318,509][real]: M-92 V
tr92014.fits[318,509][real]: M-92 B
tr92015.fits[318,509][real]: M-92 B
```

Let's check how BIAS subtraction worked:

```
cl> imstat "tr92*.fits"
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
         tr92001.fits    161862     1.578     4.234    -5.625     34.39
         tr92006.fits    161862     1328.     21.42     878.6     1471.
         tr92007.fits    161862     1465.     23.48     990.5     1576.
         tr92010.fits    161862     48.44     93.76    -5.384    10144.
         tr92011.fits    161862      48.2     84.93    -5.827     7826.
         tr92014.fits    161862     44.88     55.13    -3.731     4437.
         tr92015.fits    161862     44.78      58.4    -3.135     5166.
```

The next step is to subtract the bias or zero frame from each of the
images.  This is best done with IMARITH.  Let us first create a file with
a list of the images to process; we will use this as input and output to
IMARITH, overwriting our input data.

```
cl> files tr92006.fits > zlist
cl> files tr92007.fits >> zlist
cl> files tr9201*.fits >> zlist
cl> imhead @zlist
tr92006.fits[318,509][real]: ave of  5 Harris B Sept 1 1987
tr92007.fits[318,509][real]: ave of  5 V filter dome flat  1-sept-87
tr92010.fits[318,509][real]: M-92 V
tr92011.fits[318,509][real]: M-92 V
tr92014.fits[318,509][real]: M-92 B
tr92015.fits[318,509][real]: M-92 B
```

Execute IMARITH.

```
cl> imarith  @zlist - tr92001.fits @zlist
cl> imstat @zlist
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
         tr92006.fits    161862     1326.      20.5      878.     1458.
         tr92007.fits    161862     1464.     22.36     989.9     1553.
         tr92010.fits    161862     46.86     93.69    -7.766    10140.
         tr92011.fits    161862     46.62     84.86    -7.208     7823.
         tr92014.fits    161862     43.31     55.02    -5.113     4440.
         tr92015.fits    161862      43.2      58.3    -4.509     5169.
```

At this time, any dark subtraction would be done.  That would be done
using the task DARKSUB in the NOAO.IMRED.GENERIC package.  The frames
need to be scaled by exposure time before the subtraction is done, so
this information would need to be in the header.  We have no dark frames
so we will skip this step.

We finally arrive at the flat fielding stage.  We have two flats and they
need to be normalized before we divide them into our object frames.  
We will use IMSTATISTICS to determine the normalization value for each flat,
and then use IMARITH to create the normalized flats.

```
cl> imstat tr92006.fits,tr92007.fits fields="image,mode"
#               IMAGE      MODE
         tr92006.fits     1328.
         tr92007.fits     1469.
cl> imarith tr92006.fits / 1328 Bflat.fits
cl> imarith tr92007.fits / 1469 Vflat.fits
cl> imstat *flat.fits
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
           Bflat.fits    161862    0.9985   0.01544    0.6612     1.098
           Vflat.fits    161862    0.9965   0.01522    0.6739     1.057
```

Now we can divide each of the object frames by the appropriate flat.

```
cl> imarith tr92010,tr92011 / tr92007.fits n92010,n92011
cl> imarith tr92014,tr92015 / tr92006.fits n92014,n92015
cl> imhead n92*.fits
n92010.fits[318,509][real]: M-92 V
n92011.fits[318,509][real]: M-92 V
n92014.fits[318,509][real]: M-92 B
n92015.fits[318,509][real]: M-92 B
cl> imstat n92*.fits
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
          n92010.fits    161862   0.03201   0.06395 -0.005262     6.941
          n92011.fits    161862   0.03185   0.05788  -0.00487     5.272
          n92014.fits    161862   0.03265   0.04145 -0.003788     3.313
          n92015.fits    161862   0.03258   0.04392 -0.004163      3.84
```

Look at these final images with DISPLAY and/or IMPLOT.  Check to see if 
the sky is flat across the image.  Sometimes the dome flats are not 
sufficient for flattening images - additional sky flats may need to be used.
See the task MKSKYFLAT in the CCDRED package.

At this point we may want to delete the results since we are going to
reprocess the raw data again, but using the other path.

```
cl> imdelete tr*.fits,n92*.fits,Bflat.fits,Vflat.fits
cl> del zlist
cl> imhead m92*.fits
m92001.fits[352,512][short]: ave of 25 bias
m92006.fits[352,512][short]: ave of  5 Harris B Sept 1 1987
m92007.fits[352,512][short]: ave of  5 V filter dome flat  1-sept-87
m92010.fits[352,512][short]: M-92 V
m92011.fits[352,512][short]: M-92 V
m92014.fits[352,512][short]: M-92 B
m92015.fits[352,512][short]: M-92 B
```

## CCD reduction with CCDRED

Let's check to see what files are in our directory.

```
cl> imhead m92*.fits
m92001.fits[352,512][short]: ave of 25 bias
m92006.fits[352,512][short]: ave of  5 Harris B Sept 1 1987
m92007.fits[352,512][short]: ave of  5 V filter dome flat  1-sept-87
m92010.fits[352,512][short]: M-92 V
m92011.fits[352,512][short]: M-92 V
m92014.fits[352,512][short]: M-92 B
m92015.fits[352,512][short]: M-92 B
```

We want to use the tasks in the CCDRED package now to reduce these same data.
This is a much more streamlined technique.

The CCDRED package will process our data in the same way as we did 
previously.  However, the steps are combined into one task;  and we
can use the information in the headers of the images to drive the task.

The CCDRED package looks for certain keywords and values in the header.  If
the keywords and values have different names than those expected by the 
package then a "translation" file can be used.  The package expects
the keywords IMAGETYP (with values "object", "flat", "zero", among others),
EXPTIME (for dark subtraction), SUBSET (to define the filters), just to 
mention the ones we will be using.  

The task CCDLIST can be used as a check to be sure the package is picking
up the header information correctly.

```
cl> noao
cl> imred
cl> ccdred
cl> unlearn ccdred
cl> lpar ccdlist
       images =                 CCD images to listed
     (ccdtype = "")             CCD image type to be listed
       (names = no)             List image names only?
        (long = no)             Long format listing?
     (ccdproc = "")             CCD processing parameters
        (mode = "ql")           
cl> ccdlist m92*.fits
m92001.fits[352,512][short][unknown][]:ave of 25 bias
m92006.fits[352,512][short][unknown][]:ave of  5 Harris B Sept 1 1987
m92007.fits[352,512][short][unknown][]:ave of  5 V filter dome flat  1-sept-87
m92010.fits[352,512][short][unknown][]:M-92 V
m92011.fits[352,512][short][unknown][]:M-92 V
m92014.fits[352,512][short][unknown][]:M-92 B
m92015.fits[352,512][short][unknown][]:M-92 B
cl> ccdlist m92*.fits l+
m92001.fits[352,512][short][unknown][]:ave of 25 bias
    exposure=0 darktime=0
    [TO BE DONE] Bad pixel file needs to be specified
    [TO BE DONE] Overscan strip is 
    [TO BE DONE] Trim image section is 
    [TO BE DONE] Zero level correction
    [TO BE DONE] Dark count correction
    [TO BE DONE] Flat field correction
m92006.fits[352,512][short][unknown][]:ave of  5 Harris B Sept 1 1987
    exposure=15 darktime=15
    [TO BE DONE] Bad pixel file needs to be specified
    [TO BE DONE] Overscan strip is 
    [TO BE DONE] Trim image section is 
    [TO BE DONE] Zero level correction
    [TO BE DONE] Dark count correction
    [TO BE DONE] Flat field correction
m92007.fits[352,512][short][unknown][]:ave of  5 V filter dome flat  1-sept-87
    exposure=7 darktime=7
    [TO BE DONE] Bad pixel file needs to be specified
    [TO BE DONE] Overscan strip is 
    [TO BE DONE] Trim image section is 
    [TO BE DONE] Zero level correction
    [TO BE DONE] Dark count correction
    [TO BE DONE] Flat field correction
m92010.fits[352,512][short][unknown][]:M-92 V
    exposure=60 darktime=60
    [TO BE DONE] Bad pixel file needs to be specified
    [TO BE DONE] Overscan strip is 
    [TO BE DONE] Trim image section is 
    [TO BE DONE] Zero level correction
    [TO BE DONE] Dark count correction
    [TO BE DONE] Flat field correction
m92011.fits[352,512][short][unknown][]:M-92 V
    exposure=60 darktime=60
    [TO BE DONE] Bad pixel file needs to be specified
    [TO BE DONE] Overscan strip is 
    [TO BE DONE] Trim image section is 
    [TO BE DONE] Zero level correction
    [TO BE DONE] Dark count correction
    [TO BE DONE] Flat field correction
m92014.fits[352,512][short][unknown][]:M-92 B
    exposure=120 darktime=120
    [TO BE DONE] Bad pixel file needs to be specified
    [TO BE DONE] Overscan strip is 
    [TO BE DONE] Trim image section is 
    [TO BE DONE] Zero level correction
    [TO BE DONE] Dark count correction
    [TO BE DONE] Flat field correction
m92015.fits[352,512][short][unknown][]:M-92 B
    exposure=120 darktime=120
    [TO BE DONE] Bad pixel file needs to be specified
    [TO BE DONE] Overscan strip is 
    [TO BE DONE] Trim image section is 
    [TO BE DONE] Zero level correction
    [TO BE DONE] Dark count correction
    [TO BE DONE] Flat field correction
```

Since this is KPNO data we already have a translation file set up so let's
use it and see what happens.

```
cl> noao
cl> imred
cl> ccdred
cl> setinstrument direct review=no
cl> ccdlist m92*.fits
m92001.fits[352,512][short][zero][2]:ave of 25 bias
m92006.fits[352,512][short][flat][5]:ave of  5 Harris B Sept 1 1987
m92007.fits[352,512][short][flat][6]:ave of  5 V filter dome flat  1-sept-87
m92010.fits[352,512][short][object][6]:M-92 V
m92011.fits[352,512][short][object][6]:M-92 V
m92014.fits[352,512][short][object][5]:M-92 B
m92015.fits[352,512][short][object][5]:M-92 B
cl> type subsets
'2 0'	2
'5 0'	5
'6 0'	6
cl> dir ccddb$kpno
Revisions           demo.cl             fibers.dat          kpnoheaders.dat
camera.dat          demo.dat            fits.dat            specphot.cl
coude.cl            direct.cl           foe.cl              specphot.dat
coude.dat           direct.dat          foe.dat             sunlink.cl
cryocam.cl          echelle.cl          hydra.cl            sunlink.dat
cryocam.dat         echelle.dat         hydra.dat           template.cl
default.cl          fibers.cl           instruments.men     
cl> type ccddb$kpno/direct.dat
subset		filters

DARK		dark
BIAS		zero
OBJECT		object
'DOME FLAT'	flat
'PROJECTOR FLAT' flat
'COMPARISON'	comp
'SKY FLAT'      object
cl> ccdred.verbose=no
cl> lpar ccdred
   (pixeltype = "real real")    Output and calculation pixel datatypes
     (verbose = no)             Print log information to the standard output?
     (logfile = "logfile")      Text log file
    (plotfile = "")             Log metacode plot file
      (backup = "")             Backup directory or prefix
  (instrument = "ccddb$kpno/direct.dat") CCD instrument file
      (ssfile = "subsets")      Subset translation file
    (graphics = "stdgraph")     Interactive graphics output device
      (cursor = "")             Graphics cursor input
     (version = "2: October 1987") 
        (mode = "ql")           
      ($nargs = 0)              
```

Now the CCDRED package knows about the headers.  Notice that the package
takes care of our pixel type for us as well.  Remember that our pixel
type is "short" but the "pixeltype" parameter will let us control both
the calculation type and output type during processing.  During the
actually processing the input images are overwritten; the "backup" parameter
would let us make copies of the original data first if we wanted.

Biases and flat frames can be combined using the tasks ZEROCOMBINE and 
FLATCOMBINE.  But we will skip these steps since we have data that have already
been combined.  We are now ready to set up the parameters for CCDPROC.
Notice the two parameters called "biassec" and "trimsec".  These are currently
set to "image" - if these keywords have the correct value in the image header
then we need to do nothing.  But closer inspection will show that the values
that we computed earlier are different from the header values.

```
cl> noao
cl> imred
cl> ccdred
cl> ccdproc.biassec="[335:350,2:510]"
cl> ccdproc.trimsec="[1:318,2:510]"
cl> ccdproc.images="m92*.fits"
cl> ccdproc.ccdtype="object"
cl> ccdproc.interactive=no
cl> lpar ccdproc
       images = "m92*.fits"     List of CCD images to correct
      (output = "")             List of output CCD images
     (ccdtype = "object")       CCD image type to correct
   (max_cache = 0)              Maximum image caching memory (in Mbytes)
      (noproc = no)             List processing steps only?\\n
      (fixpix = no)             Fix bad CCD lines and columns?
    (overscan = yes)            Apply overscan strip correction?
        (trim = yes)            Trim the image?
     (zerocor = yes)            Apply zero level correction?
     (darkcor = no)             Apply dark count correction?
     (flatcor = yes)            Apply flat field correction?
    (illumcor = no)             Apply illumination correction?
   (fringecor = no)             Apply fringe correction?
     (readcor = no)             Convert zero level image to readout correction?
     (scancor = no)             Convert flat field image to scan correction?\\n
    (readaxis = "line")         Read out axis (column|line)
     (fixfile = "")             File describing the bad lines and columns
     (biassec = "[335:350,2:510]") Overscan strip image section
     (trimsec = "[1:318,2:510]") Trim data section
        (zero = "")             Zero level calibration image
        (dark = "")             Dark count calibration image
        (flat = "")             Flat field images
       (illum = "")             Illumination correction images
      (fringe = "")             Fringe correction images
  (minreplace = 1.)             Minimum flat field value
    (scantype = "shortscan")    Scan type (shortscan|longscan)
       (nscan = 1)              Number of short scan lines\\n
 (interactive = no)             Fit overscan interactively?
    (function = "chebyshev")    Fitting function
       (order = 1)              Number of polynomial terms or spline pieces
      (sample = "*")            Sample points to fit
    (naverage = 1)              Number of sample points to combine
    (niterate = 1)              Number of rejection iterations
  (low_reject = 3.)             Low sigma rejection factor
 (high_reject = 3.)             High sigma rejection factor
        (grow = 0.)             Rejection growing radius
        (mode = "ql")    
```

Since the "zero" and "flat" images are in the input list it is not
necessary to specify them.  Try running the task and see what happens.

Test options: `decimals=2`
```
cl> noao
cl> imred
cl> ccdred
cl> ccdproc m92*.fits
cl> imhead m92*.fits
m92001.fits[318,509][real]: ave of 25 bias
m92006.fits[318,509][real]: ave of  5 Harris B Sept 1 1987
m92007.fits[318,509][real]: ave of  5 V filter dome flat  1-sept-87
m92010.fits[318,509][real]: M-92 V
m92011.fits[318,509][real]: M-92 V
m92014.fits[318,509][real]: M-92 B
m92015.fits[318,509][real]: M-92 B
cl> imstat m92*.fits
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
          m92001.fits    161862     1.578     4.236    -5.577     34.42
          m92006.fits    161862     1326.     20.55     877.8     1458.
          m92007.fits    161862     1464.     22.38     989.9     1553.
          m92010.fits    161862     46.84      93.6    -7.802    10161.
          m92011.fits    161862     46.62     84.72    -7.202     7717.
          m92014.fits    161862     43.29     54.96    -5.034     4394.
          m92015.fits    161862     43.21     58.24    -5.287     5093.
```

Do not delete these images since they will be used in a later exercise.

