.help daophot Sep87 "Crowded Field Stellar Photometry'
.sh
1. Introduction

     The DAOPHOT package will provide a set of routines for performing
stellar photometry on crowded fields in either interactive or batch mode.
DAOPHOT works by fitting an empirical point spread function (PSF)
to each object in the field
allowing for overlap of closely spaced images. This document presents the
the requirements and specifications for the package and describes some of
the algorithms to be used. Most of the algorithms are described in
the original article by Peter Stetson (1987 P.A.S.P. 99,191).

.sh
2. Requirements
.ls 4
.ls (1)
The tasks in the DAOPHOT package shall take as input an IRAF imagefile
containing two-dimensional image data which has been corrected for 
pixel to pixel gain variations, high frequency variations in the background,
any nonlinearitys in the data except for those which can be specified as
a lower and/or upper bound,
and any other instrumental defects affecting the intensity value of an
individual pixel. However, it shall be possible to exclude bad pixels,
rows or columns from analysis by DAOPHOT routines in a very crude manner.
.le
.ls (2)
The tasks in the package which produce tabular output shall use the
SDAS Tables for their output and those tasks which read output from other
DAOPHOT tasks will be able to read SDAS Tables. In the future the input/output
shall make use of the DBIO package.
.le
.ls (3)
The DAOPHOT package shall work in conjunction with the APPHOT package produced
at NOAO. DAOPHOT will not have any provision to do aperture photometry of its
own. The output format from DAOPHOT tasks will be consistent with APPHOT.
.le
.ls (4)
Given as input a reduced two-dimensional image which has been processed by the
APPHOT package, the DAOPHOT package shall be able to perform the following
functions:
.ls 4
.ls o
Interactively define a PSF for the data frame. The PSF will be defined 
empirically from one or more stars in the field. The task to determine the
PSF shall be interactive and the user shall be able to use a
graphics terminal and/or an image display device to select the stars which
will make up the PSF. The user will be able to evaluate the PSF through 
different means including contour plots, 3-d mesh plots, and displaying the
PSF on an image display device.
The user shall be able to "mask" out parts of the PSF which may be contaminated 
by nearby stars, bad pixels etc. Only the non-masked portions of the PSF will
be used in the fitting routines.
.le
.ls o
Fit the PSF simultaneously to groups of stars in the image frame whose 
images overlap to some degree. The parameters in the fit shall include the
the object brightness, X and Y position of the star and potentially the sky
background. The sky shall be able to be specified as either a flat uniform
background or a simple tilted planar sky. The photometry routines shall
produce realistic errors in the photometry assuming that realistic numbers
for the characteristics of the data are input.
.le
.ls o
Subtract the fitted stars from the data frame to produce a subtracted 
image for further analysis.
.le
.ls o
Add artificial stars to the data frame in order to check accuracy and
completeness in the photometry. The user shall have control over the
number of stars added, the brightness range, the area of the image to contain
the added stars and the noise characteristics of the added stars.
.le
.le
.ls (5)
The DAOPHOT package shall include tasks to inspect and edit the results from the
photometry routines. These shall include tasks such as interactively
rejecting particular stars from the results, 
producing plots of errors versus brightness, errors versus position etc. 
.le
.ls (6)
The DAOPHOT package shall provide utility packages to handle the output
data from the fitting routines. These shall include such tasks as 
aperture growth curves, photometric calibrations, color-magnitude and
color-color diagrams.
.le
.ls (7) 
The DAOPHOT routines shall optionally keep a history file to keep 
track of the processing done on the images. This will include the values of
various parameters used in the various tasks of the DAOPHOT package.
.le
.ls (8) 
The tasks shall be able to be run in batch mode as well as interative
mode. In batch mode use of a graphics terminal or image display shall not
be required.
.le
.ls (9)
The DAOPHOT package shall be written in the SPP language in conformance with
the standards and conventions of IRAF. The code shall be portable and
device independent.
.le
.le
.sh
2.1 Limitations of the Initial DAOPHOT Package

The DAOPHOT package shall perform PSF fitting photometry with the following
restrictions:
.ls
.ls (1)
The PSF used will be determined empirically and analytic specification of
the PSF will not be possible. This restricts the use of DAOPHOT to image 
data which is not too badly undersampled.
.le
.ls (2)
There will be an upper limit to the number of stars for which the PSF will
be fit simultaneously. The initial version of DAOPHOT will have this limit
set to 60 stars.
.le
.ls (3)
The initial version of DAOPHOT will not have the sky included as a parameter
in the fitting routines. 
.le
.ls (4)
Initially the use will not be able to mask out bad portions of the PSF for
fitting.
.le
.le

.sh
3. Specifications

     The DAOPHOT package performs stellar photometry on digital data, maintained
as IRAF image files. DAOPHOT performs this photometry by fitting the PSF
to the stellar images in the image file. DAOPHOT works by fitting the PSF to 
a maximum number of stars simultaneously thus allowing for overlapping images.
Input to the package consists of an imagefile and the output from the APPHOT
package, which contains simple aperture photometry for the objects which have
been identified in the image frame, and numerous parameters controlling the
analysis algorithms. Output from the analysis tasks consists of tabular data
containing the results of the analysis routines. The output will be in the
form of SDAS tables and will thus be able to be manipulated by various
other utility tasks available in IRAF.

The CL callable part of the DAOPHOT package consists of the following routines:

.ks
.nf
	addstar	  -- adds synthetic stars to an image file
	allstar   -- fits multiple, overlapping PSFs to star images
	*calibrate -- apply photometric calibration
	*cmd	  -- color-magnitude, color-color diagrams
	daopars    -- DAOPHOT pset parameters
	examine   -- interactively examine/edit photometry results
 	group     -- divides stars into natural groupings
	*growth    -- aperture growth curves <--> PSF magnitudes
	peak      -- fit PSF to single stars in an image file
	psf       -- interactively construct a PSF for the frame
	nstar     -- fits multiple, overlapping PSFs to star images
	seepsf	  -- converts a PSF file into a IRAF image file
	select    -- selects natural groups with a certain range of sizes
	substar   -- subtract fitted profiles from an image file
.fi
.ke

There are routines available in other IRAF/STSDAS tasks for manipulating 
SDAS Tables or DBIO. The capabilities inside the DAOPHOT are specifically 
suited to dealing with large tables of results from these photometry routines. 

.sh
3.1 Standard Analysis Procedures

     Before performing DAOPHOT photometry one must perform certain other tasks
beforehand. This includes using the APPHOT package to produce an object list
and aperture photometry for objects in this list. The DAOPHOT package contains
an additional object finder but one must use APPHOT to obtain the aperture
photometry results. The standard analysis procedure, including APPHOT, is as
follows:
.ls
.ls (1)
Use an object finder to produce a list of object coordinates. This may be done
in many ways:
.ls
.ls o
By using the interactive cusrsor routines available elsewhere in IRAF and
redirecting the output into a list file.
.le
.ls o
By transforming an existing list using an existing IRAF task or the OFFSET
task in the DAOPHOT package.
.le
.ls o
By using an automatic object finding procedure such as the one available
in the APPHOT package or the one in the DAOPHOT package.
.le
.ls o
By any other program which generates a list of objects in suitable format (SDAS
Tables) for input to the APPHOT routines.
.le
.le
.ls (2)
The APPHOT package is run to measure the objects identified in the above
step. One should refer to the APPHOT documentation to understand the
algorithms and procedures which are used in APPHOT.
.le
.ls (3)
One needs to set up the parameters in the analysis routines for this particular
image file. OPTIONS allows you to set such parameters as the number of 
electrons/ADC, the fitting radius, and the radius within which the PSF 
is defined.
.le
.ls (4)
The next step is to produce a PSF for the image file currently being processed.
In crowed fields this is a tricky, iterative procedure which should be done very
carefully. This is best done using a graphics terminal and/or an image display
device.
.le
.ls (5)
If one plans on using NSTAR, then the GROUP task must be run. This task 
divides the stars in the output from the APPHOT into natural groups. The size
of the groups produced depends upon how crowded the field is and what degree of
overlap of the images one considers.
.le
.ls (6) 
Use either NSTAR, if you have grouped the objects using GROUP, or 
ALLSTAR which will dynamically group the stars as the image file is 
processed. These routines will produce the objects' positions and 
intrumental magnitudes by means of multiple-profile fits.
.le
.ls (7)
Use SUBSTAR to subtract the fitted profiles from the image file, thus producing
a new image file containing the fitting residuals. This will usually contain
many stars which were missed in the original identification because they lie
in the wings of brighter objects.
.le
.ls (8)
One now basically runs through steps (1) - (6) one or more times, 
merging the identified object lists each time to produce a master object list,
until one is satisfied with the final results. There are many subtlties in this
procedure which are described in the DAOPHOT User's Manual.
.le
.ls (9)
After obtaining the photometry results one may edit the results by throwing out
those results which do not meet certain criteria. EXAMINE is an interactive
task which allows the user to examine the results for each individual object
in the list and either accept or reject that object. There are also routines
available for courser rejection of results, e.g. reject all objects with
errors larger than 0.2 magnitudes.
.le
.ls (10)
One may wish to use the tasks to plot up color-color or color-magnitude
diagrams. Other general purpose list processing tools available in
IRAF/SDAS may also be used for analysis of DAOPHOT output.
.le
.le

.sh 
3.2 The ADDSTAR Task

     The function of ADDSTAR is to add synthetic stars to the image file.
These stars may be placed randomly by the computer, placed with a certain
distribution as specifed by the user or at predetermined locations specified
by the user. Likewise the brightness of these added objects may be completely
random or may follow a specified distribution. 

Objects are added by taking the specified PSF, scaling it, and moving it
to the desired location. ADDSTAR will also add Poisson noise to the star
images to make them more realistic.

.sh
3.2.1 ADDSTAR Parameters

     ADDSTAR has several parameters which control the addition of stars
into a image file. All data dependent parameters are query mode to ensure
that they get set properly for the particular image under consideration.
The data independent parameters are hidden mode, and are given reasonable
default values. The names, datatypes, and default values of the ADDSTAR
parameters are shown below.

.ks
.nf
Positional or query mode parameters:

	input_image	filename
	output_image	filename
	minmag		real
	maxmag		real
.fi
.ke

.ks
.nf
List structured parameter (filename may be given on command line):

	add_data	*imcur
.fi
.ke

.ks
.nf
Hidden Parameters:

	daopars		pset		"daophot$daopars.par"
	nstar		integer		100
	nframe		integer		1
	xmin		integer		1
	ymin		integer		1
	xmax		integer		NX
	ymax		integer		NY
	verbose		boolean		false
.fi
.ke

The function and format of each of these parameters is explained in
more detail below.

.ls
.ls 16 input_image
The name of the image or image section to which artificial stars will be added
.le
.ls output_image
The name of outout image which will contain the added stars.
.le
.ls minmag
The minumum magnitude of artificial star to add to the data. The magnitude
scale is set by the magnitude of the PSF.
.le
.ls maxmag
The maximum magnitude of artificial star to add to the data. The magnitude
scale is set by the magnitude of the PSF.
.le
.ls add_data
This parameter is used to specify a file as input to the ADDSTAR task. This
file should contain centroid positions and magnitudes for the stars you 
want to add. It is possible to specify the positions of the added stars
interactively with the image display by setting this parameter to *imcur.
In this case the user is prompted for the magnitude of each star to be added.
If this parameter is the null string then the stars are added in a random
fashion by the ADDSTAR routine.
.le
.ls nstar
The number of artificial stars to add to the input image file.
.le
.ls daopars
This is the name of a file containing parameters which are common to
many DAOPHOT tasks. This pset parameter serves as a pointer to the external
parameter set for the DAOPHOT algorithms. The parameters contained in this
pset and their function are described in section 3.6.1.
.le
.ls nframe
The number of new image files to create. If this parameter is greater
than one then the new image files will use the output image name as
a root and produce image files with '.xxx' appended to the root, where
xxx will range from 001 to nframe. If nframe is one then the output image
name will be used as is.
.le
.ls xmin, ymin, xmax, ymax
These define the subsection of the image in which to add the artificial
stars. The default is to add artificial stars to the complete image.
.le
.ls verbose
Controls the amount of output from the ALLSTAR function. The default is
to have minimal output to STDOUT.
.le
.le

.sh
3.2.2 ADDSTAR Output

     The output of ADDSTAR consists of two parts, an image file and an
output SDAS Table. The image file is a copy of the input image file but
with the artificial stars generated by ADDSTAR added. The output table
contains the x,y position and magnitude of each of the added stars. When the
nframe parameter is set greater than one then there will be nframe pairs of
output files generated.

.sh
3.3 ALLSTAR

     ALLSTAR fits multiple, overlapping point-spread functions to stars images
in the input image file. It uses as input the results from APPHOT and an
input PSF and will automatically reduce the entire image performing the necessary
grouping. It will recalculate the grouping after each iteration. ALLSTAR will
also produce the star-subtracted image file.

.sh
3.3.1 ALLSTAR Parameters

ALLSTAR has several parameters which control the fitting algorithms. The 
names, datatypes, default values for the ALLSTAR parameters are given below.

.ks
.nf
Positional parameters:

	input_image	filename
	photometry	filename
	output		filename
	sub_image	filename
.fi
.ke

.ks
.nf
Hidden parameters:

	daopars		pset		"daophot$daopars.par"
	max_group	integer		60
	redeterm_cent	boolean		true
	max_crit	real		2.5
	min_crit	real		1.2
	clip_exp	integer		6
	clip_range	real		2.5
	verbose		boolean		false
.fi
.ke

These parameters perform the following functions:

.ls 4
.ls 16 input_image
The name of the input image file.
.le
.ls photometry
The name of the input photometry SDAS table. This may contain output from either
the APPHOT package or from NSTAR or previous ALLSTAR runs.
.le
.ls output
The name of the SDAS table to contain the results of the psf fittting.
.le
.ls sub_image
The name of the output image file which will have all of the fitted stars
subtracted from it. If this file is the null string then no star-subtracted
image file will be produced.
.le
.ls daopars
The pset parameter file containing the DAOPHOT parameter set.
.le
.ls max_group
The maximum size group which ALLSTAR will process. The absolute maximum
is 60 stars.
.le
.ls redeterm_cent
If true then the centers of the stars are redetermined before each 
iteration.
.le
.ls max_crit
The initial value which ALLSTAR uses as the critical separation for
use in grouping stars together. For groups larger than "max_group" ALLSTAR
will use progressively smaller values for the critical separation until  the
group breaks up into units containing fewer than "max_group" stars or until
the value of "min_crit" is reached.
.le
.ls min_crit
The smallest value of the critical separation which ALLSTAR will use in
grouping stars together.
.le
.ls clip_exp, clip_range
These parameters are used to "resist bad data". These two 
parameters control the weighting of each pixel as a function of it's
residual from the fit. Clip_range us variable "a" and clip_exp is
variable "b" in the paper by Stetson (P.A.S.P. 99, 191)
.le
.le

.sh
3.3.2 The ALLSTAR PSF Fitting Algorithm

     The algorithms which ALLSTAR uses to do the psf fitting photometry are 
very nearly the same as those used by NSTAR. One is referred to Stetson,
P.A.S.P. 99, 191, for the details on the various fitting, star rejection,
and weighting algorithms used in this task.
.sh
3.3.3 The Output from ALLSTAR

     The output from ALLSTAR consists of three parts. There is the output
photometry results, an SDAS Table, and a subtracted image file. The subtracted
image file is a copy of the input image file minus the fitted stars.

For each object processed by ALLSTAR there is one row in the output SDAS
Table. Each measured object will have entries for the following items:

.nf
     star, x, y, mag, magerr, sky, niter, chi, sharp

where

     star	star ID number
     x,y	coordinates of the stellar centroid
     mag	magnitude relative to the magnitude of the PSF star
     magerr	estimated standard error of the star's magnitude
     sky	estimated sky as returned by APPHOT
     niter	number of iterations for convergence
     chi	observed pixel to pixel scatter DIVIDED BY the expected
                pixel to pixel scatter
     sharp	an index describing the spatial distribution of the residuals
                around the star. Objects with SHARP significantly greater
                than zero are extended (possibly galaxies), while objects with
                SHARP significantly less than zero may be bad pixels or cosmic
                rays
.fi

Other noteworthy pieces of information will be stored in the output SDAS
Table header. This includes such things as the time and date of processing,
the name of the PSF file, the name of the input photometry file, the
fitting radius etc.

.sh 
3.4 The CALIBRATE Task

.sh 
3.5 The CMD Task

.sh 
3.6 The DAOPARS Task

     This is a pset-task which is used to describe a particular image file
for use with the DAOPHOT package. This pset contains parameters which describe the
data, e.g. the read out noise, the background sky value, the number of photons
per ADC unit, etc., and also parameters which control the DAOPHOT tasks, e.g.
the fitting radius to use. The parameters in this pset are used by several
DAOPHOT tasks, hence their grouping into a pset.

.sh
3.6.1 daopars Parameters

     The parameters in this task either describe the data in
a particular image file
or are parameters which are used by more algorithms in more than one
DAOPHOT task. The following parameters make up this pset:

.ks
.nf

	fitrad		real		2.5 (pixels)
	psfrad		real		11.0(pixels)
	phot_adc	real		10.0
	read_noise	real		20.0
	max_good	real		32766.
	min_good	real		0.0
	sky_val		real		0.0
	numb_exp	integer		1
	comb_type	string		"average"
	var_psf		boolean		false
.fi
.ke

The function and format of each of these parameters is described below:

.ls 4
.ls 16 fitrad
The fitting radius to use in the PEAK, NSTAR, ALLSTAR and PSF tasks. Only
the pixels within one fitting radius are actually used in the fit. This should
normally be on the order of the FWHM of the stellar images.
.le
.ls psfrad
The radius of the circle within which the PSF is defined. This should be 
somewhat larger than the actual radius of the brightest star you are 
interested in.
.le
.ls maxgood
The maximum data value in ADC units at which the CCD or other detector
is believed to operate linearly.
.le
.ls mingood
The minimum data value in ADC units which should be used as "real" data.
Dead pixels, bad columns etc. in the image file can be excluded from use in 
the analysis by setting this parameters properly. Any data value which
falls below this minimum is ignored by DAOPHOT tasks.
.le
.ls sky_val
The typical sky brightness in ADC units for the image file. This parameter is
updated by the SKY task within the  DAOPHOT package.
.le
.ls phot_adc
The number of photons per ADC unit of the CCD or other detector.
.le
.ls read_noise
The readout noise in ADC units of the CCD or other detector.
.le
.ls numb_exp
The number of individual exposures which have been combined to produce the
current image file. This number combined with information on whether the
exposures were summed or averaged is used to get a better handle on the
error estimates of the photometry.
.le
.ls comb_type
Describes whether the individual exposures which went into making up this
image file were "summed" or "averaged"
.le
.ls var_psf
Controls whether the shape of the PSF is to be regarded as constant over the
complete image file. Slight and smooth variations can be accomodated by the
DAOPHOT tasks.
.le
.le

     These parameters should be initially set by the user before starting any
analysis with the DAOPHOT package. Each image file may have it's own set of
parameters and these should be stored in separate pset files. 
.sh 
3.7 The EXAMINE Task

     EXAMINE allows the user to interactively examine the results of the
DAOPHOT reduction and to accept or reject individual stars. EXAMINE will
accept as input the output photometry list from either ALLSTAR or NSTAR.
For each star in the input list the user can examine either a 3-d meshplot
or a contour diagram of both the input image and the star-subtracted image.
The results of the photometry for the star under consideration is also
displayed.

Two output star lists are produced using this task. One is a list
of stars which have been "accepted" by the user, the other being a list
of stars which have been "rejected".

If the TV option is selected then both the original image and subtracted 
image are displayed on the "stdimage" and the star under consideration is
identified. The user has the ability to blink these two frames to
evaluate the results of the photometry.

This task is controlled via input from the terminal with various keys 
performing a variety of functions.

.sh
3.7.1 EXAMINE Parameters
     There are several parameters which control various aspects of the
EXAMINE task. The parameters control such things as the input photometry
list, the type of graphical display desired and whether to use the
display capabilities.

.ks
.nf
Query mode parameters:

	phot_list		filename

	fwhm			real (pixels)
	threshold		real (in units of sigma)
	output_file		filename
.fi
.ke

.sh 
3.9 The GROUP Task

     GROUP is used to divide the stars in the image file into natural
groups prior to analysis with NSTAR. GROUP works on the following
principle: if two stars are close enough that the light of one will
influence the profile fit of the other, then they belong in the same
group.

.sh
3.9.1 GROUP Parameters

     GROUP only has a few parameters which govern its operation. These
are:

.ks
.nf
Query mode parameters:

	input_image		filename
	psf_file		filename
	crit_overlap		real
	output			filename
.fi
.ke

.ks
.nf
Hidden mode parameters:

	daopars		pset	"daophot$daopars.par"
.fi
.ke

These parameters perform the following functions:

.ls 4
.ls 16 input_image
The name of the input image file.
.le
.ls psf_file
The name of the file containing the PSF.
.le
.ls crit_overlap
The "critical overlap" before one star is determined to influence 
another. When GROUP examines two stars to see whether they might influence
each others' fits, it firts identifies the fainter of the two stars. It then
calculates the brightness of the brighter star at a distanceof one fitting
radius plus one pixel from the center of the fainter. If this brightness is
greater than the "critical overlap" times the random error per pixel, then
the brighter star is deemed to be capable of affecting the photometry of the 
fainter, and the two stars are grouped together.
.le
.ls output
The name of the SDAS table which will contain the stellar groups.
.le
.ls daopars
The name of of a pset file containing the daophot parameters. The specific
parameters which are used from this include the following:
.le
.le
.sh 
3.10 The GROWTH Task

.sh 
3.11 The OFFSET task

.sh 
3.12 The PEAK Task

     PEAK fits the PSF to a single star. It is useful for sparsely populated 
image files where the stars of interest are not blended. In this cases aperture
photometry is often fine and the use of PEAK is of limited interest. This task
is included in the DAOPHOT package mainly for completeness.

.sh
3.12.1 PEAK Parameters

     The parameters specific to the PEAK task are used for specifying the
input and output from this routine. The names of the parameters and their
functions are:

.ks
.nf
Positional or query parameters:

	input_image		filename
	psf_file		filename
	output			filename
.fi
.ke

.ks
.nf
Hidden parameters:

	daopars		pset	"daophot$daopars.par"
	verbose		boolean false
.fi
.ke

.ls 4
.ls 16 input_image
The name of the input image file.
.le
.ls psf_file
The name of the input file containing the point-spread function.
.le
.ls output
The name of the SDAS table to contain the output from PEAK.
.le
.ls verbose
If true then PEAK outputs more information about what it is doing.
.le
.ls daopars
The name of a pset file which contains the parameters specific to the
input image file. The parameters which PEAK uses from this pset include:
sthe fitting radius, the maximum and minimum good data value and whether
a variable PSF is to be used.
.le
.le
.sh 
3.13 The PSF Task

     The PSF task is used for obtaining the point-spread function which
will be used in the rest of the DAOPHOT reductions. DAOPHOT uses an
empirical point-spread function as opposed to a mathematically defined
function. The PSF is defined from the actual brightness distribution
of one or more stars in the frame under consideration. It is stored as
a two-component model: (1) an analytic Gaussian profile which approximately
matches the core of the point-spread function, and (2) a look-up table of
residuals, which are used as additive corrections to the integrated 
analytic Gaussian function.

The brightness in a hypothetical pixel at an arbitrary point within the
point-spread function is determined in the following manner. First
the bivariate Gaussian function is integrated over the area of the pixel,
and then a correction is determined by double cubic interpolation
within the lookup table, and is added to the integrated intensity.

The PSF is stored as a binary data file and is in a format specific
to DAOPHOT. The format of this file is very similar to that used by the
VMS version of DAOPHOT but is stored in binary for compactness.
A function is provided to take the PSF and convert it
to a IRAF image file so that it can be manipulated by other IRAF 
tasks. 

PSF allows the user to perform most functions from within the interactive
graphics part of its operation. PSF allows the user to modify the 
perspective of hist mesh plot, the contouring interval, the PSF radius 
etc. from within the PSF interactive graphics. 

.sh
3.13.1 PSF Parameters

     The PSF task has many parameters which specify the input and
output files as well as specifying other information. These are
divided in query mode parameters and hidden parameters.

.ks
.nf
Positional or query parameters:

	input_image		filename
	phot_list		filename
	psf_stars		filename
	psf_file		filename
.fi
.ke

.ks
.nf
Hidden parameters:

	daopars		pset	"daophot$daopars.par"
	verbose		boolean false
.fi
.ke

.ls 4
.ls 16 input_image
The name of the input image file.
.le
.ls phot_list
The name of the input file containing the aperture photometry
results for this image frame.
.le
.ls psf_stars
The name of file coordinate file containing the list of stars
to be used as PSF candidates.
.le
.ls psf_file
The name of the output file for storing the PSF.
.le
.ls verbose
If true then PEAK outputs more information about what it is doing.
.le
.ls daopars
The name of a pset file which contains the parameters specific to the
input image file. The parameters which PEAK uses from this pset include:
sthe fitting radius, the maximum and minimum good data value and whether
a variable PSF is to be used.
.le
.le
.sh 
3.14 The NSTAR Task

     NSTAR is one of DAOPHOT's multiple, simultaneous, profile-fitting
photometry routine. It is similar to ALLSTAR except that NSTAR must have
the objects grouped (using the GROUP task) and it does not dynamically
alter the groups while running. NSTAR also does not automatically produce the
star subtracted image file.

.sh
3.14.1 NSTAR Parameters

     There are several parameters which control the function of the 
NSTAR task. These are the following:

.ks
.nf
Positional or Query Parameters:

	input_image		filename
	psf_file		filename
	group_file		filename
	output_file		filename
.fi
.ke

.ks
.nf
Hidden parameters:

	daopars		pset	"daophot$daopars.par"
	verbose		boolean	false
.fi
.ke

.sh 
3.15 The SEEPSF Task

     The SEEPSF task produces an IRAF image file from the given PSF
file. This allows other IRAF tasks, especially display and plotting tasks,
to use access the point-spread function. The user has the ability to create
any size of image from the PSF enlargements being handled by a number of
different interpolation schemes.

.sh
3.15.1 SEEPSF Parameters
     
     The parameters wich control this task are limted. They basically
control the input, output and size of the image.

.ks
.nf
Positional or Query Parameters:

	psf_file		filename
	image_name		filename
	image_size		integer
.fi
.ke

.ks
.nf
Hidden parameters:

	interpolation	string  "nearest"
	boundary	string	"constant"
	constant	real	0.0
	daopars		pset	"daophot$daopars.par"
	verbose		boolean false
.fi
.ke

.ls 4
.ls 16 psf_file
This specifies the input PSF file which is to be transformed into an
IRAF image.
.le
.ls image_name
The name of the output IRAF image.
.le
.ls image_size
The size of the output image in pixels per side. Note that only square PSFs
and PSF images are alllowed.
.le
.ls interpolation
The type of interpolation to be used in expanding the image. The choices
are "nearest" neighbor, "linear" bilinear, "poly3" bicubic polynomial,
"poly5" biquintic polynomial, and "spline3" bicubic spline.
.le
.ls boundary
The type of boundary extension to use for handling references to pixels
outside the bounds of the input image. The choices are: "constant",
"nearest" edge, "reflect" about the boundary and "wrap" to the other side
of the image.
.le
.le
.sh 
3.16 The SELECT Task

     The SELECT task is used to select groups of stars with a particular
range of sizes from a group file which has been produced by GROUP. This 
task is used when some of the groups in the group file are large than the
maximum allowed in NSTAR, currently 60 stars. 

.sh
3.16.1 SELECT Parameters

     The parameters which control the SELECT task are the following:

.ks
.nf
Positional or Query Parameters:

	input_group		filename
    	output_group		filename
	min_group		integer
	max_group		integer
.fi
.ke

.le 4
.ls 16 input_group
The input group file which is to be searched for groups within the
limits specified by min_group and max_group.
.le
.ls output_group
The output group file which will consist of groups between 'min_group'
and 'max_group' in size.
.le
.ls min_group
The minimum group size to be extracted from the input group file.
.le
.ls max_group
The maximum group size to be extracted from the input group file.
.le
.le
.sh 
3.17 The SKY Task

.sh 
3.18 The SORT Task

.sh 
3.19 The SUBSTAR Task

     The SUBSTAR command takes the point-spread function for an image
frame and a file containing the x,y coordinates and apparent magnitudes
for a group of stars, usually an output file from one of the photometry
routines, shifts and scales the PSF function 
according to each position and magnitude, and then subtracts it from the 
original image frame. 

.sh
3.19.1 SUBSTAR Parameters

     The parameters for SUBSTAR control the input and output from this 
task.

.ks
.nf
Positional or Query Parameters:

	psf_file		filename
	phot_file		filename
	input_image		filename
	output_image		filename
.fi
.ke

.ks
.nf
Hidden parameters:

	verbose		boolean false
.fi
.ke

.ls 4
.ls 16 psf_file
The name of the file containing the PSF which is to be used as the template 
in the star subtraction.
.le
.ls phot_file
The file containing the photometry results for the stars which are to be
subtracted from the input image.
.le
.ls input_image
The name of the input image file from which the stars are to be subtracted.
.le
.ls output_image
The name of the output image file which will be a copy of the input frame
except for the subtracted stars.
.le
.ls verbose
If this parameter is set to true then more information about the progress
of SUBSTAR is output.
.le
.le
.sh 
4.0 Example

.endhelp
