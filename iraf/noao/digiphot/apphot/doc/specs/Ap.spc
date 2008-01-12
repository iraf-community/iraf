.help apphot Aug83 "Digital Aperture Photometry Package"
.sh
1. Introduction

    The APPHOT package will provide a set of routines for performing
aperture photometry on uncrowded or moderately crowded fields, in either
interactive or batch mode.  The basic photometry technique employed
shall be fractional-pixel aperture integration; no PSF fitting techniques
shall be employed, and no knowledge of the PSF shall be required.
This document presents the formal requirements and specifications for
the package, and describes the algorithms to be used.

.sh
2. Requirements
.ls 4
.ls (1)
The program shall take as input an IRAF imagefile containing a starfield
which has been corrected for pixel to pixel gain variations, high frequency
fluctuations in the background, nonlinearity, and any other instrumental
defects affecting the intensity value of a pixel.
.le
.ls (2)
Given as input the approximate coordinates of a single object in the image,
the program shall perform the following operations:
.ls 4
.ls o
Determine a constant background value by analysis of an annular region
surrounding the object.  The background is assumed to be flat in the region
of the object, but may contain contaminating objects or defects which
shall be detected and eliminated by the fitting algorithm.
It shall be permissible for the background region to extend beyond the
boundaries of the image; the out of bounds region of the annulus shall
be excluded from the fit.
.le
.ls o
Determine the center of the object, taking the approximate object
coordinates given as input as a starting point.
The center determination shall be resistant to the affects of nearby
contaminating objects.  The centering algorithm may assume that the object
is circularly symmetric, or nearly so, and that the object flux is positive.
.le
.ls o
Determine the integral of object minus background within one or more
circular apertures centered upon the object.  The integration shall be
performed using partial pixel techniques, to minimize the effects of
sampling.  If the aperture contains any indefinite pixels, or if the
aperture extends beyond the boundary of the image, an indefinite result
shall be returned.
.le
.le
.le
.ls (3)
The following options shall be provided to modify the operation of the
above functions:
.ls
.ls o
Use a user supplied constant background value and background noise estimate
instead of fitting the background.
.le
.ls o
Use the starting center as the actual center in all cases.
.le
.ls o
Use the starting center as the actual center if the object is very faint,
but tweak up the center if the signal to noise is above a certain threshold.
.le
.ls o
Allow the object aperture to extend beyond the image boundary,
using only that portion of the aperture which is in bounds when computing
the aperture integral.
.le
.le
.le
.ls (4)
At a minimum, the following parameters shall be calculated and output
for each object:
.ls
.ls o
The coordinates of the object, and the estimated error in these coordinates.
.le
.ls o
The mode and standard deviation of the background; the number of pixels
left in the background region after pixel rejection.
.le
.ls o
The magnitude of the object, to within an arbitary zero-point, and
the statistical uncertainty of the magnitude.  If multiple concentric
apertures are used, a magnitude and uncertainty shall be given for each.
.le
.le
.le
.ls (5)
The program shall be usable both interactively and in batch mode.
In interactive use, the user shall be able to mark the positions of the
objects by interactively positioning a cursor on a 2-dim display device.
It shall be possible to enter the control parameters for the analysis
routines interactively for each object.  In batch mode, the control
parameters shall be fixed, and object coordinates shall be taken from
a user supplied list.  The display device shall not be required in
batch mode.
.le
.ls (6)
The APPHOT package shall be written in the SPP language in conformance
with the standards and conventions of IRAF.  The code shall be portable
and device independent.
.le
.le

.sh
2.1 Summary of the Limitations of APPHOT

    The APPHOT package is designed to perform simple aperture photometry
subject to the following restrictions:
.ls
.ls (1)
Objects must be circular or nearly circular, since the aperture is
circular.
.le
.ls (2)
All pixels within the object aperture are weighted equally.  All pixels
in the object aperture must be present; the object aperture may not normally
extend outside the image.  Defective pixels within the object
aperture may not be detected.
.le
.ls (3)
The background must be approximately flat in the neighborhood of the
object being measured.  The background must have a unique mode,
or the background fitting routine will reject the object.  Any low
frequency fluctuations in the background should be removed before
using APPHOT.
.le
.ls (4)
The object aperture must be kept small to minimize the degradation of
signal to noise caused by sky pixels within the aperture, and to
minimize the effects of crowding.  Therefore, the wings of the object
will extend beyond the aperture.  Good photometric results will be
obtained only if the aperture is consistently well centered, and if the
shape and diameter of an object is constant throughout the image and
invariant with respect to magnitude.
.le
.le

.sh
3. Specifications

    The APPHOT package performs aperture photometry on digitized starfields
maintained as IRAF imagefiles.  Input to the package consists of an imagefile,
a list of object coordinates, and numerous parameters controlling the analysis
algorithms.  Output consists of successive lines of text, where each line
summarizes the results of the analysis for a particular object.  The output
may be saved in a textfile, which may easily be printed or written onto a
card image tape for export.  The package routines may be used either
interactively or in batch mode.

The CL callable part of the APPHOT package consists of the following
routines:

.ks
.nf
	apphot	 -- the main aperture photometry routine.
	coordtr	 -- translations and rotations of coord lists.
	fitsky	 -- computes mode and sigma of a sky region.
	fitpsf	 -- compute the FWHM of the PSF.
	imcursor -- reads the image cursor; used to make lists.
	immark	 -- marks objects on the display device.
	radprof	 -- computes the radial profile of an object.
.fi
.ke

Routines for general list manipulation, reading and writing card image tapes,
reading and writing images to FITS tapes, removing the instrumental signature
from the data, and so on are available elsewhere in the IRAF system.
The package is easily extended to include peak finding,
matching of object lists from different images,
background fitting and removal, and so on.  The APPHOT package shall eventually
supplant both the KPNO AUTOPHOT and KPNO "Mountain Photometry Code" packages.

.sh
3.1 Standard Analysis Procedures

    Before performing aperture photometry one must determine the radius
of the object aperture to be used, the inner radius and size of the annulus
to be used to fit the background, the full width at half max (FWHM) of
the point spread function (PSF), and so on.
Additional parameters are required by the centering algorithm.
A list of object centers must be prepared if APPHOT is to be used in
batch mode.  The standard procedure is as follows:
.ls
.ls (1)
Use RADPROF to determine values for the following parameters:

.nf
    - the object aperture radius or radii, in pixels
    - the inner radius of the annular (sky) region
    - the width of the annular region
.fi
.le
.ls (2)
Use FITPSF to fit gaussians to isolated, high signal to noise data objects,
to determine the FWHM of the point spread function.
.le
.ls (3)
Use FITSKY to determine the sky sigma (standard deviation).
APPHOT assumes that sigma is approximately constant throughout the image.
.le
.ls (4)
If one does not wish to manually mark object positions with the cursor
during analysis, i.e. when the analysis is to be done in batch, a list
of object coordinates must be prepared.  This may be done in many ways:
.ls
.ls o
By running RCURSOR with the standard output redirected into the list file.
.le
.ls o
By transforming an existing list with COORDTR, OPSTRM, MATCH, SORT, WINDOW,
the editor, or some other filter.
.le
.ls o
By an automatic object finding procedure, if one is available.
.le
.ls o
By any other program which generates a list of object coordinates,
where each line of the list describes one object, and where x and y
in pixel coordinates are given in columns one and two.  Additional
columns, if present, are ignored.
.le
.ls o
APPHOT output may be used as coordinate input in a subsequent run.
.le
.le
.le
.ls (5)
Finally, APPHOT is run to measure the objects.  The user should be familiar
with the algorithms used to fit the background, measure the object center,
and compute the aperture integral, magnitude, and errors.  The values of
all visible and hidden APPHOT parameters should be inspected before doing
any serious processing.
.le
.le

The general purpose IRAF list processing tools may be used for further
analysis of APPHOT output.
Output lists may be filtered to select objects based on the value of a
list column, i.e., all the objects within a certain magnitude range may
be selected, objects with estimated errors larger than a certain value
may be deleted, or a list may be sorted using the value in any column.
Columns may be extracted from a list to form new lists or to provide input
to a plot filter, and lists may be merged.
Arithmetic may be performed on lists to calculate colors, etc.

The remainder of this section presents detailed specifications for the analysis
procedures in the APPHOT package.

.sh
3.2 The APPHOT Program

    The function of the APPHOT procedure is to perform aperture photometry
on isolated objects within an image.  The principal input operands are the 
name of the imagefile and the rough coordinates of the objects to be processed.
The principal output operands are the coordinates and magnitudes of the
objects.

In order to perform aperture photometry APPHOT must perform the following
sequence of operations (the algorithms employed are explained in
more detail later in this section):
.ls
.ls (1)
The mode and sigma of an annular background region centered on the object
is calculated.
.le
.ls (2)
The center of the object is determined.
.le
.ls (3)
The background is subracted from the object, and the total flux within
the object aperture or apertures is calculated.
.le
.le

Steps (1) and (2) above are optional; the background and center may be
determined externally, rather than by APPHOT, if desired.
.sh
3.2.1 APPHOT parameters

    APPHOT has quite a few parameters due to the complexity of the
algorithms employed.  All data dependent parameters are query mode to
ensure that they get set properly when a new image is processed.
The data independent algorithm control parameters are hidden mode,
and are given reasonable default values.  The names, datatypes, and
default values of the APPHOT parameters are shown below.


.ks
.nf
Positional or query mode parameters:

	image		filename
	apertures	string
	annulus		real
	width_annulus	real
	fwhm_psf	real
	sky_sigma	real
.fi
.ke


.ks
.nf
List structured parameters (filename may be given on command line):

	sky_mode	*real
	coords		*imcur
.fi
.ke


.ks
.nf
Hidden parameters:

	spool		boolean		no
	output		filename	"apphot.out"
	fitsky		boolean		yes
	max_sky_iter	integer		50
	growing_radius	real		1.0 (fwhm_psf units)
	k1		real		5.0
	k2		real		2.0
	center		boolean		yes
	clean		boolean		yes
	cleaning_radius	real		0.8 (fwhm_psf units)
	clipping_radius	real		1.5 (fwhm_psf units)
	max_cen_shift	real		1.0 (fwhm_psf units)
	min_snratio	real		0.5
	zmag		real		26.0
	verbose		boolean		yes
.fi
.ke


The function and format of each of these parameters is explained in more
detail below.

.ls
.ls 16 image
The name of the image or image section to be processed.
.le
.ls output
The name of the output textfile used to spool APPHOT output.
If null, output will not be spooled.  Note that output always appears on
the standard output, whether or not spooling is in effect.
.le
.ls apertures
The radii in pixels of the concentric object apertures, given all on the
same line, delimited by blanks.  At least one aperture must be given;
the maximum number of apertures is limited by the length of a line.
A sample input string might be "5.0 5.5 6.0 6.5 7.0".  If only a single
aperture is to be used, a real expression may be used instead of a string
type argument.  The apertures need not be given in any particular order.
The average radius will be used to compute the uncertainty in the
object magnitude.
.le
.ls annulus
The inner radius of the annular sky region, in pixels.
.le
.ls width_annulus
The width of the annular sky region, in pixels.
.le
.ls fwhm_psf
The FWHM of the psf, in pixels.  Used as a scale factor to control the
internal algorithms.
.le
.ls sky_sigma
The standard deviation (noise value) of a typical region of sky in
the image.  Used for pixel rejection in the sky fitting algorithm.
.le
.ls sky_mode
The name of a list file containing the mode of the background of each of
the objects to be processed.  Required only if FITSKY is switched off.
If sky fitting is disabled, and no list file is given, APPHOT will query
for the sky value.
.le
.ls coords
The name of a list file containing the coordinates of the objects to
be processed.  If absent, objects may be marked interactively with the
cursor.
.le
.ls fitsky
A switch used to specify whether or not the background will be fitted.
If background fitting is disabled, the mode and sigma of the background
will be read from the SKY_FILE list each time an object is processed.
.le
.ls max_sky_iter
The maximum number of iterations for the sky fitting algorithm.
Since the sky fitting algorithm is guaranteed to converge,
this parameter should normally have a large value.  If the value
is zero, the median of the sky region will be used instead of the mode.
.le
.ls growing_radius
The region growing radius for pixel rejection in the sky region, in units
of FWHM_PSF.  When a bad sky pixel is detected, all pixels within
(growing_radius * fwhm_psf) pixels of the bad pixel will be rejected.
Used to exclude the wings of contaminating objects from the sky sample,
to avoid biasing the mode.
.le
.ls k1
The k-sigma clipping factor for the first phase of the sky fitting algorithm.
.le
.ls k2
The k-sigma clipping factor for the second, iterative, phase of the
sky fitting algorithm.
.le
.ls center
A switch used to specify whether or not centering is to be performed.
If centering is disabled, the initial center will be used as the object center.
.le
.ls clean
A switch used to specify whether or not the symmetry-clean algorithm
is to be employed during centering.
.le
.ls cleaning_radius
The cleaning radius for the symmetry-clean algorithm, in units of
FWHM_PSF.
.le
.ls clipping_radius
The clipping radius for the symmetry-clean algorithm, in units of
FWHM_PSF.
.le
.ls max_cen_shift
The maximum permissible shift of center, in units of FWHM_PSF.
If the shift produced by the centering algorithm is larger than this value,
the fit will terminate and no magnitude will be calculated.
.le
.ls min_snratio
Centering will be skipped if the signal to noise of the object,
as calculated from the initial center, is less than the value given
by this parameter.
.le
.ls zmag
Zero point for the output magnitude scale.
.le
.ls verbose
If enabled, the output columns are labeled.  Note that the presence
of column labels in the output may interfere with the use of the list
processing tools.
.le
.le

.sh
3.2.2 The APPHOT Background Fitting Algorithm

    A good background fit is essential to aperture photometry.  Fitting
the background is trivial in a sparse field, but difficult in a crowded
field.  In general the background region will contain contaminating objects
which must be detected and excluded if a good fit is to be obtained.

The algorithm employed here is based on the fact that contaminated pixels
are almost always spatially correlated.  Background fitting algorithms
which work with a one dimensional sample (mode, median), or with the one
dimensional histogram (mode of hgm) have difficulty rejecting the faint
wings of contaminated regions.  This is a serious defect of one dimensional
fitting algorithms, because it is these faint wings, not the bright
central pixels, which are most likely to bias the calculated background value.

The algorithm used in APPHOT is as follows:


.ks
.nf
    algorithm fit_sky

    begin
	    # Reject gross deviants.
	    compute the median of the annular region
	    detect pixels more than (k1*sky_sigma) from the median
	    reject all such pixels, without region growing

	    # Detect and reject contaminating objects.
	    while (number of iterations <= max_sky_iter) {
		compute the histogram of the reduced sample
		compute the sigma and mode of the histogram
		detect pixels more than k2*sigma from the mode
		reject all such pixels, with region growing
		if (no pix rejected or all pix rejected)
		    terminate loop
	    }

	    return the final mode, sigma, and sample size
    end
.fi
.ke


The mode of the histogram is found by cross correlating the noise function
with the histogram.  The width of the the noise function is given by the
standard deviation of the current sample.  Pixel rejection is
performed by locating all pixels more than k2*sigma from the mode,
and blindly rejecting all pixels within a certain radius of each deviant
pixel.  This simple algorithm works well because the sample is large,
and therefore there is little penalty for discarding pixels that might
not be deviant.  Region growing also tends to accelerate convergence
significantly.

Very faint contaminating objects are difficult to detect and reject.
If there are enough such objects, they should not be rejected, because
there are probably a few in the object aperture as well.  A higher sky
sigma will be calculated and the computed uncertainty in the magnitude
will increase.  The best solution to this problem may be to increase
the size of the annulus to minimize the bias and maximize the liklihood
of a detection.

.sh
3.2.3 The APPHOT Centering Algorithm

    The centering algorithm used in APPHOT is that of Auer and Van Altena,
with the addition of the symmetry-clean algorithm developed by Barry Newell.
The main algorithm is as follows:


.ks
.nf
    algorithm get_center

    begin
	    if (centering is disabled) {
		return initial center, zero uncertainty estimate

	    } else if (signal to noise of object < MIN_SNRATIO) {
		compute uncertainty using initial center
		return initial center, computed uncertainty

	    } else {
		call measure_center
		return image center and estimated uncertainty
	    }
    end
.fi
.ke


The actual center determination is carried out by the following
algorithm:


.ks
.nf
    algorithm measure_center

    begin
	    extract subarray from the main data array

	    # Perform symmetry-cleaning.
	    if (cleaning is enabled) {
		for (each pair of pixels diametrically opposed about
		    the image center beyond the cleaning radius)
			if (i2 > i1 + 2*sky_sigma)
			    replace i2 by i1
			else if (i1 > i2 + 2*sky_sigma)
			    replace i1 by i2

		perform 2*sky_sigma noniterative clip of all pixels
		beyond the clipping radius, to remove any remaining
		radially symmetric structures
	    }

	    # Compute the image center and uncertainty.
	    compute x and y marginals of the cleaned subarray
	    fit a gaussian of width FWHM_PSF to each marginal
	    compute the centering error from the covariance matrix

	    return image center and estimated uncertainty
    end
.fi
.ke


The effect of the symmetry-clean algorithm is to edit the raster,
removing any contaminating objects in the vicinity of the primary object.
This simplifies the fitting algorithm and increases its reliability,
since it does not have to deal with multipeak marginal distributions.

A gaussian is fitted to the marginal distributions because it is expected
to yield a better center determination for undersampled data.  
An alternative is to empirically derive the marginal distributions of
the psf and fit these to each data object.  This is a better approach in
some cases, but in the case of undersampled data it is difficult to derive
the marginal distributions due to sampling effects, and fitting is difficult
due to interpolation error.  The use of a gaussian eliminates interpolation
error.  Eventually, both techniques should be made available.

.sh
3.2.4 The APPHOT Aperture Integration Algorithm

    The integral of the flux within a circular aperture is computed by
fractional pixel techniques.  Pixels are assumed to be square apertures
arranged in a rectangular grid.  The fraction of a pixel which lies within
the circular APPHOT aperture is computed by an approximation, and all
such contributions are summed to produce the total integral.

The simplicity of aperture photometry limits the amount of information
available for error analysis.  Using only the noise value for the background,
the estimated error in the aperture integral is given by

	flux_error = sky_sigma * sqrt (aperture_area)

where "sky_sigma" is either the sigma calculated by the background fitting
algorithm or the parameter SKY_SIGMA, depending on whether sky fitting
is enabled, and where "aperture_area" is the fractional pixel area of the
aperture.

It is possible, however, to produce a more useful error estimate if we
include some information about the psf.  For the purposes of an improved
error estimate, we assume that the PSF is a gaussian.  Given the object center,
the background, and the FWHM of the PSF, it is trivial to fit a two dimensional
gaussian to the object.  An estimate of the average noise value for the
pixels within the aperture may then be obtained by computing the standard
deviation of the residual formed by subtracting the fitted two-dimensional
gaussian from the data.  This value is used in place of SKY_SIGMA in the
above equation for an improved estimate of the actual flux error.

In the limit as the gaussian goes to zero, both uncertainty estimates tend
to the same value, as they should.  For bright objects, the uncertainty
produced by analysis of the residual will tend to be pessimistic, since
it is unlikely that the PSF can actually be modeled by a simple gaussian.
Nonetheless, a plot of uncertainty versus magnitude should reveal objects
which are blended, which contain bad pixels, and so on.  The accuracy of
the gaussian model will determine how reliably deviant objects can be
discriminated.

.sh
3.2.5 APPHOT Output

    For each object processed, APPHOT prints a single line of text on the
standard output.  If desired, APPHOT will simultaneously spool output into
a user specified text file.  Each output line will contain the following
information (excluding the commas):


.nf
    x,y,cenerr,shift, mode,sigma,nskypix, mag1,...,magn,magerr

where

    x,y		object coordinates in pixels
    cenerr	estimated uncertainty in the object center
    shift	shift of center from initial coordinates
    mode	mode of the background
    sigma	sigma of the background
    nskypix	number of sky pixels left after rejection
    mag1	magnitude within the first annulus
    magn	magnitude within the Nth annulus
    magerr	estimated mag. uncertainty at the average radius
.fi


Note that the estimated uncertainty in the magnitude is given only for
the average object aperture radius.  The uncertainty for the other
apertures can easily be calculated given SIGMA and the area of each
aperture.  The zero point for the magnitude scale is given by the
hidden parameter ZMAG.

Additional information could be calculated and output (such as the
moments of the object and the skew of the background), but in our
experience few people ever look at such information, and a more complex
output format would be required.  Probably the calculation of anything
but object centers, magnitudes, and errors should be left to other
programs.

.sh
3.3 The COORDTR Program

    The function of COORDTR is to effect a linear translation and/or
rotation of a coordinate list.  COORDTR is a filter; coordinate lines
are read from the standard input and written to the standard output.
COORDTR is concerned only with coordinate transformations, and knows
nothing about image boundaries.  A transformed coordinate may no longer
lie within an image.

	x y other_stuff

The format of a coordinate line is shown above.  COORDTR operates only
on the coordinate pair x,y.  Any additional information on the line is
passed on to the output without modification.

COORDTR is actually a general purpose list processing operator, and
belongs in a list processing package, rather than in the APPHOT package.
When a list processing package is developed, COORDTR will be moved to
that package.

A COORDTR transformation consists of a linear translation followed
by a rotation.  Either the translation, the rotation, or both may be
skipped.  The COORDTR parameters are summarized below.


.ks
.nf
positional arguments:

	xshift		real
	yshift		real
	xcenter		real
	ycenter		real
	theta		real
.fi
.ke


.ks
.nf
hidden parameters:

	translate	boolean		yes
	rotate		boolean		no
.fi
.ke


If more than two positional arguments are given, COORDTR knows
that both a translation and a rotation are desired.  Otherwise the
boolean parameters TRANSLATE and ROTATE are read to determine
what additional parameters are needed.  Thus a simple linear translation
of +2.5 pixels in X and -0.2 pixels in Y would be specified by the
command

	coordtr (2.5, -.2, < "input", > "output")

which transforms the list in file "input", writing the output into the 
new file "output".

If a rotation is desired, XCENTER, YCENTER, and THETA must be given.
The first two parameters specify the pixel coordinates of the point
about which the rotation is to be performed, while THETA specifies
the rotation angle in degrees.  Positive THETA produces a counterclockwise
rotation, if positive X is to the right and positive Y is up.

.sh
3.4 The FITSKY Program

    The function of the FITSKY program is to determine the mode and sigma
of the specified annular regions, printing the results (mode, sigma, and npix)
on the standard output.  FITSKY is similar in operation to APPHOT, except
that its function is to fit sky, not perform aperture photometry.  The
FITSKY parameters are the following:


.ks
.nf
Positional or query mode parameters:

	image		filename
	annulus		real
	width_annulus	real
	fwhm_psf	real
.fi
.ke


.ks
.nf
List structured parameters (filename may be given on command line):

	coords		*imcur
.fi
.ke


.ks
.nf
Hidden parameters:

	spool		boolean		no
	output		filename	"fitsky.out"
	max_sky_iter	integer		50
	growing_radius	real		1.0 (fwhm_psf units)
	k1		real		5.0
	k2		real		2.0
	verbose		boolean		yes
.fi
.ke


The names and functions of the FITSKY parameters are the same as those
for APPHOT.  Note that ANNULUS may be set to zero to measure the
background within a circular aperture.  The maximum number of iterations
may be set to zero to measure the median of the sky sample.
FITSKY output may be spooled into a file and used as input to APPHOT.

.sh
3.5 The FITPSF Program

    The function of the FITPSF program is to determine the FWHM of the
point spread function.  This is done by selecting an isolated, high
signal to noise object, computing the x and y marginal profiles,
and fitting a gaussian to each profile.  Output consists of the object
center, the error in the center, and the FWHM of the fitted gaussians.
Note that the sigma of a gaussian may be obtained by dividing the FWHM
by 2.354.

	x y err x_fwhm y_fwhm

The input parameters for the FITPSF program are shown below.


.ks
.nf
Positional parameters:

	image		filename
	aperture	real
	annulus		real
	width_annulus	real
	sky_mode	real
	coords		*imcur
.fi
.ke


.ks
.nf
Hidden parameters:

	fitsky		boolean		yes
	center		boolean		yes
	spool		boolean		no
	output		filename	"fitpsf.out"
	verbose		boolean		yes
.fi
.ke


If background fitting is disabled, the parameter SKY_MODE defines
the sky level.  The background fitting algorithm is a simple median
calculation without pixel rejection or iteration.
This should be sufficient, since FITPSF is expected to be used mainly
in uncrowded regions on high signal to noise objects.

Note that FITPSF is set up to process a list of input objects.
The list processing tools (i.e., AVERAGE) may be used to average the
results to produce the final FWHM of the PSF for the image.

.sh
3.6 The IMCURSOR Program

    The function of the IMCURSOR program is to read the STDIMAGE cursor,
writing the cursor coordinates on the standard output.  The cursor is read
until the EOF character is entered to terminate the loop.  The standard
output may be redirected into a file to generate a coordinate list.
IMCURSOR has no parameters.

.sh
3.7 The IMMARK Program

    The function of IMMARK is to draw marks on the diplay device.
IMMARK is useful for verifying coordinate lists.


.ks
.nf
parameters:

	mark_type	string
	mark_size	real
	coords		*imcur
.fi
.ke


Output is the current frame of the STDIMAGE device.  Mark types
include "circle", "box", "cross", "plus", and "diamond".  The size of
a mark is given in pixels.  The third parameter is a standard coordinate
list.  If no list is given, the image cursor will be read instead.

.sh
3.8 The RADPROF Program

    The function of the RADPROF program is to compute the radial profile
of an object.  The output of RADPROF consists of a sequence of lines of
text, each line defining the profile at a single radius.  Since RADPROF
may generate many lines of output for a single input object, it is set up
to process only a single input object.  A CL while loop may be written
to process multiple objects, if desired.


.ks
.nf
positional arguments:

	image		filename
	aperture	real
	step_size	real
	annulus		real
	width_annulus	real
	sky_mode	real
.fi
.ke


.ks
.nf
hidden parameters:

	fitsky		boolean		yes
	center		boolean		yes
	verbose		boolean		yes
.fi
.ke


The radial profile is calculated from the image center out to the radius
specified by the parameter APERTURE, in steps of STEP_SIZE pixels.
The remaining RADPROF parameters are similar to those of APPHOT and will not be
discussed in detail.  If background fitting is disabled, the parameter
SKY_MODE defines the sky level.  The background fitting algorithm is
a simple median calculation without pixel rejection or iteration.
This should be sufficient, since RADPROF is expected to be used mainly
in uncrowded regions on high signal to noise objects.
Centering is via gaussian fits to the marginal profiles, without cleaning.

RADPROF output lines contain the following fields:

	r, i(r), inorm(r), fraction(r)

.nf
where

	r		radius in pixels
	i(r)		raw intensity at r
	inorm(r)	normalized intensity at r (range 0-1)
	fraction(r)	fraction of total integral at r
.fi


RADPROF does not generate any plots.  If one wishes to plot the contents
of an output column, the column may be extracted with a list processing
filter and piped to a graphics task.

.sh
4.0 Example

    A brief example may help illustrate the use of the package.  Suppose
we want to process a few hundred stars on images "blue" and "red".  We
start by analyzing the blue image.

	ap> radprof blue,15,0.5,20,10

This gives us a radial profile printout for one of the "blue" stars.
We decide that an aperture radius of 2.5 pixels is about right.
The annulus will start at a radius of 10.0 pixels and extend to 20.0 pixels.
The next step is to determine the FWHM of the PSF:

	ap> fitpsf blue,3,10,20 | tee spoolfile

By default, the program will take coordinate input by reading the image
display cursor.  When the program is waiting for cursor input, it will
cause the display cursor to blink rapidly; normally the cursor does not
blink.  One has to be aware of this, because no other prompt is issued.
We postion the cursor on several stars, and tap the space bar to measure
each one.  When finished we type the EOF character (<ctrl/z> on our systems)
to terminate the loop.  The screen will now look like this (the output 
column labels are ommitted):

.nf
	ap> fitpsf blue,3,10,20 | tee spoolfile
	 283.12  157.40 0.035  2.887  2.751
	 546.08  213.45 0.023  2.833  2.902
	 318.32  354.73 0.064  2.791  2.824
.fi

Since we elected to use TEE to spool the output, rather than the SPOOL
parameter of FITPSF, we will not see the results until all stars have been
measured.  The next step is to average the results, to determine the
final FWHM (the FITPSF output could have been piped directly to GETCOLS
without using an intermediate spoolfile, if desired).

.nf
	ap> getcols spoolfile,"4-5" | getcols | average
	2.83133 0.0569725 6 
.fi

There are many ways this average could have been computed, of course;
this is only one example.  Next, to avoid having to write down the FWHM value,
we put it into the appropriate APPHOT parameter (note that the parameter
name is abbreviated).

	ap> apphot.fwhm = 2.831

Finally, we must determine a representative backround sigma value for
the image.  This is done by using FITSKY to measure several sky areas,
and averaging column two of the output, much as we did for FITPSF.  The
final value may be saved in "apphot.sky_sigma".

By this point we have determined all the necessary parameters, and it is
time to do some photometry.  The only APPHOT argument we are sure of is
the image name parameter, so that is all we include on the command line:

.nf
	ap> apphot blue
	aperture radii, pixels: 2.4 2.5 2.75 3.0
	inner radius of sky annulus: 10
	width of sky annulus (1 - ): 10
	full width at half max of psf (2.831):
	standard deviation of the image noise function (23.733):
.fi

After responding to the prompts shown above, APPHOT will ask for the
first pair of object coordinates, and the cursor blink prompt will again
be given.  Several objects may be measured to verify that all is working.

The last step is to prepare a list of objects to be processed.  The simplest
way to do this is to interactively mark the objects with the cursor.
Later, when we process the "red" image, the same coordinate list may again
be used, possibly after filtering with COORDTR.

	ap> imcursor > objlist

At this point, all of the APPHOT parameters have been set, we have
a list of objects to be processed, and we are ready to run APPHOT in
batch mode.  We decide to save the output in the file "blue.out".
To ensure that we have a record of the parameters used for the fit,
we first print the APPHOT parameters into the output file, then we
start up the APPHOT batch run.

.nf
	ap> lparam apphot > blue.out
	ap> apphot >> blue.out &
	[1]
.fi

The batch job is now running, appending output lines to the file "blue.out".
We can proceed to set up the job for the red image, in much the same way
that we set up the job for the blue image.  When both jobs finish, we
can use the list processing tools to filter out the good objects and
calculate colors.
