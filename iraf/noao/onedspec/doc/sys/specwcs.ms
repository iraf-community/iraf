.EQ
delim $$
gsize 10
.EN
.nr PS 11
.nr VS 13
.de V1
.ft CW
.ps -2
.nf
..
.de V2
.fi
.ft R
.ps +2
..
.ND March 1993
.TL
The IRAF/NOAO Spectral World Coordinate Systems
.AU
Francisco Valdes
.AI
IRAF Group - Central Computer Services
.K2
.DY

.AB
The image formats and world coordinate systems for dispersion calibrated
spectra used in the IRAF/NOAO spectroscopy packages are described; in
particular, the image header keywords defining the coordinates are given.
These keywords appear both as part of the IRAF image structure and map
directly to FITS format.  The types of spectra include multidimensional
images with one or more spatial axes and a linear or log-linear dispersion
axis and special \fIequispec\fR and \fImultispec\fR formats having multiple
independent one dimensional spectra in a single multidimensional image.
The \fImultispec\fR format also includes general nonlinear dispersion
coordinate systems using polynomial, spline, sampled table, and look-up
table functions.
.AE

.NH
Types of Spectral Data
.LP
Spectra are stored as one, two, or three dimensional images with one axis
being the dispersion axis.  A pixel value is the flux over
some interval of wavelength and position.  The simplest example of a
spectrum is a one dimensional image which has pixel values as a
function of wavelength.
.LP
There are two types of higher dimensional spectral image formats.  One type
has spatial axes for the other dimensions and the dispersion axis may be
along any of the image axes.  Typically this type of format is used for
long slit (two dimensional) and Fabry-Perot (three dimensional) spectra.
This type of spectra is referred to as \fIspatial\fR spectra and the
world coordinate system (WCS) format is called \fIndspec\fR.
The details of the world coordinate systems are discussed later.
.LP
The second type of higher dimensional spectral image consists of multiple,
independent, one dimensional spectra stored in the higher dimensions with
the first image axis being the dispersion axis; i.e. each line is a
spectrum.  This format allows associating many spectra and related
parameters in a single data object.  This type of spectra is referred to
as \fImultispec\fR and the there are two coordinate system formats,
\fIequispec\fR and \fImultispec\fR.  The \fIequispec\fR format applies
to the common case where all spectra have the same linear dispersion
relation.  The \fImultispec\fR format applies to the general case of spectra
with differing dispersion relations or non-linear dispersion functions.
These multi-spectrum formats are important since maintaining large numbers
of spectra as individual one dimensional images is very unwieldy for the
user and inefficient for the software.
.LP
Examples of multispec spectral images are spectra extracted from a
multi-fiber or multi-aperture spectrograph or orders from an echelle
spectrum.  The second axis is some arbitrary indexing of the spectra,
called \fIapertures\fR, and the third dimension is used for
associated quantities.  The IRAF \fBapextract\fR package may produce
multiple spectra from a CCD image in successive image lines with an
optimally weighted spectrum, a simple aperture sum spectrum, a background
spectrum, and sigma spectrum as the associated quantities along the third
dimension of the image.
.LP
Many \fBonedspec\fR package tasks which are designed to operate on
individual one dimensional spectra may operate on spatial spectra by
summing a number of neighboring spectra across the dispersion axis.  This
eliminates the need to "extract" one dimensional spectra from the natural
format of this type of data in order to use tasks oriented towards the
display and analysis of one dimensional spectra.  The dispersion axis is
either given in the image header by the keyword DISPAXIS or the package
\fIdispaxis\fR parameter.  The summing factors across the
dispersion are specified by the \fInsum\fR package parameter.
.LP
One dimensional spectra, whether from multispec spatial spectra, have
several associated quantities which may appear in the image header as part
of the coordinate system description.  The primary identification of a
spectrum is an integer aperture number.  This number must be unique within
a single image.  There is also an integer beam number used for various
purposes such as discriminating object, sky, and arc spectra in
multi-fiber/multi-aperture data or to identifying the order number in
echelle data.  For spectra summed from spatial spectra the aperture number
is the central line, column, or band.  In 3D images the aperture index
wraps around the lowest non-dispersion axis.  Since most one dimensional
spectra are derived from an integration over one or more spatial axes, two
additional aperture parameters record the aperture limits.  These limits
refer to the original pixel limits along the spatial axis.  This
information is primarily for record keeping but in some cases it is used
for spatial interpolation during dispersion calibration.  These values are
set either by the \fBapextract\fR tasks or when summing neighboring vectors
in spatial spectra.
.LP
An important task to be aware of for manipulating spectra between image
formats is \fBscopy\fR.  This task allows selecting spectra from multispec
images and grouping them in various ways and also "extracts" apertures from
long slit and 3D spectra simply and without resort to the more general
\fBapextract\fR package.
.NH
World Coordinate Systems
.LP
IRAF images have three types of coordinate systems.  The pixel array
coordinates of an image or image section, i.e. the lines and
columns, are called the \fIlogical\fR coordinates.  The logical coordinates of
individual pixels change as sections of the image are used or extracted.
Pixel coordinates are tied to the data, i.e. are fixed to features
in the image, are called \fIphysical\fR coordinates.  Initially the logical
and physical coordinates are the equivalent but differ when image sections
or other tasks which modify the sampling of the pixels are applied.
.LP
The last type of coordinate system is called the \fIworld\fR coordinate
system.  Like the physical coordinates, the world coordinates are tied to
the features in the image and remain unchanged when sections of the image
are used or extracted.  If a world coordinate system is not defined for an
image, the physical coordinate system is considered to be the world
coordinate system.  In spectral images the world coordinate system includes
dispersion coordinates such as wavelengths.  In many tasks outside the
spectroscopy packages, for example the \fBplot\fR, \fBtv\fR and
\fBimages\fR packages, one may select the type of coordinate system to be
used.  To make plots and get coordinates in dispersion units for spectra
with these tasks one selects the "world" system.  The spectral tasks always
use world coordinates.
.LP
The coordinate systems are defined in the image headers using a set of
reserved keywords which are set, changed, and updated by various tasks.
Some of the keywords consist of simple single values following the FITS
convention.  Others, the WAT keywords, encode long strings of information,
one for each coordinate axis and one applying to all axes, into a set of
sequential keywords.  The values of these keywords must then be pasted
together to recover the string.  The long strings contain multiple pieces
called WCS \fIattributes\fR.  In general the WCS keywords should be left to
IRAF tasks to modify.  However, if one wants modify them directly some
tasks which may be used are \fBhedit\fR, \fBhfix\fR, \fBwcsedit\fR,
\fBwcsreset\fR, \fBspecshift\fR, \fBdopcor\fR, and \fBsapertures\fR.  The
first two are useful for the simple keywords, the two  "wcs" tasks are
useful for the linear ndspec and equispec formats, the next two are for the
common cases of shifting the coordinate zero point or applying a doppler
correction, and the last one is the one to use for the more complex
multispec format attributes.
.NH
Physical Coordinate System
.LP
The physical coordinate system is used by the spectral tasks when there is
no dispersion coordinate information (such as before dispersion
calibration), to map the physical dispersion axis to the logical dispersion
axis, and in the multispec world coordinate system dispersion functions
which are defined in terms of physical coordinates.
.LP
The transformation between logical and physical coordinates is defined by
the header keywords LTVi, LTMi_j (where i and j are axis numbers) through
the vector equation

.EQ I
    l vec~=~|m| cdot p vec + v vec
.EN

where $l vec$ is a logical coordinate vector, $p vec$ is a physical
coordinate vector, $v vec$ is the origin translation vector specified by
the LTV keywords and $|m|$ is the scale/rotation matrix
specified by the LTM keywords.  For spectra rotation terms (nondiagonal
matrix elements) generally do not make sense (in fact many tasks will not
work if there is a rotation) so the transformations along each axis are
given by the linear equation

.EQ I
    l sub i~=~LTMi_i cdot p sub i + LTVi.
.EN

If all the LTM/LTV keywords are missing they are assumed to have zero
values except that the diagonal matrix terms, LTMi_i, are assumed to be 1.
Note that if some of the keywords are present then a missing LTMi_i will
take the value zero which generally causes an arithmetic or matrix
inversion error in the IRAF tasks.
.LP
The dimensional mapping between logical and physical axes is given by the
keywords WCSDIM and WAXMAP01.  The WCSDIM keyword gives the dimensionality
of the physical and world coordinate system.  There must be coordinate
information for that many axes in the header (though some may be missing
and take their default values).  If the WCSDIM keyword is missing it is
assumed to be the same as the logical image dimensionality.
.LP
The syntax of the WAXMAP keyword are pairs of integer values,
one for each physical axis.  The first number of each pair indicates which
current \fIlogical\fR axis corresponds to the original \fIphysical\fR axis
(in order) or zero if that axis is missing.   When the first number is zero
the second number gives the offset to the element of the original axis
which is missing.  As an example consider a three dimensional image in
which the second plane is extracted (an IRAF image section of [*,2,*]).
The keyword would then appear as WAXMAP01 = '1 0 0 1 2 0'.  If this keyword
is missing the mapping is 1:1; i.e. the dimensionality and order of the
axes are the same.
.LP
The dimensional mapping is important because the dispersion axis for
the nspec spatial spectra as specified by the DISPAXIS keyword or task
parameter, or the axis definitions for the equispec and or multispec
formats are always in terms of the original physical axes.
.NH
Linear Spectral World Coordinate Systems
.LP
When there is a linear or logarithmic relation between pixels and
dispersion coordinates which is the same for all spectra the WCS header
format is simple and uses the FITS convention (with the CD matrix keywords
proposed by Hanisch and Wells 1992) for the logical pixel to world
coordinate transformation.  This format applies to one, two, and three
dimensional data.  The higher dimensional data may have either linear
spatial axes or the equispec format where each one dimensional spectrum
stored along the lines of the image has the same dispersion.
.LP
The FITS image header keywords describing the spectral world coordinates
are CTYPEi, CRPIXi, CRVALi, and CDi_j where i and j are axis numbers.  As
with the physical coordinate transformation the nondiagonal or rotation
terms are not expected in the spectral WCS and may cause problems if they
are not zero.  The CTYPEi keywords will have the value LINEAR to identify
the type of of coordinate system.  The transformation between dispersion
coordinate, $w sub i$, and logical pixel coordinate, $l sub i$, along axis i is given by

.EQ I
    w sub i~=~CRVALi + CDi_i cdot (l sub i - CRPIXi)
.EN

If the keywords are missing then the values are assumed to be zero except
for the diagonal elements of the scale/rotation matrix, the CDi_i, which
are assumed to be 1.  If only some of the keywords are present then any
missing CDi_i keywords will take the value 0 which will cause IRAF tasks to
fail with arithmetic or matrix inversion errors.  If the CTYPEi keyword is
missing it is assumed to be "LINEAR".
.LP
If the pixel sampling is logarithmic in the dispersion coordinate, as
required for radial velocity cross-correlations, the WCS coordinate values
are logarithmic and $w sub i$ (above) is the logarithm of the dispersion
coordinate.  The spectral tasks (though not other tasks) will recognize
this case and automatically apply the anti-log.  The two types of pixel
sampling are identified by the value of the keyword DC-FLAG.  A value of 0
defines a linear sampling of the dispersion and a value of 1 defines a
logarithmic sampling of the dispersion.  Thus, in all cases the spectral
tasks will display and analyze the spectra in the same dispersion units
regardless of the pixel sampling.
.LP
Other keywords which may be present are DISPAXIS for 2 and 3 dimensional
spatial spectra, and the WCS attributes "system", "wtype", "label", and
"units".  The system attribute will usually have the value "world" for
spatial spectra and "equispec" for equispec spectra.  The wtype attribute
will have the value "linear".  Currently the label will be either "Pixel"
or "Wavelength" and the units will be "Angstroms" for dispersion corrected
spectra.  In the future there will be more generality in the units
for dispersion calibrated spectra.
.LP
Figure 1 shows the WCS keywords for a two dimensional long slit spectrum.
The coordinate system is defined to be a generic "world" system and the
wtype attributes and CTYPE keywords define the axes to be linear.  The
other attributes define a label and unit for the second axis, which is the
dispersion axis as indicated by the DISPAXIS keyword.  The LTM/LTV keywords
in this example show that a subsection of the original image has been
extracted with a factor of 2 block averaging along the dispersion axis.
The dispersion coordinates are given in terms of the \fIlogical\fR pixel
coordinates by the FITS keywords as defined previously.

.DS
.ce
Figure 1: Long Slit Spectrum

.V1
WAT0_001= 'system=world'
WAT1_001= 'wtype=linear'
WAT2_001= 'wtype=linear label=Wavelength units=Angstroms'
WCSDIM  =                    2
DISPAXIS=                    2
DC-FLAG =                    0

CTYPE1  = 'LINEAR  '
LTV1    =                 -10.
LTM1_1  =                   1.
CRPIX1  =                  -9.
CRVAL1  =     19.5743865966797
CD1_1   =     1.01503419876099

CTYPE2  = 'LINEAR  '
LTV2    =                -49.5
LTM2_2  =                  0.5
CRPIX2  =                 -49.
CRVAL2  =       4204.462890625
CD2_2   =     12.3337936401367
.V2
.DE

Figure 2 shows the WCS keywords for a three dimensional image where each
line is an independent spectrum or associated data but where all spectra
have the same linear dispersion.  This type of coordinate system has the
system name "equispec".  The ancillary information about each aperture is
found in the APNUM keywords.  These give the aperture number, beam number,
and extraction limits.  In this example the LTM/LTV keywords have their
default values; i.e. the logical and physical coordinates are the same.

.DS
.ce
Figure 2: Equispec Spectrum

.V1
WAT0_001= 'system=equispec'
WAT1_001= 'wtype=linear label=Wavelength units=Angstroms'
WAT2_001= 'wtype=linear'
WAT3_001= 'wtype=linear'
WCSDIM  =                    3
DC-FLAG =                    0
APNUM1  = '41 3 7.37 13.48'
APNUM2  = '15 1 28.04 34.15'
APNUM3  = '33 2 43.20 49.32'

CTYPE1  = 'LINEAR  '
LTM1_1  =                   1.
CRPIX1  =                   1.
CRVAL1  =             4204.463
CD1_1   =     6.16689700000001

CTYPE2  = 'LINEAR  '
LTM2_2  =                   1.
CD2_2   =                   1.

CTYPE3  = 'LINEAR  '
LTM3_3  =                   1.
CD3_3   =                   1.
.V2
.DE
.NH
Multispec Spectral World Coordinate System
.LP
The \fImultispec\fR spectral world coordinate system applies only to one
dimensional spectra; i.e. there is no analog for the spatial type spectra.
It is used either when there are multiple 1D spectra with differing
dispersion functions in a single image or when the dispersion functions are
nonlinear.
.LP
The multispec coordinate system is always two dimensional though there may
be an independent third axis.  The two axes are coupled and they both have
axis type "multispec".  When the image is one dimensional the physical line
is given by the dimensional reduction keyword WAXMAP.  The second, line
axis, has world coordinates of aperture number.  The aperture numbers are
integer values and need not be in any particular order but do need to be
unique.  This aspect of the WCS is not of particular user interest but
applications use the inverse world to physical transformation to select a
spectrum line given a specified aperture.
.LP
The dispersion functions are specified by attribute strings with the
identifier \fIspecN\fR where N is the \fIphysical\fR image line.  The
attribute strings contain a series of numeric fields.  The fields are
indicated symbolically as follows.

.EQ I
         specN~=~ap~beam~dtype~w1~dw~nw~z~aplow~aphigh~[functions sub i ]
.EN

where there are zero or more functions having the following fields,

.EQ I
    function sub i~=~ wt sub i~w0 sub i~ftype sub i~[parameters]~[coefficients]
.EN

The first nine fields in the attribute are common to all the dispersion
functions.  The first field of the WCS attribute is the aperture number,
the second field is the beam number, and the third field is the dispersion
type with the same function as DC-FLAG in the \fInspec\fR and
\fIequispec\fR formats.  A value of -1 indicates the coordinates are not
dispersion coordinates (the spectrum is not dispersion calibrated), a value
of 0 indicates linear dispersion sampling, a value of 1 indicates
log-linear dispersion sampling, and a value of 2 indicates a nonlinear
dispersion.
.LP
The next two fields are the dispersion coordinate of the first
\fIphysical\fR pixel and the average dispersion interval per \fIphysical\fR
pixel.  For linear and log-linear dispersion types the dispersion
parameters are exact while for the nonlinear dispersion functions they are
approximate.  The next field is the number of valid pixels, hence it is
possible to have spectra with varying lengths in the same image.  In that
case the image is as big as the biggest spectrum and the number of pixels
selects the actual data in each image line.  The next (seventh) field is a
doppler factor.  This doppler factor is applied to all dispersion
coordinates by multiplying by $1/(1+z)$ (assuming wavelength dispersion
units).  Thus a value of 0 is no doppler correction.  The last two fields
are extraction aperture limits as discussed previously.
.LP
Following these fields are zero or more function descriptions.  For linear
or log-linear dispersion coordinate systems there are no function fields.
For the nonlinear dispersion systems the function fields specify a weight,
a zero point offset, the type of dispersion function, and the parameters
and coefficients describing it.  The function type codes, $ftype sub i$,
are 1 for a chebyshev polynomial, 2 for a legendre polynomial, 3 for a
cubic spline, 4 for a linear spline, 5 for a pixel coordinate array, and 6
for a sampled coordinate array.  The number of fields before the next
function and the number of functions are determined from the parameters of
the preceding function until the end of the attribute is reached.
.LP
The equation below shows how the final wavelength is computed based on
the $nfunc$ individual dispersion functions $W sub i (p)$.  Note that this
is completely general in that different function types may be combined.
However, in practice when multiple functions are used they are generally of
the same type and represent a calibration before and after the actual
object observation with the weights based on the relative time difference
between the calibration dispersion functions and the object observation.

.EQ I
w~=~sum from i=1 to nfunc {wt sub i cdot (w0 sub i + W sub i (p)) / (1 + z)}
.EN

The multispec coordinate systems define a transformation between physical
pixel, $p$, and world coordinates, $w$.  Generally there is an intermediate
coordinate system used.  The following equations define these coordinates.
The first one shows the transformation between logical, $l$, and physical,
$p$, coordinates based on the LTM/LTV keywords.  The polynomial functions
are defined in terms of a normalized coordinate, $n$, as shown in the
second equation.  The normalized coordinates run between -1 and 1 over the
range of physical coordinates, $p sub min$ and $p sub max$ which are
parameters of the function, upon which the coefficients were defined.  The
spline functions map the physical range into an index over the number of
evenly divided spline pieces, $npieces$, which is a parameter of the
function.  This mapping is shown in the third and fourth equations where
$s$ is the continuous spline coordinate and $j$ is the nearest integer less
than or equal to $s$.

.EQ I
    p mark~=~(l - LTV1) / LTM1_1
.EN
.EQ I
    n lineup~=~(p - p sub middle ) / (2 cdot p sub range )
.EN
.EQ I
      lineup~=~(p - (p sub max + p sub min )/2) / (2 cdot (p sub max - p sub min ))
.EN
.EQ I
    s lineup~=~(p - p sub min ) / (p sub max - p sub min ) cdot npieces
.EN
.EQ I
    j lineup~=~roman "int" (s)
.EN
.NH 2
Linear and Log Linear Dispersion Function
.LP
The linear and log-linear dispersion functions are described by a
wavelength at the first \fIphysical\fR pixel and a wavelength increment per
\fIphysical\fR pixel.  A doppler correction may also be applied.  The
equations below show the two forms.  Note that the coordinates returned are
always wavelength even though the pixel sampling and the dispersion
parameters may be log-linear.

.EQ I
    w mark~=~(w1 + dw cdot (p - 1)) / (1 + z)
.EN
.EQ I
    w lineup~=~10 sup {(w1 + dw cdot (p - 1)) / (1 + z)}
.EN

Figure 3 shows an example from a multispec image with
independent linear dispersion coordinates.  This is a linearized echelle
spectrum where each order (identified by the beam number) is stored as a
separate image line.

.DS
.ce
Figure 3: Echelle Spectrum with Linear Dispersion Function

.V1
WAT0_001= 'system=multispec'
WAT1_001= 'wtype=multispec label=Wavelength units=Angstroms'
WAT2_001= 'wtype=multispec spec1 = "1 113 0 4955.44287109375 0.05...
WAT2_002= '5 256 0. 23.22 31.27" spec2 = "2 112 0 4999.0810546875...
WAT2_003= '58854293 256 0. 46.09 58.44" spec3 = "3 111 0 5043.505...
WAT2_004= '928358078002 256 0. 69.28 77.89"
WCSDIM  =                    2

CTYPE1  = 'MULTISPE'
LTM1_1  =                   1.
CD1_1   =                   1.

CTYPE2  = 'MULTISPE'
LTM2_2  =                   1.
CD2_2   =                   1.
.V2
.DE
.NH 2
Chebyshev Polynomial Dispersion Function
.LP
The parameters for the chebyshev polynomial dispersion function are the
$order$ (number of coefficients) and the normalizing range of physical
coordinates, $p sub min$ and $p sub max$, over which the function is
defined and which are used to compute $n$.  Following the parameters are
the $order$ coefficients, $c sub i$.  The equation below shows how to
evaluate the function using an iterative definition where $x sub 1 = 1$,
$x sub 2 = n$, and $x sub i = 2 cdot n cdot x sub {i-1} - x sub {i-2}$.

.EQ I
    W~=~sum from i=1 to order {c sub i cdot x sub i}
.EN
.NH 2
Legendre Polynomial Dispersion Function
.LP
The parameters for the legendre polynomial dispersion function are the
order (number of coefficients) and the normalizing range of physical
coordinates, pmin and pmax, over which the function is defined
and which are used to compute n.  Following the parameters are the
order coefficients, c sub i.  The equation below shows how to evaluate the
function using an iterative definition where $x sub 1 = 1$, $x sub 2 = n$, and
$x sub i = ((2i-3) cdot n cdot x sub {i-1} - (i-2) cdot x sub {i-2}) / (i-1)$.

.EQ I
    W~=~sum from i=1 to order {c sub i cdot x sub i}
.EN
.LP
Figure 4 shows an example from a multispec image with independent nonlinear
dispersion coordinates.  This is again from an echelle spectrum.  Note that
the IRAF \fBechelle\fR package determines a two dimensional dispersion
function, in this case a bidimensional legendre polynomial, with the
independent variables being the order number and the extracted pixel
coordinate.  To assign and store this function in the image is simply a
matter of collapsing the two dimensional dispersion function by fixing the
order number and combining all the terms with the same order.

.DS
.ce
Figure 4: Echelle Spectrum with Legendre Polynomial Function

.V1
WAT0_001= 'system=multispec'
WAT1_001= 'wtype=multispec label=Wavelength units=Angstroms'
WAT2_001= 'wtype=multispec spec1 = "1 113 2 4955.442888635351 0.05...
WAT2_002= '83 256 0. 23.22 31.27 1. 0. 2 4 1. 256. 4963.0163112090...
WAT2_003= '976664 -0.3191636898579552 -0.8169352858733255" spec2 =...
WAT2_004= '9.081188912082 0.06387049476832223 256 0. 46.09 58.44 1...
WAT2_005= '56. 5007.401409453303 8.555959076467951 -0.176732458267...
WAT2_006= '09935064388" spec3 = "3 111 2 5043.505764869474 0.07097...
WAT2_007= '256 0. 69.28 77.89 1. 0. 2 4 1. 256. 5052.586239197408 ...
WAT2_008= '271 -0.03173489817897474 -7.190562320405975E-4"
WCSDIM  =                    2

CTYPE1  = 'MULTISPE'
LTM1_1  =                   1.
CD1_1   =                   1.

CTYPE2  = 'MULTISPE'
LTM2_2  =                   1.
CD2_2   =                   1.
.V2
.DE
.NH 2
Linear Spline Dispersion Function
.LP
The parameters for the linear spline dispersion function are the number of
spline pieces, $npieces$, and the range of physical coordinates, $p sub min$
and $p sub max$, over which the function is defined and which are used to
compute the spline coordinate $s$.  Following the parameters are the
$npieces+1$ coefficients, $c sub i$.  The two coefficients used in a linear
combination are selected based on the spline coordinate, where $a$ and $b$
are the fractions of the interval in the spline piece between the spline
knots, $a=(j+1)-s$, $b=s-j$, and $x sub 0 =a$, and $x sub 1 =b$.

.EQ I
    W~=~sum from i=0 to 1 {c sub (i+j) cdot x sub i}
.EN
.NH 2
Cubic Spline Dispersion Function
.LP
The parameters for the cubic spline dispersion function are the number of
spline pieces, $npieces$, and the range of physical coordinates, $p sub min$
and $p sub max$, over which the function is defined and which are used
to compute the spline coordinate $s$.  Following the parameters are the
$npieces+3$ coefficients, $c sub i$.  The four coefficients used are
selected based on the spline coordinate.  The fractions of the interval
between the integer spline knots are given by $a$ and $b$, $a=(j+1)-s$,
b=$s-j$, and $x sub 0 =a sup 3$, $x sub 1 =(1+3 cdot a cdot (1+a cdot b))$,
$x sub 2 =(1+3 cdot b cdot (1+a cdot b))$, and $x sub 3 =b sup 3$.

.EQ I
    W~=~sum from i=0 to 3 {c sub (i+j) cdot x sub i}
.EN
.NH 2
Pixel Array Dispersion Function
.LP
The parameters for the pixel array dispersion function consists of just the
number of coordinates $ncoords$.  Following this are the wavelengths at
integer physical pixel coordinates starting with 1.  To evaluate a
wavelength at some physical coordinate, not necessarily an integer, a
linear interpolation is used between the nearest integer physical coordinates
and the desired physical coordinate where $a$ and $b$ are the usual
fractional intervals $k= roman "int" (p)$, $a=(k+1)-p$, $b=p-k$,
and $x sub 0 =a$, and $x sub 1 =b$.

.EQ I
    W~=~sum from i=0 to 1 {c sub (i+j) cdot x sub i}
.EN
.NH 2
Sampled Array Dispersion Function
.LP
The parameters for the sampled array dispersion function consists of
the number of coordinate pairs, $ncoords$, and a dummy field.
Following these are the physical coordinate and wavelength pairs
which are in increasing order.  The nearest physical coordinates to the
desired physical coordinate are located and a linear interpolation
is computed between the two sample points.
