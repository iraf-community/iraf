.nr PS 9
.nr VS 11
.de LS
.RT
.if \\n(1T .sp \\n(PDu
.ne 1.1
.if !\\n(IP .nr IP +1
.if \\n(.$-1 .nr I\\n(IR \\$2n
.in +\\n(I\\n(IRu
.ta \\n(I\\n(IRu
.if \\n(.$ \{\
.ds HT \&\\$1
.ti -\\n(I\\n(IRu
\\*(HT
.br
..
.ND
.TL
ONEDSPEC/IMRED Package Revisions Summary: IRAF Version 2.10
.AU
Francisco Valdes
.AI
IRAF Group - Central Computer Services
.K2
P.O. Box 26732, Tucson, Arizona 85726
May 1992
.NH
Introduction
.LP
The IRAF NOAO spectroscopy software, except for the \fBlongslit\fR
package, has undergone major revisions.  The revisions to the aperture
extraction package, \fBapextract\fR, are described in a separate
document.  This paper addresses the revisions in the \fBonedspec\fR
package and the spectroscopic image reduction packages in the
\fBimred\fR package.  In addition to the revisions summary given here
there is a new help topic covering general aspects of the new
\fBonedspec\fR package such as image formats, coordinate systems, and
units.  This help topic is referenced under the name
"onedspec.package".
.LP
There are a large number of revisions both minor and major.  To avoid
obscuring the basic themes and the major revisions in a wealth of minor
detail, this document is organized into sections of increasing detail.  The
most important aspects of the revisions are described in a major highlight
section followed by a minor highlight section.  Then a reorganization chart
for the \fBonedspec\fR package is presented showing where various
tasks have been moved, which have been deleted, and which are new.
Finally, a summary of the revisions to each task is presented.
.LP
I hope that the many new capabilities, particularly as presented in the
highlight section, will outweigh any disruption in accomodating to so
many changes.
.NH
Major Highlights
.LP
The major highlights of the revisions to the NOAO spectroscopy software
are listed and then discussed below.

.DS
\(bu Non-linear dispersion calibration
\(bu Integration of dispersion coordinates with the core system 
\(bu Sinc interpolation
\(bu Plotting in user selected units including velocity
\(bu Integration of long slit spectra and 1D formats
\(bu New \fBimred\fR packages featuring streamlined reductions
.DE

Possibly the most significant revision is the generalization allowing
non-linear dispersion calibration.  What this means is that spectra do
not need to be interpolated to a uniform sampling in wavelength or
logarithmic wavelength.  The dispersion functions determined from
calibration arc lines by \fBidentify\fR, \fBreidentify\fR,
\fBecidentify\fR, or \fBecreidentify\fR can be simply assigned to the
spectra and used throughout the package.  It is also possible to assign
a dispersion table or vector giving the wavelengths at some or all of
the pixels.  Note, however, that it is still perfectly acceptible to
resample spectra to a uniform linear or log-linear dispersion as was
done previously.
.LP
For data which does not require geometric corrections, combining, or
separate sky subtraction the observed sampling need never be changed
from the original detector sampling, thus avoiding any concerns over
interpolation errors.  In other cases it is possible to just
interpolate one spectrum, say a sky spectrum, to the dispersion of
another spectrum, say an object spectrum, before operating on the two
spectra.  There are several new tasks that perform interpolations to a
common dispersion, not necessarily linear, when operating on more than
one spectrum.  In particular, the new task \fBsarith\fR and the older
task \fBsplot\fR now do arithmetic on spectra in wavelength space.
Thus, one no longer need be concerned about having all spectra
interpolated to the same sampling before doing arithmetic operations as
was the case  previously.
.LP
The trade-off in using non-linear dispersion functions is a more complex
image header structure.  This will make it difficult to import to non-IRAF
software or to pre-V2.10 IRAF systems.  However, one may resample to a
linear coordinate system in those cases before transfering the spectra as
FITS images having standard linear coordinate keywords.
.LP
On the subject of interpolation, another important addition is the
implementation of sinc interpolation.  This is generally considered
the best interpolation method for spectra, however, it must be used
with care as described below.
Sinc interpolation approximates applying a phase shift to the fourier
transform of the spectrum.  Thus, repeated interpolations do not accumulate
errors (or nearly so) and, in particular, a forward and reverse
interpolation will recover the original spectrum much more closely than
other interpolation methods.  However, for undersampled (where the fourier
transform is no longer completely represented), strong features, such as
cosmic rays or narrow emission or absorption lines, the ringing can be much
more severe than the polynomial interpolations.  The ringing is especially
a concern because it extends a long way from the feature causing the
ringing; 30 pixels with the truncated algorithm that has been added.  Note
that it is not the truncation of the interpolation function which is at
fault but the undersampling of the narrow features!
.LP
Because of the problems seen with sinc interpolation it should be used with
care.  Specifically, if there are no undersampled, narrow features it is a
good choice but when there are such features the contamination of the
spectrum by ringing is more severe, corrupting more of the spectrum,
than with other interpolation types.
.LP
The dispersion coordinates are now interfaced through the IRAF WCS
(world coordinate system) interface.  This is important to users for
two reasons.  First, operations performed on spectral images by IRAF
core system tasks and the IRAF image I/O system will have access to the
dispersion coordinates and will properly modify them as necessary.  The
most common such operation is applying an image section to a spectrum
either during an image copy or as input to another task.  In this case
the relation between the pixels in the image section and their
wavelengths is preserved.  For example one may \fBsplot\fR a section of
a large spectrum and get the correct wavelengths.  The second reason is
to allow use of proper dispersion coordinates in such IRAF tasks as
\fBlistpixels\fR, \fBimplot\fR, and \fBgraph\fR.
.LP
The new package supports a variety of spectral image formats.  The
older formats are understood when reading them.  In particular the one
dimensional "onedspec" and the two dimensional "multispec" format will
still be acceptable as input.  Note that the image naming syntax for
the "onedspec" format using record number extensions is a separate
issue and is still provided but only in the \fBimred.iids\fR and
\fBimred.irs\fR packages.  Any new spectra created are either a one
dimensional format using relatively simple keywords and a two or three
dimensional format which treats each line of the image as a separate
spectrum and uses a more complex world coordinate system and keywords.
For the sake of discussion the two formats are still called "onedspec"
and "multispec" though they are not equivalent to the earlier formats.
.LP
In addition, the one dimensional spectral tasks may also now operate on
two dimensional images directly.  This is done by using the DISPAXIS
keyword in the image header or a package dispaxis parameter if the
keyword is missing to define the dispersion axis.  In addition there is
a summing parameter in the packages to allow summing a number of lines
or columns.  If the spectra are wavelength calibrated long slit
spectra, the product of the \fBlongslit.transform\fR task, the
wavelength information will also be properly handled.  Thus, one may
use \fBsplot\fR or \fBspecplot\fR for plotting such data without having
to extract them to another format.  If one wants to extract one
dimensional spectra by summing columns or lines, as opposed to using
the more complex \fBapextract\fR package, then one can simply use
\fBscopy\fR (this effectively replaces \fBproto.toonedspec\fR).
.LP
The tasks \fBsplot\fR and \fBspecplot\fR allow use of and changes
between various dispersion units.  Spectra may be plotted in units all
the way from Hertz to Mev.  The units may also be inverted to plot in
wavenumbers, such as inverse centimeters, and the decimal log may be
applied, to plot something like log wavelength or log frequency.  One
special "unit" which is available is a velocity computed about a
specified wavelength/frequency.  The multiple unit capability was one
of the last major changes made before the V2.10 release so the complete
generalization to arbitrary units has not been completed.  Dispersion
calibration and image world coordinate system generally must still be
done in Angstroms, particularly if flux calibration is to be done.  The
generalization to other units throughout the package is planned for a
later release.
.LP
The last of the changes catagorized as a major highlight is the
addition of a number of special packages for generic or specific
types of instruments and data in the \fBimred\fR package.  Most of these
package include a highly streamlined reduction task that combines
all of the reduction operations into a single task.  For example,
the \fBspectred.doslit\fR task can extract object, standard star, and
arc spectra from long slit images, apply a consistent dispersion
function based on only a single interactively performed dispersion
solution, compute a sensitivity function and end up with flux
calibrated spectra.  Another example, is \fBhydra.dohydra\fR for
extracting, flatfielding, dispersion calibrating, and sky subtracting
spectra from the NOAO Hydra multifiber spectrograph.  There are user's
guides for each of these new reduction tasks.
.NH
Minor Highlights
.LP
There are some further highlights which are also quite important
but which are secondary to the previous highlights.  These are listed
and discussed below.

.DS
\(bu Greater use of package parameters
\(bu An observatory database
\(bu A more flexible \fBidentify/reidentify\fR
\(bu Only one \fBdispcor\fR
\(bu Spatial interpolation of dispersion solutions
\(bu Deblending of arbitrary number of gaussian components
\(bu Manipulating spectral formats
\(bu Improved fitting of the continuum and related features
\(bu Various new tasks
.DE

There is an even greater use of package parameters than in the previous
release.  Package parameters are those which are common to many of the
the tasks in the package and which one usually wants to change in
one place.  The new package parameters are the default observatory for
the data if the observatory is not identified in the image header
(discussed further below), the interpolation type used
when spectra need to be resampled either for dispersion calibration
or when operating on pairs of spectra with different wavelength
calibration, and the default dispersion axis and summing parameters
for long slit and general 2D images (as discussed in the last section).
You will find these parameters not only in the \fBonedspec\fR package but in
all the spectroscopic packages in the \fBimred\fR package.
.LP
A number of spectroscopic tasks require information about the location
of the observation.  Typically this is the observatory latitude for
computing air masses if not defined in the header.  Radial velocity
tasks, and possible future tasks, may require additional information
such as longitude and altitude.  The difficulty is that if such
parameters are specified in parameter files the default may well be
inappropriate and even if the users set then once, they may forget to
update them in later reductions of data from a different observatory.
In other words this approach is prone to error.
.LP
To address this concern observatory parameters are now obtained from an
observatory database keyed by an observatory identifier.  If the image data
contains an observatory keyword, OBSERVAT, the tasks will look up the
required parameters from the observatory database.  Thus, if the images
contain the observatory identifier, as does data from the NOAO
observatories, they will always be correctly reduced regardless of the
setting of any parameters.  Of course one has to deal with data from
observatories which may not include the observatory identifier and may not
have an entry in the observatory database.  There are provisions for sites
and individual users to define local database files and to set the default
observatory parameters.  This is all discussed in the help for the
\fBobservatory\fR task.
.LP
The dispersion function fitting tasks \fBidentify\fR and
\fBreidentify\fR have been improved in a number of important ways.
These tasks now treat the input images as units.  So for long slit and
multispectrum images one can move about the image with a few
keystrokes, transfer solutions, and so on.  When transfering solutions
between a multispectrum reference image and another multispectrum image
with the same apertures using \fBreidentify\fR, the features and
dispersion solutions are transfered aperture by aperture.  This avoids
problems encountered by having to trace successively between apertures
and having the apertures be in the same order.
.LP
On the subject of tracing in \fBreidentify\fR, in some cases it is
desirable to use the same reference spectrum with all other sampled
lines or columns in a long slit spectrum or apertures in a
multispectrum image rather than propagating solutions across the
image.  The latter method is necessary if there is a continuous and
progress shift in the features.  But if this is not the situation then
the loss of features when tracing can be a problem.  In this case the
alternative of reidentifying against the same starting reference is now
possible and there will not be the problem of an increasing loss of
features.  On the other hand, the problem of lost features, whether
tracing or not, can also be addressed using another new feature of
\fBreidentify\fR, the ability to add features from a line list.  For
both tracing and nontracing reidentifications, another useful new
feature is automatic iterative rejection of poorly fitting lines in
determining a new dispersion function noninteractively.
.LP
The nontracing reidentifications, the automatic addition of new lines, and
the iterative rejection of poorly fitting lines in determining a new
dispersion function are all responses to make the reidentification process
work better without intervention.  However, as a last resort there is also
a new interactive feature of \fBreidentify\fR.  By monitoring the log output of
the reidentification process one can have a query be made after the
automatic reidentification and function fitting to allow selectively
entering the interactive feature identification and dispersion function
fitting based on the logged output.  Thus if a fit has a particularly large
RMS or a large number of features are not found one can chose to intervene
in the reidentification process.
.LP
Dispersion calibration is now done exclusively by the task
\fBdispcor\fR regardless of the spectrum format or dispersion solution
type; i.e. solutions from \fBidentify\fR or \fBecidentify\fR.  In addition to
allowing assignment of non-linear dispersion functions, as described
earlier, \fBdispcor\fR has other new features.  One is that, in
addition to interpolating dispersion solutions between two calibration
images (usually weighted by time), it is now possible to interpolate
zero point shifts spatially when multiple spectra taken simultaneously
include arc spectra.  This is mostly intended for the new generation of
multifiber spectrographs which include some fibers assigned to an arc
lamp source.  However, it can be used for the classic photographic case
of calibration spectra on the same plate.
.LP
The limitation to four lines on the number of gaussian components which
can be deblended by the deblending option in \fBsplot\fR has been removed.
A new feature is that line positions may be input from a line list as 
well as the original cursor marking or terminal input.
In addition an option to simultaneously determine a linear background
has been added.  As a spinoff of the deblending option a new, noninteractive
task, called FITPROFS, has been added.  This task takes a list of initial
line positions and sigmas and simultaneously fits gaussians with a
linear background.  One can constrain various combination of parameters
and output various parameters of the fitting.  While it can be used to
fit an entire spectrum it becomes prohibitively slow beyond a number like
30.  A banded matrix approach is required in that case.
.LP
As mentioned earlier there is a new task called \fBscopy\fR for manipulating
spectra.  It allows changing between various formats such as producing
the separate, simple keyword structure, one dimensional images from multispec
format images, combining multiple one dimensional spectra into the
more compact multispec format, and extracting line or column averaged one
dimensional spectra from two dimensional images.  It can also be
used to select any subset of apertures from a multispec format,
merge multiple multispec format spectra, and extract regions of spectra
by wavelength.
.LP
The \fBcontinuum\fR task has been revised to allow independent
continuum fits for each aperture, order, line, or column in images
containing multiple spectra.  Instead of being based on the
\fBimages.fit1d\fR task it is based on the new task \fBsfit\fR.
\fBSfit\fR allows fitting the \fBicfit\fR functions to spectra and
outputing the results in several ways such as the ratio (continuum
normalization), difference (continuum subtraction), and the actual
function fit.  In addition it allows outputing the input data with
points found to be deviant by the iterative rejection algorithm of
\fBicfit\fR replaced by the fitted value.  This is similar to
\fBimages.lineclean\fR.  In all cases, this is may be done
independently and interactively or noninteractively when there are
multiple spectra in an image.
.LP
A number of useful new tasks have already been mentioned:
\fBfitprofs\fR, \fBsarith\fR, \fBscombine\fR, \fBscopy\fR, and
\fBsfit\fR.  There are two more new tasks of interest.  The task \fBdopcor\fR
applies doppler shifts to spectra.  It applies the shift purely to the
dispersion coordinates by adding a redshift factor which is applied by
the coordinate system interface.  This eliminates reinterpolation and
preserves both the shift applied and the original observed dispersion
function (either linear or nonlinear).  The task can also modify the
pixel values for various relativistic and geometric factors.  This task
is primarily useful for shifting spectra at high redshifts to the local
rest frame.  The second new task is called \fBderedden\fR.  It applies
corrections for interstellar reddening given some measure of the
extinction along the line of site.
.NH
ONEDSPEC Package Task Reorganization
.LP
The \fBonedspec\fR package dates back to the earliest versions of IRAF.  Some of
its heritage is tied to the reduction of IRS and IIDS spectra.  One of
the revisions made for this release has been to reorganize the various
tasks and packages.  A few tasks have been obsoleted by new tasks or
the functionality of the new dispersion coordinate system, a number
of new tasks have been added, and a number of IRS and IIDS specific
tasks have been moved to the \fBimred\fR packages for those instruments.
While these packages are organized for those particular instruments they may
also be used by data having similar characteristics of beam switching,
coincidence corrections, and the requirement of sequential numeric
extensions.
.LP
The table below provides the road map to the reorganization showing
tasks which have disappeared, been moved, been replaced, or are new.

.DS
.TS
center;
r l l l r l l.
V2.9	V2.10	ALTERNATIVE	V2.9	V2.10	ALTERNATIVE

addsets		irs/iids	process		irs/iids
batchred		irs/iids	rebin		scopy/dispcor
bplot	bplot		refspectra	refspectra
bswitch		irs/iids	reidentify	reidentify
calibrate	calibrate		sapertures
coincor		iids	sarith
combine		scombine	scombine
continuum	continuum		scopy
	deredden		sensfunc	sensfunc
dispcor	dispcor		setdisp		hedit
	dopcor		sextract	scopy
	fitprofs		sfit
flatdiv		irs/iids	sflip		scopy/imcopy [-*,*]
flatfit		irs/iids	shedit		hedit
identify	identify		sinterp	sinterp
lcalib	lcalib		slist	slist
mkspec	mkspec		specplot	specplot
names	names		splot	splot
	ndprep		standard	standard
observatory		noao	subsets		irs/iids
powercor		iids	sums		irs/iids
.TE
.DE
.NH
IMRED Packages
.LP
Many of the \fBonedspec\fR tasks from the previous release have been
moved to the \fBiids\fR and \fBirs\fR packages, as indicated above,
since they were applicable only to these and similar instruments.
.LP
A number of new specialized spectroscopic instrument reduction packages
have been added to the \fBimred\fR package.  Many of these have been in
use in somewhat earlier forms in the IRAF external package called
\fBnewimred\fR.  In addition the other spectroscopic package have been
updated based on the revisions to the \fBonedspec\fR and
\fBapextract\fR packages.  Below is a table showing the changes between
the two version and describing the purpose of the spectroscopic
packages.  Note that while many of these package are named for and
specialized for various NOAO instruments these packages may be applied
fairly straightforwardly to similar instruments from other
observatories.  In addition the same tools for multifiber and slit
spectra are collected in a generic package called \fBspecred\fR.

.DS
.TS
center;
r l l s
r l l l.
V2.9	V2.10	SPECTROSCOPY PACKAGE
	argus	Fiber:	CTIO Argus Reductions
specphot	ctioslit	Slit:	CTIO Slit Instruments
echelle	echelle	Fiber Slit:	Generic Echelle
	hydra	Fiber:	KPNO Hydra (and Nessie) Reductions
iids	iids	Scanner:	KPNO IIDS Reductions
irs	irs	Scanner:	KPNO IRS Reductions
coude	kpnocoude	Fiber/Slit:	KPNO Coude (High Res.) Reductions
	kpnoslit	Slit:	KPNO Slit Instruments
msred	specred	Fiber/Slit:	Generic fiber and slit reductions
observatory	-> noao
setairmass
.TE
.DE
.LP
An important feature of most of the spectroscopic packages are specialized
routines for combining and streamlining the different reduction operations
for a particular instrument or type of instrument.  These tasks are:

.DS
.TS
center;
r r r.
argus.doargus	ctioslit.doslit	echelle.doecslit
echelle.dofoe	hydra.dohydra	iids.batchred
irs.batchred	kpnocoude.do3fiber	kpnocoude.doslit
kpnoslit.doslit	specred.dofibers	specred.doslit
.TE
.DE
.NH
ONEDSPEC Task Revisions in V2.10
.LS ADDSETS 2
Moved to the \fBiids/irs\fR packages.
.LS BATCHRED
Moved to the \fBiids/irs\fR packages.
.LS BPLOT
The APERTURES and BAND parameters been added to select 
apertures from multiple spectra and long slit images, and bands
from 3D images. Since the task is a script calling \fBsplot\fR, the
many revisions to that task also apply. The version in the
\fBiids/irs\fR packages selects spectra using the record number 
extension syntax.
.LS BSWITCH
Moved to the \fBiids/irs\fR packages.
.LS CALIBRATE
This task was revised to operate on nonlinear dispersion 
corrected spectra and 3D images (the \fBapextract\fR "extras"). The
aperture selection parameter was eliminated (since the header
structure does not allow mixing calibrated and uncalibrated
spectra) and the latitude parameter was replaced by  the 
observatory parameter. The observatory mechanism insures that
if the observatory latitude is needed for computing an airmass
and the observatory is specified in the image header the
correct calibration will be applied. The record format syntax
is available in the \fBiids/irs\fR packages. The output spectra are
coerced to have real pixel datatype.
.LS COINCOR
Moved to the \fBiids\fR package.
.LS COMBINE
Replaced by \fBscombine\fR.
.LS CONTINUUM
This task was changed from a script based on \fBimages.fit1d\fR to a
script based on \fBsfit\fR. This provides for individual independent
continuum fitting in multiple spectra images and for additional
flexibility and record keeping.  The parameters have been 
largely changed.
.LS DEREDDEN
This task is new.
.LS DISPCOR
This is a new version with many differences. It replaces the
previous three tasks \fBdispcor\fR, \fBecdispcor\fR and \fBmsdispcor\fR. It
applies both one dimensional and echelle dispersion functions.
The new parameter LINEARIZE selects whether to interpolate the
spectra to a uniform linear dispersion (the only  option 
available previously) or to assign a nonlinear dispersion 
function to the image  without  any  interpolation.  The 
interpolation function parameter has been eliminated and the
package parameter INTERP is used to select the interpolation
function. The new interpolation type "sinc" may be used but
care should be exercised. The new task supports applying a
secondary zero point shift spectrum to a master dispersion
function and a spatial interpolation of the shifts  when 
calibration spectra are taken at the same time on a different
region of the same 2D image. The optional wavelength table may
now also be an image to match dispersion parameters. The
APERTURES and REBIN parameters have been eliminated.  If an
input spectrum has been previously dispersion corrected it will
be resampled as desired. Verbose and log file parameters have
been added to log the dispersion operations as desired. The
record format syntax is available in the \fBiids/irs\fR packages.
.LS DOPCOR
This task is new.
.LS FITPROFS
This task is new.
.LS FLATDIV
Moved to the \fBiids/irs\fR packages.
.LS FLATFIT
Moved to the \fBiids/irs\fR packages.
.LS IDENTIFY
The principle revision is to allow multiple aperture images and
long slit spectra to be treated as a unit. New keystrokes
allow jumping or scrolling within multiple spectra in a single
image. For  aperture  spectra  the database entries are 
referenced by image name and aperture number and not with image
sections. Thus, \fBidentify\fR solutions are not tied to specific
image lines in this case. There is a new autowrite parameter
which may be set to eliminate the save to database query upon
exiting. The new colon command "add" may be used to add
features based on some other spectrum or arc type and then
apply the fit to the combined set of features.
.LS LCALIB
This task has a more compact listing for the "stars" option and
allows paging a list of stars when the star name query is not
recognized.
.LS MKSPEC
This task is unchanged.
.LS NAMES
This task is unchanged.
.LS NDPREP
This task was moved from the \fBproto\fR package. It was originally
written at CTIO for CTIO data. It's functionality is largely
unchanged though it has been updated for changes in the 
\fBonedspec\fR package.
.LS OBSERVATORY
New version of this task moved to \fBnoao\fR root package.
.LS POWERCOR
Moved to the \fBiids\fR package.
.LS PROCESS
Moved to the \fBiids/irs\fR package.
.LS REBIN
This task has been eliminated. Use \fBscopy\fR or \fBdispcor\fR.
.LS REFSPECTRA
A group parameter was added to allow restricting assignments by
observing period; for example by night.  The record format
option was removed and the record format syntax is available in
the \fBiids/irs\fR packages.
.LS REIDENTIFY
This task is a new version with many new features.  The new
features include  an  interactive  options  for reviewing 
identifications, iterative rejection  of  features  during 
fitting, automatic addition of new features from a line list,
and the choice of tracing or using a single master reference
when reidentifying features in other vectors of a reference
spectrum. Reidentifications from a reference image to another
image is done by matching apertures rather than tracing. New
apertures not present in the reference image may be added.
.LS SAPERTURES
This task is new.
.LS SARITH
This task is new.
.LS SCOMBINE
This task is new.
.LS SCOPY
This task is new.
.LS SENSFUNC
The latitude parameter has been replaced by the observatory
parameter. The 'i' flux calibrated graph type now shows flux
in linear scaling while the new graph type 'l' shows flux in
log scaling. A new colon command allows fixing the flux limits
for the flux calibrated graphs.
.LS SETDISP
This task has been eliminated.  Use \fBhedit\fR or the package
DISPAXIS parameter.
.LS SEXTRACT
Replaced by \fBscopy\fR.
.LS SFIT
This task is new.
.LS SFLIP
This task has been eliminated. Use image sections.
.LS SHEDIT
This task has been eliminated. Use \fBhedit\fR if needed.
.LS SINTERP
This task is unchanged.
.LS SLIST
This task was revised to be relevant for the current spectral
image formats. The old version is still available in the
\fBiids/irs\fR package.
.LS SPECPLOT
New parameters were added to select apertures and bands, plot
additional dimensions (for example the additional output from
the extras option in \fBapextract\fR), suppress the system ID banner,
suppress the Y axis scale, output a logfile, and specify the
plotting units.  The PTYPE parameter now allows negative 
numbers to select histogram style lines.  Interactively, the
plotting units may be changed and the 'v' key allows setting a
velocity scale zero point with the cursor.  The new version
supports the new spectral WCS features including nonlinear
dispersion functions.
.LS SPLOT
This is a new version with a significant number of changes. In
addition to the task changes the other general changes to the
spectroscopy packages also apply. In particular, long slit
spectra and spectra with nonlinear dispersion functions may be
used with this task. The image header or package dispaxis and
nsum parameters allow automatically extracting spectra from 2D
image. The task parameters have been modified primarily to
obtain the desired initial graph without needing to do it
interactively. In particular, the new band parameter selects
the band in 3D images, the units parameter selects the 
dispersion units, and the new histogram, nosysid, and xydraw
options select histogram line type, whether to include a system
ID banner, and allow editing a spectrum using  different 
endpoint criteria.
.LS
Because nearly every key is used there has been some shuffling,
consolidating, or elimination of keys. One needs to check the
run time '?' help or the help to determine the key changes.
.LS
Deblending may  now  use  any  number of components and 
simultaneous fitting of a linear background. A new simplified
version of gaussian fitting for a single line has been added in
the 'k' key. The old 'k', 'h', and 'v' equivalent width
commands are all part of the single 'h' command using a second
key to select a specific option. The gaussian line model from
these modes may now be subtracted from the spectrum in the same
way as the gaussian fitting.  The one-sided options,  in 
particular, are interesting in this regard as a new capability.
.LS
The arithmetic functions between two spectra are now done in
wavelength with resampling to a  common  dispersion  done 
automatically. The 't' key now provides for the full power of
the ICFIT package to be used on a spectrum for continuum
normalization, subtraction, or line and cosmic ray removal.
The 'x' editing key may now use the nearest pixel values rather
than only the y cursor position to replace regions by straight
line segments. The mode is selected by the task option 
parameter "xydraw".
.LS
Control over the graph window (plotting limits) is better
integrated so that redrawing, zooming, shifting, and the \fBgtools\fR
window commands all work well together. The new 'c' key resets
the window to the full spectrum allowing the 'r' redraw key to
redraw the current window to clean up overplots from the
gaussian fits or spectrum editing.
.LS
The dispersion units may now be selected and changed to be from
hertz to Mev and the log or inverse (for wave numbers) of units
taken. As part of the units package the 'v' key or colon
commands may be used to plot in velocity relative to some
origin. The $ key now easily toggles between the dispersion
units (whatever they may be) and pixels coordinates.
.LS
Selection of spectra has become more complex with multiaperture
and long slit spectra. New keys allow selecting apertures,
lines, columns, and bands as well as quickly scrolling through
the lines in multiaperture spectra. Overplotting is also more
general and consistent with other tasks by using the 'o' key to
toggle the next plot to be overplotted. Overplots, including
those of the gaussian line models, are now done in a different
line type.
.LS
There are new colon commands to change the dispersion axis and
summing parameters for 2D image, to toggle logging, and also to
put comments into the log file.
.LS STANDARD
Giving an unrecognized standard star name will page a list of
standard stars available in the calibration directory and then
repeat the query.
.LS SUBSETS
Moved to the \fBiids/irs\fR packages.
.LS SUMS
Moved to the \fBiids/irs\fR packages.
