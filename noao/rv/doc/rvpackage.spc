.EQ
delim $$
.EN
.RP
.TL
Specifications for the Radial Velocity Analysis Package

.AU
Michael J. Fitzpatrick
.AI
.K2 "" "" "*"
Revised January 1990

.AB
.PP
Specifications are presented for an IRAF package to compute radial velocity,
redshift and dispersion information from both one and two dimensional
IRAF images.  Requirements and specifications for each necessary task are
described as well as the algorithms used.  Specifications of user input
and program output are also discussed.  Detailed manual pages of the tasks
are included following this document.
.AE

.NH
Introduction
.PP
The following document describes the specifications for the radial velocity
package.  This package will be designed to produce radial velocity, redshift
and dispersion data for both one and two dimensional images.  To this end
both cross correlation and Fourier techniques will be employed, thus allowing
the user to choose the method of correlation best suited to his data.  Since
the needs of the individual astronomer will differ,  the tasks in this package
will use different algorithms for computation but will share some common
output.  This common output may later be used to compare the results of one
method over another, or one parameter set over another.
.PP
Most radial velocity work is done by cross correlating a standard template star
with an unknown object spectrum.  This is most often the case with a one 
dimensional data set in which the value of interest is the heliocentric radial
velocity of the object star measured with respect to the template star 
(which is usually a radial velocity standard of known velocity).  Several
 methods will be used to provide this information:
.IP \(bu 
A standard Fourier correlation technique in which the data are transformed 
and then mulitplied one by the complex conjugate of the other and reverse
transformed, thus procuding a normalized cross correlation function.
Filtering of the data while in Fourier space is allowed.  Error calculations
are also better prepared because of the nature of the algorithms.
.IP \(bu
A squared difference method in which the sums of the squared difference between
the intensities of the two spectra are computed at each trial shift. This 
produces an unnormalized function which may be used to compute a relative 
shift and hence velocity.  Restictions on this task will be rather loose
to allow it to be used more efficiently as a "quick-look" device.
.IP \(bu
A direct correlation method, identical in operation to the squared difference
task yet producing a normalized correlation function.
.LP
All of the resulting functions are fit with a user specified function 
providing a more accurate velocity.  The details of these functions and 
relative merits of the methods are discussed below.
.PP
A second desire of astronomers using this package is to correlate a galaxy 
spectrum against a standard template spectrum.  While this may be done with
one dimensional data to produce a redshift value, it is often used with
two dimensional data in which the aim is produce a velocity dispersion of the
galaxy with respect to a distance \fIr\fR from the center.  Using this 
information at different position angles across the galaxy image, it is
possible to map a velocity field 
within the galaxy, describing it's rotation. Again, two methods will be used
to provide this information.
.IP \(bu
A Fourier quotient method in which the ratio of the Fourier transforms of 
the galaxy to the stellar spectra are fit to the Fourier transform of a chosen 
broadening function, the results of which provide the velocity dispersion,
redshift, and relative line strength parameter.
.IP \(bu
A Fourier difference method in which the difference of the Fourier transforms 
of the galaxy and star spectra are fit to the Fourier transform of a chosen 
broadening function, the results of which provide the velocity dispersion,
redshift, and relative line strength parameter.
.LP
The details of these functions and methods and their relative merits 
are discussed below.
.PP
Each of the tasks in this package will also act as an interactive parameter
editor, permitting the user to change task parameter values and immediately
examine the effect on the data.  The user may then save these parameters for
a batch (non-interactive) run or process each of the images individually.
.PP
Although it will be generally assumed that the data have been prepared using
other applications available within IRAF, a limited set of data preparation
commands will be available within each of the major tasks.  (A more in depth
description of these tasks is found below.) These commands will perform the 
following functions:
.IP \(bu
Filtering of the data while in Fourier space.  Often times it may be necessary
to filter certain frequency components from the data that may artificially
weight the correlation.  A choice of filter functions will be available and
the user may specify the fourier components over which the filter will operate.
.IP \(bu 
Removal of a local continuum.  While this task is easily accomplished with
either the \fICONTINUUM\fR task or selective filtering, it is sometimes
desireable to remove a continuum from the data at this step of the analysis.
The user specifies the order of a polynomial or spline to be fit to the 
data to remove low frequency trends in the data.  This fitted function may be
either subtracted or divided from the data.
.IP \(bu
Masking of regions to be used in the correlation.  To exclude sparse 
line regions,
bad pixels, or telluric features, it will sometimes be necessary to input a
specific region to be used in the correlation.  The user is able to specify in
either pixel number or wavelength the regions to be included in the 
correlation.  Since a broad error in the chip or wide feature may weight the
continuum fitting, continuum fitting will also take advantage of regions via
the \fICONTINUUM.SAMPLES\fR parameter.

.NH
Input Requirements and Specifications
.PP
The following requirements and specifications will be met with regard
to input format to all of the tasks:
.IP \(bu
The object and template stars may both be specified as lists.  Both lists
may be positioned forward and backward from interactive operation with a colon
command. In interactive mode the user is required to position each list
separately. In batch mode each object spectrum is correlated to each 
template in the list before moving on to the next object.
.IP \(bu
The user may specify an aperture list to be used in the correlation.  
This aperture list will be used in both the object and template spectrum.
It may be used to specify an echelle order or simply the row number in
a two dimensional image to be used if for some reason data have been stacked.
In the case of a two dimensional object/template spectrum and it's one
dimensional counterpart, the aperture number will apply only to the two
dimensional data.
.IP \(bu
The new IRAF echelle and multispec formats will be supported.
.IP \(bu
Data are required to be binned linearly in logarithm of the wavelength for 
Fourier tasks.  The squared difference or direct correlation
task may use either log-wavelength,
wavelength, or pixel scaled data. For pixel scaled data, output will
contain only pixel shift information since velocity information cannot be 
computed. Data may be rebinned automaticall with the appropriate task through
use of the \fIPROCESSPARS\fR pset (see below).
.KS
.IP \(bu
The following information must be contained in each object image header in
order for the heliocentric correction to be done properly:
.nf
.ta 1i 2i 3i
	ra	- Right Ascension of object
	dec	- Declination of object
	date-obs	- UT Date of observation
	ut	- UT time of observation
	epoch	- Epoch of observation
	exptime/otime	- Exposure time of frame
	w0/crval1	- Starting wavelength in Angstroms or log(Angstroms)
	wpc/crdelt1	- Wavelength increment in Angstroms or log(Angstroms)
.fi
Keyword translation is handled by the \fIRVKEYWORDS\fR pset as discussed
below.  Warning messages will be issued for missing keywords, which may
affect the accuracy of the results.
.KE
.IP \(bu
The user may input a number of rows that will be averaged according to
user specifications to be used in the correlation for the \fIXCOR2D\fR
task.  For two dimensional data it may be desired to average a number of 
rows into bins for computation rather than doing each row independantly.  
Presently the template spectrum is assumed to be a one dimensional spectrum,
if not only the first row will be used in the correlation.
.IP \(bu
The \fIMKBINS\fR task may be used to create bins of approximately equal 
intensity.  A description of this task and the database structure used 
is described below.

.NH 2 
Rebinning of Input Data
.PP
Input data should be dispersion corrected, and while certain tasks require that
the data be presented on a logarithmic scale, it shall be possible to input
data which are not logarithmically binned.  In this case, and if required by the
task, the data shall be rebinned automatically from the data I/O routines using
the same (or equivalent) starting wavelength and wavelength per channel values.
An informational message will be enetered into the log file indicating that 
the data have been rebinned.
.PP
The parameters controlling data rebinning will be retrieved from 
the \fIprocesspars\fR pset and the image header.  The interpolation function 
may be changed with the \fI:rb_func [s_value]\fR command followed by 
a \fI:rebin\fR command to perform the action.  This ability will be common to 
all tasks.  When data have been rebinned, a note will be made to the log file.
In batch operation of any task, the data will be rebinned automatically if
required and the appropriate notes made to the log file.
.PP
The \fIw0\fR, \fIwpc\fR and \fInpts\fR parameters will be obtained from the
current image header in the \fIprocesspars\fR value is INDEF, otherwise these
may be set to overide the current value.
.PP
If the user is not satisfied with rebinning the data using the current 
dispersion or number of points, the \fIdo_rebin\fR parameter should be turned
off and the data rebinned outside the task using one of the other available
tasks.  If \fIdo_rebin\fR is disabled and data must be rebinned, the task
will abort with an error message.
.NH 2
Continuum Removal From the Data
.PP
It shall be possible for the user to continuum normalize the data in a manner
identical to that done by the \fIonedspec.continuum\fR task.  The data
input to the Fourier tasks should be normalized by subtracting the continuum
and dividing by the average to get a mean of zero with excursions of order
unity.  The \fIrvfquot\fR and \fIrvfdiff\fR tasks need to know the value of the
spectrum average in order to compute the photon counting statustucs before 
normalization so it is advised that continuum 
normalization be left to the task (i.e. use the normalization commands
available in the tasks as opposed to the \fIcontinuum\fR task)
The apodized, normalized spectrum may be previewed by issuing a \fI:cont\fR 
command to do the normalization and a \fIn\fR keystroke to show a split-plot 
of the flattened data
.PP
Interactive flattening of the data behaves exactly like the \fIcontinuum\fR
task, except that the data are divided by the average once the continuum has
been subtracted.  If the \fIprocesspars.type\fR parameter is set to "ratio"
then the data will be normalized to a mean of unity and will not be divided
by the average.

.NH
Output Requirements and Specifications
.PP
Output from the tasks will take the form of either graphics drawn to the
standard graphics device, metacode to a graphics spool file, or text 
(sometimes verbose) output to a spool file and the screen (the abridged 
version).  With the exception of the pset tasks, all other tasks in this 
package may have graphics and or text output.
.NH 2 
Graphics Output
.NH 3
Graphics Metacode
.PP
The simplest graphical output from the tasks \fIrvfquot\fR and \fIrvfdiff\fR,
will be metacode for the quotient or difference plots (if the \fIfit_plot\fR
or \fIdiff_plot\fR parameters are set) and the FFT's of the data (if 
the \fIfft_plot\fR parameter is set).
The simplest graphical output from the tasks \fIrvsqdiff\fR and \fIrvxcor\fR 
gin

will be metacode of the correlation plot and it's fitted function (s) directed 
to a user named spool file.  
.PP
Metacode from the \fIrvdisp\fR task will be written to a user defined file
and consist only of the computed dispersion curves and fit.
.PP
These plots may later be viewed with the \fIgkimosaic\fR task to quickly view 
the results of a batch process.  Each time the user uses either the 'g' or 'y'
commands to fit to new points, metacode for the new fit will also be written.
.NH 3
Standard Graphics Plots
.PP
Plots drawn to the screen from the four main cross correlation tasks will 
consist of the following:
.IP \(bu
An overplot of the two spectra upon task startup and every time a new image
is read (except when the \fIspec_plot\fR parameter is set).  The mean of the
data will be normalized to unity to allow for the overplotting of the two
spectra, which may be at different intensity scales.  The mean is used instead
of the maximum so that cosmic ray events will not affect the plotting.
.IP \(bu
A plot of the Fourier transform of each spectrum to aide the user in choosing
a proper filter.  This plot will be generated for each spectrum's transform
and shown to the user by typing the 'f' command.  After viewing the plots
the user may issue commands to select an appropriate filter.
.IP \(bu
A graph of the fitting function.  The fitted function will be overplot on the
correlation function once the endpoints have been selected and the fit 
completed.  This plot is also written to the metacode file if specified.
.IP \(bu
For the tasks \fIrvxcor\fR and \fIrvsqdiff\fR a correlation plot produced
by those methods. This plot is also written to the metacode file if specified.
.IP \(bu
For the \fIrvfquot\fR task a plot of the ratio of the galaxy to stellar 
spectrum projected onto a unit vector $exp(-2 pi i k zeta / n)$ where $zeta$
is the logarithmic redshift.  For two dimensional galaxy spectra, 
each bin will produce a quotient plot if the \fIquot_plot\fR parameter 
is set.
.IP \(bu
For the \fIrvfdiff\fR task a plot of the difference of the galaxy and stellar 
spectrum.  For two dimensional galaxy spectra, 
each bin will produce a difference plot if the \fIdiff_plot\fR parameter 
is set or a summary plot if the \fIsummary_plot\fR parameter is set.
.NH 2
Text Output
.PP
Text output to the logfile common to each task will be the following:
.IP \(bu
An optional header explaining the meaning of each parameter if the \fIheader\fR
parameter is set.
.IP \(bu
The initial parameters of the reduction, one to a line consisting of a 
'#P' in the first two columns identifying the line as a parameter, the
parameter name, and it's value.  Each time a parameter is changed from the
command loop, a new line will be written to the log file showing the new
value of the parameter.
.IP \(bu
A keyword formatted the same as the parameter line identifying the
image name (IMAGE), the object name (OBJECT), the template image
name (TEMPLATE), the bin number used (BIN_NO), the correlation or
reduction method (CORM) and the fitting function used (FITF).
.IP \(bu
A date/time string identifying the date/time of reduction.
.IP \(bu
Any error or warning messages issued from the task.
.IP \(bu
A data record containing values input to or computed by the task.
For the \fIrvxcor\fR and \fIrvsqdiff\fR tasks, the data record will 
contain:
.RS
.IP \(bu
Heliocentric Julian Date
.IP \(bu
Computed pixel shift and error of fit to CCF
.IP \(bu
FWHM of CCF peak in pixels
.IP \(bu
Height of the peak
.IP \(bu
Observed radial velocity
.IP \(bu
Heliocentric radial velocity
.IP \(bu
The derived velocity dispersion.
.IP \(bu
Comments of reduction, identifying errors or trouble spots.
.RE
For the \fIrvfquot\fR and \fIrvfdiff\fR tasks, the data record will 
contain:
.RS
.IP \(bu
Heliocentric Julian Date
.IP \(bu
Observed redshift, line strength parameter and dispersion
.IP \(bu
Heliocentric redshift
.IP \(bu
Error of fit to difference or quotient
.IP \(bu
Comments of reduction, identifying errors or trouble spots.
.RE
.NH 2
Database Records
.PP
Dispersion calculations require that the solution from fitting the
dispersion curve (i.e. a curve produced by convolving Gaussians
of known width with stellar spectra and comparing the input gaussian
width with the derived correlation peak width) be used by the \fIrvxcor\fR
and \fIrvsqdiff\fR tasks to convert the derived correlation peak widths
to true dispersions.  Since this is usually done empirically, the 
task \fIrvdisp\fR will be used to convolve a stellar spectrum with
user specified widths and fit the resulting curve with a polynomial 
of order $n$.  The parameters used to create this curve as well as the
coefficients of the polynomial will be written in the form of a database
record.  
.PP
The name of the database file may then be passed to either the \fIrvxcor\fR
or \fIrvsqdiff\fR tasks which will use the coefficients to convert
the correlation widths.  The correlation tasks will also check each
record in a file to find one in which the parameters used for the
dispersion curve calculation match those used for the correlation.
Failing a match of parameters, no dispersion calculation will be done, however
a velocity value of the FWHM width will be printed.
Changing parameters in a task will also force a search search for a new 
record to match the parameters.
.PP
Below is an example database record used by the \fIrvdisp\fR, \fIrvxcor\fR 
and \fIrvsqdiff\fR tasks.
.nf

	#T  Aug 31 14:50 
	begin
	    image		star001
	    object		HR1762
	    corrfunc		fourier
	    fitfunc		parabola
	    filterpars
	        filter 		yes
	        filtertype	hanning
	        cuton		5
	        cutoff		150
	        fullon		0
	        fulloff		0
	    apodize		0.1
	    order		4
	    vstart		10.0
	    vincrement		5.0
	    npts		10
	    coeffs		4
		0.21345 
		0.82548 
		0.02345 
		0.00342
.fi

.NH
Interactive Parameter Editing
.PP
One common trait of all the tasks is the ability to change
the value of parameters interactively using colon commands.  The user
is able to evaluate the result of each parameter change and then decide
on the best value for his reduction.  The basic idea is to allow the user to
examine the effects of different parameter values on a typical set of
data and then process the input list with the chosen parameters.  The other
envisioned use is of an astronomer that has completely different data and
wishes to reduce each star individually.
.PP
One other point that should be noted is the interaction of parameters between
tasks in the package.  For instance, the filtering parameters set by 
the \fIfilterpars\fR pset are used by the \fIrvdisp\fR, \fIrvxcor\fR
and \fIrvsqdiff\fR tasks.  Similarly, parameters used in the \fIrvdisp\fR
task should match as closely as possible those used in the correlation
tasks since the computation of the dispersion value relies on the fit
to the dispersion curve (which may or may not be the same for different
parameters).  For this reason, many of the colon commands will be the 
same between different tasks. 
.PP
The \fI':update'\fR command is provided to save the chosen parameters to the 
task or pset parameter files.   
This command will also update the \fIfilterpars\fR
pset if given an argument of 'filter' (likewise for the other package psets).  
Similarly, the \fI':unlearn'\fR command is provided to 
reset the parameters to their default values.  It should, however, be used
with care and as a last resort to reset the parameters to their defaults. 
Each time a parameter is changed which will affect the output, a note is made 
in the spool file reflecting the new parameter value.

.br
.NH 
Use of Parameter Sets in the Package
.PP
.NH 2
Image Header Keyword Translation
.PP
The following parameters control translation of image header keywords.  If
the exposure time for the frame is given by the "EXPTIME" keyword in your
image header, as opposed to the "OTIME" keyword, just change the value of
the keyword.
.nf

	(ra = "RA")				Right Ascension keyword
	(dec = "DEC")			Declination keyword
	(ut = "UT")				UT of observation keyword
	(exptime = "OTIME")		Exposure time keyword
	(epoch = "EPOCH")		Epoch of observation keyword
	(date_obs = "DATE-OBS")	Date of observation keyword
	(w0 = "W0")			Starting wavelength keyword
	(wpc = "WPC")			Wavelength per channel keyword\n
	(hjd = "HJD")			Heliocentric Julian date
	(vobs = "VOBS")			Observed velocity keyword
	(vhelio = "VHELIO")		Heliocentric velocity keyword
	(vlsr = "VLSR")			LSR velocity keyword
.fi
.NH 2 
Processing Parameters
.PP
The following parameters control operation of the continuum removal and
data rebinning. INDEF values in the rebinning parameters indicate that
those values should be obtained from the image header.
.nf
	(do_cont = yes)			Do continuum normalization?
	(interactive = no)			Fit continuum interactively?
	(type = "difference")		Type of output (diff|ratio)
	(sample = "*")			Sample of points to use in fit
	(naverage = 1)			Number of points in sample averaging
	(function = "spline3")		Fitting function
	(order = 1)				Order of fitting function
	(low_reject = 2.)			Low rejection in sigma of fit
	(high_reject = 2.)			High rejection in sigma of fit
	(niterate = 10)		 	Number of rejection iterations
	(grow = 1.)		 		Rejection growing radius
	(scale_conser = yes)		Maintain scale of input image.
	(obj_only = no)			Normalize only object image?

	(do_rebin = yes)			Rebin data if necessary?
	(interp_mode = "poly5")		Rebin interpolation method
	(rb_order = 1)			Order of fitting function
	(w0 = INDEF)			Starting wavelength
	(wpc = INDEF)			Wavelength increment
	(npts = INDEF)			No. of output points

	(ccf_output = "ccfdemo")	Output file/image name for ccf dump
	(out_type = "image")		Type of output file to create
	(out_axis = "lag")		X-axis for output

.fi
.NH 2
Filter Parameters
.PP
The following parameters control filtering of the data while in Fourier
space.
.nf

	(filter = yes)			Filter the data before correlation?
	(filt_type = "ramp")		Filter window type
	(cuton = 1)				Cuton wavenumber for filter
	(cutoff = 100)			Cutoff wavenumber for filter
	(fullon = 10)			Wavenumber at which filter reaches one
	(fulloff = 200)			Wavenumber at which filter reaches zero
.fi
.NH 2
Fourier Plotting Parameters
.PP
The following parameters control filtering of the data while in Fourier
space.
.nf
	(plot = "amplitude")		What form of FFT plot?
	(overlay = yes)			Overlay the filter function on the plot?
	(split_plot = yes)		Produce a split plot on the screen?
	(one_image = "object")		What image is plotted if one screen
	(when = "before")		Plot FFT before or after filtering
	(log_scale = yes)		Plot on a log scale?
	(x_axis = "frequency")		What is the x-axis scaling?
	(fft_zoom = 4.)			Zoom factor if not displaying whole FFT
.fi

.NH
Cross Correlation and Fourier Techniques Used
.PP
The requirements and specifications of the correlation and Fourier techniques
to be used are described below along with the gory detail of the algorithms
themselves.
.NH 2
Requirements and Specifications
.LP
The cross correlation techniques used must provide the following operations:
.IP \(bu
Each one dimensional correlation must produce a value of the relative shift
between the object and template spectrum.
.IP \(bu
Each value of the shift derived from the correlation function shall have an 
error estimate attatched to it.
.IP \(bu
Each correlation method must be independant of the data format (i.e. longslit
data which have been averaged into rows, echelle orders, or aperture numbers).
.IP \(bu
There shall be no restriction on the length of the data to be operated upon.
.IP \(bu
The user shall be able to control the range over which the resulting 
correlation function is useful.  For example,  the user may filter out 
wavenumbers that are not to be used in the correlation, adjust the range 
over which a shift will be searched, or control the number of points in the
correlation function to be fit.
.LP
The following correlation methods will be made available by the package:
.IP \(bu
A squared difference method in which an unnormalized correlation function is
produced by summing the squared difference between the object and template
spectra at a given trial shift.
.IP \(bu
A standard Fourier correlation method in which the data are transformed and
one multiplied by the conjugate of the other and the resultant inverse 
transformed to produce a normalized correlation function.
.IP \(bu
A Fourier quotient method in which a galaxy and stellar spectrum are transformed
and their ratio fit to a broadening function, the parameters of which will
describe the relative line strength, velocity dispersion and redshift of 
the galaxy spectrum.
.IP \(bu
A Fourier difference method in which a galaxy and stellar spectrum 
are transformed and their difference fit to a broadening function, the 
parameters of which will describe the relative line strength, velocity 
dispersion and redshift of the galaxy spectrum.
.NH 2
Algorithms
.PP
The basic specific algorithms to be employed are briefly described below.
.NH 3
Fourier Cross Correlation
.PP
The Fourier cross correlation is to be done in the standard way:  The object 
and template spectrum are transformed into Fourier space and once there the
object transform is multiplied by the complex conjugate of the template
transform.  The resultant is then inverse transformed back to real space 
producing a normalized cross correlation function, the peak of which is
at a lag corresponding to the pixel shift between the two spectra.  
The error computation
and the algorithm in general will follow the work of Tonry & Davis (1979,
Ast. J, \fI84\fR, 1511).
.NH 3 
Squared Difference Correlation
.PP
This method is most commonly known at NOAO as "Daryl's program" but actually
was described by Weiss et al (1978, Astron. Astrophys., \fI63\fR, 247).  The 
method works as follows:
.EQ
	d sub j ~=~ sum from i=n1 to n2 (x sub i ~-~ y sub i+j ) sup 2
.EN
where $x sub i$ denotes the intensity of the reference spectrum and $y sub i+j$
denotes the intensity of the object spectrum at a trial shift $j$.  
The resulting $d sub j$ array produces a curve whose minimum is at the pixel 
shift between the two spectra. Unfortunately, this method produces an
unnormalized correlation function, thus making an estimate of the quality
of the correlation impossible.
.NH 3 
Fourier Quotient Method
.PP
This method was first described by Sargent et al (1977, Astrophys. J, \fI212\fR,
326) and is still a useful method for obtaining velocity dispersions in
galaxies.  It is assumed that the galaxy spectrum is a convolution of an 
appropriate mean stellar spectrum with a Doppler broadening function.
From the convolution theorem then, the
Fourier transform of the galaxy spectrum would be the product of the transform
of the stellar spectrum with the transform of the broadening function.  The
broadening function is usually assumed to be a Gaussian characterized by a
dispersion $sigma$ and a redshift $z$.
By computing the transforms of the galaxy and template (stellar) spectra, it is 
possible to fit the ratio of the galaxy transform to the stellar transform,
adopting the values of $sigma$ and $z$ which yield the best fit.
.PP
Therefore, from the definition of the discrete Fourier transform 
F(k) of a function F(j), we find that the broadening function is described
in terms of the transforms of the galaxy spectrum G(j) and the 
star spectrum S(j) as
.EQ
	{ G tilde (k) } over { S tilde (k) } ~~=~~ gamma~ exp left [ - 1 over 2 
left ( {2 pi ks} over n right ) sup 2 ~+~ { { 2 pi k zeta} over n } right ]~~ ;
.EN 
.EQ
	s ~==~ sigma over { c~ DELTA~ ln lambda} ,~~~~~~~   
	zeta ~==~ { ln (1 + z) } over { DELTA~ ln lambda }
.EN
The parameters \fIs\fR and $zeta$
are the velocity dispersion and logarithmic redshift measured in pixels 
respectively.  The parameter $gamma$
is a normalization factor which measures the strength of the galaxy lines 
with respect to the stellar lines.
.NH 3 
Fourier Difference Method
.PP
The Fourier difference method is similar to the quotient method in the
assumption that a galaxy spectrum can be treated as a mean stellar spectrum
convolved with a broadening function.  It does, however, try to remedy
the inherent deficiency of weighting certain points too heavily that
appears in the Fourier Quotient method. The Fourier difference method is
best described by noting that the galaxy and stellar spectra are fit to
each other rather than to the broadening function, thus making the error
analysis more straightforward.  If the noise in the stellar spectrum
is negligable, however, then the two methods are comparable.
.PP
For a given galaxy spectrum \fIG\fR, a stellar spectrum \fIS\fR, and a 
broadening function \fIB\fR, we wish to minimize the residual in the Fourier 
domain denoted
by
.EQ
	chi tilde sup 2 ~=~ sum from j=0 to N-1 ~( G tilde "" sub j sup * ~-~ 
B tilde "" sub j sup * G tilde "" sub j sup * )~( G tilde "" sub j ~-~ 
B tilde "" sub j G tilde "" sub j ) 
.EN
Where $G tilde$, $S tilde$, and $B tilde$ are the Fourier transforms of
the galaxy, star, and broadening function respectively.
This should simplify the numerical fitting because the convolution is now
a simple multiplication in Fourier space.

.NH
Remaining Task Algorithms
.PP
\fBNOTE:\fI  At this writing, the details of these algorithms are not 
yet defined.\fR
.NH 2
Telluric Line Removal
.PP
A task shall be written to automatically remove telluric or other artificial
features.  The cross correlation techniques will be employed to compute the
relative shift between the object and template spectra.  The relative line
depths will also be computed and the spectra divided to remove the lines
in the template spectrum from the object spectrum.  
.NH 2
Fitting (Emmision) Line Profiles
.PP
A task shall be written to do line profile fitting for the purpose of velocity
analysis.  It will be possible to trace the various profile parameters (center,
width, etc) and derive velocity information.  This task may also be used to
do postprocessing of the correlation function.

.NH
Filtering of the Data in Fourier Space
.PP
To remove noise in the data once it has been transformed into the Fourier
domain, it must be possible to filter out unwanted frequencies from the data.
Filtering the data in the Fourier domain by attenuating or eliminating 
certain frequencies has the same effect as smoothing the data in real space.
Since the data are assumed to be binned linearly in log wavelength, no 
phase shifts are introduced by the filtering.
.NH 2
Requirements and Specifications
.LP
The following filtering requirements must be met
.IP \(bu
A choice of filtering functions must be made available to the user.
.IP \(bu
The user must specify the wavenumbers over which the filter will operate.
.IP \(bu
The data must be binned linearly with the logarithm of the wavelength so as
not to introduce any phase shifts when filtering.
.IP \(bu 
Filtering of data must be possible from any of the tasks using Fourier 
techniques.
.IP \(bu
Those wavenumbers outside the specified cuton and cutoff numbers will be set to
zero, while those inside the range will be attenuated according to the
filter function chosen.
.IP \(bu
The specified range over which the filter extends must be the same for both the
object and reference spectrum.
.LP
The following filtering functions will be made available to the user:
.IP \(bu
\fBSquare\fR - A square step function in which the user specifies the
beginning and ending wavenumbers.
.IP \(bu
\fBRamp\fR - A ramp function in which the user must specify the cuton
wavenumber, the wavenumber at which the filter reaches full value, the
wavenumber at which the filter begins to decline and the cutoff wavenumber.
.IP \(bu
\fBHanning\fR -  The user must specify the cuton and cutoff wavenumbers.
The data are attentuated according to the function:
.EQ
w sub j  ~=~ 1 over 2 left [ 1. ~-~ cos left ( { 2 pi j } over { N-1 } right ) right ]
.EN
.nf
.na
		where 	j =  (wavenumber - cuton_wavenumer) 
			  N =  (cutoff - cuton) + 1
.ad
.fi
.IP \(bu
\fBWelch\fR - The user must specify the cuton and cutoff wavenumbers.
The data are attentuated according to the function:
.EQ
w sub j ~=~ 1 ~-~ left [ { j ~-~ 1 over 2 ( N - 1 ) } over { 1 over 2 ( N + 1 ) } right ] sup 2
.EN
.nf
.na
		where 	j =  (wavenumber - cuton_wavenumer) 
			  N =  (cutoff - cuton) + 1
.ad
.fi

.NH
Dispersion Calculations
.PP
Often times when computing the velocity dispersion from a correlation function,
the simplest thing to be done is to convert the full width at half maximum 
(FWHM) of the fitted peak to a velocity.  However, this is not fully correct
since the width of the correlation function is "an average of the widths of
galaxy lines quadratically added to the widths of template lines, and is
therefore the quadratic sum of two stellar widths and the velocity broadening
width".  If the function fit to the peak is a polynomial (e.g a parabola),
what is required is a method in which to convert a simple FWHM pixel width of 
the ccf to a true dispersion.  
.PP
When the correlation peak is fit and a width determined, we must define 
a relationship between the width $w$ and the width of a Gaussian profile
(the intrinsic dispersion), $gamma$.  For a given fit to the
peak, this can be expressed (following Tonry & Davis) as
.EQ
gamma ~=~ f(w) ~=~ s sub 1 ~+~ s sub 2 w ~+~ s sub 3 w sup 2 ~+~ s sub 4 w sup 3
.EN
.LP
The coefficients for this function are computed by empirically convolving
Gaussians of known velocity width with stellar spectra, thus producing
a plot of dispersion versus width which can be fit to obtain the coefficients
of the polynomial.
It must be remembered that the coefficients $s sub i$ must be recalculated
at each new instrumental setup since the function $f(w)$ is used to also
remove the instrumental distortions that can be caused by varying slit widths.
.PP
The \fIrvdisp\fR task may be used to compute the polynomial coefficients
and produce a database record of the input parameters and coefficients
that will be used bythe correlation tasks.  Multiple records per file
are permitted, allowing for varying instrumental setups and parameters since
the correlation tasks will search for a match of parameters.
.NH 2
Requirements and Specifications
.PP
To meet the needs of the astronomer in calculating dispersions, a task will
be written that meets the following specifications:
.IP \(bu
The user must specify the number of points to be computed, the velocity
increment,  and the starting dispersion velocity,
thus producing a lookup table at a specified resolution.
.IP \(bu
The user will specify the name of a database file which will contain the
$s sub i$ coefficients as well as the points used in the fit and parameters
of the task.
.IP \(bu
Each of the tasks that can compute a dispersion can be furnished the
name of this database file and use the information contained to compute
a dispersion value from the width of the correlation function.
.IP \(bu
The task will be able to run in interactive or batch mode.  In batch mode the
task will compute the dispersions and output to the database automatically.
In interactive mode the user may adjust parameters as in other tasks in this
package, allowing him to produce database files for various instrumental
setups at one time.
.IP \(bu
The task must be able to call one of the other one dimensional correlation
routines to compute the correlation functions.
.IP \(bu
The task must be able to call one of the other available fitting functions.
.IP \(bu
The task must be able to filter the data if a Fourier method is chosen.

.NH
Fitting Functions
.PP
A set of non-linear least squares routines is needed to fit the computed
correlation function to obtain a shift, or to fit the entire spectum to 
flatten it.  Polynomial fitting routines are well known and easy enough to
implement, but we also require more complex functions.  Not only is it 
desireable to fit a parabola (second order polynomial) to the peak of the
correlation function (which is a real function), but 
a Gaussian is sometimes desired.  Also, when
fitting the ratio or difference between the transforms of the galaxy and
stellar spectra, a gaussian is needed to fit the complex transform of the
broadening function.
.PP
Since a variety of fitting functions are needed and to ease in the 
incorporation of other fitting functions in the future, the non-linear
least squares package \fInlfit\fR written by Lindsay Davis for the \fIAPPHOT\fR
package will be used.  This has the distinct advatange that all that is
required to add a new function is to write routines to evaluate the
function and it's derivatives, simplifying things greatly.
.NH 2
Requirements and Specifications
.PP
The following fitting functions will be provided and are chosen through
the tasks parameters or commands:
.KS
.IP \(bu
Parabola.
.IP \(bu
One dimensional real Gaussian.
.IP \(bu
One dimensional complex Gaussian.
.IP \(bu 
$N sup th$-order polynomial.
.KE

.EQ
delim off
.EN
.NH
Tasks
.PP
The required tasks for this package are the following:
.nf

.na
    mkbins		- Create bins of approximately equal flux intensity
    observatory	- Observatory location database
    processpars	- Batch processing parameters for RV package
    filterpars		- Edit the filter function parameters 
    rvcorrect		- Compute radial velocity corrections
    rvdisp		- Produce velocity dispersions from CCF widths
    rvemfit		- Fit emmission features in spectra
    rvfdiff		- Redshifts and dispersions via Fourier Difference techniques
    rvfquot		- Redshifts and dispersions via Fourier Quotient techniques
    rvkeywords	- Keyword translation table for RV image headers
    rvselect		- Select output fields from an RV record 
    rvskyline		- Telluric line removal/fitting task
    rvstats		- Print information to aide in RV parameter selection
    rvsummary	- Print a summary table of output from RV tasks
    rvsqdiff		- Radial velocities via a squared difference correlation
    rvxcor 		- Radial velocities via Fourier cross correlation
    xcor2d		- Cross correlation of two dimensional data
.ad
.fi

.NH 2
Usage
.LP
Some examples of typical usage are listed below.
.IP \(bu
A user has a series of Longslit spectra of galaxies at differing position
angles and wishes to correlate them with a template spectrum to 
obtain redshift and dispersion information.  
The user then uses the \fIrvfdiff\fR
or \fIrvfquot\fR tasks to correlate the spectra and compute the dispersions.
.IP \(bu
A user is interested in obtaining radial velocities of a series of spectra
obtained over several nights with different instrumental setups.  
The \fIrvdisp\fR task is used to create dispersion tables for each instrumental
setup.  One of the correlation tasks is then used to correlate each set 
of spectra.
.IP \(bu
A user has a small series of unusual spectra of different spectral types 
and wishes to obtain radial velocities.
The \fIrvdisp\fR task is used to create dispersion tables for each instrumental
setup.  The user then chooses the correlation method he wishes to use and 
interactively adjusts parameters for each object spectrum, writing the results 
to the logfile when satisfied.
.IP \(bu
A user has a large number of low signal-to-noise spectra to be correlated 
to obtain a list of radial velocities.
The \fIrvdisp\fR task is used to create dispersion tables for each new 
instrumental setup (if any).  The user then chooses the correlation method 
he wishes to use and after setting up the parameters, processes the list
as a background job.  After returning from coffee, he examines the
output list and graphics spool file for bad fits which can be done by hand
later.
.IP \(bu
A user has an old set of spectra for which he was only able to obtain an
observed radial velocity and wishes to do the heliocentric velocity corrections.
The user may then use the use the \fIimages.hedit\fR tasks to insert the 
observed velocity into the image header.  The \fIrvcorrect\fR task is then
called to correct each image with respect to the sun (or even the Local 
Standard of Rest).

.NH 1
Bibliography
.PP
.XP
Press, W.H. et al 1986, \fINumerical Recipes\fR, Cambridge Univ. Press, 
	Cambridge, Ch 12.
.XP
Rabiner, L.R. and Gold, B. 1975 \fITheory and  Application  of Digital
	Signal Processing\fR, Prentice Hall,  Englewood  Cliffs, Ch 3.
.XP
Sargent, Schechter, Boksenberg and Shortridge, 1977, "Velocity Dispersions
	for 13 Galaxies", \fIAstrop. J.\fR \fB212\fR p326.
.XP
Tonry, J. and Davis,  M.  1979,  "A  Survey  of Galaxy Redshift. I. Data
	Reduction  Techniques",  \fIAstron.  J.\fR  \fB84,\fR  p 1511
.XP
Weiss, W.W. et al 1978, "A  Statistical  Approach  for  the  Determination
	of Relative Zeeman and Doppler Shifts in Spectrograms", \fIAstron. 
	Astrophys.\fR \fB63\fR, p 247. 
.XP
Willmarth, D.W and Abt, H.A., 1985, "Radial Velocities From CCD Detectors" 
	in \fIIAU Coll. No.  88,  Stellar  Radial  Velocities\fR,  p 99
.XP
Wyatt, W.F., 1985, "The  CfA  System  for  Digital  Correlations"   in 
	\fIIAU Coll. No 88,  Stellar  Radial Velocities\fR, p 123
.PP

.NH
Detailed Manual Pages
.PP
	The individual manual pages for these tasks follow this document.
