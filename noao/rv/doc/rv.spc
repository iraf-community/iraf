.help rvxcor Aug90 noao.rv
.ih
INTRODUCTION

Specifications are presented for a Fourier cross-correlation task used
to compute either relative or heliocentric radial velocities.  Input
data need not be dispersion corrected, and the user has full control
over cross-correlation function (ccf) peak fitting, continuum subtraction,
and Fourier filtering of the data.  Several sub-modes exist wherein the
user may examine the Fourier characteristics of the data, interactively
fit the continuum, and examine the input spectra themselves.  Output
options include a text logfile which may be parsed with the IRAF \fIfields\fR
task, a GKI metacode file containing a summary plot of the correlation,
and optionally a text or IRAF image of the ccf itself (from interactive 
operation only).

.ih
SUMMARY
.nf

			BASIC TASK ORGANIZATION
			
		  List of object and template spectra
				|
				|	      +------------------------+
				|	      | Rebin the data?	       |
		       +--------------+       | Fit/subtract continuum |
		       | Prepare data |<----->| Zero endpoints and pts |
		       +--------------+       |     not in sample      |
				|	      | Subtract bias          |
		                |     	      | Apodize end regions    |
		    +----------------------+  | Center in FFT array    |
		    | Compute correlation  |  +------------------------+
		    | Display correlation  |
		    | Fit correlation peak |
		    +----------------------+
				|
	   +--------------------+-----------------------+
	   ^			^		   	^
	  / \   	       / \      	       / \
	   |			|		   	|
	  \ /   	       \ /      	       \ /
	   `			`			`
+-------------------+   +---------------------+   +-------------------+
| Fourier filtering |-->| Spectra review      |-->| Continuum fitting |
|		    |<--| Mark sample regions |<--| ICFIT             |
+-------------------+   +---------------------+   +-------------------+

			TASK PARAMETERS

Input parameters:	objects
			templates
			apertures = "*"
			cursor = ""

Data preparation:	continuum = "both"
			filter = "none"
			sample = "*"
			apodize = 0.2

Peak fitting:		function = "gaussian"
			width = INDEF
			height = 0.0
			peak = no
			minwidth = 3.
			maxwidth = 11.
			weights = 1.0
			background = 0.0
			window = 20

Output parameters:	output = ""
			verbose = "yes"
			imupdate = "no"
			graphics = "stdgraph"

Control parameters:	interactive = "yes"
			autowrite = "yes"
			ccftype = "image"

Parameter sets:		continpars = ""
			filterpars = ""
			rvkeywords = ""
			observatory = ""

MAIN CORRELATION FITTING COMMAND SUMMARY

?  Print list of cursor key and colon commands
-  Subtract blended component from correlation peak
+  Toggle status line output
a  Display the antisymmetric noise component of the correlation
b  Fix background level for Gaussian fit
c  Plot polymarkers of actual CCF points on the plot
d  Deblend multiple correlation peak
e  Preview the summary plot of the correlation
f  Fourier filtering and display mode
g  Mark correlation peak lag limits and fit
j  Plot the residuals of the fit to the peak
k  Plot the ratio of the fit to the peak
l  Page the log file of results
n  Go to next (template --> aperture --> object)
o  Fit or refit object spectrum continuum for subtraction
p  Go to previous (template --> aperture --> object)
q  Quit task
r  Redraw
s  Examine object/template spectra and display mode
t  Fit or refit template spectrum continuum for subtraction
v  Print full correlation results in text window
w  Write current correlation results to the log file
x  Compute a new correlation
y  Mark correlation peak lower limit and fit
z  Expand on different correlation peak using full correlation plot

:next [template|aperture|object]	Go to next correlation pair
:parameters				List current parameters
:previous [template|aperture|object]	Go to previous correlation pair
:results [file]				Page results
:wccf file				Write the CCF to an image/text file

Other colon commands set or show parameter values

FOURIER FILTERING COMMAND SUMMARY

?  Print list of cursor key and colon commands
b  Display power spectra before filtering
f  Display Fourier transforms after filtering
g  Display Fourier transforms before filtering
i  Print period trend information
p  Display power spectra after filtering
q  Quit
r  Redraw
s  Enter spectrum mode
x  Return to correlation mode

:log_scale [yes|no]		Set log scaling on Y axis
:overlay [yes|no]		Overlay filter function
:parameters			List current parameter values
:zoom [factor]			Zoom x region

Other colon commands set or show parameter values

SPECTRUM REVIEW COMMAND SUMMARY

?  Print list of cursor key and colon commands
d  Print velocity difference between two cursor positions
e  Plot a preview of the summary plot
f  Enter Fourier mode
i  Display original input spectra
n  Display continuum subtracted spectra
q  Quit
r  Redraw
s  Mark sample regions 
x  Return to correlation mode

:sample [list]			List of sample regions

CONTINUUM FITTING COMMAND SUMMARY

See \fBicfit\fR.
.fi
.bp
.ih
NAME
rvxcor -- compute radial velocities via Fourier cross correlation
.ih
USAGE
rvxcor objects templates
.ih
PARAMETERS
.ce
INPUT PARAMETERS
.ls objects
The list of image names for the input object spectra.
.le
.ls templates
The list of image names that will be used as templates for the cross
correlation.
.le
.ls apertures = "*"
List of apertures to be used.  This number is used for \fIboth\fR the
object and reference spectra. A '*' means to process all of the
apertures in the spectrum.  If the template spectrum is one-dimensional, 
that spectrum will be used as a template for all specified apertures in 
the object spectrum.
.le
.ls cursor = ""
Graphics cursor input.
.le

.ce
DATA PREPARATION PARAMETERS
.ls continuum = "both"
Continuum subtract the spectra prior to correlation?  Possible values for
this parameter are any of the strings (or abbreviations) "object" (for object 
spectrum only), "template" (for template spectrum only), or "both" for 
continuum flattening both object and template spectra, or simply "none" for 
flattening neither spectrum.  The \fIcontinpars\fR pset is used to specify 
the continuum fitting parameters.
.le
.ls filter = "none"
Fourier filter the spectra prior to correlation?  Possible values for
this parameter are any of the strings (or abbreviations) "object" (for object 
spectrum only), "template" (for template spectrum only), or "both" for 
fourier filtering both object and template spectra, or simply "none" for 
filtering neither spectrum.  The \fIfilterpars\fR pset holds the parameters 
for the filtering (filter type and width).
.le
.ls sample = "*"
Sample regions of the spectrum to be used in the correlation specified
in pixels if the first character is a 'p' or angstroms if the first
character is an 'a'.  The default (i.e. no 'a' or 'p' as the first
character), if a range is provided, is a range specified in pixels.
This string value will be updated in an interactive session as sample
regions are re-selected. The default, '*', is the entire spectrum.  The
region is specified as a starting value, a '-', and an ending value.
.le
.ls apodize = 0.2
Fraction of endpoints to apodize with a cosine bell when preparing the data
prior to the FFT.
.le

.ce
CORRELATION PEAK FITTING PARAMETERS
.ls function = "gaussian"
Function used to find the center and width of the correlation peak.
Possible choices are "gaussian", "parabola", "lorentzian", or "center1d".
If a parabola or center1d fit is selected then only the center is determined.
.le
.ls width = INDEF
Width of the fitting region in pixels.  The fitting weights are
zero at the endpoints so the width should be something
like the expected full width.  If INDEF, then the width is
set by the \fIheight\fR and \fIpeak\fR parameters. If other than INDEF, 
this parameter will override the \fIheight\fR and \fIpeak\fR parameters.
It is recommended that an odd value of \fIwidth\fR be used to assure an
even number of pixels around the peak height.
.le
.ls height = 0.
The width of the fitting region is defined by where the correlation
function crosses this height starting from the peak.  The height is
specified as either a normalized correlation level (this is like
the 'y' interactive key) or normalized to the peak.  The type of
level is selected by the peak parameter.
.le
.ls peak = no
Measure the height parameter relative to the correlation peak value
rather than as a normalized correlation level? If yes, then \fIheight\fR
is a fraction of the peak height with an assumed base of zero.
.le
.ls minwidth = 3., maxwidth = 11.
The minimum and maximum widths allowed when the width is determined
from the height.
.le
.ls weights = 1.
Power of distance defining the fitting weights.  The points used
in fitting the correlation peak are weighted by a power of the
distance from the center as given by the equation
.nf

         1 - (distance / (width/2)) ** weights

.fi
Note that a weight parameter of zero is equivalent to uniform weights.
The center1d fitting algorithm uses it's own weighting function.
.le
.ls background = 0.0
Background level, in normalized correlation units, for a Gaussian or 
Lorentzian fitting function.  If set to INDEF, the background is a free 
parameter in the fit.
.le
.ls window = 20
Size of the window in the correlation plot.  The peak will be displayed
with a window centered on the peak maximum and two times \fIwindow\fR 
lags wide.
.le

.ce
OUTPUT PARAMETERS
.ls output = ""
File name of file to which output will be written.  If no file name is given
then no log files will be kept, but the user will be queried for a file name
if a write operation is performed.  Text output will have a ".txt" sufix 
appended, and the graphics metacode file will be appended with a ".gki" suffix.
.le
.ls verbose = "yes"
Print a verbose output record to the \fIoutput\fR file?  The verbose output
will be a single line ~160 characters wide.  The \fIfields\fR task may
be used to strip out selected columns.  Non-verbose output is ~80 chars wide.
.le
.ls imupdate = "no"
Update the image header with the computed velocities?  If set to yes, then
the image will be updated with the observed and heliocentric velocities
by adding the \fIrvkeywords.vobs\fR and \fIrvkeywords.vhelio\fR keywords
respectively.  Two-dimensional spectra will have the keywords added with 
the aperture number appended to the keyword.
.le
.ls graphics = "stdgraph"
Output graphics device.
.le

.ce
CONTROL PARAMETERS
.ls interactive = "yes"
Process spectra interactively?  
.le
.ls autowrite = "yes"
Automatically record the last fit to the log file when moving to the next
spectrum?  If set to "no", the user will be queried whether to write the
results if no write was performed, and possibly queried for a file name 
if \fIoutput\fR isn't set.  
.le
.ls ccftype = "image"
Type of output to create when writing out the correlation function with
the ":wccf file" command.  Possible choices are "text" which will be a
simple list of (lag,correlation_value) pairs, or "image" which will be an
IRAF image whose header would describe the lag limits and selected peak.
.le

.ce
ADDITIONAL PARAMETER SETS
.ls continpars = ""
The processing parameters as described in the \fIcontinpars\fR named pset.
.le
.ls filterpars = ""
This is a parameter set defining the parameters to be used in filtering the
data prior to the correlation. 
.le
.ls rvkeywords = ""
The image header keyword translation table as described in 
the \fIrvkeywords\fR named pset.
.le
.ls observatory = ""
The observatory parameter set giving the location of the observation.  These
values are used in the heliocentric correction routines.
.le
.ih
DESCRIPTION
\fIRvxcor\fR performs a Fourier cross-correlation on the input list of object
and template spectra.  Object spectra may be either one or two dimensional
(in `echelle' or `multispec' format), and may be correlated against a one
or two dimensional template.  If the template spectrum is only one dimensional
but the object is two dimensional, the template is used to correlate each of
the apertures specified by the \fIapertures\fR parameter.  Two dimensional
templates will correlate corresponding apertures.

If the input spectra are not dispersion corrected (DC-FLAG parameter missing
or less than zero) then only a pixel space correlation is done.  This is
appropriate for a simple cross-correlation of images whether spectra or not.
If the spectra are dispersion corrected a log binned correlation is
performed and various radial velocity measurements are made. At a minimum,
a relative velocity between the object and template spectra are produced.
If the image headers contain sufficient information for heliocentric
velocity corrections (see help for \fBrvkeywords\fR), the corrections are
computed and possibly recorded in the image header.  If the value of the 
heliocentric velocity is returned as INDEF, the user may use the 'v'
keystroke to see the full results of the correlation, including errors
which occured causing the corrections to not be done.

A number of operations may be performed to prepare the data for
correlation.  If a linear wavelength dispersion is defined the spectra
are rebinned to a log-linear dispersion. The starting and ending wavelength, 
the dispersion in log space, and the number of pixels are determined from
the template.  If the \fIcontinuum\fR flag is set to sonething other than 
"none", the object and/or template data will
be continuum subtracted using the fitting parameters found in the
\fIcontinpars\fR pset on input.  The data is zeroed outside the sample
region specified by the \fIsample\fR parameter, the bias is subtracted,
and the ends apodized.  If the \fIfilter\fR flag is set to something other than
"none", the data are Fourier filtered according to the parameters in 
the \fIfilterpars\fR pset prior to the correlation computation.

Once the correlation is computed, the maximum peak is found and fit
according to the \fIwidth\fR, \fIheight\fR and \fIpeak\fR parameters.
A small, unlabelled plot of the entire correlation function is 
drawn above a larger,
expanded plot centered on the peak in a window of size specified by the
\fIwindow\fR parameter.  The dashed lines in the small plot
show the limits of the expanded plot.  The bottom axis is labelled with
pixel lag and, if dispersion information is present, the top axis
is labelled with relative velocity.  The status line will contain a
summary of the pixel shift from the fit and optional velocity
information.  The 'v' keystroke may be used to suspend graphics and get
a more detailed description of the correlation and fit. To view the
antisymmetric noise component of the correlation function, simply hit 
the 'a' keystroke followed by any keystroke to return to the correlation
plot. Similarly, the 'e' keystroke may be used to preview the summary
plot of the correlation, again hitting any key to return to the correlation.

If the user is dissatisfied with the fit to the peak, he can mark the
left and right side of the peak with the 'g' keystroke to redo the fit,
or else set the cursor to mark a cutoff with the 'y' keystroke, and all
points from the peak maximum to the cursor will be fit.  To fix the background
of a Gaussian fit (i.e. change the \fIbackground\fR parameter graphically), 
type the 'b' keystroke at the desired level.  To choose a different
peak to fit, move the cursor to the top plot of the whole ccf and hit the
'z' keystroke at the desired peak.  The plot will be redrawn with the new
peak now centered in the window and a fit automatically done.  The 'r'
keystroke may be used at any time to redraw the plot, and the 'x' keystroke
can be used to compute a new correlation if any of the parameters relating
to the correlation are changed
(e.g. the apodize percentage).  New correlations are automatically computed
when new images are read in, the data are continuum subtracted, a different
region is selected for correlation, or Fourier filtering is done.

For binary star work, the user may type the 'd' and/or '-' keystrokes to fit
and then subtract up to four Gaussians to the peaks.  This feature behaves
exactly as the deblending functions in the task \fIonedspec.splot\fR.  Consult
the \fIsplot\fR help page for details.  If multiple peaks were fit, 
a separate entry
will be made in the log file for each peak with a comment that it was part of
a blended peak.  The metacode file will contain only one summary plot with
each peak marked with it's heliocentric velocity (if velocities requested)
or pixel shift and error.

To move to the next spectrum, simply hit the 'n' keystroke.  Similary,
the 'p' keystroke will move to the previous spectrum.  These commands
have a hitch, though.  By default, the next/previous commands will move
first to the next template in the template image list.  Once the end of the
template image list is reached, the next spectrum will be the next aperture
in the list specified by \fIapertures\fR, resetting the template image list
automatically and possibly updating the aperture in the template image 
as well.  Finally, after correlating all of the templates against
all of the apertures, the next/previous command will move to the next
object image, again resetting the template image and/or aperture list.
To override this sequence, the user may use the ":next"
or ":previous" commands and specify one of "aperture", "object", or
"template".  If the \fIautowrite\fR is set, the results of the last
fit will be written to the log automatically.  To write any one of the
fits explicity, use the 'w' keystroke.

The \fIrvxcor\fR task also contains three submodes discussed in detail below.
Briefly, the 'f' keystroke will put the user in the "fourier mode",
where he can examine the Fourier transform of the spectra in various
ways and change/examine the filtering parameters.  The 'o' and 't'
keystrokes let the user examine and fit the continuum for the object
and template spectra, respectively, using the \fBicfit\fR commands.
Upon exiting the continuum fitting the spectra are continuum subtracted 
and a new correlation is computed.  Finally the 's' keystroke will put
the user in "spectrum mode", in which he may graphically select the
region to be correlated, compute an approximate shift using the cursor,
or simply examine the two spectra in a variety of ways.  All of these
submodes are exited with the 'q' keystroke, after which the correlation
will be redone, if necessary, and the ccf plot redrawn.

To exit the task, the user simply types a 'q' keystroke.  Results will be saved
to the logfile automatically if one was specified, otherwise the user will
be asked if he wants to save the results, and if so queried for a file name
before exiting if no \fIoutput\fR file was defined.

(References: Tonry, J. and Davis, M. 1979 \fIAstron. J.\fR \fB84,\fR 1511, 
and Wyatt, W.F. 1985 in \fIIAU Coll. No 88, Stellar Radial Velocities\fR, 
p 123).

.ce
CURSOR KEYS AND COLON COMMANDS

.nf
?  Print list of cursor key and colon commands
-  Subtract blended component from correlation peak
+  Toggle status line output
a  Display the antisymmetric noise component of the correlation
b  Fix the background level for the Gaussian fit
c  Plot polymarkers of actual CCF points on the plot
d  Deblend multiple correlation peak
e  Preview the summary plot of the correlation
f  Fourier filtering and FFT display mode
g  Mark correlation peak lag limits and fit
j  Plot the residuals of the fit to the peak
k  Plot the ratio of the fit to the peak
l  Page the current logfile of results
n  Go to next (template --> aperture --> object)
o  Fit or refit object spectrum continuum for subtraction
p  Go to previous (template --> aperture --> object)
q  Quit task
r  Redraw
s  Examine object/template spectra and display mode
t  Fit or refit template spectrum continuum for subtraction
v  Print full correlation result in text window
w  Write current correlation results to the log file
x  Compute correlation
y  Mark correlation peak lower limit and fit
z  Expand on different correlation peak using full correlation plot

:next [template|aperture|object]	Go to next correlation pair
:parameters				List current parameters
:previous [template|aperture|object]	Go to previous correlation pair
:results [file]				Page results

Other colon commands set or show parameter values
.fi

.ih
FOURIER MODE DESCRIPTION
Fourier mode is entered from the main task via the 'f' keystroke.  By 
default, the user is presented with a split plot of the power spectra of
the object and template spectra (object on top) and the requested filter
overlayed. The X-axis is double-labelled with wavenumbers on the bottom 
and frequency on top.  The ":log_scale" command can be used to toggle 
the log scaling of the Y-axis of the plot, and the ":overlay" command 
will toggle whether or not the filter function (if specified) is overlayed 
on the plot.  By default the entire power spectrum is displayed, but 
the ":zoom" command may be used to specify a blowup factor for the 
display (e.g. ":zoom 2" will display only the first half of the power 
spectrum).  Plot scaling parameters are learned for the next invocation 
of this mode.

The plot contents may also be changed through various keystroke commands.
The 'p' keystroke will display the power spectrum (the default), the 'f'
keystroke will display the two FFT's, and the 's' keystroke will display the
unfiltered and filtered spectra vertically offset.  The 'b' and 'g' 
keystrokes may be used to examine the power spectra and FFT's 
respectively \fIbefore\fR filtering.  The user can determine the period 
trend in the data by placing the cursor at a particular wavenumber/frequency 
and hitting the 'i' keystroke (this command will not work on a plot of 
the filtered spectra).  The 'r' key will redraw whichever plot is currently
selected and a 'q' will return the user to the main task mode.

Colon commands are also used to specify or examine the filtering parameters
by simply typing a ':' followed by the parameter name found in 
the \fIfilterpars\fR pset.  The user still has full access to the colon 
commands in the main task mode.

.ce
CURSOR KEYS AND COLON COMMANDS

.nf
?  Print list of cursor key and colon commands
b  Display power spectra before filtering
f  Enter Fourier mode
g  Display Fourier transforms before filtering
i  Print period trend information
o  Display filtered and unfiltered object spectrum
p  Display power spectra after filtering
q  Quit
r  Redraw
s  Display spectra
t  Display filtered and unfiltered template spectrum
x  Return to correlation mode

:log_scale [yes|no]		Set log scaling on Y axis
:overlay [yes|no]		Overlay filter function
:parameters			List current parameter values
:zoom [factor]			Zoom x region

Other colon commands set or show parameter values
.fi

.ih
CONTINUUM MODE DESCRIPTION
Automatic continuum subtraction is controlled by the \fIcontinpars\fR
pset.  These may be reset from the main
correlation function mode.  To interactively fit and modify the continuum
fitting parameters the 'o' and 't' keys are used.  This enters
the ICFIT package which is described elsewhere.  Exiting the fitting,
with 'q', causes a recomputation of the correlation function and peak
fit.  To view the flattened spectra use the spectrum review mode
entered with the 's' key.  Fitting parameters changed while doing the
interactive continuum fitting are learned.

.ce
CURSOR KEYS AND COLON COMMANDS

See \fBicfit\fR.

.ih
SPECTRUM MODE DESCRIPTION
Spectrum mode is entered from the main task via the 's' keystroke.
The user may select plots of the original input spectra 'i', the
continuum subtracted spectra 'n', and the filtered and unfilter spectra 'f'.
In addition, a sample region for the correlation is
marked on the bottom of the plots.  To select a new sample region, use the 's' 
keystroke to select the endpoints of the region.  It may be selected 
explicity by using the ":sample" command.  The region will be checked to
see if it lies within the range of the spectrum.  The 'd' keystroke may be
used to print the difference in pixels (and/or velocity) between two points
on the spectrum.  This is useful for getting an approximate shift.  
The 'w' keystroke or ":/<command>" commands will invoke the standard 
GTOOLS windowing commands.  
To return to the correlation simply type 'q'.

(NOTE: More functionality is planned for this mode)

.ce
CURSOR KEYS AND COLON COMMANDS

.nf
?  Print list of cursor key and colon commands
d  Print velocity difference between two cursor positions
f  Enter Fourier mode
i  Display original input spectra
n  Display continuum subtracted spectra
q  Quit
r  Redraw
s  Enter Spectrum mode
w  Window graphs with GTOOLS commands
x  Return to correlation mode

:sample [list]			List of sample regions
.fi

.ih
OUTPUT FILES
If the \fIoutput\fR parameter is set, two files will be created; one with
a ".gki" suffix containing metacode output of a summary plot, and one with
text output in the standard IRAF 'list' format containing either verbose or
non-verbose output
having a ".txt" suffix.  If a write operation is performed and no output file
is specified, the user will be queried for a file name and the files will
be created.  Text file output may be have selected columns extracted
using the iraf \fIfields\fR task (where string valued fields will have
blank spaces replaced with an underscore), and specific metacode plots may
be extracted or displayed with the iraf \fIgkiextract\fR and/or
\fIstdgraph\fR/\fIgkimosaic\fR tasks.
.ls METACODE FILES
For each correlation fit recorded a metacode plot will be drawn to the file
named by the \fIoutput\fR parameter with the ".gki" extension.  This plot
will have a plot of the flattened object spectrum on top, with any selected
regions for the correlation marked.  The bottom of the plot will be similar 
to the standard correlation plot, but text will be overlaid showing the fitted
peak shift, width and computed velocities.

For a blended peak, the plot will be the same with the exception that each of
the peak components will be labelled with the computed velocity, and the
text labelling will be suppressed.
.le
.ls TEXT FILES
For each correlation fit recorded a text entry will be written to the file
named by the \fIoutput\fR parameter with the ".txt" extension.  Regardless
of whether the file contains verbose output, the file header will have comment
lines (beginning with a "#" in column 1) identifying each template used
with a letter code, followed by a description of the image name, template 
source name, velocity dispersion (which will be the same for the object 
spectra, and specified velocity.  A similar comment will be written with
a unique ID whenever a new template image is read into the task interactively.
Similar comments will also be written to identify error codes, abbreviations
used, column headings, and the region used in the correlation.
.ls NON-VERBOSE OUTPUT
(NOTE: Details about field width and such will be worked out later on...)

Non-verbose output will contain the object image name, object source name 
(usually the star name), heliocentric Julian data, aperture number,
a code field identifying whether the data were
filtered, type of fitting function, and/or continuum subtracted, the pixel
shift (and error), velocity FWHM, observed velocity, heliocentric
velocity, and velocity error, as well as an error code field.

This output will extend approximately less than 80 characters.
.le
.ls VERBOSE OUTPUT
(NOTE: Details about field width and such will be worked out later on...)

Verbose text output will contain all of the above fields in the header.
Also written will be the Tonry & Davis "R" value, correlation peak height,
FWHM error, and covariance of the correlation.

This output will extend approximately less than 160 characters.
.le
.le

.ih
ALGORITHM DISCUSSIONS
.ih
SUMMARY OF THE BASIC TASK STRUCTURE
In this section we discuss the steps taken in preparing and filtering the
data prior to the correlation computation.  We will use pseudocode to describe
this process as it is more detailed than the graphical display shown earlier.

.nf
begin
	# Data Rebinning on input
	IF (DC-FLAG flag does not exist in object)
	    DC-FLAG(object) = -1
	IF (DC-FLAG flag does not exist in template)
	    DC-FLAG(template) = -1
	IF (DC-FLAG(object) * DC-FLAG(template) < 0)
	    Print error and go on to next correlation pair
	ELSE {
	    IF (DC-FLAG exists in template header && DC-FLAG >= 0)
	       rebin data to log-linear dispersion
	    IF (DC-FLAG exists in object header && DC-FLAG >= 0)
	       rebin to log-linear using template wpc and npts values
	}

	# Continuum normaliztion and data preparation
	IF (continuum == "object" || continuum == "both")
	    fit and subtract the object continuum
	IF (continuum == "template" || continuum == "both")
	    fit and subtract the template continuum
	FOR (object and template spectrum) {
	    zero non-overlapping points
	    zero points outside sample region
	    subtract the bias
	    apodize remaining data 
	    center in FFT array of length 2^N
	    normalize data by the number of FFT points
	}

	# Do the Fourier filtering
	compute the FFT of both object and template
	IF (filter == "object" || filter == "both")
	    multiply specified filter by object FFT
	IF (filter == "template" || filter == "both")
	    multiply specified filter by template FFT

	calculate the cross correlation

	# Get interactive use set up
	display the ccf to the screen
	select max peak in ccf
	determine fit endpoints from "height" (and "nfit"/"threshold") params
	Fit the selected peak
	IF (sufficient header information) 
	    compute heliocentric velocity from pixel shift
	ELSE
	    compute only a relative velocity from pixel shift
	print results to the status line

	# Process the interactive cursor commands
	REPEAT {
	    # A few example commands
	    SWITCH (cursor command) {
			:
	    CASE 'f':
		enter Fourier mode
			:
	    CASE 'o':
		enter ICFIT for object spectrum continuum removal
			:
	    CASE 's':
		enter Spectrum mode
			:
	    CASE 't':
		enter ICFIT for template spectrum continuum removal
	    }
	}
end
.fi
.ih
PEAK FITTING ALGORITHM
Determining the center of the cross correlation peak is the key step in
measuring a relative shift or velocity between the object and template.
The width of the correlation peak is also of interest for measuring
a line broadening between the two samples.  Since people have different
preferences and prejudices about these important measurement a variety
of methods with a range of parameters is provided.

In all cases one must specify the fitting function and a sample width;
i.e. the range of points about the correlation peak to be used in the
measurement.  Note that the width defines where the fitting weights
vanish and should be something like the full width.  For the CENTER1D
algorithm the maximum weights are at the half width points while for the
other methods greater weight is given to data nearer the center.

The width may be specified in three ways.  The first is as an actual
width in pixels.  This is the most straightforward and is independent
of quirks in the actual shape of the peak.  The second way is to find
where the correlation function crosses a specified height or level.
The height may be specified in normalized correlation units or as a
fraction of the peak height.  The former is equivalent to the
interactive 'y' key setting while the latter may be used to select some
"flux" point.  A value of 0.5 in the latter would be approximately the
full widht at half intensity point except that the true zero or base of
the peak is somewhat uncertain and one needs to keep in mind that the
weights go to zero at this point.  Note that a level may be negative.
In this method the actual width may go to zero or include the entire
data range if the level fall above the peak or below the minimum of the
correlation.  The minimum and maximum width parameters are applied to
constrain the fitting region.  The last method is to interactively mark
the fitting region with the 'g' key.  A note will be made in the logfile
if an inflection of the peak is found within the sample range, indicating
a possible second peak or binary component.

There are four methods for determining the correlation peak position.
The CENTER1D algorithm has been heavily used in IRAF and is quite
stable and reliable.  It is independent of a particular model for the
shape of the peak or the background determination and is based on
bisecting the integral.  It uses antisymmetric weights with maxima
at points half way between the estimated center and the fitting
region endpoint.  A parabola fit is also independent of background
determinations and also does not determine a width.  It is included
because it is a common method of peak centering.

The gaussian and lorentzian function fits are model dependent and
determine a center, width, and peak value.  The background may also
be determined simultaneously but this extra degree of freedom
for a function which is not strictly gaussian or lorentzian may
produce results which are sensitive to details of the shape of the
correlation function.  The widths reported are the full width at
half maximum from the fits.

The parabola, gaussian, and lorentzian methods use weights which
vary continuously from 1 at the estimated center to zero at the
endpoints of the fitting region.  The functional form of the
weights is a power law with specified exponent.  A value of zero
for the exponent produces uniform weights.  However, this is
discontinuous at the endpoints and so is very sensitive to the data
window.  A value of one (the default) produces linearly decreasing weights.

All these methods produce centers which depend on the actual
data points and weights used.  Thus, it is important to iterate
using the last determined center as the center of the data window
with continuous weights in order to find a self-consistent center.
The methods are iterated until the center does not change by more
than 0.01 pixels or a maximum of 100 iterations is reached.

Errors in the pixel shift are computed from the center parameter of the fitting
function.  Velocity errors are computed based on the fitted peak height and
the antisymmetric noise as described in the Tonry & Davis paper (1979,
\fIAstron. J.\fR \fB84,\fR 1511). Dispersion/pixel-width errors are computed 
similarly.

The initial peak fit will be the maximum of the ccf.  This will be the only 
peak fit in non-interactive mode but a confidence level will be entered in
the logfile.  In interactive mode, the user may select a different peak with
the 'z' keystroke, and the maximum peak within the specified \fIwindow\fR
(centered on the cursor) will be fit.  The user has full control in interactive
mode over the points used in the fit.  Once the endpoints of the peak have
been selected, the actual data points are shown with '+' signs on the ccf,
the fitted curve drawn, and a horizontal bar showing the location of the
FWHM calculation is displayed.  The status line will show a summary of the 
fit, and the user may type the 'v' keystroke for a more detailed description
of the fit and correlation. 

.ih
EXAMPLES
.nf
    1. Cross correlate a list of 1-dimensional object spectra against
    three 1-dimensional template spectra, saving results automatically
    and not continuum subtracting or filtering the data:

	rv> rvxcor.interactive = no		# Do it in batch mode
	rv> rvxcor obj* temp1,temp2,temp3 autowrite+ continuum="no"
	>>> filter="no" output="results"

    2. Compute a velocity for a list of apertures in a 2-dimensional 
    multispec format object image, using only one aperture of a multispec
    image as a template:

	rv> rvxcor.interactive = no		# Do it in batch mode
	rv> rvxcor object.ms temp.ms[*,35:35] apertures="1-7,10,12-35"

    3. Compute a velocity by fitting a fixed number of points on the peak,
    using uniform weighting:

	rv> rvxcor obj temp width=8 weights=0.

    4. Compute a velocity by fitting a Gaussian to the points on the ccf
    peak above the 0.1 correlation level.  Constrain the number of points
    to be less than 15, and linearly decrease the weights:

	rv> rvxcor obj temp func="gaussian" width=INDEF height=0.1 
	>>> maxwidth=15 weights=1.

    5. Compute a velocity by fitting a Lorentzian to the peak, from the
    peak maximum to it's half power point:

	rv> rvxcor obj temp func-"lorentz" width=INDEF height=0.5 peak+
	>>> maxwidth=15 weights=1.

    6. Process a 1-dimensional object against a 1-dimensional template
    interactively, examining the FFT, and input spectra to define a sample
    region for the correlation:

	rv> rvxcor obj temp inter+ continuum="both" autowrite- output=""
	    Screen is cleared and ccf peak with fit displayed

	... to refit peak, move cursor to left side of peak and type 'g'
	... move cursor to right side of peak and hit any key

	    New fit is drawn and results displayed to the status line

	... type the 'v' key for a detailed description of the correlation

	    Graphics are suspended and the text screen shows various
	    parameters of the correlation and fit. 

	... type 'q' to get back to graphics mode

	... to examine the FFT's of the spectra, type the 'f' keystroke.

	    The screen is cleared and a split plot of the two power spectra
	    after filtyering is drawn with the requested filter overlayed.
	... type the 'f' keystroke
	    The screen is cleared and the absolute value of the two FFT's
	    after filtering is plotted, again with the filter overlayed.
	... type ":overlay no", followed by a 'g' keystroke
	    The spectra are redrawn prior to filtering, with no filter over-
	    lay
	... type 'q' to return to correlation mode

	    The screen is redrawn with the ccf plot and peak fit

	... type 's' to enter spectrum mode

	    The screen is cleared and the the input spectra displayed
	... type 's' to mark the endpoints of a sample region for correl-
	... ation.  Then type 'q' to quit this mode

	    A new correlation is computed and the peak refit automatically

	... type 'q' to quit the task, satisfied with the results
	    The user is asked whether he wants to save results
	... type 'y' or <cr> to save results
	    The user is prompted for an output file name since one wasn't
	    specified in the parameter set
	... type in a file name
	 
	    The task exits.
.fi

.ih
TIME REQUIREMENTS
To be determined

.ih
SEE ALSO
continpars, filterpars, observatory, rvkeywords, center1d
.bp
.endhelp
