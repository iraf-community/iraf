.LP
\0
.de XS
.DS
.ps -1
.vs -1p
.ft CB
..
.de XE
.DE
.ft R
.ps
.vs
..
.de YS
.nf
.ps -1
.vs -1p
.ft CB
..
.de YE
.fi
.ft R
.ps
.vs
..
.RP
.TL
A Reference Guide to the IRAF/DAOPHOT Package
.AU
Lindsey E. Davis
.AI
IRAF Programming Group
.K2
.ce
.TU
.br
.ce
January 1994
.AB
.PP
DAOPHOT is a software package for doing stellar photometry in crowded stellar
fields
developed by Peter Stetson (1987) of the Dominion Astrophysical
Observatory. IRAF/DAOPHOT uses the task structure and
algorithms of DAOPHOT to do crowded-field stellar photometry within the
IRAF data reduction and analysis environment.
.PP
This document briefly describes the principal similarities and differences
between DAOPHOT and IRAF/DAOPHOT, the data preparation required to
successfully use IRAF/DAOPHOT, how to examine and edit the IRAF/DAOPHOT
algorithm parameters, how to run the IRAF/DAOPHOT package tasks interactively,
non-interactively, or in the background, and how to examine
and perform simple database operations on the output photometry files.
.PP
This document is
intended as a reference guide to the details of using and
interpreting the results of IRAF/DAOPHOT not a user's cookbook or a general
guide to doing photometry in IRAF. Its goal is to take the user
from a fully reduced image of a crowded stellar field to aperture
corrected instrumental magnitudes using a small artificial image as a
sample data set.
First time IRAF/DAOPHOT users
should consult \fIA User's Guide to Stellar Photometry With IRAF\fR, by
Phil Massey and Lindsey Davis. Detailed descriptions of the DAOPHOT photometry
algorithms can be found in Stetson (1987, 1990, 1992).
.AE
.ds CH
.bp
\0
.bp
.PP
.na
.LP
\fBContents\fP
.sp 1
1.\h'|0.4i'\fBIntroduction\fP\l'|5.6i.'\0\01
.sp
2.\h'|0.4i'\fBDAOPHOT and IRAF/DAOPHOT\fP\l'|5.6i.'\0\01
.sp
3.\h'|0.4i'\fBPreparing Data for DAOPHOT\fP\l'|5.6i.'\0\03
.sp
4.\h'|0.4i'\fBSome IRAF Basics for New IRAF and DAOPHOT Users\fP\l'|5.6i.'\0\04
.br
.sp
\h'|0.4i'4.1.\h'|0.9i'\fBPre-loaded Packages\fP\l'|5.6i.'\0\04
.br
\h'|0.9i'4.1.1.\h'|1.5i'The DATAIO Package\l'|5.6i.'\0\05
.br
\h'|0.9i'4.1.2.\h'|1.5i'The PLOT Package\l'|5.6i.'\0\05
.br
\h'|0.9i'4.1.3.\h'|1.5i'The IMAGES Package\l'|5.6i.'\0\05
.br
\h'|0.9i'4.1.4.\h'|1.5i'The TV Package\l'|5.6i.'\0\05
.br
\h'|0.4i'4.2.\h'|0.9i'\fBOther Useful Packages and Tasks\fP\l'|5.6i.'\0\05
.br
\h'|0.4i'4.3.\h'|0.9i'\fBImage Types, Image Directories, and Image Headers\fP\l'|5.6i.'\0\05
.br
\h'|0.4i'4.4.\h'|0.9i'\fBThe Image Display and Image Cursor\fP\l'|5.6i.'\0\06
.br
\h'|0.4i'4.5.\h'|0.9i'\fBThe Graphics Device and Graphics Cursor\fP\l'|5.6i.'\0\07
.sp
5.\h'|0.4i'\fBSome DAOPHOT Basics for New DAOPHOT Users\fP\l'|5.6i.'\0\08
.br
.sp
\h'|0.4i'5.1.\h'|0.9i'\fBLoading the DAOPHOT Package\fP\l'|5.6i.'\0\08
.br
\h'|0.4i'5.2.\h'|0.9i'\fBLoading the TABLES Package\fP\l'|5.6i.'\0\08
.br
\h'|0.4i'5.3.\h'|0.9i'\fBRunning the Test Script\fP\l'|5.6i.'\0\08
.br
\h'|0.4i'5.4.\h'|0.9i'\fBOn-line Help\fP\l'|5.6i.'\0\09
.br
\h'|0.4i'5.5.\h'|0.9i'\fBEditing the Package Parameters\fP\l'|5.6i.'\010
.br
\h'|0.4i'5.6.\h'|0.9i'\fBEditing the Task Parameters\fP\l'|5.6i.'\011
.br
\h'|0.4i'5.7.\h'|0.9i'\fBInput and Output Image Names\fP\l'|5.6i.'\011
.br
\h'|0.4i'5.8.\h'|0.9i'\fBInput and Output File Names\fP\l'|5.6i.'\012
.br
\h'|0.4i'5.9.\h'|0.9i'\fBAlgorithm Parameter Sets\fP \l'|5.6i.'\012
.br
\h'|0.4i'5.10.\h'|0.9i'\fBInteractive Mode and Non-Interactive Mode\fP \l'|5.6i.'\014
.br
\h'|0.4i'5.11.\h'|0.9i'\fBImage and Graphics Cursor Input\fP\l'|5.6i.'\014
.br
\h'|0.4i'5.12.\h'|0.9i'\fBGraphics Output\fP\l'|5.6i.'\015
.br
\h'|0.4i'5.13.\h'|0.9i'\fBVerify, Update, and Verbose\fP\l'|5.6i.'\015
.br
\h'|0.4i'5.14.\h'|0.9i'\fBBackground Jobs\fP\l'|5.6i.'\015
.br
\h'|0.4i'5.15.\h'|0.9i'\fBTiming Tests\fP\l'|5.6i.'\016
.sp
6.\h'|0.4i'\fBDoing Photometry with DAOPHOT\fP\l'|5.6i.'\016
.br
.sp
\h'|0.4i'6.1.\h'|0.9i'\fBThe Test Image\fP\l'|5.6i.'\016
.br
\h'|0.4i'6.2.\h'|0.9i'\fBTypical Analysis Sequence\fP\l'|5.6i.'\017
.br
\h'|0.4i'6.3.\h'|0.9i'\fBCreating and Organizing an Analysis Directory\fP\l'|5.6i.'\019
.br
\h'|0.4i'6.4.\h'|0.9i'\fBReading the Data\fP \l'|5.6i.'\019
.br
\h'|0.4i'6.5.\h'|0.9i'\fBEditing the Image Headers\fP\l'|5.6i.'\019
.br
\h'|0.9i'6.5.1.\h'|1.5i'The Minimum Image Header Requirements\l'|5.6i.'\019
.br
\h'|0.9i'6.5.2.\h'|1.5i'The Effective Gain and Readout Noise\l'|5.6i.'\019
.br
\h'|0.9i'6.5.3.\h'|1.5i'The Maximum Good Data Value\l'|5.6i.'\021
.br
\h'|0.9i'6.5.4.\h'|1.5i'The Effective Exposure Time\l'|5.6i.'\022
.br
\h'|0.9i'6.5.5.\h'|1.5i'The Airmass, Filter Id, and Time of Observation\l'|5.6i.'\022
.br
\h'|0.9i'6.5.6.\h'|1.5i'Batch Header Editing\l'|5.6i.'\024
.br
\h'|0.4i'6.6.\h'|0.9i'\fBEditing, Checking, and Storing the Algorithm Parameters\fP\l'|5.6i.'\024
.br
\h'|0.9i'6.6.1.\h'|1.5i'The Critical Algorithm Parameters\l'|5.6i.'\024
.br
\h'|0.9i'6.6.2.\h'|1.5i'Editing the Algorithm Parameters Interactively with Daoedit \l'|5.6i.'\024
.br
\h'|1.5i'6.6.2.1.\h'|2.2i'The Data Dependent Algorithm Parameters \l'|5.6i.'\025
.br
\h'|1.5i'6.6.2.2.\h'|2.2i'The Centering Algorithm Parameters\l'|5.6i.'\028
.br
\h'|1.5i'6.6.2.3.\h'|2.2i'The Sky Fitting Algorithm Parameters\l'|5.6i.'\029
.br
\h'|1.5i'6.6.2.4.\h'|2.2i'The Aperture Photometry Parameters\l'|5.6i.'\029
.br
\h'|1.5i'6.6.2.5.\h'|2.2i'The Psf Modeling and Fitting Parameters\l'|5.6i.'\030
.br
\h'|1.5i'6.6.2.6.\h'|2.2i'Setting the Algorithm Parameters Graphically\l'|5.6i.'\031
.br
\h'|0.9i'6.6.3.\h'|1.5i'Checking the Algorithm Parameters with Daoedit\l'|5.6i.'\031
.br
\h'|0.9i'6.6.4.\h'|1.5i'Storing the Algorithm Parameter Values with Setimpars\l'|5.6i.'\032
.br
\h'|0.9i'6.6.5.\h'|1.5i'Restoring the Algorithm Parameter Values with Setimpars\l'|5.6i.'\032
.br
\h'|0.4i'6.7.\h'|0.9i'\fBCreating a Star List\fP\l'|5.6i.'\032
.br
\h'|0.9i'6.7.1.\h'|1.5i'The Daofind Task\l'|5.6i.'\033
.br
\h'|1.5i'6.7.1.1.\h'|2.2i'The Daofind Algorithm\l'|5.6i.'\033
.br
\h'|1.5i'6.7.1.2.\h'|2.2i'The Daofind Algorithm Parameters\l'|5.6i.'\033
.br
\h'|1.5i'6.7.1.3.\h'|2.2i'Running Daofind Non-Interactively\l'|5.6i.'\034
.br
\h'|1.5i'6.7.1.4.\h'|2.2i'Running Daofind Interactively\l'|5.6i.'\034
.br
\h'|1.5i'6.7.1.5.\h'|2.2i'The Daofind Output\l'|5.6i.'\036
.br
\h'|1.5i'6.7.1.6.\h'|2.2i'Examining the Daofind Output\l'|5.6i.'\037
.br
\h'|0.9i'6.7.2.\h'|1.5i'Rgcursor and Rimcursor\l'|5.6i.'\038
.br
\h'|0.9i'6.7.3.\h'|1.5i'User Program\l'|5.6i.'\039
.br
\h'|0.9i'6.7.4.\h'|1.5i'Modifying an Existing Coordinate List\l'|5.6i.'\039
.br
\h'|0.4i'6.8.\h'|0.9i'\fBInitializing the Photometry with Phot\fP\l'|5.6i.'\039
.br
\h'|0.9i'6.8.1.\h'|1.5i'The Phot Algorithm\l'|5.6i.'\039
.br
\h'|0.9i'6.8.2.\h'|1.5i'The Phot Algorithm Parameters\l'|5.6i.'\040
.br
\h'|0.9i'6.8.3.\h'|1.5i'Running Phot Non-interactively\l'|5.6i.'\040
.br
\h'|0.9i'6.8.4.\h'|1.5i'Running Phot Interactively\l'|5.6i.'\042
.br
\h'|0.9i'6.8.5.\h'|1.5i'The Phot Output\l'|5.6i.'\043
.br
\h'|0.9i'6.8.6.\h'|1.5i'Examining the Results of Phot\l'|5.6i.'\044
.br
\h'|0.4i'6.9.\h'|0.9i'\fBCreating a Psf Star List with Pstselect\fP\l'|5.6i.'\044
.br
\h'|0.9i'6.9.1.\h'|1.5i'The Pstselect Algorithm\l'|5.6i.'\045
.br
\h'|0.9i'6.9.2.\h'|1.5i'The Pstselect Algorithm Parameters\l'|5.6i.'\045
.br
\h'|0.9i'6.9.3.\h'|1.5i'How Many Psf Stars Should Be Selected ?\l'|5.6i.'\046
.br
\h'|0.9i'6.9.4.\h'|1.5i'Running Pstselect Non-interactively\l'|5.6i.'\047
.br
\h'|0.9i'6.9.5.\h'|1.5i'Running Pstselect Interactively\l'|5.6i.'\047
.br
\h'|0.9i'6.9.6.\h'|1.5i'The Pstselect Output\l'|5.6i.'\048
.br
\h'|0.9i'6.9.7.\h'|1.5i'Examining and/or Editing the Results of Pstselect\l'|5.6i.'\048
.br
\h'|0.4i'6.10.\h'|0.9i'\fBComputing the Psf Model with Psf\fP\l'|5.6i.'\049
.br
\h'|0.9i'6.10.1.\h'|1.5i'The Psf Algorithm\l'|5.6i.'\049
.br
\h'|0.9i'6.10.2.\h'|1.5i'Choosing the Appropriate Analytic Function\l'|5.6i.'\050
.br
\h'|0.9i'6.10.3.\h'|1.5i'The Analytic Psf Model\l'|5.6i.'\050
.br
\h'|0.9i'6.10.4.\h'|1.5i'The Empirical Constant Psf Model\l'|5.6i.'\051
.br
\h'|0.9i'6.10.5.\h'|1.5i'The Empirical Variable Psf Model\l'|5.6i.'\051
.br
\h'|0.9i'6.10.6.\h'|1.5i'Rejecting Bad Data from the Psf Model\l'|5.6i.'\051
.br
\h'|0.9i'6.10.7.\h'|1.5i'The Model Psf Psfrad and Fitrad\l'|5.6i.'\052
.br
\h'|0.9i'6.10.8.\h'|1.5i'Modeling the Psf Interactively Without a Psf Star List\l'|5.6i.'\052
.br
\h'|0.9i'6.10.9.\h'|1.5i'Fitting the Psf Model Interactively Using an Initial Psf Star List\l'|5.6i.'\054
.br
\h'|0.9i'6.10.10.\h'|1.5i'Fitting the Psf Model Interactively Without an Image Display\l'|5.6i.'\055
.br
\h'|0.9i'6.10.11.\h'|1.5i'Fitting the Psf Model Non-interactively\l'|5.6i.'\056
.br
\h'|0.9i'6.10.12.\h'|1.5i'The Output of Psf\l'|5.6i.'\057
.br
\h'|0.9i'6.10.13.\h'|1.5i'Checking the Psf Model\l'|5.6i.'\059
.br
\h'|0.9i'6.10.14.\h'|1.5i'Removing Bad Stars from the Psf Model\l'|5.6i.'\062
.br
\h'|0.9i'6.10.15.\h'|1.5i'Adding New Stars to a Psf Star Group\l'|5.6i.'\062
.br
\h'|0.9i'6.10.16.\h'|1.5i'Refitting the Psf Model With the New Psf Star Groups\l'|5.6i.'\062
.br
\h'|0.9i'6.10.17.\h'|1.5i'Computing the Final Psf Model\l'|5.6i.'\063
.br
\h'|0.9i'6.10.18.\h'|1.5i'Visualizing the Psf Model with the Seepsf Task\l'|5.6i.'\063
.br
\h'|0.9i'6.10.19.\h'|1.5i'Problems Computing the Psf Model\l'|5.6i.'\064
.br
\h'|0.4i'6.11.\h'|0.9i'\fBDoing Psf Fitting Photometry with Peak, Nstar, or Allstar\fP \l'|5.6i.'\065
.br
\h'|0.9i'6.11.1.\h'|1.5i'Fitting Single Stars with Peak\l'|5.6i.'\065
.br
\h'|1.5i'6.11.1.1.\h'|2.2i'The Peak Algorithm\l'|5.6i.'\065
.br
\h'|1.5i'6.11.1.2.\h'|2.2i'Running Peak \l'|5.6i.'\065
.br
\h'|1.5i'6.11.1.3.\h'|2.2i'The Peak Output\l'|5.6i.'\066
.br
\h'|0.9i'6.11.2.\h'|1.5i'Fitting Stars with Group, Grpselect, Nstar and Substar\l'|5.6i.'\067
.br
\h'|1.5i'6.11.2.1.\h'|2.2i'The Group and Nstar Algorithms\l'|5.6i.'\067
.br
\h'|1.5i'6.11.2.2.\h'|2.2i'Running Group, Grpselect, and Nstar\l'|5.6i.'\068
.br
\h'|1.5i'6.11.2.3.\h'|2.2i'The Nstar Output\l'|5.6i.'\070
.br
\h'|0.9i'6.11.3.\h'|1.5i'Fitting Stars With Allstar\l'|5.6i.'\071
.br
\h'|1.5i'6.11.3.1.\h'|2.2i'The Allstar Algorithm\l'|5.6i.'\071
.br
\h'|1.5i'6.11.3.2.\h'|2.2i'Running Allstar\l'|5.6i.'\072
.br
\h'|1.5i'6.11.3.3.\h'|2.2i'The Allstar Output\l'|5.6i.'\073
.br
\h'|0.4i'6.12.\h'|0.9i'\fBExamining the Output Photometry Files\fP\l'|5.6i.'\073
.br
\h'|0.4i'6.13.\h'|0.9i'\fBProblems with the Photometry\fP\l'|5.6i.'\074
.br
\h'|0.4i'6.14.\h'|0.9i'\fBDetecting Stars Missed By Daofind\fP\l'|5.6i.'\075
.br
\h'|0.4i'6.15.\h'|0.9i'\fBInitializing the Missing Star Photometry with Phot\fP\l'|5.6i.'\075
.br
\h'|0.4i'6.16.\h'|0.9i'\fBMerging Photometry Files with Pfmerge\fP\l'|5.6i.'\076
.br
\h'|0.4i'6.17.\h'|0.9i'\fBRefitting the Stars with Allstar\fP\l'|5.6i.'\076
.br
\h'|0.4i'6.18.\h'|0.9i'\fBExamining the Subtracted Image\fP\l'|5.6i.'\076
.br
\h'|0.4i'6.19.\h'|0.9i'\fBComputing an Aperture Correction\fP\l'|5.6i.'\076
.sp
7.\h'|0.4i'\fBReferences\fP\l'|5.6i.'\077
.sp
8.\h'|0.4i'\fBAppendices\fP\l'|5.6i.'\077
.br
.sp
\h'|0.4i'8.1.\h'|0.9i'\fBThe Instrumental Magnitude Scale\fP\l'|5.6i.'\077
.br
\h'|0.4i'8.2.\h'|0.9i'\fBThe Analytic Psf Models\fP\l'|5.6i.'\077
.br
\h'|0.4i'8.3.\h'|0.9i'\fBThe Error Model\fP\l'|5.6i.'\078
.br
\h'|0.4i'8.4.\h'|0.9i'\fBThe Radial Weighting Function\fP\l'|5.6i.'\078
.br
\h'|0.4i'8.5.\h'|0.9i'\fBTotal Weights\fP\l'|5.6i.'\078
.br
\h'|0.4i'8.6.\h'|0.9i'\fBBad Data Detection\fP\l'|5.6i.'\078
.br
\h'|0.4i'8.7.\h'|0.9i'\fBStellar Mergers\fP\l'|5.6i.'\079
.br
\h'|0.4i'8.8.\h'|0.9i'\fBFaint Stars\fP\l'|5.6i.'\079
.br
.bp
\0
.ds CH - % -
.bp 1
\0

.TL
A Reference Guide to the IRAF/DAOPHOT Package
.AU
Lindsey E. Davis
.AI
IRAF Programming Group
.K2
.ce
.TU
.br
.ce
January 1994

.NH
Introduction

.PP
DAOPHOT is a software package for doing stellar photometry
in crowded fields  developed by Peter Stetson of the DAO (1987, 1990, 1992).
The IRAF/DAOPHOT package uses the task structure and algorithms of DAOPHOT
to do crowded field photometry within the IRAF data reduction and
analysis environment.
.PP
Input to IRAF/DAOPHOT consists of an IRAF image file, numerous parameters
controlling the analysis algorithms and, optionally, graphics cursor and/or
image display cursor input. IRAF/DAOPHOT produces output photometry files
in either text format or STSDAS binary table format. Some IRAF/DAOPHOT tasks
also produce image output and graphics output in the form of plot metacode
files.
.PP
Separate tasks are provided for examining, editing, storing, and recalling
the analysis parameters, creating and editing star
lists, computing accurate centers, sky values and initial magnitudes
for the stars in the list, computing the point-spread function,
grouping the stars into physical associations, fitting the stars either
singly or in groups, subtracting the fitted stars from the original image,
and adding artificial test stars to the original image. A set of tools are
also provided for examining and editing the output photometry files.

.NH
DAOPHOT and IRAF/DAOPHOT

.PP
The principal similarities and differences between DAOPHOT and IRAF/DAOPHOT
are summarized below.
.IP [1]
The structure of IRAF/DAOPHOT is very similar to the
structure of DAOPHOT. All the DAOPHOT photometry tasks and many of
the utilities tasks are present in
IRAF/DAOPHOT and in many cases the DAOPHOT task names have been preserved.
A listing of the DAOPHOT photometry tasks and their closest IRAF/DAOPHOT
equivalents is shown below. 

.TS
l l
l l
l l.
DAOPHOT\tIRAF/DAOPHOT
TASK\tEQUIVALENT

add*\taddstar
allstar\tallstar
attach\tN/A
append\tpfmerge,pconcat
find\tdaofind
group\tgroup
monitor\tdaophot.verbose=yes
nomonitor\tdaophot.verbose=no
nstar\tnstar
offset\tpcalc
options\tdaoedit
peak\tpeak
photometry\tphot
pick\tpstselect
psf\tpsf
select\tgrpselect
sort\tpsort,prenumber
sub*\tsubstar
.TE

.IP [2]
Some DAOPHOT utilities tasks are missing from IRAF/DAOPHOT.
The DAOPHOT tasks \fBdump\fR, \fBexit\fR, \fBfudge\fR,
\fBhelp\fR, \fBlist\fR, and \fBsky\fR
have been replaced with general IRAF tasks, or with IRAF system facilities
that perform the equivalent function. The missing DAOPHOT utilities tasks
and their IRAF equivalents are shown below.

.TS
l l
l l
l l.
DAOPHOT\tIRAF/DAOPHOT
TASK\tEQUIVALENT

dump\tlistpixels,imexamine
exit\tbye
fudge\timreplace,fixpix,imedit
help\thelp daophot
list\timheader
sky\timstatistics,phistogram,imexamine
.TE

.IP [3]
The IRAF/DAOPHOT default algorithms are the DAOPHOT II algorithms
(Stetson 1992).
.IP [4]
Users have more choice of and control over the algorithms
in IRAF/DAOPHOT than they do in DAOPHOT. For example the
IRAF/DAOPHOT aperture photometry task \fBphot\fR offers several
sky fitting algorithms besides the default "mode" algorithm,
and full control over the sky fitting algorithm parameters.
.IP [5]
The algorithm parameters in IRAF/DAOPHOT are grouped by function into
six parameter sets or psets rather than three as in DAOPHOT.
The six IRAF/DAOPHOT parameter sets with their DAOPHOT equivalents
in brackets are:
1) \fBdatapars\fR, the data definition parameters (daophot.opt),
2) \fBfindpars\fR, the detection algorithm parameters (daophot.opt),
3) \fBcenterpars\fR, the aperture photometry centering algorithm parameters
(no equivalent),
4) \fBfitskypars\fR, the aperture photometry sky fitting parameters (photo.opt),
5) \fBphotpars\fR, the aperture photometry parameters (photo.opt),
6) \fBdaopars\fR, the IRAF/DAOPHOT psf fitting parameters (daophot.opt,
allstar.opt).
.IP [6]
The IRAF/DAOPHOT algorithm parameter sets unlike the DAOPHOT parameter sets
can be interactively examined,
edited and saved with the \fBdaoedit\fR task using the image display
and radial profile plots.
.IP [7]
The IRAF/DAOPHOT algorithm parameter sets unlike the DAOPHOT parameter sets
can be saved and restored as a function of image using the \fBsetimpars\fR task.
.IP [8]
Memory allocation in IRAF/DAOPHOT is dynamic not static as in
DAOPHOT.  IRAF/DAOPHOT allocates and frees memory as required
at run-time subject to the physical memory and swap space limitations of
the host computer.
.IP [9]
The IRAF/DAOPHOT point-spread function look-up table is stored in an
IRAF image not an ASCII table as in DAOPHOT.
.IP [10] 
Unlike DAOPHOT, the IRAF/DAOPHOT tasks \fBdaofind\fR, \fBphot\fR,
\fBpstselect\fR
and \fBpsf\fR can be run interactively using the image display and graphics
window or non-interactively. Display and graphics capabilities were
deliberately omitted from DAOPHOT to minimize portability problems.
.IP [11]
The IRAF/DAOPHOT output photometry files can be written in either text
format as in DAOPHOT or STSDAS binary table format.
.IP [12]
Unlike DAOPHOT, fields or columns in both IRAF/DAOPHOT text and
STSDAS binary table photometry files are identified
by name and have an associated units and format specifier.
The IRAF/DAOPHOT photometry file input routines search for column
names, for example "GROUP,ID,XCENTER,YCENTER,MAG,MSKY" as
appropriate but are independent
of their placement in the input file.
.IP [13]
Several general purpose IRAF/DAOPHOT tasks are available for performing
operations on the final output photometry catalogs. In addition to 
\fBpcalc\fR, \fBpconcat\fR, \fBpfmerge\fR, \fBprenumber\fR,
and \fBpsort\fR which are
also available in DAOPHOT, there are three photometry file editing tasks which
have no analog in DAOPHOT \fBpdump\fR, \fBpexamine\fR, and \fBpselect\fR.
All these tasks work on IRAF/DAOPHOT output text files or STSDAS binary
tables. An IRAF/DAOPHOT task is supplied  for converting output text files to
STSDAS binary tables so as to make use of the even more general STSDAS
tables manipulation tools in the TABLES package.
.IP [14]
The IRAF/DAOPHOT output files are self-documenting.
All the information required to comprehend the history of or decode the
output photometry file is in the file itself, including the IRAF version
number, host computer, date, time, and names of all the
input and output files and the values of all the parameters.
.PP
For the remainder of this document IRAF/DAOPHOT will be referred to
as DAOPHOT.

.NH
Preparing Data for DAOPHOT

.IP [1]
DAOPHOT assumes that the images to be analyzed exist on disk in IRAF
image format. DAOPHOT can read and write old IRAF format ".imh" images
and ST IRAF format ".hhh" images.
When the IRAF FITS kernel becomes available DAOPHOT will be able
to read FITS images on disk as well.
QPOE IRAF format ".qp" images must be rasterized before they can
be input to DAOPHOT.
.IP [2]
All internal DAOPHOT calculations are done in real precision.
The pixel type of the image data on disk may be any of the following
data types: short integer, unsigned short integer, integer, long integer,
real or double. Users should realize that the extra precision in
images of type double will not be used by DAOPHOT.
.IP [3]
The instrumental signature must be removed from the input images
prior to running DAOPHOT.  All CCD images should be overscan
corrected, bias corrected, dark current corrected and flat-fielded.
Users should be aware of the IRAF CCDRED package for reducing CCD data.
.IP [4]
DAOPHOT assumes that the input pixel data is linear.
If the data is non-linear over a large fraction of its total dynamic range,
the data must be linearized before running DAOPHOT.
.IP [5]
Saturated pixels or pixels distinguishable from good data by intensity,
do not need to be removed from the image prior to running DAOPHOT.
For example if the data
is non-linear only above 25000 counts, DAOPHOT can be instructed to 
ignore pixels above 25000 counts.
.IP [6]
Extreme-valued pixels should be removed from the images prior to running
DAOPHOT. Extreme-valued pixels include those with values at or near
the floating point limits of the host machine and host machine special
numbers produced by operations like divide by zero, floating point
underflows and overflows, etc. The latter category of extreme-valued
pixels should not be produced by IRAF software, but may be produced by
user programs including imfort programs.
Floating point operations involving such numbers will frequently cause
arithmetic exception errors, since for efficiency and portability reasons
the DAOPHOT package and most IRAF tasks do not test for
their presence.
The \fBimreplace\fR task in the PROTO  package can be used to remove extreme-
valued pixels.
.IP [7]
The background sky value should NOT be subtracted from the image prior
to entering the DAOPHOT package. The DAOPHOT fitting routines use an optimal
weighting scheme which depends on the readout noise, the gain, and the
true counts in the pixels. If the mean sky has been subtracted
then the counts in the image are not the true counts and the computed weights
will be incorrect. For similar reasons users should not attempt to
correct their magnitudes for exposure time by dividing their images
by the exposure time.
.IP [8]
Cosmic ray and bad pixel removal programs should be used with caution. If the
data and parameter values are set such that the cosmic ray and bad pixel
detection and
removal algorithms have difficulty distinguishing between stars and bad
pixels or cosmic rays,
the peaks of the stars may be clipped, altering the point-spread function
and introducing errors into the photometry.
.IP [9]
DAOPHOT assumes that the local sky background is approximately flat in the
vicinity of the object being measured. This assumption is equivalent to
requiring that the local sky region have a unique mode. Variations
in the sky background which occur on the same scale as the size of the
local sky region will introduce errors into the photometry.
.IP [10]
The point spread function must be constant or smoothly
varying with position over the entire image. This is the fundamental
assumption
underlying all of DAOPHOT. All stars in the image must be indistinguishable
except for position and magnitude. The variable point spread function
option is capable of handling second order variability as a function of
position in the image.
.IP [11]
The input images should not have undergone any operations which fundamentally
alter the image point spread function or the image statistics in a non-linear
way. For example, non-linear image restoration tasks must not be run on
the image to prior to running DAOPHOT.
.IP [12]
The gain, readout noise, exposure time,
airmass, filter, and observing time should be present and correct in the
image headers before DAOPHOT reductions are begun.
DAOPHOT tasks can extract this information from the image headers, use it
in the computations, and/or store
it in the output photometry files, greatly simplifying the analysis
and subsequent calibration procedures.
.fi

.NH
Some IRAF Basics for New IRAF and DAOPHOT Users

.NH 2
Pre-loaded Packages

.PP
Under IRAF versions 2.10 and later the DATAIO, PLOT, IMAGES, TV and NOAO
packages are pre-loaded so that all the tasks directly under them are
available when
IRAF is started. Each of these packages contains tasks which are useful
to DAOPHOT users for various reasons, and each is discussed briefly below.

.NH 3
The DATAIO Package

.PP
DAOPHOT users should be aware of the DATAIO \fBrfits\fR and \fBwfits\fR tasks
which are used to transport data into and out of IRAF. Any input 
and output images, including point-spread function look-up table images,
should normally be archived with \fBwfits\fR.
The cardimage reader and writer tasks for archiving text files,
\fBrcardimage\fR and \fBwcardimage\fR, are also located here.

.NH 3
The PLOT Package

.PP
Various general purpose image and file plotting utilities can be found
in the PLOT packages. DAOPHOT users should be aware of the interactive image
row and column plotting task \fBimplot\fR, the image contour plotting task
\fBcontour\fR, the image surface plotting task \fBsurface\fR, image
histogram plotting task \fBphistogram\fR, the image radial profile
plotting task \fBpradprof\fR, and the general purpose graphing tool
\fBgraph\fR. The tasks \fBgkidir\fR and \fBgkiextract\fR are also useful
for extracting individual plots from the plot metacode files which may
be produced by some DAOPHOT tasks.

.NH 3
The IMAGES Package

.PP
The IMAGES package contains a set of general purpose image operators. DAOPHOT
users
should be aware of the image header examining tasks \fBimheader\fR and
\fBhselect\fR, the header editing task \fBhedit\fR, the coordinate and
pixel value dumping task \fBlistpixels\fR, and the image statistics
task \fBimstatistics\fR.

.NH 3
The TV Package

.PP
The TV package contains tasks which interact with the image display including
the all important \fBdisplay\fR task for displaying images, the
interactive image examining task \fBimexamine\fR, and the \fBtvmark\fR task
for marking objects on the image display. DAOPHOT users should become 
familiar with all three of these tasks.

.NH 2
Other Useful Packages and Tasks

.PP
The NPROTO package contains two useful tasks, \fBfindgain\fR,
for computing the gain and readout noise of a CCD
from a pair of biases and flats, and \fBfindthresh\fR for computing
the standard deviation of the background in a CCD frame given the
readout noise and gain. The ASTUTIL package contains the \fBsetairmass\fR
task for computing and/or correcting the airmass given the appropriate
input data. 
Users might also wish to experiment with the tasks in the artificial
data package ARTDATA, and run the resulting images through DAOPHOT.

.NH 2
Image Types, Image Directories, and Image Headers

.PP
The IRAF image environment is controlled by several
environment variables. The most important of these for DAOPHOT users
are: \fBimtype\fR the disk image format, \fBimdir\fR the default pixel
directory, and \fBmin_lenuserarea\fR the maximum length of the image header.
The values of these environment variables can be listed
as shown below.

.YS
cl> show imtype
imh
cl> show imdir
/data/davis/pixels/
cl> show min_lenuserarea
24000
.YE

.PP
\fB"imh"\fR is the default image format for most IRAF users, \fB"hhh"\fR the
default image format for ST users, and \fB"qp"\fR the photon counting format
used for photon counting data. DAOPHOT will work transparently on
"imh" and "hhh" images. "qp" event lists must be rasterized prior to using
DAOPHOT. When IRAF supports FITS images on disk, image format "fits", DAOPHOT
will be able to work directly on FITS images as well. IRAF uses the
image name extension, e.g. "imh" to automatically sense the image
disk format on input. The output disk format is set by: 1) the
extension of the output image name if present e.g. "imh", 2) the cl
environment variable \fBimtype\fR if the output image is opened as a new
image, e.g. the output of the \fBrfits\fR task, 3) the type of the input
image if the output image is opened as a new copy of an existing image,
e.g. the output of the \fBimcopy\fR task.
.PP
\fBimdir\fR specifies the default image pixel directory for "imh" format
files. The image header files are written to the current directory
and the pixel files are written to imdir. imdir can be set
to an existing directory on a scratch disk, the current
directory "HDR$", or the subdirectory pixels under the current
directory "HDR$pixels/". DAOPHOT users should keep both the intrinsic
speed of a disk and its network configuration in mind when setting
imdir.
.PP
\fBmin_lenuserarea\fR is the size of the image header area reserved
in memory when a new or existing image is opened.
The current default value of 24000 corresponds to space for approximately
300 keywords.
If an image on disk has a header larger than this the image header will
be truncated when it is read.
For most DAOPHOT users the default value is sufficient. However users whose
images have large headers or who are
creating a point-spread function using more than ~70 stars should set
min_lenuserarea to a larger value, e.g. 40000.
.PP
The following example shows how to change the default pixel directory to
HDR$pixels/ and set min_lenuserarea to 40000.  To avoid redefining these
quantities for every session, users should enter the redefinitions into
their login.cl or loginuser.cl files.


.YS
cl> reset imdir = "HDR$pixels/"
cl> reset min_lenuserarea = 40000
.YE

.NH 2
The Image Display and Image Cursor

.PP
Several DAOPHOT tasks are interactive tasks or have an interactive as well
as a non-interactive mode. In interactive mode these tasks must be able to
read the image cursor on a displayed image and perform various
actions depending on the position of the image cursor and the keystroke
command typed.
.PP
DAOPHOT will work with the display servers Imtool, Saoimage, and Ximtool.
DAOPHOT users should be aware that both Imtool and Ximtool support multiple
frame buffers while SAOimage does not. Multiple frame buffers  are an
important feature for users who wish to compare their original
images with the DAOPHOT output images from which all the fitted
stars have been subtracted. Users running DAOPHOT on a remote machine, e.g.
one with lots of memory and/or disk space, but displaying on their local
machine also need to set the \fBnode\fR environment variable to
the name of the local machine.

.YS
cl> show node
ERROR: No such environment variable
    show (node)
cl> set node = mymachine
.YE

.PP
The maximum size of the display server frame buffer is defined by the
environment variable \fBstdimage\fR whose value can be printed as
shown below.

.YS
cl> show stdimage
imt512
.YE

In the previous example the default frames buffers are 512 pixels square.
A user whose images are 2K square will want to reset the default frame
buffer size as shown below.

.YS
cl> reset stdimage = imt2048
cl> show stdimage
imt2048
.YE

.PP
In order for image cursor read-back to function correctly the environment
variable \fBstdimcur\fR must be set to "stdimage" as shown below.

.YS
cl> show stdimcur
stdimage
.YE

To check that image cursor read-back is functioning correctly the user
should display an image and try to bring up the image display cursor
as shown below.

.YS
cl> display image 1
cl> =imcur
.YE

The image cursor should appear on the image display reading the correct
image pixel coordinates and ready to accept a
keystroke command. Any keystroke will terminate the cursor read.

.NH 2
The Graphics Device and Graphics Cursor

.PP
Some interactive DAOPHOT tasks have graphics submenus which require
them to be able to read the graphics cursor on for example a radial
profile plot and perform various
actions based on the position of the graphics cursor in the
plot and the keystroke
command issued. The default graphics device is determined by
the \fBstdgraph\fR environment variable as shown below.

.YS
cl> show stdgraph
xgterm
.YE

To check that graphics cursor read-back is functioning correctly the user
should draw a plot and try to bring up the graphics cursor as
shown below.

.YS
cl> contour image
cl> =gcur
.YE

The graphics cursor should appear in the graphics window ready to accept a
keystroke command. Any keystroke will terminate the cursor read.

.NH
Some DAOPHOT Basics for New DAOPHOT Users

.NH 2
Loading the DAOPHOT Package

.PP
The DAOPHOT package is located in the digital stellar photometry package
DIGIPHOT. To load DIGIPHOT and DAOPHOT the user types the package names
in sequence as shown below,

.YS
cl> digiphot
di> daophot
.YE

after which the following menu of tasks appears.

.YS
addstar       daotest       nstar         pexamine      psf
allstar       datapars@     pcalc         pfmerge       psort
centerpars@   findpars@     pconcat       phot          pstselect
daoedit       fitskypars@   pconvert      photpars@     seepsf
daofind       group         pdump         prenumber     setimpars
daopars@      grpselect     peak          pselect       substar
.YE

Task names with a trailing "@" are parameter set tasks.
The remaining tasks are script and/or compiled tasks.
After the DAOPHOT package is loaded the user can redisplay 
the package menu at any time with the command.

.YS
da> ? daophot
.YE

.NH 2
Loading the TABLES Package

.PP
The DAOPHOT photometry tasks write their output photometry files in 
either text format (the default) or ST binary tables format. Users wishing
to use the ST binary tables format should acquire and install
the ST TABLES external package. Without the TABLES package the DAOPHOT
photometry tasks will read and write ST binary tables, but DAOPHOT
utilities like \fBpsort\fR which call TABLES package
tasks will not run on ST binary tables.
.PP
When DAOPHOT is loaded, it checks to see if the TABLES package is defined,
and if so loads it. A warning message is issued if the TABLES package is
undefined. The TABLES package tasks can be listed at any time after DAOPHOT
is loaded with the following command.

.YS
da> ? tables
.YE

.NH 2
Running the Test Script

.PP
The DAOPHOT package includes a script task \fBdaotest\fR which
executes each of the core DAOPHOT photometry tasks in turn using a test
image stored
in  FITS format in the DAOPHOT test directory. \fBDaotest\fR is run as
shown below.

.YS
da> daotest

DAOTEST INITIALIZES THE DAOPHOT TASK PARAMETERS
TYPE 'q' or 'Q' TO QUIT, ANY OTHER KEY TO PROCEED

Name of the output test image: test

INITIALIZE THE DAOPHOT PACKAGE

TESTING THE DAOFIND TASK
TESTING THE PHOT TASK
TESTING THE PSTSELECT TASK
TESTING THE PSF TASK
TESTING THE PEAK TASK
TESTING THE GROUP TASK
TESTING THE GRPSELECT TASK
TESTING THE NSTAR TASK
TESTING THE ALLSTAR TASK (CACHE=YES)
TESTING THE ALLSTAR TASK (CACHE=NO)
TESTING THE SUBSTAR TASK
TESTING THE ADDSTAR TASK

DAOPHOT PACKAGE TESTS COMPLETED
.YE

On task completion the user will find the input image in
test.imh, the psf image in test.psf.1.imh, the subtracted image produced
by \fBallstar\fR in test.sub.1.imh, the input image with artificial stars
added in test.add.1.imh, copies of all the output photometry files in
test.log, and copies of the plots produced by the \fBpsf\fR task
in test.plot on disk.
.PP
Users should be aware that the \fBdaotest\fR task will reset the DAOPHOT
task and algorithm parameters to their default values before and after it
is executed.

.NH 2
On-line Help

.PP
A one-line description of each DAOPHOT task can be obtained by typing
the following command,

.YS
da> help daophot\fR
.YE

upon which the following package menu appears.

.YS
digiphot.daophot:
   addstar - Add stars to an image using the computed psf
   allstar - Group and fit psf to multiple stars simultaneously
centerpars - Edit the centering algorithm parameters
   daoedit - Review/edit algorithm parameters interactively
   daofind - Find stars in an image using the DAO algorithm
   daopars - Edit the daophot algorithms parameter set
   daotest - Run basic tests on the daophot package tasks
  datapars - Edit the image data dependent parameters
  findpars - Edit the star detection parameters
fitskypars - Edit the sky fitting algorithm parameters
     group - Group stars based on position and signal/noise
     nstar - Fit the psf to predefined groups of stars
      peak - Fit the psf to single stars
      phot - Compute skies and initial magnitudes for a star list
  photpars - Edit the aperture photometry parameters
       psf - Compute the point spread function
    seepsf - Compute an image from the point spread function
 setimpars - Save/restore parameter sets for a particular image
   substar - Subtract the fitted stars from the original image

     pcalc - Do arithmetic operations on list of daophot databases
   pconcat - Concatenate a list of daophot databases
  pconvert - Convert a text database to a tables database
     pdump - Print selected fields from daophot databases
   pfmerge - Merge a list of photometry databases
 pstselect - Select candidate psf stars based on proximity
 grpselect - Select groups from a daophot database
  pexamine - Interactively examine and edit a daophot database
 prenumber - Renumber stars in a daophot database
   pselect - Select records from a daophot database
     psort - Sort a daophot database\fR
.YE

.PP
All the DAOPHOT tasks have on-line manual pages which can be
listed on the terminal. The following command lists the help for the
\fBphot\fR task on the terminal.

.YS
da> phelp phot\fR
.YE

Any section of the manual pages can be listed individually.
For example the examples section of the \fBphot\fR manual page can be
listed as follows.

.YS
da> phelp phot sections=examples\fR
.YE

The help page for \fBphot\fR can be piped to the local default printer as
follows.

.YS
da> phelp phot | lprint\fR
.YE

Finally the manual pages for the whole DAOPHOT package can be printed
by typing.

.YS
da> phelp daophot.* | lprint\fR
.YE


.NH 2
Editing the Package Parameters

.PP
DAOPHOT has a package parameter set which defines the DAOPHOT
package environment. The DAOPHOT package parameters can edited
with epar as shown below.

.YS
da> epar daophot
.YE

.YS
Image Reduction and Analysis Facility
  PACKAGE = digiphot
     TASK = daophot
 (version = "Dec92")
    (text = yes)            Text file on output ?
  (verify = yes)            Verify critical parameters ?
  (update = no)             Update critical parameters ?
 (verbose = yes)            Print verbose output ?
(graphics = "stdgraph")     Default graphics device
 (display = "stdimage")     Default display device
    (mode = "ql")
.YE

To edit a parameter simply move the cursor to the parameter in question,
enter the new value, type return, and finally type \fB:wq\fR to quit and
update the parameter set. Package parameters can also be edited on the
command line as shown below.

.YS
da> daophot.text = yes
.YE

.PP
The DAOPHOT package parameters control the operation of the DAOPHOT package
as a whole.  For example the \fBtext\fR parameter specifies whether the
output photometry files will be written in text or STSDAS binary tables format,
the parameters \fBverify\fR, \fBupdate\fR, and \fBverbose\fR determine
the default mode of operation of the DAOPHOT package tasks, and the parameters
\fBgraphics\fR and \fBdisplay\fR determine the default graphics and display
devices for the entire package.

.NH 2
Editing the Task Parameters

.PP
The DAOPHOT task level parameters specify the input and output images and
files, the algorithm parameter sets, the graphics and image display input and
output devices, and the mode of operation of each DAOPHOT task.
.PP
To enter and edit the parameter set for the DAOPHOT \fBphot\fR task
the user types the following command,

.YS
cl> epar phot
.YE

after which the parameter set for the \fBphot\fR task appears on the
terminal ready for editing as shown below.

.YS
Image Reduction and Analysis Facility
PACKAGE = daophot
   TASK = phot

image   =                       Input image(s)
coords  =              default  Input coordinate list(s)
output  =              default  Output photometry file(s)
skyfile =                       Input sky value file(s)
(plotfil=                     ) Output plot metacode file
(datapar=                     ) Data dependent parameters
(centerp=                     ) Centering parameters
(fitskyp=                     ) Sky fitting parameters
(photpar=                     ) Photometry parameters
(interac=                   no) Interactive mode ?
(radplot=                   no) Plot the radial profiles?
(verify =            )_.verify) Verify critical phot parameters ?
(update =            )_.update) Update critical phot parameters ?
(verbose=           )_.verbose) Print phot messages ?
(graphic=          )_.graphics) Graphics device
(display=           )_.display) Display device
(icomman=                     ) Image cursor: [x y wcs] key [cmd]
(gcomman=                     ) Graphics cursor: [x y wcs] key [cmd]
(mode   =                   ql)
.YE

The \fBphot\fR parameters can be edited by moving
the cursor to the line opposite the parameter name, entering the new value
followed by a carriage return, and typing \fB:wq\fR to exit the
\fBepar\fR task and update the parameters.
.PP
In the following sections the \fBphot\fR task is used to illustrate
some general features of the DAOPHOT package.

.NH 2
Input and Output Image Names

.PP
The \fBphot\fR parameter \fIimage\fR
defines the image to be analyzed. The
root image name, the value of \fIimage\fR 
stripped of directory and section information,
sets up the default input and output image naming convention for the task.
Users should avoid appending the ".imh" or ".hhh" extension
to their image name specification as these extensions are not required by IRAF
image i/o and become part of the default output image names.
.PP
The \fBphot\fR task does not create an output image but DAOPHOT tasks
which do, will by default create an output image name of the form
"image.extension.?" where image is the input image name
stripped of directory
and section information, extension is an id appropriate
to the task, and ? is the next available version number.
For example the first run of the \fBsubstar\fR task on the image "image"
will create an image called "image.sub.1", the second an image
called "image.sub.2", and so on.  The default output image naming convention
can always be overridden by the user in any task.

.NH 2
Input and Output File Names

.PP
DAOPHOT uses a default input and output file naming convention based on the
root image name or the input image name  with the directory and
section specification removed.  Users should avoid appending the ".imh" or
".hhh" extension to their input image name specification as these extensions
are not required by IRAF image i/o and become part of the default input
and output file names.
.PP
If a DAOPHOT task expects its input to have been written
by another DAOPHOT task, and the input file parameter value is "default",
the task will search for an existing
file called "image.extension.?" where image is the root image
name, extension identifies the task expected to have written the file,
and version is the highest version number for that file. For example,
if the user sets the \fBphot\fR parameters \fIimage\fR and
\fIcoords\fR to "m92b" and "default", \fBphot\fR will search
for a coordinate file called "m92b.coo.#" written by the
\fBdaofind\fR task. The default input file naming convention
can be over-ridden by the user at any point.
.PP
The output file naming convention works
in an identical manner to the input file naming convention,
although in this situation ? is the next available
version number. For example if the user sets the \fBphot\fR task
parameter \fIoutput\fR to "default", the output photometry file name
will be "image.mag.?"
where ? is 1 for the first run of \fBphot\fR, 2 for the second run, and so
on.  The default output file naming convention can be over-ridden
by the user at any point.

.NH 2
Algorithm Parameter Sets 

.PP
The DAOPHOT parameters have been grouped together into parameter sets
or psets.
The use of psets encourages the logical grouping of parameters, permits
the various DAOPHOT tasks to share common parameters, and
permits the user to optionally store the DAOPHOT algorithm parameters
with the data rather than in the default uparm directory. 
.PP
Six DAOPHOT psets, \fBdatapars\fR, \fBfindpars\fR, \fBcenterpars\fR,
\fBfitskypars\fR, \fBphotpars\fR and \fBdaopars\fR
control the DAOPHOT algorithm parameters. The \fBphot\fR task 
uses four of them, \fBdatapars\fR which specifies data dependent
parameters like \fIfwhmpsf\fR (the full-width half-maximum of the psf),
\fIsigma\fR (the standard deviation of
the sky background), \fIepadu\fR and \fIreadout noise\fR
(the gain and readout noise of the detector),
and the \fBcenterpars\fR, \fBfitskypars\fR and \fBphotpars\fR parameter
sets which define the centering algorithm, sky fitting algorithm
and aperture photometry algorithm parameters respectively,
used by phot to compute initial centers, sky values,
and initial magnitudes for the stars to be analyzed. The \fBfindpars\fR pset
controls the star detection algorithm parameters used by the \fBdaofind\fR
task. The \fBdaopars\fR pset defines the psf model fitting
and evaluation parameters including the radius of the psf, the fitting radius,
and the grouping parameters used by all the psf fitting tasks.
.PP
By default the pset parameters can be examined, edited and stored 
in the user's uparm directory, in the same manner as the task level
parameters. For example to list the current \fBdatapars\fR
pset the user types.

.YS
da> lpar datapars
.YE

To edit the \fBdatapars\fR parameter set, the user types either

.YS
da> epar datapars

or

da> datapars
.YE

and edits the parameter set in the usual manner with \fBepar\fR.
All the DAOPHOT tasks which reference this
pset will pick up the changes from the uparm directory, assuming
that the \fIdatapars\fR parameter is specified as  "" in the calling task.
The user can also edit the \fBdatapars\fR
pset from within the \fBphot\fR
task or any other task which calls it as shown below.

.YS
da> epar phot
.YE

Move the cursor to the \fBdatapars\fR parameter line and type \fB:e\fR.
The menu for the 
\fBdatapars\fR pset will appear ready for editing. Edit the desired
parameters and type \fB:wq\fR. \fBEpar\fR will return to the main
\fBphot\fR parameter set after which other psets or the main task parameters
can be edited.
.PP
Psets may also be stored in user files providing
a mechanism for saving a particular pset
with the data.
The example below shows how to store a  pset in a file in the same directory
as the data and recall it for use by the \fBphot\fR task. The user types

.YS
da> epar phot
.YE

as before, enters the \fBdatapars\fR menu with \fB:e\fR and edits the
parameters.  The command \fB:w data1.par\fR
writes the parameter set to a file called "data1.par" and a \fB:q\fR
returns to the main task menu.
A file called "data1.par" containing the new \fBdatapars\fR parameters
is written in the current directory. At this point the user is still in the
\fBphot\fR parameter set at the line opposite \fBdatapars\fR. He/she
enters "data1.par" on the line opposite this parameter.
The next time \fBphot\fR is run the parameters will
be read from "data1.par" not from the pset in the uparm directory.
The new parameter set can be edited in the usual way by typing

.YS
da> epar data1.par

or

da> epar phot
.YE

Users should be sure to append a .par extension to any pset files they
create as IRAF needs this extension to identify the file as a pset.
.PP
It is possible to develop quite efficient and creative schemes for using psets.
For example a user might choose to copy each crowded stellar field
image to its own directory, copy the default psets \fBdatapars\fR,
\fBfindpars\fR, \fBcenterpars\fR, \fBfitskypars\fR, \fBphotpars\fR
and \fBdaopars\fR to the files "datapars.par", "findpars.par",
"centerpars.par", "fitskypars.par", "photpars.par" and "daopars.par" in
each image directory, and then edit
the parameter sets of the top level tasks to look for psets with those names.
Once this is done the psets in each directory can be edited at will
without ever needing to edit the names of the psets in the top
level tasks.
.PP
The individual pset parameters themselves have the same attributes as
task level parameters. Hidden pset parameters may be altered on the
command line in the same way as task parameters.
The only distinction between task level parameters and pset parameters
is that the latter may be stored in or read from a user defined file.

.NH 2
Interactive Mode and Non-Interactive Mode 

.PP
The \fBphot\fR task's \fIinteractive\fR parameter
switches the task between interactive and non-interactive mode.
.PP
In interactive mode user instructions in the form of single keystroke
commands or colon commands are read from the image cursor.
For example the \fBphot\fR task \fB'i'\fR keystroke command enters the
interactive setup menu and the \fB'v'\fR keystroke command verifies the
current parameters. The colon commands are used to show or set any parameter.
For example, if the user does not like the fact that the full-width
half-maximum of a star
as measured with the cursor is 2.5368945 he/she can set it to 2.54 by
typing \fB:fw 2.54\fR.
.PP
In non-interactive mode the input files and images are read,
the parameters are read from the psets,
and the output files are written,
all, with the exception of an optional verification step, without the
intervention of the user.
.PP
The DAOPHOT parameter editing task \fBdaoedit\fR and the photometry catalog
examining task \fBpexamine\fR are interactive tasks.
Four other DAOPHOT tasks, \fBdaofind\fR, \fBphot\fR, \fBpstselect\fR,
and \fBpsf\fR
have an interactive and a non-interactive mode. The default mode for
\fBdaofind\fR, \fBphot\fR, and \fBpstselect\fR is non-interactive while
for \fBpsf\fR
it is interactive.
The remaining DAOPHOT tasks are currently non-interactive tasks.

.NH 2
Image and Graphics Cursor Input

.PP
All tasks which can be run interactively accept commands from the logical image
cursor parameter \fIicommands\fR. Logical image cursor commands can
read from the logical image cursor, \fIicommands\fR = "" or a file,
\fIicommands\fR = "filename". The logical image cursor is normally
the physical image cursor and the value of the IRAF environment
variable \fBstdimcur\fR is normally "stdimage". In cases where the image
display device is non-existent or cursor read-back is not implemented for
a particular device the logical image cursor may be reassigned globally to the
the graphics cursor or the standard input
by setting the IRAF environment variable \fBstdimcur\fR as follows.

.YS
da> set stdimcur = "stdimage"         (image cursor default)

da> set stdimcur = stdgraph           (graphics cursor)

da> set stdimcur = "text"             (standard input)
.YE

If logical image cursor commands are read from the standard input or a
file, the commands must have the following format

.YS
[x y wcs] key [cmd]\fR
.YE

where x and y stand for the x and y position of the image cursor, wcs defines 
the world coordinate system, key is
a keystroke command, and cmd is an optional user command.
Quantities in square brackets are optional. The necessity for their
presence is dictated by the nature of the keystroke command. In the
case of the \fBphot "i"\fR keystroke described above they are required, whereas
in the case of the \fBphot "v"\fR keystroke they are not.
.PP
Some interactive commands require input from the logical graphics cursor
parameter \fIgcommands\fR which may be the logical graphics cursor,
\fIgcommands\fR = "", or a file of graphics cursor commands,
\fIgcommands\fR = "filename".
In DAOPHOT the logical graphics cursor must be set to the physical
graphics cursor and the value of the IRAF environment variable
\fBstdgcur\fR should be "stdgraph".

.NH 2
Graphics Output

.PP
The \fBphot\fR parameters \fIgraphics\fR and \fIdisplay\fR specify the
default vector graphics and image display graphics devices.
Vector graphics output is written to the user's
graphics window, and image
graphics is overlaid on the user's image display. window
All interactive vector graphics output is written to
the device specified by \fIgraphics\fR. An example of this type of graphics
output is the
radial profile plot of a star plotted by the \fBphot\fR interactive
setup menu.
Image graphics is written to the image display device
specified by \fIdisplay\fR. 
Examples of this type of output are the optional crosses 
which mark the centers of the stars being measured by \fBphot\fR.
\fBIRAF does not currently support writing interactive graphics
to the image display device
so the display marking features of DAOPHOT are not supported\fR.
The single exception occurs in the situation
where the user is running interactively
off a contour plot as described in the \fBphot\fR help documentation.
In this case marking will work if
the parameter \fIdisplay\fR is set to "stdgraph".
DAOPHOT tasks which reference \fIgraphics\fR or \fIdisplay\fR  will, in
interactive mode, issue
a warning if they cannot open either or both of these devices,
and continue execution.
.PP
Some DAOPHOT tasks permit the user to save plots of the results
for each measured star in a plot metacode file.
For example. if the \fBphot\fR task parameter \fIplotfile\fR is defined,
then for each star written to \fIoutput\fR
a radial profile plot is written to the plot metacode file \fIplotfile\fR.
\fIPlotfile\fR is opened in append mode and succeeding executions
of \fBphot\fR will write to the end of the same file.
Users should be aware plotfile can become very large and
that writing radial profile plots
to \fIplotfile\fR will greatly slow the execution of \fBphot\fR or any
other task.

.NH 2
Verify, Update, and Verbose

.PP
In non-interactive mode the algorithm parameter values are read from the psets,
critical parameters are verified if the \fIverify\fR switch is on, and 
updated if both the \fIverify\fR and \fIupdate\fR switches are on.
The \fIverify\fR and \fIupdate\fR options are also available as
separate keystroke commands in interactive mode.
Users must remember to turn off the
\fIverify\fR switch if they submit a task to the background or the task
will pause and wait indefinitely for input from the terminal.
.PP
In interactive or non-interactive mode a results summary and/or
error messages are written
to the standard output if the \fIverbose\fR switch is on.
Users must remember to redirect
any verbose output to a file if they submit the task to the background or
it will be lost.

.NH 2
Background Jobs

.PP
Any DAOPHOT task can be run in background by appending an ampersand
to the end of the command. For example the \fBphot\fR task can be run
as a background job as shown below.

.YS
da> phot image image.coo.1 image.mag.1 verbose- verify- &
.YE

The user must be sure to turn off verbose mode
and set the verify switch to no. VMS users may have to append a queue
name after the trailing ampersand.
If verbose output is desired it can be captured in a file as shown 
in the example below below. The & after the > will ensure that any error
output is also captured.

.YS
da> phot image image.coo.1 image.mag.1 verbose+ inter- verify- \\ 
    >& listing &
.YE

.NH 2
Timing Tests

.PP
Any DAOPHOT or IRAF task can be timed by prepending a $ sign to the
command as shown below.

.YS
da> $phot image image.coo.1 image.mag.1 inter- verify- verbose- &
.YE

At task termination the computer will print the cpu and elapsed
time on the terminal.
.PP
Care must be taken in using this feature
to make timing comparisons between hosts or even between runs on the same host,
as factors like which queue a task is submitted to (VMS), which version of
the OS the host is running, which  version of the compiler
two programs were compiled under, 
whether the disks are local or networked, and the number of users on the
machine will effect the elapsed time and/or the cpu time.

.NH
Doing Photometry with DAOPHOT

.NH 2
The Test Image

.PP
Each of the DAOPHOT analysis steps summarized in the following section
and discussed in detail in succeeding
sections uses the artificial image stored in fits format in
the file "daophot$test/fits3.fits" as test data. This image is small,
51 by 51 pixels, contains 10 stars whose coordinates and magnitudes
are listed below, has, a mean background level of ~100, poisson noise
statistics, a gain of 1.0,  and a readout noise of 0.0.

.YS
# Artificial Stars for Image Test

    41.0   4.0  17.268
    23.0   7.0  17.600
    18.0   8.0  17.596
    26.0  22.0  16.777
    36.0  22.0  16.317
     8.0  23.0  16.631
    31.0  25.0  16.990
    21.0  26.0  19.462
    29.0  34.0  17.606
    36.0  42.0  16.544
.YE

.PP
Results for this test image are used to illustrate the text. It is hoped
that users so inclined will be able to mimic the reductions on 
their host machine. The fact that the image is small, means that
the tasks execute quickly, it is possible to display all the
important results in the manual, and it is possible for
the user to track and examine  all the important numbers, something not
easy with larger images. Users are encouraged to construct more
challenging artificial images with the ARTDATA package, and to run
them through DAOPHOT.
.PP
All the examples in the following text were run
under IRAF 2.10.3 on a SPARCstation IPX. Users with different hardware
may see minor deviations from the output shown here due to machine
precision differences. 

.NH 2
Typical Analysis Sequence

.PP
The following sequence of operations summarizes the steps required to analyze
a crowded stellar field with DAOPHOT.
.IP [1]
Create a directory in which to analyze the image and make it the current
working directory. By default all output photometry and image files
will be written there.
.IP [2]
Read the reduced image into the working directory with the DATAIO package
task \fBrfits\fR.
.IP [3]
Check that the correct exposure time, airmass, filter id, time of
observation, gain, and readout noise are present and correct
in the image header with the \fBhselect\fR task. Enter / edit them
with the \fBhedit\fR task if they are not. Correct the exposure time for
shutter error, the airmass to mid-exposure, and the gain
and readout noise to the effective gain and readout noise, using the
\fBhedit\fR and/or \fBsetairmass\fR tasks.
.IP [4]
Edit the DAOPHOT algorithm psets with the interactive \fBdaoedit\fR
task. The parameters that require editing at this point are:
1) the numerical parameters
\fIfwhmpsf\fR (full-width at half-maximum of the point-spread function),
\fIsigma\fR (standard deviation of the background in counts), \fIdatamin\fR
(the minimum good data value in counts), \fIdatamax\fR (the maximum good
data value in counts), and the image header keyword parameters
\fIccdread\fR, \fIgain\fR, \fIexposure\fR, \fIairmass\fR, \fIfilter\fR,
and \fIobstimes\fR in the \fBdatapars\fR parameter set,
2) \fIcbox\fR (the centering box width) in the \fBcenterpars\fR parameter set,
3) \fIannulus\fR (inner radius of the sky annulus) and
\fIdannulus\fR (width of the sky annulus) in the \fBfitskypars\fR parameter set,
4) \fIapertures\fR (radii of the photometry apertures) in the \fBphotpars\fR
parameter set, and 5) \fIpsfrad\fR (maximum radius of the psf model)
and \fIfitrad\fR (psf model fitting radius) in the \fBdaopars\fR parameter set.
.IP [5]
Create an initial star list using the \fBdaofind\fR task.
Mark the detected stars on the image display with the \fBtvmark\fR task
and adjust the \fBfindpars\fR parameter \fIthreshold\fR until
a satisfactory star list is created.
.IP [6]
Compute sky background values and initial magnitudes for
the detected stars using the \fBphot\fR task and the star
list written by the \fBdaofind\fR task in step [5].
.IP [7]
Create a psf star list using the \fBpstselect\fR task
and the photometry file written by \fBphot\fR in step [6]. Mark
the coordinates of the psf stars on the image display with the
\fBtvmark\fR task
and edit out any non-stellar objects, stars with 
neighbors within \fIfitrad\fR pixels, or stars with obvious
cosmetic blemishes, using the \fBpexamine\fR  task.
.IP [8]
Compute the current psf model using the
\fBpsf\fR task, the input photometry file written by the \fBphot\fR task
in step [6], and the psf star list written by the \fBpstselect\fR task
in step [7].
.IP [9]
Fit the current psf model to the psf stars and their neighbors 
using the \fBnstar\fR task, the psf star group photometry file
written by the \fBpsf\fR task in step [8] or created by the user in step [11],
and the current psf model written by the \fBpsf\fR task in steps [8] or [13].
Subtract the fitted psf stars
and their neighbors from the original image using the \fBsubstar\fR task,
the photometry file written by the \fBnstar\fR task, and the current
psf model.
Display the subtracted image, mark the psf stars and their neighbors
on the display with the \fBtvmark\fR task,
and examine the \fBnstar\fR photometry
file and the subtracted image with the \fBpexamine\fR task.
If all the psf stars subtract out cleanly and none of them have any
significant neighbors, skip directly to step [14]. If all the  psf stars
and their neighbors subtract out cleanly, and one or more of the psf
stars do have significant neighbors, skip directly to step [13].
.IP [10]
Reexamine the subtracted image written in step [9]. Remove any psf stars
revealed by the subtraction to be  non-stellar, multiple, or to contain
cosmetic blemishes,
from the psf star list written by the \fBpsf\fR task in step
[8] using the \fBpexamine\fR task.
If any bad psf stars are detected recompute the psf model by returning to
step [8] using the newly edited psf star list in place
of the one written by the previous execution of the \fBpsf\fR task in step [8].
.IP [11]
Add any psf star neighbors too faint to be detected by the \fBdaofind\fR
task in step [5] but bright enough to effect the computation of the
psf model, to the original psf star group photometry file written
by the \fBpsf\fR task in step [8],
by estimating their positions, sky values, and magnitudes interactively
with the \fBphot\fR task, merging the results with the original psf star group
photometry file
using the \fBpfmerge\fR task, and regrouping the stars with the \fBgroup\fR
task. Refit the newly grouped psf stars and their neighbors using
the current psf model by returning to step [9],
replacing the original input group photometry file with the one
including the new psf star neighbors.
.IP [12]
Using the subtracted image written by the \fBsubstar\fR task in step [9],
note any systematic patterns in the psf star residuals with distance from
the star (\fIthese indicate a poorly chosen value for the annulus,
dannulus, function,  or psfrad parameters),
position in the image (\fIthese suggest that the psf is variable
and that the value of the varorder parameter should be increased\fR),
or intensity (\fIthis suggests problems with the image data itself, e.g.
non-linearity\fR). If the problem is in the sky fitting parameters
edit the appropriate algorithm parameters and return to step [6]. If
the problem is in the psf modeling and fitting parameters, edit the
appropriate algorithm parameters and return to step [7]. I the problem
appears to be in the data or the data reduction procedures, review the
data taking and reduction history of the image before proceeding.
.IP [13]
Subtract the psf star neighbors but not the psf stars from the original
image using the \fBsubstar\fR task,
the photometry file written by the \fBnstar\fR task
in step [9], and the psf star list and current psf model written by
the \fBpsf\fR task in step [8].
Recompute the current psf model using
the psf neighbor star subtracted image, the psf star group photometry file
written by the \fBpsf\fR task in step [8] or created by the user in step [11],
and the psf star list written in step [8].
If the \fIvarorder\fR parameter was changed
return to step [9].
Otherwise save the psf star neighbor subtracted image as it may be
required for computing the image
aperture correction in step [20], and proceed to step [14].
.IP [14]
Fit the final psf model computed in steps [8] or [13]
to the stars in the photometry file written in
step [6] using the \fBallstar\fR task.
.IP [15]
Run \fBdaofind\fR on the subtracted image produced by \fBallstar\fR in step
[14] in order to pick up stars missed by the first pass of \fBdaofind\fR in
step [5].
.IP [16]
Run \fBphot\fR on the original image using the new star list produced by
\fBdaofind\fR in step [15] and the \fBphot\fR algorithm parameters used
in step [6].
.IP [17]
Merge the photometry file produced by \fBallstar\fR in step [14] with
the one produced by \fBphot\fR in step [16] using the \fBpfmerge\fR
task.
.IP [18]
Rerun \fBallstar\fR on the original image using the merged photometry file
created in step [17] and the psf model created in steps [8] or [13]. 
.IP [19]
Repeat steps [15]-[18] as required, remembering to run \fBdaofind\fR
on the subtracted image produced by \fBallstar\fR and \fBphot\fR on the
original image.
.IP [20]
If the psf model is constant, compute the aperture correction for the
image using the original image and  a sample of bright well-isolated stars
if possible, or the image with
the psf neighbor stars subtracted if necessary, the
\fBphot\fR task, and the PHOTCAL package \fBmkapfile\fR task.
If the psf model is variable, compute the aperture correction by calculating
the mean magnitude difference, for the psf stars with any
the neighbors subtracted, between the psf model fitted magnitudes computed
by the \fBnstar\fR task, and large aperture photometry magnitudes computed
with the \fBphot\fR task.
.IP [21]
Archive the algorithm parameters for the image with the \fBsetimpars\fR task
and proceed to the next image.


.NH 2
Creating and Organizing an Analysis Directory

.PP
By default DAOPHOT reads and writes data from and to the current working
directory. To create and set a new working directory the user must
execute the commands \fBmkdir\fR and \fBchdir\fR as shown below.

.YS
da> mkdir testim
da> chdir testim
.YE

.PP
DAOPHOT can in the course of reducing a single image,
generate a large number of photometry catalogs and output images.
Users should take a moment to consider how they wish to organize their data
directories before beginning any DAOPHOT analysis.  Some possibilities for data
directory organization are: 1) by night of observation for standard star fields,
2) by star field for multi-filter observations of a crowded field, or
3) by individual image for single filter observations of several fields,
or any combination of the above.

.NH 2
Reading the Data 

.PP
DAOPHOT input images are normally read into IRAF from FITS files with
the DATAIO package task \fBrfits\fR. The following example shows how to
read the DAOPHOT test image stored in the FITS file "daophot$test/fits3.fits"
into the IRAF image test.imh.

.YS
da> rfits daophot$test/fits3.fits 1 test
File: test  Artificial Starfield  Size = 51 x 51
.YE

When IRAF supports FITS format images on disk this step will no longer be
necessary, although for some images it may still be desirable for
image i/o efficiency reasons.

.NH 2
Editing the Image Headers

.NH 3
The Minimum Image Header Requirements

.PP
Before beginning DAOPHOT reductions the user must gather
all the data required to determine the following quantities:
1) the effective readout noise of the detector in electrons, 2) the effective
gain of the detector in electrons per count, 3) the maximum good data value
of the detector in counts, 4) the effective exposure time in any units
as long as these units are identical for all the images to be analyzed
together,
5) the filter id, 6) the effective airmass of the observation at mid-exposure,
and 7) the time of the observation. 

.NH 3
The Effective Gain and Readout Noise

.PP
The DAOPHOT package tasks require correct effective
gain and readout noise values for:
1) the computation of the magnitude errors in the \fBphot\fR (gain only
required), \fBpeak\fR, \fBnstar\fR and \fBallstar\fR tasks,
2) the computation of the optimal weights used by the non-linear
least-squares fitting code in the \fBpeak\fR, \fBnstar\fR, and 
\fBallstar\fR tasks,
3) the computation of the predicted signal-to-noise
ratios in the \fBgroup\fR task,
4) the computation of the sharpness and chi statistics in the \fBpeak\fR,
\fBnstar\fR, and \fBallstar\fR tasks, and 5) the correct computation of
the poisson noise (gain only required) in the \fBaddstar\fR task.
.PP
Nominal gain and readout noise values for a single image
should be obtained from the instrument
scientist. These values should also be determined/checked empirically with the
PROTO package task \fBfindgain\fR using bias and flat-field frames that
are unprocessed and uncoadded so that the noise characteristics of the
original data are preserved.
.PP
If the input image is the sum or average of several frames
the gain and readout noise values in the image headers must be edited
from single frame to effective  gain and readout noise values
as shown below. In the following examples
gain and effective gain are in electrons / ADU,
readout noise and effective readout noise are  in electrons, and N is the
number of individual frames which
have been summed, averaged, or medianed to create the input image.

.nf
	[1]. The image is the sum of N frames

             effective gain = gain
	     effective readout noise = sqrt (N) * readout noise

	[2]. The image is the average of N frames

             effective gain = N * gain
	     effective readout noise = sqrt (N) * readout noise

	[3]. The image is the median of N frames

             effective gain = 2.0 * N * gain / 3
	     effective readout noise = sqrt (2 * N / 3) * readout noise
.fi

.PP
The following example shows how to add the correct values of gain and
readout noise, which in this very artificial example are 1.0 and 0.0
respectively, to the header of the test image with the \fBhedit\fR task.

.YS
da> imheader test l+
test[51,51][real]: Artificial Starfield with Noise
    No bad pixels, no histogram, min=71.00896, max=535.1335
    Line storage mode, physdim [51,51], length of user area 163 s.u.
    Created Mon 09:59:00 17-May-93, Last modified Mon 09:59:00 17-May-93
    Pixel file 'tucana!/d0/iraf/davis/test.pix' [ok]
    'KPNO-IRAF'           /
    '10-05-93'            /
    IRAF-MAX=           5.351335E2  /  DATA MAX
    IRAF-MIN=           7.100896E1  /  DATA MIN
    IRAF-BPX=                   32  /  DATA BITS/PIXEL
    IRAFTYPE= 'REAL    '            /  PIXEL TYPE
da> hedit test gain 1.0 add+ verify-
add test,gain = 1.
test updated
da> hedit test rdnoise 0.0 add+ verify-
add test,rdnoise = 0.
test updated
da> imheader test l+
test[51,51][real]: Artificial Starfield with Noise
    No bad pixels, no histogram, min=71.00896, max=535.1335
    Line storage mode, physdim [51,51], length of user area 244 s.u.
    Created Mon 09:59:00 17-May-93, Last modified Mon 09:59:00 17-May-93
    Pixel file 'tucana!/d0/iraf/davis/test.pix' [ok]
    'KPNO-IRAF'           /
    '10-05-93'            /
    IRAF-MAX=           5.351335E2  /  DATA MAX
    IRAF-MIN=           7.100896E1  /  DATA MIN
    IRAF-BPX=                   32  /  DATA BITS/PIXEL
    IRAFTYPE= 'REAL    '            /  PIXEL TYPE
    GAIN    =                   1.
    RDNOISE =                   0.\fR
.YE

.PP
The following example shows how to correct the single frame
values of gain and readout noise, already present in the input image
header, to account for the fact that the input image is actually the
average of three frames (note that the frames are NOT actually independent
in this example!). 

.YS
da> imsum test,test,test testav3 option=average
da> hedit testav3 gain "(3.0*gain)" verify- 
testav3,GAIN: 1. -> 3.
testav3 updated
da> hedit testav3 rdnoise "(rdnoise*sqrt(3.0))" verify-
testav3,RDNOISE: 0. -> 0.
testav3 updated
da> imheader testav3 l+
testav3.imh[51,51][real]: Artificial Starfield with Noise
    No bad pixels, no histogram, min=unknown, max=unknown
    Line storage mode, physdim [51,51], length of user area 244 s.u.
    Created Mon 11:02:22 17-May-93, Last modified Mon 11:02:22 17-May-93
    Pixel file 'tucana!/d0/iraf/davis/testav3.pix' [ok]
    'KPNO-IRAF'           /
    '10-05-93'            /
    New copy of test
    IRAF-MAX=           5.351335E2  /  DATA MAX
    IRAF-MIN=           7.100896E1  /  DATA MIN
    IRAF-BPX=                   32  /  DATA BITS/PIXEL
    IRAFTYPE= 'REAL    '            /  PIXEL TYPE
    GAIN    =                   3.
    RDNOISE =                   0.
.YE

.NH 3
The Maximum Good Data Value

.PP
Datamax is the maximum good data value in counts. Datamax
is the count level at which the detector saturates or the count
level at which it becomes non-linear, whichever is lower. DAOPHOT requires
a correct value of datamax to: 1) identify bad data in the \fBdaofind\fR,
\fBphot\fR, \fBpsf\fR, \fBpeak\fR, \fBgroup\fR, \fBnstar\fR,
and \fBallstar\fR tasks, and 2) identify saturated stars in the \fBphot\fR,
\fBpsf\fR, and \fBsubstar\fR tasks.
.PP
Users should be sure to allow adequate leeway for the detector bias level
in their determination of datamax. Test is an artificial image
linear over its entire data range. However as an example assume that it was
actually observed with a detector which is linear from 0 to 25000 counts
at a gain setting of 1.0, and that the mean bias level that was subtracted
from the raw data was ~400 counts.
In that case the user should set datamax to something like 24500 not 25000
counts.
.PP
Datamax may be stored in the image header with \fBhedit\fR
as shown below. The use of the header keyword gdatamax instead of
datamax avoids any confusion with the reserved FITS keywords
datamin and datamax should they already be present in the image header,
or the IRAF keywords iraf-max and iraf-min which have the same meaning.

.YS
da> hedit test gdatamax 24500 add+ verify-
add test,gdatamax = 24500
test updated
.YE

.NH 3
The Effective Exposure Time

.PP
The exposure time is used by the \fBphot\fR task to normalize the computed
initial magnitudes to an effective exposure time of one time unit. The
magnitude scale established in \fBphot\fR is preserved
in all the subsequent DAOPHOT analysis. Setting the correct exposure
time in the image headers before beginning DAOPHOT reductions will
simplify the book-keeping required in the later calibration step
significantly.
.PP
Exposure times should also be corrected
for any timing errors in the instrument shutter, although this is normally
important only for short exposure observations of standard stars.
.PP
The following example shows how to add the exposure time in seconds
to the image header, and how to correct it for a known shutter error
of 13 milli-seconds. Note that rather than overwrite the nominal exposure time
exptime, the user has chosen to store the corrected exposure time in
a new keyword cexptime.

.YS
da> hedit test exptime 1.0 add+ verify-
add test,exptime = 1.
test updated
da> hedit test cexptime "(exptime+.013)" add+ verify- 
add test,cexptime = 1.013
test updated
da> imheader test l+
test[51,51][real]: Artificial Starfield with Noise
    No bad pixels, no histogram, min=71.00896, max=535.1335
    Line storage mode, physdim [51,51], length of user area 365 s.u.
    Created Mon 09:59:00 17-May-93, Last modified Mon 09:59:00 17-May-93
    Pixel file 'tucana!/d0/iraf/davis/test.pix' [ok]
    'KPNO-IRAF'           /
    '10-05-93'            /
    IRAF-MAX=           5.351335E2  /  DATA MAX
    IRAF-MIN=           7.100896E1  /  DATA MIN
    IRAF-BPX=                   32  /  DATA BITS/PIXEL
    IRAFTYPE= 'REAL    '            /  PIXEL TYPE
    GAIN    =                   1.
    RDNOISE =                   0.
    GDATAMAX=                24500
    EXPTIME =                   1.
    CEXPTIME=                1.013
.YE

.NH 3
The Airmass, Filter Id, and Time of Observation

.PP
The airmass, filter id, and time of observation are not used directly by
any of the DAOPHOT tasks.  They are read from the image header and recorded
in the output photometry files. Correctly setting the airmass,
filter id,  and the time of observation in the image headers before running
any DAOPHOT tasks will however significantly reduce the book-keeping
required in the subsequent calibration step.
.PP
The airmass can be computed and/or corrected to mid-exposure with the
ASTUTIL package task \fBsetairmass\fR. By default \fBsetairmass\fR requires
that the name of the observatory, date of observation, ra and dec, epoch of
the ra and dec, sidereal time, and exposure time be recorded
in the image header in the appropriate units in the keywords
observat, date-obs, ra, dec, epoch, st, and exptime. Hopefully most or
all of this information is already in the image header but in case
it is not, the following example shows how to edit it in and run
\fBsetairmass\fR.

.YS
da> hedit test observat "CTIO" add+ verify- show-
da> hedit test "date-obs" "12/10/88" add+ verify- show-
da> hedit test ra "(str('21:51:59.0'))" add+ verify- show-
da> hedit test dec "(str('02:33:31.0'))" add+ verify- show-
da> hedit test epoch 1985.0 add+ verify- show-
da> hedit test st "(str('20:47:55.0'))" add+ verify- show-
da> setairmass test show-
da> imheader test l+
test[51,51][real]: Artificial Starfield with Noise
    No bad pixels, no histogram, min=71.00896, max=535.1335
    Line storage mode, physdim [51,51], length of user area 649 s.u.
    Created Mon 09:59:00 17-May-93, Last modified Mon 09:59:00 17-May-93
    Pixel file 'tucana!/d0/iraf/davis/test.pix' [ok]
    'KPNO-IRAF'           /
    '10-05-93'            /
    IRAF-MAX=           5.351335E2  /  DATA MAX
    IRAF-MIN=           7.100896E1  /  DATA MIN
    IRAF-BPX=                   32  /  DATA BITS/PIXEL
    IRAFTYPE= 'REAL    '            /  PIXEL TYPE
    GAIN    =                   1.
    RDNOISE =                   0.
    GDATAMAX=                24500
    EXPTIME =                   1.
    CEXPTIME=                1.013
    OBSERVAT= 'CTIO    '
    DATE-OBS= '12/10/88'
    RA      = '21:51:59.0'
    DEC     = '02:33:31.0'
    EPOCH   =                1985.
    ST      = '20:47:55.0'
    AIRMASS =             1.238106
.YE

The tortuous syntax required to enter the ra, dec, and st keywords is
necessary in order to avoid \fBhedit\fR turning strings like
"21:51:59.0" into numbers,
e.g. 21.86639. \fBSetairmass\fR permits the user to change the
default names for the date-obs and exptime image header keywords but
not those of observat, ra, dec, epoch or st.
To list the observatories in the IRAF observatory database and/or to find out
how to deal with the case of data taken at an observatory not in the
observatory database, the user should consult the help page for the
\fBobservatory\fR task.
.PP
The filter id is a string defining the filter used to take the observations.
It can be easily edited into the image header as shown below.

.YS
da> hedit test filters V add+ verify- show-
.YE

Users should be aware that any embedded blanks will be removed from the
filter id after it is read from the image header, but before it is
recorded in the photometry files. For example a filter id of "V band"
in the image header will become "Vband" in the photometry file.
.PP
The time of observation is a string defining the time at which the
observation was taken. The time of observation may be ut or local
standard time. If the time of observation is not already recorded in
the image header it can be entered in the usual fashion as shown below.

.YS
da> hedit test ut "(str('00:07:59.0'))" add+ verify- show-
.YE

.PP
After editing the "final" image header should look something like the
following.

.YS
da> imheader test l+
test[51,51][real]: Artificial Starfield with Noise
    No bad pixels, no histogram, min=71.00896, max=535.1335
    Line storage mode, physdim [51,51], length of user area 730 s.u.
    Created Mon 09:59:00 17-May-93, Last modified Mon 09:59:00 17-May-93
    Pixel file 'tucana!/d0/iraf/davis/test.pix' [ok]
    'KPNO-IRAF'           /
    '10-05-93'            /
    IRAF-MAX=           5.351335E2  /  DATA MAX
    IRAF-MIN=           7.100896E1  /  DATA MIN
    IRAF-BPX=                   32  /  DATA BITS/PIXEL
    IRAFTYPE= 'REAL    '            /  PIXEL TYPE
    GAIN    =                   1.
    RDNOISE =                   0.
    GDATAMAX=                24500
    EXPTIME =                   1.
    CEXPTIME=                1.013
    OBSERVAT= 'CTIO    '
    DATE-OBS= '12/10/88'
    RA      = '21:51:59.0'
    DEC     = '02:33:31.0'
    EPOCH   =                1985.
    ST      = '20:47:55.0'
    AIRMASS =             1.238106
    FILTER  = 'V       '
    UT      = '00:07:59.0'\fR
.YE


.NH 3
Batch Header Editing

.PP
The previous examples described in detail how to enter each of the required
keyword and value pairs into the image header using the \fBhedit\fR task.
Users with large number of header keywords to enter should consider using the
more batch oriented alternative task \fBasthedit\fR.


.NH 2
Editing, Checking, and Storing the Algorithm Parameters

.NH 3
The Critical Algorithm Parameters

.PP
The critical DAOPHOT algorithm parameters that should be set
before beginning any DAOPHOT analysis are:
1) the 
full-width at half-maximum of the psf \fIfwhmpsf\fR, the standard
deviation of the sky background in counts \fIsigma\fR, the minimum and
maximum good data values \fIdatamin\fR and \fIdatamax\fR, and the image
header keyword parameters 
\fIccdread\fR, \fIgain\fR, \fIexposure\fR, \fIairmass\fR, \fIfilter\fR,
and \fIobstimes\fR in the \fBdatapars\fR parameter set,
2) the default centering algorithm \fIcalgorithm\fR and centering box
\fIcbox\fR parameters in the \fBcenterpars\fR parameter set, 3) the sky fitting
algorithm \fIsalgorithm\fR, and the sky annulus \fIannulus\fR and
\fIdannulus\fR parameters in the \fBfitskypars\fR parameter set,
4) the \fIapertures\fR parameter in the \fBphotpars\fR parameter set,
and 5) the psf radius \fIpsfrad\fR
and fitting radius \fIfitrad\fR parameters in the \fBdaopars\fR parameter set.
The reamining parameters should be left at their default values, at least
initially.

.NH 3
Editing the Algorithm Parameters Interactively with Daoedit 

.PP
The DAOPHOT algorithm parameter editing task is \fBdaoedit\fR. \fBDaoedit\fR
permits
the user to edit all the algorithm parameter sets at once. It offers all the
capabilities of the IRAF parameter editing task \fBepar\fR, plus the
ability to set parameters using the displayed image and radial
profile plots of isolated stars.
.PP
To run \fBdaoedit\fR the user displays the image, types \fBdaoedit\fR, and waits
for the image cursor to appear ready to accept user commands. The following
example summarizes a typical \fBdaoedit\fR parameter editing session.

.YS
da> display test 1 fi+ 
da> daoedit test
.YE

.IP ...
Execute the command \fB":epar datapars"\fR and enter the correct
values for the \fIdatamax\fR parameter, and the image header
keyword parameters \fIccdread\fR, \fIgain\fR, \fIexposure\fR, \fIairmass\fR,
\fIfilter\fR, and \fIobstime\fR.
.IP ...
Choose a bright isolated star and execute the \fBr\fR cursor
keystroke command to plot its radial profile.
.IP ...
From the information in the radial plot header and the plot
itself estimate reasonable values for the full-width at
half-maximum of the psf, the sky level, and the standard
deviation of the sky level in the image.
.IP ...
Repeat the previous step for several stars in order to
confirm that the original estimated values are reasonable.
.IP ...
Execute the \fB":epar datapars"\fR command once more and enter
the estimated values of the full-width at half-maximum of the psf and
the standard deviation of the sky background in the \fIfwhmpsf\fR
and \fIsigma\fR parameters respectively.
.IP ...
Set the \fIdatamin\fR parameter to the estimated sky background level
minus k times the standard deviation of the sky background, where
k is a number between 5.0 and 7.0.

.IP 
then

.IP ...
Execute the command \fB":epar centerpars"\fR and set the \fIcbox\fR
parameter to 5 pixels or ~ 2 * \fIfwhmpsf\fR whichever is
greater.
.IP ...
Execute the command \fB":epar fitskypars"\fR and set the \fIannulus\fR
parameter to ~ 4 * \fIfwhmpsf\fR and the \fIdannulus\fR parameter to a
number between 2.5 * \fIfwhmpsf\fR and 4.0 * \fIfwhmpsf\fR.
.IP ...
Execute the command \fB":epar photpars"\fR and set the apertures
parameter to ~ 1.0 * fwhmpsf or 3 pixels whichever is greater.
.IP ...
Execute the command \fB":epar daopars"\fR and set the \fIpsfrad\fR
parameter to ~ 4 * \fIfwhmpsf\fR + 1 and the \fIfitrad\fR parameter to
~ 1.0 * \fIfwhmpsf\fR or 3 pixels whichever is greater.

.IP
or alternatively

.IP ...
Move to a bright star and execute the \fBi\fR cursor keystroke
command to enter the interactive setup menu.
.IP ...
Mark the \fIfwhmpsf\fR, \fIcbox\fR, \fIannulus\fR, \fIdannulus\fR,
\fIapertures\fR, \fIpsfrad\fR, and \fIfitrad\fR parameters with the
graphics cursor on the displayed radial profile plot, and verify and/or
roundoff the marked values.
.PP
The following sections discuss in detail how to edit each of the
parameter sets using the test image as a specific example.

.NH 4
The Data Dependent Algorithm Parameters 

.PP
A subset of the datapars parameters are used to specify the
characteristics of the detector, including the saturation or linearity
limit (\fIdatamax\fR) and noise model (\fIccdread\fR
and \fIgain\fR), and the parameters of the observation, including
exposure time (\fIexposure\fR),
airmass (\fIairmass\fR), filter (\fIfilter\fR), and time of observation
(\fIobstime\fR).
.PP
To edit the \fBdatapars\fR algorithm parameter set
from within the \fBdaoedit\fR task the user enters the command
\fB":epar datapars"\fR to invoke the \fBepar\fR task and edits the
parameters in the usual manner.
Editing is terminated with the usual \fB":wq"\fR command which returns the
user to the main \fBdaoedit\fR command loop.
.PP
After the appropriate  \fIdatamax\fR, \fIccdread\fR, \fIgain\fR,
\fIexposure\fR, \fIairmass\fR,
\fIfilter\fR, and \fIobstime\fR parameter values for the
test image are entered, the \fBdatapars\fR
parameter should look as follows.

.YS
Image Reduction and Analysis Facility
PACKAGE = daophot
   TASK = datapars

(scale  =            1.) Image scale in units per pixel
(fwhmpsf=           2.5) FWHM of the PSF in scale units
(emissio=           yes) Features are positive ?
(sigma  =             0.) Standard deviation of background in counts
(datamin=         INDEF) Minimum good data value
(datamax=         24500) Maximum good data value
(noise  =       poisson) Noise model
(ccdread=       rdnoise) CCD readout noise image header keyword
(gain   =          gain) CCD gain image header keyword
(readnoi=            0.) CCD readout noise in electrons
(epadu  =            1.) Gain in electrons per count
(exposur=      cexptime) Exposure time image header keyword
(airmass=       airmass) Airmass image header keyword
(filter =        filter) Filter image header keyword
(obstime=            ut) Time of observation image header keyword
(itime  =            1.) Exposure time
(xairmas=         INDEF) Airmass
(ifilter=         INDEF) Filter
(otime  =         INDEF) Time of observation
(mode   =            ql)
.YE

.PP
Users should realize that the values of the parameters \fIreadnoise\fR
and \fIepadu\fR will be used for the gain and readout noise if the image
header keywords specified by \fIccdread\fR and \fIgain\fR are not found
in the image header or cannot be correctly decoded. Similarly the values of
the \fIitime\fR,
\fIxairmass\fR, \fIifilter\fR, and \fIotime\fR parameters will be used
for the exposure time, airmass, filter id, and time of observation if
the image header keywords specified by \fIexposure\fR, \fIairmass\fR,
\fIfilter\fR, and \fIobstime\fR are not found in the image header
or cannot be correctly decoded.
.PP
The \fBdatapars\fR parameters \fIfwhmpsf\fR, \fIsigma\fR, and \fIdatamin\fR 
are used to: 1) determine the size of star for which the \fBdaofind\fR star
detection algorithm is optimized (fwhmpsf), 2) define the \fBdaofind\fR
algorithm detection threshold for faint objects (sigma),
3) define the fwhm of the psf for the \fBphot\fR task centering algorithms 
"gauss" and "ofilter" (fwhmpsf),
4) supply a first guess for the true fwhm of the psf to the psf
function fitting task \fBpsf\fR (fwhmpsf), 5) determine the
minimum good data value
in the \fBdaofind\fR, \fBphot\fR, \fBpsf\fR, \fBpeak\fR, \fBgroup\fR, 
\fBnstar\fR, and \fBallstar\fR tasks (datamin).
.PP
Reasonable values for these parameters can be obtained by examining the
radial profile plots of several isolated stars from within the
\fBdaoedit\fR task as outlined below:

.IP ...
Move the image cursor on the displayed image to a
reasonably bright isolated star (a good candidate is
the star at pixel 8,23 in the test image) and execute
the \fBr\fR keystroke command.
A radial and integrated profile plot of the selected
star will appear on the screen with the largest photometry aperture radius,
inner and outer radii of the sky annulus, and median sky level in the sky
annulus marked on the plot.
.IP ...
Assuming that the plot is normal, note the computed
fwhmpsf (2.6 rounded to the nearest tenth of a pixel for
the star at 8,23), median sky value (100 counts rounded
to the nearest count for the star at 8,23), and standard
deviation of the sky values (10 counts rounded to the
nearest count for the star at 8,23) written in the plot header.
These numbers suggest a value of ~50 for datamin (50 is ~5
standard deviations of the background counts below the
background count estimate)
.IP ...
Edit the estimated values into the datapars pset by
typing the command \fB":epar datapars"\fR, entering the values,
and typing \fB":wq"\fR to update the parameter set.

.IP
or

.IP ...
Enter them individually using the daoedit colon commands,
e.g. \fB":fwhmpsf 2.5"\fR, \fB":sigma 10.0"\fR, and \fB":datamin 50.0"\fR.
.IP ...
Check the new values of \fIfwhmpsf\fR, \fIsigma\fR, and \fIdatamin\fR 
by doing radial profile plots of several
other isolated stars (the stars at 36,42 and 41,4 in
the test image are good test stars).
.IP ...
On the basis of the estimated \fIfwhmpsf\fR of these stars change the
fwhmpsf parameter back to 2.5 with the command
\fB":fwhmpsf 2.5"\fR.
.IP ...
Check that the observed standard deviation of the sky
background, sigma, agrees reasonably well with the
predicted value, psigma, based on the median sky level,
and the effective gain and readout noise of the image.
For the test image these numbers are related as shown below.

.nf
	    psigma = sqrt (median sky / effective gain +
	             (effective rdnoise / effective gain) ** 2)
	           ~ sqrt (100.0 / 1.0 + (0. / 1.0) ** 2)
	           ~ 10.0
	           ~ sigma
.fi

.IP ...
If psigma and sigma are significantly different check
that the sky region  is uncrowded, that the effective
gain and readout noise values are correct, and that
earlier reduction procedures have not altered the image
statistics in some fundamental manner

.PP
The \fIemission\fR parameter must be left at "yes",
since DAOPHOT assumes that stars are local maxima not local minima.
.PP
The \fInoise\fR parameter must be left at "poisson" since poisson noise
statistics are assumed throughout the DAOPHOT package.
.PP
The \fIscale\fR parameter defines the units in which radial distances
in the image will be measured. For example if the image scale
is 0.25 "/pixel, users can set \fIscale\fR to 0.25 if they wish
to define the \fIfwhmpsf\fR, \fIcbox\fR, \fIannulus\fR, \fIdannulus\fR,
\fIapertures\fR, \fIpsfrad\fR, \fIfitrad\fR and all the other algorithm
parameters which are defined in terms of a radial distance in arc-seconds.
For simplicity most users choose to leave scale set to 1.0 and
work in pixels.
.PP
The final version of the \fBdatapars\fR parameter set should look something
like the following.

.YS
Image Reduction and Analysis Facility
PACKAGE = daophot
   TASK = datapars

(scale  =            1.) Image scale in units per pixel
(fwhmpsf=           2.5) FWHM of the PSF in scale units
(emissio=           yes) Features are positive ?
(sigma  =           10.) Standard deviation of background in counts
(datamin=           50.) Minimum good data value
(datamax=         24500) Maximum good data value
(noise  =       poisson) Noise model
(ccdread=       rdnoise) CCD readout noise image header keyword
(gain   =          gain) CCD gain image header keyword
(readnoi=            0.) CCD readout noise in electrons
(epadu  =            1.) Gain in electrons per count
(exposur=      cexptime) Exposure time image header keyword
(airmass=       airmass) Airmass image header keyword
(filter =        filter) Filter image header keyword
(obstime=       obstime) Time of observation image header keyword
(itime  =           1.0) Exposure time
(xairmas=         INDEF) Airmass
(ifilter=         INDEF) Filter
(otime  =         INDEF) Time of observation
(mode   =            ql)
.YE


.NH 4
The Centering Algorithm Parameters

.PP
The \fBcenterpars\fR parameter set controls the centering algorithms used by
the \fBphot\fR aperture photometry task. DAOPHOT users should concern
themselves with only two of these parameters, \fIcalgorithm\fR and \fIcbox\fR,
and leave the remaining \fBcenterpars\fR parameters at their default values.
.PP
\fICalgorithm\fR specifies the default \fBphot\fR centering algorithm. Its value
should be "none" if the input coordinate list is the output of the
\fBdaofind\fR task,  or "centroid", "gauss", or "ofilter" if the input
coordinate list was
created with the image or graphics cursor list tasks \fBrimcursor\fR
or \fBrgcursor\fR or the coordinates are
read from the image cursor in interactive mode. The choice of centering
algorithm is not critical since the centers are recomputed using accurate
non-linear least-squares fitting techniques during the psf fitting
process. The most efficient and simplest choice is "centroid", although
more accurate results may be obtained with "gauss" which is
very similar to the centering algorithm used in \fBdaofind\fR.
.PP
The \fIcbox\fR
parameter determines the width in scale units of the data used to compute
the center if \fIcalgorithm \fR is not "none".
For reasonable results \fIcbox\fR should be set to the equivalent of 5 or 
~ 2 * \fIfwhmpsf\fR in pixels whichever is larger.
.PP
\fBCenterpars\fR can be edited from within the \fBdaoedit\fR task
with the command \fB":epar centerpars"\fR. After editing, the \fBcenterpars\fR
parameter set should look like the example below. Note that for the test
image \fIfwhmpsf\fR is ~2.5 pixels so \fIcbox\fR is left at 5.0. 

.YS
PACKAGE = daophot
TASK = centerpars

(calgori=          none) Centering algorithm
(cbox   =            5.) Centering box width in scale units
(cthresh=            0.) Centering threshold in sigma above background
(minsnra=            1.) Minimum signal-to-noise ratio
(cmaxite=            10) Maximum iterations
(maxshif=            1.) Maximum center shift in scale units
(clean  =            no) Symmetry clean before centering
(rclean =            1.) Cleaning radius in scale units
(rclip  =            2.) Clipping radius in scale units
(kclean =            3.) K-sigma rejection criterion in skysigma
(mkcente=            no) Mark the computed center
(mode   =            ql)
.YE

.NH 4
The Sky Fitting Algorithm Parameters

.PP
The \fBfitskypars\fR parameter set controls the sky fitting algorithm
parameters used by the \fBphot\fR task. At this point DAOPHOT users should
concern themselves with only three of these parameters: \fIsalgorithm\fR,
\fIannulus\fR, and \fIdannulus\fR.
.PP
Users should realize that the \fBphot\fR task computes sky values
for the individual stars, and that these values are
used in the \fBpsf\fR task to compute the psf, averaged to form a group sky
value in the \fBpeak\fR, \fBnstar\fR and \fBallstar\fR tasks if sky refitting
is disabled (the default) or an initial sky value if sky refitting
is enabled,  and used
to compute the predicted signal-to-noise ratios in the \fBgroup\fR task.
Although the option to refit the skies at a later stage of analysis exists,
there are difficulties associated with this choice. It is
in the user's best interest to determine the skies as accurately as
possible as early as possible, since sky determination will probably be
the single most important factor in doing good photometry.
.PP
In cases where contamination of the sky region is mostly due
to crowding by neighboring stars users should use the default sky fitting
algorithm "mode"; if the variations in the background are due instead
to nebulosity or large contaminating objects so that the sky statistics are
confused
"median", "centroid", or "crosscor"  might be a better choice; in cases
where the sky statistics
are so poor that the histogram is aliased, undersampled, or sparse such
as might be the case with
very low sky backgrounds "mean" might be the best choice.
When in doubt about the correct choice the user should leave \fIsalgorithm\fR
at "mode" but examine the results carefully for accuracy at each step.
.PP
A good starting value for the inner radius of the sky annulus is ~ 4 *
\fIfwhmpsf\fR
or ~ 10 pixels for the test image. The width of the sky annulus should be
sufficient to give a reasonable sample of sky pixels, >= 5 pixels. We have
chosen a dannulus of ~4 * \fIfwhmpsf\fR or 10 pixels for the test image.
.PP
\fBFitskypars\fR can be edited from within the \fBdaoedit\fR task
with the command \fB":epar fitskypars"\fR. After editing the \fBfitskypars\fR
parameter set should look like the example below.

.YS
PACKAGE = daophot
TASK = fitskypars

(salgori=            mode) Sky fitting algorithm
(annulus=             10.) Inner radius of sky annulus in scale units
(dannulu=             10.) Width of sky annulus in scale units
(skyvalu=              0.) User sky value
(smaxite=              10) Maximum number of sky fitting iterations
(sloclip=              0.) Lower clipping factor in percent
(shiclip=              0.) Upper clipping factor in percent
(snrejec=              50) Maximum number of sky fitting rejection iteratio
(sloreje=              3.) Lower K-sigma rejection limit in sky sigma
(shireje=              3.) Upper K-sigma rejection limit in sky sigma
(khist  =              3.) Half width of histogram in sky sigma
(binsize=             0.1) Binsize of histogram in sky sigma
(smooth =              no) Boxcar smooth the histogram
(rgrow  =              0.) Region growing radius in scale units
(mksky  =              no) Mark sky annuli on the display
(mode   =              ql)
.YE

.NH 4
The Aperture Photometry Parameters

.PP
The \fBphotpars\fR parameter set controls the aperture photometry algorithm
parameters used by the \fBphot\fR task. At this point DAOPHOT users should
concern themselves with only one of these, \fIapertures\fR, the radius
of the aperture through which the initial magnitudes will be computed.
A good rule of thumb is to set the aperture radius to the maximum
of 3 pixels or 1.0 * \fIfwhmpsf\fR pixels. Although magnitudes can be measured
through more than one aperture at a time, it is the magnitude of the
smallest aperture radius along with \fIzmag\fR and the exposure time
which set the DAOPHOT instrumental magnitude scale, and
the magnitudes through the other apertures contribute nothing to the
DAOPHOT analysis until it comes time to compute accurate aperture
corrections. Therefore it is in the user's best interest to set \fIapertures\fR
to a single value at this point and carefully record it.
.PP
\fBPhotpars\fR can be edited from within the \fBdaoedit\fR task
with the command \fB":epar photpars"\fR. After editing the \fBphotpars\fR
parameter set should look like the example below. Note that in this example
\fIfwhmpsf\fR is ~2.5 pixels so \fIapertures\fR is left at 3.0. 

.YS
PACKAGE = daophot
TASK = photpars

(weighti=         constant) Photometric weighting scheme
(apertur=             3.0) List of aperture radii in scale units
(zmag   =              25.) Zero point of magnitude scale
(mkapert=               no) Draw apertures on the display
(mode   =               ql)
.YE

.NH 4
The Psf Modeling and Fitting Parameters

.PP
The \fBdaopars\fR parameter set controls the psf computation, star grouping,
and psf fitting
parameters used by the \fBpstselect\fR,  \fBpsf\fR, \fBpeak\fR,
\fBgroup\fR, \fBnstar\fR,
\fBallstar\fR, \fBsubstar\fR, and \fBaddstar\fR tasks. At this point
DAOPHOT users should
concern themselves with only two of these parameters \fIpsfrad\fR, the radius
over which the psf will be defined, and \fIfitrad\fR, the radius 
over which the psf will be fit to the individual stars. A good rule of thumb is
to set \fIpsfrad\fR to the radius at which the radial profile of the brightest
star of interest disappears into the noise plus 1, something like
~ 4 * \fIfwhmpsf\fR + 1, and
to set \fIfitrad\fR to the maximum of 3 pixels or ~ 1 * \fIfwhmpsf\fR in pixels.

.PP
\fBDaopars\fR can be edited from within the daoedit task
with the command \fB":epar daopars"\fR. After editing the \fBdaopars\fR
parameter set should look something like the example below for the
test image.

.YS
PACKAGE = daophot
TASK = daopars

(functio=           gauss) Analytic component of psf
(varorde=               0) Order of psf variation 
(nclean =               0) Number of cleaning passes
(saturat=              no) Use wings of saturated stars 
(matchra=              3.) Matching radius in scale units
(psfrad =             11.) Radius of psf in scale units
(fitrad =              3.) Fitting radius in scale units
(recente=             yes) Recenter stars during fit
(fitsky =              no) Recompute group sky value during fit
(sannulu=              0.) Inner radius of sky annulus in scale units
(wsannul=             11.) Width of sky annulus in scale units
(flaterr=            0.75) Flat field error in percent
(proferr=              5.) Profile error in percent
(maxiter=              50) Maximum number of iterations
(clipexp=               6) Data clipping exponent
(clipran=             2.5) Data clipping range in sigma
(critove=              1.) Critical overlap group for membership
(maxnsta=           10000) Maximum number of stars to fit
(maxgrou=              60) Maximum number of stars to fit per group
(mode   =              ql)
.YE

.NH 4
Setting the Algorithm Parameters Graphically

.PP
Each of the radial distance dependent parameters \fIfwhmpsf\fR, 
\fIcbox\fR, \fIannulus\fR, \fIdannulus\fR, \fIapertures,\fR,
\fIpsfrad\fR, \fIfitrad\fR can be edited
individually and interactively by marking the current radial profile
plot with the
graphics cursor after executing the appropriate keystroke command.
For example the \fBf\fR keystroke command will prompt the user to
mark the fwhm of
the psf on the current radial profile plot,
verify the marked value, and update the \fIfwhmpsf\fR parameter. 
.PP
All the radial distance dependent parameters listed above
can be edited at once my moving the
image cursor to a bright star, typing the \fB daoedit i\fR keystroke command
to invoke the interactive graphics setup menu.
The size of the radial profile plot and the sky regions
are set by the \fIscale\fR, \fIannulus\fR, and \fIdannulus\fR parameters.
The centering algorithm
used is always "centroid" regardless of the value of the \fIcalgorithm\fR
parameter, \fIcbox\fR and \fIscale\fR determine the centering box size,
and the photometry is computed inside the largest aperture specified by
the \fIapertures\fR parameter. After the user finishes marking all the
parameters on the plot
he/she is given an opportunity to verify or edit the results, e.g., change the
value for fwhmpsf from 2.536 as read from the graphics cursor to 2.5. 

.NH 3
Checking the Algorithm Parameters with Daoedit

.PP
The purpose of setting all the critical algorithm parameters to reasonable
values before beginning any DAOPHOT analysis, is to ensure that the user
gets off to a good start. Although setting the parameters to unreasonable
values often results in bizarre results which are immediately obvious,
e.g., the detection of thousands of
noise spikes, the problems can sometimes be more subtle.
For example, a sky annulus that is too close to the star will result in
measured sky values which are too high and poor subtractions of 
the fitted stars which may not be discovered until the user has
become thoroughly exasperated trying to produce good fits to the psf
stars.
.PP
The current DAOPHOT algorithm parameters can be checked at any time with
the \fBdaoedit\fR task and the \fB":lpar"\fR command. For example the
\fBdatapars\fR parameters set can be listed with the \fBdaoedit
":lpar datapars"\fR command. The remaining parameters sets \fBfindpars\fR,
\fBcenterpars\fR, \fBfitskypars\fR, \fBphotpars\fR, and \fBdaopars\fR
may be listed in the same way.
.PP
When listing the algorithm parameters users should check that:
.IP [1]
the \fBdatapars\fR image header keyword parameters \fIccdread\fR, \fIgain\fR,
\fIexposure\fR, \fIairmass\fR, \fIfilter\fR, and \fIobstime\fR are 
properly set.
.IP [2]
the \fBdatapars\fR \fIfwhmpsf\fR, \fIsigma\fR, \fIdatamin\fR, and \fIdatamax\fR parameters
are appropriate for the image. Be especially careful of datamin as the
correct value for this parameter varies with the mean sky.
.IP [3]
the \fBdatapars\fR parameter \fIscale\fR is 1.0 unless the user is
thoroughly aware of the meaning of this parameter and the consequences
of setting it to something other than 1.0, and \fIemission\fR is "yes".
.IP [4]
the \fBcenterpars\fR \fIcbox\fR parameter is
appropriate for the image and the remaining \fBcenterpars\fR parameters
are at their default values unless the user
really understands the consequences of altering these parameters.
.IP [5]
the \fBfitskypars\fR  \fIannulus\fR, and \fIdannulus\fR
parameters are appropriate for the image and the remaining \fBfitkskypars\fR
parameters are at their default values unless the user really
understands the consequences of altering these parameters.
.IP [6]
the \fBphotpars\fR \fIapertures\fR parameter is appropriate for the image
and the remaining parameters are at their default values unless the user
really understands the consequences of altering these parameters.
.IP [6]
the \fBdaopars\fR \fIpsfrad\fR and \fIfitrad\fR parameters are appropriate
for the image and all the remaining \fBdaopars\fR parameters are at their
default values unless the user really understands the consequences of
altering these parameters.

.NH 3
Storing the Algorithm Parameter Values with Setimpars

.PP
The current values of all the algorithm parameters for a particular
image may be saved in a file on disk at any
point in the reduction sequence by executing the \fBsetimpars\fR task.
The following command saves the current values of the parameters for
the test image in a file called "test.pars".

.YS
da> setimpars test no yes
.YE

Repeating the previous command at any point in the reduction sequence will
replace the stored parameter values with the current parameter values.


.NH 3
Restoring the Algorithm Parameter Values with Setimpars

.PP
At some point the user may wish to interrupt work on a particular image and
begin work on a different image. This should be no problem as long as the
user remembers to save the algorithm parameter sets  with \fBsetimpars\fR
as described in the previous section.
.PP
The command to restore the algorithm parameter sets for the test image is:

.YS
da> setimpars test yes no
.YE

or

.YS
da> setimpars test yes no parfile=test.pars 
.YE


.NH 2
Creating a Star List

.PP
The initial input to the DAOPHOT package is a star list.
Star lists may be created with the DAOPHOT package task \fBdaofind\fR,
interactively with the image or graphics cursor (the
\fBrimcursor\fR and \fBrgcursor\fR tasks), by another IRAF task, or by
any user program which writes a text file in the correct format.
.PP
Legal star lists are text files containing a list of stars, one star
per line with the x and y coordinates in columns one and two.
Blank lines, lines beginning with "#", and lines containing anything other
than numbers in columns one and two are ignored.
A sample DAOPHOT star list is shown below.

.YS
# Artificial Stars for Image Test

	41.0   4.0  17.268
	23.0   7.0  17.600
	18.0   8.0  17.596
	26.0  22.0  16.777
	36.0  22.0  16.317
 	 8.0  23.0  16.631
	31.0  25.0  16.990
	21.0  26.0  19.462
	29.0  34.0  17.606
	36.0  42.0  16.544
.YE

.NH 3
The Daofind Task

.PP
The \fBdaofind\fR task, searches for point sources 
in an image whose peak intensities are above some user-defined threshold,
computes approximate centers, magnitudes, and 
shape characteristics for all the detected objects, and writes the results
to the output star list.

.NH 4
The Daofind Algorithm

.PP
By default the \fBdaofind\fR algorithm performs the following steps:
.IP[1]
reads the \fBdaofind\fR task parameters, including the input image 
and output star list names and the \fBdatapars\fR and \fBfindpars\fR
algorithm parameters, and asks the user to verify the
\fIfwhmpsf\fR, \fIsigma\fR,
\fIthreshold\fR, \fIdatamin\fR, and \fIdatamax\fR parameters
.IP[2]
calculates the convolution kernel whose mathematical function when
convolved with the input image is to
compute the amplitude of the best-fitting Gaussian of
full-width half-maximum \fIfwhmpsf\fR at each point in the input image
.IP[3]
convolves the input image with the convolution kernel after
eliminating bad data with the \fIdatamin\fR and \fIdatamax\fR parameters,
and writes the results to a temporary convolved image
.IP[4]
searches for local maxima in the convolved image whose amplitudes are greater
than the detection threshold,  and greater than the amplitudes of any neighbors
within a region the size of the convolution kernel
.IP[5]
computes approximate centers, magnitudes, and shape
statistics for these local maxima
.IP[6]
eliminates local maxima whose centers are outside the image, and whose
sharpness and roundness statistics are outside the limits set by the user
.IP[7]
writes the centers, approximate magnitudes, sharpness and roundness
statistics, and id number for the remaining local maxima, to the
output star list
.IP[8]
deletes the convolved image

.NH 4
The Daofind Algorithm Parameters

.PP
The critical \fBdaofind\fR algorithm parameters are \fIfwhmpsf\fR,
\fIdatamin\fR, \fIdatamax\fR, \fIsigma\fR, and \fIthreshold\fR.
These parameters are verified at startup time by \fBdaofind\fR.
.PP
The \fIfwhmpsf\fR parameter should be close
to the true full-width at half-maximum of the psf in order
to optimize the detection algorithm for stellar objects. If \fIfwhmpsf\fR
is too far from the true value, stars may be omitted from the star list
and/or non-stellar objects added to it.
.PP
The \fIdatamin\fR and \fIdatamax\fR parameters are used to flag and remove
bad data from the convolved image.
If \fIdatamin\fR and \fIdatamax\fR are
too far from the true value stars may be omitted from the star list
and/or non-stellar objects added to it.
.PP
The \fIsigma\fR parameter should be close to the true standard deviation of
the sky background in an uncrowded region of the frame. This parameter
in combination with \fIthreshold\fR
determines the detection threshold in counts for faint objects. If it is
incorrect either too few or too many objects will be detected.
.PP
The \fIthreshold\fR parameter should normally be set to some small
number between 3.0 and 5.0.  If threshold is too big only
the brightest stars will be detected. If threshold is too small too
many noise spikes will be detected.

.NH 4
Running Daofind Non-Interactively

.PP
The following example shows how to run \fBdaofind\fR in non-interactive mode.

.YS
da> daofind test default 

FWHM of features in scale units (2.5) (CR or value):
        New FWHM of features: 2.5 scale units  2.5 pixels
Standard deviation of background in counts (10.) (CR or value):
        New standard deviation of background: 10. counts
Detection threshold in sigma (4.) (CR or value):
        New detection threshold: 4. sigma 40. counts
Minimum good data value (50.) (CR or value):
        New minimum good data value: 50. counts
Maximum good data value (24500.) (CR or value):
        New maximum good data value: 24500. counts

Image: test.imh  fwhmpsf: 2.5  ratio: 1.  theta: 0.  nsigma: 1.5

       40.97     4.02   -1.663   0.612   0.017     1
       23.06     7.03   -1.214   0.636  -0.019     2
       18.02     7.96   -1.318   0.622   0.010     3
       25.99    22.01   -2.167   0.658   0.001     4
       35.98    22.00   -2.499   0.572  -0.039     5
        8.02    22.97   -2.239   0.550   0.068     6
       30.97    25.01   -1.934   0.711  -0.044     7
       28.96    33.92   -1.087   0.418   0.132     8
       35.98    42.03   -2.332   0.639   0.108     9

threshold: 40. relerr: 1.140  0.2 <= sharp <= 1.  -1. <= round <= 1.\fR
.YE

If this is the first time \fBdaofind\fR has been run the results will appear
in the file "test.coo.1".
.PP
The detected objects can be marked on the image display using the
\fBtvmark\fR task as shown below.

.YS
da> display test 1 fi+
da> tvmark 1 test.coo.1 col=204
.YE

In this example the detected stars will be marked on the displayed image
as red dots. If too many faints stars have been missed the user
can rerun \fBdaofind\fR with a lower value of the \fIthreshold\fR
parameter.

.NH 4
Running Daofind Interactively

.PP
\fBDaofind\fR may also be run in interactive mode.
Most users will only exercise this option for small images which do not
require long cpu/elapsed times to perform the convolution.
.PP
The following example shows how to run \fBdaofind\fR interactively.

.YS
da> display test 1 fi+

da> daofind test default inter+
.YE

.IP ...
Type the \fBv\fR keystroke command to verify the critical algorithm
parameters.
.LP

.YS
FWHM of features in scale units (2.5) (CR or value):
        New FWHM of features: 2.5 scale units  2.5 pixels
Standard deviation of background in counts (10.) (CR or value):
        New standard deviation of background: 10. counts
Detection threshold in sigma (4.) (CR or value):
        New detection threshold: 4. sigma 40. counts
Minimum good data value (50.) (CR or value):
        New minimum good data value: 50. counts
Maximum good data value (24500.) (CR or value):
        New maximum good data value: 24500. counts
.YE

.IP ...
Type the \fBspacebar\fR keystroke command to detect the objects and write them
out to the star list file.
.LP

.YS
Image: test.imh  fwhmpsf: 2.5  ratio:  1.  theta:  0.  nsigma: 1.5

       40.97     4.02   -1.663   0.612   0.017     1
       23.06     7.03   -1.214   0.636  -0.019     2
       18.02     7.96   -1.318   0.622   0.010     3
       25.99    22.01   -2.167   0.658   0.001     4
       35.98    22.00   -2.499   0.572  -0.039     5
        8.02    22.97   -2.239   0.550   0.068     6
       30.97    25.01   -1.934   0.711  -0.044     7
       28.96    33.92   -1.087   0.418   0.132     8
       35.98    42.03   -2.332   0.639   0.108     9

threshold: 40. relerr: 1.140  0.2 <= sharp <= 1.  -1. <= round <= 1.

Output file: test.coo.1
.YE

.IP ...
Change \fIthreshold\fR to 3.0 with the colon command \fB:threshold 3.0\fR.
.IP ...
Type the \fBspacebar\fR keystroke command to detect the objects and write
them out to a new star list file.
.LP

.YS
Image: test.imh  fwhmpsf: 2.5  ratio:  1.  theta:  0.  nsigma: 1.5

      40.97     4.02   -1.975   0.577   0.017     1
      23.06     7.03   -1.526   0.604  -0.019     2
      18.02     7.96   -1.631   0.587   0.010     3
      25.99    22.01   -2.480   0.626   0.001     4
      35.98    22.00   -2.811   0.537  -0.039     5
       8.02    22.97   -2.551   0.515   0.068     6
      30.97    25.01   -2.246   0.681  -0.044     7
      21.27    25.94   -0.146   0.804  -0.558     8
      28.96    33.92   -1.400   0.379   0.132     9
      35.98    42.03   -2.645   0.606   0.108    10

threshold: 30. relerr: 1.140  0.2 <= sharp <= 1.  -1. <= round <= 1.

Output file: test.coo.2
.YE

.IP ...
Change \fIthreshold\fR to 5.0 with the colon command \fB:threshold 5.0.
.IP ...
Type the \fBspacebar\fR keystroke command to detect the objects and write them
out to a new coordinate file.
.LP

.YS
Image: test.imh  fwhmpsf: 2.5  ratio:  1.  theta:  0.  nsigma: 1.5

      40.97     4.02   -1.420   0.577   0.017     1
      23.06     7.03   -0.972   0.604  -0.019     2
      18.02     7.96   -1.076   0.587   0.010     3
      25.99    22.01   -1.925   0.626   0.001     4
      35.98    22.00   -2.257   0.537  -0.039     5
       8.02    22.97   -1.997   0.515   0.068     6
      30.97    25.01   -1.692   0.681  -0.044     7
      28.96    33.92   -0.845   0.379   0.132     8
      35.98    42.03   -2.090   0.606   0.108     9

threshold: 50. relerr: 1.140  0.2 <= sharp <= 1.  -1. <= round <= 1.

Output file: test.coo.3
.YE

.IP ...
Type the \fBq\fR keystroke, first in the image display window then the
text window to quit the task.
.LP

If this is the first run of \fBdaofind\fR,
the three star list files for the \fIthreshold\fR values of
4.0, 3.0, and 5.0 will be written to "test.coo.1", "test.coo.2", and 
"test.coo.3" respectively.
.PP
The \fBdaofind\fR results for different thresholds can be evaluated by
marking the detected objects on the image display using the \fBtvmark\fR
task and different
colors for each threshold. In the following example objects detected
at threshold=3.0 are marked in red, at threshold=4.0 in green, at threshold=
5.0 in blue.

.YS
da> display test 1 fi+
da> tvmark 1 test.coo.2 col=204 point=3
da> tvmark 1 test.coo.1 col=205 point=3
da> tvmark 1 test.coo.3 col=206 point=3
.YE

Note that the identical stars were detected at thresholds 4.0 and 5.0 but
the faint star at 21,26 was only detected at threshold=3.0. 
.PP
In this example the user decides that threshold = 4.0 is the
"best" threshold, sets the \fIthreshold\fR parameter appropriately as shown
below, and deletes the the star lists for threshold = 3.0 and threshold = 5.0.

.YS
da> findpars.threshold = 4.0
da> delete test.coo.2,test.coo.3
.YE

.NH 4
The Daofind Output

.PP
The quantities xcenter, ycenter, mag, sharpness, roundness, and id
are recorded for each detected object.  Each is described briefly below.
.IP [1]
\fIXcenter\fR and \fIycenter\fR are the coordinates of
the detected object in fractional pixel units. They are computed by
fitting one-dimensional Gaussian functions  of full-width at half-maximum
\fIfwhmpsf\fR to the x and y marginal pixel distributions centered on
the star. The computed coordinates can be overlaid on the
displayed image with the \fBtvmark\fR command.
.IP [2]
The estimated magnitude is measured relative to the detection threshold
and is defined as

.YS
mag = -2.5 * log10 (density / (relerr * threshold * sigma))
.YE

where density is the peak density of the object in the convolved image,
relerr an internally
computed factor measuring the amount by which the standard error in one pixel
in the input image must be multiplied to obtain the standard error
in one pixel in the convolved image, and threshold and sigma
are the values of the corresponding \fIthreshold\fR and \fIsigma\fR
parameters. For stellar
objects the computed magnitude is directly proportional to the true
magnitude of the star. Stars with a peak density exactly equal to
the detection threshold will have a magnitude of 0.0. The remaining
stars will have negative magnitudes.
.IP [3]
The sharpness statistic is the ratio of the amplitude of the best fitting
delta function at the position of a detected object to the amplitude of
the best fitting gaussian at the same position as shown below.

.YS
sharpness = (data - <data>) / density
.YE

The amplitude of the best fitting gaussian is simply the density
of the detected object in the convolved image.
The amplitude of the best fitting delta function is defined
as corresponding original image data value minus the average of all
the neighboring
pixels in the image <data>. Typical values of sharpness are of ~0.6 for
approximately gaussian stars and \fInsigma\fR = 1.5.
Hot pixels will have sharpness values >> 1 and cold pixels will have
sharpness values
of ~0, hence reasonable limits for the \fIsharphi\fR and \fIsharplo\fR
parameters are 1.0 and 0.2 respectively.
Increasing the size of convolution box defined by the
\fInsigma\fR parameter from its default value of 1.5 to a larger value
(smaller values should be avoided !) while keeping the \fIfwhmpsf\fR the
same, will increase the average value of the sharpness statistic because
more pixels further from the center of the star are included in the
computation of <data>. If \fInsigma\fR is changed
the \fBfindpars\fR parameters \fIsharphi\fR and \fIsharplo\fR
will also need to be changed.
.IP [4]
The roundness statistic is computed by fitting a one-dimensional
gaussian function of full-width at half-maximum \fIfwhmpsf\fR to the
x and y marginal pixel distributions.

.YS
roundness =  2.0 * (hx - hy) / (hx + hy)
.YE

hx and hy are the heights of the best fitting one-dimensional
gaussians in x and y. A totally
round object will have a roundness of ~ 0.0. If the object is very
elongated in x roundness will be a large negative number; a large positive
number if it is elongated in y.  The roundness statistic is 
effective at filtering out bad columns and rows of data. It is not
effective at filtering out objects elongated at intermediate angles.
.IP [5]
Id is a sequence number which identifies the star.

.NH 4
Examining the Daofind Output

.PP
The easiest way to check that \fBdaofind\fR is performing correctly is to mark
the detected stars on the image display with \fBtvmark\fR.
.PP
If the marked image suggests that \fBdaofind\fR is detecting too few or
too many stars the first items to check are the the values of the
\fIsigma\fR and \fIthreshold\fR parameters since these parameters determine the
detection threshold. Sigma should be
the standard deviation of the sky pixels in an uncrowded region of the image.
Threshold should normally be some number between 3.0 and 5.0. If sigma
and threshold are reasonable the user should compare the observed value of
sigma with the predicted value derived from the median background level and the
effective gain and readout noise values. If there is a significant
mismatch in these numbers the user should check the reduction history of the
image.  The number of spurious detections goes up dramatically for
thresholds less than ~3.0 * sigma. A  plot of number
of detections versus threshold will show a change in slope at some point below
this threshold.  Users who wish to detect faint objects while keeping
spurious detections at a manageable minimum should
set the detection threshold to a value just above the threshold at
which this change in slope occurs.
.PP
Users should also check the values of the
parameters \fIsharplo\fR, \fIsharphi\fR,
\fIroundlo\fR, and \fIroundhi\fR parameters to ensure that
detected objects are not being unfairly filtered out. In particular the values
of \fIsharplo\fR and \fIsharphi\fR should be changed if the \fInsigma\fR
parameter is changed.
.PP
Finally the user should check
the \fIfwhmpsf\fR, \fInsigma\fR, \fIdatamin\fR and \fIdatamax\fR parameters
for correctness since these parameters  control the computation of the
convolution kernel and the density enhancement image.
.PP
Histograms of the various columns in the \fBdaofind\fR output can be
plotted using the \fBpdump\fR and \fBphistogram\fR tasks. The following
example shows how to plot a histogram of the magnitudes.

.YS
da> pdump test.coo.1 mag yes | phistogram STDIN binwidth=.1
.YE

The various columns can also be plotted against each other.
The following example shows how to plot magnitude error versus magnitude.

.YS
da> pdump test.coo.1 mag,merr yes | graph point+
.YE

.PP
By setting the \fBdaofind\fR \fIstarmap\fR and \fIskymap\fR parameters the
user can save and examine the density enhancement image and the corresponding
background density image.  The sum of these two images should yield a
close representation of the original image except for regions of
bad data and edge pixels. Due to the nature of the convolution kernel
the starmap image will have a mean value of
~0.0 in the sky regions,  an rms \(~= relerr * sigma in the sky regions,
and positive peaks of intensity surrounded by negative valleys at the positions
of bright stars. The skymap image will have a mean value \(~= sky in 
the sky regions, an rms \(~= sqrt (sigma ** 2 / N + K * (relerr * sigma) ** 2),
(N is the number of pixels in the gaussian kernel and K is the average power
in the gaussian kernel), and dips in intensity surrounded by
bright rings at the position of the stars. 

.NH 3
Rgcursor and Rimcursor

.PP
The LISTS package tasks \fBrimcursor\fR and \fBrgcursor\fR can be used to
generate coordinate lists interactively. For example a coordinate
list can be created using the image display and the image display cursor
as shown below.

.YS
da> display test 1 fi+

da> rimcursor > test.coo
.YE

.IP ...
Move cursor to stars of interest and tap the space bar.
.IP ...
Type <EOF> to terminate the list.

.PP
A coordinate list can also be created using a contour plot and the graphics
cursor as shown below.

.YS
da> contour test

da> rgcursor > test.coo
.YE

.IP ...
Move the cursor to the stars of interest and tap the space bar.
.IP ...
Type <EOF> to terminate the list.

.PP
In both cases the text file "test.coo" contains the x and y coordinates of
the marked stars in image pixel units. The output of \fBrimcursor\fR or
\fBrgcursor\fR can
be read directly by the DAOPHOT \fBphot\fR task. 

.NH 3
User Program

.PP
Any user program which produces a text file with the stellar coordinates
listed one per line with x and y in columns 1 and 2, can be used to produce
DAOPHOT coordinate files which can be read by the \fBphot\fR task.

.NH 3
Modifying an Existing Coordinate List

.PP
The LISTS package routine \fBlintran\fR 
can be used to perform simple coordinate transformations on
coordinate lists including shifts, magnifications, and rotations.

.NH 2
Initializing the Photometry with Phot

.PP
The \fBphot\fR task computes initial centers,
sky values, and initial magnitudes for all the objects in the input
star list. The centers and magnitudes are used as starting
values for the non-linear
least-squares psf computation and fitting routines in the \fBpsf\fR,
\fBpeak\fR, \fBnstar\fR, and \fBallstar\fR tasks, and to estimate
signal-to-noise
values in the \fBgroup\fR task. The individual sky values
computed by \fBphot\fR are used directly by the \fBpsf\fR task to compute
the psf model, by the
\fBpeak\fR, \fBnstar\fR,
and \fBallstar\fR tasks to compute the group sky values, and by the
\fBgroup\fR task to estimate signal-to-noise ratios.

.NH 3
The Phot Algorithm

.PP
By default the \fBphot\fR task performs the following functions:
.IP [1]
reads in the \fBphot\fR task parameters including the input image name,
the input coordinate file name, the output photometry file name,
the \fBdatapars\fR, \fBcenterpars\fR, \fBfitskypars\fR, and \fBphotpars\fR
algorithm parameters, and determines whether the task mode of operation
is interactive or non-interactive
.IP [2]
reads in the initial coordinates of a star from the coordinate list and/or
the image cursor, and computes new coordinates for the star using the centering
algorithm defined by the \fIcalgorithm\fR parameter (if \fIcalgorithm\fR
is not "none") using data in a box whose size is defined by the \fIcbox\fR
parameter
.IP [3]
computes the sky value for the star using the default algorithm
specified by the \fIsalgorithm\fR parameter and the data in an annulus of
pixels defined by the \fIannulus\fR and \fIdannulus\fR parameters
.IP [4]
computes the instrumental magnitude  and magnitude error for each star
inside the aperture radii
specified by the \fIapertures\fR parameter using fractional pixel techniques,
the computed sky value, the standard deviation of the sky pixels, and the
gain of the CCD
.IP [6]
sets the instrumental magnitude scale for the image using the
\fBphotpars\fR \fIzmag\fR parameter and the exposure time specified by
the \fBdatapars\fR \fIexposure\fR or \fIitime\fR parameters
.IP [7]
sets the magnitude(s) to INDEF for stars which are saturated or contain
bad data,
for which the aperture is partially off the image, for which a sky
value could not be computed, or for which the
signal is fainter than the background
.IP [8]
writes the results to the output photometry file

.NH 3
The Phot Algorithm Parameters

.PP
The critical \fBphot\fR algorithm parameters are \fIcalgorithm\fR,
\fIsalgorithm\fR,
\fIannulus\fR, \fIdannulus\fR, \fIapertures\fR, \fIdatamin\fR and
\fIdatamax\fR. These parameters are verified by \fBphot\fR
at startup time.
.PP
The \fIcalgorithm\fR parameter tells \fBphot\fR how to compute
centers for the objects in the coordinate list. Calgorithm should be "none"
if the coordinate
list was computed by \fBdaofind\fR or the coordinates are known to
be precise; otherwise calgorithm should one of "centroid", "gauss", or
"ofilter". "centroid" is quick and sufficiently accurate in most cases;
"gauss" and "ofilter" take longer but are more accurate. If calgorithm is
not "none",
\fBphot\fR will ask the user to verify the centering box size
\fIcbox\fR. \fIcbox\fR should be set to 5 or ~2 * \fIfwhmpsf\fR pixels wide
whichever is greater.
.PP
The \fIsalgorithm\fR parameter tells \fBphot\fR how to compute the sky
values. If the
fluctuations in the sky background are due primarily to crowding the
default choice
"mode" should be used. If the fluctuations in the sky background
are due to nebulosity or large galaxies
and the sky statistics are confused, "median", "centroid" or
"crosscor" might be the best choice. In cases where the
background is very low and the sky histogram is sparse or
undersampled "mean" might be the best choice.
.PP
The \fIannulus\fR and \fIdannulus\fR parameters tell \fBphot\fR the
position of the sky annulus with respect to the star. The sky region
must be far enough away from the star to avoid contamination from
the star itself, but close enough
to be representative of the intensity distribution under the star. Values of
~ 4.0 * \fIfwhmpsf\fR for both parameters are good starting values.
.PP
The \fIapertures\fR parameter tells \fBphot\fR the radius of the
photometry aperture. The photometry through this aperture sets the
instrumental magnitude scale for all the subsequent DAOPHOT
reductions.  \fIApertures\fR should be ~ 1.0 * \fIfwhmpsf\fR.
.PP
The \fIdatamin\fR and \fIdatamax\fR parameters are used to detect
bad data in the photometry and sky apertures. Bad data is removed from
the sky pixel list before sky fitting takes place so it is important
that datamax and datamin, but particularly datamin, be correct.
Stars which have bad data in the photometry apertures will have their
magnitudes set to INDEF and be flagged with an error.

.NH 3
Running Phot Non-interactively

.PP
The following example shows how to run \fBphot\fR in non-interactive
mode using the results of \fBdaofind\fR as input.

.YS
da> phot test default default

Centering algorithm (none) (CR or value):
        New centering algorithm: none
Sky fitting algorithm (mode) (CR or value):
        Sky fitting algorithm: mode
Inner radius of sky annulus in scale units (10.) (CR or value):
        New inner radius of sky annulus: 10. scale units 10. pixels
Width of the sky annulus in scale units (10.) (CR or value):
        New width of the sky annulus: 10. scale units 10. pixels
File/list of aperture radii in scale units (3.0) (CR or value): 3.0
        Aperture radius 1: 3. scale units 3. pixels
Minimum good data value (50.) (CR or value):
        New minimum good data value: 50. counts
Maximum good data value (24500.) (CR or value):
        New maximum good data value: 24500. counts

test     40.97     4.02  100.7955   17.218  ok
test     23.06     7.03  100.3257   17.650  ok
test     18.02     7.96  99.40262   17.484  ok
test     25.99    22.01  101.3196   16.800  ok
test     35.98    22.00  101.1601   16.373  ok
test      8.02    22.97  98.89139   16.603  ok
test     30.97    25.01  101.2904   17.051  ok
test     28.96    33.92  100.6189   17.782  ok
test     35.98    42.03   101.043   16.594  ok

.YE

\fBPhot\fR looks for an input star list called "test.coo.?",
creates a file called "test.mag.?", and verifies
the critical parameters. By default the verbose switch is set to "yes",
so a short summary of the results for each star is printed on the
terminal as it is computed.
.PP
\fBPhot\fR may also be run non-interactively from a coordinate list created
with the image cursor list task \fBrimcursor\fR as shown below. Note
that centering has been turned on, and the verify switch has been turned off.

.YS
da> display test 1 fi+

da> rimcursor > cursor.coo

da> page cursor.coo

41.02   4.033 101 \040
22.918  6.969 101 \040
18.123  7.849 101 \040
25.951 21.939 101 \040
35.736 21.744 101 \040
 7.947 23.016 101 \040
30.843 24.777 101 \040
28.984 33.779 101 \040
36.127 41.705 101 \040

da> phot test cursor.coo default calg=centroid verify- 

test     40.92     4.04  100.8871   17.222  ok
test     23.17     6.97  100.6163   17.666  ok
test     18.04     7.92  99.55305   17.487  ok
test     25.96    21.97  101.4161   16.801  ok
test     35.94    21.98  101.2101   16.373  ok
test      8.05    23.00  98.74371   16.601  ok
test     30.94    25.02  101.3224   17.052  ok
test     28.91    33.85  100.6207   17.786  ok
test     35.96    42.08  100.9039   16.591  ok\fR
.YE

The "centroid" algorithm
computes a new center by doing an intensity-weighted sum of the
x and y marginals, whereas the \fBdaofind\fR algorithm
fits a 1D gaussian to the marginal pixel distributions in x and y.
The following example shows the results for the almost equivalent
\fBphot\fR centering algorithm "gauss".

.YS
da> phot test cursor.coo default calg=gauss verify-

test     41.00     4.03  100.8698   17.219  ok
test     23.11     7.03  100.3567   17.653  ok
test     18.02     7.96  99.40262   17.484  ok
test     25.98    21.97  101.4021   16.801  ok
test     35.96    21.99  101.2101   16.373  ok
test      8.02    22.98  98.77907   16.601  ok
test     30.97    25.02  101.2904   17.051  ok
test     28.93    33.94  100.6726   17.783  ok
test     35.97    42.02   100.976   16.593  ok\fR
.YE

The positions produced by the "gauss" algorithm are closer to
the positions computed by the \fBdaofind\fR task, 
than those computed by the "centroid" algorithm.
However as the positions computed by \fBphot\fR are used as initial
positions by the DAOPHOT tasks,
it is usually not necessary to go to the more expensive "gauss" algorithm.

.NH 3
Running Phot Interactively

.PP
\fBPhot\fR can also be configured to run interactively using the
image display and image
cursor for coordinate input. In this mode the user loads the image
into the display and runs
\fBphot\fR interactively by turning the interactive switch on as
shown below.
When the program is ready to accept input the cursor will begin
blinking in the display window. The following series of steps will
do photometry on stars selected with the image cursor.

.YS
da> display test 1 fi+

da> phot test "" default interactive+ calgorithm=centroid
.YE

.IP ...
Execute the \fBv\fR keystroke command to verify the critical parameters.
.LP

.YS
Centering algorithm (centroid) (CR or value):
        New centering algorithm: centroid
Centering box width in scale units (5.) (CR or value):
        New centering box width: 5. scale units  5. pixels
Sky fitting algorithm (mode) (CR or value):
        Sky fitting algorithm: mode
Inner radius of sky annulus in scale units (10.) (CR or value):
        New inner radius of sky annulus: 10. scale units 10. pixels
Width of the sky annulus in scale units (10.) (CR or value):
        New width of the sky annulus: 10. scale units 10. pixels
File/list of aperture radii in scale units (3.) (CR or value):
        Aperture radius 1: 3. scale units 3. pixels
Standard deviation of background in counts (10.) (CR or value):
     	New standard deviation of background: 10. counts
Minimum good data value (50.) (CR or value):
        New minimum good data value: 50. counts
Maximum good data value (24500.) (CR or value):
        New maximum good data value: 24500. counts
.YE

.IP ...
Move the cursor to the stars of interest and tap the \fBspacebar\fR to do
the photometry.
.LP

.YS
test     40.92     4.04  100.8871   17.222  ok
test     23.17     6.97  100.6163   17.666  ok
test     18.04     7.92  99.55305   17.487  ok
test     25.96    21.97  101.4161   16.801  ok
test     35.94    21.98  101.2101   16.373  ok
test      8.05    23.00  98.74371   16.601  ok
test     30.94    25.02  101.3224   17.052  ok
test     28.91    33.85  100.6207   17.786  ok
test     35.96    42.08  100.9039   16.591  ok
.YE

.IP ...
Type \fBq\fR to quit image, and \fBq\fR again to exit task.
.LP

The coordinate file name has been set to "" so that initial
positions for the stars to be measured will be read from the image
cursor, and the centering algorithm has been temporarily changed
on the command line
from "none" to "centroid" so that new centers will be computed.
The user simply points the cursor to the stars to be measured and taps the
space bar to measure the star. This option is often useful for
picking up stars missed by \fBdaofind\fR in a previous iteration,
or in cases where the user only wishes to measure a small group of stars.

.NH 3
The Phot Output

.PP
\fBPhot\fR produces a large output file containing many parameters, 
intermediate and final results. The principle
quantities of interest are: 1) the position
of the star xcenter and ycenter, 2) the sky value,
its standard deviation, and the number of pixels used to compute
it, msky, stdev, and nsky 3) the total
counts inside the aperture and the effective area of the aperture,
sum and area 4) the magnitude and magnitude error in the 
aperture, mag and merr, 
and 5) the exposure time, airmass, filter, and time of observation, itime,
xairmass, ifilter, and otime.
.IP [1]
\fIXcenter\fR and \fIycenter\fR are the computed coordinates for
the detected objects in fractional pixels. They can be overlaid on the
displayed image with the \fBtvmark\fR command. These numbers should be compared
with the initial coordinates xinit and yinit, to which they will be equal
if the centering algorithm was "none", or to which they should be close
if the centering algorithm is "centroid", "gauss", or "ofilter" assuming
that the original x and y positions were reasonable.
.IP [2]
\fIMsky\fR, \fIstdev\fR and \fInsky\fR are the estimated sky value in counts,
its standard deviation in counts, and the number of pixels used to compute it.
Users should, check that the position of the sky annulus is reasonable
and, check that the msky, stdev, and nsky values are reasonable
for a few isolated stars before proceeding. 
.IP [3]
\fISum\fR and \fIarea\fR are the total counts (star + sky) in the 
photometry aperture and area is area of the aperture in pixels squared
and should be roughly
equal to PI * r ** 2 where r is the radius of the photometry aperture
in pixels.
.IP [4]
\fIMag\fR and \fImerr\fR are the magnitude and magnitude error respectively
computed as follows.

.YS
 mag = zmag - 2.5 * log10(sum - area * msky) + 2.5 * log10(itime)

merr = 1.0857 * sqrt((sum - area * msky) / gain + area * stdev ** 2
       + area ** 2 * stdev ** 2 / nsky) / (sum - area * msky)
.YE

Users should check that the exposure time \fIitime\fR is correct since it
is used to determine the instrumental magnitude scale. The correct value
of gain is also required in order to get a correct estimate of the
magnitude error. Stdev is the observed standard deviation
of the sky pixels not the predicted value.
.IP [5]
The remaining quantities itime, \fIxairmass\fR, \fIifilter\fR, and \fIotime\fR
should be checked for correctness, e.g., were they read correctly from the image
header.

.NH 3
Examining the Results of Phot

.PP
The user can check the results of \fBphot\fR in several 
ways.  The following command will mark  all
stars in the \fBphot\fR output file on the display in red.

.YS
da> display test 1
da> pdump test.mag.1 xcenter,ycenter yes | tvmark 1 STDIN col=204
.YE

The following command will mark all the stars whose magnitudes are INDEF on
the screen in green.

.YS
da> pdump test.mag.1 xcenter,ycenter "mag == INDEF" | tvmark \\ 
    1 STDIN col=205
.YE

The following command will plot magnitude error versus magnitude for all
the stars in the photometry file.

.YS
da> pdump test.mag.1 mag,merr yes | graph STDIN point+
.YE

The following command will plot a histogram of the magnitude distribution.

.YS
da> pdump test.mag.1 mag,merr yes | phist STDIN plot_type=box
.YE

The photometry file can be examined interactively with the \fBpexamine\fR
task as shown below.

.YS
da> pexamine test.mag.1 "" test
.YE
.IP ...
A vector plot of magnitude error versus magnitude appears on the screen.
.IP ...
To examine individual stars in the vector plot move the graphics cursor to a
star and  type \fBo\fR to get a record listing for the star,
followed by \fBr\fR, \fBc\fR, or \fBs\fR to see a radial profile plot,
contour plot, or surface plot respectively, of the star.
.IP ...
To activate the image cursor type \fBi\fR, move the cursor to a star
and type \fBo\fR to get a record listing
for the star, followed by \fBr\fR, \fBc\fR or \fBs\fR to draw the desired plot.
To reactivate the graphics cursor type \fBg\fR.
.IP ...
To plot magnitude error versus x coordinate for all the stars in the file, type
\fB:xcolumn xcenter\fR and \fB:ycolumn merr\fR followed by \fBp\fR to
redraw the plot.
.IP ...
To plot a histogram of the magnitudes of the objects type \fBh\fR.
.IP ...
Type \fBq\fR to quit.

.NH 2
Creating a Psf Star List with Pstselect

.PP
The psf model fitting routines require a list of bright isolated stars
well distributed over the image to use as psf model templates.
The \fBpstselect\fR 
task is used to select suitable candidate stars from the photometry file
for input to the psf modeling task \fBpsf\fR.

.NH 3
The Pstselect Algorithm

.PP
By default the \fBpstselect\fR task performs the following functions: 
.IP[1]
reads the task parameters including the input image name, input 
photometry file, and output psf star list, reads the \fBdatapars\fR
and \fBdaopars\fR algorithm parameters, and determines whether the
task will be run interactively or non-interactively
.IP [2]
reads the dimensions of the input image from the input image header, and 
the ids, x and y coordinates, magnitudes, and sky values of up to
\fImaxnstar\fR stars from the input photometry file
.IP [2]
assigns a large negative number to the magnitudes of all stars whose
measured magnitudes are INDEF in the input photometry file
.IP [3]
sorts the stars in order of increasing magnitude so that
the saturated and brightest stars are at the beginning of the list
.IP [4]
selects the
brightest \fImaxnpsf\fR stars (where maxnpsf is a number chosen by the user)
which are, not saturated, more than \fIfitrad\fR pixels away from the edge
of the input image, have no bad data within \fIfitrad\fR pixels,
and have no brighter neighbor stars within
(\fIpsfrad\fR + \fIfitrad\fR + 2) pixels
.IP [5]
writes the ids, x and y coordinates, magnitudes, and sky values 
of the selected stars as read from the input photometry list
to the output psf star list

.NH 3
The Pstselect Algorithm Parameters

.PP
The critical \fBpstselect\fR algorithm parameters are \fIpsfrad\fR,
\fIfitrad\fR, \fIdatamin\fR, and \fIdatamax\fR.
.PP
\fIPsfrad\fR and \fIfitrad\fR are used by \fBpstselect\fR to eliminate
potential psf stars which have bright neighbors.
For the test image these parameters are currently set
to 4 * \fIfwhmpsf\fR + 1 and 1 * \fIfwhmpsf\fR or 11 and 3 pixels respectively.
However as \fBpstselect\fR is the first task to actually use the values
of these parameters, the user should
check them here one more time before running \fBpstselect\fR.
\fIFitrad\fR should be ~ 1 * \fIfwhmpsf\fR
for optimal psf model computation and fitting so the user leaves it
at its current value of 3.0. \fIPsfrad\fR should be set to the radius at which
the profile of the brightest stars of interest disappear into the
noise. Normally 4 * \fIfwhmpsf\fR + 1 pixels is a good starting value for
this quantity.
If \fIpsfrad\fR is too small the fitted stars will not subtract completely
from the input image, if it is too big DAOPHOT
will consume cpu time doing unnecessary data extractions
and subtractions.
One way to check the value of the \fIpsfrad\fR parameter is to
use the \fBdaoedit\fR task to examine
radial profiles of isolated stars in the input image as shown below.

.YS
da> display test 1 fi+

da> daoedit test
.YE
	    
.IP ...
Move cursor to star at 36,42 and press the \fBr\fR key.
.IP ...
Examine the resulting radial profile and note that the stellar profile
disappears into the noise at ~4 pixels.
.IP ...
Move the cursor to the star at 8,23 and press the \fBr\fR key.
.IP ...
Examine the radial profile and note that this stellar profile
also disappears into the noise at ~4 pixels.
.IP ...
Set \fIpsfrad\fR to 5.0 pixels by typing the command \fB:psfrad 5.0\fR.
.IP ...
Type \fBq\fR to quit the \fBdaoedit\fR task.
.YE

.PP
The \fBpexamine\fR task and the input photometry file can also be
used to examine the radial profiles of isolated stars in the
photometry file. 

.YS
da> display test 1 fi+

da> pexamine test.mag.1 "" test
.YE

.IP ...
A plot of magnitude error versus magnitude appears on the screen.
.IP ...
Type \fBi\fR to activate the image cursor.
.IP ...
Move the cursor to the star at 36,42 and type \fBr\fR, adjust the
outer radius of the plot with the command \fB:router\fR if necessary,
e.g., \fB:router 10\fR.
.IP ...
Examine the radial profile and note that it disappears into the noise at a
radius of ~4 pixels.
.IP ...
Move the cursor to the star at 8,23 and type \fBr\fR.
.IP ...
Examine the radial profile and note that that it
also disappears into the noise at a radius of ~4 pixels.
.IP ...
Type \fBq\fR to quit the pexamine task.
.LP

The new value of \fIpsfrad\fR can be stored by editing the \fBdaopars\fR
parameter set with \fBepar\fR in the usual manner or on the command
line as shown below.

.YS
da> daopars.psfrad = 5.0
.YE

.PP
Why is the value of 5.0 pixels for \fBpsfrad\fR so different from the
original estimate of 11.0 ?
There are two reasons. Firstly the stars in artificial image
test are quite faint,
with the brightest peaking at ~400 counts above background. Their stellar
profiles disappear into the noise quite quickly. Secondly the artificial
stars are gaussian in shape with a  sigma \(~= 1.0 pixels.
Unlike real stars they have almost all their light
in the core and none in the wings. For realistic optical images
11.0 pixels rather than 5.0 would be a more reasonable choice
for \fIpsfrad\fR than 5.0.
.PP
The \fIdatamin\fR and \fIdatamax\fR parameters are used to reject
psf stars with bad data within \fIfitrad\fR pixels.
If \fIdatamin\fR and \fIdatamax\fR  are set correctly before
the \fBphot\fR task is run, these parameters are redundant as stars
with bad data inside the photometry aperture will have INDEF magnitudes.
.PP
At this point the user should check that the current value of the
\fImaxnstar\fR parameter is larger than the total number of stars
in the photometry file written by the \fBphot\fR task. If \fImaxnstar\fR
is too small, \fBpstselect\fR cannot read the entire input photometry
file into memory and potential psf stars may be missed.

.NH 3
How Many Psf Stars Should Be Selected ?

.PP
How many stars should the user select to create the psf model ?
An absolute minimum
set by the mathematics is 1 star for a constant psf model,
3 stars for a linearly variable psf model, and
6 stars for a quadratically variable psf model. A more reasonable minimum
suggested by Stetson (1992) is 3 stars
per degree of freedom or,
3 stars for a constant psf model, 9 stars for a linearly variable psf model,
and 18 stars for a quadratically variable psf model. If a variable psf model
is required, it is vitally  important that the psf star list
sample the region of interest in the input image completely and
reasonably uniformly.
As the contribution of each psf star to the psf model is weighted by
its signal-to-noise, the psf stars may cover a range in magnitude without
compromising the resulting psf model.

.NH 3
Running Pstselect Non-interactively

.PP
The following example shows how to run the \fBpstselect\fR task
in non-interactive mode.

.YS
da> pstselect image default default 3

Psf radius in scale units (5.):
        New psf radius: 5. scale units 5. pixels
Fitting radius in scale units (3.):
        New fitting radius: 3. scale units 3. pixels
Minimum good data value (50.) (CR or value):
        New minimum good data value: 50. counts
Maximum good data value (24500.) (CR or value):
        New maximum good data value: 24500. counts

Star 5 has been added to the PSF star list
 X:   35.98 Y:   22.00  Mag:  16.372  Dmin: 82.96088  Dmax: 535.1335
Star 9 has been added to the PSF star list
 X:   35.98 Y:   42.03  Mag:  16.594  Dmin: 80.25255  Dmax: 489.9732
Star 6 has been added to the PSF star list
 X:    8.02 Y:   22.97  Mag:  16.603  Dmin: 71.00896  Dmax: 436.3393

Total of 3 PSF stars selected\fR
.YE

By default \fBpstselect \fR looks for an input photometry file called
"test.mag.?" and writes an output psf star list called
"test.pst.?".

.NH 3
Running Pstselect Interactively

.PP
\fBPstselect\fR may also be run interactively. In this mode of operation
the stars selected
by \fBpstselect\fR are examined by the user and accepted or rejected on
the basis of the appearance of their mesh, contour or radial profile plots
until a total of \fImaxnpsf\fR psf stars is reached.
Stars from the input photometry file which do not meet the
\fBpstselect\fR task selection criteria,
can be added  to the psf star list by the user with the image cursor
until a total of \fImaxnpsf\fR psf stars have been selected.
.PP
The following example shows how to run \fBpstselect\fR in interactive
mode.

.YS
da> pstselect image default default 3 inter+ plottype=radial

Psf radius in scale units (5.):
        New psf radius: 5. scale units 5. pixels
Fitting radius in scale units (3.):
        New fitting radius: 3. scale units 3. pixels
Minimum good data value (50.) (CR or value):
        New minimum good data value: 50. counts
Maximum good data value (24500.) (CR or value):
.YE

.IP ...
The image cursor appears on the screen  ready to accept user input.
.IP ...
Type \fBn\fR to display the first potential psf star found by
\fBpstselect\fR, \fBa\fR to select the star, or \fBd\fR to delete it.
.IP ...
Repeat the previous step for 2 more stars.
.IP ...
Type \fBl\fR to list the selected psf stars.
.IP ...
Type \fBq\fR to quit the task.

.PP
By default \fBpstselect \fR looks for an input photometry file called
"test.mag.?" and writes an output psf star list called
"test.pst.?" as before.

.NH 3
The Pstselect Output

.PP
The output psf star list consists of the ids, x and y coordinates,
magnitudes, and sky values of the selected psf stars copied from the
input photometry file without change.

.NH 3
Examining and/or Editing the Results of Pstselect

.PP
The \fBpdump\fR and \fBtvmark\fR commands can be used to mark
and label the selected psf stars on the image display as shown in the
following example.

.YS
da> display test 1 fi+
da> pdump test.pst.1 xcenter,ycenter,id yes | tvmark 1 STDIN \\ 
    col=204 label+\fR
.YE

Bad stars can be removed from the psf star list using the displayed and
labeled image and
the text editor, the \fBpselect\fR task, or
the \fBpexamine\fR task.
.PP
The following command shows how to create a new psf star list
without the psf star whose id is "5" using the \fBpselect\fR task.

.YS
da> pselect test.pst.1 test.pst.2 "id != 5"\fR
.YE

.PP
The same operation can be accomplished using the \fBpexamine\fR task
as shown below.

.YS
da> pexamine test.pst.1 test.pst.2 test 
.YE

.IP ...
A message appears on the screen to the affect that
pexamine cannot plot x versus y since the default y
column merr is not in the input file.
.IP ...
The user types \fBh\fR to plot a histogram of the magnitudes
and notes that there are three stars in the histogram.
.IP ...
The user decides that the star with id number 5 marked on the
display should be deleted because it is too crowded.
.IP ...
The user types the \fBi\fR key to bring up the image cursor, moves
it to star number 5, types the \fBd\fR key to delete the star, and
the \fBp\fR key to replot the data.
.IP ...
The user types the \fBf\fR key to make the deletions permanent and the \fBe\fR
key to exit the task, and update the output catalog.
.LP

Finally the user marks the new list on the display image using a
different marking color.

.YS
da> pdump test.pst.2 xcenter,ycenter,id yes | tvmark 1 STDIN \\
    col=205 label+
.YE
.LP

.NH 2
Computing the Psf Model with Psf

.PP
The \fBpsf\fR task computes the psf model used by the
\fBpeak\fR, \fBnstar\fR, and \fBallstar\fR tasks to do psf fitting
photometry, by the \fBgroup\fR task to estimate magnitudes for stars
whose initial magnitudes are INDEF, and by the \fBaddstar\fR and
\fBsubstar\fR tasks to add stars to and subtract stars from an image.

.NH 3
The Psf Algorithm

.PP
By default the \fBpsf\fR task performs the following functions:
.IP [1]
reads the \fBpsf\fR task parameters including, the input image name,
the input photometry file name, the input psf star list name,
the output psf image name,
the output psf star list name, the output psf star group file name, and
the \fBdatapars\fR and \fBdaopars\fR algorithm parameters
.IP [2]
reads the ids, x and y coordinates, magnitudes, and sky values
of the first \fImaxnstar\fR stars in the input photometry file
.IP [3]
reads the ids, x and y coordinates, magnitudes, and sky values
of the candidate psf stars from the input psf star list and/or
the image cursor, rejecting stars
which are not in the input photometry file, are within \fIfitrad\fR pixels
of the edge of the image, are saturated (if 
the parameter \fIsaturated\fR is "no"),
or have bad data within \fIfitrad\fR pixels
.IP [4]
computes the analytic component of the psf model 
specified by the parameter \fIfunction\fR using, data within \fIfitrad\fR
pixels of each psf star, weights proportional to the signal-to-noise
ratio in each psf star, and non-linear least-squares fitting techniques
.IP [5]
computes the residuals of each psf star from the best fit analytic function
within a radius of \fIpsfrad\fR + 2 pixels
.IP [6]
scales the residuals for each psf star to match the intensity of the first
psf star, subsamples the scaled residuals in x and y  by a factor of 2,
weights the residuals by the signal-to-noise ratio of the psf star, 
and combines the scaled, subsampled, and weighted residuals
to  create 0, 1, 3, or 6, depending on the \fIvarorder\fR parameter,
psf model look-up tables
.IP [7]
repeats steps [5] and [6] \fInclean\fR times, down-weighting the contributions
to the psf model look-up table(s) of pixels with particularly large
residuals each time through the loop
.IP [8]
estimates magnitudes for the saturated psf stars (if any exist and
if the parameter \fIsaturated\fR is "yes"),
by fitting the current psf model to the wings of the saturated stars
using the \fBpeak\fR task fitting algorithm,
.IP [9]
computes the residuals of each saturated psf star (if any exist and they
were successfully fit) from the best fit
analytic function within a radius of \fIpsfrad\fR + 2 pixels, weights
the residuals by a factor of 0.5, and adds
the contribution of the scaled, subsampled, and weighted residuals 
to the psf model look-up table(s)
.IP [10]
writes the computed analytic function parameters and 
look-up tables to the output psf image
.IP [11]
identifies all stars within  (psfrad + 2 * fitrad + 1) pixels of
a psf star as psf star neighbors, and stars within (2 * fitrad) pixels of
the psf star neighbors as friends of the neighbors
.IP [12]
writes the ids, x and y coordinates, magnitudes, and sky values of
the final list of psf stars to the output psf star list, and the group
and star ids,
x and y coordinates, magnitudes, and sky values of the psf stars,
psf star neighbors, and friends of the psf star neighbors
to the output psf star group file

.NH 3
Choosing the Appropriate Analytic Function

.PP
DAOPHOT offers several choices for the functional form of the analytic
component of the psf model (see Appendix 8.2 for details).
To achieve the best fits and to minimize interpolation errors in
the psf model look-up tables,
users should choose the analytic function that most closely
approximates the stellar psf. The options are:
.IP [1]
\fBgauss\fR (2 parameters),
a 2D elliptical gaussian function aligned along the x and y axes of the image.
Gauss is generally the best choice for well-sampled, fwhmpsf >= 2.5 pixels,
ground-based images because the interpolation errors are small
and evaluation is efficient as the function is separable in x and y.
.IP [2]
\fBmoffat25\fR and \fBmoffat15\fR (3 parameters),
elliptical Moffat functions of beta 2.5 and 1.5 respectively which can
be aligned along an arbitrary position angle. The Moffat functions
are good choices for under-sampled ground-based data.
.IP [3]
\fBlorentz\fR (3 parameters),
an elliptical Lorentz function which can be aligned along an arbitrary position
angle. The Lorenz function is a good choice for old ST data since it has
extended wings.
.IP [4]
\fBpenny1\fR (4 parameters),
a two component model consisting of an elliptical gaussian core 
which can be aligned along an arbitrary position angle and
lorentzian wings aligned along the x and y axes of the image.
The Penny1 function is a good choice for a purely analytic psf model.
.IP [5]
\fBpenny2\fR (5 parameters),
a two component model consisting of an elliptical gaussian core 
aligned along an arbitrary position angle and lorentzian wings aligned
along an arbitrary position angle which may be different from that of the
core.  The Penny2 function is a good choice for a purely analytic psf model.
.IP [6]
\fBauto\fR (2, 3, 4 or 5 parameters),
try each of the 6 analytic psf functions in turn and select the one which
yields the smallest scatter in the fit. Users should use
this option with caution 
since the greater number of free parameters in some models may
artificially produce a fit with less scatter without significantly
improving the resulting photometry.
.IP [7]
\fBlist\fR (2, 3, 4 or 5 parameters),
check only those functions in a user specified list, e.g.
"gauss,moffat25,lorentz" and select the one that gives the smallest
scatter.
.PP
Users uncertain of which analytic function to choose should leave
\fIfunction\fR set to "gauss"
and only if the results prove unsatisfactory experiment with one of the
more complicated analytic functions.

.NH 3
The Analytic Psf Model

.PP
A purely analytic psf model may be computed
by setting the \fBdaopars\fR parameter \fIvarorder\fR = -1.
Analytic psf models are
constant, i.e. they have the same shape everywhere in the
image.
In the majority
of cases this is NOT the best modeling option,
as a better representation of the true psf is almost always obtained by
computing an empirical psf model composed of an
analytic function plus one look-up table.
.PP
An analytic psf model may be required to model severely undersampled
data because interpolation errors can produce large uncertainties
in the computed look-up tables and the resulting fits.
.PP
Fields which are so crowded that 
isolated psf stars are non-existent, may also require psf modeling and 
psf star neighbor subtraction with an analytic psf model, 
before a more accurate higher order model free of ghosts produced
by the psf star neighbors can be computed.
This step is particularly important if the field is very crowded AND the
psf is known to be variable.

.NH 3
The Empirical Constant Psf Model

.PP
Most users with typical ground-based optical
data choose to compute an empirical constant psf
model composed of an analytic component and a single look-up table,
by setting the \fBdaopars\fR parameter \fIvarorder\fR = 0.
This type of model is constant, i.e. the psf model has
the same shape everywhere in the image.
.PP
Because of interpolation errors, severely undersampled data may be better
fit with a purely analytic psf model as described in the previous section.
.PP
Fields which are so crowded that 
isolated psf stars are non-existent may require psf modeling and 
psf neighbor star subtraction with an analytic psf model, 
before an accurate look-up table free of ghosts caused by the bright
psf star neighbors can be computed.

.NH 3
The Empirical Variable Psf Model

.PP
Psf models which vary linearly or quadratically
with position in the image can be computed by setting
the \fIvarorder\fR parameter to 1 or 2 respectively. 
In the first case a total of 3 look-up tables will be computed;
in the second case 6 look-up tables will be computed.
Users should always begin their analysis with \fIvarorder\fR = -1 or
0 if their data is from a telescope/instrument combination that
is unfamiliar to them. Only if the patterns of the residuals around stars
fit and subtracted with a constant psf model show systematic variations
with position
in the image, should the user proceed to experiment with the variable
psf models.
.PP
In very crowded regions it may be necessary to compute a good
variable psf model iteratively, starting with \fIvarorder\fR = -1
and proceeding to \fIvarorder\fR = 2 by, computing the psf model,
fitting the psf model to the psf stars and their neighbors, subtracting
the psf star neighbors but not the psf stars from the original image,
increasing \fIvarorder\fR by 1, and recomputing the
psf model using the subtracted image, until all the psf stars and their
neighbors subtract out cleanly.

.NH 3
Rejecting Bad Data from the Psf Model

.PP
The \fBpsf\fR task uses the \fBdatapars\fR parameters \fIdatamin\fR
and \fIdatamax\fR to flag bad data.
If the \fBdaopars\fR parameter \fIsaturated\fR is "no", a prospective
psf star
will be rejected outright if it has high or low bad data inside the fitting
radius; if \fIsaturated\fR is "yes" a star with low bad data will
be rejected outright but one with high bad data will be flagged
as saturated and accepted. Except in rare cases (see below) users should leave
\fIsaturated\fR set to "no". Stars with bad data outside the fitting radius
but inside the psf radius are flagged, and the user warned, but are still
accepted as psf stars.
.PP
All data within one fitting radius of the unsaturated psf stars is weighted by
the signal-to-noise ratio of the psf star and used to compute the 
analytic component of the psf model.
Pixels which deviate strongly from the current best fit 
analytic function are down-weighted during the course of the fit.
.PP
After the analytic function is fit, the residuals of the psf star data
from the best fit analytic function are computed, scaled to the magnitude
of the first psf star, weighted by the signal-to-noise in the psf star,
subsampled for a factor
of 2 in x and y, and added into the look-up table(s).
If there are too few psf stars with
good data to compute a particular element of the look-up table(s),
\fBpsf\fR will quit with an error. If the \fBdaopars\fR parameter
\fInclean\fR > 0, deviant pixels contributing to the psf model look-up tables
are down-weighted and the look-up table(s) are recomputed \fInclean\fR
times.
.PP
For images where all the bright candidate psf stars are saturated and all the
remaining
candidate psf stars are faint, it may be necessary to use the faint stars to
compute the analytic component of the psf model and bright saturated stars
to compute the look-up tables(s).
In this circumstance the user must set the parameter \fIsaturated\fR 
to "yes" and include several saturated stars in the psf star list.
After the analytic function and an initial set of look-up tables(s)
is computed without using the saturated psf stars, the \fBpeak\fR task
fitting algorithm is used to compute accurate magnitudes for the
saturated psf stars by fitting the wings of the saturated stars to
the current psf model. New look-up table(s) are computed
which include the contributions weighted by 0.5 of the saturated psf stars.

.NH 3
The Model Psf Psfrad and Fitrad

.PP
The \fBdaopars\fR parameter \fIpsfrad\fR defines the region over which 
the psf model is defined. This radius should equal the radius at which the
radial profile of the brightest star of interest disappears into the noise,
e.g. \(~= 5 pixels for the test image as determined
in the section describing the
\fBpstselect\fR task. The fitting radius defines the region of data around
each psf star used to compute the analytic component of the psf model
and should be the larger of the numbers 3 and  1 * \fIfwhmpsf\fR pixels.

.NH 3
Modeling the Psf Interactively Without a Psf Star List

.PP
The psf can be modeled interactively without an initial list of candidate
psf stars by displaying the image and selecting candidate psf stars
with the image
cursor. Good candidate psf stars must be in the input photometry file,
have no neighbors within \fIfitrad\fR
pixels, and be free of cosmetic blemishes. 
.PP
The following example shows how to model the psf interactively without
using an initial psf star list.

.YS
da> display test 1 fi+

da> psf test default "" default default default

Analytic psf function(s) (gauss):
        Analytic psf function(s): gauss
Order of variable psf (0):
        Order of variable psf: 0
Psf radius in scale units (5.):
        New psf radius: 5. scale units 5. pixels
Fitting radius in scale units (3.):
        New fitting radius: 3. scale units 3. pixels
Minimum good data value (50.) (CR or value):
        New minimum good data value: 50. counts
Maximum good data value (24500.) (CR or value):
        New maximum good data value: 24500. counts

Warning: Graphics overlay not available for display device.

Computing PSF for image: test
9 stars read from test.mag.1
.YE

.IP ...
A message appears on the screen telling the user how
many stars have been read from the photometry file
(users should make sure that this is the entire star list)
and the image cursor begins blinking.
.IP ...
The user types the \fBa\fR keystroke at pixel 36,42
followed by another \fBa\fR keystroke after the default
plot appears,
to add star 9 psf star list. Star 6 at pixel 8,23 is added
to the psf star list in the identical manner.
.LP

.YS
Star 9 has been added to the PSF star list
 X:   35.98 Y:   42.03  Mag:  16.594  Dmin: 80.25255  Dmax: 489.9732
Star 6 has been added to the PSF star list
 X:    8.02 Y:   22.97  Mag:  16.603  Dmin: 71.00896  Dmax: 436.3393
.YE

.IP ...
The user types the \fBl\fR keystroke command to list the selected psf stars.
.LP

.YS
Current PSF star list
    Star:    9  X:   35.98 Y:   42.03  Mag:   16.59  Sky:      101.0
    Star:    6  X:    8.02 Y:   22.97  Mag:   16.60  Sky:       98.9
.YE
.LP

.IP ...
The user types the \fBf\fR keystroke command to compute the psf model.
.LP

.YS
Fitting function gauss    norm scatter: 0.03422394

Analytic PSF fit
    Function: gauss  X: 25.  Y: 25.  Height: 523.8066  Psfmag: 16.594
    Par1: 1.082032  Par2: 1.162063

Computed 1 lookup table(s)
.YE

.IP ...
The user reviews the model fit with the \fBr\fR keystroke command and
decides to keep both psf stars.
.LP

.YS
PSF star 9 saved by user
PSF star 6 saved by user
.YE

.IP ...
The user types the \fBf\fR keystroke command to remodel the psf.
.LP

.YS
Fitting function gauss    norm scatter: 0.03422394

Analytic PSF fit
    Function: gauss  X: 25.  Y: 25.  Height: 523.8066  Psfmag: 16.594
    Par1: 1.082032  Par2: 1.162063

Computed 1 lookup table(s)
.YE

.IP ...
The user types the \fBw\fR keystroke command  to save the psf model
followed by the \fBq\fR keystroke command, executed twice, to quit the task.
.LP

.YS
Writing PSF image test.psf.1.imh
Writing output PSF star list test.pst.1
Writing output PSF star group file test.psg.1
.YE

.PP
At this point the user has created an initial psf model in the image
test.psf.1, a list of the psf stars in test.pst.1, and a list of the
psf stars and their neighbors in the file
test.psg.1 respectively.
.PP
Users may occasionally see "Star not found" messages when 
selecting psf stars with the image cursor. This may mean: 1) that the star
is truly not in the input photometry file (this can be checked
with the \fBtvmark\fR task), 2) that the image cursor 
is more than \fImatchrad\fR pixels from the position of the star
in the input photometry file (either position the image cursor more
carefully by hand or increase the value of the \fImatchrad\fR parameter), or,
3) that the input photometry file contains
more than \fImaxnstar\fR stars (increase the value of the parameter
\fImaxnstar\fR so that it is greater than the number of stars in
the photometry file).

.NH 3
Fitting the Psf Model Interactively Using an Initial Psf Star List

.PP
The \fBpsf\fR task can also be run interactively using an initial list of
psf stars chosen by the user with the \fBpstselect\fR task.
If the \fBpsf\fR task parameter \fIshowpsf\fR is "yes" (the default),
the psf stars are read from the psf star list one at a time,
a mesh, contour, or radial profile plot is displayed in the graphics window,
and the user can accept or delete the star with the \fBa\fR or \fBd\fR
keystroke commands. If
\fIshowplots\fR is "no", the psf star list is read without intervention
by the user. In both cases new stars can be added to the end of the psf
star list with the image cursor in the usual manner.
.PP
A sample run is shown below.

.YS
da> display test 1 fi+

da> pdump test.pst.1 xcenter,ycenter,id yes | tvmark 1 STDIN \\ 
    col=205 label+
.YE

.IP ...
The user marks and labels the initial list of psf stars on the image display.
.LP

.YS
da> psf test default test.pst.1 default default default

Analytic psf function(s) (gauss):
        Analytic psf function(s): gauss
Order of variable psf (0):
        Order of variable psf: 0
Psf radius in scale units (5.):
        New psf radius: 5. scale units 5. pixels
Fitting radius in scale units (3.):
        New fitting radius: 3. scale units 3. pixels
Minimum good data value (50.) (CR or value):
        New minimum good data value: 50. counts
Maximum good data value (24500.) (CR or value):
        New maximum good data value: 24500. counts

Warning: Graphics overlay not available for display device.

Computing PSF for image: test
9 stars read from test.mag.1
.YE

.IP ...
The user rejects or accepts the stars in the .pst file by typing the
\fBd\fR or \fBa\fR keystroke commands respectively after the
default plot appears.
.LP

.YS
Star 5 rejected by user
Star 9 has been added to the PSF star list
 X:   35.98 Y:   42.03  Mag:  16.594  Dmin: 80.25255  Dmax: 489.9732
Star 6 has been added to the PSF star list
 X:    8.02 Y:   22.97  Mag:  16.603  Dmin: 71.00896  Dmax: 436.3393

2 PSF stars read from test.pst.1
.YE

.IP ...
The user types the \fBl\fR keystroke command to view the psf star
list one more time.
.LP

.YS
Current PSF star list
    Star:    9  X:   35.98 Y:   42.03  Mag:   16.59  Sky:      101.0
    Star:    6  X:    8.02 Y:   22.97  Mag:   16.60  Sky:       98.9
.YE

.IP ...
The user computes the psf model with the \fBf\fR keystroke command.
.LP

.YS
Fitting function gauss    norm scatter: 0.03422394

Analytic PSF fit
    Function: gauss  X: 25.  Y: 25.  Height: 523.8066  Psfmag: 16.594
    Par1: 1.082032  Par2: 1.162063

Computed 1 lookup table(s)
.YE

.IP ...
The user saves the psf model with the \fBw\fR keystroke command.
.LP

.YS
Writing PSF image test.psf.1.imh
Writing output PSF star list test.pst.2
Writing output PSF star group file test.psg.1
.YE

.IP ...
The user types the \fBq\fR keystroke command to quit the task.
.LP

The user notes that the output psf star list is given a version number
of 2 in this example, since version 1 was written by the \fBpstselect\fR task.

.NH 3
Fitting the Psf Model Interactively Without an Image Display

.PP
Users without access to an image display, may still run \fBpsf\fR
interactively by redirecting
the image cursor commands to the terminal as shown below.

.YS
da> set stdimcur = text

da> psf test default test.pst.1 default default default

Analytic psf function(s) (gauss):
        Analytic psf function(s): gauss
Order of variable psf (0):
        Order of variable psf: 0
Psf radius in scale units (5.):
        New psf radius: 5. scale units 5. pixels
Fitting radius in scale units (3.):
        New fitting radius: 3. scale units 3. pixels
Minimum good data value (50.) (CR or value):
        New minimum good data value: 50. counts
Maximum good data value (24500.) (CR or value):
        New maximum good data value: 24500. counts

Warning: Graphics overlay not available for display device.

Computing PSF for image: test
9 stars read from test.mag.1
.YE

.IP ...
The user rejects or accepts the stars in the .pst file by typing 
the \fBd\fR or \fBa\fR keystroke commands respectively at the prompt.
.LP

.YS
Star 5 rejected by user
Star 9 has been added to the PSF star list
 X:   35.98 Y:   42.03  Mag:  16.594  Dmin: 80.25255  Dmax: 489.9732
Star 6 has been added to the PSF star list
 X:    8.02 Y:   22.97  Mag:  16.603  Dmin: 71.00896  Dmax: 436.3393

2 PSF stars read from test.pst.1
.YE

.IP ...
The user types the \fBl\fR keystroke command at the prompt to
view the psf star list one more time.
.LP

.YS
Current PSF star list
    Star:    9  X:   35.98 Y:   42.03  Mag:   16.59  Sky:      101.0
    Star:    6  X:    8.02 Y:   22.97  Mag:   16.60  Sky:       98.9
.YE

.IP ...
The user computes the psf model by typing the \fBf\fR keystroke
command at the prompt.
.LP

.YS
Fitting function gauss    norm scatter: 0.03422394

Analytic PSF fit
    Function: gauss  X: 25.  Y: 25.  Height: 523.8066  Psfmag: 16.594
    Par1: 1.082032  Par2: 1.162063

Computed 1 lookup table(s)
.YE

.IP ...
The user saves the psf model by typing the \fBw\fR keystroke command
at the prompt.
.LP

.YS
Writing PSF image test.psf.1.imh
Writing PSF output star list test.pst.2
Writing PSF output star group file test.psg.1
.YE

.IP ...
The user types the \fBq\fR keystroke command at the prompt to quit the task.
.LP

Additional stars can be added to the psf star list by commands of
the form \fB":a id#"\fR or \fB"100.2 305.6 1 a"\fR typed in at the
terminal prompt.
The user should remember to reset the image cursor to the logical
image cursor with the command \fB"reset stdimcur = stdimage"\fR
after running the \fBpsf\fR task in "no image display" mode.

.NH 3
Fitting the Psf Model Non-interactively

.PP
Finally the psf model can be fit non-interactively by setting the
\fIinteractive\fR parameter to "no", and using the list of psf stars produced by
the \fBpstselect\fR task as input. This is the preferred method for computing
the psf model when the number of psf stars is large (e.g.  the psf
model to be computed is variable).

.YS
da> psf test default test.pst.1 default default default inter-
.YE

.NH 3
The Output of Psf

.PP
\fBPsf\fR writes an output psf star list test.pst.# containing the ids, x and
y coordinates, magnitudes, and sky values, copied from the
input photometry file,
of the psf stars actually used to compute the final psf model.
Because of the ability
to add and subtract stars within \fBpsf\fR itself, this list may be different
from the input psf star list if any. A sample output psf star list is
shown below.

.YS
#K IRAF       = NOAO/IRAFV2.10EXPORT    version    %-23s
#K USER       = davis                   name       %-23s
#K HOST       = tucana                  computer   %-23s
#K DATE       = 05-28-93                mm-dd-yr   %-23s
#K TIME       = 14:34:31                hh:mm:ss   %-23s
#K PACKAGE    = daophot                 name       %-23s
#K TASK       = psf                     name       %-23s
#K IMAGE      = test                    imagename  %-23s
#K PHOTFILE   = test.mag.1              filename   %-23s
#K PSTFILE    = test.pst.1              filename   %-23s
#K PSFIMAGE   = test.psf.1              imagename  %-23s
#K GRPSFILE   = test.psg.1              filename   %-23s
#K OPSTFILE   = test.pst.2              filename   %-23s
#K SCALE      = 1.                      units/pix  %-23.7g
#K OTIME      = 00:07:59.0              timeunit   %-23s
#K IFILTER    = V                       filter     %-23s
#K XAIRMASS   = 1.238106                number     %-23.7g
#K PSFRAD     = 5.                      scaleunit  %-23.7g
#K FITRAD     = 3.                      scaleunit  %-23.7g
#
#N ID    XCENTER   YCENTER   MAG         MSKY                                  \\
#U ##    pixels    pixels    magnitudes  counts                                \\
#F %-9d  %-10.3f   %-10.3f   %-12.3f     %-12.3f
#
9        35.980    42.029    16.594      101.043
6        8.022     22.970    16.603      98.891
.YE

.PP
\fBPsf\fR also writes an output psf star group photometry file 
test.psg.? containing the group ids, and the star ids, 
x and y coordinates, magnitudes, and sky values
copied from the input photometry file,
for the psf stars and their
neighbors. A sample psf star group file is shown below.

.YS
#K IRAF       = NOAO/IRAFV2.10EXPORT    version    %-23s
#K USER       = davis                   name       %-23s
#K HOST       = tucana                  computer   %-23s
#K DATE       = 05-26-93                mm-dd-yr   %-23s
#K TIME       = 16:10:48                hh:mm:ss   %-23s
#K PACKAGE    = daophot                 name       %-23s
#K TASK       = psf                     name       %-23s
#K IMAGE      = test                    imagename  %-23s
#K PHOTFILE   = test.mag.2              filename   %-23s
#K PSTFILE    = test.pst.2              filename   %-23s
#K PSFIMAGE   = test.psf.2              imagename  %-23s
#K GRPSFILE   = test.psg.2              filename   %-23s
#K SCALE      = 1.                      units/pix  %-23.7g
#K OTIME      = 00:07:59.0              timeunit   %-23s
#K IFILTER    = V                       filter     %-23s
#K XAIRMASS   = 1.238106                number     %-23.7g
#K PSFRAD     = 5.                      scaleunit  %-23.7g
#K FITRAD     = 3.                      scaleunit  %-23.7g
#
#N ID    GROUP XCENTER   YCENTER   MAG         MSKY                            \\
#U ##    ##    pixels    pixels    magnitudes  counts                          \\
#F %-9d  %-6d  %-10.3f   %-10.3f   %-12.3f     %-14.3f
#
9        1     35.980    42.029    16.594      101.043
8        1     28.958    33.924    17.781      100.619
6        2     8.022     22.970    16.603      98.891
.YE

There are two stellar groups, one group per psf star,
in this file. The first psf star
has a single neighbor so there are two stars in the first group.
The first star in a group is always the psf star. The header parameters
record the input and output image and file names, the name of the
computed psf model, and the values
of the parameters
\fIpsfrad\fR and \fIfitrad\fR used to define the psf star groups.
.PP
The psf image contains, in the image header, the values of the
parameters that were used
to compute the psf model, the best fit values of the parameters of
the chosen analytic function, and a record of all the
psf stars used to compute the psf, and  in the image pixels, the best fit
look-up table(s) of the residuals from the analytic function
subsampled by a factor of 2.
The psf image look-up table(s) can be plotted and examined just like any
other IRAF image.
.PP
A sample psf image header is shown below.

.YS
test.psf.2[23,23][real]: PSF for image: test
    No bad pixels, no histogram, min=unknown, max=unknown
    Line storage mode, physdim [23,23], length of user area 1540 s.u.
    Created Wed 16:10:47 26-May-93, Last modified Wed 16:10:47 26-May-93
    Pixel file 'tucana!/d0/iraf/davis/test.psf.2.pix' [ok]
    IRAF    = 'NOAO/IRAFV2.10EXPORT'
    HOST    = 'tucana  '
    USER    = 'davis   '
    DATE    = '05-26-93'
    TIME    = '16:10:48'
    PACKAGE = 'daophot '
    TASK    = 'psf     '
    IMAGE   = 'test    '
    PHOTFILE= 'test.mag.2'
    PSTFILE = 'test.pst.2'
    PSFIMAGE= 'test.psf.2'
    GRPSFILE= 'test.psg.2'
    SCALE   =                   1.
    PSFRAD  =                   5.
    FITRAD  =                   3.
    DATAMIN =                  50.
    DATAMAX =               24500.
    NCLEAN  =                    0
    USESAT  =                    F
    FUNCTION= 'gauss   '
    PSFX    =                  25.
    PSFY    =                  25.
    PSFHEIGH=             523.8066
    PSFMAG  =               16.594
    NPARS   =                    2
    PAR1    =             1.082032
    PAR2    =             1.162063
    VARORDER=                    0
    FEXPAND =                    F
    NPSFSTAR=                    2
    ID1     =                    9
    X1      =                35.98
    Y1      =               42.029
    MAG1    =               16.594
    ID2     =                    6
    X2      =                8.022
    Y2      =                22.97
    MAG2    =               16.603
.YE

This psf image header records that: the psf model
was computed with a gaussian
analytic function (function = gauss), the analytic
function has two parameters (npars=2)
whose values are 1.08 and 1.16 (par1 and par2 are the fwhm of the function
in x and y respectively in this case), the psf is constant but there is
1 look-up
table (varorder = 0),  no saturated stars were used to compute the psf
(usesat=no), and no cleaning of bad pixels was done to compute the
lookup table (nclean=0). The number of psf stars and their positions
and magnitudes
are also listed. The psf model is defined over a radius of 5
pixels (psfrad = 5.0), resulting in a square look-up table with dimensions
of 2 * (nint (2 * psfrad) + 1) + 1 pixels in x and y, and a fitting radius
of 3 (fitrad = 3.0) was used to compute the analytic portion of the psf
model.
.PP
The height of the best fit gaussian psfheigh is 523.81 counts.
The psf model has been assigned a magnitude of 16.594 which is the
magnitude of the first psf star in the input photometry file.
All fits to the psf model are scaled with respect to this
magnitude. Therefore a star which is twice as bright as the psf model will
have a fitted magnitude of ~15.841.
.PP
Psfx and psfy define the distance from the center of the input image to the 
center of the edge pixels in x and y respectively, e.g psfx =
(ncols - 1.0) / 2.0 and psfy = (nlines - 1) / 2.0. These numbers are used to
evaluate the psf model only if the model is variable, \fIvarorder\fR > 0.
.PP
If the value of \fIvarorder\fR  in this example had been 1 or 2 the
psf model image would have been 3-dimensional with 3 and 6 23 by 23
pixel look-up
tables in planes 1-3 and 1-6 of the image respectively.
In both cases planes 1-3 would contain the 0th, 1st order in x,
and 1st order in y Taylor expansion coefficients around the analytic function.
In the latter case planes 4-6 would contain the
2nd order in x, 2nd order in xy, and 2nd order in y Taylor expansion
coefficients.  If the value of \fIvarorder\fR
had been -1 no look-up tables would have been computed and the
psf model image would consist of an image header but no pixel file.

.NH 3
Checking the Psf Model

.PP
To check the accuracy of the psf model, the user must fit each psf
star and its neighbors and friends
as a group using the \fBnstar\fR task, and the psf model and psf star group
photometry file
produced by \fBpsf\fR as shown below. In the following example
the user has chosen to set the rejections
file to "", so that all stars, even those too faint to be properly
fit, will be placed in the same output file.

.YS
da> nstar test test.psg.1 default default ""

Recenter the stars (yes):
        Recenter the stars: yes
Refit the sky (no):
        Refit the sky: no
Psf radius in scale units (5.):
        New psf radius: 5. scale units 5. pixels
Fitting radius in scale units (3.):
        New fitting radius: 3. scale units 3. pixels
Maximum group size in number of stars (60):
        New maximum group size: 60 stars
Minimum good data value (50.) (CR or value):
        New minimum good data value: 50. counts
Maximum good data value (24500.) (CR or value):
        New maximum good data value: 24500. counts

Group:    1 contains  2 stars
        ID:     9  XCEN:    35.98  YCEN:    42.01  MAG:    16.60
        ID:     8  XCEN:    28.96  YCEN:    33.91  MAG:    17.73
Group:    2 contains  1 stars
        ID:     6  XCEN:     8.02  YCEN:    22.97  MAG:    16.63
.YE

The results of the fits will appear in test.nst.? as shown below.

.YS
da> page test.nst.1

#K IRAF       = NOAO/IRAFV2.10EXPORT    version    %-23s
#K USER       = davis                   name       %-23s
#K HOST       = tucana                  computer   %-23s
#K DATE       = 05-28-93                mm-dd-yy   %-23s
#K TIME       = 14:46:13                hh:mm:ss   %-23s
#K PACKAGE    = daophot                 name       %-23s
#K TASK       = nstar                   name       %-23s
#K IMAGE      = test                    imagename  %-23s
#K GRPFILE    = test.psg.1              filename   %-23s
#K PSFIMAGE   = test.psf.1              imagename  %-23s
#K NSTARFILE  = test.nst.1              filename   %-23s
#K REJFILE    = ""                      filename   %-23s
#K SCALE      = 1.                      units/pix  %-23.7g
#K DATAMIN    = 50.                     counts     %-23.7g
#K DATAMAX    = 24500.                  counts     %-23.7g
#K GAIN       = 1.                      number     %-23.7g
#K READNOISE  = 0.                      electrons  %-23.7g
#K OTIME      = 00:07:59.0              timeunit   %-23s
#K XAIRMASS   = 1.238106                number     %-23.7g
#K IFILTER    = V                       filter     %-23s
#K RECENTER   = yes                     switch     %-23b
#K FITSKY     = no                      switch     %-23b
#K PSFMAG     = 16.594                  magnitude  %-23.7g
#K PSFRAD     = 5.                      scaleunit  %-23.7g
#K FITRAD     = 3.                      scaleunit  %-23.7g
#K MAXITER    = 50                      number     %-23d
#K MAXGROUP   = 60                      number     %-23d
#K FLATERROR  = 0.75                    percentage %-23.7g
#K PROFERROR  = 5.                      percentage %-23.7g
#K CLIPEXP    = 6                       number     %-23d
#K CLIPRANGE  = 2.5                     sigma      %-23.7g
#
#N ID    GROUP XCENTER   YCENTER   MAG         MERR          MSKY              \\
#U ##    ##    pixels    pixels    magnitudes  magnitudes    counts            \\
#F %-9d  %-6d  %-10.3f   %-10.3f   %-12.3f     %-14.3f       %-12.3f
#
#N         NITER SHARPNESS   CHI         PIER  PERROR                          \\
#U         ##    ##          ##          ##    perrors                         \\
#F         %-17d %-12.3f     %-12.3f     %-6d  %-13s
#
9        1     35.982    42.006    16.601      0.019         101.043           \\
           4     -0.019      0.512       0     No_error
8        1     28.962    33.912    17.730      0.074         100.619           \\
           4     0.026       1.093       0     No_error
6        2     8.017     22.968    16.628      0.021         98.891            \\
           3     0.022       0.558       0     No_error
.YE

In this example the chi values computed by \fBnstar\fR for the two
psf stars are lower than expected, ~ 0.5 instead of ~ 1.0, meaning that the
observed errors are less than the predicted errors.
This occurs because there are only 2 psf stars,
and therefore the model psf and the fitted psf stars are not totally
independent.
For example, if only one psf star is used to compute
the psf model, the chi
computed by \fBnstar\fR for that star would be ~ 0.0 and for any others
such as its neighbors ~ 1.0.
.PP
After checking that the chi values look reasonable, the user subtracts the
fitted psf stars and their neighbors from the input image
with the \fBsubstar\fR task, and examines the residuals
of the fit around the psf stars as shown below. After \fBsubstar\fR is
run the subtracted
image is displayed and the psf stars are marked in  green and the
psf neighbor stars are marked in red.

.YS
da> substar test default "" default default

Psf radius in scale units (5.):
        New psf radius: 5. scale units 5. pixels
Minimum good data value (50.) (CR or value):
        New minimum good data value: 50. counts
Maximum good data value (24500.) (CR or value):
        New maximum good data value: 24500. counts

SUBTRACTING - Star:    6 X =    8.02 Y =   22.97 Mag =   16.63
SUBTRACTING - Star:    8 X =   28.96 Y =   33.91 Mag =   17.73
SUBTRACTING - Star:    9 X =   35.98 Y =   42.01 Mag =   16.60

A total of 3 stars were subtracted out of a possible 3

da> display test.sub.1 1 fi+
da> pdump test.nst.1 xc,yc,id yes | tvmark 1 STDIN col=204 label+
da> pdump test.pst.1 xc,yc,id yes | tvmark 1 STDIN col=205 label+
.YE

.PP
The psf stars and their neighbors should subtract out cleanly
with no systematic patterns in the residuals as a function of distance
from the star (note this may not be the case if the psf is purely analytic
or so severely undersampled that the interpolation errors near the
center are very large), with magnitude, or with position in the image.
There should be no hidden
underlying neighbors revealed after the subtraction (these psf stars
should be rejected) or neighbors
that are not in the photometry file (this can be fixed up later).
The amplitudes of the fit residuals
should be consistent with the noise if sufficient stars are
used to determine the psf model.
.PP
The displayed and marked subtracted image and the output of \fBnstar\fR can
be examined in more detail with the \fBpexamine\fR task as shown below.

.YS
da> pexamine test.nst.1 "" test.sub.1
.YE
.IP ...
A plot of magnitude versus magnitude error for the psf stars and their
neighbors will appear.
.IP ...
Change the default plot to mag versus chi with the command \fB:ycolumn chi\fR
followed by the \fBp\fR keystroke command.
.IP ...
Activate the image cursor with the \fBi\fR keystroke command.
.IP ...
Move to psf star number 9 and type \fBr\fR to examine the radial profile
of the subtracted star, followed by \fBo\fR to get a listing of its position,
magnitude, magnitude error, sky value, etc.
.IP ...
Examine the radial profiles of the other subtracted psf stars and their
neighbors.
.IP ...
Type \fBq\fR to quit the task.

.PP
For the test image "test", examination of the radial profiles of the
subtracted stars shows that the residuals are
consistent with the noise in the image and have no unusual features
leading to the conclusion that the current psf model is a good representation
of the true psf.

.NH 3
Removing Bad Stars from the Psf Model

.PP
Bad psf stars detected after the psf star and neighbor subtraction,
for example those with
a cosmetic blemish, a close double, or an underlying neighbor star,
should be removed altogether from
the psf star list. This can be done by editing the psf star list written
by \fBpsf\fR with the text editor, with the \fBpselect\fR task and an
expression, e.g. "id != 5", specifying the star to be deleted, or
with the interactive \fBpexamine\fR task using the delete and update keys,
and rerunning \fBpsf\fR with the new psf star list.
.PP
This step is not required for the test image as both psf stars and their
neighbors subtracted cleanly from the image.

.NH 3
Adding New Stars to a Psf Star Group

.PP
Occasionally stars that are too faint to be included
in initial star list produced by \fBdaofind\fR and measured with \fBphot\fR, are
nevertheless sufficiently bright and close to a psf star
to affect the computation of the psf model.
Ideally the psf stars should have no such companions and/or the look-up
table cleaning option in the \fBpsf\fR task should minimize the problem of
undetected neighbors.  However in some cases it is necessary for
the user to intervene and add faint stars to the photometry list.
.PP
The easiest way to accomplish this
is to run \fBphot\fR interactively, selecting the
missing neighbor stars with the image cursor and appending the results
for the new neighbor stars to the photometry file produced by the first
run of \fBnstar\fR.

.YS
da> phot test "" psf.mag inter+ calgorithm=centroid
.YE

.IP ...
Point cursor to undetected psf star neighbors and hit spacebar.
.IP ...
Type \fBq\fR to quit.
.LP

.YS
da> prenumber psf.mag idoffset=5000
.YE

.IP ...
Renumber the stars in the new file starting with a number greater
than the number of stars in the original photometry file in order
to insure that all the stars have unique ids.
.LP

.YS
da> pfmerge test.psg.1,psf.mag test.psf.psg
.YE

.IP ...
Combine the psf star group photometry file produced by the \fBpsf\fR task
with the photometry file for the new psf star neighbors produced by
the \fBphot\fR task.
.LP

Users should note that there is no input coordinate list to \fBphot\fR in
this case.  Therefore the coordinate list is set to "", interactive mode
is on, and centering is turned on.
The remaining photometry parameters should be set exactly as they were in the
first run of \fBphot\fR.
.PP
This step is not required for the test image as all the significant
psf star neighbors were detected by the first run of the \fBdaofind\fR task.

.NH 3
Refitting the Psf Model With the New Psf Star Groups

.PP
After the \fBpsf \fR and \fBphot\fR results have been merged, the
user must regroup the psf stars and their neighbors with
the \fBgroup\fR task, and refit the
new groups with the \fBnstar\fR task.

.YS
da> group test test.psf.psg default test.grp  critov=.2 
.YE

.IP ...
Regroup the stars using a very small value for the critical overlap
parameter.
.LP

.YS
da> nstar test test.grp default default ""
.YE

.IP ...
Rerun nstar on the new psf star groups.
.LP

.YS
da> substar test test.nst.2 ""  default default
.YE

.IP ...
Check that the new psf star groups subtract cleanly from the original
image.
.PP
This step is not required for the test image as all the significant
psf star neighbors were detected by the first run of the \fBdaofind\fR task.


.NH 3
Computing the Final Psf Model

.PP
Once the psf star and psf star neighbors subtract out cleanly
from the input image with the current psf model,
a final psf model should be computed using an image from which all 
the psf star neighbors but not the psf stars have been subtracted.
To do this the user runs the \fBsubstar\fR task, setting the input photometry
file to the final output of \fBnstar\fR, and the exclude file to the final
psf star list written by the \fBpsf\fR task, and reruns \fBpsf\fR.
An example of this procedure is shown below.

.YS
da> substar test test.nst.2 test.pst.2 default default
da> psf test.sub.3 test.grp test.pst.2 test.psf.2 test.pst.3 \\ 
    test.psg.3 inter-
.YE

After this step the user should have a good psf model and can proceed to do
psf fitting photometry.
.PP
This step is not required for the test image as the single psf star
neighbor is sufficiently far from the psf star to have a negligible
effect on the computation of the psf model.

.NH 3
Visualizing the Psf Model with the Seepsf Task

.PP
The psf analytic function parameters are stored in the psf image header
and the look-up table(s) in the psf image pixels. The look-up table(s) are
subsampled by a factor of 2 with respect to the image, and cannot
be used directly to visualize what the psf model looks like
at the scale of the image. The task \fBseepsf\fR can  be used to
do this conversion as shown below.

.YS
da> seepsf test.psf.3 psf3
.YE

The output image will contain a picture of what an ideal star of magnitude
equal to the magnitude of the psf (see the psfmag keyword in the psf
image header) should look like at the center of the image. 
.PP
In the case of a variable psf the appearance of the psf model can be examined at
various 
places in the image by specifying a position at which to compute the psf
model.

.YS
da> seepsf test.psf.3 psf.13.8 xpsf=13 ypsf=8
.YE

.PP
The total power in a variable psf should be constant
as a function of position in the image even though its shape is
different. The variable psf fitting code
in DAOPHOT does perform flux conservation. Users can check this by using
the \fBimstatistics\fR task to check that there is no net power in the
look-up tables 2-3 or
2-6 if the psf order is 1 or 2. Similarly they can use \fBseepsf\fR to compute
the psf at various positions in the input image and \fBimstat\fR to check
that the net power in the psf is constant over the image
even though the shape of the
psf is variable.

.NH 3
Problems Computing the Psf Model

.PP
Computing the psf model is the most crucial step in DAOPHOT.
The \fBdaofind\fR and \fBphot\fR
steps are usually straight-forward, and the principal fitting task
\fBallstar\fR runs entirely in batch once started. However computing
a good psf model requires user input.
.PP
This section suggests a few
things to check if the computed psf model is not doing a good
job of fitting and subtracting the psf or program stars. The user 
should check:
.IP [1]
that the sky annulus chosen in the \fBphot\fR step is neither too
close or too far from the stars. If the sky annulus is too close
the computed skies will tend to be too high and the psf model
will have too small an amplitude,
producing false halos around the fitted and subtracted stars. If the sky annulus
is too far away the computed sky value will not represent the sky under the
psf star well, adding scatter to the photometry.
.IP [2]
the psf radius.
If the stars appear to be well fit in the cores but have residual halos with a
sharp inner edge around
them then the psf radius may be too small. The psf radius needs to
be big enough to give good subtractions for the brightest stars of interest.
.IP [3]
that the analytic component of the psf function is appropriate for
the data.
If the data is somewhat undersampled, fwhmpsf < 2.5 pixels,
one of the Moffat functions may give a better fit to the data than
the Gauss function. If that data is extremely undersampled an analytic
function may do better than one involving look-up tables.
.IP [4]
the psf stars.
One or more of the psf stars may not be stars, may be doubles, or contain
bad data. Although psf does try to detect and down-weight bad data it may
not be completely successful. Users need to examine the subtracted image
for objects with bad residuals and for stars with large fitted chi values
and eliminate them.
.IP [5]
for psf variability with position in the image. The true psf may be variable
and inadequately fit by a constant psf model. The user should examine the
residuals around the subtracted psf stars to see if there are patterns
with position in the image and
increase the order of the psf model if these are detected.
Large fitted chi values may also be an indication of a poor psf model. 
.IP [6]
the distribution of the psf stars.
If the psf is variable the user must ensure that the psf stars adequately
cover the region of interest in the image. For example if there are no
psf stars in a certain portion of the image the psf may not
be well represented there.
.IP [7]
the data. If the psf is a higher order than quadratic \fBpsf\fR may
not be able to model it adequately. The user should check the image data
reduction history, and investigate any image combining, bad pixel and cosmic
ray removal
operations, etc., that may have fundamentally altered the data.
The data should also be checked for linearity.
.IP [8]
the noise model. If the chi values for the fitted psf stars are unusually
large or small, the effective readout noise and gain for the image
may not be correct. The user should check that these values are
being read from the image header correctly and that they are appropriate
for the data.


.NH 2
Doing Psf Fitting Photometry with Peak, Nstar, or Allstar 

.PP
There are three psf fitting
photometry tasks in DAOPHOT. The \fBpeak\fR task fits the current psf
model to stars individually; the \fBnstar\fR task fits the psf model to
stars in fixed stellar groups simultaneously; the \fBallstar\fR task
groups and fits the psf model to stellar groups dynamically
and subtracts the fitted stars from the input image.
\fBAllstar\fR is the
task of choice for the majority of users, but all three options are
discussed in the following sections.

.NH 3
Fitting Single Stars with Peak

.PP
\fBPeak\fR is the simplest psf fitting task.
\fBPeak\fR fits the psf model to the stars in the
input photometry list individually. Because \fBpeak\fR cannot fit stars in
groups as the \fBnstar\fR and \fBallstar\fR tasks do, and in
uncrowded frames aperture photometry is often simpler and just as accurate,
\fBpeak\fR has few unique functions.
However \fBpeak\fR can be useful in cases where the user wishes to: 1) improve
the signal to noise of faint stars by taking advantage of
\fBpeak's\fR optimal weighting scheme, 2) do astrometry of single
stars, 3) fit and remove single stars from the frame in order to 
examine the underlying light distribution.

.NH 4
The Peak Algorithm

.PP
By default the \fBpeak\fR task performs the following functions:
.IP [1]
reads the task parameters, including the name of
the input image, the input photometry file, the psf model, the 
output photometry and rejections files, and the
\fBdatapars\fR and \fBdaopars\fR algorithm parameter sets
.IP [2]
reads the id, x and y coordinates, magnitude, and sky value
of a star from the input photometry file
.IP [3]
rejects the star if it has an undefined sky value, too
few good data pixels to obtain a fit,  or is too faint
.IP [4]
extracts the image data within one fitting radius of each star and
performs an optimally weighted non-linear least-squares fit of the psf model
to the extracted data 
.IP [5]
rejects the star if its signal-to-noise ratio is too low
or a unique solution cannot be found
.IP [6]
computes the best fit x, y, and magnitude for the star
.IP [7]
writes the id, new x and y coordinates, sky value, new magnitude, magnitude
error, number of iterations required to fit the star, chi statistic,
and sharpness
statistic for the fitted star to the
output photometry file, and the id, last computed x and y position,
and sky value of the rejected star, to
the rejections file
.IP [8]
repeats steps [2]-[7] for each star in the input photometry list

.NH 4
Running Peak 

.PP
A sample run of the \fBpeak\fR task is shown below. The user is prompted
for all the input and output file names and asked to verify the
critical parameters \fIrecenter\fR, \fIfitsky\fR, \fIpsfrad\fR, \fIfitrad\fR,
\fIdatamin\fR, and \fIdatamax\fR.

.YS
da> peak test default default default default

Recenter the stars (yes):
        Recenter the stars: yes
Refit the sky (no):
        Refit the sky: no
Psf radius in scale units (5.):
        New psf radius: 5. scale units 5. pixels
Fitting radius in scale units (3.):
        New fitting radius: 3. scale units 3. pixels
Minimum good data value (50.) (CR or value):
        New minimum good data value: 50. counts
Maximum good data value (24500.) (CR or value):
        New maximum good data value: 24500. counts

Star:     1 X:    40.97 Y:     4.01 Mag:    17.22 Sky:   100.74
 FIT:  Star:     1 X:    41.01 Y:     4.03 Mag:    17.22 Sky =  100.74
Star:     2 X:    23.06 Y:     7.03 Mag:    17.65 Sky:   100.33
 FIT:  Star:     2 X:    23.02 Y:     7.05 Mag:    17.75 Sky =  100.33
Star:     3 X:    18.02 Y:     7.96 Mag:    17.48 Sky:    99.40
 FIT:  Star:     3 X:    17.98 Y:     8.00 Mag:    17.57 Sky =   99.40
Star:     4 X:    25.99 Y:    22.01 Mag:    16.80 Sky:   101.34
 FIT:  Star:     4 X:    25.99 Y:    22.01 Mag:    16.81 Sky =  101.34
Star:     5 X:    35.98 Y:    22.00 Mag:    16.37 Sky:   101.12
 FIT:  Star:     5 X:    35.97 Y:    22.02 Mag:    16.39 Sky =  101.12
Star:     6 X:     8.02 Y:    22.97 Mag:    16.60 Sky:    98.89
 FIT:  Star:     6 X:     8.02 Y:    22.97 Mag:    16.63 Sky =   98.89
Star:     7 X:    30.97 Y:    25.01 Mag:    17.05 Sky:   101.29
 FIT:  Star:     7 X:    30.96 Y:    25.04 Mag:    17.05 Sky =  101.29
Star:     8 X:    28.96 Y:    33.92 Mag:    17.78 Sky:   100.62
 FIT:  Star:     8 X:    28.96 Y:    33.93 Mag:    17.74 Sky =  100.62
Star:     9 X:    35.98 Y:    42.03 Mag:    16.59 Sky:   101.04
 FIT:  Star:     9 X:    35.98 Y:    42.01 Mag:    16.60 Sky =  101.04\fR
.YE

In this example the user chose to recenter the stars (almost always
the best choice), and not to refit the sky (usually the best choice).
Recentering should only be turned off if the initial centers in the input
photometry file are known to be very accurate, e.g. they
are derived from a better seeing
image or one that has gone through some image restoration program.
Users who elect to refit the sky,
should realize that they will almost certainly need to increase
the fitting radius to
obtain a reasonable fitted sky value.  Increasing the fitting radius
however may also increase the scatter caused by neighboring stars.
.PP
The fitted stars can be subtracted from the input image with the \fBsubstar\fR
task as shown below.

.YS
da> substar test test.pk.1 "" default default\fR
.YE

.PP

.NH 4
The Peak Output

.PP
Peak writes the quantities: id, xcenter, ycenter, mag, merr, msky,
niter, chi, sharp, pier, and perror to the output photometry file and the
rejections file.
.IP [1]
Id is the id number of the star as read from the input photometry file.
.IP [2]
Xcenter and ycenter are the best fit position of the star.
If the star was rejected xcenter and ycenter will be the computed
values of x and
y at the time it was rejected. If \fIrecenter\fR is "no", xcenter and ycenter
will be the position of the star in the input photometry file.
.IP [3]
Mag and merr are the best fit magnitude and magnitude error respectively.
The instrumental magnitude is computed relative to the
magnitude assigned to the
psf model.
Mag and merr are set to INDEF if the star cannot be fit to the psf model.
.IP [4]
Msky is the sky value in the input photometry file if fitsky = "no",
otherwise it is the
fitted sky value. If the star is not fit for some reason,
msky is the computed sky value
at the time the star was rejected.
.IP [5]
Niter is the number of iterations it took to fit the star. If this
number if equal to the \fBdaopars\fR parameter \fImaxiter\fR the user should be
suspicious of the computed positions, magnitudes, and sky values.
However as the convergence criteria are
conservative the star may still be reasonably well fit.
Niter is set to 0 if the star cannot be fit  to the psf model.
.IP [6]
Chi and sharp are measures of the goodness of fit and the shape
of the star respectively. Chi should be ~ 1.0. If it is not then,
either the object is not a single star, the noise model including
one or more of the gain, readout noise, flat-fielding error, and
interpolation error parameters for the image are incorrect, the
psf model is a poor representation of the true psf, or the input
image does not conform to the requirements of the DAOPHOT package.
Sharp is a measure of the difference between the observed width  
of the object and the width of the psf model. Stars should have a sharpness
value ~ 0.0, resolved objects a sharpness of > 0.0, and cosmic rays and similar
blemishes a sharpness of < 0. Chi and sharp are set to INDEF if the star
cannot be fit to the psf model.
.IP [7]
Pier and perror are an integer error code and error string respectively.
If no error was encountered during the fit  pier is 0 and perror is
"No_error". Stars are rejected by the \fBpeak\fR task if 1) the sky value of
the star is INDEF 2) there are too few good data pixels to fit the star
3) the fitting matrix is singular meaning a unique solution could not
be found 4) the star is too faint, i.e. its signal / noise < 2.0. A fifth
condition, the solution did not converge by \fImaxiter\fR iterations, is not
used to reject the star, although users should be suspicious of a star
for which niter = \fImaxiter\fR.


.NH 3 
Fitting Stars with Group, Grpselect, Nstar and Substar

.PP
Stars can be fit simultaneously in fixed groups using the
\fBnstar\fR task.
This psf fitting technique requires grouping the stars
into physically meaningful
associations with the \fBgroup\fR and/or the \fBgrpselect\fR tasks,
fitting the stars in each group simultaneously with the \fBnstar\fR task,
and subtracting
the fitted stars from the image with the \fBsubstar\fR task.
\fBNstar\fR is the task of choice when the user wishes to explicitly 
control the grouping process or fit stars in a small number of 
widely separated groups efficiently.
\fBNstar\fR is most commonly used to fit the psf model to the psf stars and
their neighbors.

.NH 4
The Group and Nstar Algorithms

.PP
By default the \fBgroup\fR task performs the following steps:

.IP [1]
reads the task parameters, including the name of the input image, the input
photometry file, the psf model, the output photometry
file, and the \fBdatapars\fR and \fBdaopars\fR 
algorithm parameter sets
.IP [2]
reads the ids, x and y coordinates, magnitudes, and sky values
of up to \fImaxnstar\fR stars in the input photometry file, computes an
approximate magnitude for the stars with INDEF magnitudes, and sorts
the stars in increasing order of y 
.IP [3]
finds all the stars which are within \fIpsfrad\fR + \fIfitrad\fR + 1 pixels
of a given star, evaluates the psf of the brighter star at a distance
of \fIfitrad\fR pixels from
the fainter, and if this value is larger than \fIcritovlap\fR
times the expected
error per pixel,  or the two stars are within  \fIfitrad\fR + 1 pixels of
each other, adds the star to the group
.IP [4]
writes the group and star ids, x and y coordinates, magnitudes and sky values
for all the groups, to the output group photometry file.

.PP
By default the \fBnstar\fR task performs the following steps:

.IP [1]
reads the task parameters including the name of
the input image, the input group file, the psf image, the 
output group photometry and rejections files and the \fBdatapars\fR and
\fBdaopars\fR algorithm parameter sets
.IP [2]
reads the group and star ids, x and y coordinates, magnitudes, and
sky values for all the stars in a group from the input group photometry
file
.IP [3]
extracts the data within psfrad + fitrad pixels
around the group and
performs a weighted least-squares fit of the psf model to the extracted
data
.IP [4]
rejects stars which have an undefined sky value, which are too faint (more
than 12.5 magnitudes fainter than the psf), which are
too noisy (faintest star in the group less than a 1.0, 1.5, or 2.0
sigma detection after 5, 10, and 15 iterations or convergence respectively),
for which there are too few good
pixels to compute a fit, for which a unique solution cannot
be found, which have merged with another star (fainter star < 0.37 *
fwhmpsf from a brighter star in the group), which are both too
noisy and too
close to a brighter star (a star is between .37 and 1.0 fwhm of
a brighter star and is a 1.0, 1.5, or 2.0 sigma detection before
iterations 5, 10, and 15 respectively), or which are in a group
too large (> than the value of the \fImaxgroup\fR parameter) to be reduced.
.IP [5]
estimates new x and y coordinates and magnitudes for each star in
the group
.IP [6]
iterates until all the stars in the group satisfy the convergence criteria
backing up the iteration counter by 1 each time a star is rejected from
the group to allow the remaining stars time to settle into a new fit
.IP [7]
writes the star and group ids, new x and y coordinates, sky values,
new magnitudes and magnitude errors, chi and sharpness statistics
for the fitted stars to the
output group photometry and rejections files
.IP [8]
repeats steps [2]-[7] for each group in the input group photometry file.

.NH 4
Running Group, Grpselect, and Nstar

.PP
Before \fBnstar\fR can be run the stars must be grouped with the
\fBgroup\fR task as shown below.

.YS
da> group test default default default

Psf radius in scale units (5.):
       	New psf radius: 5. scale units 5. pixels
Fitting radius in scale units (3.):
       	New fitting radius: 3. scale units 3. pixels
Critical overlap in stdevs per pixel (1.): .2
       	New critical overlap: 0.2 stdevs per pixel
Minimum good data value (50.) (CR or value):
       	New minimum good data value: 50. counts
Maximum good data value (24500.) (CR or value):
       	New maximum good data value: 24500. counts

Size of     Number of
group       groups

1            4
2            1
3            1

Total of 9 stars in 6 groups\fR
.YE

The critical overlap parameter \fIcritovlap\fR determines the degree
to which crowding and/or random photometric errors are expected/allowed
to influence the photometry. If the default value of 1 is required to group
all the stars into associations of <= the current value of 
\fImaxgroup\fR stars, then unavoidable random photometric errors and
crowding errors will affect the photometry about equally.
If a critical
overlap much greater than 1 is required, then crowding errors will
dominate the random photometric errors. If a critical overlap
much less than 1 does the
job then
unavoidable random photometric errors will dominate, and crowding errors
are relatively insignificant.
In 
the previous example the user chose to set \fIcritovlap\fR to a value
much smaller
than 1 to test whether random photometric rather than crowding errors will
dominate the photometry.
.PP
If the first run of \fBgroup\fR separates all the stars into groups of less than
60 all is well and the user can proceed to the \fBnstar\fR task. Otherwise
the \fBgrpselect\fR task must be used to select out the larger groups
and subdivide them as shown in the following example.

.YS
da> grpselect test.grp.1 small.grp 1 60
.YE

.IP ...
First separate out the small groups.
.LP

.YS
da> grpselect test.grp.1 big.grp.1 61 10000
.YE

.IP ...
Next separate out the large groups.
.LP

.YS
da> group test big.grp.1 default big.grp.2 critovlap=1.0
.YE

.IP ...
Rerun the group task on the large group file with a bigger value
of critovlap.
.LP

.YS
da> pconcat small.grp,big.grp.2 all.grp
.YE

.IP ...
Finally concatenate all the new group files together.
.LP
This step is not required for the test image since there are only a
few stars and the field is not very crowded.

.PP
Run \fBnstar\fR on the grouped photometry file and \fBsubstar\fR on the
fitted photometry file.

.YS
da> nstar test default default default default

Recenter the stars (yes):
        Recenter the stars: yes
Refit the sky (no):
        Refit the sky: no
Psf radius in scale units (5.):
        New psf radius: 5. scale units 5. pixels
Fitting radius in scale units (3.):
        New fitting radius: 3. scale units 3. pixels
Maximum group size in number of stars (60):
        New maximum group size: 60 stars
Minimum good data value (50.) (CR or value):
        New minimum good data value: 50. counts
Maximum good data value (24500.) (CR or value):
        New maximum good data value: 24500. counts

Group:    1 contains  1 stars
        ID:     1  XCEN:    41.01  YCEN:     4.03  MAG:    17.22
Group:    2 contains  2 stars
        ID:     2  XCEN:    23.04  YCEN:     7.07  MAG:    17.75
        ID:     3  XCEN:    17.99  YCEN:     8.00  MAG:    17.57
Group:    3 contains  3 stars
        ID:     5  XCEN:    35.98  YCEN:    22.01  MAG:    16.39
        ID:     7  XCEN:    30.99  YCEN:    25.04  MAG:    17.04
        ID:     4  XCEN:    26.00  YCEN:    22.01  MAG:    16.80
Group:    4 contains  1 stars
        ID:     6  XCEN:     8.02  YCEN:    22.97  MAG:    16.63
Group:    5 contains  1 stars
        ID:     8  XCEN:    28.96  YCEN:    33.93  MAG:    17.74
Group:    6 contains  1 stars
        ID:     9  XCEN:    35.98  YCEN:    42.01  MAG:    16.60

da> substar test default "" default default
.YE

The parameter \fImaxgroup\fR specifies the
maximum number of stars that \fBnstar\fR will
fit simultaneously. The default value of 60
is a conservative number based
on the observed numerical behavior of the matrix inversion routines.
.PP
For most crowded field photometry applications it is simpler and easier
to use the automated \fBallstar\fR task.

.NH 4
The Nstar Output

.PP
By default \fBnstar\fR writes the quantities: id, group, xcenter, ycenter,
mag, merr, msky, niter, chi, sharp, pier, and perror to the output
photometry and rejections files.
.IP [1]
Id and group are the star and group id numbers in the input group
photometry file.
.IP [2]
Xcenter and ycenter are the best fit coordinates of the star.
If the star was rejected xcenter and ycenter will be the best fit
values of x and y at the time it was rejected. If \fIrecenter\fR is "no"
xcenter and ycenter will be the position of the star in the input
group photometry file.
.IP [3]
Mag and merr are the best fit magnitude and magnitude error respectively.
The instrumental magnitude is computed relative to the magnitude
of the psf model.
Mag and merr are set to INDEF if the star cannot be fit to the
psf model.
.IP [4]
Msky is always the individual sky for the star in the input photometry
file regardless of whether  fitsky is "no" or "yes". In the former
case the actual value of the sky used in \fBnstar\fR is
the mean of all the sky values for all the stars in the group. In the
latter case it is a fitted parameter.
.IP [5]
Niter is the number of iterations it took to fit the star. If \fBniter\fR
is equal to the parameter \fImaxiter\fR the user should be
suspicious of the result. However since the convergence criteria are
quite tightly constrained the result may still be reasonable.
Niter is set to 0 if the star cannot be fit to the psf model.
.IP [6]
Chi and sharp are measures of the goodness of fit and the star's shape
respectively. Chi should be ~ 1.0. If it is not then
either the object is not a single star,
the noise model including the ccd gain and readout noise, the flat
fielding error and the interpolation error parameters assumed for the image are
not correct, the
psf model is a poor representation of the true psf, or the input
image does not conform to the requirements of the DAOPHOT package.
Sharp is a measure of the difference between the observed width  
of the object and the width of the psf model. Stars should have sharpness
values \(~= 0.0, resolved objects sharpness values > 0.0, and cosmic rays
and similar blemishes sharpnesses values < 0.0. Chi and sharp are set to INDEF
if the star cannot be fit to the psf model.
.IP [7]
Pier and perror are an integer error code and error string respectively.
If no error was encountered during the fit, pier is 0 and perror is
"No_error".

.NH 3
Fitting Stars With Allstar

.PP
\fBAllstar\fR groups, fits
and subtracts the fitted stars from the input image without intervention
by the user.
Because the grouping process is dynamic and the best
fit stars are fit and subtracted first, fewer weak stars
and noise spikes migrate to the position of stronger stars in \fBallstar\fR
than is the case with \fBnstar\fR. \fBAllstar\fR replaces the functionality
of the tasks \fBgroup\fR, \fBgrpselect\fR, \fBnstar\fR, and \fBallstar\fR.
\fBAllstar\fR is the task of choice for doing crowded
field photometry with DAOPHOT.

.NH 4
The Allstar Algorithm

.PP
By default the \fBallstar\fR task performs the following steps:
.IP [1]
reads the task parameters including the name of the input image, the input
photometry file, the psf
model, the output photometry and rejections files, the output
subtracted image, and the \fBdatapars\fR and \fBdaopars\fR algorithm
parameter sets
sets
.IP [2]
reads the ids, x and y coordinates, magnitudes, and sky values for
the first \fImaxnstars\fR stars in the input photometry file, rejecting
at the start stars which have undefined sky values or which
are too close to another star
.IP [3]
reads the original image into a working array and initializes
two scratch arrays containing 1) the noise model and 2) the residuals from
the current best fit for all the stars
.IP [4]
at the beginning of each iteration:
1) groups the stars into physical associations that contain fewer
than \fImaxgroup\fR
stars, regrouping as necessary until all the groups are less than
\fImaxgroup\fR or until the group is too dense to reduce, 2)
subtracts the current best fit for all the stars that are still unfit from
the working copy of the image and stores the results in the residuals
array 3) initializes the weight array for all the unfitted stars
.IP [5]
during each iteration: 1) extracts the data within fitrad pixels
around each star in each group from the residuals image, 2)
performs a weighted non-linear least-squares fit of the psf model to
the extracted 
data, ignoring bad pixels and down-weighting pixels that deviate too
far from the psf model, and 3) computes new x and y coordinates and
magnitudes for each star in each group
.IP [6]
after the fourth iteration 1) writes the id, new x and y coordinates,
sky value, new magnitude and magnitude error, number of iterations
required to fit the star, and the chi and sharpness statistic of stars
which meet the convergence criteria, to
the output photometry file, 2) subtracts the fitted star permanently from 
the working copy of the image, 3) updates the
noise model in the weight array, 4) and eliminates the star from
the active star list
.IP [7]
after the fourth iteration rejects stars which: 1) are too faint
(more than 12.5 magnitudes fainter
than the psf model), 2) have too low a signal-to-
noise ratio (1.0, 1.5 and 2.0 sigma detection after 5, 10, and 15 iterations
respectively), 3) have too few good
pixels to compute a fit, 4) do not permit a unique solution, 
5) have merged with another star (star is
< 0.37 * fwhmpsf from a brighter star), 6) are both too noisy
and too close to a neighbor star (star is between 0.37 and 1.0 * fwhmpsf from
a brighter star in the group and is a 1.0, 1.5, or 2.0 sigma
detection before iterations 5, 10, and 15 respectively),
or 7) are part of a group too dense to be reduced.
.IP [8]
writes out the final version of the work array into the output subtracted
image

.NH 4
Running Allstar

.PP
\fBAllstar\fR is run as shown below.

.YS
da> allstar test default default default default default

Recenter the stars (yes):
        Recenter the stars: yes
Refit the sky (no):
        Refit the sky: no
Psf radius in scale units (5.):
        New psf radius: 5. scale units 5. pixels
Fitting radius in scale units (3.):
        New fitting radius: 3. scale units 3. pixels
Maximum group size in number of stars (60):
        New maximum group size: 60 stars
Minimum good data value (50.) (CR or value):
        New minimum good data value: 50. counts
Maximum good data value (24500.) (CR or value):
        New maximum good data value: 24500. counts

NITER = 1
NITER = 2
NITER = 3
NITER = 4
FITTING:   ID:     1  XCEN:    41.01  YCEN:     4.03  MAG:    17.22
FITTING:   ID:     4  XCEN:    26.00  YCEN:    22.01  MAG:    16.80
FITTING:   ID:     7  XCEN:    30.99  YCEN:    25.04  MAG:    17.04
FITTING:   ID:     6  XCEN:     8.02  YCEN:    22.97  MAG:    16.63
FITTING:   ID:     8  XCEN:    28.96  YCEN:    33.93  MAG:    17.74
FITTING:   ID:     9  XCEN:    35.98  YCEN:    42.01  MAG:    16.60
NITER = 5
FITTING:   ID:     2  XCEN:    23.04  YCEN:     7.05  MAG:    17.75
FITTING:   ID:     3  XCEN:    18.00  YCEN:     7.99  MAG:    17.56
FITTING:   ID:     5  XCEN:    35.98  YCEN:    22.01  MAG:    16.39
.YE

.PP
Users can easily run \fBallstar\fR as a background job as shown below.

.YS
da> allstar test default default default default default verify-  \\ 
    >& allstar.out &
.YE

.NH 4
The Allstar Output

.PP
\fBAllstar\fR writes the following
quantities: id, xcenter, ycenter, mag, merr, msky, niter, chi, sharp,
pier, and perror to the output photometry and 
rejections files.
.IP [1]
Id is the id number of the star as read from the input photometry file.
.IP [2]
Xcenter and ycenter are the best fit position of the star.
If the star was rejected xcenter and ycenter will be the computed
values of x and
y at the time it was rejected. If \fIrecenter\fR is "no", xcenter and ycenter
will be the position of the star in the input photometry file.
.IP [3]
Mag and merr are the best fit magnitude and magnitude error respectively.
The instrumental magnitude is computed relative to the magnitude of the
psf model.
Mag and merr are set to INDEF if the star cannot be fit to the psf model.
.IP [4]
Msky is the sky value in the input photometry file if \fIfitsky\fR = "no",
otherwise it is the
recomputed sky value. If \fIfitsky\fR is "yes" the sky is recomputed
every third iteration after the current best fit for the star is
subtracted from the image data. The new sky value is set to the 
average of 40% of the sky pixels, centered on the median sky value,
which are inside the sky
annulus defined by the parameters \fIsannulus\fR and
\fIwsannulus\fR. The sky value is not recomputed
if there are fewer than 100 sky pixels in the specified sky annulus
even if \fIfitsky\fR is "yes".
If the star is not fit for some reason,
msky is the sky value at the time the star was rejected.
.IP [5]
Niter is the number of iterations it took to fit the star. If this
number is equal to \fImaxiter\fR the user should be
suspicious of the result. However as the convergence criteria are
conservative the star may still be reasonably well fit.
Niter is set to 0 if the star cannot be fit to the psf model.
.IP [6]
Chi and sharp are measures of the goodness of fit and the shape
respectively. Chi should be ~ 1.0. If it is not, then
either the object is not a single star, the noise model including
one or more of the gain, readout noise, flat-fielding error, and
interpolation error parameters for the image are incorrect, the
psf model is a poor representation of the true psf, or the input
image does not conform to the requirements of the DAOPHOT package.
Sharpness is a measure of the difference between the observed width  
of the object and the width of the psf model. Stars should have a sharpness
value ~ 0.0, resolved objects a sharpness of > 0.0, and cosmic rays and similar
blemishes a sharpness of < 0. Chi and sharp are set to INDEF if the star
cannot be fit for some reason.
.IP [7]
Pier and perror are an integer error code and error string respectively.
If no error was encountered during the fit,  pier is 0 and perror is
"No_error".

.NH 2
Examining the Output Photometry Files

.PP
The identical tools can be used to examine the output of the \fBpeak\fR,
\fBnstar\fR, and \fBallstar\fR tasks.  Some examples using the output of
\fBallstar\fR are shown below.
.PP
The following command produces a plot of magnitude error versus magnitude.

.YS
da> pdump test.als.1 mag,merr yes | graph point+
.YE

The following command produces a plot of chi versus magnitude.

.YS
da> pdump test.als.1 mag,chi yes | graph STDIN point+
.YE

The following command produces a plot of chi versus sharpness.

.YS
da> pdump test.als.1 sharp,chi yes | graph STDIN point+
.YE

The output photometry file can also be examined interactively
with the \fBpexamine\fR task and the displayed subtracted image.
Note that the fitted stars are marked in green and the rejected stars
are marked in red on the display.

.YS
da> display test.sub.1 1 fi+

da> pdump test.als.1 xcenter,ycenter yes | tvmark 1 STDIN col=205

da> pdump test.arj.1 xcenter,ycenter yes | tvmark 1 STDIN col=204

da> pexamine test.als.1 "" test.sub.1
.YE

.IP ...
A plot of magnitude error versus magnitude appears on the screen.
.IP ...
The user moves to a discrepant point in the graph and types o to get a
listing of the results for the star, r to get a radial profile plot
around the subtracted star, and concludes on the basis of the plots
that the bad chi value is due to the star being a close double.
.IP ...
The user types i to switch to image cursor mode,
moves to several other stars with poor subtractions and types
s to see a surface plot of the residuals.
.IP ...
The user types q to quit.

.NH 2
Problems with the Photometry

.PP
Bad chi values in, and poor
subtractions of, \fBpeak\fR, \fBnstar\fR or \fBallstar\fR
photometry can usually be traced to: 1) a psf model which was
poorly determined in the first place, e.g. poor choice of
parameters, bad choice of
psf stars or too few stars used for determining a good variable psf model,
2) data reduction problems e.g. the mean sky value was subtracted from the
image, the image statistics have been altered,  or cosmic ray removal
clipped the tops of the stars, to give a few of many examples,
or 3) the properties of the image, e.g. non-linearity, a psf which has
very high order variability or very undersampled data, make computation
of a good psf model difficult or impossible.
.PP
Bad chi values can also be caused by incorrect
values of gain and readout noise or by a data reduction operation
which has significantly affected the image statistics.
.PP
Poor sky fitting can also cause scatter
in the photometry. Users should carefully check the position of the 
sky annulus used in \fBphot\fR if they are seeing poor subtractions.
If the images have a rapidly varying
background due,  due for example to nebulosity, it might be useful to
check out the alternate sky fitting routines, median or centroid, in the
\fBphot\fR task. The refit sky option in \fBpeak\fR and \fBnstar\fR
should be exercised with caution
since a larger fitting radius is often required
to get a reasonable sky fit, than is required to get good positions
and magnitudes, and this in turn can cause more scatter
in the photometry due to the influence of neighbors. On the other
hand the refit sky option in \fBallstar\fR can often significantly
reduce scatter in very crowded regions since it can use data closer to
or even underneath (!) the star to improve the sky estimate.
Users who use this option  must remember to set the inner
radius of the \fBallstar\fR sky annulus to avoid the inner stellar
core region where there is a lot of noise in the subtraction.
.PP
After running \fBsubstar\fR on a file produced by the \fBpeak\fR
task, users will sometimes see large holes in the data at the
position of some subtracted
stars. This is usually caused by fainter stars (which are fit individually)
migrating to the position of a brighter nearby star and then being
subtracted out twice by \fBsubstar\fR. Keeping the fitting radius small
will help minimize this problem, but if it is frequent and the frame
is somewhat crowded, the user should run \fBnstar\fR or \fBallstar\fR
instead of \fBpeak\fR. 
.PP
A similar problem can be caused by users running \fBdaofind\fR
with a very low threshold and detecting a lot of noise spikes, which
then migrate to the positions of brighter stars
and cause scatter and holes in the subtracted \fBpeak\fR photometry,
or attach themselves to noise spikes in the stellar profiles and cause
scatter and holes in the subtracted \fBnstar\fR photometry. 
Similar problems can affect \fBallstar\fR photometry but to a much
lesser degree since the
stars are grouped dynamically and subtracted from the input data as
they are fit. For all three photometry tasks spurious detections 
can consume a lot of excess computer time because the stellar groups
become much larger.

.NH 2
Detecting Stars Missed By Daofind

.PP
In very crowded fields many new stars, missed by the first run of \fBdaofind\fR,
will be detected after
the first run of \fBpeak\fR+\fBsubstar\fR, \fBnstar\fR+\fBsubstar\fR,
or \fBallstar\fR. If there
are many "missed" stars
\fBdaofind\fR should be run on the subtracted image after increasing the
\fIthreshold\fR parameter to avoid detecting the residuals
of previously subtracted stars. If there
are only a few such stars they can be "detected" by creating a coordinate
file using the subtracted image and \fBtvmark\fR in interactive mode.
Examples of both techniques are shown below.

.YS
da> daofind test.sub.1 newstars.coo threshold=5.0
.YE

or

.YS
da> display test.sub.1 1 fi+

da> pdump test.als.1 xcen,ycen yes | tvmark 1 STDIN col=204

da> tvmark 1 newstars.coo inter+
.YE

.IP ...
Move cursor to missing stars and tap the \fBa\fR key to append them to the
output coordinate file.

.NH 2
Initializing the Missing Star Photometry with Phot

.PP
The next step is to get initial photometry for the "missing"
stars. The simplest way is
to run \fBphot\fR on the original image using the coordinate list created
by \fBdaofind\fR or \fBtvmark\fR, and the same algorithm parameters as
were used
in the first run of \fBphot\fR. It is also possible to use \fBphot\fR directly
in interactive mode to create a photometry file of missed stars. Both
options are shown below.

.YS
da> phot test newstars.coo newstars.mag
.YE

or

.YS
da> display test.sub.1 1 fi+

da> pdump test.als.1 xcen,ycen yes | tvmark 1 STDIN col=204

da> phot test "" newstars.mag centroid=calgorithm inter+
.YE

.IP ...
Point the cursor to the missing stars and tap \fBspacebar\fR.
.LP

Note that if the stars are or were marked with the cursor, the user must
turn centroiding on in order to center them correctly.

.NH 2
Merging Photometry Files with Pfmerge

.PP
The photometry file containing the aperture photometry for the new stars
can be combined with the best psf fitting photometry already computed
by the \fBnstar\fR or \fBallstar\fR tasks 
for the original star list, using the task \fBpfmerge\fR as shown below.
The \fBprenumber\fR task ensures that the new stars all have unique ids.

.YS
da> pfmerge test.als.1,newstars.mag newstars.als.1
da> prenumber newstars.als.1\fR
.YE


.NH 2
Refitting the Stars with Allstar

.PP
After the photometry  files have been merged a final run of \fBallstar\fR or
\fBgroup\fR+\fBnstar\fR+\fBsubstar\fR on the combined file in order
to compute accurate magnitudes for the new stars should be made
as shown below.

.YS
da> allstar test newstars.als.1 default default default default
.YE

.NH 2
Examining the Subtracted Image

.PP
The user should search the subtracted image for any remaining unfit
stars and perform another iteration of \fBdaofind\fR, \fBphot\fR,
\fBpfmerge\fR and \fBallstar\fR to computed fitted magnitudes for
the new objects.

.NH 2
Computing an Aperture Correction

.PP
The aperture correction is the number which must be added to the
fitted instrumental magnitudes computed by the \fBpeak\fR,
\fBnstar\fR, or \fBallstar\fR tasks to produce the total instrumental
magnitude.
.PP
In order to compute aperture corrections for an image with a constant psf model
the user must:
.IP [1]
identify several bright isolated stars in the input image or subtract
all the neighbors from around several bright stars such as the
psf stars using the current psf model and the \fBsubstar\fR task
.IP [2]
using a minimum aperture radius equal to the one used in \fBphot\fR to compute
initial aperture photometry for all the crowded field stars,
and a maximum aperture radius equal to the one through which
the instrumental magnitudes of the standard stars were or will be measured,
use the \fBphot\fR task to
do multi-aperture photometry of the stars identified in [1] through at
least five apertures
.IP [3]
run the \fBmkapfile\fR task in the PHOTCAL package on the aperture
photometry file produced in 2, to determine the aperture
correction for the image as shown below
.LP

.YS
da> phot test "" test.apmags calg=centroid aperture="3,3.5,4.0,4.5 5.0"
.YE

.IP ...
Do multi-aperture photometry of the selected stars.
.LP

.YS
da> mkapfile test.apmags 5 test.apcors  
.YE

.IP ...
Compute the aperture correction between apertures 1 and 5.

.PP
To compute compute aperture corrections for an image with a variable psf model
the user must:
.IP [1]
identify several bright isolated stars in the input image or subtract
all the neighbors from around several bright stars such as the
psf stars using the current psf model and the \fBsubstar\fR task
.IP [2]
using a photometry aperture equal to the one through which
the magnitudes of the standard stars were or will be measured,
use the \fBphot\fR task to
do aperture photometry of the stars identified in [1]
.IP [3]
extract the fitted magnitudes for these stars from existing \fBnstar\fR
or \fBallstar\fR photometry or recompute them using the
\fBnstar\fR or \fBallstar\fR tasks and the current psf model
.IP [4]
set the aperture correction to the mean difference between the fitted
magnitudes computed in [3] and the aperture photometry magnitudes
computed through the large aperture in [2]

.NH
References

.LP
.nf
Stetson, P. B. 1987 Pub .A.S.P., \fB99\fR, 191
Stetson, P. B., Davis, L.E. and Crabtree, D.B. 1989, in
    \fICCDs in Astronomy\fR, G.H. Jacoby, San Francisco: Astronomical
    Society of the Pacific, 289
Stetson, P. B. 19 Pub .A.S.P., \fB102\fR, 932
Stetson, P.B, 1992, \fIUser's Manual for DAOPHOT II\fR
Stetson, P. B. 1992  in \fIAstronomical Data Analysis Software and Systems I\fR,
    D.M. Worall, C. Biemesderfer, and J. Barnes, San Francisco: Astronomical
    Society of the Pacific, 297
.fi

.NH
Appendices

.NH 2
The Instrumental Magnitude Scale

.PP
The instrumental magnitude scale is set by the magnitude assigned
to the psf model, the quantity \fIpsfmag\fR stored in the psf image header.
Psfmag is the magnitude of the first psf star in the input photometry
file, usually but not always the file written by the \fBphot\fR task.
If magnitudes were measured through more than one aperture
in \fBphot\fR, the magnitude used will be the
magnitude through the smallest aperture. 

.NH 2
The Analytic Psf Models

.PP
The functional forms of the currently supported analytic psf models
are listed below.
The quantity A is a normalization factor. The  Pn  are
the parameters which are fit during the psf modeling process.

.nf
	z = x ** 2 / p1 ** 2 + y ** 2 / p2 ** 2
	gauss = A * exp (-0.5 * z)

	z = x ** 2 / p1 ** 2 + y ** 2 / p2 ** 2 + x * y * p3
	moffat15 = A / (1 + z) ** 1.5
	moffat25 = A / (1 + z) ** 2.5

	z = x ** 2 / p1 ** 2 + y ** 2 / p2 ** 2 + x * y * p3
	lorentz = A / (1.0 + z)

	z = x ** 2 / p1 ** 2 + y ** 2 / p2 ** 2
	e = x ** 2 / p1 ** 2 + y ** 2 / p2 ** 2 + x * y * p4
	penny1 = A * ((1 - p3) / (1.0 + z) + p3 * exp (-0.693*e))

	z = x ** 2 / p1 ** 2 + y ** 2 / p2 ** 2 + p5 * x * y
	e = x ** 2 / p1 ** 2 + y ** 2 / p2 ** 2 + x * y * p4
	penny2 = A * ((1 - p3) / (1.0 + z) + p3 * exp (-0.693*e))
.fi

.NH 2
The Error Model

.PP
The  predicted errors in the the DAOPHOT photometry are computed per
pixel as shown below, where terms 1, 2, 3, and 4 represent  the  readout
noise,   the   poisson  noise,  the  flat-fielding  error,  and  the
interpolation error respectively. The quantities  readnoise,  epadu,
I,  M,  p1,  and  p2 are the effective readout noise in electrons, the
effective gain in
electrons per ADU,  the  pixel  intensity  in  ADU,  the  PSF  model
intensity in ADU, the FWHM in x in pixels, and the FWHM in y in pixels.

.nf
	error = sqrt (term1 + term2 + term3 + term4)  (ADU)
	term1 = (readnoise / epadu) ** 2
	term2 = I / epadu
	term3 = (.01 * flaterr * I) ** 2
	term4 = (.01 * proferr * M / p1 / p2) ** 2
.fi


.NH 2
The Radial Weighting Function

.PP
The  radial weighting function employed by all the psf fitting tasks
is shown below, where dx and dy are  the  distance  of  the  pixel
in question from the centroid of the star being fit.

.nf
	wtr = 5.0 / (5.0 + rsq / (1.0 - rsq))
	rsq = (dx ** 2 + dy ** 2) / fitrad ** 2
.fi

.NH 2
Total Weights

.PP
The  total weight  assigned  each  pixel  in  the  fit  is the
following.

.nf
	wtp = wtr / error ** 2
.fi

.NH 2
Bad Data Detection

.PP
Pixels less than the good data minimum \fIdatamax\fR or greater than
the good data maximum  \fIdatamax\fR are rejected immediately from the
fit.
.PP
After a few iterations and if clipexp > 0, a clipping scheme to
reject bad data is enabled. The weights of the pixels are
recomputed  as follows. Pixels having a residual of cliprange sigma
will have their weight reduced by half.

.nf
	wt = wtp / (1.0 + (residual / error / chiold /
             cliprange) ** clipexp)
.fi

.NH 2
Stellar Mergers

.PP
In order for two stars to merge during the course of the psf fitting
process either their separation must be < 0.37 * FWHM of the psf model,
or their separation must be > 0.37 * FWHM but < 1.0 * FWHM of the
psf model and the signal-to-noise ratio of the fainter  is less than 1.0, 1.5,
or 2.0 after iterations 4, 9, and 14 respectively.

.NH 2
Faint Stars

.PP
Stars are considered to be too faint if they are more than 12.5
magnitudes fainter than the psf, or if after a certain number of iterations,
they have a signal-to-noise ratio less than 2.0.
