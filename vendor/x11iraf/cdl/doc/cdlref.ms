.RP
.de us
\\$1\l'|0\(ul'
..
.TL
A Reference Guide for the IRAF Client Display Library (CDL)
.AU
Michael Fitzpatrick
.AI
NOAO/IRAF Group
.sp 0.5
February 1997
.sp 0.5
\fIRevised: August 2001\fR
\fICurrent Version:  CDL V1.8\fR

.AB
The Client Display Library (CDL) is a host interface for C, Fortran or SPP
programs allowing them to display images or overlay graphics to display
servers such as \fIXImtool\fR, \fIDS9\fR, or \fISAOimage / SAOtng\fR.
High-level procedures allow IRAF or FITS images to be displayed simply, other
routines permit access to all other server functions (e.g. cursor and image
readback, frame selection, etc).  The library also features a number of
functions for doing image overlay graphics; supported graphics primitives
include numerous point shapes, lines, circles, ellipses, polygons, annular
shapes, and text.
.AE

.pn 1
.bp
.ce
.ps +3
\fBContents\fR
.ps -3
.sp 2
1\h'|0.25i'\fBIntroduction\fP\l'|5.6i.'\0\01
.sp 0.5
2\h'|0.25i'\fBGetting Started\fP\l'|5.6i.'\0\01
.sp 0.5
3\h'|0.25i'\fBServer Connections\fP\l'|5.6i.'\0\02
.br
\h'|0.25i'3.1\h'|0.75i'Domain Sockets\l'|5.6i.'\0\02
.br
\h'|0.25i'3.2\h'|0.75i'Named FIFO Pipes\l'|5.6i.'\0\03
.br
\h'|0.25i'3.3\h'|0.75i'Inet Sockets\l'|5.6i.'\0\03
.br
\h'|0.25i'3.4\h'|0.75i'User-Defined Connections\l'|5.6i.'\0\03
.sp 0.5
4\h'|0.25i'\fBImage Display\fP\l'|5.6i.'\0\03
.br
\h'|0.25i'4.1\h'|0.75i'Overview of the Display Process\l'|5.6i.'\0\03
.br
\h'|0.25i'4.2\h'|0.75i'Displaying IRAF Images\l'|5.6i.'\0\04
.br
\h'|0.25i'4.3\h'|0.75i'Displaying FITS Images\l'|5.6i.'\0\05
.br
\h'|0.25i'4.4\h'|0.75i'Displaying Raw Pixels\l'|5.6i.'\0\05
.br
\h'|0.25i'4.5\h'|0.75i'Frame Selection\l'|5.6i.'\0\05
.br
\h'|0.25i'4.6\h'|0.75i'Clearing the Display\l'|5.6i.'\0\05
.br
\h'|0.25i'4.7\h'|0.75i'Frame Buffer Selection\l'|5.6i.'\0\06
.br
\h'|0.4i'4.7.1\h'|0.95i'Automatic Selection\l'|5.6i.'\0\06
.br
\h'|0.4i'4.7.2\h'|0.95i'The Frame Buffer Configuration File\l'|5.6i.'\0\06
.br
\h'|0.25i'4.8\h'|0.75i'Image WCS Description\l'|5.6i.'\0\07
.br
\h'|0.4i'4.8.1\h'|0.95i'Image Mappings\l'|5.6i.'\0\07
.br
\h'|0.25i'4.9\h'|0.75i'Image Colormaps\l'|5.6i.'\0\08
.br
\h'|0.4i'4.9.1\h'|0.95i'Imtool Color Model\l'|5.6i.'\0\09
.br
\h'|0.25i'4.10\h'|0.75i'ZScale Intensity Mapping\l'|5.6i.'\0\09
.br
\h'|0.25i'4.11\h'|0.75i'Image Hardcopy\l'|5.6i.'\010
.br
\h'|0.25i'4.12\h'|0.75i'Image Cursor\l'|5.6i.'\010
.br
\h'|0.4i'4.12.1\h'|0.95i'Cursor Sampling\l'|5.6i.'\011
.br
\h'|0.25i'4.13\h'|0.75i'Image Readout\l'|5.6i.'\011
.br
\h'|0.25i'4.14\h'|0.75i'Subraster I/O\l'|5.6i.'\011
.sp 0.5
5\h'|0.25i'\fBGraphics Overlay\fP\l'|5.6i.'\011
.br
\h'|0.25i'5.1\h'|0.75i'Marker Coordinates\l'|5.6i.'\011
.br
\h'|0.25i'5.2\h'|0.75i'Mapping a Previously Displayed Image\l'|5.6i.'\011
.br
\h'|0.25i'5.3\h'|0.75i'Marking a Coordinate File\l'|5.6i.'\012
.br
\h'|0.25i'5.4\h'|0.75i'Marker Colors\l'|5.6i.'\012
.br
\h'|0.25i'5.5\h'|0.75i'Marker Types\l'|5.6i.'\012
.br
\h'|0.4i'5.5.1\h'|0.95i'Point\l'|5.6i.'\013
.br
\h'|0.4i'5.5.2\h'|0.95i'Line\l'|5.6i.'\013
.br
\h'|0.4i'5.5.3\h'|0.95i'Box\l'|5.6i.'\013
.br
\h'|0.4i'5.5.4\h'|0.95i'Circle\l'|5.6i.'\013
.br
\h'|0.4i'5.5.5\h'|0.95i'Polyline\l'|5.6i.'\014
.br
\h'|0.4i'5.5.6\h'|0.95i'Polygon\l'|5.6i.'\014
.br
\h'|0.4i'5.5.7\h'|0.95i'Ellipse\l'|5.6i.'\014
.br
\h'|0.4i'5.5.8\h'|0.95i'Circular Annuli\l'|5.6i.'\014
.br
\h'|0.4i'5.5.9\h'|0.95i'Elliptical Annuli\l'|5.6i.'\014
.br
\h'|0.4i'5.5.10\h'|0.95i'Text\l'|5.6i.'\014
.br
\h'|0.25i'5.6\h'|0.75i'Text Fonts\l'|5.6i.'\015
.br
\h'|0.4i'5.6.1\h'|0.95i'In-line Font Changes\l'|5.6i.'\015
.br
\h'|0.25i'5.7\h'|0.75i'Line Widths and Styles\l'|5.6i.'\015
.br
\h'|0.25i'5.8\h'|0.75i'Deleting Markers\l'|5.6i.'\016
.br
\h'|0.4i'5.8.1\h'|0.95i'Individual Markers\l'|5.6i.'\016
.br
\h'|0.4i'5.8.2\h'|0.95i'The Entire Overlay\l'|5.6i.'\016
.br
\h'|0.25i'5.9\h'|0.75i'Redrawing the Overlay\l'|5.6i.'\016
.sp 0.5
6\h'|0.25i'\fBANSI C Function Prototypes\fP\l'|5.6i.'\017
.sp 0.5
7\h'|0.25i'\fBFortran Language Binding Notes\fP\l'|5.6i.'\017
.sp 0.5
8\h'|0.25i'\fBSPP Language Binding Notes\fP\l'|5.6i.'\018
.sp 0.5
9\h'|0.25i'\fBIIS Protocol Description\fP\l'|5.6i.'\018
.sp 0.5
10\h'|0.25i'\fBVXIMTOOL Proxy/Display Server Usage\fP\l'|5.6i.'\019
.sp 0.5
11\h'|0.25i'\fBC Interface Summary\fP\l'|5.6i.'\021
.sp 0.5
12\h'|0.25i'\fBC Example Tasks\fP\l'|5.6i.'\023
.br
\h'|0.25i'12.1\h'|0.75i'Display Example\l'|5.6i.'\023
.br
\h'|0.25i'12.2\h'|0.75i'Interactive Graphics Overlay Example\l'|5.6i.'\027
.br
\h'|0.25i'12.3\h'|0.75i'Image Mosaic Example\l'|5.6i.'\031
.sp 0.5
13\h'|0.25i'\fBFortran Interface Summary\fP\l'|5.6i.'\033
.sp 0.5
14\h'|0.25i'\fBFortran Example Tasks\fP\l'|5.6i.'\035
.br
\h'|0.25i'14.1\h'|0.75i'Display Example\l'|5.6i.'\035
.br
\h'|0.25i'14.2\h'|0.75i'Interactive Graphics Overlay Example\l'|5.6i.'\036
.sp 0.5
15\h'|0.25i'\fBSPP Interface Summary\fP\l'|5.6i.'\038
.pn 1

.NH
Introduction
.LP
	For more than a decade IRAF has used a \fIdisplay server\fR as the
primary means for image display.  IRAF client tasks connect to the server
and send or read data using a modification of the IIS Model 70 protocol,
originally through named fifo pipes but more recently using unix domain
or inet sockets.  The advantage to this approach was that IRAF client tasks
could make use of the image display functionality without duplicating
the code needed for actually displaying the image.  The longtime disadvantage
was that the IIS protocol used was arcane and undocumented and therefore 
largely unavailable to applications outside of the IRAF project.  The
Client Display Library (CDL) provides a public C and Fortran interface for
displaying images and overlay graphics that is independent of the underlying
protocol used.
.LP
	Unlike the interface used by IRAF applications, the CDL is meant to
provide an easy-to-use, fully featured interface for applications that can
be easily evolved for future display servers, communications schemes, or
display functionality.  Indeed, the CDL is independent of IRAF itself (as
are the display servers) so display tasks can be written for any discipline
or application.
.LP
	While this guide assumes programs are written in C, Fortran programmers
should find the translation straightforward by referring to the Fortran
interface summary.  The package source files include example tasks as does
this guide; users with problems, questions, or bug reports are encouraged to
contact \fIiraf@noao.edu\fR.  A small code sample demonstrating the problem
would be very helpful in finding a solution to any reported problems.

.NH 
Getting Started
.LP
	All C programs must include the header file \fB"cdl.h"\fR in order
to get package definitions for constants such as colors and structure
definitions used.  The Fortran interface does not \fIrequire\fR anything
similar, however for fortran compilers which support an \f(CWinclude\fR
directive a \fBcdlftn.inc\fR file may be used to define symbolic constants
passed to procedures, this file must be included by each procedure using the
CDL.  Fortran programs not using this file must pass in the constants 
explicitly, needed values are found throughout this manual.  C procedures
which return an integer value will return a positive number to indicate an
error has occurred and print an error message, otherwise zero is returned.
.LP
	The \fBcdl_open()\fR procedure is used to establish a connection
to the server and initialize the package, it returns a CDL structure pointer
that is passed to other CDL procedures.  For C programs this means
a separate pointer may be maintained for each server connection, the Fortran
interface is limited to only one server connection per process since the
pointer is maintained internally.  The connection is terminated using the
\fBcdl_close()\fR procedure.  Between these two calls may be any combination
of CDL procedure calls for doing image display or overlay graphics.
.LP
	For example, the simplest possible program for displaying an IRAF
image would look something like:
.sp 0.5
.nf
		\f(CW#include "cdl.h"

		main (int argc, char *argv[])
		{
		    CDLPtr cdl = cdl_open ((char *)0);
		    cdl_displayIRAF (cdl, argv[1], 1, 1, 1, 1);
		    cdl_close (cdl);
		}\fR
.fi
.sp 0.5
.LP
This program displays band one of an image named on the command line to the
server in frame one using the default 512x512 frame buffer, zscaling the
pixels to 8-bit values automatically. No error checking is performed to verify
that a connection was established or that the argument is a valid IRAF image.
Most programs will be more complex than this but it should be clear that
image display from client applications is a now trivial operation.

.us "Synopsis"
.sp 0.5
.nf
	\f(CW#include "cdl.h"\fR

	\f(CWCDLPtr cdl_open (char *imtdev)\fR
	\f(CWvoid cdl_close (CDLPtr cdl)\fR
.fi

.NH
Server Connections 
.LP
	The \fBcdl_open()\fR procedure takes a single argument specifying the 
type of connection to make to the server, this routine also initializes
the CDL package.  If this is a NULL pointer the CDL will attempt to first
connect on a unix domain socket, if that fails the standard IRAF /dev/imt1*
fifo pipes are tried.  The syntax for the \fIimtdev\fR argument is as follows:
.sp 0.5
				\f(CW<domain> : <address>\fR
.sp 0.5
.LP
where <domain> is one of "\fBinet\fR" (internet tcp/ip socket), "\fBunix\fR"
(unix domain socket) or "\fBfifo\fR" (named pipe).  The form of the address
depends upon the domain, as illustrated in the examples below.  The address
field may contain up to two "%d" fields.  If present, the user's UID will be
substituted (e.g. "unix:/tmp/.IMT%d"). The default connection if no imtdev
is specified is "unix:/tmp/.IMT%d", failing that a connection is attempted
on the /dev/imt1[io] named fifo pipes.   
.NH 2
Domain Sockets
.LP
	Domain sockets are sockets created on the local host.  The connection
is usually faster than an inet socket and comparable to a fifo.  If the
socket name is specified with a '%d' field the client can be assured of a 
unique socket name for each user allowing multiple clients to be run on the
same host by different users.
.LP
.us "Example"
.sp 0.5
.nf
\f(CW
	/* Connection to a local host using socket domain socket. */
	if ((cdl = cdl_open ("unix:/tmp/.IMT%d")) == NULL) {
	     fprintf (stderr, "cannot open domain socket connection\\n");
	     exit (1);
	} \fR
.fi
.NH 2
Named FIFO Pipes
.LP
	This is the traditional approach, and the only one supported by
SAOimage (although recent versions contain support for sockets). Any named
fifo pipe may be used, the syntax for the \fIimtdev\fR string in this case is
.sp 0.5
		\fBfifo:\f(CW<input_fifo>\fB:\f(CW<output_fifo>\fR
.sp 0.5
.LP
.us "Example"
.sp 0.5
.nf
	\f(CW/* Connection to a local host using named fifo pipes. */
	if ((cdl = cdl_open ("fifo:/dev/imt1i:/dev/imt1o")) == NULL) {
	     fprintf (stderr, "cannot open fifo pipe connection\\n");
	     exit (1);
	} \fR
.fi
.NH 2
Inet Sockets
.LP
	Inet sockets are connections between hosts via a tcp/ip socket.
This permits connecting to the server over a remote network connection
anywhere on the Internet.    
.LP
.us "Example"
.sp 0.5
.nf
	\f(CW/* Connection to a local host using socket 5137. */
	if ((cdl = cdl_open ("inet:5137")) == NULL) {
	     fprintf (stderr, "cannot open inet socket connection\\n");
	     exit (1);
	}

	/* Connection to a remote internet host using socket 5137. */
	if ((cdl = cdl_open ("inet:5137:foo.bar.edu")) == NULL) {
	     fprintf (stderr, "cannot open inet socket connection\\n");
	     exit (1);
	} \fR
.fi
.NH 2
User-Defined Connections
.LP
	Since IRAF V2.10.3 client tasks have been able to use an \fBIMTDEV\fR
unix environment variable to set the connection type, the syntax of this 
variable is the same as described above.  If the \fIcdl_open()\fR procedure
is called with a NULL pointer the IMTDEV environment variable will
automatically be checked.  To explicitly use this (or any other) variable
in the client task the \fIcdl_open()\fR procedure may be called as
e.g.
.sp 0.5
.nf
	\f(CWif ((cdl = cdl_open (getenv("IMTDEV"))) == NULL) {
	     fprintf (stderr, "cannot open server connection\\n");
	     exit (1);
	}\fR
.fi
.NH
Image Display
.NH 2
Overview of the Display Process
.LP
	Basic image display is done most easily using the high-level
\fBcdl_displayIRAF()\fR, \fBcdl_displayFITS()\fR and \fBcdl_displayPix()\fR
procedures.  These routines automatically define an image WCS and mapping,
clear the frame, set the frame buffer configuration and center the image
in the display.  For most applications these are all that will be needed,
but the \fBcdl_writeSubRaster()\fR procedure can also be used to display an
image.  For example, to display one image in a mosaic or other cases where
the task needs low-level access to position the image or write raw pixel
values.
.LP
	In these cases it is the responsibility of the client program to
prepare the server for display.  The basic steps involved in displaying an
image include
.sp 0.5
.TS
center;
lB lB
- -
l lI.
Operation	CDL Procedure
Selecting the frame	cdl_setFrame()
Clear the frame	cdl_clearFrame()
Select the frame buffer configuration	cdl_selectFB()
Set the frame buffer configuration	cdl_setFBConfig()
Scale the image pixels to 201 display values	cdl_zscaleImage()
Compute the raster placement in the frame buffer	 
Construct a \fInode!path\fR image path
Set the image mapping	cdl_setMapping()
Define the image WCS	 
Set the image WCS	cdl_setWCS()
Write the pixels to the display	cdl_writeSubRaster()
.TE
.sp 0.5
In cases like a mosaic display, obviously some steps (e.g. clearing the
frame, selecting the configuration, etc) will only need to be done once.
XImtool V1.3 and later version support multiple WCSs in a single frame
so each piece of the mosaic should define a mapping and an independent
WCS.  The last step in the display here should be a single WCS for the
entire mosaic such as "\fIdetector coordinates\fR", without this the
coordinates used by default will be based on the last WCS sent to the
display.  Servers which do not support mappings will just ignore the
mapping information, but may still require a frame buffer WCS for other
tasks to operate correctly.  For simple displays of single images, the
high-level routines handle all of these steps automatically, they are
included here as checklist of what must be considered when using the CDL
for low-level display.
.NH 2
Displaying IRAF Images
.LP
	The \fBcdl_displayIRAF()\fR procedure can be used to display an
IRAF OIF format image (i.e. images with a \fI.imh\fR extension) by
simply passing in the image name.  Pixel files for the image must be
accessible from the local machine but can be in any directory, the HDR$
syntax for the imdir is also recognized.  Images may be three dimensional,
the \fIband\fR argument is used to select the image band to be displayed.
The \fIframe\fR and \fIfbconfig\fR arguments select the frame and frame
buffer size respectively, the special symbolic value \fBFB_AUTO\fR may be
used for the \fIfbconfig\fR argument to have the procedure automatically
select the frame buffer most appropriate for the image size.
If the \fIzscale\fR flag is greater than zero
the image will automatically be converted to 8-bit values using the zscale
mapping algorithm.  The function returns a positive value if the image
cannot be accessed or displayed for any reason, an error message will be
printed.
.LP
	The \fIcdl_isIRAF()\fR procedure returns a positive value if the
filename argument is recognized as an IRAF image, it does not check whether
the pixel file can be successfully accessed.  For simply reading the pixels
from an IRAF image the \fBcdl_readIRAF()\fR procedure may be used.  The
function returns a zero value and sets the output pixel array, image
dimensions and pixel size if successful, otherwise the function returns a
positive value.  Note that
the output pixel values may need to be scaled before they can be displayed.

.us "Synopsis"
.nf
	\f(CWint cdl_displayIRAF (CDLPtr cdl, char *fname, int band,
	    int frame, int fbconfig, int zscale)\fR
	\f(CWint cdl_isIRAF (char *fname)\fR
	\f(CWint cdl_readIRAF (char *fname, int band, uchar **pix,
	    int *nx, int *ny, int *bitpix, char *title)\fR
.fi
.NH 2
Displaying FITS Images
.LP
	The \fBcdl_displayFITS()\fR procedure can be used to display a
\fIsimple\fR FITS image by name.  A "simple" FITS file is assumed to be
one containing a single image and having no extensions.  Other types of
FITS files may of course be displayed but the client will have to use other
means to import the pixels.  FITS image extensions may be supported in a future
release of the CDL.  The \fIframe\fR and \fIfbconfig\fR
arguments select the frame and frame buffer size respectively, 
the special symbolic value \fBFB_AUTO\fR may be
used for the \fIfbconfig\fR argument to have the procedure automatically
select the frame buffer most appropriate for the image size.
If the
\fIzscale\fR flag is greater than zero the image will automatically be
converted to 8-bit values using the zscale mapping algorithm.  The function
returns a positive value if the image cannot be accessed or displayed for
any reason, an error message will be printed.
.LP
	The \fIcdl_isFITS()\fR procedure returns a positive value if the
filename argument is recognized as a simple FITS image.  For simply reading
the image pixels the \fBcdl_readFITS()\fR procedure may be used.  The
output pixel array, image dimensions and pixel size are returned if
successful otherwise the function returns a positive value.  Note that
the returned pixel values may need to be scaled before they can be displayed.

.us "Synopsis"
.nf
	\f(CWint cdl_displayFITS (CDLPtr cdl, char *fname, int frame,
	    int fbconfig, int zscale)\fR
	\f(CWint cdl_isFITS (char *fname)\fR
	\f(CWint cdl_readFITS (char *fname, uchar **pix, int *nx, int *ny,
	    int *bitpix, char *title)\fR
.fi
.NH 2
Displaying Raw Pixels
.LP
	The \fBcdl_displayPix()\fR procedure can be used to display an
arbitrary array of pixels of any size.  The \fInx\fR and \fIny\fR arguments
are the raster dimensions, and \fIbitpix\fR is the pixel size and has the
same meaning as the FITS BITPIX keyword.  The \fIframe\fR and \fIfbconfig\fR
arguments select the frame and frame buffer size respectively, 
the special symbolic value \fBFB_AUTO\fR may be
used for the \fIfbconfig\fR argument to have the procedure automatically
select the frame buffer most appropriate for the image size.
If the
\fIzscale\fR flag is greater than zero the image will automatically be
converted to 8-bit values using the zscale mapping algorithm.

.us "Synopsis"
.nf
	\f(CWint cdl_displayPix (CDLPtr cdl, uchar *pix, int nx, int ny,
	    int bitpix, int frame, int fbconfig, int zscale)\fR
.fi
.NH 2
Frame Selection
.LP
	Frame selection is normally done as an argument to one of the display
procedures, however frames may be explicitly selected using the
\fBcdl_setFrame()\fR procedure.  This allows client programs to essentially
"blink" frames independently, as long as the server supports multiple frames.
The \fBcdl_getFrame()\fR procedure may be used to get the current frame 
set in the server.

.us "Synopsis"
.nf
	\f(CWvoid cdl_setFrame (CDLPtr cdl, int frame)\fR
	\f(CWvoid cdl_getFrame (CDLPtr cdl, int *frame)\fR
.fi
.NH 2
Clearing the Display
.LP
	The current display frame may be explicitly cleared using the
\fBcdl_clearFrame()\fR procedure.  The frame is also cleared prior to 
displaying new images by the procedures \fBcdl_displayPix()\fR,
\fBcdl_displayFITS()\fR, and \fBcdl_displayIRAF()\fR.

.us "Synopsis"
.nf
	\f(CWint cdl_clearFrame (CDLPtr cdl)\fR
.fi
.NH 2
Frame Buffer Selection
.LP
	The default frame buffer used is 512x512, other sizes may be 
selected using the \fBcdl_setFBConfig()\fR procedure.  To set the frame
buffer size the client passes the frame buffer number as defined in 
the frame buffer configuration file (see below) while setting the image
WCS.  It is important to note that the frame buffer isn't actually changed
in the server until a subsequent \fBcdl_setWCS()\fR call, either directly
or through some other procedure which sets the WCS (e.g. one of the display
procedures).
.LP
	To get the size of the currently defined frame buffer the user
may call the \fBcdl_getFBConfig()\fR procedure.  This returns not only
the current configuration number, but the size as well.  To get the size
and any arbitrary configuration without actually setting it, the
\fBcdl_lookupFBSize()\fR procedure may be used.  Any configuration not actually
defined in the frame buffer configuration file is returned as the default
512x512 size.

.us "Synopsis"
.nf
	\f(CWvoid cdl_setFBConfig (CDLPtr cdl, int configno)\fR
	\f(CWvoid cdl_getFBConfig (CDLPtr cdl, int *configno, int *width,
	    int *height, int *nframes)\fR
	\f(CWvoid cdl_lookupFBSize (CDLPtr cdl, int configno, int *width,
	    int *height, int *nframes)\fR
.fi
.NH 3
Automatic Selection
.LP
	The \fBcdl_selectFB()\fR procedure may be used to select the most
appropriate frame buffer to use for a given image size.  If possible a frame
buffer the same size as the image will be used, otherwise one that is
larger will be chosen.  Rather than simply selecting the first configuration 
larger than the image, the procedure searches the entire configuration file
selecting the one with the least empty space in both dimensions.  If the 
\fIreset\fR flag is non-zero this frame is set automatically by the 
procedure, otherwise the selected dimension is simply returned to the 
calling program.  In either case the new frame buffer will not take effect
until a new WCS is defined for the frame.

.us "Synopsis"
.nf
	\f(CWvoid cdl_selectFB (CDLPtr cdl, int nx, int ny, int *fb,
		int *w, int *h, int *nf, int reset)
.fi
.NH 3
The Frame Buffer Configuration File
.LP
	The size of the frame buffer is not passed directly to the server
since this is not part of the communications protocol used.  Instead, the
frame buffer number is sent as part of the WCS header packet.  So that 
both the server and client can know that a particular frame buffer number
corresponds to a specific size, a \fIframe buffer configuration file\fR 
is used which both the client and server read.
.LP
	The default configuration file is /usr/local/lib/imtoolrc, this can
be overridden by defining an \fBIMTOOLRC\fR environment variable naming
the file to be used, or by creating a .imtoolrc file in your home directory.
Since the server must also read the same file, this must be done before
starting both the client and server applications.
.LP
The format of the frame buffer configuration file is
.sp 0.5
	\fIconfigno nframes width height [extra fields]\fP
.sp 0.5
e.g.
.sp 0.5
          1  2  512  512
          2  2  800  800
          3  1 1024 1024          # comment
          :  :   :    :

At most 128 frame buffer sizes may be defined, each configuration may define
up to 4 frames, configuration numbers need not be sequential but should be 
in ascending order.
.NH 2
Image WCS Description
.LP
	The image WCS is defined using the \fBcdl_setWCS()\fR procedure.  The
WCS defines a mapping between any linear coordinate system and the image
pixels, for our purposes we will discuss how the WCS is used to map the frame
buffer pixels to image coordinates.  It is passed to the server in a string
of the form:
.sp 0.5
.nf
                \fIname\fR - \fItitle\fR\\n
		a b c d tx ty z1 z2 zt
.fi
.sp 0.2
where:
.sp 0.2
              \fBX\fR' = a * \fBX\fR + c * \fBY\fR + tx
              \fBY\fR' = b * \fBX\fR + d * \fBY\fR + ty

The terms \fIa, b, c\fR, and \fId\fR define a rotation of the WCS wrt the 
pixel (i.e. frame buffer)
coordinates, the \fItx\fR and \fIty\fR values are translation terms relative
to the upper-left corner of the display.  The
remaining three values define the intensity mapping of the display pixels;
\fIz1\fR is the minimum pixel value used in the transformation, \fIz2\fR is
the maximum value, and \fIzt\fR defines the type of transformation used (0 for
none, 1 for linear, 2 for log10).  
.LP
	The WCS may be set explicitly by the calling program or a default
appropriate for the image will be set automatically by the high-level
display procedures, otherwise a WCS for the frame buffer is defined
(i.e. returned coordinates are frame buffer coords).  As an example of how
the WCS is defined, the default WCS for an image \fIIMX\fR x \fIIMY\fR
pixels in a frame buffer \fIFBX\fR x \fIFBY\fR pixels is defined as
.sp 0.5
.nf
\f(CW        a  =  1.0;                         /* no rotation */
        b  =  0.0;
        c  =  0.0;
        d  = -1.0;
        tx = (\fIIMX\f(CW / 2) - (\fIFBX\f(CW / 2) + 1;    /* center in FB */
        ty = (\fIFBY\f(CW / 2) + (\fIIMY\f(CW / 2);
        z1 = z1;                           /* zscale values */
        z2 = z2;
        zt = 1;\fR
.fi

.us "Synopsis"
.nf
	\f(CWint cdl_setWCS (CDLPtr cdl, char *name, char *title,
	    float a, float b, float c, float d, float tx, float ty,
	    float z1, float z2, int zt)\fR
	\f(CWint cdl_getWCS (CDLPtr cdl, char *name, char *title,
	    float *a, float *b, float *c, float *d, float *tx, float *ty,
	    float *z1, float *z2, int *zt)\fR
.fi
.NH 3
Image Mappings
.LP
	Beginning with \fBX11IRAF V1.3\fR the \fIXImtool\fR display server
has had the ability to use multiple world coordinate systems in a frame
(e.g. subrasters of a mosaic display).  To do this, the \fBIRAF\fR and 
\fBCDL\fR display interfaces were modified to pass in extra information 
with the WCS string to define the mapping of the image pixels to the frame
buffer pixels.  This extra information allows the XImtool to know when the
cursor in within one of the image subrasters and compute coordinates
appropriately.  (See the \fIXImtool\fR documentation for details on how
this is done exactly).
.LP
	The CDL will automatically determine when the connection is first
established whether the server is aware of this new mapping information.
Calls to send or receive mapping data will be ignored for servers which are
not aware of the extra data in the string.  For servers which can use the
mappings, and where a mapping has been provided,  the WCS string now looks
like
.sp 0.5
.nf
                \fIname\fR - \fItitle\fR\\n
		a b c d tx ty z1 z2 zt\\n
		\fBregion_name sx sy snx sny dx dy dnx dny\\n
		object_ref\R
.fi
.sp 0.5
where the new parameters are defined to be:
.sp 0.2
.TS
center;
lB lI.
region_name	User-defined name for the region.
sx, sy, snx, sny	Source rect in the object.
dx, dy, dnx, dny	Destination rect in the display frame buffer.
object_ref	Full node!/path image specification.
.TE
The \fIobject_ref\fR should be a complete node!path specification to the
image including any image section or extension.  This is needed by the
XImtool coordinate/pixel plug-in to map the same image as was displayed by
the client.  The path and node are required to ensure the image will be
found properly.  The \fIregion_name\fR can be anything such as \fIimage\fR,
\fIsubraster1\fR, or \fIccd3\fR.  The purpose of this field is to provide 
some named value for the mapping that may be useful to other client programs
needing to access the mapping.  \fISource\fR coordinates refer to the image
pixels, \fIdestination\fR coordinates refer to the frame buffer.
.LP
	Because we did not wish to change any of the existing interfaces,
mappings must be set prior to the \fBcdl_setWCS()\fR call  using the
\fBcdl_setMapping()\fR procedure.  The mapping is stored until the WCS is
actually sent.  Similarly, a mapping may be retrieved \fIafter\fR a
\fBcdl_getWCS()\fR call using the \fBcdl_getMapping()\fR procedure to 
return the values read with the last WCS retrieval.  Since there can at
times be more than one WCS in a frame, it's also possible to query the
mapping for a particular WCS by number (e.g. the WCS number returned in a
cursor read) using the \fIcdl_queryMap()\fR procedure.  See the sample
programs in the appendices for examples of how these procedures might be
called.

.us "Synopsis"
.nf
	\f(CWint cdl_getMapping(CDLPtr cdl, char *region,
	    float *sx, float *sy, int *snx, int *sny,
	    int *dx, int *dy, int *dnx, int *dny, char *ref);
	\f(CWint cdl_setMapping(CDLPtr cdl, char *region,
	    float sx, float sy, int snx, int sny,
	    int dx, int dy, int dnx, int dny, char *ref);
	\f(CWint cdl_queryMap(CDLPtr cdl, int wcs, char *region,
	    float *sx, float *sy, int *snx, int *sny,
	    int *dx, int *dy, int *dnx, int *dny, char *objref);
.fi

.NH 2
Image Colormaps
.LP
	The IIS protocol used does not permit the downloading of user-defined
colormaps, all images are loaded as raw grayscale values according to the
XImtool colormap model used by currently supported servers.  All images
containing private colormaps or more than the 201 grayscale values defined
by the Imtool colormap model must either convert the image to 8-bit
grayscale values by calling the CDL zscale procedures (\fBcdl_computeZscale()\fR
and \fBcdl_zscaleImage()\fR) or scale the images in client code with 
user LUTs.  The CDL zscale procedures scale image to 201 grayscale values
so that they are displayed to the full 8-bit range, user LUT
transformations or user code for converting to grayscale from a private
colormap procedures should do the same.
.NH 3
Imtool Color Model
.LP
	The IMTOOL color model defines at most 201 grayscale values for use in 
displaying the image, a set of 16 static colors are also defined for overlay
graphics.  Pixel values sent to the server should be already scaled to this 
model, i.e. the image pixels should be scaled to the range 1-200, values 
above this will either represent the overlay colors or will wrap around to
8-bit values.  The CDL zscale procedures will automatically scale
arbitrary pixel values to use this color model, the overlay procedures
assume color values are defined for the static color range 201-217 but any
8-bit value may be used.
.LP
	A summary of the color model values is included below:
.TS
center;
lB lB cB cB lB lB
l l c c l l.
Color	Description		Color	Description
0	Background		208	Cyan
1 - 200	Image data		209	Magenta
201	Cursor (white)		210	Coral
202	Background (black)		211	Maroon
203	White		212	Orange
204	Red		213	Khaki
205	Green		214	Orchid
206	Blue		215	Turquoise
207	Yellow		216	Violet
217	Wheat		218-255	undefined
.TE
.NH 2
ZScale Intensity Mapping
.LP
	Since most display servers are only capable of displaying 8-bit pixel
values,  images with more than 8-bits per pixel must be scaled prior to
display.  For linear transformations this is typically done using a simple
conversion of the image min/max values to the 256 grayscale values, however
this doesn't produce very good results when most pixel values are near one
of the extremes (usually the image min for astronomical images).  To solve
this IRAF uses a \fIzscale\fR mapping algorithm where a sampling grid is used
to approximate the image min/max values rather than computing it directly,
a line is then fit to these sample pixels to determine the optimal
transformation to the display values.  This is not only more efficient but
maps the most common pixel values to the display range producing a better image.
.LP
	The CDL has several routines for doing the same transformation: the
\fIcdl_computeZscale()\fR procedure is used to compute the optimal \fIz1\fR
and \fIz2\fR
values (the min/max used for the zscale transform) for an image of any pixel
size.  The \fIbitpix\fR argument is the number of bits-per-pixel for the input
array and has the same meaning as for the FITS \fIBITPIX\fR keyword. To then
transform the image using these values (or user-defined values) the
\fIcdl_zscaleImage()\fR procedure is used.  The input pixels are modified by
this procedure but the array is not reallocated to the smaller size needed by
an 8-bit array.  The \fBcdl_setSample()\fR and \fBcdl_setSampleLines()\fR
procedures can be used to change the sampling grid and number of sample
points (the default is 600 points on 5 lines).  The \fBcdl_setContrast()\fR
procedure can be used to change the default contrast adjustment to the slope
used in the transformation (the default is 0.25).  If a value of zero is
given then the minimum and maximum of the intensity sample is used as the
z1/z2 value.
.LP
	Each of the CDL display procedures has a \fIzscale\fR flag to
automatically scale the pixels prior to display.  Applications wishing to 
set their own z1/z2 values will need to call the zscale procedures and
disable this flag.  By default cdl_zscaleImage() will use a linear transform,
the \fBcdl_setZTrans()\fR procedure may be used to change this.  Acceptable
values are \fBCDL_UNITARY\fR (zero) for a unitary transform, \fBCDL_LINEAR\fR
(one) for a linear transform, or \fBCDL_LOG\fR (two) for a log10 transform.

.us "Synopsis"
.nf
	\f(CWvoid cdl_computeZscale (CDLPtr cdl, uchar *pix, int nx,
	    int ny, int bitpix, float *z1, float *z2)\fR
	\f(CWvoid cdl_zscaleImage (CDLPtr cdl, uchar **pix, int nx,
	    int ny, int bitpix, float z1, float z2)\fR

	\f(CWvoid cdl_setZTrans (CDLPtr cdl, int ztrans)\fR
	\f(CWvoid cdl_getZTrans (CDLPtr cdl, int *ztrans)\fR
	\f(CWvoid cdl_setZScale (CDLPtr cdl, float z1, float z2)\fR
	\f(CWvoid cdl_getZScale (CDLPtr cdl, float *z1, float *z2)\fR

	\f(CWvoid cdl_setSample (CDLPtr cdl, int nsample)\fR
	\f(CWvoid cdl_setSampleLines (CDLPtr cdl, int nlines)\fR
	\f(CWvoid cdl_setContrast (CDLPtr cdl, float contrast)\fR
	\f(CWvoid cdl_getSample (CDLPtr cdl, int *nsample)\fR
	\f(CWvoid cdl_getSampleLines (CDLPtr cdl, int *nlines)\fR
	\f(CWvoid cdl_getContrast (CDLPtr cdl, float *contrast)\fR

.fi
.NH 2
Image Hardcopy 
.LP
	While most servers include some hardcopy capability of their own the 
CDL provides two procedures for creating hardcopy images from the client
(e.g. for a batch processing application).  The client will typically read
back the entire image, frame buffer, of just a subraster and pass those
pixels to the print procedure.  Images will be written as Pseudocolor
Postscript (to preserve the overlay marker colors) and may be disposed to
a file using the \fBcdl_printPixToFile()\fR procedure or to any command string
accepting input from \fIstdin\fR (typically just an 'lpr' command) by using
the \fBcdl_printPix()\fR procedure.

.us "Synopsis"
.nf
	\f(CWint cdl_printPix (CDLPtr cdl, char *cmd, uchar *pix, int nx,
	    int ny, int annotate)\fR
	\f(CWint cdl_printPixToFile (CDLPtr cdl, char *fname, uchar *pix,
	    int nx, int ny, int annotate)\fR
.fi
.NH 2
Image Cursor
.LP
	The image cursor is read using the \fBcdl_readCursor()\fR procedure.
The returned value is the cursor \fI(x,y)\fR position as floating point value
in terms of the currently define image WCS.  Note that this position must
be converted to integer if it is to be used in one of the marker procedures.

.us "Synopsis"
.nf
	\f(CWint cdl_readCursor (CDLPtr cdl, int sample, float *x,
	    float *y, int *wcs, char *key)\fR
.fi

The \fIwcs\fR argument is defined as 

.nf
        wcs = frame * 100 + wcs_number
.fi

(where wcs_number=0 for frame buffer coords and 1 for image coords).  So,
you can get the frame as simply

.nf
        frame = (int) (wcs / 100)
.fi

.NH 3
Cursor Sampling
.LP
	If the cdl_readCursor() \fIsample\fR flag is non-zero the \fIlogical
image cursor\fR position is returned immediately, otherwise the display server
will wait for a keystroke before returning the cursor position.  The logical
image cursor is the last value set by a \fIcdl_setCursor()\fR call or the last
value returned by a \fIcdl_readCursor()\fR call.  When sampling the cursor 
position the keystroke value is undefined.
.NH 2
Image Readout
.LP
	The CDL maintains an internal knowledge of where an image has been
positioned if it was displayed using one of the \fIcdl_display*\fR procedures.
The \fBcdl_readImage()\fR procedure may be used to read back the entire image
pixels from the server ignoring the region of the frame buffer outside of
the image, the \fBcdl_readFrameBuffer()\fR procedure will read back the 
contents of the entire frame buffer.  The dimensions of the array are returned
in the \fInx\fR and \fIny\fR arguments.

.us "Synopsis"
.nf
	\f(CWint cdl_readImage (CDLPtr cdl, uchar **pix, int *nx,
	    int *ny)\R
	\f(CWint cdl_readFrameBuffer (CDLPtr cdl, uchar **pix,
	    int *nx, int *ny)\R
.fi
.NH 2
Subraster I/O
.LP
	The \fBcdl_writeSubRaster()\fR procedure is used to write an arbitrary
raster to any location in the display.  Similarly the
\fBcdl_readSubRaster()\fR procedure is used to read back an arbitrary raster.
When an image has previously been displayed the subraster position is given
in image coordinates (e.g. when writing a subregion of edited pixels),
otherwise the position is in frame buffer coordinates (e.g. to display
multiple images per frame you should use the cdl_writeSubRaster() call).
See the section on \fIMarker Coordinates\fR for further explanation of the
coordinate systems used.  

.us "Synopsis"
.nf
	\f(CWint cdl_writeSubRaster (CDLPtr cdl, int lx, int ly, int nx,
	    int ny, uchar *pix)\fR
	\f(CWint cdl_readSubRaster (CDLPtr cdl, int lx, int ly, int nx,
	    int ny, uchar **pix)\fR
.fi
.NH
Graphics Overlay
.NH 2
Marker Coordinates
.LP
	All marker positions are assumed to be image pixel coordinates, 
although there is no requirement that the position be on the image itself.
When an image WCS is defined (using the CDL display procedures or explicitly)
the origin of the coordinates used shifts from the frame buffer lower-left
to the lower-left of the image as displayed in the frame.   Negative
positions are allowed and will either refer to empty pixels if the frame
buffer is larger than the image, or pixels outside the frame buffer
boundaries.  Raster I/O requests will be clipped to the frame buffer
endpoints, a request completely outside the frame buffer is an error.
.NH 2
Mapping a Previously Displayed Image
.LP
	Ideally any application wishing to draw markers on an image will have
also displayed that image, however the \fBcdl_mapFrame()\fR procedure may
be used to map the requested frame for marker overlay.  It does this by
reading the WCS defined for that frame and assumes an image has been
displayed and centered in the frame buffer, then resets the internal CDL
image position.   For this reason it should not be used when multiple image
mappings have been displayed to a frame unless the marker overlay can be
done reliably using the last WCS displayed to the frame buffer.
If no image has been displayed the frame buffer is mapped
directly.  This can be used for example to map an empty frame for displaying
just the markers without an image, or for mapping another frame's WCS for
use on the current display.  The frame is not changed by the procedure call
however the current WCS \fIis\fR changed.

.us "Synopsis"
.nf
	\f(CWint cdl_mapFrame (CDLPtr cdl, int frame)\fR
.fi
.NH 2
Marking a Coordinate File
.LP
	Since a common function for programs will be to mark a list of
coordinates, the high-level \fBcdl_markCoordsFile()\fR procedure is
provided to make this easier.  The input parameters include a filename
expected to contain a set of (x,y) points (real or integer), and arguments
specifying the point type, size and color to draw.  If the \fIlabel\fR 
argument is positive each marker point will be labeled with it's relative
number in the file.  The size, type and color arguments all have the same
meaning as for the \fBcdl_markPoint()\fR procedure described below.

.us "Synopsis"
.nf
	\f(CWint cdl_markCoordsFile (CDLPtr cdl, char *fname, int type,
	    int size, int color, int label)\fR
.fi
.NH 2
Marker Colors
.LP
	Markers may be drawn using any 8-bit value, in order to use the 
static overlay colors the color must be in the range 201-217 (see above for
notes on the XImtool color model).  The "\fIcdl.h\fR" include file
for C programs, the "\fIcdlftn.inc\fR" include for fortran programs,
or the "\fIcdlspp.h\fR" include for SPP programs, defines the following
symbolic constants for each of the static overlay colors:
.sp 0.5
.TS
center;
lB l c c lB l.
C_BLACK	202			C_CORAL	210
C_WHITE	203			C_MAROON	211
C_RED	204			C_ORANGE	212
C_GREEN	205			C_KHAKI	213
C_BLUE	206			C_ORCHID	214
C_YELLOW	207			C_TURQUOISE	215
C_CYAN	208			C_VIOLET	216
C_MAGENTA	209			C_WHEAT	217
.TE
.NH 2
Marker Types
.LP
	Currently supported marker types include: \fI
.TS
center;
l l l l l.
Point	Line	Box	Polyline	Polygon
Circle	Circular Annuli	Ellipse	Elliptical Annuli	Text		
.TE
.LP
	\fRThe "\fIcdl.h\fR" include file for C programs, the
"\fIcdlftn.inc\fR" include for fortran programs, or the "\fIcdlspp.h\fR"
include file SPP programs, defines the following symbolic constants for
each of the defined \fIPoint\fR marker types:
.TS
center;
lB r c c lB r.
M_FILL	1			M_CIRCLE	64
M_POINT	2			M_STAR	128
M_BOX	4			M_HLINE	256
M_PLUS	8			M_VLINE	512
M_CROSS	16			M_HBLINE	1024
M_DIAMOND	32			M_VBLINE	2048
.TE
.LP
	Point markers are drawn using the \fBcdl_markPoint()\fR procedure,
point types may be logically \fIOR\fR'd to create composite markers, closed
shapes such as a circles, diamonds, or squares may be \fIOR\fR'd with the
M_FILL flag to flood-fill the point with the current overlay color.
.NH 3
Point
.LP
	The \fBcdl_markPoint()\fR procedure is used to mark a specific point
on the image using one of the marker types listed above.  The marker is 
centered at the coordinates specified by the \fIx\fR and \fIy\fR arguments,
\fItype\fR is an integer flag indicating what kind of marker to draw and 
may be a composite type by logically ORing two or more marker types.
\fISize\fR is the width and height of the marker measured in pixel unxits, 
and \fIcolor\fR is the color used to draw the marker.  If the \fInumber\fR
argument is greater than zero that number will be drawn next to the point
as a label, creating text labels for point markers can be done using the
\fIcdl_markPointLabel\fR procedure.
.LP
	Most marker names are fairly obvious but several are worth special
mention:  The M_DIAMOND, M_CIRCLE and M_BOX marker types may be logically
\fIOR\fRed with the M_FILL flag to produce a filled marker type.  Unless
\fIOR\fRd with the M_POINT flag all point markers will leave the center
pixel unchanged.  The M_HLINE and M_VLINE markers are most useful in
astronomical applications to mark an individual star, they are horizontal
and vertical lines respectively with a gap in the middle third of the marker
(the M_HBLINE and M_VBLINE are identical but with a width of 3 pixels).
.sp 0.5
.us "Synopsis"
.nf
	\f(CWint cdl_markPoint (CDLPtr cdl, int x, int y, int number,
	    int size, int type, int color)\fR
	\f(CWint cdl_markPointLabel (CDLPtr cdl, int x, int y, char *label
	    int size, int type, int color)\fR
.fi
.NH 3
Line
.LP
	The \fBcdl_markLine()\fR procedure is used to draw a line of the
specified color between points (\fIxs,ys\fR) and (\fIxe,ye\fR).
.sp 0.5
.us "Synopsis"
.nf
	\f(CWint cdl_markLine (CDLPtr cdl, int xs, int ys, int xe, int ye,
	    int color)\fR
.fi
.NH 3
Box
.LP
	The \fBcdl_markBox()\fR procedure is used to draw a box of the
specified color with endpoints specified by (\fIlx,ly\fR) and (\fIux,uy\fR).
If the \fIfill\fR flag is set the box will be filled with the marker color.
.sp 0.5
.us "Synopsis"
.nf
	\f(CWint cdl_markBox (CDLPtr cdl, int lx, int ly, int ux, int uy,
	    int fill, int color)\fR
.fi
.NH 3
Circle
.LP
	The \fBcdl_markCircle()\fR procedure is used to draw a circle of the
specified color with a center at (\fIx,y\fR) and radius \fIradius\fR.
If the \fIfill\fR flag is set the circle will be filled with the marker color.
.sp 0.5
.us "Synopsis"
.nf
	\f(CWint cdl_markCircle (CDLPtr cdl, int x, int y, int radius,
	    int fill, int color)\fR
.fi
.NH 3
Polyline
.LP
	The \fBcdl_markPolyline()\fR procedure is used to draw a line
connecting the \fInpts\fR points specified by the \fIxpts\fR and
\fIypts\fR array in the desired \fIcolor\fR.
.sp 0.5
.us "Synopsis"
.nf
	\f(CWint cdl_markPolyline (CDLPtr cdl, int *xpts, int *ypts,
	    int npts, int color)\fR
.fi
.NH 3
Polygon
.LP
	The \fBcdl_markPolygon()\fR procedure is used to draw a closed
polygon consisting of \fInpts\fR vertices specified by the \fIxpts\fR and
\fIypts\fR array in the desired \fIcolor\fR.  The last point in the array
will automatically be connected to the first point by the procedure.
If the \fIfill\fR flag is set the circle will be filled with the marker color.
.sp 0.5
.us "Synopsis"
.nf
	\f(CWint cdl_markPolygon (CDLPtr cdl, int *xpts, int *ypts,
	    int npts, int fill, int color)\fR
.fi
.NH 3
Ellipse
.LP
	The \fBcdl_markEllipse()\fR procedure is used to draw an ellipse of the
specified color with a center at (\fIx,y\fR) and semimajor-axis \fIxrad\fR
and semiminor-axis \fIyrad\fR pixels long.  A rotation angle for the ellipse
may be specified by passing a non-zero \fIangle\fR argument, the angle is
measured in degrees from the positive x-axis.
If the \fIfill\fR flag is set the circle will be filled with the marker color.
.sp 0.5
.us "Synopsis"
.nf
	\f(CWint cdl_markEllipse (CDLPtr cdl, int x, int y, int xrad,
	    int yrad, float ang, int fill, int color)\fR
.fi
.NH 3
Circular Annuli
.LP
	The \fBcdl_markCircAnnuli()\fR procedure is used to draw
\fInannuli\fR circles separated by \fIsep\fR pixels each.  The circle is
centered at (\fIx,y\fR) with an initial radius of \fIradius\fR pixels.
.sp 0.5
.us "Synopsis"
.nf
	\f(CWint cdl_markCircAnnuli (CDLPtr cdl, int x, int y, int radius,
	    int nannuli, int sep, int color)\fR
.fi
.NH 3
Elliptical Annuli
.LP
	The \fBcdl_markEllipAnnuli()\fR procedure is used to draw
\fInannuli\fR ellipses separated by \fIsep\fR pixels each.  The ellipse is
centered at (\fIx,y\fR) with an initial semimajor and semiminor axis 
specified by the \fIxrad\fR and \fIyrad\fR arguments.  Each ellipse will
be optionally rotate by an \fIangle\fR degrees as measured from the positive
x-axis.
.sp 0.5
.us "Synopsis"
.nf
	\f(CWint cdl_markEllipAnnuli (CDLPtr cdl, x, y, xrad, yrad, ang,
	    int nannuli, int sep, int color)\fR
.fi
.NH 3
Text
.LP
	The \fBcdl_markText()\fR procedure is used to draw a text string
specified by \fIstr\fR argument with an initial position at (\fIx,y\fR)
and optionally rotated by \fIangle\fR degrees as measured from the positive
x-axis.  The default \fIsize\fR is 1.0 and is approximately a 6x13 font,
the font size may be scaled by any fractional amount.
.sp 0.5
.us "Synopsis"
.nf
	\f(CWint cdl_markText (CDLPtr cdl, int x, int y, char *str,
	    float size, float angle, int color)\fR
.fi
.NH 2
Text Fonts
.LP
	The \fBcdl_setFont()\fR procedure is used to choose between one
of four available fonts as the text marker default: Roman, Greek, Futura,
Bold and Times respectively.  By default the Roman font will be used. 
The width of the lines used to draw the text may also be set. 
.sp 0.5
.us "Synopsis"
.nf
	\f(CWvoid cdl_setFont (CDLPtr cdl, int font)\fR
	\f(CWvoid cdl_setTextWidth (CDLPtr cdl, int width)\fR
.fi
.sp 0.5
A complete listing of the Greek character mappings can be found in the
file 'greek.ps' in the 'doc' subdirectory of the CDL distribution.
	The \fIRoman\fR font is the font implemented in the original version
of the CDL and works well for most applications.  Both the \fIGreek\fR and
\fITimes\fR fonts are hi-resolution fonts which work best for larger frame
buffers but can produce publication quality text.  The \fIFutura\fR font
is a simpler font which can produce better results than the default on small
size frame buffers.  A \fIBold\fR font automatically increases the text line
width by one pixel over the current setting and may be used with any font.
.NH 3
In-Line Font Changes
.LP
	Text markers are drawn using the font selected with
the \fIcdl_setFont()\fR
routine, however fonts may be change within a string itself (e.g. to set
a Greek character) using a \\f escape sequence.  The escape is followed 
by the character 'R' to set a Roman font, 'G' for Greek, 'F' for
futura, 'B' for bold 
and 'T' for Times.  Any number of escapes are permitted within a string,
the font change will remain in effect until it is changed, or the end of 
string at which point any subsequent strings will again be drawn with the
default font.  Additionally a 'P' in the escape sequence will change the
font to the one previously used, whatever that may be.
.LP
	The CDL also supports a sub/superscripting of text which can only
be done with the font escapes.  In this case the escape character followed by
a 'U' produces a superscript and a 'D' produces a subscript.  The changes may
be nested permitting several levels of sub/superscripts,  these escapes may
also be used in conjunction with a font change to cause the sub/superscript
to be drawn with a different font.  A superscript escape will remain in
effect until the end of the string or a \\fD escape is seen.  Similarly a
subscript remains in effect until the end of the string of a \\fU escape.
Sub/superscripted text is drawn using a smaller font size, there is presently
no way to specify a different size for the sub/superscripted text.
.TS
center;
cb s
l l.
Summary of Font Escapes
.sp 0.5
\\\\fR	change to Roman font
\\\\fG	change to Greek font
\\\\fF	change to Futura font
\\\\fT	change to Times font
\\\\fB	change to bold font
\\\\fP	change to previous font
\\\\fU	begin relative superscripted text
\\\\fD	begin relative subscripted text
.TE
.NH 2
Line Widths and Styles
.LP
	The \fBcdl_setLineWidth()\fR procedure can be used to set the line
width used to draw polygon or polyline markers, point markers will not
be affected.  The \fBcdl_setLineStyle()\fR procedure is used to set a line
style other than solid.
.sp 0.5
.us "Synopsis"
.nf
	\f(CWvoid cdl_setLineWidth (CDLPtr cdl, int width)\fR
	\f(CWvoid cdl_setLineStyle (CDLPtr cdl, int style)\fR
.fi
.LP
	\fRThe "\fIcdl.h\fR" include file for C programs, the
"\fIcdlftn.inc\fR" include for fortran programs, or the "\fIcdlspp.h\fR"
include file SPP programs, defines the following symbolic constants for
each of the defined line styles:
.TS
center;
lB r c c lB r.
L_SOLID	0			L_DASHED	1
L_DOTTED	2			L_DOTDASH	3
L_HOLLOW	4			L_SHADOW	5
.TE
.LP
	The \fIhollow\fR line style is drawn with a linewidth of five pixels,
two pixels of color, a black line, and two pixels of color.  It is best 
used when the marker will traverse extreme changes in brightness, due to the
thickness of the line it may work best with larger frame buffers.  The 
\fIshadow\fR linestyle is drawn as two pixels of color and two pixels of black
and should be used for similar brightness variations, however it effectively
shows up as a line only two pixels wide and may be preferred for medium or
smaller frame buffers.
.LP
	The three dashed linestyles are drawn using "gap" spacings of 5
pixels in between line segments.  Whether or not these gaps are resolved 
depends on the size of the frame buffer being used and the magnification
used in the display server.  By default they should resolve completely 
using frame buffers up to 1024x1024 pixels, or magnification factors
displaying 1024x1024 pixels.  If larger sizes are needed the image should
be subsampled prior to display to maintain the marker resolution needed 
for these linestyles.

.NH 2
Deleting Markers
.LP
	When markers are drawn the underlying subraster is first saved to
an internal structure, erasure is done by simply redisplaying the saved
raster.  Problems can arise however when markers overlap;  when deleting a
marker that is \fIunder\fR another marker the original pixels can overwrite
the pixels of the marker on top.  This is an unfortunate side effect of the
simple scheme used in this version of the package, users can call the
\fBcdl_redrawOverlay()\fR procedure to help clean up any artifacts left
behind.
.NH 3
Individual Markers
.LP
	The \fBcdl_deleteMark()\fR procedure is used to delete a single
marker from the display().  The (\fIx,y\fR) argument is either the center
position of the marker if that is know by the application, more typically
it will be an approximate position.  In the latter case the marker whose
center is closest to this position will be deleted.  For markers with no
defined center  the distance used to decide if the marker should be
deleted is the distance from the argument position to the edge of the 
marker.  For example, distance from a box or polygon is measured as the
distance from to one of the sides, for text it is the distance to the
start of the text string.  There is no way to \fIun\fRdelete a marker other
than to redraw it.
.sp 0.5
.us "Synopsis"
.nf
	\f(CWint cdl_deleteMark (CDLPtr cdl, int x, int y)\fR
.fi
.NH 3
The Entire Overlay
.LP
	To erase all markers currently displayed use the
\fBcdl_clearOverlay()\fR procedure.  Markers are erased in the reverse order
they were drawn to help reduce the chance that overlaying markers will leave
stray pixels.
.sp 0.5
.us "Synopsis"
.nf
	\f(CWint cdl_clearOverlay (CDLPtr cdl)\fR
.fi
.NH 2
Redraw
.LP
	The \fBcdl_redrawOverlay()\fR procedure can be used to redraw all
markers currently in the display list.  This is sometimes needed when 
subraster I/O procedures are used to redisplay subregions and overwrite
existing markers.
.sp 0.5
.us "Synopsis"
.nf
	\f(CWint cdl_redrawOverlay (CDLPtr cdl)\fR
.fi
.NH
ANSI C Function Prototypes
.LP
	The current release of CDL provides full ANSI C function prototypes
for all public and private procedures.  By default these will not be used 
even on systems with native ANSI compilers, however.
To make use of the CDL prototypes users will
need to define the macro \fBCDL_ANSIC\fR either when compiling the program
(using the -D option to the compiler),
or as a definition in the program source preceding the 'cdl.h' include
directive.
.LP
	For example,
.sp 0.5
.nf
	\f(CW#define  CDL_ANSIC
	#include "cdl.h"
	     :
	main (int argc, char **argv)
	     :\fR
.fi
.sp 0.5
or when compiling using something like
.sp 0.5
.nf
	\f(CWcc -DCDL_ANSIC client.c libcdl.a -lm\fR
.fi
.sp 0.5
	Note that when using CDL_ANSIC to build the client program it is
also required that the CDL itself be built in the same way to avoid
confusing FPE errors.  Similarly, when building client tasks that \fIdo
not\fR
use CDL_ANSIC you must use a version of the library that has not been
compiled with prototypes.
.LP
	The reason is that the float args to the CDL procedures in the
library, or in your task calls, are
promoted to doubles when compiling those procedures, but may only be passed as
floats in your code (or as double where the CDL is expecting float).  
This means the argument stack is off by 4 bytes for each float arg and
the values interpreted by the CDL procedure will be corrupted.
If you're going to use the prototypes you'll
need to edit the CDL Imakefile to define "-DCDL_ANSIC" in the EXTRA_DEFINES
so it will use the prototypes and everything will line up.  You will then
need to rebuild the libcdl.a as well as relink your program.
.NH
Fortran Language Binding Notes
.LP
	The Fortran language binding routines are implemented in C but
should be accessible from any fortran program as though they were real
fortran subroutines.  The calling sequences are the same as with the C
library routines with the following exceptions:
.IP
\(bu  The CDL package pointer is maintained internally so no 'cdl' pointer is
passed in the fortran interface.
.IP
\(bu  All routines which are integer procedures in the C interface return an
extra 'ier' argument to contain the error flag.  All Fortran functions are
implemented as subroutines.
.IP
\(bu  The procedure names are the same except that \fIcdl_\fR has been
replaced with \fIcf\fR in the fortran binding.  If your compiler is
case-sensitive then use all lower case letters.
.LP
The binding has been tested on a number of different platforms without 
problems.  The procedure names haven't been restricted to the traditional
6-character fortran names since most modern compilers can handle longer
names, if yours isn't one of them contact \fIiraf@noao.edu\fR for help
in changing the names.
.LP
	Since the CDL is implemented as a set of C routines, the one aspect
that cannot be overlooked in the fortran binding is the between Fortran and
C storage order for arrays.  In most cases this will not be a problem since
the CDL routines are just passing around pointers even if they live for a 
short while in a fortran program.  The problem comes when using the fortran
program to read the arrays, for example in using the array returned by the
\fBcfreadIRAF()\fR procedure, or when passing in arrays for display that
originated in the user's fortran code.  In these cases the array \fBmust\fR
be transposed to be interpreted correctly.  It was assumed that in most
applications arrays returned by CDL procedures would be immediately passed
to other CDL procedures so having the binding routines
transpose the array to/from
Fortran storage order was unnecessarily inefficient.  This may be changed in
later releases if required.

.NH
SPP Language Binding Notes
.LP
	The SPP language binding is experimental and is intended to provide
a way to quickly prototype tasks, it should not be used in production code
as it may not be as portable as the rest of the task.  In essence this 
binding is a layer on top of the Fortran binding since most IRAF platforms
still use Fortran as the intermediate code.  The calling sequences are the
same as with the Fortran library routines with the following caveats:
.IP
\(bu  The 'cdlspp.h' SPP include file is required by all files which call
CDL routines.  The binding names are actually SPP macros to resolve the
current 6 character limit on procedure names.
.IP
\(bu  All character string arguments must be dimensioned to at least SZ_FNAME 
characters in length.
.IP
\(bu  The CDL package pointer is maintained internally so no 'cdl' pointer is
passed in the fortran interface.
.IP
\(bu  All routines which are integer procedures in the C interface return an
extra 'ier' argument to contain the error flag.  All SPP functions are
implemented as subroutines.
.IP
\(bu  On HPUX or IBM RS6000 systems the 'cdlspp.h' file must be edited to
remove the trailing underscores from the procedure name macros.  This is
because on these platforms the fortran compiler will not append an
underscore to the SPP symbols as it does on other platforms.

.NH
IIS Protocol Description
.LP
	The communications protocol used by the CDL and servers such as 
\fIXImtool\fR and \fISAOimage\fR, is a slightly modified version of that used
by the IIS Model 70.  All operations are initiated by sending a header 
packet containing a \fIthing id\fR (tid) and \fIsubunit\fR selecting the
function to be performed, optionally followed by data up to 32K bytes long.
The IIS header packet used is defined as
.nf
		\f(CWstruct  iism70 {
        		short   tid;
        		short   thingct;
        		short   subunit;
        		short   checksum;
        		short   x, y, z;
        		short   t;
		};\fR   
.fi

The \fIthing count\fR field contains the negative number of bytes of data
that will be sent following the header packet.  The IIS header checksum is
computed as 
.nf
\f(CW
    checksum = 0177777 - (tid + subunit + thingct + x + y + z + t);
\fR
.fi
The four IIS registers are set differently depending on the operation, a
summary of the header packets for each operation is summarized below. 

.KS
.ce 1
\fBIIS Header Packet Summary\fR
.TS
tab(:);
c c c c c c c c c.
:TID:Subunit:Tct:X:Y:Z:T:Data
.T&
l | l | l | c | c | c | l | l | l |.
:_:_:_:_:_:_:_:_
Read Data:IIS_READ\fB|\fPPACKED:MEMORY:-NB:x:y:fr:-:NB
Write Data:IIS_WRITE\fB|\fPPACKED:MEMORY:-NB:x:y:fr:-:NB
Read Cursor:IIS_READ:IMCURSOR:-:-:-:-:-:-
Write Cursor:IIS_WRITE:IMCURSOR:-:x:y:wcs:-:-
Set Frame:IIS_WRITE:LUT\fB|\fPCOMMAND:-1:-:-:-:-:2
Erase Frame:IIS_WRITE \fB|\fP fb:FEEDBACK:-:-:-:fr:-:-

Old Write WCS:IIS_WRITE\fB|\fPPACKED:WCS:-N:-:-:fr:fb:320
Old Read WCS:IIS_READ:WCS:-:-:-:fr:wcs:320

WCS Version?:IIS_READ:WCS:-:1:1:-:-:320
WCS by Number?:IIS_READ:WCS:-:1:-:fr:wcs:1024 
New Write WCS:IIS_WRITE\fB|\fPPACKED:WCS:-N:1:-:fr:fb:1024
New Read WCS:IIS_READ:WCS:-:1:-:fr:wcs:1024
:_:_:_:_:_:_:_:_
.TE
.KE
.TS
l l l.
Where	NB	= number of bytes expected or written
	x	= x position of operation in frame buffer coords
	y	= y position of operation in frame buffer coords
	fr	= frame number (passed as bitflag (i.e. 1, 2 ,4 8, etc)
	fb	= frame buffer config number (zero indexed)
	N	= length of WCS string
	wcs	= WCS number (usually zero)
	Data	= the number of bytes of data to be read or written following the header packet.

	IIS_WRITE	 = 0400000
	IIS_READ	 = 0100000   
	COMMAND	 = 0100000
	PACKED	 = 0040000
	IMC_SAMPLE	 = 0040000
		
	MEMORY	 = 001
	LUT	 = 002
	FEEDBACK	 = 005
	IMCURSOR	 = 020
	WCS	 = 021
.TE
.LP
TID fields can be logically OR'd with the PACKED flag indicating the number
of data bytes is exactly \fIthingct\fR bytes long, otherwise \fIthingct\fR
must be specified as half the number of data bytes.  In a cursor read, if
the IIS_READ flag is OR'd with IMC_SAMPLE the logical cursor position (i.e.
the last value read or set) is returned immediately, otherwise the server
will wait for a keystroke to be hit before returning a string containing the
(x,y) position, wcs of the read, and the keystroke.  When setting the frame
you must send a short integer in the data containing the frame selected.

.NH
VXIMTOOL Proxy/Display Server Usage
.LP
\fIVXIMTOOL\fR is a image display server process much like \fIXIMTOOL\fR,
except that all it normally does is respond to datastream requests
to read and write to internal frame buffers maintained as arrays in memory.
Multiple frame buffers and frame buffer configurations are supported.  It
can be used to debug CDL programs by printing out the 
protocol packets received, or can simply be used as a dummy server in cases
where no image display is really needed.  By enabling the \fI-proxy\fR
flag the server can also be used to repeat the datastream requests to a
list of other servers, effectively splitting the image display to a number
of other servers.  See the \fIvximtool\fR man page for details on other 
command-line arguments and usage.
.LP
The program was originally intended as a debugging tool, either in the
development of CDL clients directly or in cases where the display may
need to go to separate screens as part of a larger project.  For example,
engineers may wish to "eavesdrop" on the system by viewing images displayed
by CDL clients used as part of a data acquisition system.  It can also be
used as a memory-only display server for CDL clients which need to be run
in the background as part of a pipeline processing system requiring a
frame buffer for image marking.
.LP
In proxy mode the program acts as a relay for the IIS datastream packets,
sending image data, frame requests, etc. to a list of other servers specified
on the command line.  The effect of this is to allow a client to display to
this program which then re-displays to each of the other named servers.  
Of course CDL clients can also do this internally by opening multiple
connections, using \fIvximtool\fR in proxy mode adds the functionality to
programs which may use this feature only ocasionally.  A maximum of 8 servers
may be named, they may be either on the local host or a remote machine 
and connections can be established using either fifos or sockets.  See
above or the \fIvximtool\fR man page for details on how to specify the
server connection.
.LP
The current implementation has a few restrictions users should keep in mind:
.IP
\(bu  The time to display an image or perform any output operation scales
with the number of connected hosts.  Each IIS packet is forwarded to each
host in turn before processing the next input packet, and connection over a slow
network will delay the entire process.
.IP
\(bu  Cursor and image readback are done by sending the request \fIonly\fR to
the first server named on the command line.  This is done to avoid forcing
a cursor mode on all servers which cannot be terminated when a response is
received from only one server, and means that the first server named should
be the one used to control interactive sessions.  The remaining servers
however can still respond to cursor requests from other applications
connected to that server on another channel.
.IP
\(bu  All named servers must be running prior to starting the proxy server.  
The connection to the remote servers is established when this task is first
run and if no server is running that connection will be ignored. The task will
exit if no remote servers can be found for display.
.IP
\(bu  Any connected server that shuts down while the proxy server is running
is likely to cause the program to crash on the next display. 
.bp
.NH
C Interface Summary
.LP
.in 0.5i
#include  "\fBcdl.h\fR"
.in -0.5i

.TS
center;
r l.
CDLPtr \fBcdl_open\fR	(imtdev)
int \fBcdl_displayPix\fR	(cdl, pix, nx, ny, bitpix, frame, fbconfig, zscale)
char \fBcdl_readCursor\fR	(cdl, sample, x, y, wcs, key)
int \fBcdl_setCursor\fR	(cdl, x, y, wcs)
int \fBcdl_setWCS\fR	(cdl, name, title, a, b, c, d, tx, ty, z1, z2, zt)
int \fBcdl_getWCS\fR	(cdl, name, title, a, b, c, d, tx, ty, z1, z2, zt)
void \fBcdl_setFrame\fR	(cdl, frame)
int \fBcdl_clearFrame\fR	(cdl)
void \fBcdl_close\fR	(cdl)

int \fBcdl_setMapping\fR	(cdl, region, sx,sy,snx,sny, dx,dy,dnx,dny, ref)
int \fBcdl_getMapping\fR	(cdl, region, sx,sy,snx,sny, dx,dy,dnx,dny, ref)
int \fBcdl_queryMap\fR	(cdl, wcs, region, sx,sy,snx,sny, dx,dy,dnx,dny, objref)

int \fBcdl_displayIRAF\fR	(cdl, fname, band, frame, fbconfig, zscale)
int \fBcdl_isIRAF\fR	(fname)
int \fBcdl_readIRAF\fR	(fname, band, pix, nx, ny, bitpix, title)

int \fBcdl_displayFITS\fR	(cdl, fname, frame, fbconfig, zscale)
int \fBcdl_isFITS\fR	(fname)
int \fBcdl_readFITS\fR	(fname, pix, nx, ny, bitpix, title)

void \fBcdl_computeZscale\fR	(cdl, pix, nx, ny, bitpix, z1, z2)
void \fBcdl_zscaleImage\fR	(cdl, pix, nx, ny, bitpix, z1, z2)

int \fBcdl_printPix\fR	(cdl, cmd, pix, nx, ny, annotate)
int \fBcdl_printPixToFile\fR	(cdl, fname, pix, nx, ny, annotate)

int \fBcdl_readImage\fR	(cdl, pix, nx, ny)
int \fBcdl_readFrameBuffer\fR	(cdl, pix, nx, ny)
int \fBcdl_readSubRaster\fR	(cdl, lx, ly, nx, ny, pix)
int \fBcdl_writeSubRaster\fR	(cdl, lx, ly, nx, ny, pix)

void \fBcdl_selectFB\fR	(cdl, nx, ny, fb, w, h, nf, reset)
void \fBcdl_setFBConfig\fR	(cdl, configno)
void \fBcdl_getFBConfig\fR	(cdl, configno, w, h, nf)
void \fBcdl_lookupFBSize\fR	(cdl, configno, w, h, nf)

void \fBcdl_setZTrans\fR	(cdl, ztrans)
void \fBcdl_setZScale\fR	(cdl, z1, z2)
void \fBcdl_setSample\fR	(cdl, nsample)
void \fBcdl_setSampleLines\fR	(cdl, nlines)
void \fBcdl_setContrast\fR	(cdl, contrast)
void \fBcdl_setName\fR	(cdl, imname)
void \fBcdl_setTitle\fR	(cdl, imtitle)

void \fBcdl_getFrame\fR	(cdl, frame)
void \fBcdl_getZTrans\fR	(cdl, ztrans)
void \fBcdl_getZScale\fR	(cdl, z1, z2)
void \fBcdl_getSample\fR	(cdl, nsample)
void \fBcdl_getSampleLines\fR	(cdl, nlines)
void \fBcdl_getContrast\fR	(cdl, contrast)
void \fBcdl_getName\fR	(cdl, imname)
void \fBcdl_getTitle\fR	(cdl, imtitle)

int \fBcdl_mapFrame\fR	(cdl, frame)
int \fBcdl_markCoordsFile\fR	(cdl, fname, type, size, color, label)
int \fBcdl_markPoint\fR	(cdl, x, y, number, size, type, color)
int \fBcdl_markPointLabel\fR	(cdl, x, y, label, size, type, color)
int \fBcdl_markLine\fR	(cdl, xs, ys, xe, ye, color)
int \fBcdl_markBox\fR	(cdl, lx, ly, ux, uy, fill, color)
int \fBcdl_markPolygon\fR	(cdl, xarray, yarray, npts, fill, color)
int \fBcdl_markPolyline\fR	(cdl, xarray, yarray, npts, color)
int \fBcdl_markCircle\fR	(cdl, x, y, radius, fill, color)
int \fBcdl_markCircAnnuli\fR	(cdl, x, y, radius, nannuli, sep, color)
int \fBcdl_markEllipse\fR	(cdl, x, y, xrad, yrad, rotang, fill, color)
int \fBcdl_markEllipAnnuli\fR	(cdl, x, y, xrad, yrad, ang, nannuli, sep, color)
int \fBcdl_markText\fR	(cdl, x, y, str, size, angle, color)
int \fBcdl_setFont\fR	(cdl, font)
int \fBcdl_setTextWidth\fR	(cdl, width)
int \fBcdl_setLineWidth\fR	(cdl, width)
int \fBcdl_setLineStyle\fR	(cdl, style)
int \fBcdl_deleteMark\fR	(cdl, x, y)
int \fBcdl_clearOverlay\fR 	(cdl)
int \fBcdl_redrawOverlay\fR 	(cdl)
.TE

.bp
.NH
C Example Tasks
.LP
	The examples shown here are for demonstration purposes only.  They
are based on working example tasks in the CDL source \fIexamples\fR
subdirectory, see the programs there for the full program listing.

.NH 2
Display Example
.LP
.ps -2
.vs -2
.nf
\f(CW
#include <stdio.h>
#include <unistd.h>
#include "cdl.h"

/*  \fIDISPLAY -- Example task to display an image as a command-line task.
  *  This task is meant to show three ways the CDL can be used to display
  *  an image, see the code comments for a description of each method.
  *
  *  Examples:
  *      To display a simple IRAF or FITS file:
  *	    % ./display -frame 2 image.imh
  *         % ./display image.fits
  *
  *      To display a FITS file as a raw image:
  *          % ./display -nx 512 -ny 512 -depth 16 -hskip 5760 -raw dpix.fits
  *
  *  Usage:
  * 	display [-depth N] [-fits] [-frame N] [-fbconfig N] [-hskip N]
  *	    [-iraf] [-nozscale] [-nx N] [-ny N] [-raw] [-zscale] file\fP
 */

#define NONE	-1
#define	IRAF	 0
#define	FITS	 1
#define	RAW	 2

main (argc, argv)
int	argc;
char	*argv[];
{
	CDLPtr	cdl;
	char 	*fname, title[128];
	int	i, status = 0, frame = 1, fbconfig = 0, zscale = 1;
	int	format = NONE, nx = 0, ny = 0, depth = 8, hskip = 0;
	float 	z1, z2;
	int	fb_w, fb_h, nf;
	unsigned char *pix = NULL;
	
	/* \fIProcess the command line options.\fP */
	if (argc > 1) {
	    for (i=1; i < argc; i++) {
            if (strcmp (argv[i], "-depth") == 0)         depth = atoi (argv[++i]);
            else if (strcmp (argv[i], "-fits") == 0)     format = FITS;
            else if (strcmp (argv[i], "-frame") == 0)    frame = atoi (argv[++i]);
            else if (strcmp (argv[i], "-fbconfig") == 0) fbconfig = atoi (argv[++i]);
            else if (strcmp (argv[i], "-hskip") == 0)    hskip = atoi (argv[++i]);
            else if (strcmp (argv[i], "-iraf") == 0)     format = IRAF;
            else if (strcmp (argv[i], "-nozscale") == 0) zscale = 0;
            else if (strcmp (argv[i], "-nx") == 0)       nx = atoi (argv[++i]);
            else if (strcmp (argv[i], "-ny") == 0)       ny = atoi (argv[++i]);
            else if (strcmp (argv[i], "-raw") == 0)      format = RAW;
            else if (strcmp (argv[i], "-zscale") == 0)   zscale = 1;
	    }
	}

	/* \fIOpen the package and a connection to the server.\fP */
	if (!(cdl = \fBcdl_open\fP ((char *)getenv("IMTDEV"))) )
	   exit (-1);

	fname = argv[argc-1];

	/* \fIMETHOD 1:  Displays the image using the high-level format display
 	  * call.  Display as an IRAF image if the option was set indicating
	  * this is the format, otherwise test the file to see if it is anyway.\fP
	 */
	if (format == IRAF || (format == NONE && \fBcdl_isIRAF\fP (fname))) {
	    status = \fBcdl_displayIRAF\fP (cdl, fname, 1, frame, FB_AUTO, zscale);

	/* \fIMETHOD 2:  Uses the CDL procedure for getting image pixels from
	  * a known format, minimal work required to display an image.  The
 	  * point here is that you can use this method to process the image
	  * yourself prior to display, e.g. subsample the pixels, apply a user
	  * LUT, etc but still use the CDL to get the raw image and do the
	  * display.\fP
	 */
	} else if (format == FITS || (format == NONE && \fBcdl_isFITS\fP (fname))) {

	    /* \fIGet the FITS image pixels, exit w/ an error status if something
	          * went wrong, the procedure will print what that was.\fP
	     */
	    if (\fBcdl_readFITS\fP (fname, &pix, &nx, &ny, &depth, title)) {
        	\fBcdl_close\fP (cdl);		/* \fIclose the package\fP  */
        	exit (1);			/* \fIexit w/ error code\fP */
	    }

	    /* \fINow select a frame buffer large enough for the image. The 
	          * fbconfig number is passed in the WCS packet, but the display
	          * call below will compute the correct WCS for the image and
	          * transmit that prior to display, all we're doing here is
	          * setting up the FB to be used.\fP
	     */
	    if (fbconfig == 0)
	        \fBcdl_selectFB\fP (cdl, nx, ny, &fbconfig, &fb_w, &fb_h, &nf, 0);

	    /* \fILastly, display the pixels to the requested frame, do any
	          * zscaling requested using the CDL procedure.\fP
	     */
	    status = \fBcdl_displayPix\fP (cdl, pix, nx, ny, depth, frame, 
		fbconfig, zscale);

	/* \fIMETHOD 3:  Displays an image of raw pixels.  The client code is
	  * responsible for reading the image and calling all the procedures
	  * needed for image display, initialize the frame, zscaling pix, etc.
	  * While we assume a simple raster format in this program, the user
	  * code can read a compressed image format such as GIF, mosaic multiple
	  * images for display as a single image, or just about anything that
	  * produces a raster for display. The intent here is to show all the
	  * lowest level calls needed for displaying the image.\fP
	 */
	} else if (format == RAW) {
	    FILE	*fd;
	    int		lx, ly;

	    if (nx == 0 || ny == 0) {
	        fprintf (stderr, "No size given for raw data.\\n");
	        exit (1);
	    }

	    /* \fIOpen the image file if we can.\fP */
	    if (fd = fopen (fname, "r")) {

		/* \fISeek to the offset specified.\fP */
		lseek (fileno(fd), (off_t) hskip, SEEK_SET);

		/* \fIAllocate the pixel pointer and read the data.\fP */
		pix = (unsigned char *) malloc (nx * ny * (depth / 8));
		fread (pix, depth/8, nx * ny, fd);

		/* \fIIf we're zscaling and depth is more than 8-bits, do that.\fP */
		if (zscale && depth > 8) {
		    \fBcdl_computeZscale\fP (cdl, pix, nx, ny, depth, &z1, &z2);
		    \fBcdl_zscaleImage\fP (cdl, &pix, nx, ny, depth, z1, z2);
		}

		/* \fINow select a frame buffer large enough for the image.  
		 * We'll ask that this be reset but the change won't go to
		 * the server until we send in the WCS below.\fP
		 */
		\fBcdl_selectFB\fP (cdl, nx, ny, &fbconfig, &fb_w, &fb_h, &nf, 1);

		/* \fICompute the image placement so it's centered in the frame,
		 * but note the cdl_writeSubRaster() routine can place an
		 * arbitrary raster anywhere in the frame buffer.\fP
		lx = (fb_w / 2) - (nx / 2);
		ly = fb_h - ((fb_h / 2) + (ny / 2));

		/* \fISet the mapping we'll send with the WCS which must be
		 * called before the cdl_setWCS() call since the data is sent
		 * with the WCS and not as a separate call.\fP
		 *
		 * \fRFirst we must compose a node!path prefix for the image.\fP */
		 */
		gethostname (node, 512);
		(void) getcwd (path, 512);
		if (*fname == '/')
		    (void) sprintf (path_prefix, "%s!%s", node, fname);
		else
		    (void) sprintf (path_prefix, "%s!%s/%s", node, path, fname);

		\fBcdl_setMapping\fP (cdl, "image", 0., 0., nx, ny, lx, ly, nx, ny,
		   path_prefix);

		/* \fIFor the WCS we assume a simple linear transform where the
		 * image is Y-flipped, the (x,y) translation is computed so
		 * it is correct for an frame buffer >= than the image size.\fP
		 */
		\fBcdl_setWCS\fP (cdl, fname, "", 1., 0., 0., -1., 
		    (float) (nx / 2) - (fb_w / 2) + 1,      /* \fIX trans.\fP    */
		    (float) (fb_h / 2) + (ny / 2),          /* \fIY trans.\fP    */
		    z1, z2, CDL_LINEAR);                    /* \fIZ transform\fP */

		/* \fIFinally, display the pixels.\fP  */
		if (\fBcdl_writeSubRaster\fR (cdl, lx, ly, nx, ny, pix))
		    status = 1;
	    } else
		status = 1;
	} else {
	    if (access (fname, F_OK) == 0)
		fprintf (stderr, "'%s': unknown image format.\\n", fname);
	    else
		fprintf (stderr, "'%s': image does not exist.\\n", fname);
	    status = 1;
	}

	/* \fINow just free the pixel pointer to clean up.\fP */
	if (pix)
	    free ((unsigned char *) pix);
	\fBcdl_close\fP (cdl);		/* \fIclose the package\fP */
	exit (status);
}
\fR
.fi
.vs +2
.ps +2
.bp
.NH 2
Interactive Graphics Overlay Example
.LP
.ps -2
.vs -2
.nf
\f(CW
#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include "cdl.h"

/* \fI
  *  TVMARK -- Example task for displaying an marking images.  This program
  *  can be used to either display an image and overlay points defined in
  *  a coordinate file, map an existing display frame for marking, or option-
  *  ally enter a cursor command loop after either of these providing other
  *  marking capability.  All options support minimum match.
  *
  *  Examples:
  *	% tvmark dpix.fits
  * 	% tvmark -coords coords -color 205 dpix.fits
  *	% tvmark -frame 2
  *	% tvmark -coords coords -interactive dpix.fits
  *
  *  Usage:
  * 	tvmark [-frame N] [-fbconfig N] [-coords <file>] [-size N] [-color N]
  *	     [-nolabel] [-fill] [-interactive] [image]\fP
 */

main (argc, argv)
int	argc;
char	*argv[];
{
	CDLPtr	cdl;
	char 	*fname = NULL, *cfname = NULL;
	int	i, status = 0, fill = 0, frame = 1, fb = FB_AUTO, zscale = 1;
	int	color = 201, label = 1, size = 9, interactive = 0;
	float 	z1, z2;
	int	fb_w, fb_h, nf;
	unsigned char *pix = NULL;
	
	/* \fIProcess the command line options.\fP */
	if (argc > 1) {
	    for (i=1; i < argc; i++) {
            if (strncmp(argv[i], "-color",4) == 0) color = atoi (argv[++i]);
            else if (strncmp(argv[i], "-coords",4) == 0) cfname = argv[++i];
            else if (strncmp(argv[i], "-fbconfig",3) == 0) fb = atoi (argv[++i]);
            else if (strncmp(argv[i], "-fill",4) == 0) fill = 1;
            else if (strncmp(argv[i], "-frame",3) == 0) frame = atoi (argv[++i]);
            else if (strncmp(argv[i], "-interactive",4) == 0) interactive = 1;
            else if (strncmp(argv[i], "-nolabel",4) == 0) label = 0;
            else if (strncmp(argv[i], "-nozscale",4) == 0) zscale = 0;
            else if (strncmp(argv[i], "-size",2) == 0) size = atoi (argv[++i]);
	    else 
	        fname = argv[i];
	    }
	}

	/* \fIOpen the package and a connection to the server.\fP */
	if (!(cdl = \fBcdl_open\fP ((char *)getenv("IMTDEV"))) )
	   exit (-1);

	/* \fIIf an image was specified display it first, otherwise assume the
	  * image has already been loaded in the frame and mark that.\fP
	 */
	if (fname) {
	    if (\fBcdl_isIRAF\fP (fname))
	        status = \fBcdl_displayIRAF\fP (cdl, fname, 1, frame, fb, zscale);
	    else if (\fBcdl_isFITS\fP (fname))
	        status = \fBcdl_displayFITS\fP (cdl, fname, frame, fb, zscale);
	    else {
		if (access (cfname, F_OK) == 0)
	            fprintf (stderr, "'%s': unknown image format.\\n", fname);
	 	else
	            fprintf (stderr, "'%s': image doesn't exist.\\n", fname);
	        status = 1;
	    }
	    if (status)  goto err_;
	} else {
	
	    /* \fIIf we've requested a special frame buffer, set it now.\fP */
	    if (fb > 0)
		  \fBcdl_setFBConfig\fP (cdl, fb);

	    /* \fIMap the current display frame for use as an image.\fP */
	    \fBcdl_mapFrame\fP (cdl, frame);
	}

	/* \fIIf a coordinate file was specified read the file and mark those
	  * coords with points.\fP
	 */
	if (cfname) 
            \fBcdl_markCoordsFile\fP (cdl, cfname, M_STAR, size, color, label);

	/* \fILastly, start up an interactive cursor loop if needed.\fP */
	if (interactive)
	    tvmInteractive (cdl, label, fill, color, size);
	
	/* \fIClose the package and clean up.\fP */
err_:	\fBcdl_close\fP (cdl);			
	exit (status);
}

/*  \fITVMINTERACTIVE -- Process commands interactively.\fP  */

tvmInteractive (cdl, label, fill, color, size)
CDLPtr	cdl;
int	label, fill, color, size;
{
	float   angle = 0.0, rx, ry, txsize = 1.;
	int     nx, ny, i, x, y, x2, y2, wcs;
	int     number=1, radius=11, xrad=11, yrad=6, nannuli=3, sep=5;
	char    key, cmd[SZ_NAME], str[SZ_NAME];
	unsigned char *pix;

	/* \fIProcess commands until a 'q' keystroke is hit.\fP */
	while (\fBcdl_readCursor\fP (cdl, 0, &rx, &ry, &wcs, &key) != 'q') {
	    x = (int) (rx + 0.5);	/* \fIconvert to int pixels\fP */
	    y = (int) (ry + 0.5);

	    switch (key) {
	    case ':':			/* \fIprocess a colon command\fP */
		putchar (':');
		gets (str);
		for (i=0; str[i] != ' ' && str[i]; i++)
		    cmd[i] = str[i];
		cmd[i++] = '\0';

		if (strcmp (cmd, "angle") == 0)        angle = atof (&str[i]);
		else if (strcmp (cmd, "color") == 0)   color = atoi (&str[i]);
		else if (strcmp (cmd, "fill") == 0)    fill = atoi (&str[i]);
		else if (strcmp (cmd, "number") == 0)  number = atoi (&str[i]);
		else if (strcmp (cmd, "nannuli") == 0) nannuli = atoi (&str[i]);
		else if (strcmp (cmd, "label") == 0)   label = atoi (&str[i]);
		else if (strcmp (cmd, "sep") == 0)     sep = atoi (&str[i]);
		else if (strcmp (cmd, "size") == 0)    size = atoi (&str[i]);
		else if (strcmp (cmd, "txsize") == 0)  txsize = atof (&str[i]);
		else if (strcmp (cmd, "xrad") == 0)    xrad = atoi (&str[i]);
		else if (strcmp (cmd, "yrad") == 0)    yrad = atoi (&str[i]);
		else if (strcmp (cmd, "print") == 0) {
		    \fBcdl_readFrameBuffer\fP (cdl, &pix, &nx, &ny);
		    \fBcdl_printPix\fP (cdl, NULL, pix, nx, ny, 1);
		} else if (strcmp (cmd, "snap") == 0) {
		    \fBcdl_readFrameBuffer\fP (cdl, &pix, &nx, &ny);
		    \fBcdl_printPixToFile\fP (cdl, &str[i], pix, nx, ny, 1);
		} else if (strcmp (cmd, "status") == 0) {
		    printf ("angle	= %-5.3g\tcolor	= %d\t", angle, color);
		    printf ("fill	= %-5d\tnumber	= %d\\n", fill, number);
		    printf ("nannuli	= %-5d\tsep	= %d\t", nannuli, sep);
		    printf ("size	= %-5d\ttxsize	= %g\\n", size, txsize);
		    printf ("xrad	= %-5d\tyrad	= %d\t", xrad, yrad);
		    printf ("label	= %-5d\\n", label);
		}
		break;

	    case '?':
		/* ......\fIhelp procedures\fP */
		break;

	    case 'p':				/* \fIplus mark\fP	*/
		\fBcdl_markPoint\fP (cdl, x, y, (label ? number++ : 0), size, M_PLUS, color);
		break;
	    case 'x':				/* \fIcross mark\fP	*/
		\fBcdl_markPoint\fP (cdl, x, y, (label ? number++ : 0), size, M_CROSS, color);
		break;
	    case '.':				/* \fIpoint mark\fP	*/
		\fBcdl_markPoint\fP (cdl, x, y, (label ? number++ : 0), size, M_POINT, color);
		break;
	    case '*':				/* \fIstar mark\fP	*/
		\fBcdl_markPoint\fP (cdl, x, y, (label ? number++ : 0), size, M_STAR, color);
		break;
	    case '_':				/* \fIhoriz dash mark\fP*/
		\fBcdl_markPoint\fP (cdl, x, y, (label ? number++ : 0), size, M_HBLINE, color);
		break;
	    case '|':				/* \fIvert dash mark\fP	*/
		\fBcdl_markPoint\fP (cdl, x, y, (label ? number++ : 0), size, M_VBLINE, color);
		break;
	    case 'o':				/* \fIcircle mark\fP	*/
		\fBcdl_markPoint\fP (cdl, x, y, (label ? number++ : 0), size, M_CIRCLE|fill, color);
		break;
	    case 's':				/* \fIsquare mark\fP	*/
		\fBcdl_markPoint\fP (cdl, x, y, (label ? number++ : 0), size, M_BOX|fill, color);
		break;
	    case 'v':				/* \fIdiamond mark\fP	*/
		\fBcdl_markPoint\fP (cdl, x, y, (label ? number++ : 0), size, M_DIAMOND|fill, color);
		break;

	    case 'b':				/* \fIBox\fP		*/
		printf ("Hit another key to define the box...\\n");
	  	(void) \fBcdl_readCursor\fP (cdl, 0, &rx, &ry, &wcs, &key);
	    	x2 = (int) (rx + 0.5); 		y2 = (int) (ry + 0.5);
		\fBcdl_markBox\fP (cdl, x, y, x2, y2, fill, color);
		break;
	    case 'c':				/* \fICircle\fP		*/
		printf ("Hit another key to set radius ...\\n");
	  	(void) \fBcdl_readCursor\fP (cdl, 0, &rx, &ry, &wcs, &key);
	    	x2 = (int) (rx + 0.5); 		y2 = (int) (ry + 0.5);
		radius = (int) sqrt ((double) ((x2-x)*(x2-x) + (y2-y)*(y2-y)));
		\fBcdl_markCircle\fP (cdl, x, y, radius, fill, color);
		break;
	    case 'd':				/* \fIDelete marker\fP	*/
		\fBcdl_deleteMark\fP (cdl, x, y);
		break;
	    case 'e':				/* \fIEllipse\fP	*/
		\fBcdl_markEllipse\fP (cdl, x, y, xrad, yrad, angle, fill, color);
		break;
	    case 'l':				/* \fILine\fP		*/
		printf ("Hit another key to set line endpoint...\\n");
	  	(void) \fBcdl_readCursor\fP (cdl, 0, &rx, &ry, &wcs, &key);
	    	x2 = (int) (rx + 0.5); 		y2 = (int) (ry + 0.5);
		\fBcdl_markLine\fP (cdl, x, y, x2, y2, color);
		break;
	    case 't':				/* \fIText string\fP	*/
		printf ("Text string: ");
		gets (str);
		\fBcdl_markText\fP (cdl, x, y, str, txsize, angle, color);
		break;
	    case 'C':				/* \fICircular annuli\fP*/
		\fBcdl_markCircAnnuli\fP (cdl, x, y, radius, nannuli, sep, color);
		break;
	    case 'D':				/* \fIDelete all markers\fP*/
		\fBcdl_clearOverlay\fP (cdl);
		break;
	    case 'E':				/* \fIElliptical annuli\fP*/
		\fBcdl_markEllipAnnuli\fP (cdl, x, y, xrad, yrad, angle, nannuli, sep, color);
		break;
	    default:
		break;
	    }
	}
}
\fR
.fi
.vs +2
.ps +2
.bp
.NH 2
Image Mosaic Example
.LP
.in 0.5i
.ps -2
.vs -2
.nf
#include <stdio.h>
#include <unistd.h>
#\fBinclude "cdl.h"\fP

/* \fIMOSAIC -- Example task to mosaic several images on a display. Demonstrates
 * usage of low-level routines for complex display operations.\fP
 */

main (argc, argv)
int	argc;
char	*argv[];
{
	CDLPtr	cdl;
	char 	*fname = NULL, title[128];
	int	i, j, k, status=0, label=0, frame=1, fb=FB_AUTO, zscale=1;
	int	sample=1, pad=0, col=204, imx, imy, bitpix, nimages, nim;
	int	ii, xinit, rowx, rowy, nnx, nny, fb_w, fb_h, nf, mx, my, nx, ny;
	float 	z1, z2;
	unsigned char *pix = NULL;
	
	/* \fIProcess the command line options.\fP */
	if (argc > 1) {
	  for (i=1; i < argc; i++) {
            if (strncmp (argv[i], "-fbconfig",3) == 0) fb=atoi(argv[++i]);
	    else if (strncmp (argv[i],"-frame",3) == 0) frame=atoi(argv[++i]);
	    else if (strncmp (argv[i],"-color",3) == 0) col=atoi(argv[++i]);
	    else if (strncmp (argv[i],"-label",4) == 0) label=1;
	    else if (strncmp (argv[i],"-nozscale",4) == 0) zscale=0;
	    else if (strncmp (argv[i],"-nx",3) == 0) nx=atoi(argv[++i]);
	    else if (strncmp (argv[i],"-ny",3) == 0) ny=atoi(argv[++i]);
	    else if (strncmp (argv[i],"-pad",4) == 0) pad=atoi(argv[++i]);
	    else if (strncmp (argv[i],"-sample",4) == 0) sample=atoi(argv[++i]);
	    else 
	        break;
	  }
	}
	nimages = argc - i;

	/* \fIOpen the package and a connection to the server.\fP */
	if (!(cdl = \fBcdl_open\fP ((char *)getenv("IMTDEV"))) )
	   exit (-1);

	/* \fIClear the frame to begin.\fP */
	(void) \fBcdl_clearFrame\fP (cdl);

	/* \fILoop over each of the images in the list.\fP */
	nim = rowx = rowy = nnx = nny = 0;
	for (k=0; k < ny && nim < nimages; k++) {
	    rowy += nny + pad; 
	    for (rowx = xinit, j=0; j < nx && nim < nimages; j++) {

		/* \fIGet the image name for display.\fP */
		fname = argv[i++];

		/* \fIFigure out what kind of image it is and get the pixels.\fP */
            	if (cdl_isIRAF (fname))
		    status = \fBcdl_readIRAF\fP (fname, 1, &pix, &imx, &imy, &bitpix, title);
            	else if (cdl_isFITS (fname))
		    status = \fBcdl_readFITS\fP (fname, &pix, &imx, &imy, &bitpix, title);
            	else {
		    fprintf(stderr, "'%s': unknown or nonexistant image.\\n", fname);
		    status = 1;
            	}
            	if (status)  goto err_; 

		/* \fICompute subsampled image size.\fP */
		if (sample > 1)
		    nnx = imx / sample, nny = imy / sample;
		else
		    nnx = imx, nny = imy;

		/* \fIUnless we asked for a specific FB size find one large enough
		 * to handle the mosaic.  We don't check to be sure what's
		 * returned is really large enough.\fP
		 */
		if (nim == 0 && fb == FB_AUTO)
		    \fBcdl_selectFB\fP (cdl, nx*nnx+(pad*(nx-1)), ny*nny+(pad*(ny-1)), &fb, &fb_w, &fb_h, &nf, 1);
		else {
		    \fBcdl_setFBConfig\fP (cdl, fb);
		    \fBcdl_lookupFBSize\fP (cdl, fb, &fb_w, &fb_h, &nf);
		}

		/* \fIDefine a WCS for the frame.\fP */
		\fBcdl_setWCS\fP (cdl, "image mosaic", title, 1., 0., 0., -1., 0., (float) ny*imy+(pad*(ny+1)), 1., 255., 1);

		/* \fIThe first time through figure out the placement so the
	   	 * entire mosaic is centered in the frame.\fP
		 */
		if  (nim == 0) {
		    mx = (nx * nnx) + pad * (nx-1);
		    my = (ny * nny) + pad * (ny-1);
		    rowy = (fb_h - my) / 2;
		    xinit = rowx = (fb_w - mx) / 2;
		}

		/* \fICompute the zscaled imaged pixels.\fP */
		if (zscale) {
		    \fBcdl_computeZscale\fP (cdl, pix, imx ,imy, bitpix, &z1, &z2);
		    \fBcdl_zscaleImage\fP (cdl, &pix, imx ,imy, bitpix, z1, z2);
		}

		/* \fISubsample the image if requested.\fP */
		if (sample > 1) {
		    int l, m, n=0; 
		    for (l=0; l < imy; l+=sample)
		        for (m=0; m < imx; m+=sample)
		            pix[n++] = pix[(l*imx)+m];
		}

		/* \fIWrite the image to the frame buffer.\fP */
            	if (\fBcdl_writeSubRaster\fP (cdl, rowx, rowy, nnx, nny, pix)) goto err_; 

		/* \fIDraw the image name as a label.\fP */
		if (label) \fBcdl_markText\fP (cdl, rowx+10, rowy+10, fname, 1., 0., col);

		nim++; 	rowx += nnx + pad; 
	    }
	}
	
	/* \fIClose the package and clean up.\fP */
err_:	\fBcdl_close\fP (cdl);			
	exit (status);
}
.fi
.vs +2
.ps +2
.in -0.5i

.bp
.NH
Fortran  Interface Summary
.LP
.in 0.5i
include  "\fBcdlftn.inc\fR"
.in -0.5i

.TS
center;
r l.
\fBcfopen\fR	(imtdev, ier)
\fBcfdisplayPix\fR	(pix, nx, ny, bitpix, frame, fbconfig, zscale, ier)
\fBcfreadCursor\fR	(sample, x, y, key, ier)
\fBcfsetCursor\fR	(x, y, wcs, ier)
\fBcfsetWCS\fR	(name, title, a, b, c, d, tx, ty, z1, z2, zt, ier)
\fBcfgetWCS\fR	(name, title, a, b, c, d, tx, ty, z1, z2, zt, ier)
\fBcfsetFrame\fR	(frame)
\fBcfclearFrame\fR	(ier)
\fBcfclose\fR	()

\fBcfsetMapping\fR	(region, sx,sy,snx,sny, dx,dy,dnx,dny, ref, ier)
\fBcfgetMapping\fR	(region, sx,sy,snx,sny, dx,dy,dnx,dny, ref, ier)
\fBcfqueryMap\fR	(wcs, region, sx,sy,snx,sny, dx,dy,dnx,dny, objref, ier)

\fBcfdisplayIRAF\fR	(fname, band, frame, fbconfig, zscale, ier)
\fBcfisIRAF\fR	(fname, isiraf)
\fBcfreadIRAF\fR	(fname, band, pix, nx, ny, bitpix, title, ier)

\fBcfdisplayFITS\fR	(fname, frame, fbconfig, zscale, ier)
\fBcfisFITS\fR	(fname, isfits)
\fBcfreadFITS\fR	(fname, pix, nx, ny, bitpix, title, ier)

\fBcfcomputeZscale\fR	(pix, nx, ny, bitpix, z1, z2)
\fBcfzscaleImage\fR	(pix, nx, ny, bitpix, z1, z2)

\fBcfprintPix\fR	(cmd, pix, nx, ny, annotate, ier)
\fBcfprintPixToFile\fR	(fname, pix, nx, ny, annotate, ier)

\fBcfreadImage\fR	(pix, nx, ny, ier)
\fBcfreadFrameBuffer\fR	(pix, nx, ny, ier)
\fBcfreadSubRaster\fR	(lx, ly, nx, ny, pix, ier)
\fBcfwriteSubRaster\fR	(lx, ly, nx, ny, pix, ier)

\fBcfselectFB\fR	(nx, ny, fb, w, h, nf, reset)
\fBcfsetFBConfig\fR	(configno)
\fBcfgetFBConfig\fR	(configno, w, h, nf)
\fBcflookupFBSize\fR	(configno, w, h, nf)

\fBcfsetZTrans\fR	(ztrans)
\fBcfsetZScale\fR	(z1, z2)
\fBcfsetSample\fR	(nsample)
\fBcfsetSampleLines\fR	(nlines)
\fBcfsetContrast\fR	(contrast)
\fBcfsetName\fR	(imname)
\fBcfsetTitle\fR	(imtitle)

\fBcfgetFrame\fR	(frame)
\fBcfgetZTrans\fR	(ztrans)
\fBcfgetZScale\fR	(z1, z2)
\fBcfgetSample\fR	(nsample)
\fBcfgetSampleLines\fR	(nlines)
\fBcfgetContrast\fR	(contrast)
\fBcfgetName\fR	(imname)
\fBcfgetTitle\fR	(imtitle)

\fBcfmapFrame\fR	(frame, ier)
\fBcfmarkPoint\fR	(x, y, number, size, type, color, ier)
\fBcfmarkcoordsfile\fR	(fname, type, size, color, label, ier)
\fBcfmarkPointLabel\fR	(x, y, label, size, type, color, ier)
\fBcfmarkLine\fR	(xs, ys, xe, ye, color, ier)
\fBcfmarkBox\fR	(lx, ly, ux, uy, fill, color, ier)
\fBcfmarkPolygon\fR	(xarray, yarray, npts, fill, color, ier)
\fBcfmarkPolyline\fR	(xarray, yarray, npts, color, ier)
\fBcfmarkCircle\fR	(x, y, radius, fill, color, ier)
\fBcfmarkCircAnnuli\fR	(x, y, radius, nannuli, sep, color, ier)
\fBcfmarkEllipse\fR	(x, y, xrad, yrad, rotang, fill, color, ier)
\fBcfmarkEllipAnnuli\fR	(x, y, xrad, yrad, ang, nannuli, sep, color, ier)
\fBcfmarkText\fR	(x, y, str, size, angle, color, ier)
\fBcfsetfont\fR		(font)
\fBcfsettextwidth\fR	(width)
\fBcfsetlwidth\fR	(width)
\fBcfsetlstyle\fR	(style)
\fBcfdeleteMark\fR	(x, y, ier)
\fBcfclearOverlay\fR	(ier)
\fBcfredrawOverlay\fR	(ier)
.TE
.bp
.NH
Fortran Example Tasks
.LP
	The examples shown here are for demonstration purposes only.  They
are based on working example tasks in the CDL source \fIexamples\fR
subdirectory, see the programs there for the full program listing.

.NH 2
Display Example
.in 0.5i
.ps -2
.vs -2
.nf
\f(CW
C ========================================================================
C  FDISPLAY -- Example fortran program showing the use of the Client
C  Display Library (CDL) Fortran interface for displaying images.
C ========================================================================

	PROGRAM FDISPLAY
	character*64   imname

C 	\fIInitialize the CDL package\fP
	call \fBcfopen\fP (0, ier)
	if (ier .gt. 0) then
	    write (*,*) 'open: Error return from CDL'
	    goto 999
	endif

	write (*, "('Image Name: ', $)")
	read (5, *) imname
	write (*, "('Frame Number: ', $)")
	read (5, *) iframe
	write (*, "('Frame buffer configuration number: ', $)")
	read (5, *) ifb

C	\fIIf we've got a FITS format image, go ahead and display it.\fP
	call \fBcfisFITS\fP (imname, isfits)
	if (isfits .gt. 0) then
	    call \fBcfdisplayFITS\fP (imname, iframe, ifb, 1, ier)
	else 
C	    \fIWe've got an IRAF format image, go ahead and display it.\fP
	    call \fBcfisIRAF\fP (imname, isiraf)
	    if (isiraf .gt. 0) then
	        call \fBcfdisplayIRAF\fP (imname, 1, iframe, ifb, 1, ier)
	    else 
C	    	\fIUnrecognized image, punt and exit.\fP
	        write (*,*) 'Unrecognized image format'
	    endif
	endif

C 	\fIClean up and exit.\fP
999	continue
	call \fBcfclose\fP (ier)
	end
\fR
.fi
.ps +2
.vs +2
.in -0.5i
.bp
.NH 2
Interactive Graphics Overlay Example
.in 0.5i
.ps -2
.vs -2
.nf
\f(CW
C ==========================================================================
C  FTVMARK --  Example fortran program showing the use of the Client
C  Display Library (CDL) Fortran interface for doing graphics overlay. No
C  checking of the error flag is done here for space considerations.
C ==========================================================================

	PROGRAM FTVMARK
	include 	"\fBcdlftn.inc\fP"
	character*64   	imname

C 	\fIInitialize the CDL package\fP
	call \fBcfopen\fP (0, ier)

	write (*, "('Image Name: ', $)") 
	read (5, *) imname
	write (*, "('Frame Number: ', $)")
	read (5, *) iframe
	write (*, "('Frame buffer configuration number: ', $)")
	read (5, *) ifb

C	\fIIf we've got a FITS format image, go ahead and display it.\fP
	call \fBcfisFITS\fP (imname, isfits)
	if (isfits .gt. 0) then
	    call \fBcfdisplayFITS\fP (imname, iframe, ifb, 1, ier)
	else 
C	    \fIWe've got an IRAF format image, go ahead and display it.\fP
	    call \fBcfisIRAF\fP (imname, isiraf)
	    if (isiraf .gt. 0) then
	        call \fBcfdisplayIRAF\fP (imname, 1, iframe, ifb, 1, ier)
	    else 
C	        \fINo valid image given, so map the current display for marking.\fP
	        call \fBcfmapFrame\fP (iframe)
	    endif
	endif

C 	\fINow that we've got an image displayed or mapped, enter a cursor loop to mark the image. \fP
	call markInteractive ()

C 	\fIClean up and exit\fP
999	continue
	call \fBcfclose\fP (ier)
	end

C   \fIMARKINTERACTIVE -- Subroutine for processing the cursor loop.\fP
	subroutine markInteractive ()
	include 	"\fBcdlftn.inc\fP"
	real		angle, rx, ry, txsize
	integer		nx, ny, x, y, x2, y2, fill, size, color
	integer		number, radius, xrad, yrad, nannuli, sep
	character	key
	character*64	cmd, str

C  	\fIAllocate a 1024x1024 array for pixels. \fP
	character	pix(1048576)

C	\fI....Initialize the local parameters to use\fP

C	\fIRead a cursor keystroke telling us what to do.\fP
10 	call \fBcfreadCursor\fP (0, rx, ry, key, ier)

C	\fIRound the real cursor position to integer pixel positions.\fP
	    x = nint (rx + 0.5)
	    y = nint (ry + 0.5)

C	\fICheck the keystroke and take the appropriate action.\fP
C	    \fIColon Commands\fP
	    if (key .eq. ':') then
C		\fIRead a three character command and value field and process the colon command\fP
		read (*,'(A3, i4)') cmd, ival
		if (cmd(1:3) .eq. 'ang') then
		    angle = real (ival)
		else if (cmd(1:3) .eq. 'col') then
		    color = ival
		else if (cmd(1:3) .eq. 'fil') then
		    fill = ival
\fI 			:
			....and so on to set local variables
			:\fR
		else if (cmd(1:3) .eq. 'pri') then
C		    \fIPrint contents of the current frame buffer\fP
		    call \fBcfreadFrameBuffer\fP (pix, nx, ny, ier)
		    call \fBcfprintPix\fP ("lpr", pix, nx, ny, 1, ier)
		else if (cmd(1:3) .eq. 'sta') then
			\fI....print out the status (value) of variables\fP
	 	endif

C	    \fIPoint Markers\fP
	    else if (key .eq. 'p') then
		call \fBcfmarkPoint\fP (x, y, 1, size, M_PLUS, color, ier)
	    else if (key .eq. 'x') then
		call \fBcfmarkPoint\fP (x, y, 1, size, M_CROSS, color, ier)
	    else if (key .eq. '_') then
		call \fBcfmarkPoint\fP (x, y, 1, size, M_HBLINE, color, ier)
	    else if (key .eq. 'o') then
C		\fIExample of a filled point marker \fP
		call \fBcfmarkPoint\fP (x, y, 1, size, or(M_CIRCLE,fill), color, ier)
\fI 			:
			....and so on to set other types of point markers\fR

C	    \fIOther Markers\fP
	    else if (key .eq. 'b') then
		print '("Hit another key to define the box ....")'
 		call \fBcfreadCursor\fP (0, rx, ry, key, ier)
	    	x2 = nint (rx + 0.5)
	    	y2 = nint (ry + 0.5)
		call \fBcfmarkBox\fP (x, y, x2, y2, fill, color, ier)
	    else if (key .eq. 'd') then
		call \fBcfdeleteMark\fP (x, y, ier)
	    else if (key .eq. 'e') then
		call \fBcfmarkEllipse\fP (x, y, xrad, yrad, angle, fill, color, ier)
            else if (key .eq. 't') then
                print '("Text string:  ", $)'
                read (*,'(A64)') str
                call \fBcfmarkText\fP (x, y, str, txsize, angle, color, ier)
\fI 			:
			....and so on to set other types of markers\fR

C	    \fIQuit\fP
	    else if (key .eq. 'q') then
		goto 998
	    endif

C	\fILoop back until we want to quit\fP
	goto 10
998	continue
	end
\fR
.fi
.ps +2
.vs +2
.in -0.5i
.bp
.NH
SPP Interface Summary
.LP
.in 0.5i
#include  "\fBcdlspp.h\fR"
.in -0.5i

.TS
center;
r l.
\fBcdl_open\fR	(imtdev, ier)
\fBcdl_displayPix\fR	(pix, nx, ny, bitpix, frame, fbconfig, zscale, ier)
\fBcdl_readCursor\fR	(sample, x, y, wcs, key, ier)
\fBcdl_setCursor\fR	(x, y, wcs, ier)
\fBcdl_setWCS\fR	(name, title, a, b, c, d, tx, ty, z1, z2, zt, ier)
\fBcdl_getWCS\fR	(name, title, a, b, c, d, tx, ty, z1, z2, zt, ier)
\fBcdl_setFrame\fR	(frame)
\fBcdl_clearFrame\fR	(ier)
\fBcdl_close\fR	()

\fBcdl_setMapping\fR	(region, sx,sy,snx,sny, dx,dy,dnx,dny, ref, ier)
\fBcdl_getMapping\fR	(region, sx,sy,snx,sny, dx,dy,dnx,dny, ref, ier)
\fBcdl_queryMap\fR	(wcs, region, sx,sy,snx,sny, dx,dy,dnx,dny, objref, ier)

\fBcdl_displayIRAF\fR	(fname, band, frame, fbconfig, zscale, ier)
\fBcdl_isIRAF\fR	(fname, isiraf)
\fBcdl_readIRAF\fR	(fname, band, pix, nx, ny, bitpix, title, ier)

\fBcdl_displayFITS\fR	(fname, frame, fbconfig, zscale, ier)
\fBcdl_isFITS\fR	(fname, isfits)
\fBcdl_readFITS\fR	(fname, pix, nx, ny, bitpix, title, ier)

\fBcdl_computeZscale\fR	(pix, nx, ny, bitpix, z1, z2)
\fBcdl_zscaleImage\fR	(pix, nx, ny, bitpix, z1, z2)

\fBcdl_printPix\fR	(cmd, pix, nx, ny, annotate, ier)
\fBcdl_printPixToFile\fR	(fname, pix, nx, ny, annotate, ier)

\fBcdl_readImage\fR	(pix, nx, ny, ier)
\fBcdl_readFrameBuffer\fR	(pix, nx, ny, ier)
\fBcdl_readSubRaster\fR	(lx, ly, nx, ny, pix, ier)
\fBcdl_writeSubRaster\fR	(lx, ly, nx, ny, pix, ier)

\fBcdl_selectFB\fR	(nx, ny, fb, w, h, nf, reset)
\fBcdl_setFBConfig\fR	(configno)
\fBcdl_getFBConfig\fR	(configno, w, h, nf)
\fBcdl_lookupFBSize\fR	(configno, w, h, nf)

\fBcdl_setZTrans\fR	(ztrans)
\fBcdl_setZScale\fR	(z1, z2)
\fBcdl_setSample\fR	(nsample)
\fBcdl_setSampleLines\fR	(nlines)
\fBcdl_setContrast\fR	(contrast)
\fBcdl_setName\fR	(imname)
\fBcdl_setTitle\fR	(imtitle)

\fBcdl_getFrame\fR	(frame)
\fBcdl_getZTrans\fR	(ztrans)
\fBcdl_getZScale\fR	(z1, z2)
\fBcdl_getSample\fR	(nsample)
\fBcdl_getSampleLines\fR	(nlines)
\fBcdl_getContrast\fR	(contrast)
\fBcdl_getName\fR	(imname)
\fBcdl_getTitle\fR	(imtitle)

\fBcdl_mapFrame\fR	(frame, ier)
\fBcdl_markCoordsFile\fR	(fname, type, size, color, label, ier)
\fBcdl_markPoint\fR	(x, y, number, size, type, color, ier)
\fBcdl_markPointLabel\fR	(x, y, label, size, type, color, ier)
\fBcdl_markLine\fR	(xs, ys, xe, ye, color, ier)
\fBcdl_markBox\fR	(lx, ly, ux, uy, fill, color, ier)
\fBcdl_markPolygon\fR	(xarray, yarray, npts, fill, color, ier)
\fBcdl_markPolyline\fR	(xarray, yarray, npts, color, ier)
\fBcdl_markCircle\fR	(x, y, radius, fill, color, ier)
\fBcdl_markCircAnnuli\fR	(x, y, radius, nannuli, sep, color, ier)
\fBcdl_markEllipse\fR	(x, y, xrad, yrad, rotang, fill, color, ier)
\fBcdl_markEllipAnnuli\fR	(x, y, xrad, yrad, ang, nannuli, sep, color, ier)
\fBcdl_markText\fR	(x, y, str, size, angle, color, ier)
\fBcdl_setFont\fR	(font)
\fBcdl_setTextWidth\fR	(width)
\fBcdl_setLineWidth\fR	(width)
\fBcdl_setLineStyle\fR	(style)
\fBcdl_deleteMark\fR	(x, y, ier)
\fBcdl_clearOverlay\fR	(ier)
\fBcdl_redrawOverlay\fR	(ier)
\fBcdl_setDebug\fR	(level)
.TE
