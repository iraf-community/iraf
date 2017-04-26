# X11IRAF -- X11/GUI development utilities and applications developed by the
# IRAF Project, National Optical Astronomy Observatories, 1994.
#
# Version 1.2 - Fri Apr 28 14:01:32 MST 2000
# (Principal authors Doug Tody, Mike Fitzpatrick. Email: iraf@noao.edu)

    README		This file
    README.install	Installation instructions for source or binaries
    Imakefile		Global x11iraf Imakefile
    Revisions		Package revisions log
    X11IRAF.tmpl	Imakefile template for x11iraf

    bin,<arch>		The binaries get installed here
    lib,<arch>		The libraries get installed here
    doc			Postscript documentation for the programs
    include		The header files get installed here
    app-defaults	The applications defaults files get installed here
    man			The manual pages are installed here

    cdl			Source for the Client Display Library (CDL)
    obmsh		Source for the OBM shell
    xgterm		Source for xgterm (xterm with OBM graphics)
    ximtool		Source for ximtool (image display server/browser)
    guidemo		Some sample GUI applications
    xtapemon		IRAF tape monitor GUI
    vximtool		Source for Virtual XImtool dummy display server

    obm			Source for the Object Manager (OBM)
    xpm			OBM-compatible copy of the X pixmap source
    xaw3d		OBM-compatible copy of the 3D Xaw widget set
    util		Miscellaneous utility programs

The Xaw3D and XPM libraries are publically available software packages which
x11iraf only uses, we did did not develop them.  Similarly OBM uses Tcl as
well as a collection of Xt-based widgets, including the FWF (Free Widget
Foundation) widgets, which were developed by others.  Even if you already
have copies of these libraries on your system you should let x11iraf use the
included versions, as interface evolution will likely render other versions
incompatible with x11iraf (we periodically update our copies and perform the
necessary integration).

Both XGterm and XImtool rely upon the IRAF Object Manager for the GUI.
The GUIDEMO package illustrates how to use GUIs in IRAF applications.

XGterm provides a Tek 4012 compatible graphics terminal emulation plus, for
clients in the know, a datastream driven widget server capability using the
Object Manager to provide full access to the underlying toolkit and widget
set.  The remote client application downloads a GUI file to the widget
server (xgterm) which executes the GUI.  While the GUI is executing it
exchanges messages with the remote client application at runtime via
interprocess communication.  In the case of Xgterm, this currently uses
a serial (tty based) protocol.

XImtool is an image display server.  This provides an image display
capability to remote client applications using the standard imtool/iis image
display protocol.  The image display server allows a number of image frame
buffers to be created and displayed.  The client can read and write data in
these frame buffers.  Any frame or combination of frames can be displayed.
Various display options are provided, e.g., zoom and pan, flip about either
axis, frame blink, windowing of the display, and colortable enhancement.

XTapemon is a conventional Xt/Athena application which allows the status of
an IRAF tape job to be monitored continuously while the tape is being
accessed.

Manual pages are included for all the above utilities.


INSTALLATION

Before building x11iraf you should check the archives on iraf.noao.edu to
see if there are already pre-built binaries available for your platform.
To build x11iraf type, in the x11iraf root directory,

    % xmkmf
    % make World		# (or preferably, make World >& spool &)

This will do a "make World", build everything, and install into the x11iraf
bin and other install directories.  To install somewhere else you can manually
move the binaries to /usr/local/bin or wherever you wish to install them.
Alternatively the X11IRAF.tmpl file can be edited to customize the install.
The minimal install requires moving the executables in bin to a public
directory, and installing the app-defaults files "XGterm" and "XTapemon".
ximtool does not require installation of an app-defaults file.  See the
manual pages for more detailed configuration instructions.

To build the GUIDEMO IRAF package type "mkpkg" in the guidemo directory.

The LIB and INCLUDE directories should be referenced in applications which
use any of the x11iraf libaries (libobm.a, libXaw3d.a, libXpm.a, libcdl.a).

