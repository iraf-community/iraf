IRAF V2.14EXPORT Release Notes
==============================

:Authors: IRAF Group
:Date: December 1, 2007
:Abstract: These release notes provide a summary of the major changes in V2.14.
 This is a major release of IRAF and will be available for all
 supported platforms. More detailed technical documentation of all
 system changes will be found in the ‘notes.v212’ files in the
 ``iraf$local`` directories. Detailed revisions notes for each application
 package are in the package directories in a file called Revisions,
 e.g. ``apphot$Revisions``.

IRAF V2.14 is a major-version release of the system and requires a FULL
INSTALLATION of the package, even if you have an existing V2.13 system.
The Installation Guide (pciraf.ps.Z) contains a detailed description of
the installation process for both first-time installations and updates
to existing systems. Installation will require downloading the AS, IB,
and NB distribution files from this directory. The AS distribution is
common to all PC-IRAF platforms and is required for any installation.
There are separate sets of binaries (the IB and NB distributions) for
each PC-IRAF platform and only those binaries required for a particular
platform will need to be downloaded.

PC-IRAF V2.14 supports the following platforms:

+-----------------------------+-------------+--------------------------+
| System                      | D           | Additional Systems       |
|                             | istribution |                          |
+=============================+=============+==========================+
| MacOSX 10.4 and higher      | MACX        | (Intel or PPC arch)      |
+-----------------------------+-------------+--------------------------+
| Linux RedHat 9              | RHUX        | Fedora, Enterprise,      |
|                             |             | CentOS                   |
+-----------------------------+-------------+--------------------------+
| Debian 3.1 (Sarge) and      | LNUX        | Other Debian-based       |
| higher                      |             | systems and all other    |
|                             |             | linux                    |
+-----------------------------+-------------+--------------------------+
| Cygwin                      | CYGW        | Windows XP only          |
+-----------------------------+-------------+--------------------------+

**NOTE**: The SUSE architecture has been dropped as of this release,
however support for SuSE systems is available in the LNUX architecture.
Future releases may seek to merge RHUX and LNUX into a single Linux
platform release to minimize the number of required distributions.

All versions are supported with a single IRAF distribution, although you
need to install separate binaries for for each platform.

HIGHLIGHTS OF THIS RELEASE
--------------------------

The primary changes to IRAF V2.14 were done to support the new Mac Intel
and Cygwin platforms, as well as fix long-standing issues with the
viability of the system on recent Linux and Mac OS X systems. However, a
number of key features and tasks will be new to users.

The following list is a partial summary of changes in this release:

-  Mac (Intel) Now a Supported Platform

-  Cygwin (for Windows XP) Now a Supported Platform

-  SUSE dropped as system architecture

-  ECL Now Default Interpreter

   Starting with this release, the ‘cl’ command to start IRAF will
   invoke the ECL interpreter as the default. Users can still access the
   old CL by starting with the command “% cl -o”

-  FITS Now Default Image File Format

   Starting with this release, FITS will be the default image format for
   all output images. Use of OIF (i.e. the “.imh” format) is still
   available by changing the ‘imtype’ variable setting.

-  ZPN Projection now supported by all tasks using the MWCS interface

   The ZPN projection driver from the Cambridge Astronomical Survey Unit
   was installed in the MWCS interface. ZPN is a zenithal/azimuthal
   polynomial projection.

-  RV.FXCOR now fits Gaussian peaks using double precision. Additionally
   the HJD output is printed to 5 decimals precision.

-  Access to 2MASS and USNO-B1 catalogs from the ASTCAT package

-  Display overlay colors may be set with expressions

   -  The masks specified by the ‘bpmask’ or ‘overlay’ parameters may
      now be set using image data or colors represented by expressions.
      See the ‘Overlay Colors’ section of the DISPLAY help page for
      details.
   -  Transparent masks (using the name ‘transparent’ or a mapped color
      less than zero) are now permitted.

-  IMEDIT has the following changes:

   -  A new option to do vector constant replacement was added. This is
      particularly useful for editing bad pixel masks.
   -  New options ‘=’, ‘<’, and ‘>’ to replace all pixels with values
      ==, <=, or >= to the value at the cursor with the constant value
      was added. This is useful for editing object masks.
   -  The ‘?’ help page is now set by an environment variable rather
      than hardcoded to a file in lib$src. The environment variable is
      imedit_help and is set in tv.cl to point to the file in the source
      directory.

-  New Tasks In the System:

   -  images.tv:

      -  BPMEDIT - Examine and edit bad pixel masks associated with
         images

   -  images.imcoords:

      -  MKCWCS - Make or update a simple celestial wcs
      -  MKCWWCS - Make or update a simple celestial/wavelength 3D wcs

   -  lists:

      -  RAVERAGE - Compute running average, standard deviation and
         envelope

   -  noao.nproto:

      -  SKYSEP - Compute are separation between two RA/Dec values
      -  SKYGROUP - Group a list containing RA/Dec into spatial sublists

-  New Parameters

   -  images.imutil:

      -  HSELECT - Added a ‘missing’ parameter to be used when keywords
         isn’t in header.

-  New Environment Variables

   Two new environment variables are available for use in IRAF
   networking:

   -  KS_RETRY, if defined, is the number of retry attempts using the
      default rsh (or KSRSH) protocol. The task will sleep for 1 second
      between attempts and then loop back to try again to make the
      connection. This is meant to avoid potential clashes between
      multiple machines connecting simultaneously as with the pipeline.

   -  KS_NO_RETRY which when defined instructs the task *not* to attempt
      a retry using the fallback rexec protocol. This test is made after
      the KS_RETRY checks to allow for various combinations of settings
      to allow the code to skip retries entirely (i.e. define only
      KS_NO_RETRY), retry using the default protocol but not with rexec
      (i.e. define KS_RETRY as some value and set KS_NO_RETRY), or retry
      only with rexec (i.e. old behavior, don’t define anything).

-  Increased Buffer Sizes

   -  Various environment buffers were increased to allow for longer
      settings of $PATH or other strings that would occassionally
      overflow.
   -  SZ_IMNAME was increased from 79 to 128 characters.
   -  SZ_ERRMSG in system error strings increased from 80 chars to
      SZ_LINE (1023) to allow inclusion of full paths in error messages.
   -  The FFT procedures in the VOPS interface now permit vectors of
      2^31 points (up from 2^15)

-  New Developer Libraries.

   Several new libraries are available for SPP developers:

   -  Installed the ‘mef’ library from the FITSUTIL package for doing
      general MEF manipulation. This will remove the dependency on
      FITSUTIL from several external packages.
   -  Installed the ‘dbc’ routines from the FITSUTIL package. This is an
      extension to the imgead header database routines that allow for a
      comment field to be created/manipulated in the header.
   -  xtools$xtargs.x is a simple interface to parse an argument string
      consisting of a list of whitespace separated keyw=value pairs.
