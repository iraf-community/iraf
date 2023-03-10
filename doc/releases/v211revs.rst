IRAF V2.11EXPORT Release Notes
==============================

:Authors: IRAF Group
:Date: August 27, 1997
:Abstract: Modifications and additions to IRAF V2.11EXPORT, compiled since the last
 documented release of IRAF, V2.10.3, are summarized below. V2.11EXPORT
 is a major release of IRAF and will be available for all supported
 platforms. These release notes provide a summary of the major changes in
 V2.11. More detailed technical documentation of all system changes will
 be found in the notes.v210 and notes.v211 files in the
 ``iraf$doc`` and ``iraf$local`` directories.

Things to be aware of or watch out for
--------------------------------------

Parameter file changes
~~~~~~~~~~~~~~~~~~~~~~

Since this is a major release we recommend that you do a *mkiraf* and
delete all your old parameter files. You may choose not to do this if
you are in the midst of a project and have setups that may be difficult
to reproduce. Old IMAGES package parameter files will no longer be
recognized, however, because of the package reorganization mentioned
below. Generally, old parameter files are merged automatically with any
new parameter files if there have been any changes, but if you do have
problems you will need to *unlearn* the task before you can proceed. A
list of the parameter file changes appears below and you may wish to
check this list to see how this will affect your setups.

The automatic parameter file update/merge mechanism, which is used if
you do not initialize your parameters with *mkiraf*, is based on file
date comparisons. If you run IRAF V2.10 after V2.11 has been
installed, the file dates on your uparm parameter files will be more
recent than the V2.11 installation date. If you then try to run V2.11,
the automatic parameter file merge/update will fail due to the file
dates. The system only updates personal parameter files which are
older than the update date of the system. A *mkiraf* avoids the
problem if you delete your parameter files, causing them to be updated
from the system default versions.

Networking change
~~~~~~~~~~~~~~~~~

The “set node = foo” syntax, used to enable remote image display under
IRAF networking, has changed. The new syntax requires that an
exclamation be appended to the node name as in the example below (this
dates back to V2.10.4 so many users will already be familiar with the
feature).

::

   cl> set node = "orion!"

Image format change
~~~~~~~~~~~~~~~~~~~

The internal IRAF image format (.imh images) has changed. V2.11EXPORT
can read the old image format but the new image format is not readable
by V2.10.4 or earlier versions. This means that you can not easily go
from the new IRAF system (V2.11) to an old one (V2.10.4 or earlier)
unless you first convert the V2.11 IRAF images to FITS files. All your
old V2.10.4 or earlier images are readable by V2.11EXPORT. The benefit
is that the new image format is machine independent, slightly more
storage efficient, and supports long file pathnames. If it is necessary
to be able to read images written by V2.11 with older software, V2.11
can be made to write the old IRAF image format by setting the
*oifversion* environment variable, e.g., “set oifversion = 1” (the
default is version 2). See below for details.

FITS kernel
~~~~~~~~~~~

A FITS image kernel is available in V2.11, allowing runtime read and
write access to FITS files on disk. There are many related changes to
IRAF image i/o and FITS support. More information on the new image
kernel, and on the expanded FITS support available in V2.11, is given
below.

RFITS/WFITS changes
~~~~~~~~~~~~~~~~~~~

*rfits* and *wfits* have been modified to support multi-extension FITS
files. The extension numbering convention used is the same as in the
FITS image kernel. As a result, users who read simple FITS files on disk
with a command such as “rfits diskfilename 1 foo” will find that this no
longer works - instead use “rfits diskfilename 0 foo”. See below for
details.

Merged SunOS and Solaris IRAF systems
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A single installation of Sun/IRAF will now simultaneously support both
SunOS and Solaris (previously separate IRAF distributions were required
for each).

Tape access
~~~~~~~~~~~

The “tapecap” mechanism has changed. The system now looks for the
filename “tapecap.” followed by the default “tapecap”. : should be the
hostname (as used by IRAF networking) of the server hosting the tape
drives described by the tapecap file. For example if host “gemini”
serves up some tape drives it’s tapecap file is named “tapecap.gemini”.
If a server-specific tapecap file is not found the default “tapecap” (on
the possibly remote server node) is used. This feature allows a single
IRAF installation to be shared by multiple servers.

Default magnitude zero changed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The default APPHOT magnitude zero point has been changed from 26.0 to
25.0 to bring it into agreement with the DAOPHOT package default value
and thereby avoid confusion for users who switch back and forth between
packages. The affected APPHOT tasks are *phot*, *photpars*, *polypars*,
*polyphot*, *qphot*, *radprof*, and *wphot*. The APPHOTX package in the
addon DIGIPHOTX package will retain the old zero point values until IRAF
2.11 is released after which they will be updated.

The default value of the magzero parameter in *imexamine* was changed
from 30.0 to 25.0 for consistency with the DIGIPHOT package.

IMAGES package changes
----------------------

The IMAGES package has been reorganized by function into the 7
subpackages listed below.

::

   imcoords - Image coordinates package
   imfilter - Image filtering package
      imfit - Image fitting package
     imgeom - Image geometric transformation package
    immatch - Image matching and combining package
     imutil - Image utilities package
         tv - Image display utilities package

The new IMAGES package contains a total of 82 tasks, including 26 new
tasks from the IMMATCH and VOL external addon packages, 6 existing PROTO
package tasks, and 1 existing NOAO.PROTO package task. The undocumented
IMAGES package IMDEBUG and its 6 tasks have been deleted from the IMAGES
package. User should use the tasks in the ARTDATA package instead.

This reorganization of the IMAGES package should be mostly transparent
to the user and not affect any existing scripts, unless you were using
any of the 6 deleted tasks. By default, the IMAGES subpackages are
automatically loaded when you log in to the CL. Old parameter files will
not be recognized since the package names have changed.

Major system changes
--------------------

New FITS image kernel (FXF)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

V2.11 introduces a new image kernel providing runtime access to FITS
multi-extension image datafiles. What this means is that IRAF tasks can
now read and write FITS images directly at runtime. The native IRAF
image format (used by images with the .imh extension), remains the
default as it is the most efficient and well-tested format. IMH, FITS,
and the other types of images supported by IRAF can be used
interchangeably in most IRAF tasks. Although we have extensively tested
the new FITS image kernel, it is still evolving, is complex, and still
has some bugs. Users should use it with caution. Please let us know of
any problems.

Besides support for classical FITS images, the new FITS kernel also
supports multi-extension FITS files: several FITS files packed into one
large file with a PHU (Primary Header Unit) that contains global header
information shared by the other files. Multi-extension FITS files are
0-indexed, with the PHU being 0 and the first image extension (or other
data extension) at index 1. If there is no PHU then the first FITS image
is 0 and there is no global header.

For further details about the FITS kernel please see the new FITS Kernel
User’s Guide by Nelson Zarate.

Changes to the IRAF native image format (OIF)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  It was necessary to change the IRAF image format to increase the
   maximum path length for header and pixel files. This made it
   necessary to change the disk image format, since the old format only
   allowed 80 characters for the pixel file pathname. The path lengths
   can now be up to 255 characters.

   Support for two versions of the image and pixel file headers was
   added. V2.11 will read both the old image format (V1) and the new
   image format (V2). But the new image format is not readable by older
   versions of IRAF.

-  Native format IRAF images (OIF type or extension “.imh”) are now
   machine independent, for example, a PC and a Sun can now access the
   same images.

-  Support was added for byte swapping pixels. With the machine
   independent image header, this allows .imh images to be read on any
   node (integer) or any IEEE-compatible node (floating).

-  Some pointers: “strings foo.imh” (or other tools like the “less” file
   viewer) can be used at the Unix level to look at the text contained
   in the new V2 OIF image headers.

IMFORT changes
~~~~~~~~~~~~~~

-  IMFORT was brought up to date to read and write the new V2 “.imh”
   images. The old V1 format is still supported but new images are
   written using the new machine independent V2 format by default.

-  Image headers can now be any size (the old IMFORT had a fixed,
   relatively low, limit on the image header size).

-  The “min_lenuserarea” variable is now supported by IMFORT (since
   IMFORT is host level the variable must be defined in the host
   environment). The builtin default header buffer is 64000 chars, which
   is about 800 card images.

Environment variables
~~~~~~~~~~~~~~~~~~~~~

Several new environment variable have been added to the system.

-  The new environment variable *imextn* determines the image kernels
   (image file formats) recognized by IRAF and defines the mapping of
   imagefile extensions to these image formats. A file that does not
   have an extension listed in imextn may not be recognized as an image
   by all IRAF tasks. The default value of imextn is as follows:

   ::

      imextn = "oif:imh fxf:fits,fit plf:pl qpf:qp stf:hhh,??h"

   IRAF tasks will not recognize a file as an image unless it has an
   extension (except *rfits* which will read FITS files on disk that
   have no extensions). The *rename* task can be used to add extensions
   to image files if needed. “imextn” can be redefined (use reset imextn
   = “new-value”) to modify the mapping of extensions to image types.

   The meaning of the fields of the default “imextn” are as follows.
   Each substring corresponds to a single kernel. The “xxx:” is the
   internal name of the image kernel, i.e. “oif”, “fxf”, “plf”, etc. A
   comma delimited list of the extensions, or extension patterns,
   associated with that image format follows the colon. For example, for
   the FITS image kernel, the internal kernel name is “fxf” and the
   system default file extensions are “.fits” and “.fit”.

   -  oif:imh - The original (native) IRAF image format.

   -  fxf:fits,fit - The FITS image extension format, which supports
      classical FITS images as well as multi-extension FITS files.

   -  plf:pl - The pixel list format used for compressed pixel masks.

   -  qpf:qp - The QPOE image format for event list data (typically
      X-ray or other high energy data).

   -  stf:hhh,??h - The Space Telescope runtime GEIS image format.

   Users can define extensions for the fxf and stf kernels. For example,
   if you have FITS files on disk that have a .ft extension then you can
   reset imextn so that IRAF will recognize these image extensions:

   ::

      cl> reset imextn="fxf:ft"

   The new .ft extension for the FITS kernel images will now override
   the default values - the other kernels remain unchanged. “.fits” will
   no longer be recognized as a FITS file unless you include it in the
   list of extensions for the “fxf” kernel.

   The first extension given for a kernel defines the default file
   extension for new images of that type (rather than e.g. the value of
   imtype, or a builtin default).

   The value of “imextn” is only read once when a process starts up.
   Hence it is advisable to do a “flpr” (flush process cache) after
   changing this variable, to force all processes to re-read it.

-  The environment variable *imtype* defines the default image type for
   new images created by IRAF. If a file extension is given explicitly
   when creating a new image then this file extension, in combination
   with the “imextn” mappings, determines the type of the new image.
   Otherwise the type is determined by the value of “imtype”. Typical
   values are “imh” for native IRAF images, or “fits” for FITS images.
   The internal kernel name documented above for “imextn” can be used
   instead of a file extension to ensure that the desired image format
   is used regardless of what extensions are assigned to that type by
   imextn.

   The default value of imtype is “oif,noinherit” which means that the
   IRAF native format is the default for all new images, regardless of
   the type of the input image (i.e. do not “inherit” the input image
   type). “inherit” was the default in V2.10 and earlier versions of
   IRAF.

   For example, if you wish to use FITS as the default for new images
   you can set imtype=fits as follows:

   ::

         cl> reset imtype="fits"
         cl> flpr                  % needed before the next task execution

   Assuming “imextn” defines “.fits” as a valid file extension for FITS
   imagefiles (kernel “fxf”), all new images produced by IRAF will be
   FITS files with the extension .fits unless some other extension is
   given explicitly when creating a new image.

   ::

         cl> reset imtype = "imh,inherit"

   This example sets the default type for new images to “.imh” for
   native format images, but enables image type inheritance. By default
   new images will be of the same type as the input image. For example
   if a FITS image is being read another FITS image will be output, or
   if a pixel mask is being read a pixel mask will be created.

   You can override the default output image type specified by imtype by
   giving an image extension (as defined by imextn) explicitly in the
   image name, e.g. “pix.imh”, “pix.fits”, “pix.pl” and so on.

-  A new environment variable called *imclobber* has been added. The
   default value is set to no. If imclobber is set to yes, images can
   and will be overwritten, without warning, when you create new images.

-  The original IRAF image format (OIF) kernel now supports an
   environment variable *oifversion* which, if defined, specifies the
   file version for new OIF images (for example, oifversion=2 produces
   the new format, or version 2 images). By default the variable is
   undefined, the builtin OIF default will be in effect, and new-format
   images will be generated. This variable is provided only for
   backwards compatibility, e.g., when using V2.11 IRAF with old
   software which cannot easily be updated.

New intrinsic functions
~~~~~~~~~~~~~~~~~~~~~~~

Two new intrinsic functions were added to the CL, *imaccess* and
*defvar*. *imaccess* tests whether an image exists, e.g.,
(imaccess(“dev$pix”)) or (imaccess(foo.fits[3])). *defvar* tests whether
an environment variable exists, e.g. (defvar(“imextn”)).

System default modifications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  The default size of the user area (*min_lenuserarea*) was increased
   to 64000 (about 800 80 character cards). There was some ambiguity
   about units for min_lenuserarea; it should be consistently characters
   now.

-  The maximum number of open IRAF logical files was increased from 128
   to

   256. This is good news for *imcombine* users.

-  Various buffer limits were increased:

   -  The IRAF line length was increased from 161 to 1023 characters.
      One often ran into this lower limit when entering a long list of
      input image names, for example.

   -  CL commands can now be 2047 characters long (the old limit was

      1024) 

            -  this is particularly useful for scripts.

   -  IRAF file names can now be up to 255 characters long.

   -  Expanded file names (pathnames) can be up to 511 characters long.

Libraries added
~~~~~~~~~~~~~~~

The Starlink positional astronomy library SLALIB was added to the math
package.

Graphics changes
~~~~~~~~~~~~~~~~

-  SGI Translator change: Modified the header ID string produced by
   sgi2uapl to be “%!PS”, this is required by certain models of
   printers.

-  Installed graphcap support for the STSDAS PostScript graphics kernel.

-  The SGI graphics kernel was upgraded with a better roman font, and a
   new greek font was added. To use the new high-quality greek fonts use
   the “:raw-latex:`\fG`” escape sequence when you use the “T” keystroke
   to mark text in a plot, e.g.,
   :raw-latex:`\fGa `:raw-latex:`\fRHydra `would produce ” Hydra”. The
   greek font may also be used in label parameters for tasks like GRAPH,
   for example

   ::

          cl> graph dev$pix xlabel="Wavelength\\fG(A)"

   would produce an Angstrom symbol in parenthesis. The double backslash
   is necessary to pass the escape string thru the CL. A file containing
   the mappings for the greek fonts and other special characters can be
   found at http://iraf.noao.edu/greek.ps.

FITS-related core-level task changes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IMHEADER
^^^^^^^^

The behavior of *imheader* has changed a bit - typed with no arguments
it will list all the images in the current directory, as in the
following example:

::

           cl> imhead
           pix4.imh[512,512][short]: m51  B  600s
           boc.fits: FXF: must specify which FITS extension (boc.fits)
           fits.fits[512,512][short]: m51  B  600s
           pix.fits[512,512][short]: m51  B  600s
           pix3.fits[512,512][short]: m51  B  600s
           pix5.fits: FXF: must specify which FITS extension (pix5.fits)
           zero.fits: FXF: must specify which FITS extension (zero.fits)
           mask.pl[512,512][int]: m51  B  600s
           foo.qp[1024,811][int]:

The multi-extensions FITS files show up in the list with the message
“FXF: must specify which FITS extension”, since these files contain
multiple images and the task does not know which image to open to get
header information.

At present imheader does not use “imextn” to determine what is and is
not an image. The parameter “imlist” defines the valid imagefile
extensions. If imextn is modified “imlist” may need to be modified as
well.

RENAME
^^^^^^

The *rename* task was modified to allow a destination directory to be
specified without changing the filename. A new “field” parameter option
“all” was added and made the default so the entire filename is changed
(or moved in the case of a destination directory). When field is set to
“extn” filenames without an extension will not have the new extension
appended so a filename isn’t generated which can get overwritten.

*rfits*/*wfits*
^^^^^^^^^^^^^^^

Some changes were also implemented in *rfits* and *wfits* to add support
for multi-extension FITS files.

-  The default action of *wfits* when writing to tape is unchanged for
   single image files.

   *wfits* now has a new parameter called “fextn” and it is set to
   “fits”. This parameter only affects FITS files written by *wfits* to
   disk - the extension .fits will automatically be added to the
   filenames, so that the files will be automatically recognized by the
   FITS image kernel.

   *wfits* also has two additional parameters called “extensions” and
   “global_hdr” that deal with writing multi-extension FITS files.

-  The default action of *rfits* from tape to disk remains unchanged.

   If *rfits* is used to read FITS files on disk then users need to be
   aware of a change to the behavior of the “file-list” parameter.
   File-list is now used to define the list of files on the tape as well
   as the list of extensions in a multi-extension FITS image. To read
   single FITS images on disk use “” as the value for “file-list”. Some
   users have been using “1” for this value but now that value will try
   to read the first extension which doesn’t exist - use “0” if you wish
   to put something there.

   *rfits* will unpack multi-extension FITS files upon a read. If you
   wish to retain the multi-extension FITS format then use T2D and
   RENAME.

The help pages have been updated to reflect these changes.

*wfits* now writes ushort (16 bit unsigned short) images to FITS images
with bitpix=16, bscale=1.0, and bzero=32768.0 by default. *rfits* will
read these images back as type ushort.

General changes
~~~~~~~~~~~~~~~

-  The GSURFIT package has been updated to include support for the
   “half” cross terms option useful in computing plate solutions. Tasks
   that can make use of this feature have been updated although their
   default behaviors have not changed.

-  The code which computes the CD matrix from CDELT/CROTA was modified.
   The old code computed the diagonal (scale) terms correctly but the
   rotation terms were evidently incorrect. The old code was based on
   the 1988 Hanisch and Wells WCS paper and the new code is based on a
   more recent paper by Mark Calabretta et al, which supersedes the 1988
   representation. The affect of this change should be limited as it
   only affects rotated images for which CDELT is given but no CD matrix
   is defined. (V2.10.4p2)

-  Several new celestial coordinate projection functions have been
   added. Users with IPAC data that use the CAR projection function
   should now be able to read the image coordinates directly with
   LISTPIXELS, etc.

New tasks, or old tasks moved to new packages
---------------------------------------------

::

    Task Name      Package                  Function

    CCXYMATCH      IMCOORDS         Four new tasks for computing and evaluating
    CCMAP                           simple astrometric plate solutions and storing
    CCSETWC                         them in the image headers in fits-compatible
    CCTRAN                          format.

    CCFIND         IMCOORDS         CCFIND locate objects in an image given a
                                    celestial coordinate list and the image wcs.

    IMCCTRAN       IMCOORDS         Two new tasks for transforming celestial
    SKYCTRAN                        coordinate lists and image celestial
                                    coordinate systems from one celestial
                                    coordinate system to another.

    STARFIND       IMCOORDS         STARFIND automatically detects stellar objects
                                    in a list of images.

    WCSCTRAN       IMCOORDS         A new task for transforming between IRAF image
                                    coordinate systems.

    WCSEDIT        IMCOORDS         Two unaltered former PROTO package tasks for
    WCSRESET                        editing IRAF image coordinate systems.

    FRMEDIAN       IMFILTER         Four new tasks for doing circular/elliptical/
    FRMODE                          ring image median filtering.
    RMEDIAN
    RMODE

    IM3DTRAN       IMGEOM           The former addon VOL package task IM3DTRAN for
                                    doing 3D image transposes.

    GEOXYTRAN      IMMATCH          A new task for transforming coordinate lists
                                    using the results of the GEOMAP task.

    IMCENTROID     IMMATCH          Four new tasks for computing matched pixel
    SKYXYMATCH                      lists. IMCENTROID is a modified version of the
    WCSXYMATCH                      PROTO package task of the same name.
    XYXYMATCH

    SKYMAP         IMMATCH          Two new tasks for computing geometric
    WCSMAP                          transforms using the image coordinate system
                                    information.

    IMALIGN        IMMATCH          Three new tasks for doing automated image
    SREGISTER                       registration. IMALIGN is a modified version
    WREGISTER                       of the PROTO package task of the same name.

    WCSCOPY        IMMATCH          A new task for copying the coordinate system
                                    of a reference image to a set of input images.

    PSFMATCH       IMMATCH          A new task for matching the PSFs of a set of
                                    input images to the PSF of a reference image
                                    using Fourier techniques.

    LINMATCH       IMMATCH          A new task for matching the linear intensity a
                                    scale of a set of input images to the
                                    intensity scale of a reference image.

    IMFUNCTION     IMUTIL           The former PROTO package task for applying a
                                    single argument function to an image.

    IMJOIN         IMUTIL           The former addon VOL package task for joining
                                    same-dimensioned images along a specified
                                    dimension.

    IMREPLACE      IMUTIL           The former PROTO package task IMREPLACE for
                                    replacing bad pixels based on their value.

    IMTILE         IMUTIL           A modified version of the NOAO.PROTO IRMOSAIC
                                    task for tiling same sized 2D images into a
                                    single mosaiced image.

    EXPORT         DATAIO           Two new tasks from the external IMCNV package
    IMPORT                          for exporting IRAF images to binary formats
                                    and for importing binary files into IRAF
                                    images.

    TEXT2MASK      PROTO            This new task appears in conjunction with a
                                    new  pixel mask based version of FIXPIX.

    IMEXTENSIONS   PROTO            This task selects and lists image extensions 
                                    in  files.   Image extensions currently occur 
                                    in  multi-extension FITS files and multi-group 
                                    Geiss (STF format) files.
      
    CCDMASK        CCDRED           This new task creates a pixel mask from a
                                    CCD image.

    AIDPAR         ONEDSPEC         New parameter set for automatic line
                                    identification for the tasks AUTOIDENTIFY,
                                    IDENTIFY and REIDENTIFY.

    AUTOIDENTIFY   ONEDSPEC         A new task to automatically identify lines
                                    and fit the dispersion function.

    SKYTWEAK       ONEDSPEC         Sky spectra are shifted and scaled to best
                                    subtract sky features from data spectra.

    TELLURIC       ONEDSPEC         Telluric calibration spectra are shifted and
                                    scaled to best divide out telluric features
                                    from data spectra.

    ASTCALC        ASTUTIL          An astronomical calculator.

    ASTRADIUS      ASTUTIL          Finds images within a circle on the sky.

Task and package deletions
--------------------------

CUBE, DUMP, GSUBRAS, MAXMIN, MKIMAGE, MKTEST: These tasks have been
superseded by the equivalent tasks in the IMAGES or ARTDATA packages.
The functionality of these tasks still exists in the
iraf$pkg/images/lib/zzdebug.x file.

REGISTER: This task has been renamed to GREGISTER.

IMALIGN, IMCENTROID, IMFUNCTION, IMREPLACE, WCSEDIT, WCSRESET: Moved to
the IMAGES package.

Modifications to old tasks
--------------------------

Grouped by package, a list of modifications to old tasks in IRAF are
summarized below. We have included modifications since the V2.10.3
release.

-  **IMFILTER:**

   -  FMEDIAN, FMODE, MEDIAN, MODE:

      Minimum and maximum good data value parameters zloreject and
      zhireject and a verbose option parameter were added.

   -  MEDIAN, MODE

      The 64 x 64 maximum kernel size limit was removed from these
      tasks.

-  **IMMATCH:**

   -  GEOMAP

      Renamed the output parameter to database for the sake of
      consistency with other new images tasks.

      Changed the default value of the reject parameter from 0.0 to
      INDEF.

      Added the transforms parameter. Transforms permits the user to
      specify the names of the output database records explicitly.

      Added the parameter results. Results permits the user to save a
      summary of the results including a description of the transform
      geometry, and a listing of the input coordinates, the fitted
      coordinates, and the fit residuals.

      Added the fitgeometry parameter. Fitgeometry permits the user to
      constrain the linear part of the fit to: 1) x and y shifts only,
      2) x and y shifts and rotation only, 3) x and y shifts and x and y
      scale changes only, 4) x and y shifts, rotation, and a scale
      change only, 5) x and y shifts, rotation, x and y scale changes
      only, and 5) x and shifts, rotation, skew, and x and y scale
      changes.

   -  GREGISTER

      Renamed the register task gregister to emphasize that it is paired
      with the geomap task and to avoid confusion with the new
      registration tasks.

   -  GEOTRAN, GREGISTER

      Renamed the transform parameter to transforms.

      Added the verbose parameter.

   -  GEOTRAN

      Added the ability to write to a section of an existing image.

   -  IMCOMBINE

      The limit of the number of images that may be combined has been
      removed. If the number of images exceeds the maximum number of
      open images permitted then the images are stacked in a single
      temporary image and then combined with the project option. Note
      that this will double the amount of diskspace temporarily. There
      is also a limitation in this case that the bad pixel mask from the
      first image in the list will be applied to all the images.

      Integer offsets may be determined from the image world coordinate
      system.

      A combination of ushort and short images now defaults to integer.

      Added support for type ushort images.

      Added a new options for computing offsets using the image wcs.

      Removed the limit on the maximum number of images that can be
      combined.

   -  IMALIGN, IMCENTROID

      Renamed the images parameter to input, changed the reference
      parameter mode from hidden to automatic, and reversed the order of
      the reference and coords parameters.

-  **IMUTILS:**

   -  IMEXPR

      Modified the task so that it will accept an image name that looks
      like a number in the first few characters, but which is really an
      image name. For example, “123.imh” or “../foo.imh”. The previous
      version of imexpr was treating any string which looked like a
      number in the first few characters as a numeric constant.
      (V2.10.4p2)

   -  IMREPLACE

      The lower value is now rounded up for integer images so that a
      range like 5.1-9.9 affects pixels 6-9 instead of 5-9.

   -  IMSUM

      Now allows “ushort” data types.

-  **TV:**

   -  DISPLAY

      The bad pixel mask, overlay mask, sample mask, and overlay colors
      parameters and functionality have been added. The “nsample_lines”
      parameter is now an “nsample” parameter.

      Bugs in the coordinate system sent to the image display for cursor
      readback were fixed.

   -  IMEDIT

      If xorder or yorder are zero then a median background is computed
      for the ‘a’ and ‘b’ keys.

   -  IMEXAMINE

      (‘a’ and ‘r’): The fit to the radial profile points now includes
      both a Gaussian and a Moffat profile. The Moffat profile exponent
      parameter, beta, may be fixed or left free to be fit.

      (‘a’ and ‘r’): New estimates fo the FWHM were added to the ‘a’ and
      ‘r’ keys. These include the Moffat profile fit noted above, a
      direct measurement of the FWHM from the radially binned profile,
      and a Gaussian or Moffat fit to the radial enclosed flux profile.
      The output from these keys was modified to include the new
      information.

      (‘a’ and ‘r’): The direct FWHM may be used to iteratively adjust
      the fitting radius to lessen the dependence on the initial fitting
      radius value.

      (‘k’): Added a kimexam parameter set.

      The default value of rimexam.magzero parameter was changed from
      30.0 to 25.0 for consistency with the digiphot package.

-  **PROTO:**

   -  FIELDS

      The default value for the lines parameter was changed to an open
      upper limit.

      Changed the default value of the lines parameter from “1-999” to
      “1-” so that the upper bound would be open ended.

   -  FIXPIX

      This task replaces the old task (now obsolete.ofixpix) and works
      with the more general pixel mask facilities. It also provides
      greater flexibility in choosing the interpolation direction.

-  **ICFIT** used in various tasks:

   -  ICFIT

      The :xyshow output was modified to 1) not include colon labels,

      2) print (X, Y, Y fit, Weight) instead of (X, Y fit, Y), and 3)
         the printed values are those actually used in the fit when
         using composite points (naverage not 1).

-  **ARTDATA:**

   -  MK1DSPEC

      Lorentzian and Voigt profiles were added and the parameters and
      input line list format were changed. The widths are now FWHM
      instead of gaussian sigmas.

   -  MKOBJECTS, MKNOISE

      The default value of “ranbuf” was changed to zero.

   -  GALLIST

      The default value for the minimum elliptical galaxy axial ratio
      was changed to 0.3 to cover the range E0-E7 uniformly.

   -  MKPATTERN

      Now allows ndim=0 to make an dataless header. Now allows ushort
      pixel type.

-  **ASTUTIL:**

   -  SETJD

      The checking of the epoch keyword value was improved. Previously
      if there was a problem with the keyword value (missing or
      malformed) the task would use the epoch of the observation. Now it
      is an error if an epoch keyword is specified but the epoch value
      can’t be determined. Also a leading ‘B’ or ‘J’ is allowed and a
      warning will be given if the epoch value is unlikely.

   -  ASTHEDIT

      There are new astronomical functions and input/output functions.
      The command syntax may now use “=” as a delimiter as well as the
      whitespace.

      A new parameter “update” allows protecting images and accessing
      read-only images for the purpose of calculating and printing
      quantities.

      The special variable name “$I” has the value of the image name, $D
      the current date, and $T the current time.

      The case of no image name creates and deletes a temporary image so
      the task can be used purely as a calculator (but see astcalc).

-  **CCDRED:**

   -  CCDPROC

      The bad pixel fixing was modified to allow use of pixel masks,
      images, or the text file description. Bad pixel masks are the
      desired description and use of text files is only supported for
      backward compatibility. Note that support for the trimmed or
      untrimmed conversion from text files has been eliminated.

      Line-by-line overscan/prescan subtraction is now provided with
      three simple algorithms.

   -  COMBINE

      The limit of the number of images that may be combined has been
      removed. If the number of images exceeds the maximum number of
      open images permitted then the images are stacked in a single
      temporary image and then combined with the project option. Note
      that this will double the amount of diskspace temporarily. There
      is also a limitation in this case that the bad pixel mask from the
      first image in the list will be applied to all the images.

      Integer offsets may be determined from the image world coordinate
      system.

      Fixed a bug where a variable was improperly used for two different
      purposes causing the algorithm to fail. This also affected
      IMCOMBINE and SCOMBINE. See bug 316 for details. (V2.10.4p2)

   -  COSMICRAYS

      | The output bad pixel data accidentally included some extra
        fields making it incorrect to use the file directly with
        BADPIXIMAGE.
      | The extra diagnostic fields were removed. For details, see bug
        311 in the buglog. (V2.10.4p2)

-  **ECHELLE:**

   -  ECIDENTIFY

      The dispersion units are now determined from a user parameter, the
      coordinate list, or the database entry.

-  **IMRED** Spectral Packages:

   -  DOARGUS, DOFIBERS, DOHYDRA

      A sky alignment option was added.

      The aperture identification can new be taken from image header
      keywords.

      The initial arc line identifications is done with the automatic
      line identification algorithm.

   -  DOSLIT, DO3FIBER

      The initial arc line identifications is done with the automatic
      line identification algorithm.

-  **ONEDSPEC:**

   Support for the Sloan Sky Survey was added by allowing multifiber
   reductions with up to 500 fibers with non-linear dispersions.
   (V2.10.4p2)

   -  BPLOT

      Fixed a general problem in BPLOT and SLIST that caused a
      segmentation violation error. See buglog 312 for details.
      (V2.10.4p2)

   -  FITPROFS

      Modified to include lorentzian and voigt profiles. The parameters
      and positions file format have changed in this version. A new
      parameter controls the number of Monte-Carlo samples used in the
      error estimates.

   -  IDENTIFY

      The dispersion units are now determined from a user parameter, the
      coordinate list, or the database entry. A new key, ‘e’, has been
      added to add features from a line list without doing any fits.
      This is like the ‘l’ but without the automatic fitting before and
      after adding new features.

      A new key, ‘b’, has been added to apply an automatic line
      identification algorithm.

      The ‘x’ key has been changed to use the automatic line
      identification algorithm. The allows finding much larger shifts.

      The match parameter may now be specified either in user
      coordinates or in pixels. The default is now 3 pixels.

      The default threshold value has been changed to 0.

   -  REIDENTIFY

      A new parameter, “search”, was added to specify a search radius
      for the automatic line identification algorithm.

      The “nlost” parameter now also applies when not tracing; i.e. it
      will issue a warning and not record a solution if the specified
      number of features is lost when reidentifying against a specific
      reference spectrum as is done with multispec data.

      The task will now work with only a warning if the reference image
      is absent; i.e. it is possible to reidentify given only the
      database.

      The addfeatures function will now add features before a fit if
      there are no reference database features. Previously features
      could only be added after an initial fit using the reference
      features and, so, required the reference database to contain
      features for reidentification. This new feature is useful if one
      wants to uses a dispersion function from one type of calibration
      but wants to add features for a different kind of calibration.

   -  SAPERTURES

      This task has been modified to allow use of image header keywords
      as done in the APEXTRACT package.

   -  SARITH

      Previously both w1 and w2 had to be specified to select a range to
      be used. Now if only one is specified the second endpoint defaults
      to the first or last pixel.

      The noise band in multispec data is only copied from the primary
      spectrum and not modified. This is a kludge until the noise is
      handled properly.

      Fixed a problem in SARITH and SCOPY where a segmentation error
      occurred when a wavelength range was specified in the reverse
      sense of the data and without rebinning. See buglog 323 for
      details. (V2.10.4p2)

   -  SBANDS

      Fixed a problem in SBANDS that caused a segmentation violation
      when the number of input bandpasses was greater than 10. See
      buglog 298 for details. (V2.10.4p2)

   -  SCOPY

      Previously both w1 and w2 had to be specified to select a range to
      copy. Now if only one is specified the second endpoint defaults to
      the first or last pixel.

   -  SPECPLOT

      The scale and offset parameters may now be a value, a filename, or
      and image header keyword.

      The ‘f’ key was added to toggle between world and logical pixel
      coordinates.

   -  SPLOT

      The profile fitting and deblending was expanded to include
      lorentzian and voigt profiles. A new parameter controls the number
      of Monte-Carlo samples used in the error estimates.

   -  RSPECTEXT

      The task now automatically senses the presence of a header.

-  **APEXTRACT:**

   -  APALL, APSUM, APNOISE, APNORMALIZE, APSCATTER, APTRACE,
      APRECENTER, APRESIZE, APMASK, APFIND, APFLATTEN

      The “apertures” parameter can be used to select apertures for
      resizing, recentering, tracing, and extraction. This parameter
      name was previously used for selecting apertures in the
      recentering algorithm. The new parameter name for this is now
      “aprecenter”.

   -  APALL, APSUM

      The “nsubaps” parameter now allows onedspec and echelle output
      formats. The echelle format is appropriate for treating each
      subaperture as a full echelle extraction.

   -  APALL

      The aperture ID table information may now be contained in the
      image header under the keywords SLFIBnnn.

   -  APSUM

      The dispersion axis parameter was moved to purely a package
      parameter.

      As a final step when computing a weighted/cleaned spectrum the
      total fluxes from the weighted spectrum and the simple unweighted
      spectrum (excluding any deviant and saturated pixels) are computed
      and a “bias” factor of the ratio of the two fluxes is multiplied
      into the weighted spectrum and the sigma estimate. This makes the
      total fluxes the same. In this version the bias factor is recorded
      in the logfile if one is kept. Also a check is made for unusual
      bias factors. If the two fluxes disagree by more than a factor of
      two a warning is given on the standard output and the logfile with
      the individual total fluxes as well as the bias factor. If the
      bias factor is negative a warning is also given and no bias factor
      is applied. In the previous version a negative (inverted) spectrum
      would result.

-  **RV:**

   -  RVIDLINES, RVREIDLINES

      These tasks now work in the units of the input spectra.

   -  FXCOR

      The input spectra are converted to Angstroms for the
      crosscorrelation and analysis. Thus the velocities will be
      correctly computed regardless of the units of the input spectra.

-  **DAOPHOT:**

   -  DAOFIND

      A major bug in the DAOFIND task centering code that affects the
      computed x and y positions was fixed. Users should refer to bug
      log entry number 332 for details. (V2.10.4p2)

      A new roundness statistic was added to the DAOFIND output to bring
      the task into conformance with standalone DAOPHOT II. The new
      statistic is sensitive to and tries to eliminate detected objects
      which are significantly elongated in directions other than 0, 90,
      180, and 270 degrees. The original roundness statistic is stored
      in the ground column, the new one in the sround column. The same
      default roundness limits apply to both statistics. (V2.10.4p2)

   -  DAOPARS

      Added a new parameter “mergerad” to the DAOPARS parameter set.
      Mergerad permits the user to control the merging process. If
      mergerad is INDEF (the default setting) the default merging radius
      is used. If mergerad is 0 object merging is turned off altogether.
      Otherwise the user can set the merging radius to a specific value.
      Mergerad is used by the nstar and allstar tasks, but their default
      behavior is unchanged. (V2.10.4p2)

      Changed the name of the “critovlap” parameter to “critsnratio” to
      avoid confusion with the meaning of the parameter especially with
      regard the mergerad parameter. The behavior of the parameter is
      unchanged. (V2.10.4p2)

.. _parameter-file-changes-1:

Parameter file changes
----------------------

The parameter file changes below are for modifications between V2.10.4
and V2.11. For a list of parameter file changes between V2.10.3 and
V2.10.4 see the file iraf/v210/params.v2104.Z in the IRAF FTP archives.

In the tables below each parameter change is identified with one of the
following codes followed by task_name.parameter_name and the description
of the change.

-  n = new parameter
-  c = changed/modified parameter
-  d = deleted parameter

**TV:**

::

    n  display.nsample: replaces nsample_lines
    d  display.nsample_lines: replaced by nsample
    n  display.bpmask: specify a bad pixel mask
    n  display.bpdisplay: specify display mode for bad pixel mask
    n  display.bpcolors: specify overlay colors for bad pixel mask
    n  display.overlay: specify an overlay mask
    n  display.ocolors: specify overlay colors for overlay mask
    n  display.zmask: specify a sample mask for the zscale calculation
    c  imedit.xorder: now allows a value of zero for median background
    c  imedit.yorder: now allows a value of zero for median background
    n  rimexam.fittype: specify a profile type to fit - default is now moffat
    n  rimexam.iterations: specify number of iterations to adjust fitting radius
    n  rimexam.beta: specify beta value for moffat fitting
    c  rimexam.buffer: default value changed from 1 to 5
    c  rimexam.width: default value changed from 2 to 5
    c  rimexam.magzero: default value changed from 30 to 25
    n  wcslab.overplot: specify overplotting

**DATAIO:**

::

    n  wfits.fextn: extension to append to output disk FITS filename - default is
         fits
    n  wfits.extensions: write all images to a single FITS file ? - default is no
    n  wfits.global_hdr: prepend a global header to the FITS extensions - default
         is yes

**IMAGES:**

::

    n  fmedian.zloreject: good data minimum
    n  fmedian.zhireject: good data maximum
    n  fmedian.verbose: verbose option
    n  fmode.zloreject: good data minimum
    n  fmode.zhireject: good data maximum
    n  fmode.verbose: verbose option
    n  median.zloreject: good data minimum
    n  median.zhireject: good data maximum
    n  median.verbose: verbose option
    n  mode.zloreject: good data minimum
    n  mode.zhireject: good data maximum
    n  mode.verbose: verbose option
    n  geomap.transforms: list of record names
    n  geomap.results: results summary file
    n  geomap.fitgeometry: fitting geometry
    d  geomap.output: renamed to database
    c  geomap.reject: changed from 0.0 to INDEF
    n  [g]register.verbose: verbose option
    d  [g]register.transform: renamed to transfo
    n  geotran.verbose: verbose option
    d  geotran.transform: renamed to transforms
    c  imcombine.offsets: now allows specifying "wcs" to compute offsets from WCS
    d  imalign.images: renamed to input
    c  imalign.reference: went from hidden to auto
    c  imalign.coords: reversed places with reference
    d  imcentroid.images: renamed to input
    c  imcentroid.reference: went from hidden to auto
    c  imcentroid.coords: reversed places with reference
    n  imheader.imlist: default image names

**PLOT:**

::

    n  graph.ltypes: specify the line types
    n  graph.colors: specify the colors

**PROTO:**

::

    n  fixpix.masks: new version specifies bad pixel masks
    n  fixpix.linterp: specify mask values for line interpolation
    n  fixpix.cinterp: specify mask values for column interpolation
    n  fixpix.pixels: list pixels that are modified
    d  fixpix.badpixels: new version does not use bad pixel region description
    c  fields.lines: default value changed from "1-9999" to "1-"

**ARTDATA:**

::

    n  mk1dspec.profile: define the profile type
    n  mk1dspec.gfwhm: replaces sigma for gaussian profile width
    n  mk1dspec.lfwhm: width for lorentzian profile
    c  artdata.randbuf: default value changed from 1000 to 0.
    c  mkpattern.ndim: allows a value of 0 for a dataless header.
    c  mkpattern.pixtype: allows ushort.
    c  gallist.ar: default value changed to 0.3.
    d  mk1dspec.sigma: replaced by gfwhm

**ASTUTIL:**

::

    n  rvcorrect.keywpars: added to define keywords used
    n  asthedit.prompt: used for new calculator option
    n  asthedit.update: update the header
    n  asthedit.oldstyle: to allow backward compatibility

**APEXTRACT, IMRED** spectral packages:

::

    n  apnoise.apertures: select apertures to be used
    n  apfit.apertures: select apertures to be used
    n  apedit.apertures: select apertures to be used
    n  apfind.apertures: select apertures to be used
    n  apnormalize.apertures: select apertures to be used
    n  apscatter.apertures: select apertures to be used
    n  apsum.apertures: select apertures to be used
    n  aptrace.apertures: select apertures to be used
    n  apresize.apertures: select apertures to be used
    n  apmask.apertures: select apertures to be used
    n  apflatten.apertures: select apertures to be used
    n  aprecenter.apertures: select apertures to be used
    n  aprecenter.aprecenter: was what was previously the apertures parameter
    n  apall.apertures: select apertures to be used
    n  apall.aprecenter: was what was previously the apertures parameter

**ARGUS, HYDRA, SPECRED:**

::

    n  doargus.crval: for automatic line identifications
    n  doargus.crdelt: for automatic line identifications
    n  doargus.skyalign: night sky alignment option
    n  dohydra.crval: for automatic line identifications
    n  dohydra.crdelt: for automatic line identifications
    n  dohydra.skyalign: night sky alignment option
    n  dofibers.crval: for automatic line identifications
    n  dofibers.crdelt: for automatic line identifications
    n  dofibers.skyalign: night sky alignment option
    n  do3fiber.crval: for automatic line identifications
    n  do3fiber.crdelt: for automatic line identifications

**ARGUS, HYDRA, KPNOCOUDE, KPNOSLIT, SPECRED:**

::

    c  params.match: default value changed to -3 (3 pixels instead of Angstroms)
    c  sparams.match: default value changed to -3 (3 pixels instead of Angs)

**ONEDSPEC, IMRED** spectral packages:

::

    d  fitprofs.sigma: replaced by gfwhm
    d  fitprofs.function: replaced by profile
    d  fitprofs.fitsigmas: replaced by fitgfwhm
    d  rspectext.header: removed since the task now senses the header information

**ONEDSPEC, LONGSLIT, IMRED** spectral packages:

::

    n  identify.units: specify the desired units for the dispersion function
    n  identify.crval: for automatic line identifications
    n  identify.crdelt: for automatic line identifications
    n  identify.aidpars: parameter set for automatic line identifications
    c  identify.match: default value changed to -3 (3 pixels instead of Angstroms)
    c  identify.threshold: default value changed from 10 to 0.
    c  reidentify.match: default value changed to -3 (3 pixels instead of Angs)
    c  reidentify.threshold: default value changed from 10 to 0.
    n  reidentify.crval: for automatic line identifications
    n  reidentify.crdelt: for automatic line identifications
    n  reidentify.aidpars: parameter set for automatic line identifications
    n  reidentify.search: specify a search radius for the automatic line 
          identification algorithm
    n  ecidentify.units: specify the desired units for the dispersion function
    n  fitprofs.profile: define the profile type
    n  fitprofs.gfwhm: replaces sigma for gaussian profile width
    n  fitprofs.lfwhm: width for lorentzian profile
    n  fitprofs.fitgfwhm: replaces fitsigmas
    n  fitprofs.fitlfwhm: select whether to fit lorentzian profile widths
    n  fitprofs.nerrsample: allows control of the error calculation accuracy
    n  splot.nerrsample: allows control of the error calculation accuracy

**CCDRED:**

::

    c  ccdproc.fixfile: this now specifies a bad pixel mask
    c  combine.offsets: now allows specifying "wcs" to compute from WCS

**RV:**

::

    n  rvcorrect.par: Added the KEYWPARS pset declaration

**DAOPHOT:**

::

    c  daopars.critsnratio: critical S/N ratio for group membership - changed
          the name only from critovlap (V2.10.4p2)
    n  daopars.mergerad: critical object merging radius in scale units
       (V2.10.4p2)
