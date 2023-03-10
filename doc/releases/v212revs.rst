IRAF V2.12EXPORT Release Notes
==============================

:Authors: IRAF Group
:Date: January 25, 2002, updated March 25, 2002
:Abstract: These release notes provide a summary of the major changes in V2.12.
 This is a major release of IRAF and will be available for all supported
 platforms. More detailed technical documentation of all system changes
 will be found in the ‘notes.v211’ and ‘notes.v212’ files in the
 ``iraf$doc`` and ``iraf$local`` directories. Detailed revisions notes
 for each application package are in the package directories in a file
 called Revisions, e.g. ``apphot$Revisions``.

Highlights of This Release
--------------------------

Pixel Mask Support Added to FITS Kernel
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The FITS kernel was modified to add support for storing images in
extensions as compressed pixel masks using the binary table extension.
These masks may be accessed like any other image and allow for tasks to
more easily store bad-pixel masks, regions masks, or error arrays in the
same image file as the science data.

New Pixel Mask Tasks
~~~~~~~~~~~~~~~~~~~~

Several new tasks have been added to the system PROTO package for
anipulating pixel masks:

-  MIMSTATISTICS allows image statistics to be computed while rejecting
   pixels specified by an input mask.

-  MSKEXPR task is a general-purpose mask expression evaluator similar
   to IMEXPR for images, but has builtin boolean region functions which
   can be used to set values inside or outside of certain geometric
   shapes.

-  MSKREGIONS creates an output mask based on an input text description.
   Region descriptions can be composed of geometric shapes and logical
   operation on mask regions.

-  OBJMASKS in the NPROTO package is a new task for detecting objects in
   an image and creating an output catalog or pixel mask of found
   objects.

Shared Memory Limitations Eased
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The IMAGES and DAOPHOT packages executables are now linked statically to
remove per-process memory limitations imposed by the IRAF shared library
on Sun and Dec Alpha systems. Previously tasks such as DAOFIND and
IMCOMBINE were limited to 268Mb on Sun systems, these tasks can now use
up to the machine memory limits.

Image I/O Buffer Sizes Increased
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Support for large image I/O was improved by changes to the internal
buffer sizes. These buffers may automatically adjust to optimal values
for the image being accessed, however new environment variables may be
set to further tune the buffers at the user level. Where needed
applications tasks were modified to take advantage of these buffer size
changes to force the imio buffer to be the size of the input image.

Simplified Installation Script
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The install script was rewritten to clarify the output and provide some
basic checking of the IRAF system setup prior to installation, and to do
some of the most common post-install configuration. The script will
print an explanation of any errors it finds and suggest corrective
action, the hope is this will lead the user past some of the most common
installation errors.

In addition, the SYSINFO diagnostic script which does more extensive
checking of the system is also now part of the distribution. This script
can be used to verify the system once the install is complete, or to
generate a report of the system configuration if needed by site support.
An UNINSTALL script to remove iraf command links and files created by
the INSTALL script is also available to remove IRAF from a machine. All
scripts are now installed in the hlib$ directory.

New HELP GUI and Output Options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The HELP task was enhanced to have a new GUI option for XGterm users.
This is essentially the XHELP task which has been available in the
GUIAPPS external package for some time, however the task is fully
backwards compatible and the text-mode output is still the default. As
part of this work, help pages may also now be formatted as either HTML
or Postscript for web presentation or pretty-printing to a hardcopy
device. The LROFF task was similarly modified to provide direct
conversion of lroff text sources.

DISPLAY Task Changes
~~~~~~~~~~~~~~~~~~~~

As part of the recent X11IRAF enhancements, the DISPLAY task and others
such as IMEXAMINE which interact with the display server were modified
to take advantage of the new features in XImtool V1.3. These include
support for 16 frame buffers (increased from 4 in previous versions),
and enhanced WCS readout capabilities. The changes are fully backwards
compatible for use with older XImtool versions or display servers such
as SAOimage, SAOtng, or DS9 which have not yet been updated.

X11IRAF V1.3 is being released simultaneously (but still separately)
with IRAF V2.12. While V2.12 is fully compatible with older versions of
X11IRAF, however users will need to upgrade both systems to take full
advantage of all the new features. Users should consult the X11IRAF
Release Notes for details on what has changed there.

New Packages
~~~~~~~~~~~~

Several new packages are available in this release (see the NOAO package
change notes below for details):

-  A new ASTCAT package for extracting astrometric and photometric
   calibration data from remote or local catalogs was added to NOAO.

-  A new CRUTIL package for doing cosmic ray detection and removal
   package was installed in the IMRED package.

-  A new QUADRED reduction package for QUAD format data was installed in
   the IMRED package. This is a generalized replacement for the
   ARED.QUAD and XCCDRED external packages for processing CTIO and ESO
   FORS1 multi-amplifier data.

-  A new OBSUTIL package was installed in NOAO. This is a collection of
   tasks from various external packages which are useful to plan or
   carry out observations.

New Developer Libraries
~~~~~~~~~~~~~~~~~~~~~~~

Several new libraries are available for SPP developers:

-  PSIO is a new Postscript text generation library installed in
   sys$psio.

-  CATQUERY is a remote astrometric/photometric catalog access lib
   installed in the XTOOLS utility library.

-  SKYWCS is a sky coordinate transformation library installed in the
   XTOOLS utility library.

IRAF System Revisions Summary
-----------------------------

-  The IRAF shared library version number was incremented for SunOS and
   Solaris systems. See below for details on how this change will affect
   external packages and locally-compiled software.

-  The maximum number of nodes in a local iraf network was increased
   from 320 to 512.

-  The max number of open files in FIO, FIO_MAXFD, was increased from
   256 to 4096. This is the “hard limit” on the maximum number of open
   files in an IRAF process.

-  The maximum number of host level open files, MAXOFILES, was increased
   from 64 to 256. This is the maximum number of files that can be
   simultaneously open at the host level. It determines the maximum
   number of files that can be simultaneously open by an IRAF process in
   the usual case.

-  The number of keywords in a group header block for STF images (i.e.
   the MAX_PCOUNT) was increased from 50 to 99 in the STF image kernel.

-  Added support for the bitwise boolean operators: ‘&’ (and), ‘\|’
   (or), ‘^’ (xor), and ‘~’ (not/complement), to vectory expression
   evaluator fmtio$evvexpr.gy. The IMEXPR task was modified to allow
   these new bitwise operations.

-  Added new vector operators to VOPS library: alan, alank (logical AND)
   and alor, alork (logical OR). These take any integer data as input
   (short, int, long) and return a logical (expressed as int) result.

-  The ‘imextn’ environment variable will now accept upper-case
   extensions to specify image types.

-  Host Command Execution: The way command line arguments are parsed was
   modified to make it easier to set the value of a string parameter to
   the null string. Whitespace is still skipped in @par files as before,
   however null strings are valid parameter values and will no longer
   cause a parameter prompt.

-  The MKPKG special file list link support was enhanced to allow
   replacing LFLAGS (the link flags variable) as well as the entire link
   line. This makes is possible to write special-file list entries for
   packages which need e.g. to be compiled nonshared on certain
   platforms without creating a platform specific mkpkg file for the
   package itself.

-  The HSI zawset.c routine which controls a process working set size
   was modified to automatically detect the physical size of system
   memory (with a maximum return value of 2Gb). The hard upper limit on
   memory utilization defined by the unix kernel can be limited either
   by the value return by the IRAF kernel (up to 90% of physical
   memory), or by the value set in the user environment variable
   MAXWORKSET (given in units of Mb).

-  New stdimage display devices were added to support the display of
   Gemini GMOS CCD data. These devices are named ‘imt45’ thru ‘imt49’
   and correspond to the following frame buffer sizes:

   ::

      imt45   2080 x 4644        # imt45|imtgmosccd
      imt46   6400 x 4644        # imt46|imtgmos
      imt47   3200 x 2322        # imt47|imtgmos2
      imt48   1600 x 1161        # imt48|imtgmos4
      imt49    800 x  581        # imt49|imtgmos8

Core IRAF Revisions Summary
---------------------------

New Tasks
~~~~~~~~~

-  imcoords:

   -  ccget: extract objects from a test file catalog
   -  ccstd: transform to and from standard astrometric coordinates

-  proto:

   -  mimstatistics: do image statistics through a mask
   -  rskysub: sky subtract images using running mean or median
   -  mskexpr: general mask expression evaluator
   -  mskregions: create a mask from a list of region specifications

Existing Tasks with New Parameters or New Parameter Defaults
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  immatch:

   -  imcentroid: new parameter maxshift
   -  imalign: new parameter maxshift
   -  geomap: new parameter maxiter, default reject = 3.0 not INDEF

-  imcoords:

   -  ccmap: new parameter maxiter, default reject = 3.0 not INDEF
   -  imcctran: new parameter longpole

-  imutil:

   -  hedit: new parameter addonly
   -  imstatistics: new parameters nclip, lsigma, usigma, cache

Existing Tasks with New Capabilities
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  immatch:

   -  imcentroid: optionally rejects objects whose centers wander too
      much
   -  imalign: optionally rejects objects whose centers wander too much
   -  geomap: iterative rejection capability added

-  imcoords:

   -  ccmap: iterative rejection capability added
   -  imcctran: support for non-zenithal projections added

-  imutil:

   -  hedit: support for add keyword only if new option
   -  imstatistics: support for iterative rejection and memory caching
      added
   -  imexpr: support for bitwise operators or, and, xor, and not added

NOAO Package Revisions Summary
------------------------------

New NOAO Packages
~~~~~~~~~~~~~~~~~

-  astcat: Astronomical catalog and surveys access package

-  crutil: Cosmic ray detection and removal package

-  obsutil: Observing utilities package

New NOAO Package Tasks
~~~~~~~~~~~~~~~~~~~~~~

-  apphot:

   -  pcalc: Do arithmetic operations on a list of apphot databases
   -  pconvert: Convert a text database to a tables database
   -  pdump: Print selected fields from a list of apphot databases
   -  pexamine: Interactively examine and edit an apphot database
   -  prenumber: Renumber stars in an apphot database
   -  pselect: Select records from an apphot database
   -  psort: Sort an apphot database

-  astcat:

   -  aclist: List the supported astrometric catalogs
   -  agetcat: Extract astrometry files from astrometric catalogs
   -  afiltcat: Filter astrometry files derived from astrometric
      catalogs
   -  adumpcat: Catalog access debugging task
   -  aslist: List the supported image surveys
   -  agetim: Extract FITS images from image surveys
   -  ahedit: Initialize the image wcs and set standard keywords
   -  aimfind: Select images containing catalog objects
   -  adumpim: Image survey access debugging task
   -  aregpars: Default region parameter set
   -  acatpars: Default astrometry file format parameter set
   -  afiltpars: Default astrometry file filtering parameters
   -  aimpars: Default image data parameters
   -  awcspars: Default image wcs parameters

-  crutil:

   -  cosmicrays: Remove cosmic rays using flux ratio algorithm
   -  craverage: Detect CRs against average and avoid objects
   -  crcombine: Combine multiple exposures to eliminate cosmic rays
   -  credit: Interactively edit cosmic rays using an image display
   -  crfix: Fix cosmic rays in images using cosmic ray masks
   -  crgrow: Grow cosmic rays in cosmic ray masks
   -  crmedian: Detect and replace cosmic rays with median filter
   -  crnebula: Detect and replace cosmic rays in nebular data

-  obsutil:

   -  psfmeasure: Measure PSF sizes from stellar images
   -  specfocus: Determine spectral focus and alignment variations
   -  starfocus: Determine direct focus variations from stellar images
   -  ccdtime: CCD photometry exposure time calculator
   -  pairmass: Plot airmass vs time for a given coordinate
   -  sptime: Spectroscopic exposure time calculator
   -  specpars: Spectrograph instrument parameters for sptime
   -  bitcount: Accumulate the bit statistics for a list of images
   -  findgain: Estimate the gain and readnoise of a CCD
   -  shutcor: Shutter correction from images of varying exposure times

-  nproto:

   -  objmasks: detect and catalog objects in image

Existing Packages and Tasks with New Parameters or New Parameter Defaults
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  apphot: new package parameters wcsin, wcsout, and cache

   -  center: new parameters wcsin, wcsout, cache
   -  daofind: new parameters wcsout, cache
   -  fitpsf: new parameters wcsin, wcsout, cache
   -  fitsky: new parameters wcsin, wcsout, cache
   -  phot: new parameters wcsin, wcsout, cache
   -  polymark: new parameters wcsin, wcsout, cache
   -  polyphot: new parameters wcsin, wcsout, cache
   -  qphot: new parameters wcsin, wcsout, cache
   -  radprof: new parameters wcsin, wcsout, cache
   -  wphot: new parameters wcsin, wcsout, cache
   -  txdump: replaced by pdump, available as a hidden task

-  astutil:

   -  setairmass: new parameters ra, dec, equinox, st, ut, scale

-  daophot: new package parameters wcsin, wcsout, wcspsf, and cache

   -  addstar: new parameters wcsin, wcsout, wcspsf, and cache
   -  allstar: new parameters wcsin, wcsout, and wcspsf
   -  daoedit: new parameters cache
   -  daofind: new parameters wcsout, and cache
   -  group: new parameters wcsin, wcsout, wcspsf, and cache
   -  nstar: new parameters wcsin, wcsout, wcspsf, and cache
   -  peak: new parameters wcsin, wcsout, wcspsf, and cache
   -  phot: new parameters wcsin, wcsout, and cache
   -  psf: new parameters wcsin, wcsout, and cache
   -  substar: new parameters wcsin, wcsout, and cache

.. _existing-tasks-with-new-capabilities-1:

Existing Tasks with New Capabilities
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  apphot:

   -  center: coordinate system support, optional image cacheing
   -  daofind: coordinate system support, optional image cacheing
   -  fitpsf: coordinate system support, optional image cacheing
   -  fitsky: coordinate system support, optional image cacheing
   -  phot: coordinate system support, optional image cacheing
   -  polymark: coordinate system support, optional image cacheing
   -  polyphot: coordinate system support, optional image cacheing
   -  qphot: coordinate system support, optional image cacheing
   -  radprof: coordinate system support, optional image cacheing
   -  wphot: coordinate system support, optional image cacheing

-  astutil:

   -  setairmass: ra, dec, equinox, st, ut, scale are no longer
      hardwired
   -  rvcorrect: more flexibility in setting ut

-  daophot:

   -  addstar: coordinate system support, optional image cacheing
   -  allstar: coordinate system support
   -  daoedit: optional image cacheing
   -  daofind: coordinate system support, optional image cacheing
   -  group: coordinate system support, optional image cacheing
   -  nstar: coordinate system support, optional image cacheing
   -  peak: coordinate system support, optional image cacheing
   -  phot: coordinate system support, optional image cacheing
   -  psf: coordinate system support, optional image cacheing
   -  substar: coordinate system support, optional image cacheing

General Package Changes
-----------------------

NOAO
~~~~

ONEDSPEC
^^^^^^^^

More than 999 apertures are now allowed.

APPHOT
^^^^^^

-  Coordinate Support:

   All the apphot tasks have been modified to accept input coordinates
   in the logical, tv, physical, or world systems, and to write output
   coordinates in the logical, tv, or physical coordinate systems. One
   consequence of this is that the apphot tasks will now work correctly
   on image sections in interactive mode. Another is that users can now
   work directly on image sections while preserving the coordinate
   system of the parent image.

-  Image Cacheing Support:

   All the apphot tasks which accept image pixel input have been
   modified to optional cache the entire input image in memory. Cacheing
   may significantly improve the performance of tasks where many random
   access operations are performed.

-  File and image name directory information removed from output files
   All the apphot tasks have been modified to strip directory infor-
   mation from the image and coordinate file names written to the output
   files, to the terminal, and to the plot headers. The colon commands
   will still read and write full image and coordinate file path names.

-  New PTOOLS Tasks Added

   The ptools package tasks pcalc, pconvert, pdump, prenumber, pselect
   and psort were added to the apphot package. The functionality of the
   old txdump task as been replaced by the pdump. TXDUMP is still avail-
   able as a hidden task.

ASTCAT
^^^^^^

The astcat package is a set of tasks for extracting astrometric and
photometric calibration data from remote or local catalogs, filtering
the data, extracting FITS images from remote or local surveys, and
adding standard keywords to the extracted images. There is also a task
for selecting images which contain catalog objects and locating the
catalog objects in the image.

IMRED.CRUTIL
^^^^^^^^^^^^

Cosmic ray detection and removal package. This package includes new
tasks and links to tasks from other package. It replaces the CRUTIL
external package.

IMRED.QUADRED
^^^^^^^^^^^^^

Reduction package for QUAD format data. This replaces the ARED.QUAD and
XCCDRED external packages for processing CTIO and ESO FORS1 multi-
amplifier data.

DAOPHOT
^^^^^^^

-  Coordinate Support

   All the daophot tasks have been modified to accept input coordinates
   in the logical, tv, physical, or world systems, and to write the
   output coordinates in the logical, tv, or physical coordinate
   systems. One consequence of this is that the daophot tasks will now
   work correctly on image sections in interactive mode. Another is that
   users can now work directly on image sections while preserving the
   coordinate system of the parent image.

-  Image Cacheing Support

   All the daophot tasks which accept image pixel input have been
   modified to optionally cache the entire input image in memory.
   Cacheing signif- icantly improves the performance of the tasks when
   many random access operations are performed. The cacheing already
   performed by the ALLSTAR task is unchanged.

-  File and image name directory information removed from output files

   All the daophot tasks have been modified to strip directory
   information from the image and coordinate file names written to the
   output files, to the terminal, and to the plot headers. The colon
   commands will still read and write full image and coordinate file
   path names.

OBSUTIL
^^^^^^^

New observing utilities package. This collects tasks from the NMISC,
SPECTIME, PROTO, and NLOCAL external package which are useful to plan or
carry out observations. The new tasks are:

::

   PSFMEASURE    STARFOCUS   SPECFOCUS    CCDTIME
   PAIRMASS      SPTIME      BITCOUNT     FINDGAIN
   SHUTCOR

OBSOLETE
^^^^^^^^

-  Added tasks OIMCOMBINE and OIMSTATISTICS which are the previous
   versions from V2.113b system

-  Deleted the ODISPLAY task

General Task Changes
--------------------

.. _noao-1:

NOAO
~~~~

ONEDSPEC.SPLOT
^^^^^^^^^^^^^^

Rather than refusing to evaluate errors when there is negative data,
negative data is treated as zero.

ASTUTIL.SETAIRMASS
^^^^^^^^^^^^^^^^^^

Modified to have greater flexibility in selecting the keyword defining
the universal time. New parameters define the keywords for RA, dec,
equinox, siderial time, universal time, and astrospheric scale height.

ASTUTIL.RVCORRECT
^^^^^^^^^^^^^^^^^

Modified to have greater flexibility in selecting the keyword defining
the universal time.

IMRED.ECHELLE.ECIDENTIFY
^^^^^^^^^^^^^^^^^^^^^^^^

Help page describes how to externally evaluate the dispersion fcns.

IMRED.CCREDRED.COSMISRAYS
^^^^^^^^^^^^^^^^^^^^^^^^^

Task was removed (see CRUTIL)

NPROTO.FINDGAIN
^^^^^^^^^^^^^^^

Task was removed (see OBSUTIL)

NPROTO.OBJMASKS
^^^^^^^^^^^^^^^

This is a new task for detecting objects in an image and creating an
output catalog or pixel mask of found objects.

TWODSPEC.LONGSLIT.FITCOORDS
^^^^^^^^^^^^^^^^^^^^^^^^^^^

-  Help page describes the contents of the database and how to
   externally evaluate the fits.

-  The RMS is shown in the graph title and in the :show output.

TWODSPEC.APEXTRACT.APEDIT
^^^^^^^^^^^^^^^^^^^^^^^^^

When there is just one aperture the background regions are shown on the
graph without needing to enter the ‘b’ background mode.

IMAGES
~~~~~~

TV.DISPLAY
^^^^^^^^^^

-  The mask overlay feature when the displayed image is a reduction of
   mask (e.g. a block average) now uses the maximum of all mask pixels
   within the display pixel.

-  The task will now allow up to 16 frame buffers to be used for the
   display if allowed by the server. (Currently requires XIMtool V1.3).

TV.IMEXAMINE
^^^^^^^^^^^^

-  A new key ‘t’ allows output of a region centered on the cursor as an
   image for further analysis by other programs.

-  The task will now allow up to 16 frame buffers to be used for the
   display if allowed by the server. (Currently requires XIMtool V1.3).

-  Cursor readback will now properly detect the correct image when more
   than one image is displayed per frame, e.g. in a mosaic. (Currently
   requires XIMtool V1.3).

IMMATCH.IMCOMBINE
^^^^^^^^^^^^^^^^^

-  New parameters “headers”, “bpmasks”, “rejmasks”, “nrejmasks”, and
   “expmasks” provide additional types of output. The old parameters
   “rejmask” and “plfile” were removed. The new “nrejmasks” parameter
   corresponds to the old “plfile” and the new “rejmasks” parameter
   corresponds to the old “rejmask”.

-  There is a new “combine” type “sum” for summing instead of averaging
   the final set of offset, scaled, and weighted pixels.

-  There is a new parameter “outlimits” to allow output of a subregion
   of the full output. This is useful for raster surveys with large
   numbers of images.

-  Additional keywords may appear in the output headers.

-  Scaling is now done relative to the first image rather than an
   average over the images. This is done so that flux related keywords
   such as exposure time and airmass remain representative.

-  A median calculation was made faster.

-  The previous version is available in the OBSOLETE package.

IMMATCH.IMCENTROID
^^^^^^^^^^^^^^^^^^

IMMATCH.IMALIGN
^^^^^^^^^^^^^^^

A new parameter maxshift has been added to the imcentroid and imalign
tasks. Maxshift defines the maximum permitted difference between the
predicted and computed shifts. It is used to reject objects whose
positions have wandered too far from the predicted positions.

IMMATCH.GEOMAP
^^^^^^^^^^^^^^

IMCOORDS.CCMAP
^^^^^^^^^^^^^^

An iterative rejection capability has been added to the geomap and ccmap
tasks. The new parameter maxiter in combination with the existing
parameter reject define the rejection parameter. The default value of
the reject parameter has been changed from INDEF to 3.0.

The colon command “:order ” has been added to the geomap and ccmap
tasks. The new command enables the user to change all the order
parameters simultaneously when experimenting with different fitting
functions.

IMCOORDS.STARFIND
^^^^^^^^^^^^^^^^^

The starfind task background estimation algorithm has been modified so
that it no longer depends on the value and density of the central pixel.

IMCOORDS.IMCCTRAN
^^^^^^^^^^^^^^^^^

Support for non-zenithal projections has been added to the imcctran
task. The previous technique of rotating the cd matrix does not work
properly for these functions. The new parameter longpole was added to
imcctran. Longpole enables the user to select either the cd matrix or
longpole / latpole method for transforming zenithal projections.

IMCOORDS.CCGET
^^^^^^^^^^^^^^

The new task ccget was added to the imcoords package. Ccget extracts
objects in a user specified region from a simple text file catalog.

IMCOORDS.CCSTD
^^^^^^^^^^^^^^

The task ccstd was added to the imcoords package. Ccstd transforms pixel
and celestial coordinates to standard coordinates and vice versa.

IMUTIL.HEDIT
^^^^^^^^^^^^

The new parameter addonly was added to hedit task. The addonly switch is
used to add a parameter to the image header only if it does not already
exist. The addonly switch has a precedence intermediate between the add
and delete switches.

IMUTIL.IMSTATISTICS
^^^^^^^^^^^^^^^^^^^

An interactive rejection capability has been added to the imstatistics
task. The new parameters nclip, lsigma, and usigma define the rejection
parameters. A memory cacheing option was also added to imstatistics in
order to optionally speed up performance if iterative rejection is en-
abled or the midpt/mode is computed.

IMUTIL.IMEXPR
^^^^^^^^^^^^^

Support for the bitwise operators or (|), and (&), exclusive or (^), and
not (~) has been added to the imexpr task. The logical operators or (||)
and and (&&) have been made truly logical i.e. they return 0’s or 1’s,
rather than results of a bitwise or and and.

PROTO
~~~~~

MIMSTATISTICS
^^^^^^^^^^^^^

The new task mimstatistics has been added to the proto package.
Mimstatistics does image statistics through a mask.

RSKYSUB
^^^^^^^

The new task rskysub was added to the proto package. Rskysub does a
running mean or median sky subtraction on an ordered list of images
using optional background scaling and object masking.

MSKEXPR
^^^^^^^

The new task mskexpr has been added to the proto package. Mskexpr
creates a new mask from a user supplied expression, an optional
reference image, and an optional reference mask.

MSKREGIONS
^^^^^^^^^^

The new task mskregions has been added to the proto package. Mskregions
creates a new mask or modifies an existing mask using a list of region
definitions or region expressions.

XTOOLS
~~~~~~

SKYWCS
^^^^^^

A new library skywcs has been added to the xtools package. The skywcs
library is a set of routines for managing image and catalog celestial
coordinate systems and for transforming from one celestial coordinate
system to another. Skywcs is layered on the Starlink Positional
Astronomy library slalib which is installed in the iraf math package.

CATQUERY
^^^^^^^^

A new library catquery was added to the xtools package. The catquery
library is a set of routines for doing local and remote catalog and
image survey access.

SYSTEM
~~~~~~

HELP
^^^^

Task was modified to call the XHELP code to run the GUI version of the
task if requested. Task output is the same if the device remains the
default ‘terminal’ value, however resetting the ‘device’ parameter to
one of ‘gui’, ‘html’, or ‘ps’ will either spawn the GUI task under
xgterm or print the converted help page to the stdout.

LROFF
^^^^^

The task was enhanced with a new ‘format’ parameter that allows the text
to be formatted as one of: plain-text, HTML, or Postscript.

Parameter File Changes
----------------------

In the tables below each parameter change is identified with one of the
following codes followed by task name and the description of the change.

-  n = new parameter
-  c = changed/modified parameter
-  d = deleted parameter

CL
~~

::

     n cl              Added the new CL parameter "release".  This
                   is a string valued parameter with values such
                   as "2.11.3a", "2.12", "3.0" etc.  This differs
                   from "version" which is a descriptive string
                   such as "NOAO/IRAF V2.11.3 EXPORT".  There can
                   be multiple releases of one version of the
                   software, and "release" specifies exactly what
                   build the software is.  The release strings are
                   composed in such a way that they can be used
                   in expressions, e.g. (release >= 2.11.3) would
                   be true for IRAF V2.11.3 and all subsequent
                   releases.

DATAIO
~~~~~~

::

     c dataio.export       Made the 'format' parameter automatic mode
     c dataio.import       Made the 'format' parameter automatic mode

.. _images-1:

IMAGES
~~~~~~

::

     n imcoords.imcctran       Added a new parameter longpole to the imcctran
                   task. If longpole=yes then coordinate transfor-
                   mations with zenithal projections will be rot-
                   ated using longpole rather than the CD matrix.

     c immatch.wregister       Fixed boundary option typo, "refect" to "reflect".
     c immatch.sregister       Fixed boundary option typo, "refect" to "reflect".

     n immatch.imcentroid      Added a new parameter maxshift to the imcentroid
       immatch.imalign     and imalign tasks.  Maxshift is the maximum perm-
                   itted difference between the computed and predicted
                   shifts. Maxshift can be used to reject objects whose
                   centers have wandered too far from the expected
                   center. By default maxshift is undefined.

     n immatch.geomap      Added a new parameter maxiter to the geomap and
       immatch.ccmap       ccmap tasks. Maxiter defines the maximum number of
                   rejection iterations and has a default value of 0
                   for no rejection.  

     c immatch.geomap      Changed the default value of the ccmap and geomap
     c immatch.ccmap       parameter reject from INDEF to 3.0.

     c immatch.imcombine       Numerous changes, see details above

     c imgeom.imlintran        Changed the nrows argument names to nlines 


     n imutil.hedit        Added a new addonly parameter to the hedit task. If
                   addonly is set a new field will only be added to
                   the image header if it does not already exist.

     n tv.imexamine        Added new parameters 'output', 'ncoutput', and
                   'nloutput' used by the new 't' keystroke when
                   outputting an image section centered on the cursor.

.. _system-1:

SYSTEM
~~~~~~

::

     n help            New parameters required for GUI options, output
                   formats for HTML/PS, printer, etc.
     n lroff           Added new 'format' parameter for HTML/PS output 

UTILITIES
~~~~~~~~~

::

     c utilities.surfit        Added support for the half cross-terms option to
                   the surfit task.  This involved changing the type
                   of the xterms parameter from boolean (yes/no) to
                   string (none,half,full).

.. _noao-2:

NOAO
~~~~

ASTUTIL
^^^^^^^

::

       n astutil.setairmass    new parameters ra, dec, equinox, st, ut, scale

DIGIPHOT
^^^^^^^^

::

       n apphot            new package parameters wcsin, wcsout, and cache
       n apphot.center         new parameters wcsin, wcsout, cache
       n apphot.daofind        new parameters wcsout, cache
       n apphot.fitpsf         new parameters wcsin, wcsout, cache
       n apphot.fitsky         new parameters wcsin, wcsout, cache
       n apphot.phot       new parameters wcsin, wcsout, cache
       n apphot.polymark       new parameters wcsin, wcsout, cache
       n apphot.polyphot       new parameters wcsin, wcsout, cache
       n apphot.qphot      new parameters wcsin, wcsout, cache
       n apphot.radprof        new parameters wcsin, wcsout, cache
       n apphot.wphot      new parameters wcsin, wcsout, cache

       n daophot           new package params wcsin, wcsout, wcspsf, and cache
       n daophot.addstar       new parameters wcsin, wcsout, wcspsf, and cache
       n daophot.allstar       new parameters wcsin, wcsout, and wcspsf
       n daophot.daoedit       new parameters cache
       n daophot.daofind       new parameters wcsout, and cache
       n daophot.group         new parameters wcsin, wcsout, wcspsf, and cache
       n daophot.nstar         new parameters wcsin, wcsout, wcspsf, and cache
       n daophot.peak      new parameters wcsin, wcsout, wcspsf, and cache
       n daophot.phot      new parameters wcsin, wcsout, and cache
       n daophot.psf       new parameters wcsin, wcsout, and cache
       n daophot.substar       new parameters wcsin, wcsout, and cache

.. _onedspec-1:

ONEDSPEC
^^^^^^^^

::

       n standard          new parameter mag, magband, and teff.  These 
       n splot         params can be use to specify calibration files
       n lcalib                as blackbody curves scale to a specified magnitude

TWODSPEC
^^^^^^^^

::

       c apextract.apall1      Reduced the 'polysep' parameter.
       c apextract.apdebug     Reduced the 'polysep' parameter.
       c apextract.apfit1      Reduced the 'polysep' parameter.
       c apextract.apnoise1    Reduced the 'polysep' parameter.
       c apextract.apnorm1     Reduced the 'polysep' parameter.
       c apextract.apparams    Reduced the 'polysep' parameter.

Details of Major System Changes
-------------------------------

FITS kernel changes
~~~~~~~~~~~~~~~~~~~

The FITS kernel was modified to add support for storing images in
extensions as compressed pixel masks. The mask is stored as a binary
table using the “ZIMAGE” (compressed image) convention proposed by
White, Greenfield, Pence, and Tody in 1999:

`Specifications for Storing Compressed Images in FITS Binary
Tables <http://heasarc.gsfc.nasa.gov/docs/software/fitsio/compression/compress_image.html>`__

In the current implementation only the “PLIO_1” compression algorithm is
implemented. Mask extensions may be read or written directly by the
kernel. When writing a new extension it will be appended to the MEF
file. To append an image to a MEF file as a mask, include “type=mask” in
the image kernel section when the output image is opened.

Masks are interfaced to the system as images and may be read and written
like any other image via IMIO. They have a normal image header and can
be manipulated with any program that deals with images. The pixel type
is INT.

It is also possible to access a mask image as a PLIO mask. An IMSTATI
query for IM_PLDES parameter will return the PLIO mask descriptor. While
a mask extension is opened under IMIO it is represented as a PLIO mask
and may be accessed in this form like any other mask.

The mask image is stored in the FITS binary table (BINTABLE) extension
when the image is closed, and is loaded from the extension when the
image is opened. The compression representation used to store the mask
in the binary table is the same as is used within PLIO. The new (V2.12)
encoding is used, allowing very large masks to be stored. Currently
masks up to 3D are supported. Data on each 2D mask plane will be
compressed in both X and Y as with PLIO. The depth of the mask is
preserved.

Although a mask is stored as a binary table the format of the table is
not completely general. In the current implementation there can be only
one column in the table (COMPRESSED_DATA). This is an integer-valued
variable length array column containing, for each line of the
N-dimensional image, the PLIO compressed version of that image line. The
actual compressed data is stored in the heap area of the table. Multiple
image lines may point to the same compressed line list, e.g., to store
the empty line or to compression in Y.

Large Image Support
~~~~~~~~~~~~~~~~~~~

The following changes were made to enable IMIO to use larger buffer
sizes to optimize i/o for large images:

The default file buffer size set by IMIO is unchanged: it is still about
512 MB, the value set for V2.11.2. However, a new parameter IM_BUFFRAC
was added. Both IM_BUFSIZE and IM_BUFFRAC are used to help determine the
FIO buffer size set when an image is opened. The logic for this is
implemented in imsetbuf.x.

Backwards compatibility. If you do nothing about IMIO/FIO buffers in
your program, the system may transparently use a larger buffer for
larger images. If you set BUFSIZE in your program, the system will by
default use the value you give, or possibly a larger value, if the image
you are accessing is very large. If you set BUFSIZE and you want to
guarantee that the value you set is used (even for very large images)
then you should also set BUFFRAC=0 to ensure that only BUFSIZE is used.

How it works. BUFFRAC specifies the default FIO buffer size used to
access an image, expressed as a percentage of the size of the image. For
example, the builtin default value of BUFFRAC=10 will try to make a FIO
buffer 10% of the size of the image. The actual value used will be given
by BUFFRAC, but will be at least BUFSIZE, and no more than a builtin
default maximum value, currently 32 MB. Given the builtin defaults, the
buffer size will range from 0.5 to 32 MB depending upon the size of the
image being accessed. As noted above, BUFSIZE and BUFFRAC can be set to
force the buffer size to a specific value if desired.

Environment variables for both parameters are provided. The names are
“IMIO_BUFSIZE” (specified as bytes) and “IMIO_BUFFRAC” (specified as a
decimal fraction). If defined, these override (at image open time) the
builtin default values for both parameters. An IMSET call by the
application will override all such defaults.

The FIO buffer allocated will not be larger than the size of the image.
The FIO buffer will also not exceed the maximum size set by the file
driver being accessed. For example, for PLIO images the file buffer will
not exceed about 2KB, even for a very large mask. This is because the
“pixel file” for a PLIO image is dev$null, the driver for which
specifies a maximum i/o buffer size of 2K (the real file to load or save
the mask will use a different descriptor).

The intent here is to provide an adaptive mechanism for setting the FIO
buffer size used to access an image, which automatically adapts to the
size of the image being accessed. If you access a lot of small images
you will get smaller buffers - everything will be as before. If you
access very large images, you may get large buffers up to the builtin
maximum value of (currently) 32 MB.

Using large buffers could cause a machine to run out of memory. However,
it is likely that if someone is working on 300 MB images that they are
using a machine which has a memory at least that large - probably
larger. If there are problems, the environment variable overrides can be
used to tune IMIO.

The reason for large file buffers is to limit the number of disk data
transfers, and hence the number of disk seeks. Using buffers larger than
a certain amount (32 MB is generous) is probably counterproductive. If
the i/o system provides 20 MB/sec i/o transfers, 32 MB will take 1.6
seconds. This should be more than a large enough granularity to provide
efficient i/o, hence is a reasonable limit (at this point paging effects
are likely to dominate).

Virtual Memory Cache
~~~~~~~~~~~~~~~~~~~~

The VMcache client interface and daemon provide a method by which
data-intensive IRAF tasks (or non-IRAF tasks for that matter) can manage
how files/images are maintained in virtual memory to avoid excessive
system paging. In essence it’s a way to “lock” a specific image in
memory to improve performance. As of this release no tasks in the system
have been modified to make use of the VMcache daemon, however installing
it in the system at this point provides a framework for future
applications and systems development.

The following notes summarize the changes made for this feature and
describe it’s function in more detail. A more complete description of
the interface, environment variables which control it, etc can be found
in the main systems revisions file iraf$local/notes.v211.

The source for the developmental version of the VMcache library and the
VMcache daemon (vmcached) have been installed in the unix$boot tree and
the HSI binary file driver was modified to add VMcache client support.
This adds two new capabilities to the driver: 1) built-in support for
direct i/o (on systems that support it), and 2) a client interface to
the VMcache daemon to permit the daemon to optimally manage binary file
i/o if a VMcache daemon is present.

The vmcached code is complete but only enough debug/testing was done to
support development of the VMcache client interface for IRAF (the
vmcached code is debugged but the new version of the vmcache library
code has not been tested). Since the daemon can be utilized outside the
normal IRAF release we do not have to fully develop it for the release.

It should be stressed that VMcache is only useful or warranted for
systems that are very data intensive. The standard host operating system
file access heuristics are best for “normal” processing where either the
system is not really busy, or the datafiles are not excessively large.
On systems with very large physical memories where massive amounts of
data are being processed, VMcache can make a significant difference in
overall system performance.

VMcache is too complex to document here. Without going into the details,
its function is to manage a cache of files in system virtual memory.
Files can be explicitly cached or uncached, or they can be “accessed”,
and VMcache will decide whether or not to cache the file in virtual
memory. This is what the VMcache client interface does: every time it
accesses (opens or extends) a file larger then the VM threshold it sends
an “access” directive to the VMcache daemon. The daemon sends back a
response of 0 (file not cached; use direct i/o to access the file), or 1
(file cached in VM; use normal VM-buffered i/o to access the file). Even
if a file is not cached the daemon keeps track of all accesses. Files
which are frequently accessed will have a higher priority and are more
likely to be cached in memory.

The VMcache daemon is a separate system-level program outside of IRAF.
This is necessary to provide a central system-wide cache controller. It
also provides flexibility, allowing multiple versions of the daemon to
exist, e.g., to allow experimentation with different types of caching
algorithms. It also allows easy customization of the daemon
independently of the IRAF applications using the VMcache client
interface.

.. _new-developer-libraries-1:

New Developer Libraries
~~~~~~~~~~~~~~~~~~~~~~~

Several new libraries are now available for developers:

-  **PSIO** New Postscript text generation library installed in the
   sys$psio.

   The PSIO interface is used to format a block of text as Postscript
   output on a page of a given size (Letter, Legal, A4 or B5). See the
   psio$README file for details.

-  **CATQUERY** Remote astrometric/photometric catalog access lib
   installed in the XTOOLS utility library.

   The catquery package provides a set of routines for local and remote
   catalog and image survey server access. The sup- ported catalogs and
   image surveys are described in records stored in a catalog and image
   survey configuration file respectively. The catalog and image survey
   records specify the network address, the query format, and the output
   format for each supported catalog or image display server. See “help
   catalogs” and “help surveys” for details.

-  **SKYWCS** Sky coordinate transformation library installed in the
   XTOOLS utility library.

   The skywcs package contains a simple set of routines for managing sky
   coordinate information and for transforming from one sky coordinate
   system to another. The sky coordinate system is defined either by a
   system name, e.g. “J2000”, “galactic”, etc., or by an image system
   name, e.g. “dev\ :math:`ypix" or "dev`\ ypix world”.

System Changes Which May Affect You
-----------------------------------

SHARED LIBRARY VERSION INCREMENTED (Sun/IRAF only)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The IRAF shared library for SunOS and Solaris platforms has been
incremented with this release due to the nature of various system
changes. Existing IRAF binaries (e.g. locally written software or
external packages) will continue to run using the old shared image,
however they will need to be recompiled against V2.12 in order to pick
up the numerous system bug fixes and features in this release. In
particular, pixel masks produced by V2.12 IRAF tasks may be incompatible
with external packages which have not been recompiled.

EXTERNAL PACKAGE RECOMPILATION
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The V2.12 release contains changes to the FIO and PLIO/PMIO interface
header files used by numerous applications. Relinking of an external
package may fail to pick up these changes and not recompile a source
file which uses one of these header files if the mkpkg file doesn’t
correctly list all of the dependencies (nearly all packages have one or
more mkpkg files which have this problem). In the worst cases this could
lead to a runtime error due to the incompatibilities.

For this reason we recommend that all packages and local tasks be
recompiled (completely from source\* (rather than simply relinked
against the new version) to assure that all changes and new features
will be included. Recompilation also guarantees that packages can take
advantage of some of the larger buffer sizes and optimizations in this
release. Site support can supply a list of missing mkpkg dependencies
for most external packages being developed outside NOAO that wish to fix
these files for a future release.

.. _parameter-file-changes-1:

PARAMETER FILE CHANGES
~~~~~~~~~~~~~~~~~~~~~~

As with all major releases, we recommend that you do a MKIRAF and delete
all your old parameter files after the IRAF upgrade. You may choose not
to do this if you are in the midst of a project and have setups that may
be difficult to reproduce.

The automatic parameter file update/merge mechanism, which is used if
you do not initialize your parameters with MKIRAF, is based on file date
comparisons. If you run IRAF V2.11 after V2.12 has been installed, the
file dates on your uparm parameter files will be more recent than the
V2.12 installation date. If you then try to run V2.12, the automatic
parameter file merge/update will fail due to the file dates. The system
only updates personal parameter files which are older than the update
date of the system. A MKIRAF avoids the problem if you delete your
parameter files, causing them to be updated from the system default
versions.

INSTALLATION SCRIPT CHANGES
~~~~~~~~~~~~~~~~~~~~~~~~~~~

As the first step of an ongoing effort to simplify the installation and
system configuration, the IRAF install script was rewritten to do some
error-checking of the iraf setup, present a simplified and easier to
read output, and do some common post-install configuration of the
system. Additionally, the SYSINFO diagnostic script for finding system
errors and reporting on the configuration, and a new UNINSTALL script
for removing IRAF files/links from the system have also been installed.
The old install script is still available as a fallback in case problems
with the new script are found.

HELP SYSTEM CHANGES
~~~~~~~~~~~~~~~~~~~

The HELP task was modified with several new parameters controlling the
display and formatting of the help pages. Help may now be presented as
formatted text (as before), HTML, or fully formatted Postscript.
Additionally, users running under an XGterm window can use the task in a
new GUI mode. The help GUI allows users to browse the help system and
easily search for tasks/topics using a familiar web-like interface. The
GUI mode is not the default, but can be enabled easily using the
‘device’ parameter.

IMAGE DISPLAY CHANGES
~~~~~~~~~~~~~~~~~~~~~

Tasks which display images or interact with the image display were
modified to take advantage of new features added to XImtool V1.3
(e.g. the multiple WCS and pixel-value readouts and 16 display frame
buffers). These changes were done in a backwards compatible way so
interaction with display servers such as SAOimage, SAOtng, DS9, or older
XImtool versions should be unaffected. If problems are dis- covered a CL
environment variable ‘disable_wcs_maps’ can be defined to force all of
the old behaviors. These changes do not add any new functionality to the
tasks themselves, only the underlying display protocols.

PLIO Changes
~~~~~~~~~~~~

The LEN and BLEN fields of the encoded line list (LL) descriptor would
limit the length of a pixel area (and hence the size of a pixel mask) to
the max size of a signed short, 32768. This was due to the use of a
simple array of type short to encode the line list (which simplifies
handling considerably). Nonetheless the limit to 32K was unacceptable.
The fix adopted was to increase the LL header from 3 to 7 words. Two 16
bit words are now used to encode each of LEN and BLEN. A “version” word
was added to allow the old, new, and future encodings to be
distinguished. A “hdrlen” word was added to parameterize the length of
the LL header, rather than fix it at compile time as in the initial
version. With this change, the maximum length of an image line under
PLIO is increased from 32768 to 1073741824 (32768*32768). All the higher
level PLIO code is integer, so should already support larger masks.

This was done in such a way that old line lists can still be read,
although PLIO will always write out new format line lists (pixel mask
files and images, QPOE, and MWCS all store encoded line lists in
external storage, so backwards compatibility is important; also existing
complied programs will continue to generate the old format). The cost is
8 bytes per encoded line list. For most masks this should only increase
the size of the mask by a few percent at most.

NEW ENVIRONMENT VARIABLES
~~~~~~~~~~~~~~~~~~~~~~~~~

The following new environment variables may be defined to tune the size
of the system file i/o buffers used by the image i/o system. The system
will automatically adjust to use larger buffers when accessing larger
images, these variables may be set to further optimize the buffers

-  **IMIO_BUFSIZE**: Size of the FIO buffer size in bytes.

-  **IMIO_BUFFRAC**: FIO buffer size expressed as a percentage of the
   image size. Actual value will be at least BUFSIZE and no more than
   BUFMAX.

-  **IMIO_BUFMAX**: Max size of FIO buffer which will override the 32Mb
   default.

Other miscellaneous environment variables:

-  **disable_wcs_maps**: If defined or set to ‘yes’, this variable will
   force any tasks which interact with the image display to use the old
   protocols.

-  **pspage**: Variable which is used by the PSIO interface to set the
   default page size. Acceptable values include “letter” (the default)
   for US Letter, “legal” for US Legal, and “a4” and “b5” for the most
   common European sizes. Pspage can be used by the new HELP and LROFF
   tasks to automatically set the desired Postscript page size.
