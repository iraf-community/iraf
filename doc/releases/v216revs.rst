IRAF V2.16 Release Notes
========================

:Authors: IRAF Group
:Date: March 22, 2012
:Abstract: These notes provide a summary of the major changes in the V2.16 release.
 V2.16 includes a number of bugfixes and new tasks, but is primarily a
 platform-support release to add new Virtual Observatory (VO)
 capabilities to the core system. Full integration of VO capabilities
 requires new functionality to be available in all tasks and not simply
 in new VO applications. In particular, tasks are now all able to deal
 with remote data as easily as local disk files, and VO-specific formats
 are supported as transparently as other long-used table formats.
 Specifics about the new core capabilities are described in the
 highlights section below.

These new core capabilities have been fairly extensively tested and are
considered stable, however we expect there will no doubt be instances
that cause trouble that should be reported. Because bugs in new core
capabilities will affect all tasks, a simplified update procedure is
also featured in this release to assist in applying patches to the
entire system. This follows the earlier model for installing external
packages where the following ‘make’ commands in the $iraf directory will
suffice to bring a system up to date following a bug-fix release:

::

   make latest         # Update entire system (core + extpkgs)
   make latest_src     # Update only source code
   make latest_core    # Update only the core iraf system

   make check_latest   # Check is system is latest released version

We expect that in the months immediately following the initial release
there will need to be one or more updates to the entire system.

More detailed technical documentation of all system changes will be
found in the ``notes.v215`` file in the ``iraf$local directory``,
detailed revisions notes for each application package are in the package
directories in a file called ``Revisions``, e.g. ``onedspec$Revisions``.

Highlights of This Release:
---------------------------

64-bit Bug Fixes
~~~~~~~~~~~~~~~~

A number of 64-bit bugs remaining in the v2.15 release have been fixed,
both in specific applications as well as system-level interfaces such as
IMIO. As of this writing, there are no known 64-bit issues in the system
and we now consider these architectures to be stable.

All users of 64-bit systems are STRONGLY ENCOURAGED TO UPDATE to v2.16
in order to fix these bugs in their installation of IRAF. Specific bugs
fixed by this release are given below, detailed changes

“Free” Licenses
~~~~~~~~~~~~~~~

Previous releases of IRAF were seen as being “non-free” due to legacy
license retrictions in the system. In particular, the NCAR graphics code
restricted re-distribution of IRAF by parties other than NOAO, and
comments in the XYACC source directory were taken to mean that an
original BSD source license was required.

We are happy to report that both restrictions have been removed from the
IRAF system: The XYACC code was rewritten prior to the v2.15.1a release
and is covered under the OpenSolaris CDDL license. Agreements between
NOAO and UCAR were reached to allow the NCAR code in IRAF to
(retroactively) be covered by the current UCAR license allowing
redistribution.

A new COPYRIGHTS file and the text of the licenses now in use can be
found in the $iraf/local directory as well as the distrubution directory
on the iraf.noao.edu server.

A new VO-CL
~~~~~~~~~~~

IRAF v2.16 features a modified version of the CL, called VOCL, that
provides new builtin functions to support VO task development. This
version of the CL is fully backward compatible with the earlier ECL
(which remains available) and since it is required for the proper
operation of the VO package tasks, will be the version started by
default.

In addition to VO data and service integration, the VOCL is able to
exchange messages with other desktop applications using the SAMP
protocol. Proprietary message types are implemented that allow arbitrary
execution of IRAF commands, and which set/get both parameter and
environment values. Custom messages may also be defined by users to
permit any two IRAF sessions on the same machine to exchange messages.
This level of interoperability allows other applications designed for
e.g. table or spectrum visualization to be used seemlessly within the
IRAF environment.

VO Package
~~~~~~~~~~

IRAF v2.16 is now distributed with a new VO external package as part of
the core release (similar to how the NOAO external package is considered
part of the ‘core’). This package provides utility tasks and
applications specifically for accessing an analyzing VO data and is
meant to provide a toolbox for scientists to incorporate VO data into
their reductions.

The VO package is expected to evolve quickly as new tasks and features
are added, and due to limited testing, is considered to be only a
Beta-quality release at this stage. A detailed listing of tasks in the
VO package is given below.

Enhance @-file Capabilities
~~~~~~~~~~~~~~~~~~~~~~~~~~~

IRAF has long had the ability to the ‘@-files’ as a mean of passing in
lists of images/files to tasks. For v2.16, the concept was expanded to
include other things that logically have as lists, e.g. a column in a
table of remote data URLS, the implicit list of images in an MEF file,
etc.

In these enhanced templates, things like MEF files and tables can be
expanded automatically using the @-file operator to let any task that
takes an image list process the extensions automagically. Additionally,
selection of images based on image header keywrods can also be done
dynamically using expressions in the template strings. For example,
consider the following templates:

::

   @file*                          expand all files beginning w/ 'file'
   @file//".fits"                  append ".fits" to contents of 'file'
   @mef.fits                       expand image extensions of an MEF file
   @mef.fits[SCI]                  select SCI extensions from MEF file
   @mef.fits[SCI,2][noinherit]     select v2 SCI extns, add kernel param
   @mef.fits[1-16x2]               select range of extensions from MEF file
   @mef.fits[+1-8]                 create a list of extensions for an MEF
   *.fits[1:100,1:100]             append section to all FITS images
   @@file[1:100,1:100]             append section to expanded MEFs in file
   *.fits[filter?='V']             select images where FILTER contains 'V'
   @*.fits[gain==3.0]              select extns where GAIN keyword is 3.0
   *.fits[filter?='V';gain==2.5]   select using multiple OR's expressions

These templates could be used e.g. to run IMSTAT on all the extensions
of a FITS file with a single command, as in

::

     Enhanced @-files        Old @-files
     ----------------        -----------
                             cl> imext mef.fits output=file > list.dat
     cl> imstat @mef.fits    cl> if (imext.nimages > 0)
                             >>>     imstat @list.dat 
                             cl> del list.dat

where before it would have been necessary to expand the extensions
explicitly using a second task into a standard @file. Notice how the
other examples above provide even more refined selection of the
extensions, either within a single MEF or across multiple images.

These changes are all fully backward compatible but provide new syntax
to give users a powerful and compact way to dynamically build image
lists for use by all tasks. These new features will be especially useful
for script developers tired of managing temp files of image lists, or
those who just need to quickly examine MEF files or a directory of
images.

System File Cache
~~~~~~~~~~~~~~~~~

The system file cache is used to provide local storage for URL files,
i.e. when a URL is encountered it is downloaded and put into the
cache.  If that same URL is used again (e.g. from within a script),
the cached file is passed to the task instead of accessing the URL a
second time.  The cache directory used is determined by the ‘cache’
environment variable set in the user’s login.cl file, or by the
``hlib$zzsetenv.def`` file. Upon login, files in the cache older than
'cache_age' days will automatically be removed. This value may be
changed in the login.cl file or the ``hlib$zzsetenv.def`` as needed.

URL Support by all tasks
~~~~~~~~~~~~~~~~~~~~~~~~

Because both queries for VO data, and access to remote files/images is
typically done using HTTP, the use of a URL in place of a local
file/image name is now supported by all tasks. This feature
transparently allows use of remote data from archives as input to all
tasks (use of URLs for output from tasks is not supported at this time).
For example, from the CL commandline

::

   cl> imstat http://iraf.noao.edu/votest/dpix.fits
   cl> tinfo http://iraf.noao.edu/votest/usno-b.xml
   cl> type http://iraf.noao.edu/index.html

or programmatically from a script such as

::

   s1 = "http://archive.stsci.edu/wuppe/search.php?RA=0.0&DEC=0.0&SR=30."
   type (s1)

The file returned by the URL is automatically placed in the system file
cache meaning the URL can be used repeatedly without actually
downloading the file each time it is used. This can be especially
important in scripts where a URL may be constructed or retrieved from a
result table and be used multiple times within the script. For example,
in the use of ‘s1’ above, the URL is actually a catalog query string
that returns a VOTable document; When the TYPE task is called, we are
able to execute the query and print the results in a single step as
opposed to having a separate download step. Further, because the URL is
now cached we can continue to use the ‘s1’ script variable to refer to
the result table in other tasks without having to re-execute the query
each time.

VOTable Support by all tasks
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Part of the core VO support in v2.16 is the ability for all tasks that
expect tablular input (e.g anything using the TABLES interfaces) to also
now accept the VOTable XML documents used by the VO. When a VOTable is
encountered, it is automatically converted internally to a FITS binary
table and placed in the system file cache, this cached file is actually
what is being manipulated by the task.

In some cases, tasks in the VO external package that are specifically
VOTable-aware will be able to use (and expect) the raw XML document
directly. In many cases it will be possible to use VOTables as input to
VO-aware tasks without having to manage the XML yourself, in other cases
common tasks such as extracting a row/column from the table will behave
as they always have.

SAMP Support by all tasks
~~~~~~~~~~~~~~~~~~~~~~~~~

SAMP is the Simple Applications Messaging Protocol, an XML-RPC messaging
system for desktop applications developed within the VO. The new VOCL is
able to send an receive SAMP messages in order to allow IRAF to
interoperate with other VO-enabled applications.

URLGET Task (New!)
~~~~~~~~~~~~~~~~~~

A new URLGET task is available in the SYSTEM package that may be used in
scripts much as one might use the host WGET task to access a URL (see
the help page). In particular, this task is used by the package update
mechanism to provide a download capability on systems where e.g. ‘wget’
is not available.

FCACHE Task (New!)
~~~~~~~~~~~~~~~~~~

A new FCACHE task is available to manipulate and clean the system file
cache. In particular, a command such as

::

   cl> fcache init

can be used to completely re-initialize the file cache when the disk
begins to fill or in some cases to remedy problems encountered with use
of URLs.

Simplified Build From Source
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At the request of users, a toplevel ‘Makefile’ is now available for
building or configuring the system with a single command. This Makefile
is a simple driver for scripts that do all the work using conventional
IRAF commands. Allowed ‘make’ command targets include:

::

   all             alias for 'update'
   sysgen          do a complete sysgen
   update          update system since last sysgen
   updatex         update with debugging flags enabled
   src             clean system of current binaries
   clean           clean system of current binaries
   pristine        clean system of all binaries
   tables          compile the TABLES package
   noao            compile the NOAO package
   summary         print core/noao/tables spool file summaries
   showarch        show currently configure arch
   <arch>          reconfigure for named architecture

   latest          Update entire system to latest patch release
   latest_src      Update only source code
   latest_core     Update only the core iraf system

   check_latest    Check is system is latest released version

Simplified Download/Install Process
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IRAF systems now come in single-file downloads for the most common
configurations:

::

   iraf-src.tar.gz           Source code only
   iraf-all.tar.gz           Source + all supported platform binaries
   iraf-linux.tar.gz         Source + linux/linux64 platform binaries
   iraf-macosx.tar.gz        Source + macosx/macintel platform binaries

   iraf.lnux.x86.tar.gz      Source + 32-bit Linux binaries
   iraf.lnux.x86_64.tar.gz   Source + 64-bit Linux binaries
   iraf.macx.uni.tar.gz      Source + 32-bit OSX binaries
   iraf.macx.x86_64.tar.gz   Source + 64-bit OSX binaries

With these distributions files, installation is a simple matter of
unpacking the tarball in the desired IRAF root directory (typically some
place like /iraf/iraf) and running the install script. The traditional
multi-part distribution files also remain available.

Simplified External Package Installation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

External packages may be installed and defined dynamically and no longer
need to be manually configured. The $iraf/exern directory contains a
‘configure’ script to create the files needed for a single-command
installation of an external package (and it’s dependencies). For
example,

::

   % cd $iraf/extern       # go to extern directory
   % ./configure           # configure system (only once)
   % make mscred           # install the MSCRED package

Packages defined in this way will be available the next time you login
to the system. See the $iraf/extern/README file for details.

Improved Documentation
~~~~~~~~~~~~~~~~~~~~~~

Thanks to Jason Quinn for continued suggestions on improved wording of
help pages and careful proofreading of the docs.

Platform Support:
~~~~~~~~~~~~~~~~~

IRAF V2.16 supports the following platforms:

-  PC-IRAF

   -  32-bit Linux (LNUX.X86)
   -  32-bit Mac OS X 10.4 and higher (ppc and intel) (MACX.UNI)
   -  64-bit Linux (LNUX.X86_64)
   -  64-bit Mac OS X 10.4 and higher (intel) (MACX.X86_64)

Note that PC platforms not mentioned here specifically may still be
supported by one or more of the distributions (e.g. Ubuntu can use
LNUX).

CORE IRAF REVISIONS SUMMARY
---------------------------

This section describes changes to tasks in the IRAF core system other
than routine bug fixes.

New Tasks
~~~~~~~~~

-  VO-CL: A new version of the CL call ‘vocl’ is now the default CL on
   startup. This version supports both SAMP messaging to interopate with
   other VO applications, as well as new builtin functions to query VO
   data and services.

-  system.urlget: Native IRAF task to retrieve and HTTP URL

-  system.fcache: List, clean or manipulate the system file cache

Existing Tasks with New Parameters or New Parameter Defaults
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  images.imcoords.ccmap: A new option “tweak” was added to the values
   for the “refpoint” parameter to allow controlling whether to tweak
   the input tangent point.

-  images.imcoords.ccmap: New parameters xref and yref can be set to a
   value or header keyword in order to constrain the solution to the
   specified reference pixel.

Existing Tasks with New Capabilities
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  All Tasks: Now able to use an HTTP URL in places where an input
   image/file is expected.

-  All Tasks Using Lists: Support for enhanced @-files

-  All Tasks Expecting Tabular Input: Now able to use VOTable XML
   documents

-  images.imcoords.ccdmap: Changes to allow constraining WCS solutions
   to specified tangent point parameters (reference pixel and reference
   coordinate). This adds parameters so potentially requires users to
   update scripts.

NOAO PACKAGE REVISIONS SUMMARY
------------------------------

This section describes changes to tasks in the NOAO package tasks other
than routine bug fixes.

New NOAO Package Tasks
~~~~~~~~~~~~~~~~~~~~~~

-  noao.onedspec:

   -  hirescal - Apply HIRES wavelengths to flux data to produce an IRAF
      file.

Existing Packages and Tasks with New Parameters or New Parameter Defaults
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

N/A

.. _existing-tasks-with-new-capabilities-1:

Existing Tasks with New Capabilities
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  artdata.mkobjects: Added Sersic model profiles. This was done using
   only the model name to avoid any additional parameters.

-  astutil.observatory: Added entries for SOAR, Baker Observatory,
   McDonald Obs. HET, the Jack C. Davis Observatory and Langkawai
   National Observatory

-  onedspec/splot: Added overplotting of individual components to
   deblending code.

VO PACKAGE REVISIONS SUMMARY
----------------------------

New VO Package Tasks
~~~~~~~~~~~~~~~~~~~~

The VO package is new to the IRAF v2.16 release. Current contents of the
package include:

-  Toplevel apps:

   -  registry: Query the VO Registry

-  Toolbox sub-package:

   Data Query/Access Tools

   -  getcat: Query catalog data services in the VO
   -  getimg: Query image data services in the VO
   -  getspec: Query spectral data services in the VO (NYI)
   -  getlines: Query spectral line data services in the VO (NYI)
   -  vodata: General purpose query of VO data service

   Image Utilities

   -  dss: Display a DSS2 image of a named field
   -  imgcat: Create a catalog of detections in an image
   -  wcsinfo: Summarize the WCS information of an image
   -  dispname: Get the currently displayed image name

   VO Service Tools

   -  sesame: Resolve an object name to a position

   Simple Catalog Tools

   -  nedoverlay: Overlay NED objects in the image display
   -  obslogoverlay: Overlay an observation catalog (HST, XMM, etc)
   -  radiooverlay: Overlay NVSS radio contours in the image display
   -  xrayoverlay: Overlay RASS3 X-Ray contours in the image display

   Registry Tools

   -  mkregdb - Create a local VO Registry database
   -  regdb - Manage/Query a local VO Registry database
   -  regmetalist - List the metadata fields of a Registry record

   Votable Utility Tools

   -  votcopy: Copy a VOTable to another format
   -  votget: Download data referenced in a VOTable
   -  votpos: Extract the main positional columns from a VOTable
   -  votsize: Get the size of a VOTable

   Table Utilities

   -  colbyid: Identify VOTable column by ID attribute
   -  colbyucd: Identify VOTable column by UCD attribute
   -  colbyname: Identify VOTable column by NAME attribute
   -  tabclip: Clip a table to given boundaries
   -  taboverlay: General table overlay in the image display

   External Applications

   -  aladin: Start/Stop/Status of the Aladin image display application
   -  hub: Start/Stop/Status of the SAMP Hub
   -  topcat: Start/Stop/Status of the TOPCAT table display application

   SAMP Message Handlers

   -  overhandler: Default SAMP handler for image overlays
   -  tblhandler: Default SAMP handler for table loading messages
   -  imghandler: Default SAMP handler for image loading messages

   Hidden Tasks

   -  qstring: Generate a query string URL
   -  makewcs: Create an IRAF WCS from a plate solution
   -  prettystr: Pretty-print a long string

.. _existing-packages-and-tasks-with-new-parameters-or-new-parameter-defaults-1:

Existing Packages and Tasks with New Parameters or New Parameter Defaults
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

N/A

.. _existing-tasks-with-new-capabilities-2:

Existing Tasks with New Capabilities
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

N/A

BUG LOGS FIXED BY THIS RELEASE
------------------------------

The following buglog entries are fixed by the this V2.16 release:

::

   NUMBER: 574
   MODULE: daophot.psf
   SYSTEM: -V2.14
   DATE:   Tue Apr 13 22:01:52 MST 2010
   FROM:   fitz

   BUG:    The ":function" command was not properly saving the new function
           when refitting is done with the 'f' keystroke.  This is because the
           fitting function reinitializes the parameters to the startup
           values without first saving the modified function.

   STATUS: Fixed for the next release

::

   NUMBER: 575
   MODULE: all tasks using the icfit tools
   SYSTEM:  - V2.14
   DATE:   Mon Jun 28 14:08:48 MST 2010
   FROM:   valdes

   BUG:    The icfit tools are used in many tasks involving 1D function fitting.
          These include onedspec tasks like continuum and identify.  The
          tools provide for a grow radius where any sigma rejected points
          have neighbors also rejected.  The logic was wrong
          in two ways; one where if a neighbor was also a rejected point
          it did not also reject neighbors of that point, and another where
          the grow radius units were used both as in pixels and in user
          coordinates.  In reality the grow is supposed to be in user
          coordinate units.  In addition some tasks, like continuum, incorrectly
          described the units adding to the confusion.

   STATUS: Fixed for the next IRAF release.

::

   NUMBER: 576
   MODULE: imcombine
   SYSTEM: V2.14
   DATE:   Wed Nov 17 15:20:16 MST 2010
   FROM:   valdes

   BUG:    The addition of the image names using imcmb="$I" does not work for
           input images with a square bracket; e.g. foo[1], foo[im1], foo[*,*].
           The IMCMB value, in order to allow long filenames, is stripped of
           any path.  For an obscure reason related to VMS directories this
           code failed to find a rootname.

   STATUS: This has been fixed for the next release.

::

   NUMBER: 577
   MODULE: dohydra, dofibers, doargus, do3fiber
   SYSTEM: -V2.15.1
   DATE:   Fri Feb 11 12:30:46 MST 2011
   FROM:   valdes

   BUG:    The tasks will shorten root input image names to six characters by
           using the first five and last characters.  Depending on the style
           of image names this can result in name conflicts.  The reason for
           this shortening is no longer known so it is now considered a bug.
           Workarounds are to be aware of this and rename image names to avoid
           conflicts.

   STATUS: This is fixed in the next release.  The fix is to modify the file
           $iraf/noao/imred/src/fibers/proc.cl as shown (replace lines 125 to
           129 with "extn = extn // ".ms").  If you don't have permission
           to make this change then copy the file to your iraf "home$"
           directory, edit it, load the desired package, and then override
           the definition of the file with "redefine proc = home$proc.cl".

           125,129c125
           < i = strlen (extn)
           < if (i < 7)
           <     extn = extn // ".ms"
           < else
           <     extn = substr (extn, 1, 5) // substr (extn, i, i) // ".ms"
           ---
           > extn = extn // ".ms"

::

   NUMBER: 578
   MODULE: splot, scombine, fxcor, identify tasks, dispcor, disptrans
   SYSTEM: v2.15-V2.15.1a  (64-bit platforms only)
   DATE:   Tue Mar  8 21:57:38 MST 2011
   FROM:   fitz

   BUG:    The 64-bit port changes to smw.h improperly added a P2R() macro to 
       the APLOW/APHIGH macro declarations.  This was causing tasks with 
       2-D data to make an out-of-bounds request for data and leading to
       and error message such as

           ERROR: Pixel subscript out of bounds (spec.fits) 

       Normal onedspec data or use on 32-bit platforms is not affected.

   STATUS: Fixed for the next release.  A re-application of the v2.15.1a patch
       file will replace the affected binaries on 'linux64' and 'macintel'
       platforms.

::

   NUMBER: 579
   MODULE: onedspec.specplot
   SYSTEM: V2.15
   DATE:   Thu Mar 31 10:41:56 MST 2011
   FROM:   fitz

   BUG:    SPECPLOT can sometimes throw a segmentation violation or not
           recognize valid input spectra due to an incorrect macro definition
           on 64-bit platforms (linux64 and macintel only).

   STATUS: This has been fixed for the next release.  Patched x_onedspec.e
           binaries can be installed from

             ftp://iraf.noao.edu/iraf/v215/support/<arch>/x_onedspec.e

           where the <arch> is either 'linux64' or 'macintel'.

::

   NUMBER: 580
   MODULE: imcombine and variants
   SYSTEM: -V2.15.1
   DATE:   Fri Apr  1 10:53:41 MST 2011
   FROM:   valdes

   BUG:    When the grow options is used with masks or partially overlapping
       data a segmentation could occur.  This is because when data is
       absent (because of non-overlap) or excluded (because of mask) an
       identifier value was not initialized.  The only workaround is to
       not use the grow options.

   STATUS: Fixed for future patches and releases.

::

   NUMBER: 581
   MODULE: splot
   SYSTEM: V2.15-
   DATE:   Mon Jun  6 17:21:27 MST 2011
   FROM:   valdes

   BUG:    When using the deblending options a memory free error occurs with
       64-bit versions.  This is caused by allocating an integer array and
       freeing it as a real array.

   STATUS: Fixed in future patches and releases.

::

   NUMBER: 582
   MODULE: utilities.curfit
   SYSTEM: -V2.15.1
   DATE:   Fri Jul 29 12:40:08 MST 2011
   FROM:   valdes

   BUG:    For input data with two or more values having the same x value
           there is an arithmetic exception when setting the niterate parameter
           greater than zero during interactive fitting.  This occurs because
           a check for the distance between two points for the purpose of the
           grow option divides by the distance.  This is done even if no growing
           is requested (grow=0).  The workaround is to edit the input so that
           the values are not exactly the same.

   STATUS: This condition has been eliminated for the next release.

::

   NUMBER: 583
   MODULE: apall,
   SYSTEM: V2.15
   DATE: Mon Mar 5 08:51:03 MST 2012
   FROM: valdes

   BUG:    The optimal extraction for significantly tilted spectra, the Marsh
           algorithm, has bug which manifests only under 64-bit architectures.
           The symptom is a crash, usually a memory or segmentation panic.
           The only workarounds are 1) go to an 32-bit system or 2) don't
           use the optimal extraction option.

   STATUS: Fixed for V2.16.
