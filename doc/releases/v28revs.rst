IRAF Version 2.8 Revisions Summary
==================================

:Authors: IRAF Group
:Date: June 30, 1989

Introduction
------------

This revisions notice coincides with the release of version 2.8 of IRAF.
The V2.8 release is a general release for all supported IRAF hosts.

The following is a brief description of some of the new features
available in IRAF Version 2.8. This is not intended to be an exhaustive
list, but rather a brief summary of the major changes since the last
major release of IRAF Version 2.5 in July 1987 and subsequent
intermediate releases primarily to support Sun/IRAF: IRAF Version 2.6
(February 1988), IRAF Version 2.6+ (March 1988), and IRAF Version 2.7
(December 1988).

More detailed revisions notes are available in the system notes files in
the ``iraf$doc`` and ``iraf$local`` directories, as well as in the
online revisions notes for the various packages.

IRAF System Revisions
---------------------

This document highlights the most notable revisions made to the IRAF
core system software for Version 2.8. This is only a revisions summary;
no attempt is made to provide detailed technical documentation for each
revision, nor is there any attempt to exhaustively summarize all
revisions. A complete record of all core system revisions will be found
in the *System Notes* for V2.8. Additional information on some of the
topics covered below will be found in the various *Installation Guides*
and *Site Manager’s Guides*, and in the *IRAF User and Technical
Documentation* manual sets.

Copyright notice
~~~~~~~~~~~~~~~~

Subject to AURA and NSF approval, the IRAF software will be copyrighted
sometime during 1989. As a first step in this process, a copyright
notice has been added to all core system source files. The notice reads
as follows: “Copyright(c) 1986 Association of Universities for Research
in Astronomy Inc”. We will also be adding a file called COPYRIGHT to the
distribution stating the terms of the copyright and associated licensing
agreement for the software.

The intent of this action is solely to protect the software from
unauthorized commercial exploitation, and the copyright grants, or will
grant, the right to copy, modify, and redistribute the IRAF software
provided the original copyright notice remains intact, the software is
made available in source form, and the rights we grant are passed on
with the software. We wish to prevent others, especially commercial
firms, from copyrighting IRAF software in their own name and possibly
taking away the rights we grant with the software. Granting the right to
modify and redistribute IRAF software does not mean we want to encourage
people to do so, we merely want them to have the legal right to do so if
they feel they need to.

Major system enhancements
~~~~~~~~~~~~~~~~~~~~~~~~~

The information in this section is provided primarily for the benefit of
IRAF site managers and programmers. The reader interested primarily in
science applications may wish to skip ahead. Some systems level
familiarity with the current IRAF system is assumed.

Layered software enhancements
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A given IRAF installation consists of the core IRAF system, and any
number of **layered software products** or **external packages**. The
goal of the layered software enhancements introduced in V2.8 is to make
layered software products self contained and hence independent of the
core system and of other layered software. Examples of layered software
products are the NOAO packages, LOCAL, STSDAS, PROS, and so on.

The layered software enhancements make it possible to install or
deinstall a layered product by modifying only a single file in the core
IRAF system. The core system may be updated without affecting layered
software, and vice versa. Since layered products are independent and are
simple to install, IRAF can easily be configured with only those
packages needed at a particular site. Software developers benefit from
the layered software enhancements because the facilities provided for
development and maintenance of layered software are equivalent to those
provided for development of the core IRAF system and the NOAO packages.
User sites benefit because it is easy to extend the system with LOCAL
packages of their own making.

Each layered product (usually this refers to a tree of packages) is a
system in itself, similar in structure to the core IRAF system. Hence,
there is a LIB (global system library), one or more BINs (binary file
directories), a Help database, a set of global environment definitions,
and all the sources and runtime files, all contained within the same
directory tree. Layered software products, in their source only form,
are portable without change to any computer which runs IRAF.

The hlib$extern.pkg file
''''''''''''''''''''''''

This is the file which is modified to install or deinstall layered
software products. To install a layered product, one creates a directory
to hold the software, restores the files to disk, and edits the
``extern.pkg`` file to tell IRAF the name of the root package of the
layered product, and where the root directory is located. If the layered
software is distributed in source only form it will also be necessary to
recompile the software, but this is a completely automated process.

NOAO and LOCAL packages reorganized
'''''''''''''''''''''''''''''''''''

As part of the project to better support layered software, the NOAO and
LOCAL packages have been reorganized as layered products. These packages
are now structurally equivalent to third party (non-NOAO) packages,
except that the directory trees are rooted in IRAF. Both packages are
now self contained, with their own LIB, BINs, Help database, etc., and
with an entry in ``extern.pkg``, like other layered products. The NOAO
package serves as a working example of how to configure a layered
package. The reorganization of these packages should be transparent to
anyone merely using the system.

The template LOCAL
''''''''''''''''''

The LOCAL package included with the distributed system has been stripped
of all NOAO site-local tasks and restructured as a layered product, the
*template local*. The template local contains only two sample tasks and
is not intended as an end-user package, but rather as a template to be
copied and modified by sites to construct their own site dependent LOCAL
package. The desire to be able to easily develop and maintain locally
added packages was one of the major motivations for the layered software
enhancements project, and we hope that sites will realize the
significance of this new capability and take advantage of it.

CL now supports package level BIN directories
'''''''''''''''''''''''''''''''''''''''''''''

Rather than assuming a global BIN directory for all tasks and packages,
the CL now permits multiple BIN directories, each BIN directory being
associated with the package of definition and all subpackages of that
package (unless they have their own BIN). A new BIN directory is
declared with the optional argument ``bindir=*path`` in the ``package``
statement, e.g., in a package script task.

MKPKG support for package environments
''''''''''''''''''''''''''''''''''''''

Layered packages now have their own private LIB, including an
environment definitions file (``zzsetenv.def``), mkpkg global include
file (``mkpkg.inc``), and, optionally, a mkpkg special file list file
for each supported host system, listing files requiring special
compilation to work around host compiler bugs or whatever. The full
mkpkg environment is formed by reading the IRAF core system environment
and mkpkg definitions and include files, followed by the package
definitions and include files. Reading of the package environment occurs
*only* if mkpkg is called with the “**-p**” flag, or if the variable
``PKGENV`` is defined in the user’s environment.

Another way of expressing this is, when using mkpkg within a layered
package, one must now specify the name of the layered package in order
to pick up the package environment definitions. For example, to update
the MTLOCAL package in NOAO, one would type “``mkpkg -p noao update``”
in the ``mtlocal`` directory. If this is not done compilation errors may
result, or the executable may not be successfully installed in the
package BIN directory.

Multiple architecture support
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A single IRAF system (or layered package) can now simultaneously support
any number of machine architectures using multiple BIN directories
sharing a single machine independent copy of IRAF. Each BIN directory
contains all the object modules, object libraries, and executables for a
particular architecture. An architecture can represent either a type of
hardware, e.g., sparc, mc68020+f68881, mc68020+ffpa, vax, etc., or a
software distinction, e.g., systems compiled with different sets of
compiler flags, or different versions of a system. Multiple
architectures are now supported both for IRAF execution, and for IRAF
based software development, e.g., a single version of IRAF can now be
used to develop and run IMFORT programs on both Sun-3 and Sun-4 nodes.

The only case where multiple architecture support is used at the present
time is in Sun/IRAF, which is often installed on a heterogeneous network
of workstations, e.g., Sun-3s with various hardware floating point
options, and Sun-4s. A single copy of IRAF will be configured with
several BIN directories, one for each supported architecture, and NFS
mounted on all the network nodes which will be using IRAF. There is no
reason that this feature need be restricted to use with Sun/IRAF,
however.

IRAFBIN and IRAFARCH
''''''''''''''''''''

Starting with IRAF V2.8, the old environment variable ``IRAFBIN`` has
been obsoleted and replaced by ``IRAFARCH``. On machines which support
multiple architectures, the latter defines the architecture to be used
for both IRAF execution and software development. If only IRAF execution
is needed the variable is optional, with the best architecture being
selected automatically when the CL is started. If one will be doing
software development (including IMFORT) it is best to define the
variable in the host environment before starting IRAF or doing any host
level software development. Typical values of ``IRAFARCH`` for a Sun
workstation are “sparc”, “i386”, “f68881”, and “ffpa”.

System libraries moved to the BIN directory
'''''''''''''''''''''''''''''''''''''''''''

As part of the revisions required for multiple architecture support for
software development, all object libraries have been moved from the
global, architecture independent LIB to the architecture dependent BIN,
with the LIB entries being replaced by symbolic links (in the case of
Sun/IRAF). This should be transparent to both end users and programmers.

New bin.generic architecture
''''''''''''''''''''''''''''

On Sun/IRAF systems, which are distributed configured for multiple
architecture support, the system architecture is set to ``generic`` in
the distributed system. What this means is that all architecture
dependent files (objects and object libraries) have been removed from
the system directories and archived in the file ``OBJS.arc`` in the BIN
directory for each architecture. Rebuilding any of the packages in a
system would require restoring the binaries for a particular
architecture, e.g., typing “``mkpkg sparc``” at the IRAF root would
restore the sparc binaries for the core system on a Sun/IRAF
installation. Note that this *only* affects software development for the
system in question; software development for external packages or
private user software is not affected.

Shared library facility
^^^^^^^^^^^^^^^^^^^^^^^

IRAF version 2.8 adds support for a general shared library facility for
UNIX based systems. Although currently only used with Sun/IRAF, this
facility is potentially useful for other UNIX based IRAF systems as well
(VMS/IRAF already has its own shared library facility).

What the shared library facility does is take most of the IRAF system
software (currently the contents of the ``ex``, ``sys``, ``vops``, and
``os`` libraries) and link it together into a special sharable image,
the file ``S.e`` in each core system BIN directory. This file is mapped
into the virtual memory of each IRAF process at process startup time.
Since the shared image is shared by all IRAF processes, each process
uses less physical memory, and the process pagein time is reduced,
speeding process execution. Likewise, since the subroutines forming the
shared image are no longer linked into each individual process
executable, substantial disk space is saved for the BIN directories.
Link time is correspondingly reduced, speeding software development.

With the introduction of the shared library facility, the disk space
required for Sun/IRAF is substantially reduced. Due to the increased
memory sharing and reduced process pagein times performance is
substantially improved, especially on systems like the Sun/386i which
has a relatively slow SCSI disk and often limited memory. The disk size
of small programs is reduced by up to a factor of ten in some cases,
e.g., an executable for a small program that was formerly 250 Kb in size
might be as small as 25 Kb if the shared library is used and the shared
image symbols are omitted at link time.

User interface changes
~~~~~~~~~~~~~~~~~~~~~~

Calling IRAF tasks from the host environment
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The IRAF main and zmain were modified to make it easier to call IRAF
tasks as host level tasks, i.e., without having to set up a command file
and run the process with the standard input redirected. In the new
scheme, any extra arguments given on the process command line are passed
into the IRAF main as a command buffer containing the IRAF command or
commands to be run. For example,

::

   cl> x_system.e netstatus

would run the command ``netstatus`` in process ``x_system.e``.

::

   cl> x_system.e count "files=*.x"

would run the ``count`` task, counting all “.x” files in the current
directory.

::

   cl> x_system.e count "files=*.x 4>_o"

would do the same, redirecting the output at the IRAF main level to the
file ``_o``.

::

   cl> x_system.e 'directory @pars $nargs=0'

would run the ``directory`` task with the given parameter set, with
``$nargs`` set to 0. If any of the parameters to a task are omitted the
task will query the terminal for them in the usual way, so for example

::

   cl> alias count "$iraf/bin/x_system.e count files="

would make the IRAF task ``count`` available in UNIX, allowing the IRAF
template specifying the files to be counted to be either given on the
UNIX command line, or prompted for if omitted. Given the above alias,
one could enter a UNIX command such as

::

   cl> count 'cl$*.h'

This feature is available in all UNIX based versions of IRAF V2.8, but
did not make it into VMS/IRAF version 2.8.

Image packing density control (impkden)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Some users have complained about images taking up more disk space than
they have to, due to the IMIO feature which conditionally blocks image
lines to fill an integral number of disk blocks. This can result in more
efficient image i/o but can also make a significant difference in the
amount of disk space consumed by an image in some cases.

IMIO can actually support both block-aligned and fully packed images.
The decision is made at image creation time and is based on the **image
packing density** if image lines are block aligned. If the packing
density is too low for a block-aligned image, a fully packed image is
created to avoid wasting disk space. The default minimum packing density
is 0.6, i.e., up to 40% wasted space before IMIO switches to full
packing (no wasted space).

For finer control over the packing density, the user can now specify the
optional environment variable ``impkden``, the numeric value being the
minimum packing density. For example,

::

   cl> set impkden = 1.0

would completely disable block-alignment of image lines in IMIO.

User libraries (IRAFULIB)
^^^^^^^^^^^^^^^^^^^^^^^^^

It is now possible for the programmer (SPP or IMFORT) to specify a
private directory to be searched at compile or link time when developing
IRAF or IMFORT programs. This is done by defining the path to the
directory in the user environment as the variable ``IRAFULIB``. When
locating a particular file, this directory will be searched *before* the
IRAF system libraries are searched, hence this feature may be used to
substitute custom versions of files in the IRAF system libraries, e.g.,
for debugging purposes.

New logical printer device LPR
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A new logical line printer or plotter device ``lpr`` is now supported on
all UNIX/IRAF systems. This treats the UNIX task *lpr* as a kind of
pseudo-device, leaving it up to UNIX to decide what physical device to
dispose of the output to. This default is system dependent, but on some
systems can be controlled by defining the variable ``PRINTER`` in the
user environment.

Machine independent help database
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The IRAF ``help`` task uses a precompiled binary database to speed help
keyword searching. This file is now machine independent, allowing it to
be generated on one system and included in software distributions
without having to be recompiled. In addition, as part of the layered
software support, ``help`` now allows each external package to have its
own private help database. The first time ``help`` is run, all such
databases are read and linked to produce a database containing entries
for all help modules in the core system and all installed external
packages. The help database file is the file ``helpdb.mip`` in the LIB
directory of the core system and each external package.

Set terminal type will no longer hangup
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On systems, e.g., workstations, which provide virtual terminal windows
which can change in size, IRAF may query the terminal at run time to
determine the screen size. This query is performed, for example, at
login time if the terminal type is set to ``gterm`` or ``sun``. Formerly
this could cause the login process to hang indefinitely (i.e., until the
user typed return or interrupt) if the terminal did not respond to the
size query, as would happen when the terminal type was set improperly
and the actual terminal ignored the query. Thanks to the addition of
non-blocking raw terminal i/o in V2.8 IRAF, the terminal screen size
query will now time out with a warning message to reset the terminal
type, if the terminal does not respond to the query within several
seconds.

Installing a new version of IRAF obsoletes old user parameter files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The problem of old, obsolete user (``uparm``) parameter files being used
with a newly installed version of IRAF, which could lead to “parameter
not found” error aborts, has been fixed. The CL now checks the date of
the file ``utime`` in HLIB, and refuses to use the user pfile if it is
older than either ``utime`` or the package pfile provided with the new
system. The contents of old user pfiles are merged into the new system
pfile, as before, preserving learned parameter values even when the user
pfile is obsolete.

@file list bug fixed
^^^^^^^^^^^^^^^^^^^^

The problem of the “@file” (at-file-list) syntax not working when the
file in question was not in the current directory has been fixed.

Programming interface changes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IMFORT pixel directory control
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

IMFORT has been modified to permit specification of the pixel file
directory by the calling program. The modifications are completely
upwards compatible, i.e., existing programs linked with the new
interface will still create pixel files in the same directory as the
header file, with “HDR$” in the image header.

The Fortran programmer may set or query the pixel file directory using
the following routines:

::

   imsdir (dir)            # set pixel directory pathname
   imgdir (dir)            # get pixel directory pathname

where *dir* is a Fortran character variable. The value should be either
“HDR$” (the default) or a concatenable host directory pathname (i.e.,
trailing / required for UNIX). Once set, the pixel directory will be
used for all subsequent image create or rename operations by the calling
process.

For example,

::

   call imsdir ("/tmp3/pixels/")
   call imcrea (image1, axlen, naxis, dtype, ier)
   call imcrea (image2, axlen, naxis, dtype, ier)

If desired the default pixel directory may be specified in the host
environment as ``imdir`` or ``IMDIR`` before running the program. IMFORT
will check the host environment for this environment variable then use
“HDR$” as the default if no host definition is found.

Note that although this is similar to setting the value of ``imdir`` in
the IRAF environment, IMFORT programs are not part of the IRAF
environment and are not affected by changes to the IRAF ``imdir``. Also,
since IMFORT is a host level facility and IRAF networking is not
supported, the network prefix (e.g., “node!”) is omitted from the
pixelfile pathname, and since IMFORT programs are not necessarily used
in conjunction with IRAF, the “``..``” (hidden file protection) files
are not used to protect against image deletion.

Image display interface: IMD
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A new interface IMD has been added to provide a rudimentary facility for
interactive image display device control. This is an interim prototype
interface which will be replaced by the new display interfaces when the
latter become available.

The IMD interface operates by mapping an image display device frame
buffer onto an IMIO image descriptor. The display frame buffer may then
be randomly edited by normal image i/o operations, e.g., to modify
subrasters of the displayed image, or overlay the image with color
graphics. The image pixel to display frame buffer coordinate
transformation is supported, allowing applications to work in image
pixel coordinates if desired. This interim interface is what is used by
the new display oriented tasks ``imexamine``, ``imedit``, and
``tvmark``.

Image masks: PLIO, PMIO, MIO
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following new VOS interfaces have been added in V2.8 to provide a
general boolean or integer image mask facility.

::

   PLIO    pixel list i/o
   PMIO    pixel (image) mask i/o
   MIO masked image i/o (image i/o through a mask)

PLIO is a general interface for storing and manipulating
multidimensional integer valued rasters containing regions of constant
value (i.e., masks). The masks are stored in a highly compressed form,
the size of the compressed mask being a function of the information
content of the mask. Both pixel array and range list i/o facilities are
provided, as well as a set of general boolean raster operators, e.g., to
extract or insert subrasters, AND or OR a source with a destination, do
the same through a stencil, draw regions of various kinds (point, line,
box, circle, polygon), and so on. See the ``PLIO.hlp`` file in the PLIO
source directory for further information.

An interactive debug program (``plio$zzdebug.x``) is provided for
experimenting with masks. Note that PLIO is a stand alone interface and
is not tied in any way to IMIO, even though the data structure operated
upon is similar to an image matrix.

PMIO is very similar to PLIO except that it is used to associate a masks
with an IMIO maintained reference image. Currently, the PMIO mask must
be the same resolution as the physical reference image. All coordinates
input to PMIO are in the \*image section coordinates\` of the reference
image. Hence, given a physical image and associated mask, one can
operate upon both through a user specified image section transparently
to the applications program. This includes all PLIO style boolean
rasterop operations, as well as mask pixel and range list i/o. The PMIO
interface is layered upon PLIO and IMIO, and the calling sequences are
identical with PLIO except for the package prefix, and the addition of
several new PMIO specific routines.

MIO is essentially an extension of image i/o for pixel i/o through a
mask. The central routines are the following:

::

                   mio_setrange (mp, vs, ve, ndim)
   n|EOF = mio_[gp]lseg[silrdx] (mp, ptr, mval, v, npix)

One defines a rectangular region of the image with mio_setrange, and
then sequentially reads or writes line segments until all pixels visible
through the mask have been accessed. This type of i/o should be ideal
for most image processing applications which need to operate upon only
those pixels visible through a region mask (e.g., a surface fitting
task), upon all pixels except those on a bad pixel mask (e.g., any
analysis program), and so on.

PLIO (or PMIO) masks may be stored in binary files on disk, the files
having the extension “``.pl``”. The V2.8 version of IMIO has the
capability to treat such masks as if they were images, allowing masks to
be easily displayed, used in image expressions, converted to image
matrices and vice versa, etc. Applications may do either pixel or *range
list i/o* to a mask image via IMIO, if MIO is not suitable for some
reason.

Photon images: QPOE, QPIO, QPEX
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A new set of VOS interfaces supporting photon or **event list data** are
now available. The QPOE interface implements the Position Ordered Event
list object, which consists of a general header mechanism plus an event
list, wherein the events are little data structures, e.g., the
attributes required to describe a photon detection (position, energy,
time, etc.). QPOE is designed to efficiently access very large event
lists, e.g., several hundred thousand or several million events in size.
Builtin event attribute filtering and region filtering capabilities are
provided for selecting photons from the event list. These filtering
capabilities may be combined with the sampling capability to produce
filtered, block averaged image matrices from event lists.

The QPOE interfaces are the following:

::

   QPOE    header and file access and management facilities
   QPIO    raw and filtered event i/o
   QPEX    event attribute filter mechanism
   QPF IMIO/IKI kernel for image interface to QPOE files

QPOE and QPF add a new image type to the system, with ``.qp`` file
extension. Hence, event list data can be used as input to any of the
image processing tasks in standard IRAF, in addition to being analyzed
by tasks which deal with the individual photon events. A QPOE image is
contained in a single file. When a QPOE file is accessed as an image the
interface filters and samples the event list in real time, using a user
defined filter, block averaging factor, region mask, and so on,
producing the image matrix seen by applications at the IMIO level. The
QPOE object may be repeatedly examined with different event filters to
view the data in different ways.

The QPOE interface, in addition to providing an event list capability
for IRAF, serves as a prototype for the “flex-header” portion of the new
image structures project. Many of the capabilities to be provided for
image storage under the new image structures are already present in
QPOE.

Further information is given in the ``QPOE.hlp`` file in the QPOE source
directory.

File manager: FMIO
^^^^^^^^^^^^^^^^^^

A new VOS library FMIO has been installed. FMIO is “File Manager I/O”,
and is used to implement a simple binary file manager which maintains
the file data of so-called “lfiles” (lightweight files) inside a single
host binary file. The system overhead for accessing lfiles is much less
than that of host files, and many lfiles can be used to store a complex
data structure without cluttering a host directory or incurring the
inefficiency of accessing host files. FMIO is part of the DFIO project
and will serve as the lowest level interface within DFIO; it is also
used currently in the QPOE interface. Additional information is given in
the README file in the source directory for the interface.

IMIO changes
^^^^^^^^^^^^

IMIO is the image i/o interface, the standard IRAF VOS interface for
managing all varieties of image data.

Mask image support
''''''''''''''''''

IMIO now supports a new type of image, the **mask image**, stored as a
highly compressed binary (PLIO) file with the extension “``.pl``”. Image
masks are most commonly used to store information describing selected
pixels in an associated data image. An image mask is logically a boolean
or integer image, up to 28 bits deep, containing information only on
selected pixels or regions of pixels. Masks are stored in highly
compressed format, e.g., a simple mask may be stored in only a few
hundred bytes of space. Mask images are readable, writable, and randomly
modifiable, like ordinary raster images.

Photon image support
''''''''''''''''''''

Support has also been added to IMIO for **event list images**, stored as
position ordered event list datafiles using the QPOE interfaces. This
new image type has the extension “``.qp``”. QPOE images are read-only
under IMIO. Subject to that restriction, they may be accessed like any
other image by any IRAF image analysis program. Accessing an event list
image as a raster image necessarily involves a runtime sampling
operation, wherein the events in the region of interest are accumulated
into an initially zero image matrix; in the process the event list may
optionally be filtered by event attribute or event position, e.g.,

::

   cl> display "xray.qp[t=(30:40),pha=10,block=4]"

would display the QPOE image ``xray.qp`` with a blocking factor of 4,
selecting only those events with ``t`` (time) in the range 30 to 40 and
for which ``pha`` (energy) has the value 10. The event attributes and
their names are user definable and may vary for different types of data.

IMPUTH
''''''

A new procedure ``imputh`` has been added to the IMIO header access
library. The new procedure is used to append FITS like HISTORY or
COMMENT cards to the image header.

IMPARSE
'''''''

The calling sequence of the internal IMIO procedure ``imparse`` has
changed. Although this procedure is internal to the IMIO interface and
is not supposed to be used within applications, there may be
applications which make use of this procedure. Any such applications
must be modified to reflect the new procedure calling sequence or
runtime problems are guaranteed.

Null string environment variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The semantics of the VOS procedures ``envgets`` and ``envfind`` have
changed. This could affect existing programs and any programs which use
these functions should be checked to make certain they will still work
properly.

These procedures, used to fetch the string values of environment
variables, return the length of the output string as the function value.
Formerly, a value of zero would be returned both when the named variable
existed but had a null string value, and when the variable was not
found. This made it impossible to discriminate between the case of a
variable not being defined, and one which is defined but has a null
value. The routines have been changed to return the value ERR (a
negative integer) if the variable is not defined. Programs which do not
wish to make the distinction between undefined and null-valued should
check for a function value less than or equal to zero. Programs which
check for a function value equal to zero will fail if the named variable
is not defined.

Environment substitution in filenames
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The VOS filename mapping code has been modified to add support a
powerful new environment substitution syntax. Previously the only
environment substitution mechanism available was the logical directory
facility, which could only be used to parameterize the directory field.
The new facility may be used to perform environment substitution
anywhere in a filename. This is used in IRAF version 2.8 to implement
multiple architecture support, e.g.,

::

   cl> set bin = "iraf$bin(arch)/"

is how the core system BIN is defined in V2.8 IRAF. The syntax
“``(arch)``” tells the filename mapping code to substitute the string
value of the environment variable ``arch``, if defined. If the variable
is not defined the null string is substituted. Hence, if the host system
does not implement multiple architecture support and ``arch`` is not
defined, BIN is defined as “``iraf$bin/``”, which is the backwards
compatible definition. If ``arch`` is defined as, e.g., “``.vax``”, then
BIN is defined as “``iraf$bin.vax/``”. The new feature allows use of a
single environment variable to define the architecture, not only to form
filenames, but for other purposes as well, e.g., to generate compiler
switches or to control library searching in ``mkpkg``.

Nonblocking raw terminal i/o
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The VOS file i/o interfaces have been modified to add support for
nonblocking terminal i/o. This facility makes it possible to, in effect,
“poll” the terminal to see if there is any input waiting to be read, to
allow interaction without having a program block if the user has not
typed anything.

The immediate application of this in version 2.8 was the modification of
the ``stty`` (set-terminal) facility to implement a time out for the
terminal size query. Formerly, ``stty`` would hang up indefinitely when
the terminal type was set to “gterm” but the actual terminal was
something different, causing the screen size query to be ignored.

In the more general case, nonblocking terminal i/o makes possible a new
class of user interface, which is not only interactive, but **event
driven**. Nonblocking i/o makes it possible for an application to be
continually processing, while checking the terminal occasionally to see
if the user has input any commands.

At present, nonblocking i/o is always used in conjunction with raw mode
input from a terminal. A new flag ``F_NDELAY``, defined in ``<fset.h>``,
is used to enable or disable nonblocking i/o. For example,

::

   call fseti (fd, F_RAW, YES)

enables conventional blocking, single character raw mode reads, and

::

   call fseti (fd, F_RAW, YES + F_NDELAY)

enables nonblocking raw mode input (``YES``, ``NO``, and ``F_NDELAY``
are bit flags). These modes are mutually exclusive, e.g., the first call
may be issued while nonblocking raw mode is in effect to make the reads
block, and vice versa. A call to ``fset(fd,F_RAW,NO)`` disables both raw
mode and nonblocking mode. Once nonblocking raw mode is in effect one
merely reads characters from the terminal in the usual way, using
``getc``. EOF is returned if a read is performed when no data is
available for input, otherwise the next character is returned from the
input queue. Further information on nonblocking i/o is given in the
system notes file.

Function call tables (ZFUNC)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

IRAF has always had the ability to compute the integer valued address of
a procedure, store that address in a table, and later use the address as
an argument to one of the ``zcall`` kernel primitives to call the
addressed procedure. This facility has been extended by the addition of
a set of ``zfunc`` primitives, used to call integer valued *functions*.
Only integer valued functions are supported (in order to simplify the
kernel support required), but in the systems oriented applications where
procedure call tables are used, this is unlikely to be a serious
limitation.

Sun/IRAF specific revisions
~~~~~~~~~~~~~~~~~~~~~~~~~~~

IEEE exception handling
^^^^^^^^^^^^^^^^^^^^^^^

By default the IEEE hardware is now configured, on all Sun systems, with
the invalid, overflow, and divide by zero IEEE exceptions enabled, and
with the default rounding direction and precision modes (nearest,
extended) in effect. This configuration should ensure that all
questionable floating point operations are detected, and that no IEEE
“funny numbers” (NaN, Inf, etc.) get into the data. These values, since
they don’t behave like ordinary numbers, can cause programs to
misbehave, e.g., go into an infinite loop. In Sun/IRAF V2.8, if a
computation results in an IEEE funny number being generated, an
exception abort will result. The most common example is divide by zero.

The IRAF/IEEE interface offers a special debug feature that may be of
interest to programmers developing numerically sensitive software. If
desired, one can change the default rounding direction and precision
(e.g., to test the numerical stability of applications) by using the
debugger to set a nonzero value of the variable ``debug_ieee`` before
running an executable. The procedure for doing this is documented in the
system notes file.

IMTOOL enhancements
^^^^^^^^^^^^^^^^^^^

A number of enhancements and bug fixes have been made for V2.8 to the
SunView based IMTOOL image display server. The most notable changes are
summarized here; refer to the IMTOOL manual page for a more complete
description of the new features.

Software ZOOM added
'''''''''''''''''''

IMTOOL, which has had for some time the ability to pan about on a large
image, now has the ability to zoom as well. Both pan and zoom are
controlled very conveniently by the middle mouse button: place the mouse
on an object and tape the middle button once to pan the object to the
center of the display window; tap it again and the image will be zoomed.
Zoom, currently implemented by writing directly into the hardware frame
buffer, is very fast, almost as fast as a normal unzoomed window
refresh. The default set of zoom factors is 1,2,4,8 after which the
sequence wraps around to 1. The zoom factors are user configurable via
the IMTOOL setup panel; very large zoom factors, e.g., x64, are
possible. Dezoom (making a large image smaller) is not currently
supported.

WCSDIR eliminated
'''''''''''''''''

The host level ``WCSDIR`` environment variable, and the text file used
to communicate image coordinate (WCS) information between the display
task and the display server, have been eliminated. All WCS information
is now passed via the datastream used to pass commands and data between
the client and the display server. This eliminates the need for users to
have to remember to define ``WCSDIR`` in order to get coordinates in
image units, and some subtle process synchronization problems are
eliminated as well.

In a related change, the frame buffer configuration index is no longer
passed in during a frame erase, hence it is no longer necessary to erase
a frame before displaying an image to ensure that a frame buffer
configuration change is passed to the server. The configuration index is
now passed when the WCS information for a frame is set.

Graphics colors
'''''''''''''''

IMTOOL now allocates a range of pixel values for use as graphics overlay
colors. Setting a frame buffer pixel to one of these values causes it to
always be displayed with the assigned color. The graphics color values
are not affected by windowing the display. The most common use of
graphics colors with V2.8 IRAF is for drawing graphics into a displayed
frame with the new ``tvmark`` task, available in PROTO. See the IMTOOL
manpage for a table listing the color index assignments.

New imtoolrc entries
''''''''''''''''''''

Several new predefined frame buffer configurations have been added to
the default ``imtoolrc``. These include an 128 pixel square frame buffer
(``imt128``), a 256 pixel square frame buffer (``imt256``), and a full
screen display with the same aspect ratio as a 35 mm slide
(``imtfs35``).

System crash (FIFO) bug fixed
'''''''''''''''''''''''''''''

Versions of SunOS through at least 4.0.1 have a bug in the FIFO driver
code which can cause the internal kernel FIFO data buffer to be
deallocated while it is still in use. This will result in a bad kernel
which will eventually panic and reboot the system. This is the cause of
the IMTOOL crash problem which some sites may have experienced. IMTOOL
has been modified to avoid the circumstances (repeated 4096 byte
transfers) which cause the bug to surface. So far as we know, the real
bug (in SunOS) has not yet been fixed, but at least on the NOAO systems,
the frequency of occurrence of the system crashes is greatly reduced
with the new version of IMTOOL which incorporates the workaround for the
SunOS bug.

Cursor marking now disabled by default
''''''''''''''''''''''''''''''''''''''

When the interactive image cursor read facility was first added to
IMTOOL, the default response to each cursor read was to draw a small
white dot at the position of the cursor. This is convenient when marking
a series of objects to make a list, but with the increasing number of
IRAF programs making user of the interactive image cursor, it has been
necessary to change the default to disable automatic marking of each
cursor read. The cursor mark feature is still available as an option and
can be enabled via the setup panel.

Ctrl/b may be used for manual blinking
''''''''''''''''''''''''''''''''''''''

In addition to the list of blink frames and the timed blink feature
IMTOOL has provided for some time, it is now possible to manually cycle
through the blink frames with the key. Typing while the mouse is in the
image window will cause the display to display the next blink frame in
sequence.

F4 key will now toggle setup panel
''''''''''''''''''''''''''''''''''

The F4 function key on the Sun keyboard may now be used to toggle
whether or not the setup panel is displayed. This provides a single
keystroke alternative to calling up the setup panel with the frame menu.

VMS/IRAF specific revisions
~~~~~~~~~~~~~~~~~~~~~~~~~~~

NEWUISDISP added to VMS/IRAF
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Nigel Sharp’s ``NEWUISDISP`` display program, used for image display
under UIS on microvaxes with bitmapped displays, is now available in the
standard VMS/IRAF release, in the directory ``[IRAF.VMS.UIS]``.

New INSTALL.COM script
^^^^^^^^^^^^^^^^^^^^^^

A new ``INSTALL.COM`` script (also written by Nigel Sharp) has been
added to VMS/IRAF. This script, run by the system manager to install
selected IRAF executable images, will now automatically check for and
deinstall any old versions of the executables before installing the new
ones.

VMS 4.7/5.0
^^^^^^^^^^^

Testing of the standard V2.8 VMS/IRAF release, which was prepared on VMS
4.7, on a VMS 5.0 system has thus far not revealed any problems (NOAO is
still running VMS 4.7 as our standard system). Hence it appears that the
standard V2.8 VMS/IRAF will *run* under VMS 5. It is likely, however,
that any attempt to *recompile* VMS/IRAF under VMS 5 would cause
problems, since we have not yet tried to rebuild IRAF under VMS 5, and
such a major operating system upgrade will often require changes to the
IRAF code. The system may be relinked under VMS 5 if desired, and this
does not appear to cause any problems, but neither does there appear to
be any benefit to be gained from doing so.

Summary of IRAF System Packages Revisions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  The tasks RFITS and WFITS in the DATAIO package now support the
   reading and writing of arbitrary sized data blocks (IRAF version 2.7
   and later).

-  Several new tasks were added to the IMAGES package. IMCOMBINE (IRAF
   version 2.6 and later) provides for the combining of images by a
   number of algorithms. The new task CHPIXTYPE (IRAF version 2.7 and
   later) changes the pixel types of a list of input images. The task
   IMSLICE slices images into images of one less dimension (IRAF version
   2.8). The task IMSTACK has been moved into the IMAGES package
   (although it still resides in PROTO as well).

The IMSTATISTICS task has been rewritten and now allows the user to
select which statistical parameters to compute and print (IRAF version
2.8). The IMRENAME task has been modified to allow “in place” image
renames, used chiefly for moving the pixel files to a new IMDIR.

Several other tasks in the IMAGES package were modified (IRAF version
2.8). IMSHIFT was modified to accept a list of shifts from a file.
REGISTER and GEOTRAN were modified to accept a list of transforms
instead of only a single one. IMHISTOGRAM has undergone extensive
revision including support for “box” type plots, support for linear or
log scaling in the y coordinate, as well as support for antialiasing of
the histogram bins.

-  All the tasks in the IMAGES.TV package were modified (IRAF version
   2.8) so that if a task is used with an unsupported display device a
   message is printed to that effect.

-  The STTY task in the LANGUAGE package has been improved (IRAF version
   2.6 and later) to better facilitate its “playback” feature. These
   changes have been documented in the online help for the task. This
   feature is little used by external sites but can be a very useful
   instructional aid if users are aware of its capability.

-  A new task PVECTOR was added to the PLOT package that allows one to
   plot an arbitrary vector in a two dimensional image (IRAF version 2.6
   and later).

The task STDPLOT was modified (IRAF version 2.8) so that it uses the
more popular SGI kernel rather than the NSPP (NCAR) kernel (STDPLOT is
now equivalent to the SGIKERN task). A new task NSPPKERN was added that
uses the NSPP kernel.

-  Two new tasks were added to the SYSTEM package (IRAF version 2.8).
   The task DEVICES simply prints the ``dev$devices.hlp`` file as edited
   by the site manager listing available devices on the local host or
   network. The REFERENCES task is used to search the help database for
   all tasks or other help modules pertaining to a given topic, e.g.,
   ``references vector`` will list all tasks that have the string
   “vector” in their one line description.

Glossary of New Tasks in the IRAF System Packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+------------+---------------------+-----------------------------------+
| Task       | Package             | Description                       |
+============+=====================+===================================+
| chpixtype  | images              | Change the pixel type of a list   |
|            |                     | of images                         |
+------------+---------------------+-----------------------------------+
| devices    | system              | Print information on the locally  |
|            |                     | available devices                 |
+------------+---------------------+-----------------------------------+
| imcombine  | images              | Combine images pixel-by-pixel     |
|            |                     | using various algorithms          |
+------------+---------------------+-----------------------------------+
| imslice    | images              | Slice images into images of lower |
|            |                     | dimension                         |
+------------+---------------------+-----------------------------------+
| imstack    | images              | Stack images into a single image  |
|            |                     | of higher dimension               |
+------------+---------------------+-----------------------------------+
| nsppkern   | plot                | Plot metacode on a NSPP (NCAR)    |
|            |                     | plotter device                    |
+------------+---------------------+-----------------------------------+
| pvector    | plot                | Plot an arbitrary vector in a 2D  |
|            |                     | image                             |
+------------+---------------------+-----------------------------------+
| references | system              | Find all help database references |
|            |                     | for a given topic                 |
+------------+---------------------+-----------------------------------+

In addition, there are new image display oriented tasks ``imexamine``,
``imedit``, and ``tvmark`` in the PROTO package in NOAO (used to
interactively examine and edit images, or draw graphics into image
display frames). These really belong in the core system but have been
placed in ``noao.proto`` since they are prototype tasks.

NOAO Package Revisions
----------------------

Some of the major revisions to the NOAO packages are listed below.

Summary of NOAO Packages Revisions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

New NOAO Packages
^^^^^^^^^^^^^^^^^

Several new packages have been added to the NOAO suite of packages.

-  The APPHOT package is a set of tasks for performing aperture
   photometry on uncrowded or moderately crowded stellar fields in
   either interactive or batch mode. This package is now installed in
   the DIGIPHOT package (IRAF version 2.7 and later). The APPHOT package
   was available as an add-on package to IRAF version 2.5 and later
   while it was undergoing alpha testing. Many new features have been
   added to the package since it first became available including the
   new task QPHOT (quick aperture photometry) and interaction with the
   image display cursor for supported image displays (Sun workstation,
   IIS model 70).

-  The CCDRED package provides tools for the easy and efficient
   reduction of CCD images. This package has been installed in the IMRED
   package (IRAF version 2.6 and later). The CCDRED package was also
   available as an add-on to IRAF version 2.5.

A short demonstration of many of the tasks in the CCDRED package is
provided with the DEMO task in the CCDRED.CCDTEST package.

-  The IMRED.ECHELLE package has been replaced with a more sophisticated
   collection of tasks for reducing echelle type data (IRAF version 2.7
   and later). The new ECHELLE package recognizes a new image format in
   which each extracted echelle order becomes a line in a two
   dimensional image rather than having a separate one dimensional
   spectrum for each order, although this old output format is still
   available as an option. Several new tasks exist for computing and
   applying a wavelength calibration to the data using the echelle
   relationship between the orders (ECIDENTIFY, ECREIDENTIFY, and
   ECDISPCOR) as well as for manipulating the new echelle format
   (ECSELECT, ECCONTINUUM, and ECBPLOT).

-  The IRRED package has been added to the IMRED package. The IRRED
   package collects together in one place those tasks used most
   frequently by users reducing IR data such as that taken with the IR
   imager at KPNO. The IRMOSAIC and IRALIGN tasks were available with
   IRAF version 2.6 and later. IRMOSAIC takes an ordered list of input
   images and places them on a grid in an output image. IRALIGN uses
   this grid and a coordinate list of overlapping objects from the
   individual subrasters to produce an aligned output image. The tasks
   IRMATCH1D and IRMATCH2D were available with IRAF version 2.7 and
   later. These tasks are similar to IRALIGN expect that the intensities
   of adjacent subrasters can be matched as well. A script called
   MOSPROC (IRAF version 2.8) has also been added that prepares a list
   of images for a quick look mosaic.

-  The MSRED package has been added to the IMRED package. The MSRED
   package is a collection of tasks used for reducing multispectral
   types of data, e.g. fiber arrays, where the individual spectra are
   for different objects. Like the ECHELLE package, it also has its own
   multispectral image format (a two dimensional image in which each
   line is an extracted spectrum). Several new tasks have been added to
   the package for wavelength calibration of multispectral data.

Modifications to Existing NOAO Packages
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-  The ASTUTIL package was reorganized (IRAF version 2.6 and later - see
   IRAF Newsletter No. 3 for details) and several tasks were added
   and/or modified. A new task ASTTIMES computes and prints astronomical
   dates and times given a local date and time. A new task RVCORRECT
   computes and prints radial velocity corrections for an observation.
   The tasks PRECESS and GALACTIC were modified slightly using different
   but more accurate algorithms.

The new task SETAIRMASS (IRAF version 2.8) computes the effective
airmass and middle UT of an exposure. This task was also made available
in the TWODSPEC and IMRED packages.

-  The two tasks in the IMRED.BIAS package, COLBIAS and LINEBIAS, were
   modified slightly (IRAF version 2.7 and later) so that the fitting
   parameters for the overscan region can be set by the user as hidden
   parameters to the tasks.

-  The task COSMICRAYS (from the CCDRED package) was made available in
   the IMRED.GENERIC package (IRAF version 2.6 and later).

-  A new task called SYNDICO has been added to the IMRED.VTEL package
   (IRAF version 2.6 and later). SYNDICO makes glossy prints on the NOAO
   Dicomed printer of the synoptic, full disk, magnetograms and
   spectroheliograms taken at the vacuum telescope at Kitt Peak.

-  Modifications were made to the IMRED.DTOI package. These changes have
   been documented in IRAF Newsletter No. 4.

-  Three new tasks in the ONEDSPEC package, REFSPECTRA, SEXTRACT, and
   SPECPLOT, were made available in the IMRED.COUDE, IMRED.IIDS,
   IMRED.IRS, and IMRED.SPECPHOT packages.

-  Many new tasks and features have been added to the ONEDSPEC package.

The SENSFUNC task was completely rewritten (IRAF version 2.6 and later)
to allow determination of extinction, display of flux calibrated
spectra, and many new features for displaying and manipulating the data.

IDENTIFY, REIDENTIFY and DISPCOR were modified (IRAF version 2.6 and
later) so that a dispersion solution from IDENTIFY could be shifted
without changing the original shape of the coordinate function (see IRAF
Newsletter No. 3 for details).

A new deblending algorithm was added to SPLOT (IRAF version 2.7 and
later). See the online help for SPLOT as well as the article in IRAF
Newsletter No. 4.

The tasks in the ONEDSPEC.ONEDUTIL package were absorbed into the
ONEDSPEC package (IRAF version 2.7 and later).

The EXTINCT task disappeared with its functionality being taken over by
a rewritten CALIBRATE (IRAF version 2.7 and later).

The COEFS task was moved to the IMRED.IIDS and IMRED.IRS packages since
this is a very instrument specific task (IRAF version 2.7 and later).

Three new tasks were added to the package. SEXTRACT (IRAF version 2.6
and later) extracts subspectra from one dimensional input spectra.
REFSPECTRA (IRAF version 2.7 and later) takes over part of the
functionality of the old DISPCOR task and allows the user to define
which arc spectra are to be used in the calculation of the dispersion
solution of object spectra. SPECPLOT (IRAF version 2.8) is a new
plotting task that allows the compression of many spectra to a page (see
IRAF Newsletter No. 6).

-  Several new tasks have been added to the PROTO package.

Four tasks were added to IRAF version 2.6 and later. BSCALE is a task
that can be used to linearly scale images by the mean, average, or mode
of the image. IRMOSAIC and IRALIGN can be used to combine many frames
into one large image. These three tasks are also available in the
IMRED.IRRED package. MKHISTOGRAM calculates the histogram of the data in
a text file.

Three new tasks were added to IRAF version 2.7 and later. IMSLICE is a
task that slices an image into images of lower dimension. IRMATCH1D and
IRMATCH2D are two tasks that allow combining of many overlapping images
while matching the background intensities in two different ways.

Three new tasks have been added to IRAF version 2.8 that allow the user
to interact with the image display (for supported display devices, ie
Sun workstation, IIS model 70). IMEXAMINE allows the user to
interactively examine portions of the displayed image. TVMARK allows the
user to mark objects on the image display. IMEDIT allows the user to
interactively edit an image.

-  The APEXTRACT package in the TWODSPEC package has had several rounds
   of modifications, as discussed in the IRAF Newsletters, No. 3 and 4.
   These changes included improved techniques and additional options for
   the extraction of data.

A new task, APSCATTER, has been added to the package (IRAF version 2.8).
This task determines and subtracts scattered light from two dimensional
aperture or echelle spectra. The task was also made available from
within the ECHELLE package. This task was discussed in IRAF Newsletter
No. 6.

Modifications and Additions to Calibration Data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The calibration data used by some of the tasks in the TWODSPEC,
ONEDSPEC, and many of the IMRED packages are kept in a directory called
ONEDSTDS in ``noao$lib``. The current contents of this directory are
best summarized by paging through its README file, e.g.,

::

   cl> page noao$lib/onedstds/README

Two additional line lists (used by IDENTIFY) have been added to this
directory (IRAF version 2.8). These lists, ``vacidhenear.dat`` and
``vacthorium.dat``, are simply the standard ``.dat`` files in air
wavelengths converted to vacuum wavelengths. The equation used for the
conversion as well as the appropriate reference in the literature are
contained in the README file.

The ``thorium.dat`` file has been updated to contain thorium lines from
3180 Angstroms to 9540 Angstroms (IRAF version 2.6 and later). Please
see the README file for the source.

Two new directories have been added containing flux information for
standard stars (IRAF version 2.6 and later): SPECHAYESCAL and SPEC50CAL.
Both of these lists are from Massey et al., 1988, Ap. J., Vol. 328,
p. 315.

Glossary of New Tasks in the NOAO Packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+------------+---------------------+-----------------------------------+
| Task       | Package             | Description                       |
+============+=====================+===================================+
| apsc       | apextract           | Fit and subtract scattered light  |
| atter [1]_ |                     |                                   |
+------------+---------------------+-----------------------------------+
| apselect   | apphot              | Extract select fields from apphot |
|            |                     | output files                      |
+------------+---------------------+-----------------------------------+
| asttimes   | astutil             | Compute UT, Julian day, epoch,    |
|            |                     | and sidereal time                 |
+------------+---------------------+-----------------------------------+
| b          | ccdred              | Create a bad pixel mask image     |
| adpiximage |                     | from a bad pixel file             |
+------------+---------------------+-----------------------------------+
| b          | proto               | Brightness scale images: new =    |
| scale [2]_ |                     | (old-bzero) / bscale              |
+------------+---------------------+-----------------------------------+
| c          | ccdred              | Discussion of CCD                 |
| cdgeometry |                     | coordinate/geometry keywords      |
+------------+---------------------+-----------------------------------+
| ccdgroups  | ccdred              | Group CCD images into image lists |
+------------+---------------------+-----------------------------------+
| ccdhedit   | ccdred              | CCD image header editor           |
+------------+---------------------+-----------------------------------+
| ccdlist    | ccdred              | List CCD processing information   |
+------------+---------------------+-----------------------------------+
| ccdproc    | ccdred              | Process CCD images                |
+------------+---------------------+-----------------------------------+
| ccdred     | ccdred              | CCD image reduction package       |
+------------+---------------------+-----------------------------------+
| ccdtypes   | ccdred              | Description of the CCD image      |
|            |                     | types                             |
+------------+---------------------+-----------------------------------+
| c          | apphot              | Compute accurate centers for a    |
| enter [3]_ |                     | list of objects                   |
+------------+---------------------+-----------------------------------+
| cente      | apphot              | Edit the centering parameters     |
| rpars [4]_ |                     |                                   |
+------------+---------------------+-----------------------------------+
| combine    | ccdred              | Combine CCD images                |
+------------+---------------------+-----------------------------------+
| cosmi      | ccdred              | Detect and replace cosmic rays    |
| crays [5]_ |                     |                                   |
+------------+---------------------+-----------------------------------+
| daofind    | apphot              | Find stars in an image using the  |
|            |                     | DAO algorithm                     |
+------------+---------------------+-----------------------------------+
| d          | ccdred              | Combine and process dark count    |
| arkcombine |                     | images                            |
+------------+---------------------+-----------------------------------+
| dat        | apphot              | Edit the data dependent           |
| apars [6]_ |                     | parameters                        |
+------------+---------------------+-----------------------------------+
| demo       | ccdtest             | Run a demonstration of the CCD    |
|            |                     | reduction package                 |
+------------+---------------------+-----------------------------------+
| ecbplot    | echelle             | Batch plots of echelle spectra    |
+------------+---------------------+-----------------------------------+
| e          | echelle             | Fit the continuum of echelle      |
| ccontinuum |                     | spectra                           |
+------------+---------------------+-----------------------------------+
| ecdispcor  | echelle             | Dispersion correct spectra        |
+------------+---------------------+-----------------------------------+
| ecidentify | echelle             | Identify features in spectrum for |
|            |                     | dispersion solution               |
+------------+---------------------+-----------------------------------+
| ec         | echelle             | Automatically reidentify features |
| reidentify |                     | in spectra                        |
+------------+---------------------+-----------------------------------+
| ecselect   | echelle             | Select and extract apertures from |
|            |                     | echelle spectra                   |
+------------+---------------------+-----------------------------------+
| fitpsf     | apphot              | Model the stellar psf with an     |
|            |                     | analytic function                 |
+------------+---------------------+-----------------------------------+
| fitsky     | apphot              | Compute sky values in a list of   |
|            |                     | regions                           |
+------------+---------------------+-----------------------------------+
| fitskypars | apphot              | Edit the sky fitting parameters   |
+------------+---------------------+-----------------------------------+
| f          | ccdred              | Combine and process flat field    |
| latcombine |                     | images                            |
+------------+---------------------+-----------------------------------+
| flatfields | ccdred              | Discussion of CCD flat field      |
|            |                     | calibrations                      |
+------------+---------------------+-----------------------------------+
| guide      | ccdred              | Introductory guide to using the   |
|            |                     | CCDRED package                    |
+------------+---------------------+-----------------------------------+
| imedit     | proto               | Examine and edit pixels in images |
+------------+---------------------+-----------------------------------+
| imexamine  | proto               | Examine images using image        |
|            |                     | display, graphics, and text       |
+------------+---------------------+-----------------------------------+
| imslice    | proto               | Slice images into images of lower |
|            |                     | dimension                         |
+------------+---------------------+-----------------------------------+
| i          | ccdred              | Instrument specific data files    |
| nstruments |                     |                                   |
+------------+---------------------+-----------------------------------+
| ir         | proto               | Align the mosaiced image produced |
| align [7]_ |                     | by irmosaic                       |
+------------+---------------------+-----------------------------------+
| irma       | proto               | Align and intensity match image   |
| tch1d [8]_ |                     | produced by irmosaic              |
+------------+---------------------+-----------------------------------+
| irma       | proto               | Align and intensity match image   |
| tch2d [9]_ |                     | produced by irmosaic              |
+------------+---------------------+-----------------------------------+
| irmo       | proto               | Mosaic an ordered list of images  |
| saic [10]_ |                     | onto a grid                       |
+------------+---------------------+-----------------------------------+
| m          | ccdred              | Make fringe correction images     |
| kfringecor |                     | from sky images                   |
+------------+---------------------+-----------------------------------+
| m          | proto               | List or plot the histogram of a   |
| khistogram |                     | data stream                       |
+------------+---------------------+-----------------------------------+
| mkillumcor | ccdred              | Make flat field iillumination     |
|            |                     | correction images                 |
+------------+---------------------+-----------------------------------+
| m          | ccdred              | Make iillumination corrected flat |
| killumflat |                     | fields                            |
+------------+---------------------+-----------------------------------+
| mkimage    | ccdtest             | Make or modify an image with      |
|            |                     | simple values                     |
+------------+---------------------+-----------------------------------+
| mkskycor   | ccdred              | Make sky iillumination correction |
|            |                     | images                            |
+------------+---------------------+-----------------------------------+
| mkskyflat  | ccdred              | Make sky corrected flat field     |
|            |                     | images                            |
+------------+---------------------+-----------------------------------+
| mosproc    | irred               | Prepare images for quick look     |
|            |                     | mosaicing                         |
+------------+---------------------+-----------------------------------+
| msdispcor  | msred               | Dispersion correct spectra        |
+------------+---------------------+-----------------------------------+
| ms         | msred               | Reidentify features from/to a     |
| reidentify |                     | multispec image                   |
+------------+---------------------+-----------------------------------+
| msselect   | msred               | Select and extract apertures from |
|            |                     | spectra                           |
+------------+---------------------+-----------------------------------+
| observe    | ccdtest             | Create an artificial CCD          |
|            |                     | observation                       |
+------------+---------------------+-----------------------------------+
| phot       | apphot              | Measure magnitudes for a list of  |
|            |                     | stars                             |
+------------+---------------------+-----------------------------------+
| photpars   | apphot              | Edit the photometry parameters    |
+------------+---------------------+-----------------------------------+
| polymark   | apphot              | Create polygon lists for polyphot |
+------------+---------------------+-----------------------------------+
| polypars   | apphot              | Edit the polyphot parameters      |
+------------+---------------------+-----------------------------------+
| polyphot   | apphot              | Measure magnitudes inside a list  |
|            |                     | of polygonal regions              |
+------------+---------------------+-----------------------------------+
| qphot      | apphot              | Measure quick magnitudes for a    |
|            |                     | list of stars                     |
+------------+---------------------+-----------------------------------+
| radprof    | apphot              | Compute the stellar radial        |
|            |                     | profile of a list of stars        |
+------------+---------------------+-----------------------------------+
| refspe     | onedspec            | Assign wavelength reference       |
| ctra [11]_ |                     | spectra to other spectra          |
+------------+---------------------+-----------------------------------+
| rvcorrect  | astutil             | Compute radial velocity           |
|            |                     | corrections                       |
+------------+---------------------+-----------------------------------+
| setair     | astutil             | Compute effective airmass for an  |
| mass [12]_ |                     | exposure                          |
+------------+---------------------+-----------------------------------+
| set        | ccdred              | Set instrument parameters         |
| instrument |                     |                                   |
+------------+---------------------+-----------------------------------+
| sext       | onedspec            | Extract subspectra from           |
| ract [13]_ |                     | dispersion corrected spectra      |
+------------+---------------------+-----------------------------------+
| spec       | onedspec            | Stack and plot multiple spectra   |
| plot [14]_ |                     |                                   |
+------------+---------------------+-----------------------------------+
| subsection | ccdtest             | Create an artificial subsection   |
|            |                     | CCD observation                   |
+------------+---------------------+-----------------------------------+
| subsets    | ccdred              | Description of CCD subsets        |
+------------+---------------------+-----------------------------------+
| syndico    | vtel                | Make dicomed print of daily grams |
|            |                     | 18 cm across                      |
+------------+---------------------+-----------------------------------+
| tvmark     | proto               | Mark objects on the image display |
+------------+---------------------+-----------------------------------+
| wphot      | apphot              | Measure magnitudes for a list of  |
|            |                     | stars with weighting              |
+------------+---------------------+-----------------------------------+
| z          | ccdred              | Combine and process zero level    |
| erocombine |                     | images                            |
+------------+---------------------+-----------------------------------+

.. [1]
   Tasks also in echelle and msred packages.

.. [2]
   Tasks also in irred package.

.. [3]
   Tasks also in irred package.

.. [4]
   Tasks also in irred package.

.. [5]
   Tasks also in generic package.

.. [6]
   Tasks also in irred package.

.. [7]
   Tasks also in irred package.

.. [8]
   Tasks also in irred package.

.. [9]
   Tasks also in irred package.

.. [10]
   Tasks also in irred package.

.. [11]
   Tasks also in coude, echelle, iids, irs, msred, and specphot
   packages.

.. [12]
   Tasks also in imred and twodspec packages.

.. [13]
   Tasks also in coude, iids, irs, and specphot packages.

.. [14]
   Tasks also in coude, echelle, iids, irs, msred, and specphot
   packages.
