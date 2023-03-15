IRAF V2.15 Release Notes
========================

:Authors: IRAF Group
:Date: November 22, 2010 (revised February 24, 2011)
:Abstract: These notes provide a summary of the major changes in the V2.15 release.
 IRAF V2.15 is a major release of the IRAF system for all supported
 platforms. The main feature of this release is support for 64-bit Linux
 and Mac OSX systems, although there are numerous other enhancements as
 well. Because changes to the system interfaces were fairly substantial,
 and extended test release was made available to the community to gather
 feedback about potential problems in the science code. There were only a
 few reported problems and we now consider the system to be stable for
 general release and recommended for all users.

 More detailed technical documentation of all system changes will be
 found in the ‘notes.v214’ file in the ``iraf$local`` directory, detailed
 revisions notes for each application package are in the package
 directories in a file called Revisions, e.g. onedspec Revisions.
 Please see also the ‘sysnotes.v215’ file in this directory for a
 detailed list of the V2.15 changes, and similar files for the detailed
 revisions of each patch.

Highlights of This Release:
---------------------------

64-bit Platform Binary Support
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Linux and Mac OSX systems running 64-bit operating systems are now
supported in native 64-bit binary mode. There are currently NO plans to
support 64-bit versions of other operating systems (e.g. Windows,
Solaris, or FreeBSD).

New MEMIO Interface
~~~~~~~~~~~~~~~~~~~

The MEMIO interface was necessarily changed to accomodate the 64-bit
upgrade, specifically we added extra structure to a memory object to aid
in debugging (e.g. sentinal values before/after a segment, a reporting
mechanism etc). This adds a slight bit of overhead to each heap memory
allocation, but we consider this to be minor given the capabilities of
today’s systems. A suite of environment variables can now also be set to
manage/monitor this new interface (see below).

Single ‘linux’ Architecture for 32-bit Systems
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is part of the overall architecture changes implemented in v2.15
systems. The original logic for separate ‘suse’, ‘redhat’ and ‘linux’
architectures was due to differences between these platforms in the
GCC/GLibc systems used that were inherently incompatibile. These
differences have since become irrelevant and now impose a burden to
support and maintain separate platforms, and so a common ‘linux’
architecture is now preferred.

Because external packages may lag in the changes needed to switch
architectures, we try to support legacy references to e.g. ‘redhat’ as
an alias to the new ‘linux’ structure and hope that the previous
deprecation (in v2.14) of ‘suse’ has now been removed entirely. Since
64-bit support requires a new architecture name (i.e. ‘linux’ versus
‘linux64’) there will be no confusion between the 32- and 64-bit
versions of systems.

Mac OSX Architecture Changes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In previous IRAF releases the ‘macintel’ archicture was used for all OSX
versions running on Intel hardware, and ‘macosx’ was reserved for
PowerPC (PPC) CPUs. In order to avoid yet another IRAF architecture name
to distinguish 64 and 32-bit Intel systems, and because OSX systems are
moving generally towards Intel CPUs with 64-bit capabilities anyway, we
decided that future IRAF systems will favor 64-bit Intel architectures
as users upgrade machines.

What this means is that starting with v2.15, the IRAF ‘macintel’
architecture will now refer exclusively to 64-bit Intel hardware. In
order to retain compatibility with older systems, the ‘macosx’ IRAF
architecture will encompass 32-bit binaries for both PPC and Intel CPUs
in a Universal binary format.

This is a departure from past platform architecture naming conventions
where a new unique name is used, however future MacOSX systems will all
be Intel-based and hardware will be 64-bit capable so shifting the focus
to ‘macintel’ being the primary platform and ‘macosx’ supporting older
systems made the most sense. (In hindsight, we would have preferred to
be able to use ‘macosx’ as being more descriptive).

The ‘macosx’ architecture now includes both PPC and Intel binaries in
the system libraries (and those produced for external packages) and
executables. These binaries are compatible for OSX 10.4 and later
systems and the system will automatically use the appropriate binary for
the hardware (e.g. on an OSX 10.4 PowerPC machine the PPC binary for the
‘macosx’ arch will execute the PPC binary in the iraf executable file).

While earlier IRAF versions would allow that PPC ‘macosx’ binaries could
be run on Intel systems using the Rosetta sytem, this isn’t currently
allowed with v2.15 as an Intel system would automatically use the Intel
binary. Thus, to test 32-bit PPC binaries you will need a machine with a
PPC CPU. This is seen as something that would affect a minimal number of
users.

Compile-Time VOS/Kernel Prototype Checking
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All library code in the IRAF ‘Virtual Operating Interface’ (VOS) and
IRAF Kernal (i.e. the $iraf/unix//os procedures) now have function
prototypes that are checked during compilation of any SPP code.

Intra-Package Prototype Checking
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a feature that will be added in the near future and will provide
the same level of prototype checking as with the core system libraries.
The idea is that a ‘libpkg.a’ will produce a local prototype file that
ensures all calls within the package meet the prototype calls. As with
VOS/Kernel checking, this will validate the code within a package to
trap any potential errors.

All C Library Code Converted to Clean ANSI-C
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As part of the 64-bit port, all C library code in the VOS and kernel was
converted from the old-style “K&R” style to true ANSI C with full
function prototypes. Additionally, the code was cleaned up to remove any
warnings generated by the compiler, i.e. compilation with “-Wall” under
GCC now compiles cleanly to further ensure the integrity of the code.

Cross-Compilation of Architectures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Compilation is now defined by the user environment rather than the
machine used to do the build, as before. What this means is that (within
reason) it is now possible to cross-compile for a different IRAF
architecture on machines which support such compilation. For instance,
on a 64-bit Mac OSX Snow Leopard system (where the native IRAF
architecture is ‘macintel’) one can now compile code for the 32-bit
‘macosx’ architecture (either PPC or Intel) by simply setting the
‘IRAFARCH’ environment varable as ‘macosx’ and reconfiguring the
system/package approriately.

The same is true for Linux 64-bit systems and ‘linux64’ and ‘linux’
architectures, meaning one doesn’t need access to machines to produce
binaries. Compilation flags are set auomatically based on the IRAFARCH
variable in order to produce the desired binaries.

User-Selectable Architectures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As with compilation, the IRAFARCH variable can be used to determine
which binaries to run. For example, on a 64-bit Linux system (where the
default arch would be ‘linux64’) the commands

::

   % setenv IRAFARCH linux
   % cl

would allow a user to run the 32-bit ‘linux’ binaries and thus switch
easily between systems to compare results and validate the science
results of a reduction/analysis task. The same is true for Mac (Intel)
systems where the 64-bit macintel and 32-bit macosx architectures could
be selected by the user.

SVG Graphics Device for Better Web Presentation of Plots
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

SVG graphics are an XML format supported by many modern browsers that
permit scalable graphics for web presentation. This means that plots
produced by IRAF may be used in web pages without loss of resolution or
aliasing effects seen when using e.g. GIF images of a plot.

A new ‘g-svg’ graphcap device was added to make use of this new driver.
It produces a file called ‘sgiXXXX.svg’ in the current working directory
(where ‘XXXX’ is a process id) and may be used as e.g.

::

   cl> contour dev$pix dev=g-svg

or in cursor mode with a “:.snap g-svg”. Either instance should be
followed with a ‘gflush’ or ‘:.gflush’ to flush to output to disk. SVG
files can be embedded into HTML documents with the tag, the tag, or the
tag.

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
   noao            compile the NOAO package
   summary         print core/noao/tables spool file summaries
   showarch        show currently configure arch
   <arch>          reconfigure for named architecture

Simplified Download/Install Process
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This release introduces a change in the distribution model used for
IRAF. Specifically, we recognize the majority of users will be running
on single-user machines such as personal laptops or desktop systems (the
earlier distribution model was based on the idea of a central server
supporting many client machines). As such, the need for separate source
and binary distributions and a separation of these in the directory
hierarchy is now unnecessarily complicated for most users. While the
previous form of the distribution files are still available, the
preferred method is to use the single-file distributions. All
distribution files include full source, differences are in which
binaries are also included.

Simplified External Package Installation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Dynamic package loading is a new feature in v2.15 that allows for
package directories created in the ``iraf$extern`` directory to be
automatically defined when the CL is started. The means that external
package installation no longer *requires* that the ``hlib$extern.pkg``
file be edited to define the package, although that remains an option
for packages which somehow cannot conform to this new scheme.

COLOR and VOL packages now part of Core System
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Both the COLOR and VOL external packages have been incorporated into the
PROTO package. These packages are no longer being developed but are
required by some users so will continue to be supported as prototype
software.

Improved Documentation
~~~~~~~~~~~~~~~~~~~~~~

Thanks to Jason Quinn, the help pages for dozens of tasks have been
cleaned of long-standing typo and formatting errors. In many cases
Jason’s suggestions have also served to clarify the meaning of the text,
or correct the help wrt to how to task actually operates.

Platform Support:
-----------------

IRAF V2.15 supports only the following platforms:

-  PC-IRAF

   -  supports RedHat 9 thru Fedora/RHEL/Centos (LNUX)
   -  supports Mac OS X 10.4 and higher (ppc and intel) (MACX)
   -  supports Debian 3.1 and higher (LNUX)

   (The following platforms are planned but not yet available:)

   -  supports FreeBSD 6.3 and higher (FBSD)
   -  supports Solaris 10 (x86) (SSOL)
   -  supports Cygwin (Windows XP and Vista) (CYGW)

-  Sun/IRAF

   -  supports SunOS 4.1 (SOS4)
   -  supports Solaris 5.5.1 thru Solaris 10 (SSUN)

Note that PC platforms not mentioned here specifically may still be
supported by one or more of the distributions (e.g. Ubuntu can use
LNUX).

CORE IRAF REVISIONS SUMMARY
---------------------------

This section describes changes to tasks in the IRAF core system other
than routine bug fixes.

New Tasks
~~~~~~~~~

-  images.imcoords:

   -  hpctran - Convert between HEALPix row and spherical coordinate

-  proto:

   -  mkglbhdr - Make global header from keywords in images and
      reference

-  system:

   -  bench - Demonstration benchmark task

Existing Tasks with New Parameters or New Parameter Defaults
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  images.imfit.fit1d

   -  Adds new ‘bpm’ bad pixel mask parameter

-  images.imutil.nhedit

   -  A ‘rename’ parameter switch was added to renaming a keyword.

Existing Tasks with New Capabilities
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  images.immatch.imcombine: New ‘quadrature’ and ‘nmodel’ options to
   the ‘combine’ parameter are used for error propagation either with
   input sigma images (quadrature) or where the pixel sigmas may be
   computed by the noise model used by this task (nmodel).

NOAO PACKAGE REVISIONS SUMMARY
------------------------------

This section describes changes to tasks in the NOAO package tasks other
than routine bug fixes.

New NOAO Package Tasks
~~~~~~~~~~~~~~~~~~~~~~

-  noao.nproto:

   -  skygroup: Group a list containing RA and Dec into spatial sublists
   -  skysep: Compute arc separation of two RA/Dec values

Existing Packages and Tasks with New Parameters or New Parameter Defaults
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  artdata.mkpattern: Added ‘long’ to allowed list of data types.

-  onedspec.specplot: Added new ‘transform’ parameter to allow scaling
   the spectrum pixel values. Currently only ‘log’ is implemented.

-  twodspec.apextract.apall: Changed default for ’maxsep from 1000 to
   100000.

.. _existing-tasks-with-new-capabilities-1:

Existing Tasks with New Capabilities
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  astcat.agetcat Added usnob1@usno, usnoa2@usno, nomad@usno and
   act@usno

-  imred.doecslit, imred.doefoe, imred.doslit: Changed the default
   maxsep from 1000 to 100000. Unless users reset the default their
   expectation is that marking apertures will not skip an aperture
   number no matter how far apart the aperturers are.

-  fibers.skysub.cl: Added ‘sum’ as an enumerated “combine” choice.

-  nproto.skysep: Added an ‘enum’ to the ‘raunit’ param to enforce
   choices.

-  obsutil.sptime: Made all graphs auto-scale. Added a “generic”
   disperser type to force using the desired wavelength and dispersion
   without defining a grating whose mapping between position on the
   detector and wave- length might be wrong.

-  twodspec.apextract: The ‘s’ key now works on the current aperture
   rather than the nearest.

BUG LOGS FIXED BY THIS RELEASE
------------------------------

The following buglog entries are fixed by the this V2.15 release:

::

   NUMBER: 567
   MODULE: apall, apedit
   SYSTEM: V2.11-V2.14.1
   DATE:   Tue Oct  7 10:53:14 MST 2008
   FROM:   valdes

   BUG:    The :parameters and :apertures commands cause the task to exit with
       an error that parameter "apertures" isn't found.  This problem has
       existed for a long time due to a missing parameter in the hidden
       parameter sets.

   STATUS: Fixed for the next release.

::

   NUMBER: 568
   MODULE: imcombine
   SYSTEM: -V2.14.1
   DATE:   Tue Oct  7 12:52:35 MST 2008
   FROM:   valdes

   BUG:    When using avsigclip, ccdclip, or sigclip rejection around the
       median (mclip=yes) the resulting final median may be incorrect.
       This will generally only occur if unusually small low sigma values,
       such as lsigma=1, are used.  This was due to using a wrong
       variable.

   STATUS: This is fixed for the next release.

::

   NUMBER: 570
   MODULE: imexpr, mskexpr, or tasks other asks using the expression evaluator
   SYSTEM: -V2.14.1
   DATE:   Mon Nov  3 22:16:41 MST 2008
   FROM:   valdes

   BUG:    Use of the built-in functions mod, min, max, and median produce an
       "incompatible types" error even though the types of the arguments are
       correct.  This is a due to a coding error.  There is no workaround
       other than using alternative ways to express the desired
       expression.

   STATUS: Fixed for the next release.

::

   NUMBER: 571
   MODULE: IMAGES.IMUTIL.HSELECT
   SYSTEM: V2.14
   DATE:   Fri Jan  2 21:41:53 MST 2009
   FROM:   fitz

   BUG:    The use of a '$' in a field name was causing the 'missing' value
       to always be printed even if the field exists in the image.  This
       was caused by a failure to check for the character and removing it
       prior to getting the value from the header.  There is no workaround,
       the code change is trivial.

   STATUS: Fixed for the next release.

::

   NUMBER: 573
   MODULE: mscimage
   SYSTEM: - V4.9 August, 2008
   DATE:   Fri Sep 18 08:36:30 MST 2009
   FROM:   valdes (discovered and diagosed by Thomas de Boer)

   BUG:    The task ignores the parameters "boundary" and "blank" which are
       fixed to be "constant" and "0." respectively.

   STATUS: This is fixed for the next release.
