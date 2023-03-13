IRAF 2.17 Release notes
=======================

:Authors: IRAF Community
:Date: January 07, 2012

The current IRAF version 2.17 is available from github at

https://github.com/iraf-community/iraf/releases/latest/

Changes to the NOAO 2.16.1 sources include:

-  **Community maintainance**

   IRAF is no longer maintained by NOAO, but by the community of
   volunteers. Contributions of bug fixes, documentation or improvements
   are welcome.

-  **All known non-free code removed**

   Although IRAF 2.16.1 was claimed to be “free software”, `it contained
   source code that is not freely
   distributable <https://iraf-community.github.io/iraf-v216/license-problems>`__; namely code copied
   from the book `“Numerical Recipes in
   Fortran” <http://numerical.recipes/>`__. This code is replaced with
   free equivalents. The IRAF community edition is `Open
   Source <https://opensource.org/docs/osd>`__, and as such included in
   Debian.

-  **IRAF ported to other architectures**

   IRAF is now ported to a number of little endian architectures (ARM,
   PowerPC, MIPS, x32, RISC-V64) and operating systems (GNU Hurd and
   FreeBSD). Specifically, the Mac M1 processor is supported now.

-  **Major bug fixes**

   Many bugs of the 2.16.1 release are fixed. Some of he major ones are:

   -  Linux systems crashed with “Out of memory” (`2.12 release
      notes <https://github.com/iraf-community/iraf/blob/9590f4/doc/notes.v212#L1065-L1075>`__)
   -  ``noao.digiphot.photcal.fitparams`` failed with a segmentation
      fault on 64-bit systems
      (`iraf.net#1467834 <https://iraf.net/forum/viewtopic.php?showtopic=1467834>`__)
   -  The system wide IRAF installation changed the permissions of
      ``/tmp/``, creating a major security hole in the system
      (`iraf-v216#23 <https://iraf-community.github.io/iraf-v216/issues/23>`__)
   -  On Linux systems, self-compiled tasks gave wrong results
      (`iraf.net#1467841 <https://iraf.net/forum/viewtopic.php?showtopic=1467841>`__)
   -  On modern systems, background execution did not work
      (`iraf.net#1467431 <https://iraf.net/forum/viewtopic.php?showtopic=1467431>`__)
   -  The original code produced errornous executables when build on
      Linux versions later than 2012. It also did not build from
      scratch, but required an already compiled IRAF version.

-  **Simple CI test framework added**

   The tests are defined and documented in
   `MarkDown <https://github.com/iraf-community/iraf/blob/main/test/README.md>`__
   files. Tests are run with Github Actions on Linux and MacOS X
   platforms.

-  **VO package and vocl removed**

   The VO package, and the vocl shell heavily depend on a number of Java
   jars, where the creation from sources is undocumented. The package
   also uses outdated VO standards. A discussion with Mike Fitzpatrick
   resulted in his plan to `move the VO functionality into an external
   package <https://iraf-community.github.io/iraf-v216/issues/90>`__. Therefore, no attempt was put into
   getting these problems fixed, and the VO stuff was cut out. The
   VOTable functionality, however, remains available

Detailed list of changes
------------------------

This list shows all pull requests that were merged since 2.16.1.

Since 2.16.1+2021.06.14
~~~~~~~~~~~~~~~~~~~~~~~

-  Consistently format doc/help examples
   (`#195 <https://github.com/iraf-community/iraf/pull/195>`__)
-  Fix some HTML help output glitches
   (`#194 <https://github.com/iraf-community/iraf/pull/194>`__)
-  Remove duplicate argument in call
   (`#189 <https://github.com/iraf-community/iraf/pull/189>`__)
-  Revert corruption of unix/os/net/rexec.c file
   (`#180 <https://github.com/iraf-community/iraf/pull/180>`__)
-  Force using POSIX shell in extpkg.cl script
   (`#179 <https://github.com/iraf-community/iraf/pull/179>`__)
-  Support freeBSD variants
   (`#174 <https://github.com/iraf-community/iraf/pull/174>`__)
-  Separate development (softools) packages
   (`#172 <https://github.com/iraf-community/iraf/pull/172>`__)
-  Remove obsolete tasks and links to iraf.noao.edu
   (`#170 <https://github.com/iraf-community/iraf/pull/170>`__)

Since 2.16.1+2018.11.01
~~~~~~~~~~~~~~~~~~~~~~~

-  Cleanup for unneeded and obsolete files
   (`#166 <https://github.com/iraf-community/iraf/pull/166>`__)
-  fix slalib bug in sla_EQEQX
   (`#160 <https://github.com/iraf-community/iraf/pull/160>`__)
-  Ignore existing iraf env var in ./install
   (`#157 <https://github.com/iraf-community/iraf/pull/157>`__)
-  Cleanup ecl and cl
   (`#156 <https://github.com/iraf-community/iraf/pull/156>`__)
-  Add macOS arm64 support
   (`#131 <https://github.com/iraf-community/iraf/pull/131>`__)
-  Replace hard-coded host$bin paths by IRAFPATH
   (`#128 <https://github.com/iraf-community/iraf/pull/128>`__)
-  Remove include/drvrsmem.h
   (`#126 <https://github.com/iraf-community/iraf/pull/126>`__)
-  Fix cpu_time calculation in unix/os/zgtime.c
   (`#118 <https://github.com/iraf-community/iraf/pull/118>`__,
   `#136 <https://github.com/iraf-community/iraf/pull/136>`__,
   `#173 <https://github.com/iraf-community/iraf/pull/173>`__)
-  Move zsvjmp assembler files to unix/os and merge them
   (`#117 <https://github.com/iraf-community/iraf/pull/117>`__)
-  Use PLT when calling sigsetjmp on i386
   (`#116 <https://github.com/iraf-community/iraf/pull/116>`__)
-  Adjust external licenses
   (`#115 <https://github.com/iraf-community/iraf/pull/115>`__)
-  Definitely use flex to generate ``unix/generix/lexyy.c``
   (`#112 <https://github.com/iraf-community/iraf/pull/112>`__)
-  Avoid multiple definition of ``errflag``
   (`#111 <https://github.com/iraf-community/iraf/pull/111>`__)
-  Enable the use of Public Domain Ratfor to process ``.r`` files
   (`#103 <https://github.com/iraf-community/iraf/pull/103>`__,
   `#171 <https://github.com/iraf-community/iraf/pull/171>`__)
-  Remove some C compiler warnings
   (`#97 <https://github.com/iraf-community/iraf/pull/97>`__)
-  Fix non-working fft841 code by replacing it
   (`#95 <https://github.com/iraf-community/iraf/pull/95>`__)
-  Add LAPACK license
   (`#88 <https://github.com/iraf-community/iraf/pull/88>`__)
-  Rename ``mkfloat.sh`` to ``mkfloat``
   (`#87 <https://github.com/iraf-community/iraf/pull/87>`__)
-  Add support for the DEC Alpha processor
   (`#79 <https://github.com/iraf-community/iraf/pull/79>`__)
-  Fix and improve the shell scripts
   (`#75 <https://github.com/iraf-community/iraf/pull/75>`__,
   `#76 <https://github.com/iraf-community/iraf/pull/76>`__,
   `#77 <https://github.com/iraf-community/iraf/pull/77>`__,
   `#85 <https://github.com/iraf-community/iraf/pull/85>`__,
   `#86 <https://github.com/iraf-community/iraf/pull/86>`__,
   `#113 <https://github.com/iraf-community/iraf/pull/113>`__)

Since 2.16.1+2018.06.15
~~~~~~~~~~~~~~~~~~~~~~~

-  Add riscv64 support
   (`#72 <https://github.com/iraf-community/iraf/pull/72>`__)
-  Fix buffer length in ``urlget.x``
   (`#70 <https://github.com/iraf-community/iraf/pull/70>`__)
-  Mention Chisato Yamauchi as copyright owner of the x86_64
   ``zsvjmp.s`` code
   (`#67 <https://github.com/iraf-community/iraf/pull/67>`__)
-  Adjust calling of nttools subdir in ``pkg/utilities/mkpkg``
   (`#65 <https://github.com/iraf-community/iraf/pull/65>`__)
-  Update and modernize top-level information files
   (`#64 <https://github.com/iraf-community/iraf/pull/64>`__,
   `#73 <https://github.com/iraf-community/iraf/pull/73>`__)
-  Check for the existence of the ``arch`` variable before using it
   (`#63 <https://github.com/iraf-community/iraf/pull/63>`__)
-  Improve prototyping in bootlib
   (`#62 <https://github.com/iraf-community/iraf/pull/62>`__)
-  Appended ``ZTTYSZ()`` function to get width and height of terminal
   (`#58 <https://github.com/iraf-community/iraf/pull/58>`__)
-  Replace readline library by libedit on macos
   (`#57 <https://github.com/iraf-community/iraf/pull/57>`__)
-  Clean and fix shell scripts
   (`#50 <https://github.com/iraf-community/iraf/pull/50>`__,
   `#51 <https://github.com/iraf-community/iraf/pull/51>`__,
   `#53 <https://github.com/iraf-community/iraf/pull/53>`__,
   `#54 <https://github.com/iraf-community/iraf/pull/54>`__,
   `#55 <https://github.com/iraf-community/iraf/pull/55>`__,
   `#75 <https://github.com/iraf-community/iraf/pull/75>`__,
   `#76 <https://github.com/iraf-community/iraf/pull/76>`__,
   `#77 <https://github.com/iraf-community/iraf/pull/77>`__)
-  Fix variable declaration in noao/obsutil/src/findgain.cl
   (`#47 <https://github.com/iraf-community/iraf/pull/47>`__)
-  Remove unused empty files
   (`#45 <https://github.com/iraf-community/iraf/pull/45>`__)
-  Add manpages
   (`#44 <https://github.com/iraf-community/iraf/pull/44>`__)
-  Update cfitsio to 3.450
   (`#43 <https://github.com/iraf-community/iraf/pull/43>`__)
-  votable: Fix data type of loop variable
   (`#41 <https://github.com/iraf-community/iraf/pull/41>`__)

Since 2.16.1+2018.03.10
~~~~~~~~~~~~~~~~~~~~~~~

-  Implement the ‘apropos’ command
   (`#37 <https://github.com/iraf-community/iraf/pull/37>`__)
-  Don’t check for updates
   (`#36 <https://github.com/iraf-community/iraf/pull/36>`__)
-  Update cfitsio to 3.440
   (`#34 <https://github.com/iraf-community/iraf/pull/34>`__)
-  Fix background execution in cl and ecl
   (`#32 <https://github.com/iraf-community/iraf/pull/32>`__)
-  Port IRAF to several architectures
   (`#31 <https://github.com/iraf-community/iraf/pull/31>`__)

Since 2.16.1+2018.02.04
~~~~~~~~~~~~~~~~~~~~~~~

(Pull Requests from `iraf/iraf-v216 <https://iraf-community.github.io/iraf-v216>`__)

-  Update cfitsio to 3.430 (`#135 <https://iraf-community.github.io/iraf-v216/issues/135>`__)
-  Fix off-by-one problem in xppcode.c (`#133 <https://iraf-community.github.io/iraf-v216/issues/133>`__)
-  Remove files that were generated by ``generic.e`` or ``xyacc.e``
   (`#132 <https://iraf-community.github.io/iraf-v216/issues/132>`__)

Since 2.16.1+2017.12.28
~~~~~~~~~~~~~~~~~~~~~~~

(Pull Requests from `iraf/iraf-v216 <https://iraf-community.github.io/iraf-v216>`__)

-  Make photcal 64-bit capable (`#130 <https://iraf-community.github.io/iraf-v216/issues/130>`__)
-  f2c: Fix allocated size of Dimblock (`#127 <https://iraf-community.github.io/iraf-v216/issues/127>`__)

Since 2.16.1
~~~~~~~~~~~~

(Pull Requests from `iraf/iraf-v216 <https://iraf-community.github.io/iraf-v216>`__)

-  Check filepointer for ``NULL`` in ``envinit`` before trying to close.
   (`#126 <https://iraf-community.github.io/iraf-v216/issues/126>`__)
-  Add the required credits for the IRAF64 project.
   (`#125 <https://iraf-community.github.io/iraf-v216/issues/125>`__)
-  Use ``strncpy`` and ``snprintf`` to fill file header in wtar
   (`#124 <https://iraf-community.github.io/iraf-v216/issues/124>`__)
-  Specify explicit format for ``fprintf()``
   (`#123 <https://iraf-community.github.io/iraf-v216/issues/123>`__)
-  Limit number of ``finfo`` structs returned by ``KI_ZFINFO`` to
   ``MAX_ARGS`` (`#122 <https://iraf-community.github.io/iraf-v216/issues/122>`__)
-  Fix ``isalnum()`` and friends for non-ascii values
   (`#121 <https://iraf-community.github.io/iraf-v216/issues/121>`__)
-  Use curl in ``pkgget`` (`#115 <https://iraf-community.github.io/iraf-v216/issues/115>`__)
-  Fix comparison for some optional command line arguments in xc
   (`#111 <https://iraf-community.github.io/iraf-v216/issues/111>`__)
-  Add a trailing ``\0`` to the end of variable format strings in
   ``pkg/tbtables/fitsio/`` (`#110 <https://iraf-community.github.io/iraf-v216/issues/110>`__)
-  Fix OS dirnames in ``README`` (`#108 <https://iraf-community.github.io/iraf-v216/issues/108>`__)
-  Adjust f2c’s internal ``integer`` size for ILP64
   (`#107 <https://iraf-community.github.io/iraf-v216/issues/107>`__)
-  Replace ``d1mach.f`` and ``r1mach.f`` by C sources
   (`#106 <https://iraf-community.github.io/iraf-v216/issues/106>`__)
-  Handle negative pointers in ``sys/nmemio``
   (`#105 <https://iraf-community.github.io/iraf-v216/issues/105>`__)
-  Remove all executables and binaries in ``make src``
   (`#104 <https://iraf-community.github.io/iraf-v216/issues/104>`__)
-  *[linux64]* Correct underlines in ``mem`` symbol in ``zsvjmp.s``
   (`#102 <https://iraf-community.github.io/iraf-v216/issues/102>`__)
-  Correct string length of ``baseurl`` initialization in
   ``chkupdate.x`` (`#101 <https://iraf-community.github.io/iraf-v216/issues/101>`__)
-  Fix segfault when opening a ``STRING_FILE``
   (`#100 <https://iraf-community.github.io/iraf-v216/issues/100>`__)
-  Fix statement order in ``vfn_expand_ldir``
   (`#99 <https://iraf-community.github.io/iraf-v216/issues/99>`__)
-  Fix linenumber generation with ``xpp -x`` (rpp and spp))
   (`#98 <https://iraf-community.github.io/iraf-v216/issues/98>`__)
-  Fix template expansion in ``generic.c``
   (`#94 <https://iraf-community.github.io/iraf-v216/issues/94>`__)
-  Remove VO related packages and libraries
   (`#93 <https://iraf-community.github.io/iraf-v216/issues/93>`__,
-  Initialize ``oscwd`` in ``zglobl.c`` (`#91 <https://iraf-community.github.io/iraf-v216/issues/91>`__)
-  Check for identical addresses before ``strcpy()`` in ``mkpkg/tok.c``
   (`#89 <https://iraf-community.github.io/iraf-v216/issues/89>`__)
-  Fix type of arguments for several procedure calls
   (`#88 <https://iraf-community.github.io/iraf-v216/issues/88>`__)
-  Bugfix for ``unix/os/gmttolst.c`` and ``unix/zgmtco.c``
   (`#87 <https://iraf-community.github.io/iraf-v216/issues/87>`__)
-  Fix location of ``yaccpar.x`` (`#84 <https://iraf-community.github.io/iraf-v216/issues/84>`__)
-  *[macosx]* Fix syntax error in ``readline/mkpkg`` on macosx
   (`#83 <https://iraf-community.github.io/iraf-v216/issues/83>`__)
-  Remove absolute paths from header (`#82 <https://iraf-community.github.io/iraf-v216/issues/82>`__)
-  Reverse the condition when iraf should be set
   (`#81 <https://iraf-community.github.io/iraf-v216/issues/81>`__)
-  *[macosx]* Fix MacOSX min version on ``zsvjmp_i386.s``
   (`#80 <https://iraf-community.github.io/iraf-v216/issues/80>`__)
-  Fix lex source files in xpp and generic
   (`#79 <https://iraf-community.github.io/iraf-v216/issues/79>`__)
-  *[macintel]* Replace ``setpgrp(...)`` with POSIX ``setpgid()``
   (`#78 <https://iraf-community.github.io/iraf-v216/issues/78>`__)
-  Avoid identical src/target in ``strcpy()`` when creating library
   names in xc (`#77 <https://iraf-community.github.io/iraf-v216/issues/77>`__)
-  *[linux]* Consequently add ``-m32`` flags if compiling for linux(32))
   (`#76 <https://iraf-community.github.io/iraf-v216/issues/76>`__)
-  Convert to ANSI C to fix return types of functions in ``memlog.c``
   (`#75 <https://iraf-community.github.io/iraf-v216/issues/75>`__)
-  Limit entries in bitmask to 64 bit. (`#74 <https://iraf-community.github.io/iraf-v216/issues/74>`__)
-  Accept zero date in archives (`#71 <https://iraf-community.github.io/iraf-v216/issues/71>`__)
-  Fix computation of offset in memory allocation at 32 bit
   (`#67 <https://iraf-community.github.io/iraf-v216/issues/67>`__)
-  Fix ``ADDR_TO_LOC`` for i386 (32 bit))
   (`#62 <https://iraf-community.github.io/iraf-v216/issues/62>`__)
-  Fix declaration of ``cdsmem`` in rpp (`#60 <https://iraf-community.github.io/iraf-v216/issues/60>`__)
-  Force iraf to align on 128-bit boundaries
   (`#57 <https://iraf-community.github.io/iraf-v216/issues/57>`__)
-  Remove ``curl/types.h`` includes (`#51 <https://iraf-community.github.io/iraf-v216/issues/51>`__)
-  Fixed spelling error, “the” not “teh”.
   (`#47 <https://iraf-community.github.io/iraf-v216/issues/47>`__)
-  *[linux64]* Call the PLT for ``__sigsetjmp`` instead of calling
   directly (`#45 <https://iraf-community.github.io/iraf-v216/issues/45>`__)
-  Removed an extra ``linux64`` (`#44 <https://iraf-community.github.io/iraf-v216/issues/44>`__)
-  Build vendor libs before starting the ``NOVOS`` build
   (`#40 <https://iraf-community.github.io/iraf-v216/issues/40>`__)
-  Fixed recursive error in definition of ``LFLAGS``
   (`#39 <https://iraf-community.github.io/iraf-v216/issues/39>`__)
-  Convert ``mklibs`` to ``/bin/sh`` (`#38 <https://iraf-community.github.io/iraf-v216/issues/38>`__)
-  Replace or remove non-free code (Numerical Recipes etc.))
   (`#37 <https://iraf-community.github.io/iraf-v216/issues/37>`__)
-  Add continious integration testing with travis-CI
   (`#36 <https://iraf-community.github.io/iraf-v216/issues/36>`__)
-  Replace absolute symlinks in sys/osb by relative ones
   (`#33 <https://iraf-community.github.io/iraf-v216/issues/33>`__)
-  Don’t remove sticky bit from /tmp on install
   (`#24 <https://iraf-community.github.io/iraf-v216/issues/24>`__)
-  Fix setting of non-default IRAF root (`#22 <https://iraf-community.github.io/iraf-v216/issues/22>`__)
-  Clean up sources from unnecessary code (`#2 <https://iraf-community.github.io/iraf-v216/issues/2>`__,
   `#14 <https://iraf-community.github.io/iraf-v216/issues/14>`__, `#15 <https://iraf-community.github.io/iraf-v216/issues/15>`__,
   `#16 <https://iraf-community.github.io/iraf-v216/issues/16>`__, `#17 <https://iraf-community.github.io/iraf-v216/issues/17>`__,
   `#18 <https://iraf-community.github.io/iraf-v216/issues/18>`__, `#20 <https://iraf-community.github.io/iraf-v216/issues/20>`__,
   `#25 <https://iraf-community.github.io/iraf-v216/issues/25>`__, `#68 <https://iraf-community.github.io/iraf-v216/issues/68>`__,
   `#69 <https://iraf-community.github.io/iraf-v216/issues/69>`__, `#70 <https://iraf-community.github.io/iraf-v216/issues/70>`__,
   `#113 <https://iraf-community.github.io/iraf-v216/issues/113>`__, `#116 <https://iraf-community.github.io/iraf-v216/issues/116>`__,
   `#117 <https://iraf-community.github.io/iraf-v216/issues/117>`__)

.. |GitHub release| image:: https://img.shields.io/github/release/iraf-community/iraf.svg
   :target: https://github.com/iraf-community/iraf/releases/latest
