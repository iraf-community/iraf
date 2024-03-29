# IRAF 2.17.1

The latest NOAO IRAF release is 2.16.1 from October 2013. Intermediate
releases were snapshots based on that latest available source code. These
snapshots were tagged with their release date in the version number.
Changes to the 2.16.1 sources include:

* __Community maintainance__

   IRAF is no longer maintained by NOAO, but by the community of
   volunteers. Contributions of bug fixes, documentation or improvements
   are welcome.

* __All known non-free code removed__

   Although IRAF 2.16.1 was claimed to be "free software", it contained
   source code that is not freely distributable; namely code copied from the
   book ["Numerical Recipes in Fortran"](http://numerical.recipes/). This
   code is replaced with free equivalents. The IRAF community edition is
   [Open Source](https://opensource.org/docs/osd), and as such included in
   Debian.

* __Installation procedure reworked__

   The installation scripts are replaced with makefiles, and the build
   and installation procedures now use the standard targets, standard
   paths, and customization environment variables. The standard
   installation place is `/usr/local/lib/iraf`. On a build error the
   build now stops now immediately, making it easier to find the
   cause.
   
* __IRAF ported to other architectures__

   IRAF is now ported to a number of little endian architectures (ARM,
   PowerPC, MIPS, x32, RISC-V64) and operating systems (GNU Hurd and
   FreeBSD). Specifically, the Mac M1 processor is supported now.

   The code was adjusted to be compliant with newer (C99) C standards,
   and to be compileable with actual compilers.

* __Major bug fixes__

  Many bugs of the 2.16.1 release are fixed. Some of he major ones are:
  
   - Linux systems crashed with "Out of memory" (13 year old bug;
     [2.12 release notes](https://github.com/iraf-community/iraf/blob/9590f4/doc/notes.v212#L1065-L1075))
   
   - `noao.digiphot.photcal.fitparams` failed with a segmentation
     fault on 64-bit systems
     ([iraf.net](https://iraf.net/forum/viewtopic.php?showtopic=1467834))
   
   - The system wide IRAF installation changed the permissions of
     `/tmp/`, creating a major security hole in the system
     (https://iraf-community.github.io/iraf-v216/issues/23)
   
   - On Linux systems, self-compiled tasks gave wrong results
     ([iraf.net](https://iraf.net/forum/viewtopic.php?showtopic=1467841))
   
   - On modern systems, background execution did not work
     ([iraf.net](https://iraf.net/forum/viewtopic.php?showtopic=1467431))

   -  The original code produced errornous executables when build on
      Linux versions later than 2012. It also did not build from
      scratch, but required an already compiled IRAF version.

* __Simple CI test framework added__

    The tests are defined and documented in
    [MarkDown](https://github.com/iraf-community/iraf/blob/main/test/README.md)
    files. Tests are run using Github Actions on Linux and MacOS X platforms.

* __VO package and vocl removed__

    The VO package, and the vocl shell heavily depend on a number of
    Java jars, where the creation from sources is undocumented. The
    package also uses outdated VO standards. A discussion with Mike
    Fitzpatrick resulted in his plan to [move the VO functionality
    into an external
    package](https://iraf-community.github.io/iraf-v216/issues/90).
    Therefore, no attempt was put into getting these problems fixed,
    and the VO stuff was cut out.  The VOTable functionality, however,
    remains available
