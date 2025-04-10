IRAF 2.18.1 Release Notes
=========================

:Authors: IRAF Community, NOIRLab
:Date: April 10, 2025

The current IRAF version 2.18.1 is available from Github at

https://github.com/iraf-community/iraf/releases/tag/v2.18.1

Changes to the version 2.17.1 include:

- **Integration of NOIRLab changes**

  NOIRLab recently published their own 2.18 version with the scope
  limited to the support of the Gemini data reduction pipeline. All
  NOIRLab changes are bugfixes, based on NOAO version 2.16.1. They
  were reviewed and (if useful) integrated into the IRAF baseline.
  This merge also triggers the update of the major version number to
  be in sync with the NOIRlab version.

- **VOTable support removed**

  This is a consequence of merging the changes from NOIRlab, resulting
  in a significant simplification of the code. If this creates
  significant problems to the users, we will re-add VOTable support,
  however.
  
- **Command Language start script renamed to irafcl**

  As already done since years in several Linux distributions, the
  **cl** and **ecl** scripts are renamed to **irafcl**. **cl** and
  **ecl** often conflict with the Common Lisp executable names and
  require third special effort by third parties to circumvent
  problems. For compatibility, **cl** and **ecl** are still provided
  as links, but their use is deprecated. Please use **irafcl**
  instead. A man page is provided.

- **Support for the LoongArch architecture**

  Support for using IRAF on the Chinese LoongArch LA64 architecture
  was added. Binaries will be provided as part of the Debian loong64
  port.

- **Bug fixes and code cleanup**

  As before, a number of bugs was fixed in this release. Cleaning up
  the code and modernization of the code structure continued. This is
  in large parts a benefit of integrating the NOIRLab changes.


Detailed list of changes
------------------------

This list shows all pull requests that were merged since 2.17.1.

Since 2.18
~~~~~~~~~~

- Protoize CL and ECL (`#391 <https://github.com/iraf-community/iraf/pull/391>`__)
- Fix build on Fedora-40: Use explicit pointer casts in some more places (`#396 <https://github.com/iraf-community/iraf/pull/396>`__)
- Use time_t in bootlib (`#405 <https://github.com/iraf-community/iraf/pull/405>`__)
- NOIRLAB: tbtables: automatically add .tab extn if necessary (`#408 <https://github.com/iraf-community/iraf/pull/408>`__)
- NOIRLAB: Move stxtools (library) package from utilities/nttools to package root (`#406 <https://github.com/iraf-community/iraf/pull/406>`__)
- NOIRLAB: Change stsdas to st4gem for psikern entries in graphcat (`#412 <https://github.com/iraf-community/iraf/pull/412>`__)
- Remove VMcache client code from IRAF (`#417 <https://github.com/iraf-community/iraf/pull/417>`__)
- Add hurd64 to the list of supported architectures (`#418 <https://github.com/iraf-community/iraf/pull/418>`__)
- Delete gx generated files (`#433 <https://github.com/iraf-community/iraf/pull/433>`__)
- Make IRAF compileable with gcc-15 and newer clang (`#429 <https://github.com/iraf-community/iraf/pull/429>`__)
- Silence warnings (`#435 <https://github.com/iraf-community/iraf/pull/435>`__,
  `#432 <https://github.com/iraf-community/iraf/pull/432>`__)
- Shrink math bevington (`437 <https://github.com/iraf-community/iraf/pull/437>`__)
- Fix type of jumpcon in (e)cl/main.c (`439 <https://github.com/iraf-community/iraf/pull/439>`__)
- Prepare release (`#434 <https://github.com/iraf-community/iraf/pull/434>`__,
  `#438 <https://github.com/iraf-community/iraf/pull/438>`__)


Since 2.17.1
~~~~~~~~~~~~

- Makefile install fixes found when creating the Debian package (`#315 <https://github.com/iraf-community/iraf/pull/315>`__)
- Strip more files from the installed package (`#316 <https://github.com/iraf-community/iraf/pull/316>`__)
- C tweaks to make f2c compileable on Debian/mipsel (`#317 <https://github.com/iraf-community/iraf/pull/317>`__)
- Fix iraf$ variable for "inplace" (personal) installation (`#318 <https://github.com/iraf-community/iraf/pull/318>`__)
- Rename ecl canonical name to irafcl (`#323 <https://github.com/iraf-community/iraf/pull/323>`__)
- Remove version information from CITATION.cff (`#332 <https://github.com/iraf-community/iraf/pull/332>`__)
- Remove VOTable support  (`#330 <https://github.com/iraf-community/iraf/pull/330>`__)
- Extend (E)CL testing (`#368 <https://github.com/iraf-community/iraf/pull/368>`__)
- NOIRLAB: Re-add the vtel package (`#343 <https://github.com/iraf-community/iraf/pull/343>`__)
- NOIRLAB: Update observatory database  (`#344 <https://github.com/iraf-community/iraf/pull/344>`__)
- NOIRLAB: Clean up tbtables package (`#345 <https://github.com/iraf-community/iraf/pull/345>`__)
- NOIRLAB: Force exclusion of .git directory from processing in rmbin and rmfiles (`#362 <https://github.com/iraf-community/iraf/pull/362>`__)
- NOIRLAB: Remove unused variabled from nttables (`#351 <https://github.com/iraf-community/iraf/pull/351>`__)
- NOIRLAB:  Restore missing font data files in sys/gio/fonts (`#356 <https://github.com/iraf-community/iraf/pull/356>`__)
- NOIRLAB: string buffer fix in mii_readc in "help" package (`#355 <https://github.com/iraf-community/iraf/pull/355>`__)
- NOIRLAB: Potential xc/xpp string overflow fixes (`#349 <https://github.com/iraf-community/iraf/pull/349>`__)
- NOIRLAB: Small fixes in IRAF system packages (`#363 <https://github.com/iraf-community/iraf/pull/363>`__)
- NOIRLAB: Remove or comment out unused variables (`#352 <https://github.com/iraf-community/iraf/pull/352>`__)
- NOIRLAB: Small bugfixes for libsys (`#361 <https://github.com/iraf-community/iraf/pull/361>`__)
- NOIRLAB: Small "noao" package fixes (`#360 <https://github.com/iraf-community/iraf/pull/360>`__)
- NOIRLAB: fixes and cleanup for the "mkpkg" tool (`#348 <https://github.com/iraf-community/iraf/pull/348>`__)
- NOIRLAB: cosmetic change for the "generic" tool (`#347 <https://github.com/iraf-community/iraf/pull/347>`__)
- NOIRLAB: minor libboot.a improvements (`#350 <https://github.com/iraf-community/iraf/pull/350>`__)
- NOIRLAB: Improve (E)CL code internals (`#364 <https://github.com/iraf-community/iraf/pull/364>`__)
- NOIRLAB: Rewrite file taken from IRAF64 (`#353 <https://github.com/iraf-community/iraf/pull/353>`__)
- NOIRLAB: zzsetenv.def updates (`#365 <https://github.com/iraf-community/iraf/pull/365>`__)
- NOIRLAB: several patches for libos.a (`#346 <https://github.com/iraf-community/iraf/pull/346>`__)
- Use fpurge/__fpurge to cancel buffered output (`#366 <https://github.com/iraf-community/iraf/pull/366>`__)
- Add support for LoongArch (`#371 <https://github.com/iraf-community/iraf/pull/371>`__)
- Update INSTALL.md typo (`#373 <https://github.com/iraf-community/iraf/pull/373>`__)
- New Github runner for Mac M1 (`#374 <https://github.com/iraf-community/iraf/pull/374>`__)
- Fix problems with using f2c.e/f77.sh on plain Fortran files (`#370 <https://github.com/iraf-community/iraf/pull/370>`__)
- Remove outdated options from XC (`#372 <https://github.com/iraf-community/iraf/pull/372>`__)
- Provide compatibility link to mkfloat.csh (`#375 <https://github.com/iraf-community/iraf/pull/375>`__)
- NOIRLAB: Don't add to helpdb var if no helpdb.mip (sqiid/upsqiid) (`#378 <https://github.com/iraf-community/iraf/pull/378>`__)
- CI workflow: update actions/checkout to v4 (`#382 <https://github.com/iraf-community/iraf/pull/382>`__)
- NOIRLAB: Add numerical recipes to libxtools for Noirlab compatibility (`#387 <https://github.com/iraf-community/iraf/pull/387>`__)
- Allow empty IRAFARCH in irafpath() (`#388 <https://github.com/iraf-community/iraf/pull/388>`__)
