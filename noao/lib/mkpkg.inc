# Global MKPKG definitions for the NOAO packages.

$set XFLAGS	= "$(XFLAGS) -p noao"
$set XVFLAGS	= "$(XVFLAGS) -p noao"
$set LFLAGS	= "$(LFLAGS) -p noao"

$ifeq (MACH, sparc) then
$include "noao$lib/mkpkg.sf.SUN4"
$else $ifeq (MACH, i386) then
$include "noao$lib/mkpkg.sf.I386"
$else $ifeq (MACH, f68881) then
$include "noao$lib/mkpkg.sf.SUN3"
$else $ifeq (MACH, ffpa) then
$include "noao$lib/mkpkg.sf.SUN3"
$else $ifeq (MACH, vms) then
$include "noao$lib/mkpkg.sf.VMS"
$else $ifeq (MACH, mips) then
$include "noao$lib/mkpkg.sf.MIPS"
$else $ifeq (MACH, m68k) then
$include "noao$lib/mkpkg.sf.M68K"
$else $ifeq (MACH, a88k) then
$include "noao$lib/mkpkg.sf.A88K"
$end
