# Global MKPKG definitions for the NOAO packages.

$set XFLAGS	= "$(XFLAGS) -p noao"
$set XVFLAGS	= "$(XVFLAGS) -p noao"
$set LFLAGS	= "$(LFLAGS) -p noao"

$ifeq (MACH, ssun) then
    $include "noao$lib/mkpkg.sf.SSUN"
$else $ifeq (MACH, sparc) then
    $include "noao$lib/mkpkg.sf.SUN4"
$else $ifeq (MACH, i386) then
    $include "noao$lib/mkpkg.sf.I386"
$else $ifeq (MACH, f68881) then
    $include "noao$lib/mkpkg.sf.SUN3"
$else $ifeq (MACH, ffpa) then
    $include "noao$lib/mkpkg.sf.SUN3"
$else $ifeq (MACH, vms) then
    $include "noao$lib/mkpkg.sf.VMS"
$else $ifeq (MACH, ddec) then
    $include "noao$lib/mkpkg.sf.DDEC"
$else $ifeq (MACH, dmip) then
    $include "noao$lib/mkpkg.sf.DMIP"
$else $ifeq (MACH, rs6000) then
    $include "noao$lib/mkpkg.sf.AIX3"
$else $ifeq (MACH, mips) then
    $include "noao$lib/mkpkg.sf.MIPS"
$end
