# Global MKPKG definitions for the COLOR package.

$set XFLAGS	= "$(XFLAGS) -p color"

# Special file lists

$ifeq (MACH, f68881) then
    $include "color$lib/mkpkg.sf.SUN3"
$else $ifeq (MACH, ffpa) then
    $include "color$lib/mkpkg.sf.SUN3"
$end
