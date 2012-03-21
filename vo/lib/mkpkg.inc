# Global MKPKG definitions for the package.

$set XFLAGS	= "$(XFLAGS) -p vo -p noao"
$set XVFLAGS	= "$(XVFLAGS) -p vo -p noao"
$set LFLAGS	= "$(LFLAGS) -p vo -p noao"

#$ifeq (MACH, ssun) then
#    $include "vo$lib/mkpkg.sf.SSUN"
#$else $ifeq (MACH, cygwin) then
#    $include "vo$lib/mkpkg.sf.CYGWIN"
#$else $ifeq (MACH, macosx, macintel) then
#    $include "vo$lib/mkpkg.sf.MACOSX"
#$else $ifeq (MACH, redhat, linux, suse) then
#    $include "vo$lib/mkpkg.sf.REDHAT"
#$else $ifeq (MACH, sunos) then
#    $include "vo$lib/mkpkg.sf.SUNOS"
#$else $ifeq (MACH, ssun) then
#    $include "vo$lib/mkpkg.sf.SSUN"
#$else $ifeq (MACH, sparc) then
#    $include "vo$lib/mkpkg.sf.SUN4"
#$end
