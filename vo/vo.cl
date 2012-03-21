#{ VO -- VO Client Application Package

# Load dependent packages.
nproto
astutil
digiphot
apphot
photcal
onedspec
if (deftask ("astcat"))
    astcat
else
    ;

# This package requires FITS image type and various kernel parameters.
reset imtype = "fits"
if (defvar ("fkinit"))
    set fkinit = envget ("fkinit") // ",append,padlines=10,cachesize=60"
else
    set fkinit = "append,padlines=10,cachesize=60"

# Initialize the VOClient
#vocinit


cl < "vo$lib/zzsetenv.def"
package vo, bin = vobin$

# Logical directories.
set	voapps		= "vo$src/"
set 	vojava 		= "vo$java/"
set 	vodata 		= "vo$votest/data/"

# Sub-packages.
set	votest		= "vo$votest/"
set	votools		= "vo$votools/"

# Set the local package environment.
set	clobber		= yes
set	imclobber	= yes
set	imtype		= "fits"


# Compiled tasks.


# Script tasks.
task 	registry	= vosrc$registry.cl	# Resource discovery
#task	sloanspec	= vosrc$sloanspec.cl	# Query for SDSS spectra 
#task 	vizier		= vosrc$vizier.cl
#task	datascope	= vosrc$datascope.cl	# Demo apps
#task	skybot		= vosrc$skybot.cl
#task	fchart		= vosrc$fchart.cl


# Hidden tasks.

# Subpackages
task	votest.pkg	= votest$votest.cl
task	votools.pkg	= votools$votools.cl

# Load packages.
votools
vo

clbye()
