#{ VOTOOLS -- VO Toolbox Sub-Package

# Logical directories.
reset 	votools 	= "vo$votools/"
reset 	handlers 	= "vo$handlers/"
reset 	vojava 		= "vo$java/"
reset 	vodata 		= "vo$votest/data/"


package votools

# Compiled tasks.
task	dalclient,
	dispname,
	makewcs,
	sesame,
	votcopy,
	votsize,
	votget,
	vodata,
	sbquery		= votools$x_votools.e


# Script tasks.
task 	tblhandler	= handlers$tblhandler.cl    # SAMP handlers
#task 	imghandler	= handlers$imghandler.cl
task 	overhandler	= handlers$overhandler.cl

task	nedoverlay	= votools$nedoverlay.cl     # Catalog overlays
task	radiooverlay	= votools$radiooverlay.cl 
task	xrayoverlay	= votools$xrayoverlay.cl 
task	obslogoverlay	= votools$obslogoverlay.cl 

task 	dss		= votools$dss.cl	    # Image access

task 	getimg		= votools$getimg.cl         # Data access
task 	getcat		= votools$getcat.cl
task 	getspec		= votools$getspec.cl
task 	imgcat		= votools$imgcat.cl

task 	regdb		= votools$regdb.cl
task 	mkregdb		= votools$mkregdb.cl
task	wcsinfo		= votools$wcsinfo.cl

task 	hub		= votools$hub.cl	    # External apps
task 	topcat		= votools$topcat.cl
task 	aladin		= votools$aladin.cl

task 	taboverlay	= votools$taboverlay.cl	    # Table tools
task 	votpos		= votools$votpos.cl
task 	tabclip		= votools$tabclip.cl

task 	regmetalist	= votools$regmetalist.cl    # Utility routines
task 	colbyid		= votools$colbyid.cl
task 	colbyname	= votools$colbyname.cl
task 	colbyucd	= votools$colbyucd.cl
task 	qstring		= votools$qstring.cl
task 	prettystr	= votools$prettystr.cl


# Foreign (Java) tasks.
printf ("$task $voclientd   = $%s/voclientd\nkeep\n", osfn("vojava$"))  | cl()


# Hidden tasks.
#hidetask dalclient
hidetask regmetalist
#hidetask tabclip
#hidetask sbquery
hidetask qstring

#hidetask vocdctl
hidetask voclientd


clbye()
