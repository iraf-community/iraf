#{ CRUTIL -- Cosmic Ray Utility Package

package crutil

reset	crusrc			= "crutil$src/"

task	crcombine	= crusrc$crcombine.cl
task	crnebula	= crusrc$crnebula.cl
task	crfix		= crusrc$crfix.cl
task	credit		= crusrc$credit.cl

task	cosmicrays,
	craverage,
	crgrow,
	crmedian	= crusrc$x_crutil.e


clbye()
