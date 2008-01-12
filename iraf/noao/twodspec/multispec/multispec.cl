#{ MULTISPEC -- The MULTISPEC package.

package	multispec

task	newextraction,
	findpeaks,
	msset,
	mslist,
	fitfunction,
	msextract,
	newimage,
	modellist,
	msplot,
	fitgauss5	= multispec$x_multispec.e

# Scripts
task	_msfindspec1	= multispec$_msfindspec1.cl
task	_msfindspec2	= multispec$_msfindspec2.cl
task	_msfindspec3	= multispec$_msfindspec3.cl

clbye
