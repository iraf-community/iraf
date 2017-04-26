#{ CCDTEST -- CCDRED Test package

package ccdtest

task	mkimage		= ccdtest$x_ccdred.e
task	artobs		= ccdtest$artobs.cl
task	subsection	= ccdtest$subsection.cl
task	demo		= ccdtest$demo.cl

clbye()
