#{ CCDTEST -- CCDRED Test package

package ccdtest

task	mkimage		= ccdtest$x_ccdred.e
task	observe		= ccdtest$observe.cl
task	subsection	= ccdtest$subsection.cl
task	demo		= ccdtest$demo.cl

clbye()
