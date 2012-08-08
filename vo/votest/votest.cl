#{ VOTEST -- VO Unit Test Sub-Package

# Logical directories.
reset 	data 		= "votest$data/"
reset 	tests		= "votest$tests/"


package votest

# Compute the host-specific path to the data directory.
votest.data_path	= osfn ("data$")

# Compiled tasks.

# Script tasks.
task 	test		= votest$test.cl

task 	run_test	= votest$run_test.cl
task 	mkout   	= votest$mkout.cl
task 	mkcache   	= votest$mkcache.cl


# Foreign (Java) tasks.

# Hidden tasks.
hidetask run_test, mkout, mkcache


clbye()
