images
plot

#{ BENCH -- Benchmarks package.

package bench

set	bench		= "pkg$bench/"

task	fortask		= "bench$fortask.cl"
task	subproc		= "bench$subproc.cl"
task	plots		= "bench$plots.cl"

task	$ptime,
	$getpar,
	$wipc.bb,
	$rrbin,
	$rbin,
	$wbin,
	$rtext,
	$wtext		= "bench$x_bench.e"

clbye()
