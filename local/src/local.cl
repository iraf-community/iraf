#{ Package script task for the LOCAL package.  Add task declarations here
# for any locally added tasks or subpackages.

cl < "local$lib/zzsetenv.def"
package local, bin=localbin$

task	bswap,
	pavg		= "local$src/x_local.e"

clbye()
