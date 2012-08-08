#{ Package script task for the LISTS package, a collection of routines
# for processing textual lists.

package lists

set	lists		= "pkg$lists/"

task	table,
	tokens,
	columns,
	unique,
	lintran,
	$rgcursor,
	rimcursor,
	words		= "lists$x_lists.e"

task	average		= "lists$average.cl"
task	raverage	= "lists$raverage.cl"

clbye()
