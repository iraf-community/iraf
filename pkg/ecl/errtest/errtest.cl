#{ ERRTEST.CL -- Package declaration for the CLERR recovery/test suite.

package errtest

task	$fpe,
	$segvio,
	$spperr		= "errtest$spperrs.e"

task	errif		= "errtest$errif.cl"
task	errtype		= "errtest$errtype.cl"
task	$sfpe		= "errtest$sfpe.cl"
task	nested		= "errtest$nested.cl"
task	recursion	= "errtest$recursion.cl"

task	$zztest		= "errtest$zztest.cl"
task	$printvals	= "errtest$printvals.cl"

task	nest0		= "errtest$nest0.cl"
task	recur0		= "errtest$recur0.cl"

task	test_iferr	= "errtest$test_iferr.cl"

hidetask nest0, recur0

clbye()
