dataio

#{ The MTLOCAL special format NOAO tape reader package.

package	mtlocal

task	rcamera,
	rpds,
	rrcopy,
	rdumpf,
	ldumpf,
	r2df,
	ridsout,
	ridsfile,
	ridsmtn		= "mtlocal$x_mtlocal.e"

task	widstape	= "onedspec$irsiids/x_onedspec.e"

clbye()
