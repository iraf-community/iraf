#{ APPHOT -- Digital aperture photometry package.

lists

package apphot

task apselect,
     center,
     daofind,
     fitpsf,
     fitsky,
     radprof,
     phot,
     polymark,
     polyphot,
     qphot,
     wphot	= "apphot$x_apphot.e"

task centerpars = "apphot$centerpars.par"
task fitskypars = "apphot$fitskypars.par"
task datapars = "apphot$datapars.par"
task photpars = "apphot$photpars.par"
task polypars = "apphot$polypars.par"

#task lintran = "pkg$lists/x_lists.e"

clbye()
