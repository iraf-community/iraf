#{ APPHOT -- Digital aperture photometry package.

dataio			# rfits task required by aptest.cl script
lists			# lintran task in the lists package

package apphot

task center,
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
task datapars   = "apphot$datapars.par"
task findpars	= "apphot$findpars.par"
task fitskypars = "apphot$fitskypars.par"
task photpars   = "apphot$photpars.par"
task polypars   = "apphot$polypars.par"

task aptest	= "apphot$aptest.cl"

# PTOOLS tasks

task pexamine	= "apphot$x_ptools.e"

task    xyplot          = "ptools$xyplot.par"
task    histplot        = "ptools$histplot.par"
task    radplot         = "ptools$radplot.par"
task    surfplot        = "ptools$surfplot.par"
task    cntrplot        = "ptools$cntrplot.par"

task txdump	= "ptools$x_ptools.e"

hidetask    xyplot, histplot, radplot, surfplot, cntrplot

clbye()
