# Generic routine for setting parameters.

ccdred.pixeltype = "real real"
ccdred.verbose = yes
ccdred.logfile = "logfile"
ccdred.plotfile = ""
ccdred.backup = ""
ccdred.instrument = "ccddb$kpno/direct.dat"
ccdred.ssfile = "subsets"
ccdred.graphics = "stdgraph"
ccdred.cursor = ""

ccdproc.fixpix = no
ccdproc.overscan = yes
ccdproc.trim = yes
ccdproc.zerocor = yes
ccdproc.darkcor = no
ccdproc.flatcor = yes
ccdproc.readcor = no
ccdproc.scancor = no
ccdproc.readaxis = "line"
ccdproc.biassec = "image"
ccdproc.trimsec = "image"
ccdproc.interactive = yes
ccdproc.function = "chebyshev"
ccdproc.order = 1
ccdproc.sample = "*"
ccdproc.naverage = 1
ccdproc.niterate = 1
ccdproc.low_reject = 3
ccdproc.high_reject = 3
ccdproc.grow = 0

flatcombine.reject = "crreject"
flatcombine.rdnoise= "rdnoise"
flatcombine.gain="gain"
