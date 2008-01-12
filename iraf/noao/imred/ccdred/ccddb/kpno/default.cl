# Default KPNO parameters.

ccdred.pixeltype = "real real"
ccdred.verbose = yes
ccdred.logfile = "logfile"
ccdred.plotfile = ""
ccdred.backup = ""
ccdred.instrument = "ccddb$kpno/default.dat"
ccdred.ssfile = "subsets"
ccdred.graphics = "stdgraph"
ccdred.cursor = ""

ccdproc.ccdtype = ""
ccdproc.fixpix = no
ccdproc.overscan = yes
ccdproc.trim = yes
ccdproc.zerocor = yes
ccdproc.darkcor = no
ccdproc.flatcor = no
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

combine.rdnoise= "rdnoise"
combine.gain="gain"
zerocombine.rdnoise= "rdnoise"
zerocombine.gain="gain"
flatcombine.rdnoise= "rdnoise"
flatcombine.gain="gain"
flatcombine.reject = "crreject"
