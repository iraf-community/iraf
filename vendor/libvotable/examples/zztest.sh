#!/bin/csh -f

/bin/rm -f foo*

votget_spp.e votget votable=data/sia_m51.xml base=foo ucd="" col=0 verbose=no
