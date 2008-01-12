# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
# Fitsio package.
# Revision history:
#	17 Sep-1993 by RAShaw:	Rename `fits2tape' to `fitscopy'

task	     fitscopy = t_fitscopy,	
	     gftoxdim = t_gftoxdim,
	     catfits  = t_catfits,
	     strfits  = t_rfits,
	     stwfits  = t_wfits,
	     xdimtogf = t_xdimtogf
