#{ ZOOM -- Zoom in on a portion of the display.

# zoom_factor,i,a,2,1,4,factor by which image scale is to be expanded
# window,b,h,no,,,window enlarged image

{
	if (window)
	    _dcontrol (zoom=zoom_factor, roam=yes, window=yes)
	else
	    _dcontrol (zoom=zoom_factor, roam=yes)
}
