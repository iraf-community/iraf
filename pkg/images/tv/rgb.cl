#{ RGB -- Select rgb display mode.

# red_frame,i,a,1,1,4,red frame
# green_frame,i,a,2,1,4,green frame
# blue_frame,i,a,3,1,4,blue frame
# window,b,h,no,,,window RGB frames

{
	_dcontrol (type="rgb", red_frame=red_frame, green_frame=green_frame,
	    blue_frame=blue_frame, rgb_window=window)
}
