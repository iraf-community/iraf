#{ LUMATCH -- Match the lookup tables for two frames.

# frame,i,a,,1,4,frame to be adjusted
# ref_frame,i,a,,1,4,reference frame

{
	_dcontrol (frame=frame, alternate=ref_frame, match=yes)
}
