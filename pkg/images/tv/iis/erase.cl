#{ ERASE -- Erase a greyscale display frame.

# frame,i,a,1,1,4,frame to be erased
# saveframe,i,h

{
	saveframe = _dcontrol.frame
	_dcontrol (frame=frame, erase=yes)
	_dcontrol (frame = saveframe)
}
