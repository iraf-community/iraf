#{ BLINK -- Blink 2, 3, or 4 frames.

# frame1,i,a,,,,Frame1
# frame2,i,a,,,,Frame2
# frame3,i,a,,,,Frame3
# frame4,i,a,,,,Frame4
# rate,r,h,1.,,,Blink rate (sec per frame)

{
	if ($nargs == 3) {
	    _dcontrol (alternate = frame1 // " " // frame2 // " " //
		frame3, blink+, rate=rate)
	} else if ($nargs == 4) {
	    _dcontrol (alternate = frame1 // " " // frame2 // " " //
		frame3 // " " // frame4, blink+, rate=rate)
	} else {
	    _dcontrol (alternate = frame1 // " " // frame2, blink+, rate=rate)
	}
}
