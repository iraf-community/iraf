# MASKCOLOR -- A color for a mask value.

procedure mcolors (colors, maskval, dataval)

pointer	colors			#I Mask colormap object
int	maskval			#I Mask value
short	dataval			#U Data value to be set

int	i, j, offset, color

begin
	color = Memi[colors+2]
	offset = Memi[colors+3]
	do i = 2, Memi[colors] {
	    j = 4 * i - 4
	    if (maskval >= Memi[colors+j] && maskval <= Memi[colors+j+1]) {
		color = Memi[colors+j+2]
		offset = Memi[colors+j+3]
		break
	    }
	}

	if (offset == YES)
	    color = maskval + color
	if (color >= 0)
	    dataval = color
end


procedure mcolorr (colors, maskval, dataval)

pointer	colors			#I Mask colormap object
int	maskval			#I Mask value
real	dataval			#U Data value to be set

int	i, j, offset, color

begin
	color = Memi[colors+2]
	offset = Memi[colors+3]
	do i = 2, Memi[colors] {
	    j = 4 * i - 4
	    if (maskval >= Memi[colors+j] && maskval <= Memi[colors+j+1]) {
		color = Memi[colors+j+2]
		offset = Memi[colors+j+3]
		break
	    }
	}

	if (offset == YES)
	    color = maskval + color
	if (color >= 0)
	    dataval = color
end
