# PT_RANGES -- Procedure to convert apphot ranges to the format expected
# by the xtools ranges package.

int procedure pt_ranges (aranges, ranges, element, maxch)

char	aranges[ARB]	# input ranges
char	ranges[ARB]	# output ranges
int	element		# range element
int	maxch		# maximum number of characters in ranges

char	left_bkt, right_bkt
int	findex, lindex, nchars, ip
int	stridx(), ctoi()
data	left_bkt /'['/, right_bkt /']'/

begin
	# Test for existence of ranges.
	element = 1
	ranges[1] = EOS
	if (aranges[1] == EOS)
	    return (OK)

	# Test for range delimiters.
	findex = stridx (left_bkt, aranges)
	lindex = stridx (right_bkt, aranges)
	if (findex == 0 || lindex == 0 || (lindex <= findex + 1))
	    return (ERR)

	# Compute the element selection.
	ip = 1
	nchars = ctoi (aranges[findex+1], ip, element)
	if (nchars == 0)
	    element = 1

	# Copy the ranges portion.
	call strcpy (aranges[findex+1], ranges, lindex - findex - 1)
	return (OK)
end
