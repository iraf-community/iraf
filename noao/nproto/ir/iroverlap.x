# IR_OVERLAP -- Procedure to compute the overlap between two rectangles.

int procedure ir_overlap (pc1out, pc2out, pl1out, pl2out, c1out, c2out,
	l1out, l2out, oc1out, oc2out, ol1out, ol2out)

int	pc1out, pc2out		# previous subraster column limits
int	pl1out, pl2out		# previous subraster line limits
int	c1out, c2out		# current subraster column limits
int	l1out, l2out		# current subraster line limits
int	oc1out, oc2out		# overlap column limits
int	ol1out, ol2out		# overlap line limits

begin
	# Check for the case where no intersection is present.
	if (c1out > pc2out || c2out < pc1out || l1out > pl2out ||
	    l2out < pl1out)
	    return (NO)

	# Compute the column overlap limits.
	if (pc1out <= c1out)
	    oc1out = c1out
	else
	    oc1out = pc1out
	if (pc2out <= c2out)
	    oc2out = pc2out
	else
	    oc2out = c2out

	# Compute the line overlap limits.
	if (pl1out <= l1out)
	    ol1out = l1out
	else
	    ol1out = pl1out
	if (pl2out <= l2out)
	    ol2out = pl2out
	else
	    ol2out = l2out

	return (YES)
end
