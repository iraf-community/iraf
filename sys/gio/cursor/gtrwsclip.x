# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GTR_POLYCLIP -- Clip a convex polygon to a box.  If the polygon is entirely
# outside the box 0 is returned; if the polygon is entirely within the box 1
# is returned, otherwise the polygon is clipped and a value other than 0 or 1
# is returned.  This is based on code by Paul Heckbert from Graphics Gems,
# 1985/1989.

int procedure gtr_polyclip (pv, npts, x1, x2, y1, y2)

short	pv[ARB]			#U polygon to be clipped
int	npts			#U number of points in polygon
int	x1,x2,y1,y2		#I clipping box

pointer	sp, p1, p2, pt
int	x1out, x2out, y1out, y2out, i
int	gtr_cliptoplane()
define	nopts_ 91

begin
	x1out = 0;  x2out = 0
	y1out = 0;  y2out = 0

	# Count vertices which are outside with respect to each of the
	# four planes.

	do i = 1, npts*2, 2 {
	    if (pv[i+0] < x1)  x1out = x1out + 1
	    if (pv[i+0] > x2)  x2out = x2out + 1
	    if (pv[i+1] < y1)  y1out = y1out + 1
	    if (pv[i+1] > y2)  y2out = y2out + 1
	}

	# Is the polygon entirely inside the clipping box?
	if (x1out + x2out + y1out + y2out == 0)
	    return (1)

	# Is the polygon entirely outside the clipping box?
	if (x1out == npts || x2out == npts || y1out == npts || y2out == npts)
	    return (0)

	# If we get here the polygon partially intersects the clipping box.
	# Clip against each of the planes that might cut the polygon, clipping
	# the previously clipped polygon in each step.  This is done in
	# floating point to minimize accumulation of error when interpolating
	# to the clipping plane to compute a new polygon vertex when the plane
	# is crossed.

	call smark (sp)
	call salloc (p1, npts * 4, TY_REAL)
	p2 = p1 + npts * 2

	call achtsr (pv, Memr[p1], npts * 2)

	if (x1out > 0)
	    if (gtr_cliptoplane (p1, p2, npts, 0, -1.0, real(x1)) == 0)
		goto nopts_
	    else {
		pt = p1;  p1 = p2;  p2 = pt
	    }
	if (x2out > 0)
	    if (gtr_cliptoplane (p1, p2, npts, 0,  1.0, real(x2)) == 0)
		goto nopts_
	    else {
		pt = p1;  p1 = p2;  p2 = pt
	    }
	if (y1out > 0)
	    if (gtr_cliptoplane (p1, p2, npts, 1, -1.0, real(y1)) == 0)
		goto nopts_
	    else {
		pt = p1;  p1 = p2;  p2 = pt
	    }
	if (y2out > 0)
	    if (gtr_cliptoplane (p1, p2, npts, 1,  1.0, real(y2)) == 0)
		goto nopts_
	    else {
		pt = p1;  p1 = p2;  p2 = pt
	    }

	call achtrs (Memr[p1], pv, npts * 2)
	call sfree (sp)
	return (npts)

nopts_
	call sfree (sp)
	return (0)
end


# GTR_CLIPTOPLANE --  Clip the convex polygon P1 against a plane, copying
# the inbounds portion to the output polygon P2.

int procedure gtr_cliptoplane (p1, p2, npts, index, s, ref)

pointer	p1			#I pointer to input polygon
pointer	p2			#I pointer to output polygon
int	npts			#U number of polygon points or vertices
int	index			#I index of coordinate to be tested
real	s			#I sign for comparison
real	ref			#I value to compare against

int	nout, i
pointer	op, u, v
real	tu, tv, t

begin
	nout = 0
	op = p2

	u = p1 + (npts - 1) * 2
	tu = s * Memr[u+index] - ref
	v = p1

	do i = 1, npts {
	    # On old polygon P1, U is previous vertex, V is current vertex,
	    # TV is negative if vertex V is in.

	    tv = s * Memr[v+index] - ref

	    if (! ((tu <= 0 && tv <= 0) || (tu > 0 && tv > 0))) {
		# Edge crosses plane; add intersection point to P2.
		t = tu / (tu - tv)
		Memr[op+0] = Memr[u+0] + t * (Memr[v+0] - Memr[u+0])
		Memr[op+1] = Memr[u+1] + t * (Memr[v+1] - Memr[u+1])
		nout = nout + 1
		op = op + 2
	    }

	    if (tv <= 0) {
		# Vertex V is in, copy it out.
		Memr[op+0] = Memr[v+0]
		Memr[op+1] = Memr[v+1]
		nout = nout + 1
		op = op + 2
	    }

	    u = v
	    tu = tv
	    v = v + 2
	}

	npts = nout
	return (nout)
end
