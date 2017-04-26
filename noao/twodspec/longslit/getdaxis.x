include	<mwset.h>


# GET_DAXIS -- Get logical dispersion axis.

procedure get_daxis (im, laxis, paxis)

pointer	im			#I IMIO pointer
int	laxis			#O Logical dispersion axis
int	paxis			#O Physical dispersion axis

real	ltm[2,2], ltv[2]
pointer	mw, tmp, mw_openim()
int	imgeti(), clgeti()
errchk	imaddi, mw_openim, mw_gltermr

begin
	# Get the dispersion axis from the header or package parameter.
	iferr (paxis = imgeti (im, "dispaxis")) {
	    paxis = clgeti ("dispaxis")
	    call imaddi (im, "dispaxis", paxis)
	}
	laxis = paxis

	# Check for a transposed image.
	iferr {
	    mw= NULL
	    tmp = mw_openim (im); mw = tmp
	    call mw_gltermr (mw, ltm, ltv, 2)
	    if (ltm[1,1] == 0. && ltm[2,2] == 0)
		laxis = mod (paxis, 2) + 1
	} then
	    ;
	if (mw != NULL)
	    call mw_close (mw)
end
