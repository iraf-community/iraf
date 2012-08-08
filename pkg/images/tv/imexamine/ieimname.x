# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IE_IMNAME -- Get the name of the image displayed in a display frame.

procedure ie_imname (ds, frame, imname, maxch)

pointer	ds			#I display descriptor
int	frame			#I display frame
char	imname[maxch]		#O image name
int	maxch			#I max chars out

int	snx, sny, dx, dy, dnx, dny, status, imd_query_map()
real	sx, sy
pointer	sp, reg, dname, iw
pointer	iw_open()
errchk	imd_query_map, iw_open

begin
	call smark (sp)
	call salloc (reg, SZ_FNAME, TY_CHAR)
	call salloc (dname, SZ_FNAME, TY_CHAR)

	if (imd_query_map (frame, Memc[reg], sx, sy, snx, sny, dx, dy, dnx, dny,
	    Memc[dname]) == ERR) {
	    iw = iw_open (ds, frame/100, Memc[dname], SZ_FNAME, status)
	    call iw_close (iw)
	}

	# call imgimage (Memc[dname], imname, maxch)
	call strcpy (Memc[dname], imname, maxch)

	call sfree (sp)
end
