# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IE_IMNAME -- Get the name of the image displayed in a display frame.

procedure ie_imname (ds, frame, imname, maxch)

pointer	ds			#I display descriptor
int	frame			#I display frame
char	imname[maxch]		#O image name
int	maxch			#I max chars out

int	status
pointer	sp, dname, iw
pointer	iw_open()
errchk	iw_open

begin
	call smark (sp)
	call salloc (dname, SZ_LINE, TY_CHAR)

	iw = iw_open (ds, frame, Memc[dname], SZ_LINE, status)
	call iw_close (iw)

	# call imgimage (Memc[dname], imname, maxch)
	call strcpy (Memc[dname], imname, maxch)

	call sfree (sp)
end
