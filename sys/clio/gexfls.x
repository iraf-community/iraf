# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GEXFLS -- Externally callable procedure for flushing graphics output.
# Called by the CLIO procedure CLGCUR to flush graphics output prior to
# a cursor read.  The main complication is that since CLGCUR is part of
# the CLIO package and may be used in a program that does not generate
# any graphics, we do not want to directly reference any GIO procedures
# since this would force the linker to load much of GIO.

procedure gexfls()

int	gflush
pointer	gp[2]
common	/gxflcm/ gp, gflush

begin
	if (gflush != NULL) {
	    if (gp[1] != NULL)
		call zcall1 (gflush, gp[1])
	    if (gp[2] != NULL)
		call zcall1 (gflush, gp[2])
	}
end


# GEXFLS_SET -- Set pointers to the gflush procedure for a stream.

procedure gexfls_set (stream, gp_value, epa_gflush)

int	stream			# graphics stream
pointer	gp_value		# graphics descriptor
int	epa_gflush		# EPA of the gflush procedure

int	gflush
pointer	gp[2]
common	/gxflcm/ gp, gflush

begin
	if (stream == STDGRAPH || stream == STDIMAGE) {
	    gp[stream-STDGRAPH+1] = gp_value
	    gflush = epa_gflush
	}
end


# GEXFLS_CLEAR -- Clear the pointer to the gflush procedure for a stream.

procedure gexfls_clear (stream)

int	stream			# graphics stream
int	gflush
pointer	gp[2]
common	/gxflcm/ gp, gflush

begin
	if (stream == STDGRAPH || stream == STDIMAGE)
	    gp[stream-STDGRAPH+1] = NULL
end
