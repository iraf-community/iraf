# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GMFTITLE -- Insert a title (comment) into the output metacode instruction
# stream.  No graphics output is generated.  The purpose of the metafile
# title is to document the contents of metafiles.

procedure gmftitle (gp, mftitle)

pointer	gp			# graphics descriptor
char	mftitle[ARB]		# metafile title

begin
	call gpl_flush()
	call gki_mftitle (GP_FD(gp), mftitle)
end
