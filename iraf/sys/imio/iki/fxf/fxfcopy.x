# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include	<error.h>

# FXF_COPY -- Copy an image.  A special operator is provided for fast, blind
# copies of entire images.

procedure fxf_copy (kernel, oroot, oextn, nroot, nextn, status)

int	kernel			#I IKI kernel
char	oroot[ARB]		#I old image root name
char	oextn[ARB]		#I old image extn
char	nroot[ARB]		#I new image root name
char	nextn[ARB]		#I old image extn
int	status

pointer	sp
pointer	ohdr_fname, nhdr_fname

begin
	call smark (sp)
	call salloc (ohdr_fname, SZ_PATHNAME, TY_CHAR)
	call salloc (nhdr_fname, SZ_PATHNAME, TY_CHAR)

	# Generate filenames.
	call iki_mkfname (oroot, oextn, Memc[ohdr_fname], SZ_PATHNAME)
	call iki_mkfname (nroot, nextn, Memc[nhdr_fname], SZ_PATHNAME)

	iferr (call fcopy (Memc[ohdr_fname], Memc[nhdr_fname]))
	    call erract (EA_WARN)

	call sfree (sp)
	status = OK
end
