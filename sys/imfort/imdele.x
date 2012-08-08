# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMDELE -- Delete an image (both the header file and the pixel file).
# It is not an error if there is no pixel file.

procedure imdele (image, ier)

%	character*(*) image
int	ier			# receives error status

pointer	sp, imname

begin
	call smark (sp)
	call salloc (imname, SZ_PATHNAME, TY_CHAR)

	call f77upk (image, Memc[imname], SZ_PATHNAME)
	call imdelx (Memc[imname], ier)

	call sfree (sp)
end
