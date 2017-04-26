# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMCOPY -- Fast copy of an entire image.  No fancy sections, type conversions,
# etc. are permitted if this is used.

procedure imcopy (old, new)

char	old[ARB]	# old image
char	new[ARB]	# new image

begin
	call iki_init()
	call iki_copy (old, new)
end
