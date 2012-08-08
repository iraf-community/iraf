# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IM_INIT_NEWIMAGE -- Initialize the header of a new image.

procedure im_init_newimage (im, len_imhdr)

pointer	im
int	len_imhdr
long	clktime()

begin
	call strcpy ("imhdr", IM_MAGIC(im), SZ_IMMAGIC)
	IM_HDRLEN(im) = len_imhdr
	IM_PIXTYPE(im) = DEF_PIXTYPE
	IM_CTIME(im) = clktime (long(0))
	IM_MTIME(im) = IM_CTIME(im)
	IM_TITLE(im) = EOS
	IM_HISTORY(im) = EOS
	Memc[IM_USERAREA(im)] = EOS
end
