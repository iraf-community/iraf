include	<imhdr.h>

# T_IMTITLE -- Set the title of an image.

procedure t_imtitle ()

char	image[SZ_FNAME]			# Image to be editted

pointer	im, immap()

begin
	# Access image.
	call clgstr ("image", image, SZ_FNAME)
	im = immap (image, READ_WRITE, 0)

	# Substitute new title.
	call clgstr ("title", IM_TITLE(im), SZ_IMTITLE)

	# Unmap image.
	call imunmap (im)
end
