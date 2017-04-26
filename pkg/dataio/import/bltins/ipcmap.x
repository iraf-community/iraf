include "../import.h"

# IPCMAP.X -- Procedures for colormap application or lookup.


# IP_GRAY_CMAP - Apply the colormap to an array of pixels and convert the
# pixels to grayscale using the NTSC formula.

procedure ip_gray_cmap (data, len, cmap)

char	data[ARB]				#i pixel values
int	len					#i how many of 'em
pointer	cmap					#i colormap pointer

int	i
short	val, ip_gcmap_val()

begin
	do i = 1, len {
            val = data[i] + 1
	    data[i] = ip_gcmap_val (val, cmap)
	}
end


# IP_GCMAP_VAL - Apply the colormap to a single pixel and convert the
# result to grayscale using the NTSC formula.

short procedure ip_gcmap_val (pix, cmap)

char	pix					#i pixel value
pointer	cmap					#i colormap pointer

short	val

begin
        val = (R_COEFF * CMAP(cmap,IP_RED,pix) +
               G_COEFF * CMAP(cmap,IP_GREEN,pix) +
               B_COEFF * CMAP(cmap,IP_BLUE,pix))
	return (val)
end


# IP_RGB_VAL - Given a grayscale value figure out what the requested color
# component is from the colormap.

short procedure ip_rgb_val (pix, cmap, color)

char	pix					#i pixel value
pointer	cmap					#i colormap pointer
int	color					#i requested color

short	i, val

begin
	# Need to optimize this later...  For now just compute the colormap
	# grayscale values until we find a match and use the index.
	i = 0
	val = -1
	while (val != pix && i <= 256) {
	    i = i + 1
            val = (R_COEFF * CMAP(cmap,IP_RED,i) +
                   G_COEFF * CMAP(cmap,IP_GREEN,i) +
                   B_COEFF * CMAP(cmap,IP_BLUE,i))
	}

	switch (color) {
	case IP_RED:
	    val = CMAP(cmap,IP_RED,i-1)
	case IP_GREEN:
	    val = CMAP(cmap,IP_GREEN,i-1)
	case IP_BLUE:
	    val = CMAP(cmap,IP_BLUE,i-1)
	}
	return (val)
end
