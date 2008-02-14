include <lexnum.h>
include "export.h"


define	EX_COLORMAPS	"|aips0|blue|color|grayscale|greyscale|green|halley\
			 |heat|rainbow|red|staircase|standard|overlay|"

define	AIPS0		1		# builtin colormaps
define  BLUE		2
define  COLOR		3
define  GRAYSCALE	4
define  GREYSCALE	5
define  GREEN		6
define  HALLEY		7
define  HEAT		8
define  RAINBOW		9
define  RED		10
define  STAIRCASE	11
define  STANDARD	12
define  OVERLAY		13


# EX_READ_CMAP - Read a colormap into the colormap structure.  We assume the
# colormap is either a normalized CLT of RGB values between zero and one, or
# RGB integer values between 0 and 255.  The format of the file is three 
# values per line given as a red, green, and blue color.  If the first line
# contains a single number assume it's the number of colors.  A maximum of
# 256 colors will be read, if fewer values are read the remaining colors will 
# be filled with zeros.

procedure ex_read_cmap (ex, cmname)

pointer	ex					#i colormap pointer
char	cmname[ARB]				#i colormap file name

pointer	cmap
pointer	sp, line
real	r, g, b, scale
int	i, stat, fd, type, ncolors

int	open(), fscan(), nscan()
int	getline(), lexnum(), strdic()
errchk	open

define	rdmap_		99

begin
	# See if this is a builtin colormap request.
	if (strdic(cmname,cmname,SZ_LINE,EX_COLORMAPS) > 0) {
	    call ex_bltin_cmap (ex, cmname)
	    return
	}

	# Open the colormap filename.
	iferr (fd = open (cmname, READ_ONLY, TEXT_FILE))
	    call error (0, "Cannot open requested colormap file.")

	# Check the first line to see if it's the number of colors or a
	# CLT entry.
	stat = fscan (fd)
    	    call gargr (r)
    	    call gargr (g)
    	    call gargr (b)
	if (nscan() == 1) {
	    ncolors = r
	    goto rdmap_
	} else if (nscan() == 3) {
	    call seek (fd, BOF)
rdmap_	    call smark (sp)
	    call salloc (line, SZ_LINE, TY_CHAR)
	    stat = getline (fd, Memc[line])
	    i = 1
	    ncolors = 256
	    type = lexnum (Memc[line], i, stat)

	    if (type == LEX_REAL)
		scale = 255.0
	    else if (type == LEX_DECIMAL)
		scale = 1.0
	    else
	        call error (0, "Colormap file has an unknown format.")

	    call sfree (sp)
	} else
	    call error (1, "Colormap file has an unknown format.")

	# Read in a normalize colormap file.
	cmap = EX_CMAP(ex)
	for (i=1; fscan(fd) != EOF && i <= ncolors; i=i+1) {
    	    call gargr (r)
    	    call gargr (g)
    	    call gargr (b)

    	    CMAP(cmap,EX_RED,i)   = max (0, min (255, int (r * scale + 0.5)))
    	    CMAP(cmap,EX_GREEN,i) = max (0, min (255, int (g * scale + 0.5)))
    	    CMAP(cmap,EX_BLUE,i)  = max (0, min (255, int (b * scale + 0.5)))
	}
	ncolors = i
	EX_NCOLORS(ex) = ncolors
	
	# Close the file.
	call close (fd)
end


# EX_SCALE_CMAP - Scale the colormap with the requested brightness and
# contrast values.

procedure ex_scale_cmap (cmap, ncolors, brightness, contrast)

pointer	cmap				#i colormap pointer
int	ncolors				#i number of colors in map
real	brightness			#i brightness offset
real	contrast			#i contrast scale

pointer	sp, ctmp
int	i, c1, c2
short 	r, g, b
real	x, y, z, frac, slope, offset

begin
	call smark (sp)
	call salloc (ctmp, 3*CMAP_SIZE, TY_CHAR)
	call aclrc (Memc[ctmp], 3*CMAP_SIZE)

	slope = max (-7.0, min (7.0, contrast))
	offset = max (0.0, min (1.0, brightness))

     	# Compute the scaled colormap.
	do i = 1, ncolors {
             x = real (i) / real (ncolors)
             y = (x - offset) * slope + 0.5
 
             if (y <= 0.0) {
                 r = CMAP(cmap,EX_RED,  1)
                 g = CMAP(cmap,EX_GREEN,1)
                 b = CMAP(cmap,EX_BLUE, 1)
             } else if (y >= 1.0) {
                 r = CMAP(cmap,EX_RED,  ncolors)
                 g = CMAP(cmap,EX_GREEN,ncolors)
                 b = CMAP(cmap,EX_BLUE, ncolors)
             } else {
                 z = y * (ncolors - 1)
                 c1 = max (1, int (z))
                 c2 = min (ncolors-1, c1 + 1)
                 frac = z - c1
                 r = CMAP(cmap,EX_RED,c1) * (1.0 - frac) + 
		     CMAP(cmap,EX_RED,c2) * frac
                 g = CMAP(cmap,EX_GREEN,c1) * (1.0 - frac) + 
		     CMAP(cmap,EX_GREEN,c2) * frac
                 b = CMAP(cmap,EX_BLUE,c1) * (1.0 - frac) + 
		     CMAP(cmap,EX_BLUE,c2) * frac
             }
     
	     CMAP(ctmp,EX_RED,  i) = r
	     CMAP(ctmp,EX_GREEN,i) = g
	     CMAP(ctmp,EX_BLUE, i) = b
        }
	call amovc (Memc[ctmp], Memc[cmap], 3*CMAP_SIZE)

	call sfree (sp)
end


# EX_BLTIN_CMAP - Load a predefined colormap.

procedure ex_bltin_cmap (ex, cmname)

pointer	ex				#i task struct pointer
char	cmname[ARB]			#i colormap name

pointer	cmap
int	i, j, strdic()

include "cmaps.inc"

begin
	j = 1
	cmap = EX_CMAP(ex)
	EX_NCOLORS(ex) = CMAP_SIZE

	switch (strdic (cmname, cmname, SZ_LINE, EX_COLORMAPS)) {
	case AIPS0:
	    do i = 1, 256 {
                CMAP(cmap,EX_RED,i)   = aips0[j]
                CMAP(cmap,EX_GREEN,i) = aips0[j+1]
                CMAP(cmap,EX_BLUE,i)  = aips0[j+2]
		j = j + 3
	    }
	case BLUE:
	    call aclrs (Mems[cmap], 3*CMAP_SIZE)
	    do i = 1, 256
                CMAP(cmap,EX_BLUE,i)   = i - 1
	case COLOR:
	    do i = 1, 256 {
                CMAP(cmap,EX_RED,i)   = color[j]
                CMAP(cmap,EX_GREEN,i) = color[j+1]
                CMAP(cmap,EX_BLUE,i)  = color[j+2]
		j = j + 3
	    }
	case GRAYSCALE, GREYSCALE:
	    do i = 1, 256 {
                CMAP(cmap,EX_RED,i)   = i - 1
                CMAP(cmap,EX_GREEN,i) = i - 1
                CMAP(cmap,EX_BLUE,i)  = i - 1
	    }
	case GREEN:
	    call aclrs (Mems[cmap], 3*CMAP_SIZE)
	    do i = 1, 256
                CMAP(cmap,EX_GREEN,i)   = i - 1
	case HALLEY:
	    do i = 1, 256 {
                CMAP(cmap,EX_RED,i)   = halley[j]
                CMAP(cmap,EX_GREEN,i) = halley[j+1]
                CMAP(cmap,EX_BLUE,i)  = halley[j+2]
		j = j + 3
	    }
	case HEAT:
	    do i = 1, 256 {
                CMAP(cmap,EX_RED,i)   = heat[j]
                CMAP(cmap,EX_GREEN,i) = heat[j+1]
                CMAP(cmap,EX_BLUE,i)  = heat[j+2]
		j = j + 3
	    }
	case RAINBOW:
	    do i = 1, 256 {
                CMAP(cmap,EX_RED,i)   = rainbow[j]
                CMAP(cmap,EX_GREEN,i) = rainbow[j+1]
                CMAP(cmap,EX_BLUE,i)  = rainbow[j+2]
		j = j + 3
	    }
	case RED:
	    call aclrs (Mems[cmap], 3*CMAP_SIZE)
	    do i = 1, 256
                CMAP(cmap,EX_RED,i)   = i - 1
	case STAIRCASE:
	    do i = 1, 256 {
                CMAP(cmap,EX_RED,i)   = staircase[j]
                CMAP(cmap,EX_GREEN,i) = staircase[j+1]
                CMAP(cmap,EX_BLUE,i)  = staircase[j+2]
		j = j + 3
	    }
	case STANDARD:
	    do i = 1, 256 {
                CMAP(cmap,EX_RED,i)   = standard[j]
                CMAP(cmap,EX_GREEN,i) = standard[j+1]
                CMAP(cmap,EX_BLUE,i)  = standard[j+2]
		j = j + 3
	    }
	case OVERLAY:
	    do i = 1, 256 {
                CMAP(cmap,EX_RED,i)   = overlay[j]
                CMAP(cmap,EX_GREEN,i) = overlay[j+1]
                CMAP(cmap,EX_BLUE,i)  = overlay[j+2]
		j = j + 3
	    }
	}
end
