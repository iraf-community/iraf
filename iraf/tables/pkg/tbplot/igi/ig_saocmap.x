include <ctotok.h>
include <ctype.h>
include "igi.h"

define	NONE	0
define	RED	1
define	GREEN	2
define	BLUE	3

define	NUM_COLOR	256
define	MIN_INDEX	0
define	MAX_INDEX	200
define	MIN_INTEN	0
define	MAX_INTEN	255

include <psiescape.h>

procedure ig_saocmap (igs)

#  ig_saocmap -- Parse the colormap table in the functional (nodal)
#  form written by SAOimage.  Uses gio escape to pass it to the kernel.

#  Based on SAOimage C code, see copyright notice included
#  Z. G. Levay  24 July 1992
## 7/28/92  Add optional argument to force imd graphics colors.  ZGL
## 8/7/92   Stretch map to use more of the range in case we are
##          adjusting for server graphics colors.  Fix test for using
##          graphics.  ZGL
## 8/10/92  Fixed an indexing problem in inttab() causing the colormap
##          to be screwy and probably mess up something else.  ZGL
## 8/11/92  Restored graphics colormap indexes to match the dumped
##          display raster from the servers.  ZGL
## 10/7/92  Explicitly use defines from psiescape.h so igi works in the
##          tables package.  ZGL
## 6/30/93  Replaced the guts of inttab() from the version in
##          playpen.scmapc.  These ought to be the same, but for now,
##          here we use short cmap values while scmapc uses reals.
##
## 2/18/98  Fixed the declaration to ctor() to return an INT as it should be
##	    This fixes a FLOATING POINT ERROR that was returned when using
##	    the function.  WJH

## From the original SAOimage code:
# * Copyright:	1989 Smithsonian Astrophysical Observatory
# *		You may do anything you like with this file except remove
# *		this copyright.  The Smithsonian Astrophysical Observatory
# *		makes no representations about the suitability of this
# *		software for any purpose.  It is provided "as is" without
# *		express or implied warranty.

.help
      +------ gio color index
      |
      |    +- Server (imtool/SAOimage) colors
      |    |
      |    V
      |     0  sunview background color (nominally white)
      | 1-200  frame buffer data values, windowed
      V   201  cursor color (nominally white)
      1   202  black		  0   0   0
      2   203  white		255 255 255
      3   204  red		255   0	  0
      4   205  green		  0 255   0
      5   206  blue		  0   0 255
      6   207  yellow		255 255	  0
      7   208  cyan		  0 255 255
      8   209  magenta		255   0 255
          210  coral		255 114	 86
          211  maroon		255  52 179
          212  orange		255 165   0
          213  khaki		255 246 143
          214  orchid		255 131 250
          215  turquoise	 64 224 208
          216  violet		238 130 238
          217  wheat		255 231 186
    
          218-254 reserved for use by other windows
              255 sunview foreground color (nomially black)
.endhelp

pointer	igs			# igi parameters structure

pointer	sp
pointer	saocmap
pointer	line, word
int	ip, jp
int	nch
int	scm
bool	pscf
real	value
int	color
real	rgamma, ggamma, bgamma
int	tok
bool	index
pointer	inds, ints
int	id
pointer	cmap[3]
bool	debug
pointer	tokvals			# Token value structure
int	token
int	igps
short	maxlut
int	minind, maxind		# Range of map indexes
bool	graphics		# Include graphics colors

int	gettok()
int	open() getline(), ctowrd(), fscan(), ctotok()
bool	streq()
real	ggamav()
int	ctor()

begin
	tokvals = TOKEN_VALUE(igs)
	igps = PLOT_PARMS(igs)

	call lcmdcat (igs, YES)

	token = gettok (igs)

	if (IS_NEWCOMMAND(token)) {
	    call eprintf ("No data file specified ")
	    return
	}

	# Optional argument to use graphics colors
	token = gettok (igs)
	graphics = !IS_NEWCOMMAND(token)

	call smark (sp)
	call salloc (saocmap, SZ_FNAME, TY_CHAR)

	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (word, SZ_LINE, TY_CHAR)

	call malloc (inds, NUM_COLOR, TY_REAL)
	call malloc (ints, NUM_COLOR, TY_REAL)

	# Output cmap luts
	call malloc (cmap[RED],   int(PS_IMAGE_LUT_SIZE), TY_SHORT)
	call malloc (cmap[GREEN], int(PS_IMAGE_LUT_SIZE), TY_SHORT)
	call malloc (cmap[BLUE],  int(PS_IMAGE_LUT_SIZE), TY_SHORT)

	debug = DEBUG_OUTPUT(igs) == YES

	call lcmdcat (igs, NO)
	call cmdcat  (igs, NO)

	call strcpy (LOP_VALC(tokvals), Memc[saocmap], SZ_FNAME)

	if (streq (Memc[saocmap], "STDIN"))
	    scm = STDIN
	else
	    scm = open (Memc[saocmap], READ_ONLY, TEXT_FILE)

	pscf  = false
	color = NONE
	nch   = 0
	index = true

	#  Initialize color map to zero
	call amovks (0, Mems[cmap[RED]],   NUM_COLOR)
	call amovks (0, Mems[cmap[GREEN]], NUM_COLOR)
	call amovks (0, Mems[cmap[BLUE]],  NUM_COLOR)

	while (fscan (scm) != EOF) {
	    # For each line

	    call gargwrd (Memc[word], SZ_LINE)

	    if (debug) {
		call eprintf ("Word A: %s\n")
		    call pargstr (Memc[word])
	    }

	    if (streq (Memc[word], "PSEUDOCOLOR")) {
		pscf = true
		break
	    }
	}

	if (!pscf) {
	    call close (scm)
	    call error (0, "PSEUDOCOLOR keyword not found")
	}

	maxlut = PS_MAX_LUT_VALUE
	minind = MIN_INDEX

	if (graphics)
	    maxind = MAX_INDEX
	else
	    maxind = NUM_COLOR - 1

	MG_CMMIN(igps) = real(minind)
	MG_CMMAX(igps) = real(maxind)

	while (getline (scm, Memc[line]) != EOF) {
	    # Scan lines for Color keyword, RED, GREEN, or BLUE

	    if (debug) {
		call eprintf("Scanned Line: %s\n")
			call pargstr(Memc[line])
	    }
	    ip = 1
	    nch = ctowrd (Memc[line], ip, Memc[word], SZ_LINE)

	    if (debug) {
		call eprintf ("Word B: %s\n")
		    call pargstr (Memc[word])
	    }

	    if (streq (Memc[word], "RED:")) {
		color = RED
		rgamma = ggamav (Memc[line], ip, debug)
		id = 0

	    } else if (streq (Memc[word], "GREEN:")) {
	
		call inttab (Memr[inds], Memr[ints], id,
		    Mems[cmap[color]], NUM_COLOR, rgamma,
		    maxlut, minind, maxind, debug)

		color = GREEN
		ggamma = ggamav (Memc[line], ip, debug)
		id = 0

	    } else if (streq (Memc[word], "BLUE:")) {
		call inttab (Memr[inds], Memr[ints], id,
		    Mems[cmap[color]], NUM_COLOR, ggamma,
		    maxlut, minind, maxind, debug)

		color = BLUE
		bgamma = ggamav (Memc[line], ip, debug)
		id = 0

	    } else if (Memc[word] == '\#')
		next

	    else {
		# Working on a color
		ip = 1
		repeat {	# To end of line
		    tok = ctotok (Memc[line], ip, Memc[word], SZ_LINE)

		    if (debug) {
			call eprintf ("Token: %s  %d %d  TOK: %d \n")
			    call pargstr (Memc[word])
			    call pargi (nch)
			    call pargi (ip)
			    call pargi(tok)
			call eprintf("TOK_NUMBER = %d\n")
				call pargi(TOK_NUMBER)
		    }

		    if (tok == TOK_NUMBER) {
			jp = 1

			nch = ctor (Memc[word], jp, value)

			if (index) {
			    #  An index value
			    id = id + 1
			    call fillim (Memr[inds], id, value)
			    index = false

			} else {
			    #  An intensity value
			    call fillim(Memr[ints], id, value)
			    index = true
			}

			if (debug) {
			    call eprintf ("Value: %f  %d %d\n")
				call pargr (value)
				call pargi (nch)
				call pargi (ip)
			}
		    }

		} until (tok == TOK_NEWLINE)
	    }
	}

	# Fill the last (Blue) color map line
	call inttab (Memr[inds], Memr[ints], id,
	    Mems[cmap[color]], NUM_COLOR, bgamma,
	    maxlut, minind, maxind, debug)

	if (graphics)
	    # Force "standard" graphics colors (1,201:217)
	    # to make imd graphics correct
	    call grfcolor (Mems[cmap[RED]], Mems[cmap[GREEN]],
		Mems[cmap[BLUE]], NUM_COLOR,
		rgamma, ggamma, bgamma, maxlut)

	if (debug)
	    # List the colormap values on STDOUT
	    call listcm (Mems[cmap[RED]], Mems[cmap[GREEN]],
		Mems[cmap[BLUE]], NUM_COLOR)

	#  Send the lut to the kernel
	#  Red
	call gescape (GIO_GP(igs), int(PS_IMAGE_RED_LUT), Mems[cmap[RED]],
	    int(PS_IMAGE_LUT_SIZE))

	#  Green
	call gescape (GIO_GP(igs), int(PS_IMAGE_GREEN_LUT), Mems[cmap[GREEN]],
	    int(PS_IMAGE_LUT_SIZE))

	#  Blue
	call gescape (GIO_GP(igs), int(PS_IMAGE_BLUE_LUT), Mems[cmap[BLUE]],
	    int(PS_IMAGE_LUT_SIZE))

	call mfree (inds, TY_REAL)
	call mfree (ints, TY_REAL)

	call mfree (cmap[RED],   TY_SHORT)
	call mfree (cmap[GREEN], TY_SHORT)
	call mfree (cmap[BLUE],  TY_SHORT)

	call sfree (sp)

	call close (scm)
end


real procedure ggamav (line, ip, debug)

char	line[ARB]
int	ip
bool	debug

int	nch
real	gamma
char	word[SZ_LINE]

int	ctowrd()
int     ctor()
bool	streq()

begin
	nch = ctowrd (line, ip, word, SZ_LINE)

	gamma = 1.0

	if (streq (word, "gamma")) {
	    nch = ctor (line, ip, gamma)

	    if (debug) {
		call eprintf ("gamma: %d %d %f\n")
		    call pargi (ip)
		    call pargi (nch)
		    call pargr (gamma)
	    }
	}

	return (gamma)
end


procedure fillim (vec, id, val)

real	vec[ARB]
int	id
real	val

begin
	vec[id] = val
end


procedure inttab (inds, ints, nnode, cmap, cmapsize,
    gamma, maxlut, minind, maxind, debug)

real	inds[ARB]
real	ints[ARB]
int	nnode
short	cmap[ARB]
int	cmapsize
real	gamma
short	maxlut
int	minind, maxind
bool	debug

real	x, x1, x2, dx
real	y, y1, y2, dy

int	i, i1, i2, di
int	dj

real	sl
int	node
real	b

begin
	i1 = minind
	i2 = maxind
	di = real (i2 - i1)

	node = 1

	x1 = inds[node]
	x2 = inds[node+1]
	dx = x2 - x1
	
	y1 = ints[node]
	y2 = ints[node+1]
	dy = y2 - y1
	
	if (debug) {
	    call eprintf("INTTAB: Got this far, dx=%g, dy=%g ...\n")
		call pargr(dx)
		call pargr(dy)
	}

	sl = dy / dx
	b  = y1 - sl * x1

	if (debug) {
	    call eprintf ("Node %d\n")
		call pargi (node)

	    call eprintf ("X: %f %f %f %d\n")
		call pargr (x1)
		call pargr (x2)
		call pargr (dx)
		call pargi (di)

	    call eprintf ("Y: %f %f %f %d\n")
		call pargr (y1)
		call pargr (y2)
		call pargr (dy)
		call pargi (dj)

	    call eprintf ("Slope %f Intercept %f\n")
		call pargr (sl)
		call pargr (b)
	}

	do i = i1, i2 {
	    x = real (i - i1) / di

	    if (x <= x1)
		y = y1

	    else if (x > x2) {
		if ((node + 1) == nnode)
		    y = y2

		else {
		    node = node + 1
	
		    x1 = inds[node]
		    x2 = inds[node+1]
		    dx = x2 - x1
	
		    y1 = ints[node]
		    y2 = ints[node+1]
		    dy = y2 - y1
	
		    sl = dy / dx
		    b  = y1 - sl * x1

		    if (debug) {
			call eprintf ("Node %d\n")
			    call pargi (node)

			call eprintf ("X: %f %f %f %d\n")
			    call pargr (x1)
			    call pargr (x2)
			    call pargr (dx)
			    call pargi (di)

			call eprintf ("Y: %f %f %f %d\n")
			    call pargr (y1)
			    call pargr (y2)
			    call pargr (dy)
			    call pargi (dj)

			call eprintf ("Slope %f Intercept %f\n")
			    call pargr (sl)
			    call pargr (b)
		    }

		    y = x * sl + b
		}

	    } else {

		y = x * sl + b
	    }

	    y = min (max (y, 0.0), 1.0)
	    y = y ** (1.0 / gamma)

	    cmap[i+1] = short (y * real (maxlut))

	    if (debug) {
		call printf ("%d  x: %f;  y: %f\n")
		    call pargi (i)
		    call pargr (x)
		    call pargr (y)
		call flush (STDOUT)
	    }
	}
end


procedure listcm (red, green, blue, cmsiz)

short	red[ARB], green[ARB], blue[ARB]
int	cmsiz

int	i

begin
	do i = 1, cmsiz {
	    call printf ("%4d%5d%5d%5d\n")
		call pargi (i)
		call pargs (red[i])
		call pargs (green[i])
		call pargs (blue[i])
	}
end


procedure grfcolor (red, green, blue, ncol, rgamma, ggamma, bgamma, maxlut)

#  GRFCOLOR -- force the 18 graphics colormap values to match the display
#  server (imtool/SAOimage) graphics colors.  All of the index
#  locations must be the same so the values in a dumped display raster
#  match their colormap values.

short	red[ARB], green[ARB], blue[ARB]
int	ncol
real	rgamma, ggamma, bgamma
short	maxlut

short	apgamma()

#define	APGAMMA "(short((($1)*real(maxlut))**(1.0/$1)))"

begin
        #      0 = sunview background color (normally white)
	red[1]     = maxlut
	green[1]   = maxlut
	blue[1]    = maxlut

        #    201 = cursor color (white)
	red[202]   = maxlut
	green[202] = maxlut
	blue[202]  = maxlut

       #    202 black:		  0   0   0
	red[203]   = 0
	green[203] = 0
	blue[203]  = 0

        #    203 white:		255 255 255
	red[204]   = maxlut
	green[204] = maxlut
	blue[204]  = maxlut

        #    204 red:		255   0	  0
	red[205]   = maxlut
	green[205] = 0
	blue[205]  = 0

        #    205 green:		  0 255   0
	red[206]   = 0
	green[206] = maxlut
	blue[206]  = 0

        #    206 blue:		  0   0 255
	red[207]   = 0
	green[207] = 0
	blue[207]  = maxlut

        #    207 yellow:	255 255   0
	red[208]   = maxlut
	green[208] = maxlut
	blue[208]  = 0

        #    208 cyan:		  0 255 255
	red[209]   = 0
	green[209] = maxlut
	blue[209]  = maxlut

        #    209 magenta:	255   0 255
	red[210]   = maxlut
	green[210] = 0
	blue[210]  = maxlut

        #    210 coral:		255 114	 86
	red[211]   = maxlut
	green[211] = apgamma (0.447, ggamma, maxlut)
	blue[211]  = apgamma (0.337, bgamma, maxlut)

        #    211 maroon:	255  52 179
	red[212]   = maxlut
	green[212] = apgamma (0.204, ggamma, maxlut)
	blue[212]  = apgamma (0.702, bgamma, maxlut)

        #    212 orange:	255 165   0
	red[213]   = maxlut
	green[213] = apgamma (0.647, ggamma, maxlut)
	blue[213]  = 0

        #    213 khaki:		255 208 143
	red[214]   = maxlut
	green[214] = apgamma (0.965, ggamma, maxlut)
	blue[214]  = apgamma (0.561, bgamma, maxlut)

        #    214 orchid:	255 131 250
	red[215]   = maxlut
	green[215] = apgamma (0.514, ggamma, maxlut)
	blue[215]  = apgamma (0.980, bgamma, maxlut)

        #    215 turquoise:	 64 224 208
	red[216]   = apgamma (0.213, rgamma, maxlut)
	green[216] = apgamma (0.878, ggamma, maxlut)
	blue[216]  = apgamma (0.816, bgamma, maxlut)

        #    216 violet:	238 130 238
	red[217]   = apgamma (0.933, rgamma, maxlut)
	green[217] = apgamma (0.510, ggamma, maxlut)
	blue[217]  = apgamma (0.933, bgamma, maxlut)

        #    217 wheat:		255 231 186
	red[218]   = maxlut
	green[218] = apgamma (0.906, ggamma, maxlut)
	blue[218]  = apgamma (0.729, bgamma, maxlut)

        #    255 = black (sunview foreground color)
	red[256]   = 0
	green[256] = 0
	blue[256]  = 0
end


short procedure apgamma (value, gamma, maxlut)

real	value
real	gamma
short	maxlut

begin
	return short (real (maxlut) * (value ** (1.0/gamma)))
end
