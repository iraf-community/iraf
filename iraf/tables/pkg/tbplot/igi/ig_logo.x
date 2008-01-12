include <gset.h>
include <math.h>
include "igi.h"

# IG_LOGO -- Draw the Space Telescope Science Institute logo.
#
# A new WCS mapping is defined within this routine.
#
# 5/1/87, Z.G. Levay, STScI/CSC 
# Based on a Fortran/NCAR routine written by Cliff Stoll  8 nov 83 

#  8/20/91 Removed ^Ls. ZGL
## 16 June 1992  Change hard-coded QUALITY to FONTSET.  ZGL

procedure ig_logo (igs)

pointer igs		# Parameters structure descriptor

pointer	igps
int	just
real	pen_width	# Pen width
real	size		# Height of logo in NDC
real	dx, dy		# Viewport scale in WC/NDC
real	xs, ys		# Size in WC
real	ar		# Device aspect ratio
int	xsign, ysign
real	xp, yp
bool	words
int	token
pointer	tokvals

int	gettok()
real	ggetr(), gstatr()

begin
	call lcmdcat (igs, YES)
	igps = PLOT_PARMS(igs)

	pen_width = gstatr (GIO_GP(igs), G_PLWIDTH)
	size = MG_EXPAND(igps) * MG_PNTSIZE(igps)
	# Fudge size
	size = size * sqrt (2.0)

	# Device aspect ratio
	ar = ggetr (GIO_GP(igs), "ar")

	# Scale of viewport in WC/NDC
	call ggscale (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps), dx, dy)
	xs = size * abs (dx)
	ys = size * abs (dy)

	if (ar > 0.0 && ar < 1.0)
	    # Landscape
	    xs = xs * ar
	else if (ar > 1.0)
	    # Portrait
	    ys = ys / ar

	# Justification
	#    7  8  9
	#    4  5  6
	#    1  2  3

	tokvals = TOKEN_VALUE(igs)
	token = gettok (igs)

	if (token == CONSTANT) {
	    call lcmdcat (igs, NO)
	    if (LOP_TYPE(tokvals) == TY_INT)
		just = LOP_VALI(tokvals)
	    else
		just = int (LOP_VALR(tokvals))
	} else if (IS_NEWCOMMAND(token))
	    just = 5
	else {
	    call eprintf ("Invalid logo justification:  %s ")
		call pargstr (LOP_VALC(tokvals))
	    return
	}

	words = just > 0

	just  = abs (just)
	just  = max (min (just, 9), 1)
	xsign = mod (just - 1, 3) - 1
	ysign = (just - 1) / 3 - 1
	xp = MG_XPOS(igps) + (xsign - 1) * xs / 2.0
	yp = MG_YPOS(igps) + (ysign - 1) * ys / 2.0

#	call gamove (GIO_GP(igs), xp,    yp)
#	call gadraw (GIO_GP(igs), xp+xs, yp)
#	call gadraw (GIO_GP(igs), xp+xs, yp+ys)
#	call gadraw (GIO_GP(igs), xp,    yp+ys)
#	call gadraw (GIO_GP(igs), xp,    yp)

	call gsetr (GIO_GP(igs), G_PLWIDTH, MG_LWEIGHT(igps)*size*14.0)
	call gseti (GIO_GP(igs), G_CLIP, NO)
	call setltype (igs, SOLID_LINE)

	# Draw the logo
	call draw_logo (GIO_GP(igs), xp, yp, xs, ys)

	if (words) {
	    call gsetr (GIO_GP(igs), G_PLWIDTH, MG_LWEIGHT(igps)*size*7.0)
	    call label_logo (igs, xp, yp, xs, ys)
	}

	call gsetr  (GIO_GP(igs), G_PLWIDTH, pen_width)
	call gamove (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps))
	call gflush (GIO_GP(igs))
	call cmdcat (igs, YES)
end


# Draw the space telescope line-drawn logo.
# Assumes that isotropic scaling has occured;  this routine (alone)
# needs x min and y min of 1.0
# and   x max and y max of at least 67.0
# [since the logo is taller than it is wide, x max need only be 47.0 
#  but make sure that one unit of x is equal to 1 unit of y or else
#  the circles will become elllipses]

define	X_CENTER	0.5 		# Center of symmetry in x
define	BOX_LEFT	0.03125
define	BOX_RIGHT	0.96875
define	BOX_BOTTOM	0.03125
define	BOX_TOP		0.96875
define	BOX_IN_RIGHT	0.90625
define	BOX_IN_LEFT	0.09375
define	TUBE_LEFT_X	0.1458
define	TUBE_LEFT_Y	0.7708
define	TUBE_RIGHT_X	0.8542
define	TUBE_RIGHT_Y	0.7708
define	BEAM_1_LEFT_X	0.27075
define	BEAM_1_LEFT_Y	0.63623
define	BEAM_1_RIGHT_X	0.6875
define	BEAM_1_RIGHT_Y	0.9521
define	BEAM_2_LEFT_X	0.3125
define	BEAM_2_LEFT_Y	0.9521
define	SEC_RAD		0.1875		# Secondary radius
define	SEC_Y_CENT	0.96875		# Center of secondary in Y
define	PUPIL_RAD	0.40625		# Radius of entrance pupil
define	PUPIL_Y_CENT	0.96875		# Center of entrance pupil in Y
define	PRIM_RAD	0.234375	# Radius of primary
define	PRIM_Y_CENT	0.5875		# Center of primary in Y
define	EARTH_Y		0.1042
define	EARTH_CENTER	0.1875
define	EARTH_RAD	0.4792		# Radius of Earth

procedure draw_logo (gp, wx, wy, xs, ys)

pointer	gp

real	wx, wy
real	xs, ys

real	x, y

begin
	# outline the telescope with a square box:
	# start at right side of entrance pupil:
	x = BOX_IN_RIGHT * xs + wx;  y = BOX_TOP * ys + wy
	call gamove (gp, x, y)
	x = BOX_RIGHT * xs + wx
	call gadraw (gp, x, y)
	y = BOX_BOTTOM * ys + wy
	call gadraw (gp, x, y)
	x = BOX_LEFT * xs + wx
	call gadraw (gp, x, y)
	y = BOX_TOP * ys + wy
	call gadraw (gp, x, y)
	x = BOX_IN_LEFT * xs + wx
	call gadraw (gp, x, y)

	# Front end of the telescope tube
	call circle_maker (gp, wx, wy, xs, ys, 
	    PUPIL_RAD, X_CENTER, PUPIL_Y_CENT, 0.0, 361.0)

	# Secondary mirror 
	call circle_maker (gp, wx, wy, xs, ys, 
	    SEC_RAD, X_CENTER, SEC_Y_CENT, 0.0, 361.0)

	# Begin making the telescope tube
	# Pen up, then draw a straight line for the right side of
	# telescope tube: 
	x = TUBE_RIGHT_X * xs + wx;  y = TUBE_RIGHT_Y * ys + wy
	call gamove (gp, x, y)

	# keep the pen down [by sending a negative radius to circle_maker],
	# draw the bottom of the telescope tube:
	call circle_maker (gp, wx, wy, xs, ys, 
	    -1.0*PRIM_RAD, X_CENTER, PRIM_Y_CENT, 128.0, 232.0)

	# pen is still down, so just draw straight line up side of telescope:
	# draw the left side of the telescope tube:
	x = TUBE_LEFT_X * xs + wx;  y = TUBE_LEFT_Y * ys + wy
	call gadraw (gp, x, y)

	# Two interior sections of the primary and 
	# The beam of light
	# pen up, then draw the upper right section of the primary:
	call circle_maker (gp, wx, wy, xs, ys, 
	    PRIM_RAD, X_CENTER, PRIM_Y_CENT, 22.0, 78.0)

	# Leave the pen down, and draw the
	# Light path from primary to right hand side of secondary
	x = BEAM_1_RIGHT_X * xs + wx;  y = BEAM_1_RIGHT_Y * ys + wy
	call gadraw (gp, x, y)

	# From secondary to earth:
	x = X_CENTER * xs + wx;  y = EARTH_Y * ys + wy
	call gadraw (gp, x, y)

	# from earth to left hand side of secondary:
	x = BEAM_2_LEFT_X * xs + wx;  y = BEAM_2_LEFT_Y * ys + wy
	call gadraw (gp, x, y)

	# from secondary to primary:
	x = BEAM_1_LEFT_X * xs + wx;  y = BEAM_1_LEFT_Y * ys + wy
	call gadraw (gp, x, y)

	# and now the upper left section of the primary mirror:
	# and, without lifting the pen [negative radius#], draw the
	# upper left section of the primary mirror:
	call circle_maker (gp, wx, wy, xs, ys, 
	    -1.0*PRIM_RAD, X_CENTER, PRIM_Y_CENT, 282.0, 338.0)

	# Earth
	call circle_maker (gp, wx, wy, xs, ys, 
	    EARTH_RAD, X_CENTER, -EARTH_CENTER, 297.0, 423.0)
end


define	LABEL_SIZE	0.2255
define	LABEL_TOP	0.9533
define	LABEL_SEP	0.2422

procedure label_logo (igs, wx, wy, xs, ys)

pointer igs		# Parameters structure descriptor
real	wx, wy
real	xs, ys

real	size
real	xp, yp
pointer	igps

begin
	igps = PLOT_PARMS(igs)

	size = MG_EXPAND(igps) * LABEL_SIZE
	xp = wx + xs
	yp = wy + LABEL_TOP * ys
	call mgostr (igs, xp, yp, "SPACE", size, 0.0, 3, MG_FONTSET(igps))
	yp = yp - LABEL_SEP * ys
	call mgostr (igs, xp, yp, "TELESCOPE", size, 0.0, 3, MG_FONTSET(igps))
	yp = yp - LABEL_SEP * ys
	call mgostr (igs, xp, yp, "SCIENCE", size, 0.0, 3, MG_FONTSET(igps))
	yp = yp - LABEL_SEP * ys
	call mgostr (igs, xp, yp, "INSTITUTE", size, 0.0, 3, MG_FONTSET(igps))
end


# Draw a circle or segment of a circle.
# Radius of circle is given by RADIUS
# Center of circle is recevied as X_CENTER and Y_CENTER
# The segment of the arc is given from start_angle to end_angle
# both of which are to be in degrees.
# If radius is received positive, then a pen-up move will occur first.
# If radius is received negative, then pen will not be lifted before
# beginning the circle.
# in either case, the pen will remain down upon exiting this subroutine.

define	ARC_SIZE	0.03

procedure circle_maker (gp, wx, wy, xs, ys, 
	    radius, x_center, y_center, start_deg, end_deg)

pointer	gp
real	wx, wy
real	xs, ys
real	radius			# circle radius
real	x_center, y_center
real	start_deg, end_deg

real	start_rad, end_rad
real	step_rad		# Increment in angle around circle
real	radians			# local loop counter
real	x, y			# local position

begin
	start_rad = DEGTORAD (start_deg);  end_rad = DEGTORAD (end_deg)

	# X and Y position of the beginning of the circle
	x = x_center +  radius * sin (start_rad)
	x = x * xs + wx
	y = y_center +  radius * cos (start_rad)
	y = y * ys + wy
	
	# if necessary, move to the beginning:
	if (radius >= 0.0) 
	    call gamove (gp, x, y)

	step_rad = ARC_SIZE / abs (radius)

	do radians = start_rad, end_rad, step_rad {
	    # Scoot through rest of circle, step by step
	    x = x_center + abs (radius) * sin (radians)
	    x = x * xs + wx
	    y = y_center + abs (radius) * cos (radians)
	    y = y * ys + wy

	    call gadraw (gp, x, y)
	}

	# Make sure we get the last bit
	x = x_center + abs (radius) * sin (end_rad)
	x = x * xs + wx
	y = y_center + abs (radius) * cos (end_rad)
	y = y * ys + wy

	call gadraw (gp, x, y)
end
