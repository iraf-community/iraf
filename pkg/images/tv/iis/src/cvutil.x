# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gki.h>
include	<imhdr.h>
include	"cv.h"
include	"../lib/ids.h"

# CVUTIL -- utility control routines for cv package

############ CLEAR display ############
# CVCLEARG -- clear all of graphics (bit) planes

procedure cvclearg (frame, color)

short	frame[ARB]
short	color[ARB]

int	count
int	cv_move()

include "cv.com"

begin
	count = cv_move (frame, Mems[cv_stack])
	count = count + cv_move (color, Mems[cv_stack+count])
	call gescape (cv_gp, IDS_SET_GP, Mems[cv_stack], count)
	call gclear (cv_gp)
end

# CVCLEARI -- clear specified image frames

procedure cvcleari (frames)

short	frames[ARB]

include "cv.com"

begin
	call cv_iset (frames)
	call gclear (cv_gp)
end

############ CURSOR and BUTTON ############
# CV_RDBUT -- read button on trackball (or whatever)
# if none pressed, will get zero back

int procedure cv_rdbut()

int	oldcnum
real	x, y
int	button
int	gstati

include "cv.com"

begin
	oldcnum = gstati (cv_gp, G_CURSOR)
	call gseti (cv_gp, G_CURSOR, IDS_BUT_RD)
	call ggcur (cv_gp, x, y, button)
	call gseti (cv_gp, G_CURSOR, oldcnum)
	return(button)
end

# CV_WTBUT -- wait for button to be pressed, then read it

int procedure cv_wtbut()

int	oldcnum
real	x, y
int	button
int	gstati

include "cv.com"

begin
	oldcnum = gstati (cv_gp, G_CURSOR)
	call gseti (cv_gp, G_CURSOR, IDS_BUT_WT)
	call ggcur (cv_gp, x, y, button)
	call gseti (cv_gp, G_CURSOR, oldcnum)
	return(button)
end

# CV_RCUR -- read cursor.  The cursor read/set routines do not restore
# the cursor number...this to avoid numerous stati/seti calls that
# usually are not needed.

procedure cv_rcur (cnum, x, y)

int	cnum
real	x,y
int	junk

include	"cv.com"

begin
	call gseti (cv_gp, G_CURSOR, cnum)
	call ggcur (cv_gp, x, y, junk)
end

# CV_SCUR -- set cursor

procedure cv_scur (cnum, x, y)

int	cnum
real	x,y

include "cv.com"

begin
	call gseti (cv_gp, G_CURSOR, cnum)
	call gscur (cv_gp, x, y)
end


# CV_RCRAW -- read the raw cursor (return actual screen coordinates).

procedure cv_rcraw (x, y)

real	x,y

include	"cv.com"

begin
	call cv_rcur (IDS_CRAW, x, y)
end

# CV_SCRAW -- set raw cursor

procedure cv_scraw (x, y)

real	x,y

include "cv.com"

begin
	call cv_scur (IDS_CRAW, x, y)
end


# cvcur -- turn cursor on or off

procedure cvcur (instruction)

int	instruction

include "cv.com"

begin
	Mems[cv_stack]   = IDS_CURSOR
	Mems[cv_stack+1] = IDS_WRITE
	Mems[cv_stack+2] = 1
	Mems[cv_stack+3] = IDS_EOD
	Mems[cv_stack+4] = IDS_EOD
	Mems[cv_stack+5] = 1
	Mems[cv_stack+6] = instruction
	call gescape (cv_gp, IDS_CONTROL, Mems[cv_stack], 7)
end

############ DISPLAY ############
# cvdisplay

procedure cvdisplay (instruction, device, frame, color, quad)

int	instruction
int	device
short	frame, color, quad

int	i
int	cv_move()

include	"cv.com"

begin
	Mems[cv_stack] = instruction
	i = cv_move (frame, Mems[cv_stack+1])
	i = i + cv_move (color, Mems[cv_stack+1+i])
	i = i + cv_move (quad, Mems[cv_stack+1+i])
	call gescape (cv_gp, device, Mems[cv_stack], 1+i)
end

############ MATCH ############
# cvmatch -- build match escape sequence

procedure cvmatch (lt, fr, cr, frames, color)

int	lt			# type
short	fr[ARB]			# reference frame and color
short	cr[ARB]
short	frames[ARB]		# frames to be changed
short	color[ARB]		# and colors

int 	count, n
int	cv_move()

include	"cv.com"

begin
	Mems[cv_stack] = IDS_MATCH
	Mems[cv_stack+1] = lt
	count = cv_move (fr, Mems[cv_stack+3])
	count = count + cv_move (cr, Mems[cv_stack+3+count])
	n = count
	Mems[cv_stack+count+3] = 0		# unused offset
	count = count + cv_move (frames, Mems[cv_stack+4+count])
	count = count + cv_move (color, Mems[cv_stack+4+count])
	Mems[cv_stack+2] = count - n
	call gescape (cv_gp, IDS_CONTROL, Mems[cv_stack], count+4)
end

############ OFFSET ############
# cvoffset -- set offset registers

procedure cvoffset( color, data)

short	color[ARB]
short	data[ARB]

int	count, cv_move()
int	i

include	"cv.com"

begin
	Mems[cv_stack] = IDS_OUT_OFFSET
	Mems[cv_stack+1] = IDS_WRITE
	Mems[cv_stack+3] = IDS_EOD	# no-op the frames slot
	count = cv_move (color, Mems[cv_stack+4])
	Mems[cv_stack+4+count] = 1	# (unused) offset
	i = cv_move (data, Mems[cv_stack+5+count])
	i = i - 1			# don't include EOD of "data"
	Mems[cv_stack+2] = i
	call gescape (cv_gp, IDS_CONTROL, Mems[cv_stack], i+count+5)
end

############ PAN ############
# cvpan -- move the image(s) around
# The x,y coordinates are NDC that, it is assumed, came from a cursor
# read, and therefore are of the form
#     ((one_based_pixel-1)/(resolution)) *(GKI_MAXNDC+1)  /  GKI_MAXNDC
# The division by GKI_MAXNDC turns into NDC what was GKI ranging from
# 0 through 511*64 (for IIS) which conforms to the notion of specifying
# each pixel by its left/bottom GKI boundary.

procedure cvpan (frames, x, y)

short	frames[ARB]
real	x,y		# position in NDC

int	count, cv_move()

include	"cv.com"

begin
	Mems[cv_stack] = IDS_SCROLL
	Mems[cv_stack+1] = IDS_WRITE
	Mems[cv_stack+2] = 3
	count = cv_move (frames, Mems[cv_stack+3])
	Mems[cv_stack+3+count] = IDS_EOD	# all colors
	Mems[cv_stack+4+count] = 1		# (unused) offset
	Mems[cv_stack+5+count] = x * GKI_MAXNDC
	Mems[cv_stack+6+count] = y * GKI_MAXNDC
	Mems[cv_stack+7+count] = IDS_EOD	# for all frames
	call gescape (cv_gp, IDS_CONTROL, Mems[cv_stack], count+8)
end

############ RANGE ############
# cvrange -- scale ouput before final look up table

procedure cvrange ( color, range)

short	color[ARB]
short	range[ARB]

int	cv_move(), count, i

include	"cv.com"

begin
	Mems[cv_stack] = IDS_RANGE
	Mems[cv_stack+1] = IDS_WRITE
	Mems[cv_stack+3] = IDS_EOD		# all frames
	count = cv_move (color, Mems[cv_stack+4])
	Mems[cv_stack+4+count] = 1		# (unused) offset
	i = cv_move (range, Mems[cv_stack+5+count])
	i = i - 1 				# don't include EOD of "range"
	Mems[cv_stack+2] = i
	call gescape (cv_gp, IDS_CONTROL, Mems[cv_stack], i+count+5)
end

############ RESET display ############
# cvreset -- reset display
#  SOFT   -- everything but lookup tables and image/graphics planes
# MEDIUM  -- everything but image/graphics planes
#  HARD   -- everything...planes are cleared, all images OFF

procedure cvreset (hardness)

int	hardness

include	"cv.com"

begin
	Mems[cv_stack] = hardness
	call gescape (cv_gp, IDS_RESET, Mems[cv_stack], 1)
end


############ SNAP a picture ############
# cvsnap -- takes a full picture of image display

procedure cvsnap (fname, snap_color)

char	fname[ARB]		# image file name
int	snap_color

pointer	im, immap(), impl2s()
int	i, factor
real	y

include	"cv.com"

begin
	im = immap(fname, NEW_FILE, 0)
	IM_PIXTYPE(im) = TY_SHORT
	IM_LEN(im,1) = cv_xres
	IM_LEN(im,2) = cv_yres

	Mems[cv_stack] = IDS_SNAP
	Mems[cv_stack+1] = IDS_WRITE
	Mems[cv_stack+2] = 1		# frame, color are not relevant
	Mems[cv_stack+3] = IDS_EOD
	Mems[cv_stack+4] = IDS_EOD
	Mems[cv_stack+5] = 0
	Mems[cv_stack+6] = snap_color
	call gescape (cv_gp, IDS_CONTROL, Mems[cv_stack], 7)

	factor = cv_yres/10 + 1
	call eprintf ("  (%% done: ")
	call flush (STDERR)
	do i = 0, cv_yres-1 {
	    if ( mod(i,factor) == 0) {
		call eprintf ("%d ")
		    call pargi (int(10*i/cv_yres)*10)
		    call flush (STDERR)
	    }
	    y = real(i)*cv_ycon / GKI_MAXNDC.
	    call ggcell (cv_gp, Mems[impl2s(im,i+1)], cv_xres, 1, 0.0,
	                  y, 1.0, y)
	}
	call eprintf ("100)\n")
	
	call imunmap(im)
	Mems[cv_stack] = IDS_R_SNAPDONE
	call gescape (cv_gp, IDS_RESET, Mems[cv_stack], 1)
end

############ SPLIT ############
# cvsplit -- set split screen position

procedure cvsplit (x, y)

real	x,y			# NDC coordinates

include	"cv.com"

begin
	Mems[cv_stack] = IDS_SPLIT
	Mems[cv_stack+1] = IDS_WRITE
	Mems[cv_stack+2] = 2
	Mems[cv_stack+3] = IDS_EOD		# no-op frame and color
	Mems[cv_stack+4] = IDS_EOD
	Mems[cv_stack+5] = 1			# (unused) offset
	# NOTE multiplacation by MAXNDC+1 ... x, and y, are never == 1.0
	#    ( see split.x)
	# and truncation effects will work out just right, given what the
	# image display kernel does with these numbers
	Mems[cv_stack+6] = x * (GKI_MAXNDC+1)
	Mems[cv_stack+7] = y * (GKI_MAXNDC+1)
	call gescape (cv_gp, IDS_CONTROL, Mems[cv_stack], 8)
end

############ TEXT ############
# Write text

procedure cvtext (x, y, text, size)

real	x, y, size
char	text[ARB]

char	format[SZ_LINE]

include	"cv.com"

begin
	call sprintf (format, SZ_LINE, "s=%f")
	    call pargr (size)
	call gtext (cv_gp, x, y, text, format)
end

############ WHICH ############
# Tell which frames are one.  The best we can do now is
# tell if any, and if so, which is the "first"

procedure cvwhich (fr)

short	fr[ARB]

real	x,y
int	cnum, oldcnum
int	gstati

include	"cv.com"

begin
	# Use here the fact that if cursor number is zero, the
	# kernel will return the number of the first displayed
	# frame, or "ERR" if none.
	oldcnum = gstati (cv_gp, G_CURSOR)
	cnum = 0
	call gseti (cv_gp, G_CURSOR, cnum)
	call ggcur (cv_gp, x, y, cnum)
	call gseti (cv_gp, G_CURSOR, oldcnum)
	fr[1] = cnum
	fr[2] = IDS_EOD
end

############ WLUT ############
# cvwlut ... change lookup tables
# the data is in form of line endpoints.

procedure cvwlut (device, frames, color, data, n)

int	device
short	frames[ARB]
short	color[ARB]
short	data[ARB]
int	n

int	count, cv_move()

include	"cv.com"

begin
	# Device had better refer to a look-up table, or who knows
	# what will happen!
	Mems[cv_stack] = device
	Mems[cv_stack+1] = IDS_WRITE
	Mems[cv_stack+2] = n
	count = cv_move (frames, Mems[cv_stack+3])
	count = count + cv_move (color, Mems[cv_stack+3+count])
	Mems[cv_stack+3+count] = 1		# (unused) offset
	call amovs (data, Mems[cv_stack+count+4],n)
	call gescape (cv_gp, IDS_CONTROL, Mems[cv_stack], n+count+4)
end

############ ZOOM ############
# cvzoom -- zoom the image
# See comment under PAN about x and y.

procedure cvzoom (frames, power, x, y)

short	frames[ARB]
int	power
real	x,y

int	count, cv_move()

include	"cv.com"

begin
	Mems[cv_stack] = IDS_ZOOM
	Mems[cv_stack+1] = IDS_WRITE
	Mems[cv_stack+2] = 3
	count = cv_move (frames, Mems[cv_stack+3])
	Mems[cv_stack+3+count] = IDS_EOD		# (unused) color
	Mems[cv_stack+4+count] = IDS_EOD		# (unused) offset
	Mems[cv_stack+5+count] = power
	Mems[cv_stack+6+count] = x * GKI_MAXNDC
	Mems[cv_stack+7+count] = y * GKI_MAXNDC
	call gescape (cv_gp, IDS_CONTROL, Mems[cv_stack], count+8)
end

############ SUBROUTINES ##############
# CV_MOVE -- transfer an array into the escape data array; returns number
# of items transfered.

int procedure cv_move (in, out)

short	in[ARB]
short	out[ARB]

int	count

begin
	count = 0
	repeat {
	    count = count + 1
	    out[count] = in[count]
	} until (in[count] == IDS_EOD)
	return (count)
end

# CV_ISET -- Tell the image kernel that i/o is to be done for the
# specified frame/frames.

procedure cv_iset (frames)

short	frames[ARB]

short	idata[30]
int	i, cv_move()

include	"cv.com"

begin
	i = cv_move (frames, idata)
	idata[i+1] = IDS_EOD			# all bit planes
	call gescape (cv_gp, IDS_SET_IP, idata, i+1)
end

# CV_GSET -- Tell the image kernel that i/o is to be done for the
# specified colors.

procedure cv_gset (colors)

short	colors[ARB]

short	idata[30]
int	i, cv_move()

include	"cv.com"

begin
	idata[1] = IDS_EOD			# all "frames"
	i = cv_move (colors, idata[2])
	call gescape (cv_gp, IDS_SET_GP, idata, i+1)
end
