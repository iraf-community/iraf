# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<fset.h>
include	<fio.h>
include	<gki.h>

# GKI_GETCURSOR -- Read the cursor position in device coordinates.
#
# BOI GKI_GETCURSOR L CN
#
#        L(i)            4
#        CN(i)           cursor number
#
#        The kernel reads graphics  cursor  number  CN  and  returns  the
#        keystroke  value  (if  any)  and  the  cursor  position  in  NDC 
#        coordinates.   The  cursor  attributes  are  returned   in   the 
#        following format:
#
#                BOI GKI_CURSORVALUE L CN KEY SX SY RN RX RY
#
#        where
#
#                L(i)            10
#                CN(i)           cursor number
#                KEY(i)          keystroke value (>= 0 or EOF)
#                SX(i)           NDC X screen coordinate of cursor
#                SY(i)           NDC Y screen coordinate of cursor
#                RN(i)           raster number or zero
#                RX(i)           NDC X raster coordinate of cursor
#                RY(i)           NDC Y raster coordinate of cursor
#
# The screen or display window coordinates SX and SY of the cursor are
# returned for all devices.  Only some devices support multiple rasters.
# If the device supports rasters and the cursor is in a rasters when read, the
# rasters number and rasters coordinates are returned in RN,RX,RY.  This is in
# addition to the screen coordinates SX,SY.  If rasters coordinates are not
# returned, the rasters number will be set to zero and RX,RY will be the same
# as SX,SY.

procedure gki_getcursor (fd, cursor, cn, key, sx, sy, raster, rx, ry)

int	fd			#I output file
int	cursor			#I cursor to be read
int	cn			#O cursor number actually read
int	key			#O keystroke value or EOF
int	sx, sy			#O screen coordinates of cursor
int	raster			#O raster number
int	rx, ry			#O raster coordinates of cursor

int	epa
int	nchars, read()
short	gki[GKI_GETCURSOR_LEN]
short	cur[GKI_CURSORVALUE_LEN]
data	gki[1] /BOI/, gki[2] /GKI_GETCURSOR/, gki[3] /GKI_GETCURSOR_LEN/
include	"gki.com"
errchk	write, flush, read

begin
	# If the kernel is inline it will return the cursor value in the
	# graphics stream FIO buffer just as if the kernel were resident
	# in another process.  We rewind the buffer after the kernel writes
	# into it in preparation for the read below.

	if (IS_INLINE(fd)) {
	    call fseti (fd, F_CANCEL, OK)
	    epa = gk_dd[GKI_GETCURSOR]
	    if (epa != 0)
		call zcall1 (epa, cursor)
	    call seek (fd, BOFL)

	} else {
	    # Write cursor read instruction to the kernel.
	    gki[GKI_GETCURSOR_CN] = cursor
	    call write (gk_fd[fd], gki, GKI_GETCURSOR_LEN * SZ_SHORT)

	    # If the kernel is a subprocess we must call PR_PSIO to allow the
	    # kernel to read the instruction and return the cursor value.

	    if (IS_SUBKERNEL(fd)) {
		call seek (fd, BOFL)
		call zcall3 (gk_prpsio, KERNEL_PID(fd), fd, FF_READ)
		call seek (fd, BOFL)
	    } else
		call flush (gk_fd[fd])
	}

	# Read and decode the cursor value instruction.
	nchars = GKI_CURSORVALUE_LEN * SZ_SHORT
	if (read (fd, cur, nchars) < nchars)
	    key = EOF
	else if (cur[1] != BOI || cur[2] != GKI_CURSORVALUE)
	    call syserr (SYS_GGCUR)
	else {
	    cn     = cur[GKI_CURSORVALUE_CN]
	    key    = cur[GKI_CURSORVALUE_KEY]
	    sx     = cur[GKI_CURSORVALUE_SX]
	    sy     = cur[GKI_CURSORVALUE_SY]
	    raster = cur[GKI_CURSORVALUE_RN]
	    rx     = cur[GKI_CURSORVALUE_RX]
	    ry     = cur[GKI_CURSORVALUE_RY]
	}

	call fseti (fd, F_CANCEL, OK)
end
