# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>

# GKI_RETCURSORVALUE -- Return a cursor value.  Used by a graphics kernel to
# return a cursor value to GIO in response to a GETCURSOR instruction.
#
# BOI GKI_CURSORVALUE L CN POS KEY
#    
#       L(i)		7
#	CN(i)		cursor read
#	POS(p)		GKI coordinates of cursor
#	KEY(i)		keystroke value

procedure gki_retcursorvalue (fd, x, y, key, cursor)

int	fd			# output file
int	x, y			# cursor coordinates (GKI coords)
int	key			# keystroke value
int	cursor			# code for cursor read

short	gki[GKI_CURSORVALUE_LEN]
data	gki[1] /BOI/, gki[2] /GKI_CURSORVALUE/, gki[3] /GKI_CURSORVALUE_LEN/

begin
	gki[GKI_CURSORVALUE_CN   ] = cursor
	gki[GKI_CURSORVALUE_POS  ] = x
	gki[GKI_CURSORVALUE_POS+1] = y
	gki[GKI_CURSORVALUE_KEY  ] = key

	call write (fd, gki, GKI_CURSORVALUE_LEN * SZ_SHORT)
end
