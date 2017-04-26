# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IDS_SETCURSOR -- Set the position of a cursor.

procedure ids_setcursor (x, y, cursor)

int	x, y			# new position of cursor
int	cursor			# cursor to be set

begin
	call zcursor_set(cursor, x, y)
end
