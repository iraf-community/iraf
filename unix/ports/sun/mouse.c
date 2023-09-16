/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <suntool/sunview.h>

/* MOUSE.C -- Routines for saving and restoring the mouse position.  These
 * are used by a window which needs to grab the mouse and set it to a specific
 * position, e.g., because the user has entered a command in to another window
 * requesting a cursor read by the process running in the current window.
 * (It was NOT easy to figure out how to do this in SunView, but at least I
 * was able to do it...).
 */

/* GET_ABSMOUSEPOS -- Get the current position of the mouse in absolute screen
 * coordinates.
 */
get_absmousepos (mywinfd, x, y)
int	mywinfd;		/* any window on current screen will do */
int	*x, *y;			/* mouse position (output) */
{
	struct	screen rootscreen;
	int	rootfd;

	win_screenget (mywinfd, &rootscreen);
	rootfd = open (rootscreen.scr_rootname, 0);

	*x = win_get_vuid_value (rootfd, LOC_X_ABSOLUTE);
	*y = win_get_vuid_value (rootfd, LOC_Y_ABSOLUTE);

	close (rootfd);
}


/* SET_ABSMOUSEPOS -- Set the mouse position in absolute screen coordinates.
 */
set_absmousepos (mywinfd, x, y)
int	mywinfd;		/* any window on current screen will do */
int	x, y;			/* desired mouse position */
{
	struct	screen rootscreen;
	int	rootfd;

	win_screenget (mywinfd, &rootscreen);
	rootfd = open (rootscreen.scr_rootname, 0);
	win_setmouseposition (rootfd, x, y);
	close (rootfd);
}
