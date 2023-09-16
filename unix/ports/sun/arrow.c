/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <sundev/kbio.h>
#include <sundev/kbd.h>

/*
 * ARROW.C -- Code to enable and disable the arrow key function-key mappings
 * (R8,10,12,14).
 */

#define	NKEYS	4
static	unsigned char station[NKEYS] = { 0x45, 0x5b, 0x5d, 0x71 };
static	unsigned char entry[NKEYS] = { RF(8), RF(10), RF(12), RF(14) };
static	struct kiockey o_key[NKEYS];


/* DISABLE_ARROW_KEYS -- Save the arrow key keyboard translation table
 * entries, and then disable the mapping of the function keys to the ANSI
 * arrow key sequences.   This is necessary to read the function key as
 * an event rather than an escape sequence in a Sunview event handler.
 */
disable_arrow_keys()
{
	register int	fd, i;
	struct	kiockey	key;
	int	status = 0;

	if ((fd = open ("/dev/kbd", 2)) == -1)
	    return (-1);

	for (i=0;  i < NKEYS;  i++) {
	    o_key[i].kio_station = station[i];
	    if ((status = ioctl (fd, KIOCGETKEY, &o_key[i])) != 0)
		break;
	    key = o_key[i];
	    key.kio_entry = entry[i];
	    if ((status = ioctl (fd, KIOCSETKEY, &key)) != 0)
		break;
	}

	close (fd);
	return (status);
}


/* ENABLE_ARROW_KEYS -- Restore the saved arrow key keyboard translation table
 * entries.
 */
enable_arrow_keys()
{
	register int	fd, i;
	struct	kiockey	key;
	int	status = 0;

	if ((fd = open ("/dev/kbd", 2)) == -1)
	    return (-1);

	for (i=0;  i < NKEYS;  i++)
	    if ((status = ioctl (fd, KIOCSETKEY, &o_key[i])) != 0)
		break;

	close (fd);
	return (status);
}
