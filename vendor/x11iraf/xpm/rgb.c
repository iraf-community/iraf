/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* rgb.c:                                                                      *
*                                                                             *
*  XPM library                                                                *
*  Rgb file utilities                                                         *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

/*
 * Part of this code has been taken from the ppmtoxpm.c file written by Mark
 * W. Snitily but has been modified for my special need
 */

#include "xpmP.h"
#ifdef VMS
#include "sys$library:ctype.h"
#include "sys$library:string.h"
#else
#include <ctype.h>
#if defined(SYSV) || defined(SVR4)
#include <string.h>
#else
#include <strings.h>
#endif
#endif

/*
 * Read a rgb text file.  It stores the rgb values (0->65535)
 * and the rgb mnemonics (malloc'ed) into the "rgbn" array.  Returns the
 * number of entries stored.
 */
int
xpmReadRgbNames(rgb_fname, rgbn)
    char *rgb_fname;
    xpmRgbName rgbn[];

{
    FILE *rgbf;
    int i, items, red, green, blue;
    char line[512], name[512], *rgbname, *n, *m;
    xpmRgbName *rgb;

    /* Open the rgb text file.  Abort if error. */
    if ((rgbf = fopen(rgb_fname, "r")) == NULL)
	return 0;

    /* Loop reading each line in the file. */
    for (i = 0, rgb = rgbn; fgets(line, sizeof(line), rgbf); i++, rgb++) {

	/* Quit if rgb text file is too large. */
	if (i == MAX_RGBNAMES) {
	    /* Too many entries in rgb text file, give up here */
	    break;
	}
	/* Read the line.  Skip silently if bad. */
	items = sscanf(line, "%d %d %d %[^\n]\n", &red, &green, &blue, name);
	if (items != 4) {
	    i--;
	    continue;
	}

	/*
	 * Make sure rgb values are within 0->255 range. Skip silently if
	 * bad.
	 */
	if (red < 0 || red > 0xFF ||
	    green < 0 || green > 0xFF ||
	    blue < 0 || blue > 0xFF) {
	    i--;
	    continue;
	}
	/* Allocate memory for ascii name. If error give up here. */
	if (!(rgbname = (char *) XpmMalloc(strlen(name) + 1)))
	    break;

	/* Copy string to ascii name and lowercase it. */
	for (n = name, m = rgbname; *n; n++)
	    *m++ = isupper(*n) ? tolower(*n) : *n;
	*m = '\0';

	/* Save the rgb values and ascii name in the array. */
	rgb->r = red * 257;		/* 65535/255 = 257 */
	rgb->g = green * 257;
	rgb->b = blue * 257;
	rgb->name = rgbname;
    }

    fclose(rgbf);

    /* Return the number of read rgb names. */
    return i < 0 ? 0 : i;
}

/*
 * Return the color name corresponding to the given rgb values
 */
char *
xpmGetRgbName(rgbn, rgbn_max, red, green, blue)
    xpmRgbName rgbn[];			/* rgb mnemonics from rgb text file */
    int rgbn_max;			/* number of rgb mnemonics in table */
    int red, green, blue;		/* rgb values */

{
    int i;
    xpmRgbName *rgb;

    /*
     * Just perform a dumb linear search over the rgb values of the color
     * mnemonics.  One could speed things up by sorting the rgb values and
     * using a binary search, or building a hash table, etc...
     */
    for (i = 0, rgb = rgbn; i < rgbn_max; i++, rgb++)
	if (red == rgb->r && green == rgb->g && blue == rgb->b)
	    return rgb->name;

    /* if not found return NULL */
    return NULL;
}

/*
 * Free the strings which have been malloc'ed in xpmReadRgbNames
 */
void
xpmFreeRgbNames(rgbn, rgbn_max)
    xpmRgbName rgbn[];
    int rgbn_max;

{
    int i;
    xpmRgbName *rgb;

    for (i = 0, rgb = rgbn; i < rgbn_max; i++, rgb++)
	XpmFree(rgb->name);
}
