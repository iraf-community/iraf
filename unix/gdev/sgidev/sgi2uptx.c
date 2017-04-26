/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define	NBITS_CHAR	8	/* number of bits in a char */
#define	MASK	64	/* printronix raster flag */
#define	START_LINE	"\005"		/* start of raster line control code */
#define	END_LINE	"\012"		/* end   of raster line control code */
#define	START_PAGE	"\014"		/* form feed */

/* PRINTRONIX translator from the SGI bitmap file to the STDOUT */

int
main (int argc, char *argv[])
{

   FILE	*fpi;
   char	*buffer;
   int	n, len_buf, nlines;
   int index; /* goes through all 8 files */
   int	px, py;

   px = atoi (argv[1]);
   py = atoi (argv[2]);
   len_buf = px/NBITS_CHAR;
   buffer = (char *) malloc (len_buf);

   for (index=3; index<argc; index++) {

      if (index != 3) fwrite (START_PAGE, 1, 1, stdout);
      fpi = fopen (argv[index], "r");
      nlines = py;
   
      while (fread (buffer, len_buf, 1, fpi)) {

         /* Keep track of number of lines left on the page. */
         if (!(nlines--)) {
	    nlines += py;
            fwrite (START_PAGE, 1, 1, stdout);
	 }

         /* Turn on the raster flag on every data byte */
         for (n = 0; n < len_buf; n++) buffer[n] |= MASK;

         /* Now copy out this line and bracket it with the control codes. */
         fwrite (START_LINE, 1, 1, stdout);
         fwrite (buffer, len_buf, 1, stdout);
         fwrite (END_LINE, 1, 1, stdout);

      }

      fclose (fpi);

   }

   return (0);
}
