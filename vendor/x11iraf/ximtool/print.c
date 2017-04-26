#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include "ximtool.h"


/*
**  PRINT.C -- Printer interface.
**
**		      xim_print (xim, x0,y0, nx,ny)
**
**		    ximp_rename (xim, old, new)		# print alert action
**		    ximp_cancel (xim, fname)		# print alert action
**
**	     xim_initPrinterOps (xim)
**	    xim_initPrinterList (xim)
**	     xim_getPrinterInfo (xim, printer)
**
**  xim_print prints the indicated region of the display frame (raster 0).
**  If nx or ny is zero the full display frame is printed.  The output device
**  or file and all print options are maintained in the printer context within
**  the XIM descriptor.  The xim_initPrinter routines should be called at
**  startup to initialize the print options and load the list of local printers.
*/

void 	xim_initPrinterOps();
int 	xim_getPrinterInfo();

static void printstat();
static void xim_initPrinterList();


/*  XIM_PRINT -- Print the indicated region of the current display frame to
**  the printer device or to a file.
*/

xim_print (xim, x0,y0, nx,ny)
register XimDataPtr xim;
int x0,y0, nx,ny;				/* region of source raster */
{
	register PSImagePtr psim = xim->psim;
	register PrintCfgPtr pcp = xim->pcp;
        register FrameBufPtr fb = xim->df_p;
	register ColorMapPtr cm = &colormaps[fb->colormap-1];
	unsigned char r[256], g[256], b[256];
	unsigned char *pixels = NULL;
	static char tmpfile[SZ_FNAME];
	static char fname[SZ_FNAME];
	static char text[SZ_LINE];
	int w, h, ncolors;
	FILE *fp;
	char *mktemp();


	bzero (text, SZ_LINE);
	bzero (fname, SZ_FNAME);
	bzero (tmpfile, SZ_FNAME);


	/* Get the display pixels and colormap.  The routine allocates a 
	 * pointer to the pixels we'll need to free when we're done.
	 */
	printstat (xim, "Get image data...");
	pixels = xim_readDisplay (xim, x0,y0,nx,ny, &w,&h, r,g,b, &ncolors);
	if (!pixels)
	    return (-1);

	/* Set up some of the EPS options and load the colormap. */
 	if (psim->label && (psim->page.flags & EPS_DOTITLE))
 	    eps_setLabel (psim, fb->ctran.imtitle);
        eps_setCmap (psim, r, g, b, ncolors);
	eps_setTransform (psim, fb->ctran.z1, fb->ctran.z2, fb->ctran.zt,
	    fb->offset, fb->scale, cm->name);


        /* Now call the main routine to output the EPS file.
	 */
	if (pcp->diskfile) {

	    /* Print to a file.  If we are not clobbering an existing file
	     * and we can open the output file write to it directly and be
	     * done with it.  If there is any problem we we to a temporary
	     * file and issue an alert with actions to be taken if the user
	     * decides to proceed or cancel the operation.
	     */
	    if (strchr (pcp->printFile, (int)'%'))
	        sprintf (fname, pcp->printFile, pcp->seqno++);
	    else
	        strcpy (fname, pcp->printFile);

	    if (access (fname, F_OK) < 0) {
		if (fp = fopen (fname, "w")) {
		    struct stat fs;

		    printstat (xim, "Generating postscript output...");
		    eps_print (psim, fp, pixels, w, h, 8, 0);
		    fclose (fp);

		    stat (fname, &fs);
		    sprintf (text, "Wrote %d bytes to '%s'",
			(int)fs.st_size, fname);
		    printstat (xim, text);

		} else {
		    sprintf (text, "Could not open file %s", fname);
		    xim_alert (xim, text, NULL, NULL);
		}

	    } else {
		/* Named file already exists.  Write a temporary file and
		 * post an alert to ask the user if they want to overwrite
		 * the existing file.
		 */
		char ok_action[SZ_LINE];
		char cancel_action[SZ_LINE];
		char tmpfile[SZ_FNAME];
		char *ip, *op, *last;


		bzero (tmpfile, SZ_FNAME);
		bzero (ok_action, SZ_LINE);
		bzero (cancel_action, SZ_LINE);

		/* Write to a temporary file in the same directory as fname. 
		*/
		for (ip=fname, op=tmpfile, last=tmpfile;  *op = *ip++;  op++) {
		    if (*op == '/')
			last = op + 1;
		}
		*last = '\0';
		strcat (tmpfile, "ximpXXXXXX");
		if (mktemp(tmpfile) == (char *)NULL)
		    return (-1);

		if (!(fp = fopen (tmpfile, "w"))) {
		    sprintf (text, "Cannot open temporary file:\n%s", tmpfile);
		    xim_alert (xim, text, NULL, NULL);
		    return (-1);
		}
		printstat (xim, "Generating postscript output...");
		eps_print (psim, fp, pixels, w, h, 8, 0);
		fclose (fp);

		for (ip=fname; *ip!='\0' ; ip++) ;
		for ( ; *ip != '/' && ip > fname; ip--) ;
		sprintf (text, "%s\n%s\n%s", 
		    "File already exists:", (ip == fname ? fname : ++ip),
		    "Overwrite this file?");

		sprintf (ok_action, "print rename %s %s", tmpfile, fname);
		sprintf (cancel_action, "print cancel %s", tmpfile);

		xim_alert (xim, text, ok_action, cancel_action);
	    }

	} else {
	    /* Print to a printer device. */
	    strcpy (tmpfile, "/tmp/ximpXXXXXX");
	    if (mktemp(tmpfile) == (char *)NULL)
		return (-1);

	    if (!(fp = fopen (tmpfile, "w")))
		return (-1);
	    printstat (xim, "Generating postscript output...");
	    eps_print (psim, fp, pixels, w, h, 8, 0);
	    fclose (fp);

	    printstat (xim, "Printing file...");
	    sprintf (text, "cat %s | %s", tmpfile, pcp->printCmd);
	    system (text);			/* dispose to printer */
	    unlink (tmpfile);			/* delete tmp file */

	    printstat (xim, "Done.");
	}

	/* Clean up and return. */
	free ((char *) pixels);
	return (0);
}

pbob () { int i = 0; }


/*  The following implement the ok and cancel actions posted by the alert in
**  xim_print above.
*/

void
ximp_rename (xim, old, new)
register XimDataPtr xim;
char *old, *new;
{
	char text[SZ_LINE];
	struct stat fs;

	bzero (text, SZ_LINE);
	unlink (new);

	if (rename(old,new) != 0 || stat(new,&fs) != 0) {
	    sprintf (text, "Could not write file %s", new);
	    printstat (xim, text);
	} else {
	    sprintf (text, "Wrote %d bytes to %s", fs.st_size, new);
	    printstat (xim, text);
	}
}

void
ximp_cancel (xim, fname)
register XimDataPtr xim;
char *fname;
{
	printstat (xim, "Print cancelled.");
	unlink (fname);
}


/* XIM_INITPRINTEROPS -- Initialize the printer operations.
*/
void
xim_initPrinterOps (xim)
register XimDataPtr xim;
{
        register PrintCfgPtr pcp;
	char buf[SZ_LINE];
	PSImagePtr eps_init();


	/* Open a pointer to the EPS structure. */
        xim->psim = eps_init();

	/* Read the printer configuration file. */
        xim_initPrinterList (xim);

	/* Initialize options. */
        xim_message (xim, "printOptions", "papersize letter");
        xim_message (xim, "printOptions", "orientation portrait");
        xim_message (xim, "printOptions", "colortype gray");
        xim_message (xim, "printOptions", "autoscale True");
        xim_message (xim, "printOptions", "annotate True");
        xim_message (xim, "printOptions", "dotitle True");
        xim_message (xim, "printOptions", "docolorbar True");
        xim_message (xim, "printOptions", "doborders True");

        /* Allocate the printer configuration struct. */
        xim->pcp = pcp = (PrintCfgPtr) malloc (sizeof (PrintCfg));

        pcp->seqno = 0;
        pcp->printno = 0;
        pcp->diskfile = 0;
        strcpy (pcp->printFile, "frame%d.eps");

	bzero (buf, SZ_LINE);
        sprintf (buf, "printerName %s", printer_list[0].printerName);
        xim_message (xim, "printOptions", buf);

	strcpy (pcp->printCmd, printer_list[0].printCmd);
        sprintf (buf, "printCmd %s", pcp->printCmd);
        xim_message (xim, "printOptions", buf);
}


/* XIM_INITPRINTERLIST -- Read the printer configuration file and initialize
** the structure with the list of printers and commands.  Send the printer
** list to the GUI, maintain the command list internally.
*/

static void
xim_initPrinterList (xim)
register XimDataPtr xim;
{
	register int i;
	register FILE *fp;
	char 	 buf[SZ_LINE], plist[MAX_PRINTERS*20];
	char  	 *ip, *pn, *pc, *pl;


	bzero (buf, SZ_LINE);
	bzero (plist, MAX_PRINTERS*20);

	if (access (xim->printConfig, R_OK) == 0) {
	    if (!(fp = fopen (xim->printConfig, "r")))
		return;

            /* Scan the printer configuration file.
             */
            while (fgets (buf, SZ_LINE, fp) != NULL) {
                /* Skip comment lines and blank lines. */
                for (ip=buf;  *ip == ' ' || *ip == '\t';  ip++)
                    ;
                if (*ip == '\n' || *ip == '#')
                    continue;

		/* Now scan up to the tab for the list entry, i.e the printer
		 * name we'll use in the GUI.
	 	 */
		pn = printer_list[nprinters].printerName;
		while (*ip != '\t') 
		    *pn++ = *ip++;
		*pn++ = '\0';
		ip++;

		while (!isalnum(*ip))
		    ip++;

		/* Everything up to a comment char or the newline is the
		 * command to be used to dispose the EPS.
		 */
		pc = printer_list[nprinters].printCmd;
		while (*ip != '#' && *ip != '\n') 
		    *pc++ = *ip++;
		*pc++ = '\0';

		nprinters++;
		bzero (buf, SZ_LINE);
	    }
	    fclose (fp);

	    /* Erase the remaining entries in the default printer list */
	    for (i=nprinters; i < MAX_PRINTERS; i++) {
		strcpy (printer_list[i].printerName, " ");
		printer_list[i].printCmd[0] = '\0';
	    }
	} else {
	    while (printer_list[nprinters].printerName[0] != '\0' && 
		nprinters < MAX_PRINTERS)
		    nprinters++;
	}


	/* Now build up a list of the printers, either from the config
	 * file or the fallback, and send it to the GUI.
	 */
        for (i=0, pl=plist;  i < nprinters;  i++) {
            *pl++ = '"';
            for (ip = printer_list[i].printerName;  *pl = *ip++;  pl++)
                ;
            *pl++ = '"';
            *pl++ = '\n';
        }
        *pl++ = '\0';

        xim_message (xim, "printerList", plist);
}


/* XIM_GETPRINTERINFO -- For a given printer name search the printer list
** array and update the GUI with the selected printer name and command.
*/

int
xim_getPrinterInfo (xim, printer)
register XimDataPtr xim;
char *printer;
{
	register int i;
        register PrintCfgPtr pcp = xim->pcp;

	/* Scan down the printer list until we find the requested device. */
	for (i=0; strcmp(printer_list[i].printerName, printer) != 0; i++)
	    if (i >= MAX_PRINTERS)
		return (pcp->printno);

	return (pcp->printno = i);
}


/* PRINTSTAT -- Internal routine for print status messages.
*/

static void
printstat (xim, message)
register XimDataPtr xim;
char *message;
{
	char text[SZ_LINE];

	bzero (text, SZ_LINE);
	sprintf (text, "status {%s}", message);
        xim_message (xim, "printOptions", text);
}
