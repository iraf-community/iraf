/************************************************************************
**  VODALUTIL.C  -- Utility procedures for the DAL interface worker
**  procedures.
**
**  M. Fitzpatrick, NOAO, July 2007
*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include "VOClient.h"
#include "voAppsP.h"


extern  int	format, iportal;
extern  int	html_border, html_color, html_header;

void    vot_initKML (FILE *fd, svcParams *pars);
void    vot_printHTMLRow (FILE *fd, char *line, int isHdr, int rownum);
void    vot_closeKML (FILE *fd);



/************************************************************************
**  INITHTML -- Initialize the KML output file header.
*/
void
vot_initHTML (FILE *fd, svcParams *pars)
{
    if (!fd)
	return;

    if (html_header)
	fprintf (fd, "<html>\n<body>\n");

    fprintf (fd, "<table border=\"%d\">\n",
	(html_border ? 5 : 0));
}


/************************************************************************
**  PRINTHTMLROW -- Write a row in an HTML table.
**  file.
*/
void
vot_printHTMLRow (FILE *fd, char *line, int isHdr, int rownum)
{
    char  *ip, *hp, *sp, *dp, *vp, *tab, *col, delim, val[SZ_LINE];


    if (!fd || !line)
        return;

    for (dp=line; *dp && *dp != '\n'; dp++) /* get the data line          */
        ;
    dp++;    
    hp = line;                    	/* get the header line            */
    
    sp  = (isHdr ? hp : dp);
    tab = (isHdr ? "th" : "td");
    if (isHdr)
        col = "eec";
    else if (html_color)
        col = ((rownum % 2) == 0) ? "ccc" : "eee";
    else
        col = "fff";

    delim = ((format == F_CSV) ? ',' :
              ((format == F_TSV) ? '\t' :
              ((format == F_ASCII) ? ' ' :  ',')));

    fprintf (fd, "<tr>");

    for (ip=sp; *ip; ) {
	bzero (val, SZ_LINE);
	for (vp=val; *ip && *ip != '\n' && *ip != delim; ) {
	    if (*ip == '>') {
	        strcpy (vp, "&gt;");
		vp += 4, ip++;
	    } else if (*ip == '<') {
	        strcpy (vp, "&lt;");
		vp += 4, ip++;
	    } else {
	        *vp++ = *ip++;
 	    }
	}

	if (strncmp (val, "http://", 7) == 0)  {
	    if (iportal && strstr (val, ".fits") == (char *) NULL)  {
	        fprintf (fd, "<%s style=\"background:#%s\">", tab, col);
	        fprintf (fd, "<a href=\"javascript:render('%s');\">%s</a></%s>",
	            val, val, tab);
	    } else if (!iportal) {
	        fprintf (fd, 
		    "<%s style=\"background:#%s\"><a href='%s'>%s</a></%s>",
	            tab, col, val, val, tab);
	    } else {
	        fprintf (fd, "<%s style=\"background:#%s\">%s</%s>",
	            tab, col, val, tab);
	    }
	}  else
	    fprintf (fd, "<%s style=\"background:#%s\">%s</%s>",
	        tab, col, val, tab);
	if (*ip)
 	    ip++;
	else
	    break;
    }

    fprintf (fd, "</tr>\n");
}


/************************************************************************
**  CLOSEHTML -- Close the HTML output file.
*/
void
vot_closeHTML (FILE *fd)
{
    if (!fd)
	return;

    fprintf (fd, "</table>\n");
    if (html_header)
	fprintf (fd, "</body>\n</html>\n");

    fclose (fd);
}


