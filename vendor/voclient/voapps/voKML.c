/************************************************************************
**  VOKML.C  -- Utility procedures for writing Google KML files.
**
**  M. Fitzpatrick, NOAO, July 2007
*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <math.h>
#include "VOClient.h"
#include "voAppsP.h"


extern  int 	format, debug, errno, extract;
extern  int 	kml_max, kml_sample, kml_region,  kml_label, kml_verbose;
extern  int 	kml_bySvc, kml_byObj, kml_byBoth;

extern	Service	*svcList;
extern	Object	*objList;

void    vot_initKML (FILE *fd, svcParams *pars);
void    vot_printKMLPlacemark (FILE *fd, char *id, double ra, 
		double dec, char *line, char *acref, svcParams *pars);
void    vot_mkPlaceDescr (FILE *fd, char *line, char *acref, svcParams *pars);
void    vot_closeKML (FILE *fd);

void    vot_concatKML (char *fname);
void    vot_concatKMLByService (FILE *fd);
void    vot_concatKMLByObject (FILE *fd);
void    vot_cleanKML ();
int     vot_copyKMLFile (char *root, char *sname, FILE *fd);

char   *vot_getSName (char *root);
char   *vot_getOName (char *root);



/************************************************************************
**  INITKML -- Initialize the KML output file header.
*/
void
vot_initKML (FILE *fd, svcParams *pars)
{
  float x   = pars->ra - 180.0,		/* query center coords		*/
	y   = pars->dec;
  float llx, lly, urx, ury;		/* bounding region box		*/


  if (!fd)
      return;

  /* Compute the bounding box, remember the cos(delta) term so the
  ** box scales properly.
  */
  llx = pars->ra - (pars->sr / cos(( y * M_PI / 180.0))) - 180.0,
  lly = pars->dec - pars->sr,
  urx = pars->ra + (pars->sr / cos(( y * M_PI / 180.0))) - 180.0,
  ury = pars->dec + pars->sr;


  fprintf (fd, "<kml xmlns=\"http://earth.google.com/kml/2.2\" ");
  fprintf (fd,  "   hint=\"target=sky\">\n");
  fprintf (fd, "<Document>\n");
  fprintf (fd, "  <Style id=\"VO-CLI-Query\">\n");
  fprintf (fd, "    <BalloonStyle>\n");
  fprintf (fd, "       <text><center><b>$[name]</b></center><br/>");
  fprintf (fd, "$[description]</text>\n");
  fprintf (fd, "    </BalloonStyle>\n");
  fprintf (fd, "    <PolyStyle>\n");
  fprintf (fd, "        <color>ffffffff</color><fill>0</fill>\n");
  fprintf (fd, "    </PolyStyle>\n");
  fprintf (fd, "  </Style>\n");
  fprintf (fd, "  <Placemark>\n");
  fprintf (fd, "    <visibility>1</visibility>\n");
  fprintf (fd, "    <flyToView>1</flyToView>\n");
  if (pars->oname[0])
      fprintf (fd, "    <name>%s</name>\n", pars->oname);
  else
      fprintf (fd, "    <name>Query Center</name>\n");

  fprintf (fd, "    <description>\n");
  fprintf (fd, "      <![CDATA[VO-CLI Query parameters:\n");
  fprintf (fd, "        <table>\n");
  fprintf (fd, "        <tr><td><b>Service Name:</b></td><td>%s</td></tr>\n",
	pars->name);
  fprintf (fd, "        <tr><td><b>Object Name:</b></td><td>%s</td></tr>\n",
	pars->oname);
  fprintf (fd, "        <tr><td><b>Right Ascension:</b></td><td>%f</td></tr>\n",
	(float)pars->ra - 180.0);
  fprintf (fd, " <tr><td><b>Declination:</b></td><td>%f</td></tr>\n", 
	(float)pars->dec);
  fprintf (fd, "        <tr><td><b>Size:</b> </td><td>%.2f Deg</td></tr>\n",
	pars->sr);
  fprintf (fd, "        </tr>\n");
  fprintf (fd, "        </table>\n");
  fprintf (fd, "      ]]>\n");

  fprintf (fd, "      </description>\n");
  fprintf (fd, "      <styleUrl>#VO-CLI-Query</styleUrl>\n");
  fprintf (fd, "      <Point><coordinates>%f,%f,0</coordinates></Point>\n",
	x, y);


  if (kml_region) {
      fprintf (fd, "      <Polygon><outerBoundaryIs><LinearRing>\n");	
      fprintf (fd, "        <fill>0</fill>\n");
      fprintf (fd, "        <coordinates>\n");
      fprintf (fd, "		%f,%f,0\n", llx, lly);
      fprintf (fd, "		%f,%f,0\n", llx, ury);
      fprintf (fd, "		%f,%f,0\n", urx, ury);
      fprintf (fd, "		%f,%f,0\n", urx, lly);
      fprintf (fd, "		%f,%f,0\n", llx, lly);
      fprintf (fd, "        </coordinates>\n");
      fprintf (fd, "      </LinearRing></outerBoundaryIs></Polygon>\n");	
  }
  fprintf (fd, "      </Placemark>\n");	
}


/************************************************************************
**  PRINTKMLPLACEMARK -- Write a placemark for the given point to the KML
**  file.
*/
void
vot_printKMLPlacemark (FILE *fd, char *id, double ra, double dec, 
	char *line, char *acref, svcParams *pars)
{
    double   x = ra - 180.0,   		/* Neet to convert to lat/lon  */
	     y = dec;

    if (!fd)
	return;

    fprintf (fd, "    <Placemark>\n");
    if (kml_label)
        fprintf (fd, "      <name>%s</name>\n", id);

    fprintf (fd, "      <styleUrl>#randomIcon</styleUrl>\n");

    if (kml_verbose) {
        fprintf (fd, "      <description>\n");
	vot_mkPlaceDescr (fd, line, acref, pars); 
        fprintf (fd, "      </description>\n");
    }

    fprintf (fd, "      <Point> <coordinates>%f,%f,0</coordinates> </Point>\n",
	x, y);
    fprintf (fd, "    </Placemark>\n");
}


/************************************************************************
**  MKPLACEDESCR -- Make a placemark description from the query result.
**  We assume the 'line' is a header line and a data row; use this to make
**  a keyw/value table
*/
void
vot_mkPlaceDescr (FILE *fd, char *line, char *acref, svcParams *pars)
{
    char  *ip, *hp, *dp, *vp, delim, val[SZ_LINE];
    extern char *toSexa (double pos);


    if (!fd || !line)
	return;

    for (dp=line; *dp != '\n'; dp++) 	/* get the data line		*/
	;
    dp++;
    hp = line;				/* get the header line		*/


    delim = ((format == F_CSV) ? ',' :
              ((format == F_TSV) ? '\t' :
              ((format == F_ASCII) ? ' ' :  ',')));

	    
    fprintf (fd, "<![CDATA[");
    fprintf (fd, "<font size=\"+1\" color=\"#a00\">");
    fprintf (fd, "<b>Resource:&nbsp; %s&nbsp;&nbsp;&nbsp;", pars->name);
    fprintf (fd, "Object:&nbsp; %s&nbsp;&nbsp;&nbsp;\n", pars->oname);
    fprintf (fd, "RA:&nbsp; %s&nbsp;&nbsp;&nbsp;\n", toSexa(pars->ra / 15.0));
    fprintf (fd, "Dec:&nbsp; %s</b>\n", toSexa(pars->dec));
    fprintf (fd, "</font>\n");
    fprintf (fd, "<table border=\"1\" height=\"20\">\n");
    fprintf (fd, "<tr>");

    for (ip=hp; *ip; ) {
	bzero (val, SZ_LINE);
	for (vp=val; *ip && *ip != '\n' && *ip != delim; )
	    *vp++ = *ip++;
	fprintf (fd, "<th>%s</th>", val);
	if (!*ip || *ip == '\n')
	    break;
	else if (*ip)
 	    ip++;
    }

    fprintf (fd, "\n</tr><tr>\n");

    for (ip=dp; *ip; ) {
	bzero (val, SZ_LINE);
	for (vp=val; *ip && *ip != delim; )
	    *vp++ = *ip++;
 	ip++;
	    fprintf (fd, "<td>%s</td>", val);
    }

    fprintf (fd, "</tr></table>\n");

    if (acref[0] && (strstr(line,"image/g") || strstr(line,"image/j"))) {
	fprintf (fd, "<hr noshade=\"5\"><font size=\"+1\">");
	fprintf (fd, "Preview Image</font><br>\n");
	fprintf (fd, "<img src=\"%s\" ", acref);
	fprintf (fd, "width=\"300\" height=\"300\">\n");
    } else if (acref[0] && (strstr(line,"image/fits"))) {
	fprintf (fd, "<hr noshade=\"5\"><font size=\"+1\">");
	fprintf (fd, "Preview Image Not Available</font><br>\n");
    }


    fprintf (fd, "]]>\n");
}


/************************************************************************
**  CLOSEKML -- Close the KML output file.
*/
void
vot_closeKML (FILE *fd)
{
    if (!fd)
	return;

    fprintf (fd, "</Document>\n</kml>\n");	
    fclose (fd);
}


/************************************************************************
**  CONCATKML --  Concatenate the KML file generated by the query into a
**  single, hierarchical document grouped either by the service, the 
**  object/position (default), or both.
*/
void
vot_concatKML (char *fname)
{
    FILE   *fd = (FILE *) NULL;
    extern int nservices, nobjects;


    if (fname[0] == '-')
	fd = stdout;
    else if ((fd = fopen (fname, "w+")) == (FILE *) NULL) {
	fprintf (stderr, "ERROR: Cannot open KML file: '%s'\n", fname);
	return;
    }

    /* Write the preamble to the file.
    */
    fprintf (fd, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    fprintf (fd, "<kml xmlns=\"http://earth.google.com/kml/2.2\" ");
    fprintf (fd, " hint=\"target=sky\">\n"),
    fprintf (fd, "<Folder id=\"Root\">\n");

    fprintf (fd, "  <Style id=\"randomIcon\">\n");
    fprintf (fd, "   <IconStyle>\n");
    fprintf (fd, "      <color>ffffffff</color>\n");
    fprintf (fd, "      <colorMode>random</colorMode>\n");
    fprintf (fd, "      <scale>1.2</scale>\n");
    fprintf (fd, "      <Icon>\n");
    fprintf (fd, 
    " <href>http://maps.google.com/mapfiles/kml/paddle/ylw-blank.png</href>\n");
    fprintf (fd, "      </Icon>\n");
    fprintf (fd, "   </IconStyle>\n");
    fprintf (fd, "  </Style>\n");
    fprintf (fd, "  <open>1</open>\n");

    if (nservices > 1 && nobjects > 1) {
        if (kml_byObj) {
        fprintf (stderr, "concat by Object\n");
	    vot_concatKMLByObject (fd);

        } else if (kml_bySvc) {
	    vot_concatKMLByService (fd);

        } else if (kml_byBoth) {
	    fprintf (fd, "  <Folder id=\"byObj\">\n");
	    fprintf (fd, "    <name>By Object</name>\n");
	    fprintf (fd, "    <open>0</open>\n");
	    vot_concatKMLByObject (fd);
	    fprintf (fd, "  </Folder>\n");

	    fprintf (fd, "  <Folder id=\"bySvc\">\n");
	    fprintf (fd, "    <name>By Service</name>\n");
	    fprintf (fd, "    <open>0</open>\n");
	    vot_concatKMLByService (fd);
	    fprintf (fd, "  </Folder>\n");
        }

        /*  Clean up the intermediate files if needed.
        */
        if (format & F_KML || (extract & EX_KML && extract & EX_COLLECT))
	    vot_cleanKML ();
    }

    /*  Write the end of the file to close it.
    */
    fprintf (fd, "\n</Folder>\n</kml>\n");

    /*  Close the file descriptors.
    */
    if (fd != stdout)
	fclose (fd);
}


/************************************************************************
**  CONCATKMLBYOBJECT --   Concatenate the KML files for the query grouped
**  by the object/position
*/
void
vot_concatKMLByObject (FILE *fd)
{
    Service *svc = svcList;		/* the service list		*/
    Proc    *proc;			/* process list in each service	*/
    Proc    *ps;
    char    sname[SZ_FNAME],
	    oname[SZ_FNAME],
	    obj[SZ_FNAME];


    /*  Loop over the "query matrix" by object.  The process table will
    **  have the same objects for each resource so we can use with the
    **  first service's list of objects/positions.
    */
    for (proc=svcList->proc; proc; proc=proc->next) {
        bzero (oname, SZ_FNAME);
        strcpy (oname, vot_getOName (proc->root));

	fprintf (fd, "  <Folder id=\"o_%s\">\n", oname);
	fprintf (fd, "    <name>%s</name>\n", oname);
	fprintf (fd, "    <open>0</open>\n");

	/*  Go through the list of services to find this object.  This is
	**  known to be somewhat inefficient for the moment, but we don't
	**  expect the query matrix to be large, and if it is this is still
	**  a small overhead compared to the queries.
	*/
    	for (svc=svcList; svc; svc=svc->next) {
    
	    for (ps=svc->proc; ps; ps=ps->next) {
                bzero (obj, SZ_FNAME);
                strcpy (obj, vot_getOName (ps->root));

	        if (strcmp (oname, obj) == 0) {
                    bzero (sname, SZ_FNAME);
                    strcpy (sname, vot_getSName (svc->proc->root));

		    if (debug) {
                        fprintf (stderr, "\t\t%s.%s\t%s\n",
		            oname, sname, ps->root);
		    }

		    /* At this point we have the following:
		    **
		    **	  oname      - name of object we using to group
		    **	  sname      - name of the service we're processing
		    **	  ps->root   - name of root file associated w/ result
		    **
		    ** The job now is simply to concatenate any KML file onto
		    ** the final output.
		    */

		    vot_copyKMLFile (ps->root, sname, fd);
		    break;
	        }
	    }
    	}
	fprintf (fd, "  </Folder>\n");
    }

    return;
}


/************************************************************************
**  CONCATKMLBYSERVICE --  Concatenate the KML files for the query grouped
**  by the data service. 
*/
void
vot_concatKMLByService (FILE *fd)
{
    Service *svc = svcList;		/* the service list		*/
    Proc    *proc;			/* process list in each service	*/
    char    sname[SZ_FNAME];
    char    oname[SZ_FNAME];


    for (svc=svcList; svc; svc=svc->next) {

        bzero (sname, SZ_FNAME);
        strcpy (sname, vot_getSName (svc->proc->root));

	fprintf (fd, "  <Folder id=\"o_%s\">\n", sname);
	fprintf (fd, "    <name>%s</name>\n", sname);
	fprintf (fd, "    <open>0</open>\n");

        for (proc=svc->proc; proc; proc=proc->next) {
            bzero (oname, SZ_FNAME);
            strcpy (oname, vot_getOName (proc->root));
	    vot_copyKMLFile (proc->root, oname, fd);
	}

	fprintf (fd, "  </Folder>\n");
    }

    return;
}



/************************************************************************
**  Utility routines to extract bits from the root filename.
*/
char *
vot_getSName (char *root)
{
    char  *ip, *op;
    static char val[SZ_FNAME];

    bzero (val, SZ_FNAME);
    for (ip=root, op=val; *ip && *ip != '_'; )
	*op++ = *ip++;

    return (val);
}

char *
vot_getOName (char *root)
{
    char  *ip, *op;
    static char val[SZ_FNAME];

    bzero (val, SZ_FNAME);

    /* skip service name and type.
    */
    for (ip=root; *ip && *ip != '_'; ) ip++;  ip++;
    for (       ; *ip && *ip != '_'; ) ip++;  ip++;

    /* get object name	*/
    for (op=val; *ip && *ip != '.' && *ip != '_'; ) 
	*op++ = *ip++;

    return (val);
}


/************************************************************************
**  COPYKMLFILE -- Copy a KML file to the output file descriptor for inclusion
**  in a grander hierarchy.  To do this we copy out only the inner part of
**  the <Document> in a single file.
*/
int
vot_copyKMLFile (char *root, char *name, FILE *fd) 
{
    char  line[4096], fname[SZ_FNAME];
    FILE  *ifd;


    bzero (fname, SZ_FNAME);
    sprintf (fname, "%s.kml", root);

    if (access (fname, R_OK) == 0) {
        if ((ifd = fopen (fname, "r")) == (FILE *) NULL) {
	    fprintf (stderr, "Warning: Cannot open file '%s'\n", fname);
	    return (ERR);
	}
    } else {
	/* A missing file just means there's no data, but we want to
	** reflect that in the file as an empty folder.
	*/
	return (OK);
    }

    fprintf (fd, "    <Folder id=\"s_%s\">\n", name);
    fprintf (fd, "      <name>%s</name>\n", name);
    fprintf (fd, "      <open>0</open>\n");
    fprintf (fd, "      <flyToView>1</flyToView>\n");
    fprintf (fd, "      <styleUrl>#randomIcon</styleUrl>\n");

    /* Skip ahead to the start of the part we're interested in.
    */
    bzero (line, 4096);
    while (fgets (line, 4096, ifd)) {
	if (strstr (line, "<Document>"))
	    break;
        bzero (line, 4096);
    }

    /* (Slow) Copy the file until the end of the Document.
    */
    bzero (line, 4096);
    while (fgets (line, 4096, ifd)) {
	if (strstr (line, "</Document>"))
	    break;
	fprintf (fd, "%s", line);
        bzero (line, 4096);
    }

    fprintf (fd, "    </Folder>\n");

    fclose (ifd);
    return (OK);
}


/************************************************************************
**  CLEANKML -- Clean up the intermediate KML files.
*/
void
vot_cleanKML ()
{
    Service *svc = svcList;		/* the service list		*/
    Proc    *proc;			/* process list in each service	*/
    char    fname[SZ_FNAME];


    for (svc=svcList; svc; svc=svc->next) {
        for (proc=svc->proc; proc; proc=proc->next) {
	    bzero (fname, SZ_FNAME);
	    sprintf (fname, "%s.kml", proc->root);
	    unlink (fname);

	    bzero (fname, SZ_FNAME);
	    sprintf (fname, "%s.csv", proc->root);
	    unlink (fname);
	}
    }
}
