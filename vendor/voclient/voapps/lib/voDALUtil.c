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
#include <time.h>
#include <pthread.h>
#include "VOClient.h"
#include "voAppsP.h"


extern int   errno, nservices, nobjects, quiet, format, simple_out, numout;
extern int   debug, verbose, all_named, all_data, save_res, extract;
extern int   meta, dverbose, count, count_only, file_get, use_name;
extern int   kml_max, kml_sample, kml_region,  kml_label;
extern char *output;

extern  Service *svcList;
extern  Object  *objList;

extern  char *vot_getSName (char *root);
extern  char *vot_getOName (char *root);



int     vot_extractResults (char *result, char delim, svcParams *pars);
int     vot_printCount (Query query, svcParams *pars, int *count);
char    vot_svcTypeCode (int type);
char   *vot_getOFName (svcParams *pars, char *extn, int pid);
char   *vot_getOFIndex (svcParams *pars, char *extn, int pid);
char   *vot_openExFile (svcParams *pars, int nrows, char *extn, FILE **ofd);
char   *vot_procTimestamp (void);
char   *vot_getExtn (void);
void	vot_printCountHdr (void);
void    vot_printCountLine (int nrec, svcParams *pars);
void    vot_dalExit (int code, int count);
void    vot_printHdr (int fd, svcParams *pars);
void    vot_concat ();

static  void  vot_clean (char *extn);
static  int   vot_copyFile (char *root, char *extn, char *name, 
	    FILE *fd, int hdr, int nrows);

extern  char *vot_normalize (char *str);
extern  char *vot_normalizeCoord (char *str);

extern  void  vot_initKML (FILE *fd, svcParams *pars);
extern  void  vot_printKMLPlacemark (FILE *fd, char *id, 
		double ra, double dec, char *line, char *acref,
		svcParams *pars);
extern  void  vot_closeKML (FILE *fd);

extern  void  vot_initHTML (FILE *fd, svcParams *pars);
extern  void  vot_printHTMLRow (FILE *fd,  char *line, int isHdr, int rnum);
extern  void  vot_closeHTML (FILE *fd);





/************************************************************************
**  VOC_EXTRACTRESULTS -- Extract the position and acref information from
**  a DAL query result.  We use the first occurance of the POS_EQ_MAIN_<t>
**  UCDs for the position and either ID_MAIN or an artificial ID.  Format
**  for the position file is fixed but may be generalized later to allow
**  the order/delimiter to be controlled by the user.  The acref file
**  contains the contents of the VOX:Image_AccessReference element.
*/
int
vot_extractResults (char *result, char delim, svcParams *pars)
{
    int   id=-1, ra=-1, dec=-1, acref=-1, colnum, rownum, nrows, ncols, np, na;
    char  col[SZ_LINE], s_id[SZ_LINE], s_ra[SZ_LINE], s_dec[SZ_LINE];
    char  s_acref[SZ_URL], afname[SZ_LINE], pfname[SZ_LINE];
    char  hfname[SZ_LINE], kfname[SZ_LINE];
    char *ip = result, *sres, *op, *lp;
    char  hline[SZ_RESULT], line[SZ_RESULT];
    FILE *pfd = (FILE *)NULL;
    FILE *afd = (FILE *)NULL;
    FILE *kfd = (FILE *)NULL;
    FILE *hfd = (FILE *)NULL;

    static int row_count = 0;


    if (!result)
	vot_dalExit (E_NODATA, 0);
	

    /* Skip leading whitespace
    */
    for (ip=result; isspace(*ip) || *ip == '\n'; ip++)
	;
    sres = ip;

    /* Count the number of result records, and skip the header line.
    */
    for (nrows=0, ip=sres; *ip; ip++) {
	if (*ip == '\n')
	    nrows++;
    }
    if (nrows > 0) 
	nrows--;

    if (nrows <= 0) {
	if (debug)
	    fprintf (stderr, "WARNING:  No Data found.\n");
	return (0);
    }

    /* Return the row count only if we're not extracting anything.
    */
    if (!extract)
	return (nrows);


    /* Get the desired column indices.
    */
    ip = &result[1];
    colnum = 0;
    bzero ((lp = hline), SZ_RESULT);
    while (1) {
	bzero (col, SZ_LINE);
	for (op=col; *ip && *ip != delim && *ip != '\n';)
	    *op++ = *lp++ = *ip++;

	if ((*ip == delim && *(ip+1) == delim) || !col[0]) {/* no UCD	*/
	    colnum+=2;
	    ip += 2;
	    bzero (col, SZ_LINE);
	    continue;
	}


	if ((strcasecmp ("ID_MAIN", col) == 0) ||
	    (strcasecmp ("meta.id;meta.main", col) == 0))
	        id = colnum;
	else if ((strcasecmp ("POS_EQ_RA_MAIN", col) == 0) ||
	    (strcasecmp ("pos.eq.ra;meta.main", col) == 0))
	        ra = colnum;
	else if ((strcasecmp ("POS_EQ_DEC_MAIN", col) == 0) ||
	    (strcasecmp ("pos.eq.dec;meta.main", col) == 0))
	        dec = colnum;

	else if (strcasecmp ("VOX:Image_AccessReference", col) == 0)
	    acref = colnum;
	else if (strcasecmp ("meta.ref.url", col) == 0)
	    acref = colnum;
	else if (strcasecmp ("DATA_LINK", col) == 0)
	    acref = colnum;

	while (*ip == delim && *ip != '\n') 	/* Skip the delimiter.  */
	    *lp++ = *ip++;

	if (*ip == '\n') {	/* Only process first line here */
	    *lp++ = *ip;
	    break;
	}
	colnum++;
    }
    ncols = colnum + 1;

    if (debug) {
	printf ("%s: %c id=%d  ra=%d  dec=%d  acref=%d  extract=%d  (%d,%d)\n",
	    pars->name, vot_svcTypeCode(pars->type), id, ra, dec,
	    acref, extract, ncols, nrows);
    }

    /* Begin processing.
    */
    for (ip=sres; *ip != '\n'; ip++) ;	/* skip header line	  */
    ip++;

    bzero (s_id, SZ_LINE);
    bzero (s_ra, SZ_LINE);
    bzero (s_dec, SZ_LINE);
    bzero (s_acref, SZ_URL);
    bzero (line, SZ_RESULT);

    if (extract & EX_POS && (abs(ra) >= 0 && abs(dec) >= 0)) {
	bzero (pfname, SZ_LINE);
	strcpy  (pfname, vot_openExFile (pars, nrows, "pos", &pfd) );
    }
    if (extract & EX_ACREF && acref >= 0) {
	bzero (afname, SZ_LINE);
	strcpy  (afname, vot_openExFile (pars, nrows, "urls", &afd) );
    }
    if ((extract & EX_KML) && (abs(ra) >= 0 && abs(dec) >= 0)) {
	bzero (kfname, SZ_LINE);
	strcpy  (kfname, vot_openExFile (pars, nrows, "kml", &kfd) );

	vot_initKML (kfd, pars);
    }
    if (extract & EX_HTML) {
	if (output && output[0] == '-') {
	    hfd = stdout;
	} else {
	    bzero (hfname, SZ_LINE);
	    strcpy  (hfname, vot_openExFile (pars, nrows, "html", &hfd) );
	}

	vot_initHTML (hfd, pars);
	vot_printHTMLRow (hfd, hline, TRUE, 0);
    }

#ifdef FD_DEBUG
	fprintf (stderr, "%s: pfd=%d  afd=%d  kfd=%d  hfd=%d\n",
	    pars->name, (int)pfd, (int)afd, (int)kfd, (int)hfd);
#endif

    colnum = 0;
    rownum = 1;
    np = 0;
    na = 0;
    strcpy (line, hline);
    lp = line + strlen (line);

    while (*ip) {
	memset (col, 0, SZ_LINE);		/* get value		*/
	for (op=col; *ip && *ip != delim && *ip != '\n';)
	    *op++ = *lp++ = *ip++;

	*lp++ = *ip;				/* save to line buffer	*/
	if (*ip && *ip != '\n') 		/* skip the delimiter  	*/
	    ip++;

	if (id >= 0 && colnum == id)
	    strcpy (s_id, vot_normalize(col));
	else if (ra >= 0 && colnum == ra)
	    strcpy (s_ra, vot_normalizeCoord(col));
	else if (dec >= 0 && colnum == dec)
	    strcpy (s_dec, vot_normalizeCoord(col));
	else if (acref >= 0 &&  colnum == acref)
	    strcpy (s_acref, col);

	if (*ip == '\n') {
	    if (pfd) {
		if (s_id[0])
		    fprintf (pfd, "%s\t%s\t%s\n", 
			vot_normalize(s_id), s_ra, s_dec);
		else
		    fprintf (pfd, "obj%03d\t%s\t%s\n", rownum, s_ra, s_dec);
		np++;
	    }

	    if (afd && s_acref[0]) {
		fprintf (afd, "%s\n", s_acref);
		na++;
	    }
	    if (kfd) {
		/* See if we're sampling the output.  */
		if (row_count < kml_max) {
		    if (!kml_sample || (row_count % kml_sample) == 0) {
		        vot_printKMLPlacemark (kfd, s_id, atof(s_ra),
			    atof(s_dec), line, s_acref, pars);
		    }
		}
	    }
	    if (hfd)
		vot_printHTMLRow (hfd, line, FALSE, row_count);

	    if (kfd || hfd) {
    		bzero (line, SZ_RESULT);
    		strcpy (line, hline);
    		lp = line + strlen (line);
	    }

	    colnum = 0;				/* reinitialize		*/
	    rownum++;
	    bzero (s_id, SZ_LINE);
	    bzero (s_ra, SZ_LINE);
	    bzero (s_dec, SZ_LINE);
	    bzero (s_acref, SZ_URL);
	    ip++;

	    row_count++;
	} else
	    colnum++;
    }
    if (debug) printf ("%s: np = %d  na = %d\n", pars->name, np, na);

    if (pfd) {
	fclose (pfd);
	if (!np) unlink (pfname);
    }
    if (afd) {
	fclose (afd);
	if (!na) unlink (afname);
    }
    if (hfd) vot_closeHTML (hfd);
    if (kfd) vot_closeKML (kfd);


    return (nrows);
}


/************************************************************************
**  Open an extraction file.
*/
char *
vot_openExFile (svcParams *pars, int nrows, char *extn, FILE **ofd)
{
    static char fname[SZ_LINE];
    FILE *fd = (FILE *) NULL;

    bzero (fname, SZ_LINE);
    strcpy (fname, (use_name ? 
	    vot_getOFName (pars, extn, (int)getpid()) : 
	    vot_getOFIndex (pars, extn, (int)getpid())) );

    if ((fd = fopen (fname, "w+")) == (FILE *) NULL) {
	fprintf (stderr, "exiting ERROR opening position file\n");
        vot_dalExit (E_NONE, nrows);
    }

    *ofd = fd;
    return ( fname );
}


/************************************************************************
**  Construct a standard filename from the service params.
*/
char *
vot_getOFName (svcParams *pars, char *extn, int pid)
{
    static char fname[SZ_LINE], *root, spid[16];


    bzero (fname, SZ_LINE);
    bzero (spid, 16);
    sprintf (spid, "_%d", pid);


    /* Create the root part of the name. */
    if (simple_out) {
        sprintf (fname, "%s_%s", vot_normalize (pars->name), pars->oname); 

    } else if (output) {
	root = (output[0] == '-' ? "tmp" : output);

	if (numout)
            sprintf (fname, "%s_%03d_%03d", root, 
		pars->svc_index, pars->obj_index);
	else if (nservices == 1 && nobjects > 1)
            sprintf (fname, "%s_%03d", root,  pars->index);

	else if (nservices > 1 && nobjects == 1)
            sprintf (fname, "%s_%s", root,  vot_normalize(pars->name));

	else if (nservices > 1 || nobjects > 1)
            sprintf (fname, "%s_%s_%c_%03d_%s", 
		root,
	        vot_normalize (pars->name), 
	        vot_svcTypeCode(pars->type), pars->index, spid);
	else
	    strcpy (fname, root);

    } else {

	if (nservices==1 && nobjects > 1)
            sprintf (fname, "%s_%s", vot_normalize (pars->name), pars->oname); 
	else
            sprintf (fname, "%s_%c_%s",
	        vot_normalize (pars->name), 
	        vot_svcTypeCode(pars->type), 
	        (pars->oname[0] ? pars->oname : "pos"));

        if (nservices > 1)
            strcat (fname, spid);
    }

    if (extn) {
        strcat (fname, ".");
        strcat (fname, extn);
    }

    return (fname);
}


char *
vot_getOFIndex (svcParams *pars, char *extn, int pid)
{
    static char fname[SZ_LINE], *root, spid[16];

    bzero (fname, SZ_LINE);
    bzero (spid, 16);
    sprintf (spid, "_%d", pid);


    /* Create the root part of the name. */
    if (output) {
	root = (output[0] == '-' ? "tmp" : output);

	if (numout)
            sprintf (fname, "%s_%03d_%03d", root, 
		pars->svc_index, pars->obj_index);

	else if (nservices == 1 && nobjects > 1)
            sprintf (fname, "%s_%03d", root,  pars->index);

	else if (nservices > 1 && nobjects == 1)
            sprintf (fname, "%s_%s", root,  vot_normalize(pars->name));

	else if (nservices > 1 || nobjects > 1)
            sprintf (fname, "%s_%s_%c_%03d_%s", 
		root,
	        vot_normalize (pars->name), 
	        vot_svcTypeCode(pars->type), pars->index, spid);
	else
	    strcpy (fname, root);

    } else {

	if (nservices==1 && nobjects > 1)
            sprintf (fname, "%s_%03d", vot_normalize (pars->name), pars->index); 
	else
            sprintf (fname, "%s_%c_%03d",
	        vot_normalize (pars->name), 
	        vot_svcTypeCode(pars->type), pars->index);

        if (nservices > 1)
            strcat (fname, spid);
    }

    if (extn) {
        strcat (fname, ".");
        strcat (fname, extn);
    }

    return (fname);
}


/************************************************************************
**  COUNTRESULTS -- Count the number of results in a raw VOTable.
*/
int
vot_countResults (char *result)
{
    int nrows  = 0;
    char  *tr  = (char *) result, 
	  *pos = (char *) result;


    /* Count the number of <TR> elements in the table.  Note we assume
    ** there is only a single resource, or that the totatl count is what
    ** we desire.  Allow for upper or lower-case tag names.
    */
    while (tr) {
	if ((tr = strstr (pos, "<TR>")) || (tr = strstr (pos, "<tr>")))
	    pos = tr + 4, nrows++;
	else
	    break;
    }

    return (nrows);
}


/************************************************************************
**  Exit the process with the given code.  Before leaving, we create a 
**  semaphore based on the pid and set the value to be the result count.
**  This allows us to pass back the information to the parent thread when
**  setting the status.
*/
void
vot_dalExit (int code, int count)
{
    int  rc, sem_id, id = getpid();
    int  status = code;

    if ((sem_id = semget ((key_t)id, 1, IPC_CREAT | 0777)) >= 0)
	rc = semctl (sem_id, 0, SETVAL, count);

/*
    exit (code);
*/
    pthread_exit ((void *) &status);
}


/************************************************************************
** Print the header for the result table.
*/
void
vot_printHdr (int fd, svcParams *pars)
{
    char   buf[SZ_LINE];

    bzero (buf, SZ_LINE);
    sprintf (buf, "#    Service:  %s\n#      Title:  %-64.64s\n",
        pars->name, pars->title);
    write (fd, buf, strlen (buf));

    bzero (buf, SZ_LINE);
    sprintf (buf, 
	"#    ObjName:  %s\n#   Position:  %f %f\n#       Size:  %f\n", 
	pars->oname, pars->ra, pars->dec, pars->sr);
    write (fd, buf, strlen (buf));

    bzero (buf, SZ_LINE);
    sprintf (buf, "# Query Date:  %s\n#\n", vot_procTimestamp()); 
    write (fd, buf, strlen (buf));
}


/************************************************************************
** Print the header for the result count table.
*/
void
vot_printCountHdr ()
{
    extern int format, nobjects;


    if (quiet)
	return;

    if (format == F_CSV && count == 0) {
        if (use_name || all_named)
            fprintf (stderr, "# %sService,%sNRecs\n", 
		((nobjects > 1) ? "Rec," : ""),
		((nobjects > 1) ? "ObjName," : ""));
        else
            fprintf (stderr, "# %sService,%sNRecs\n",
		((nobjects > 1) ? "Rec," : ""),
		((nobjects > 1) ? "RA,Dec," : ""));
    } else if (!meta) {
  	extern int inventory;

        if (nobjects == 1 || inventory) {
            fprintf (stderr, "# %-20s  %s   %s\n# %-20.20s  %s   %s\n",
                "Service", "NRec", "Typ Resource Title",
                "-------", "----", 
		"--- ---------------------------------------------");

        } else {
            if (use_name || all_named) {
                fprintf (stderr, "# %3s  %-12.12s\t%-15.15s\t%s\n",
                    "Rec", "Service", "Source Name", "NRecs\n");
                fprintf (stderr, "# %3s  %-12.12s\t%-15.15s\t%s\n",
                    "---", "-------", "-----------", "-----");
            } else {
                fprintf (stderr, "# %3s  %-12.12s\t  %-24.24s\t%s\n",
	            "Rec", "Service", "    RA         Dec", "NRecs");
                fprintf (stderr, "# %3s  %-12.12s\t  %-24.24s\t%s\n",
	            "---", "-------", "-----------------------", "-----");
            }
        }
    }
}


/************************************************************************
** Print an individual line of the result count table.
*/
int
vot_printCount (Query query, svcParams *pars, int *res_count)
{
    QResponse qr  = (QResponse) 0;		/* Query response	 */
    int    nrec;


    if ((qr = voc_executeQuery (query)) <= 0) {
	return (E_REQFAIL);

    } else {
	nrec = voc_getRecordCount(qr);
	*res_count = nrec;

        if (count_only && nrec <= 0) 
	    return (E_NODATA);
        if (count < 0 && nrec > 0)
	    return (E_NODATA);
	if (nrec <= 0)
	    return (E_NODATA);

	vot_printCountLine (nrec, pars);
        return (E_NONE);
    }

    return (E_NONE);
}


/************************************************************************
** Print an individual line of the result count table.
*/
void
vot_printCountLine (int nrec, svcParams *pars)
{
    int    fmt;
    extern int nobjects;
    extern void ppMultiLine();
    extern char *toSexa();


    if (nrec == 0 || quiet)
	return;

    fmt = (count == 1) ? F_ASCII : pars->fmt;
fmt = F_ASCII;

    switch (fmt) {
    case F_CSV|F_HTML:
    case F_CSV|F_KML:
    case F_TSV:
    case F_ASCII:
	if (nobjects == 1) {
            printf ("  %-20.20s  %4d    %c  ",
		pars->name, nrec, 
		vot_svcTypeCode (pars->type));
	    ppMultiLine (pars->title,  35, 45, 1024);
	    printf ("\n");
        } else if (use_name || all_named) {
            printf ("  %3d  %-12.12s\t%-15.15s\t%6d\n",
	        pars->index, pars->name, pars->oname, nrec);
        } else {
	    /*
            printf ("  %3d  %-12.12s\t%10.6f %10.6f\t%6d",
	        pars->index, pars->name, pars->ra, pars->dec, nrec);
	    */
            printf ("  %3d  %-12.12s\t", pars->index, pars->name); 
	    printf ("%12.12s ", toSexa (pars->ra/15.0));
	    printf ("%12.12s\t%5d", toSexa(pars->dec), nrec);
	    if (pars->oname[0] && pars->oname && strcmp ("none", pars->oname))
                printf (" (%s)", pars->oname);
            printf ("\n");
        }
	break;
    case F_CSV:
	if (nobjects == 1) {
            printf ("%s,%d,%s\n", pars->name, nrec, pars->title);
        } else if (use_name || all_named) {
            printf ("%d,%s,%s,%d\n",
	        pars->index, pars->name, pars->oname, nrec);
        } else {
	    /*
            printf ("%d,%s,%.6f,%.6f,%d\n",
	        pars->index, pars->name, pars->ra, pars->dec, nrec);
	    */
            printf ("%d,%s,", pars->index, pars->name);
	    printf ("%s,", toSexa (pars->ra/15.0));
	    printf ("%s,%d\n", toSexa (pars->dec), nrec);
        }
	break;
    case F_FITS:
        fprintf (stderr, "FITS binary table not yet supported\n");
	break;
    default:
	break;
    }
}


/************************************************************************
** Return a simple character type code for the specified service type.
*/
char
vot_svcTypeCode (int type)
{
    switch (type) {
    case SVC_CONE:	return ('C');		break;
    case SVC_SIAP:	return ('I');		break;
    case SVC_SSAP:	return ('S');		break;
    case SVC_VIZIER:	return ('T');		break;
    case SVC_OTHER:	return ('?');		break;
    }

    return ('?');
}


/************************************************************************
**  GETEXTN -- Get the filename extension given the current format.
*/
char *
vot_getExtn ()
{
    if (format & F_CSV)			/* figure out the extension	*/
        return ("csv");
    else if (format & F_ASCII)
        return ("asv");
    else if (format & F_TSV)
        return ("tsv");
    else if (format & F_RAW)
        return ("xml");
    else if (format & F_KML)
        return ("kml");
    else if (format & F_XML)
        return ("xml");

    return ((char *) NULL);
}


/************************************************************************
**  PROCTIMESTAMP -- Return the timestamp with the system newline removed.
*/
char *
vot_procTimestamp ()
{
    time_t clock = time (0);
    char   *tstr = ctime (&clock);

    tstr[24] = '\0';            /* kill the newline     */

    return (tstr);
}



/************************************************************************
**  CONCAT --  Concatenate the files generated by the query into a
**  single document ordered by the service.
*/
void
vot_concat ()
{
    FILE   *fd = (FILE *) NULL;
    Service *svc = svcList;		/* the service list		*/
    Proc    *proc;			/* process list in each service	*/
    char    *extn, fname[SZ_FNAME];
    int	    nrows = 0;


    extn = vot_getExtn ();		/* figure out the extension	*/

    for (svc=svcList; svc; svc=svc->next) {

        if (output && output[0] == '-') {
	    fd = stdout;
        } else {
	    /* Open a separate file for each service. Since we can't rely
	    ** one services returning the same columns it only makes sense
	    ** to concatenate similar files.
	    */
	    bzero (fname, SZ_FNAME);
	    sprintf (fname, "%s_%d.%s",
		vot_getSName(svc->proc->root), getpid(), extn);

	    if ((fd = fopen (fname, "w+")) == (FILE *) NULL) {
	        fprintf (stderr, "ERROR: Cannot open output file: '%s'\n",
		    fname);
	        continue;
	    }
        }

	/* Concatenate results for each object.
	*/
	nrows = 0;
        for (proc=svc->proc; proc; proc=proc->next)
	    nrows += vot_copyFile (proc->root, extn, vot_getOName(proc->root), 
		fd, (proc == svc->proc), nrows);

        if (fd != stdout)   	/*  close the file descriptor 	*/
	    fclose (fd);
    }

    /*  Clean up the intermediate files if needed.
    */
    if (fd == stdout || (extract & EX_COLLECT))
	vot_clean (extn);
}


/************************************************************************
**  COPYFILE -- Copy a result file to the output XML file.
*/
static int
vot_copyFile (char *root, char *extn, char *name, FILE *fd, int hdr, int nrows) 
{
    char  line[4096], fname[SZ_FNAME];
    FILE  *ifd;
    int   nr = 0;


    bzero (fname, SZ_FNAME);
    sprintf (fname, "%s.%s", root, extn);

    if (access (fname, R_OK) == 0) {
        if ((ifd = fopen (fname, "r")) == (FILE *) NULL) {
	    fprintf (stderr, "Warning: Cannot open file '%s'\n", fname);
	    return (0);
	}
    } else
	return (0);

    /* (Slow) Copy the file until the end of the Document.
    */
    bzero (line, 4096);
    while (fgets (line, 4096, ifd)) {
	if (!nrows && hdr && line[0] == '#')
	    /*fprintf (fd, "# %s", &line[1]); */
	    fprintf (fd, "%s", &line[1]);
	else if (nrows && !hdr && line[0] == '#')
	    ;
	else {
	    fprintf (fd, "%s", line);
	    nr++;
	}
        bzero (line, 4096);
    }
    fflush (fd);

    fclose (ifd);

    return (nr);
}


/************************************************************************
**  CLEANXML -- Clean up the intermediate VOTable files when producing
**  the compiled XML doc..
*/
static void
vot_clean (char *extn)
{
    Service *svc = svcList;		/* the service list		*/
    Proc    *proc;			/* process list in each service	*/
    char    fname[SZ_FNAME];

    for (svc=svcList; svc; svc=svc->next) {
        for (proc=svc->proc; proc; proc=proc->next) {
	    bzero (fname, SZ_FNAME);
	    sprintf (fname, "%s.%s", proc->root, extn);
	    unlink (fname);
	}
    }
}
