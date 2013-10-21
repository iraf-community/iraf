/****************************************************************************
**  VOARGS.C -- Procedures for commandline argument handling.  We also do
**  some of the heavy lifting for registry and object resolution.
**
**  M. Fitzpatrick, NOAO, June 2007
*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include "VOClient.h"
#include "votParse.h"
#include "voAppsP.h"


#define OBJ_DEBUG	0		/* local debug options		*/


extern int   nobjects, nservices;
extern int   verbose, quiet, debug, errno, force_svc, meta;
extern int   all_data, use_name, all_named, url_proc, obj_list;
extern int   force_read, table_hskip, table_nlines, table_sample;
extern char *typestr, *bpass, *delim, *cols, *ecols, *output;

extern Object  *objList, *objTail;

int objIndex		= 0;

int ra_col		= 1;	/* input table RA column default	*/
int ra_span		= 1;	/* input table RA column span		*/
int dec_col		= 2;	/* input table Dec column default	*/
int dec_span		= 1;	/* input table Dec column span		*/
int id_col		= 0;	/* input table ID column  (0 => none)	*/
int id_span		= 0;	/* input table ID column span		*/


int    vot_parseObjectList (char *list, int isCmdLine);
int    vot_printObjectList (FILE *fd);
void   vot_freeObjectList (void);
void   vot_readObjFile (char *fname);

static int   vot_objectResolver (char *idlist, int nwords, int isCmdLine);
static int   vot_loadVOTable (char *fname);
static int   vot_countWords (FILE *fd);
static int   vot_parseCmdLineObject (char *idlist);
static void  vot_getColumns ();

static char *vot_parseTableLine (char *idlist, int nwords,
		double *ra, double *dec); 

extern char  *vot_copyStdin (void);
extern char  *vot_getTableCol (char *line, int col, int span);
extern char  *toSexa (double val);
extern char  *strcasestr ();
extern char  *vot_normalize (char *str);
extern char  *vot_normalizeCoord (char *coord);
extern char  *vot_mktemp (char *root);
extern char  *vot_getTableCell (int vot, int row, int col);

extern int   isVOTable (char *fname);
extern void  vot_skipHdr (FILE *fd);


/* Utility procedures.
*/
extern int    isSexagesimal (char *str);
extern int    isDecimal (char *str);
extern float  sexa (char *s);
extern char   *toSexa (double pos);
extern char   *toSexaTime (int nsec);




/****************************************************************************
**  Parse a string containing object names or positions. The string may be a
**  single object, a comma-delimited list, or the name of a file containing
**  the same.  Additionally, values positions specified as either sexagesimal
**  or decimal RA/Dec (assumed J2000).  Positions may be whitespace or 
**  comma-delimited.
*/
int
vot_parseObjectList (char *list, int isCmdLine) 
{
    FILE  *fd;
    char  line[SZ_LINE];
    int   i, nl = 1, nwords;


    if (access (list, R_OK) == 0) {

	if (isVOTable (list)) {
	    nl = vot_loadVOTable (list);
	    return (0);
	}

	/* Process the file contents. 
	*/
	if ((fd = fopen (list, "r")) == (FILE *) NULL) {
	    fprintf (stderr, "ERROR: Cannot open file '%s'\n", list);
	    return (1);
	}

        /* Count the number of words (tokens) on each line of the list.
        */
	vot_skipHdr (fd);
	if ((nwords = vot_countWords (fd)) < 0 && !force_read) {
	    fprintf (stderr,
		"ERROR: Can't parse object/position table '%s' ", list);
	    fprintf (stderr, "(Variable number of columns).\n");
	    fclose (fd);
	    exit (1);
	}

        /* Parse the column description so we can properly read a table.
        */
        vot_getColumns ();
	vot_skipHdr (fd);			/* prepare		*/

	while (fgets (line, SZ_LINE, fd)) {	/* do it		*/

	    /* Resolve the object name.
	    */
	    vot_objectResolver (line, nwords, FALSE);

	    /* Enforce the max number of lines to read.
	    */
	    if (table_nlines > 0 && nl >= table_nlines)
		break;
	    else
		nl++;

	    /* Enforce the table sampling.
	    */
	    if (table_sample > 1) {
		for (i=1; i < table_sample; i++) {
		    if (fgets (line, SZ_LINE, fd) == (char *)NULL)
			break;
		}
	    }
	}

	fclose (fd);
	use_name = 0;

    } else 
	vot_objectResolver (list, 1, isCmdLine);


    if (debug) 
	vot_printObjectList (stderr);
    if (obj_list) {
        FILE *fd;
        char  listfile[SZ_FNAME];

        memset (listfile, 0, SZ_FNAME);
        sprintf (listfile, "%s.objects", output);
        if ((fd = fopen (listfile, "w+")) != (FILE *) NULL) {
            vot_printObjectList (fd);
            fclose (fd);
        }
    }

    return (0);
}


/****************************************************************************
**  LOADVOTABLE -- Read a VOTable and load the Object list with its contents.
*/
static int
vot_loadVOTable (char *fname)
{
    int  vot = 0, i;
    int  nr, nc, nresults = 0, nwords = 0;
    int  c_ra, c_dec, c_id;
    char ra[SZ_FNAME], dec[SZ_FNAME], id[SZ_FNAME];
    char buf[SZ_LINE];


    if ((vot = vot_openVOTABLE (fname)) ) {

        int   res, tab, data, tdata;

        res   = vot_getRESOURCE (vot);      /* get handles          */
        if ((tab   = vot_getTABLE (res)) <= 0)
            return (-1);
        if ((data  = vot_getDATA (tab)) <= 0)
            return (-1);
        if ((tdata = vot_getTABLEDATA (data)) <= 0)
            return (-1);
        nr    = vot_getNRows (tdata);
        nc    = vot_getNCols (tdata);

        c_ra  = vot_colByUCD (tab, "POS_EQ_RA_MAIN", "pos.eq.ra");
        c_dec = vot_colByUCD (tab, "POS_EQ_DEC_MAIN", "pos.eq.dec");
        c_id  = vot_colByUCD (tab, "ID_MAIN", "meta.id");
	if (OBJ_DEBUG)
	    fprintf (stderr, "%d rows x %d cols  ra:%d  dec:%d  id:%d\n", 
		nr, nc, c_ra, c_dec, c_id);

	for (i=0; i < nr; i++) {
            bzero (ra,  SZ_FNAME); 
            strcpy (ra,  vot_getTableCell(vot, i, c_ra));

            bzero (dec,  SZ_FNAME); 
            strcpy (dec,  vot_getTableCell(vot, i, c_dec));

	    if (c_id >= 0) {
                bzero (id,  SZ_FNAME); 
                
                strcpy (id,  vot_getTableCell(vot, i, c_id));
		nwords = 3;
	    } else
		nwords = 2;

	    if (OBJ_DEBUG)
		fprintf (stderr, "%d:  ra:%s  dec:%s  id:%s\n", i, ra, dec, id);

	    /* Build up a fake line of input containing the RA/Dec/ID on
	    ** a line.  We do it this way so the normalize() procedures
	    ** don't overwrite a string.
	    */
	    bzero (buf, SZ_LINE);
	    strcpy (buf, vot_normalizeCoord (ra));
	    strcat (buf, "  ");
	    strcat (buf, vot_normalizeCoord (dec));
	    strcat (buf, "  ");
	    strcat (buf, (c_id >= 0 ? vot_normalize (id) : ""));

	    if (OBJ_DEBUG) fprintf (stderr,  "resolve buf = '%s'\n",  buf);

	    vot_objectResolver (buf, nwords, FALSE);
	    nresults++;
	}
    } else {
	fprintf (stderr, "Cannot open votable '%s'\n", fname);
	exit (1);
    }

    if (vot > 0)
        vot_closeVOTABLE (vot);

    return (nresults);
}


/****************************************************************************
**  GETCOLUMNS -- Parse the column description parameter to figure out how 
**  to read a table properly.  We only allow for ra/dec/id and the default is
**  that the 'id' column is not present (col=0).  Otherwise, columns are
**  given as a comma-delimited, one-indexed list of integers.  A range may
**  be specified to span multiple columns for reading e.g. multi-word ID tags
**  or whitespace-delimited sexagesimal.
*/

static void
vot_getColumns ()
{
    char *ip = cols, *op, ra_c[SZ_FNAME], dec_c[SZ_FNAME], id_c[SZ_FNAME];
    int  first, last;


    ip = (ecols ? ecols : cols);

    bzero (ra_c, SZ_FNAME);  			/* get RA column	*/
    for (op=ra_c; *ip && *ip != ','; )
        *op++ = *ip++;
    if (*ip) ip++;

    bzero (dec_c, SZ_FNAME);   			/* get Dec column	*/
    for (op=dec_c; *ip && *ip != ','; )
        *op++ = *ip++;
    if (*ip) ip++;

    bzero (id_c, SZ_FNAME);   			/* get ID column	*/
    if (*ip) {
        for (op=id_c; *ip; )
            *op++ = *ip++;
    }


    /* Now parse each of the column strings and create the col/span values.
    */
    if ((ip = strchr (ra_c, (int)'-'))) {	/* set RA columns	*/
	*ip++ = '\0';
	first = ra_col = atoi (ra_c);
	last  = atoi (ip);
	ra_span = (last - first + 1);
    } else {
	ra_col = atoi (ra_c);
	ra_span = 1;
    }

    if ((ip = strchr (dec_c, (int)'-'))) {	/* set Dec columns	*/
	*ip++ = '\0';
	first = dec_col = atoi (dec_c);
	last  = atoi (ip);
	dec_span = (last - first + 1);
    } else {
	dec_col = atoi (dec_c);
	dec_span = 1;
    }

    if (id_c[0]) {
        if ((ip = strchr (id_c, (int)'-'))) {	/* set ID columns	*/
	    *ip++ = '\0';
	    first = id_col = atoi (id_c);
	    last  = atoi (ip);
	    id_span = (last - first + 1);
        } else {
	    id_col = atoi (id_c);
	    id_span = 1;
        }
    }

    if (OBJ_DEBUG) {
	fprintf (stderr, "ra_c='%s' dec_c='%s' id_c='%s'\n",ra_c,dec_c,id_c);
	fprintf (stderr, "     ra:  col=%d  span=%d\n", ra_col, ra_span);
	fprintf (stderr, "    dec:  col=%d  span=%d\n", dec_col, dec_span);
	fprintf (stderr, "     id:  col=%d  span=%d\n", id_col, id_span);
    }
}


/****************************************************************************
**  Free the service list and reset the counter.
*/
void
vot_freeObjectList ()
{
    Object *cur, *next;

    for (cur=objList; cur; cur=next) {
	next = cur->next;
	if (cur)
	    free ((void *) cur);
    }
    nobjects = 0;
    objList = objTail = (Object *) NULL;
}


/****************************************************************************
**  Resolve a service name/list to the proper service URL and store the
**  result in the 'objList' global which we assume is declared in the caller.
*/
static int
vot_objectResolver (char *idlist, int nwords, int isCmdLine)
{
    char    *id = (char *)NULL, *name = (char *)NULL, *ip;
    double  ra, dec;
    Object *obj;
    Sesame  sesame;


    /*  The task was originally written to allow comma-delimieted objects
    **  to be specified on the command line, or as multiple objects in a
    **  file.  To preserve this functionality we'll call the old code when
    **  given a command-line argument, otherwise we'll parse the file
    **  according to more strict rules below.
    */
    if (isCmdLine) {
	return ((int)vot_parseCmdLineObject (idlist));
    }


    /*  If there is only a single item on the line then it can only be either
    **  the name of a file to be processed, or an object to be resolved.
    */
    if (nwords == 1) {

        if (access (idlist, R_OK) == 0) {		/* file 	*/
	    vot_parseObjectList (idlist, FALSE);
    	    return (OK);

	} else {					/* resolve name	*/
            /* Clobber the newline and do a poor-man's URL encoding of
            ** embedded spaces.
            */
            for (ip=idlist; *ip; ip++) {
                /*  preserve spaces.....  */
                if (*ip == ' ') *ip = '+';
                if (*ip == '\n') *ip = '\0';
            }

	    sesame = voc_nameResolver (idlist);
	    ra   = voc_resolverRA (sesame);
	    dec  = voc_resolverDEC (sesame);
	    name = idlist;

	    /* If the positions are zero, make sure the errs are also zero
	    ** before deciding that the resolver failed.  We won't bother
	    ** with this object but will print a warning.
	    */
	    if (ra == 0.0 && dec == 0.0) {
	        if (voc_resolverRAErr (sesame) == 0.0 &&
	            voc_resolverDECErr (sesame) == 0.0) {
			fprintf (stderr,
			    "Warning: Cannot resolve '%s'....skipping\n", name);
			return (ERR);
	        }
	    } else 
	        all_named = 1;
	}

        if (verbose && id && !quiet) {
	    fprintf (stderr,"# Resolver: %-20s -> ", (name ? name : "(none)"));
	    fprintf (stderr," %11.11s", toSexa(ra));
	    fprintf (stderr," %12.12s", toSexa(dec));
	    fprintf (stderr,"\n");
        }

    } else {

	/* The line represents a table of some kind.  In the simplest form
	** this could just be the RA/Dec in a 2-column list but it could
	** also be a CSV file where the 'cols' parameter tell us where to
	** find the values.  The 'id' string may be optional in the table,
	** if not specified we'll make something up.
	*/
	id = vot_parseTableLine (idlist, nwords, &ra, &dec); 

        if (verbose > 1&& id && !quiet) {
	    fprintf (stderr,"# Resolver: %-16.16s", (id ? id : "(none)"));
	    fprintf (stderr," %11.11s", toSexa(ra));
	    fprintf (stderr," %12.12s", toSexa(dec));
	    fprintf (stderr,"\n");
        }

	if (OBJ_DEBUG)
	    fprintf (stderr, "ra=%f dec=%f id='%s'\n", ra, dec, (id?id:"null"));
    }

    /* Save results in the object list.
    */
    obj =  (Object *)calloc (1,sizeof(Object));
    if (!objList)
	objList = objTail = obj;
    else
	objTail->next = (Object *)obj;

    strcpy (obj->name, (name ? name : ""));
    strcpy (obj->id, (id ? id : ""));
    obj->ra  = ra;
    obj->dec = dec;
    obj->index = objIndex++;

    objTail  = obj;


    return (OK);
}


/****************************************************************************
**  PARSETABLELINE --  Parse a line of table input to extract the values for
**  the given ra/dec/id columns.   Values that span multiple columns are
**  concatenated to a single space-delimited string.
*/
static char *
vot_parseTableLine (char *idlist, int nwords, double *ra, double *dec)
{
    char *id = (char *) NULL, *sra = (char *) NULL, *sdec = (char *) NULL;



    if (ecols == (char *)NULL && nwords == 3) {
	/* No user-specified columns, see if we can figure it out.
	*/
	char *c1 = calloc (1, SZ_LINE);
	char *c2 = calloc (1, SZ_LINE);
	char *c3 = calloc (1, SZ_LINE);

	strcpy (c1, vot_getTableCol (idlist, 1, 1));
	strcpy (c2, vot_getTableCol (idlist, 2, 1));
	strcpy (c3, vot_getTableCol (idlist, 3, 1));

	if ( (strchr(c1,(int)'.') && strchr(c2,(int)'.')) ||
	     (strchr(c1,(int)':') && strchr(c2,(int)':')) )  {

	     /* ID is column 3 */
             sra  = strdup (vot_normalizeCoord (c1));
             sdec = strdup (vot_normalizeCoord (c2));
             id   = vot_normalize (c3);

	} else if ( (strchr(c2,(int)'.') && strchr(c3,(int)'.')) ||
	     (strchr(c2,(int)':') && strchr(c3,(int)':')) )  {

	     /* ID is column 1 */
             id   = vot_normalize (c1);
             sra  = strdup (vot_normalizeCoord (c2));
             sdec = strdup (vot_normalizeCoord (c3));

	} else {
	     fprintf (stderr, "ERROR: Unable to parse table colummns\n");
	     *ra = *dec = 0.0;
	     free (c1); free (c2); free (c3);
	     return ((char *)NULL);
	}
        *ra = (strchr (sra, (int)':') ?  (sexa (sra) * 15.) : atof (sra));
        *dec = (strchr (sdec, (int)':') ?  sexa (sdec) : atof (sdec));

	free (c1); free (c2); free (c3);
	free (sra); free (sdec);

    } else {
        sra = vot_normalizeCoord (vot_getTableCol (idlist, ra_col, ra_span));
        *ra = (strchr (sra, (int)':') ?  (sexa (sra) * 15.) : atof (sra));

        sdec = vot_normalizeCoord (vot_getTableCol (idlist, dec_col, dec_span));
        *dec = (strchr (sdec, (int)':') ?  sexa (sdec) : atof (sdec));

        if (nwords == 3 && id_col == 0)
            id  = vot_normalize (vot_getTableCol (idlist, 3, 1));
        else
            id  = vot_normalize (vot_getTableCol (idlist, id_col, id_span));
    }

    return ((char *) id);
}


/****************************************************************************
**  Resolve a service name/list to the proper service URL and store the
**  result in the 'objList' global which we assume is declared in the caller.
*/
static int
vot_parseCmdLineObject (char *idlist)
{
    char    *ip, *op, *id, opos[SZ_LINE];
    double  ra, dec;
    Object *obj;
    Sesame  sesame;
    extern  double sr;


    /* Resolve the (list) of object names/positions and add them to the
    ** service list to be processed.
    */
    ip = idlist; 
    while (*ip) {

	/* Break up the input list into a single ID to resolve.
	*/
	op = &opos[0];
	bzero (opos, SZ_LINE);

	/* We allow positions to be comma-delimited, but objects
	** list are processed individually.
	*/
	while (*ip && *ip != '\n') {
	    if (*ip == ',') {
		if (isDecimal(opos) || isSexagesimal(opos)) {
		    *op++ = ' '; ip++;
		} else {
		    *ip++ = '\0';
		    break;
		}
	    } else
		*op++ = *ip++;
	}
	if (*ip && *(ip-1)) 		/* only advance on a position  	*/
	    ip++;


	/*  Process the name, position or file.
	*/
        if (access (opos, R_OK) == 0) {			/* file 	*/
	    vot_parseObjectList (opos, FALSE);
	    continue;

	} if (isDecimal (opos)) {			/* decimal 	*/
	    for (op=opos; !isspace (*op); op++) 
		;
	    ra  = atof (opos);
	    dec = atof (op);
	    id  = (char *) NULL;
	    all_named = 0;

	} else if (isSexagesimal (opos)) {		/* sexagesimal	*/
	    for (op=opos; !isspace (*op); op++) 
		;
	    ra  = sexa (opos) * 15.0;
	    dec = sexa (op);
	    id  = (char *) NULL;
	    all_named = 0;

	} else {					/* resolve name	*/
	    char *ip = opos;

            /* Clobber the newline and do a poor-man's URL encoding of
            ** embedded spaces.
            */
            for (ip=opos; *ip; ip++) {
                /*  preserve spaces.....  */
                if (*ip == ' ') *ip = '+';
                if (*ip == '\n') *ip = '\0';
            }

	    sesame = voc_nameResolver (opos);
	    ra   = voc_resolverRA (sesame);
	    dec  = voc_resolverDEC (sesame);
	    id = opos;
	    all_named = 1;

	    /* If the positions are zero, make sure the errs are also zero
	    ** before deciding that the resolver failed.  We won't bother
	    ** with this object but will print a warning.
	    */
	    if (ra == 0.0 && dec == 0.0) {
	        if (voc_resolverRAErr (sesame) == 0.0 &&
	            voc_resolverDECErr (sesame) == 0.0) {
			fprintf (stderr,
			    "Warning: Cannot resolve '%s'....skipping\n", opos);
			continue;
	        }
	    }
	}

        if (verbose && id && !quiet)
	    fprintf (stderr,"# Resolver: %-20s -> %-10.10s %.6f %.6f  (%.2f)\n",
		opos, (id ? id : "(none)"), ra, dec, (float)sr);

        /* Save results in the object list.
        */
	obj =  (Object *)calloc (1,sizeof(Object));
        if (!objList)
	    objList = objTail = obj;
	else
	    objTail->next = (Object *)obj;

	strcpy (obj->name, (id ? id : ""));
	obj->ra  = ra;
	obj->dec = dec;
	obj->index = objIndex++;

        objTail  = obj;
    }

    return (0);
}


/****************************************************************************
**  Utility routines to print and count the object list.
*/

int
vot_countObjectList ()
{
    register int i = 0;
    Object *obj = objList;

    if (!obj)
	return (0);
    else
        while ((obj = obj->next)) i++;

    return (i+1);
}


int
vot_printObjectList (FILE *fd)
{
    register int i = 0;
    Object *obj = objList;

    fprintf (fd, "# Objects queried:  %d\n#\n", vot_countObjectList());
    fprintf (fd, "# %4s\t%-12.12s\t%11.11s\t%12.12s\n",
	"Id", "Name", "RA", "Dec");
    do {
	fprintf (fd, "%4d\t%-12.12s\t%11.6f\t%12.6f\n", i++, 
	    obj->name, obj->ra, obj->dec);
    } while ((obj = obj->next));

    return (0);
}



/******************************************************************************
**  COUNTWORDS -- Count the number of words (tokens) on each line of the list.
**  If the number varies for each line it's likely to be a table we can't
**  handle so throw an error, otherwise assume we have a single item or
**  a consistent table.  Allowed delimiters include whitespace, tabs,
**  commas or '|'.  Lines beginning with a '#' are ignored.
*/
static int
vot_countWords (FILE *fd)
{
    char *tok, *line = (char *) NULL;
    char *sep = delim, *del = (char *) NULL;
    int   ntok = -1, last_ntok = -1;

    line = malloc(4096);

    while (1) {

	bzero (line, 4096);			/* get a line of data	*/
	if (fgets (line, SZ_FNAME, fd) == (char *)NULL)
	    break;

        if (line[0] == '#' || line[0] == '\n')  /* skip comments/blank	*/
            continue;

        if (last_ntok < 0) {                    /* first line only      */
            del = strpbrk (line, " \t,|");
            if (del) {
                sep = calloc (1,16);
                sep[0] = del[0];
            } else
                sep = delim; 
        }

        for (ntok=0, tok=strtok(line, sep); tok; tok=strtok(NULL, sep)) {
            if (tok[0] != '\n')
                ntok++;
        }

        if (last_ntok >= 0 && ntok != last_ntok) {
            free ((void *) line);
            return (-1);
        }
        last_ntok = ntok;
    }

    rewind (fd);
    free ((void *) line);

    return (ntok);
}


void
vot_readObjFile (char *fname)
{
    char tmpfile[SZ_FNAME];


    bzero (tmpfile, SZ_FNAME);
        
    if (strcmp (fname, "-") == 0) {		/* read from stdin	*/

	strcpy (tmpfile, vot_copyStdin ());

	/* Parse the input and unlink the temp file.
	*/
        vot_parseObjectList (tmpfile, TRUE);
        if (access (tmpfile, R_OK) == 0)
            unlink (tmpfile);

    } else if (access (fname, R_OK) == 0)
        vot_parseObjectList (fname, TRUE);
}
