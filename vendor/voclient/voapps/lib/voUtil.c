/****************************************************************************
**  VOUTIL.C -- Utility procedures for the VO-CLI tasks.
** 
**  M. Fitzpatrick, NOAO, June 2007
*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <time.h>

#include <curl/curl.h>
#include <curl/easy.h>

#include "VOClient.h"
#include "votParse.h"
#include "voAppsP.h"
#include "voApps.h"


#define SZ_RESBUF		(256*256)
#define SZ_QUERY		40960
#define SZ_QSTRING		40960

#define DEF_RESATTR		"ServiceURL"
#define MAX_ATTRS		32

static int  reg_nresolved 	= 0;


extern int  verbose, count, debug, errno, meta, res_all, quiet, do_votable;
extern int  group, nterms, table_hskip, ecols;
extern char *terms[], *delim;


int   vot_regResolver (char *id, char *svctype, char *bpass, char *subject,
		    	char *clevel, char *fields, int index, int exact, 
			int dalOnly, char **result);
int   vot_regSearch (char **ids, int nids, char *svctype, char *bpass,
		    	char *subject, char *clevel, int orValues, 
			int votable, FILE *vot_fd,
			int dalOnly, int sortRes, int terse);


void  pretty_print (char *result, int nresults);
void  pretty_print_table (char *result, int nresults, char *fields);
void  ppMultiLine (char *result, int poffset, int pwidth, int maxchars);
void  ppResSummary (char *result, int nresults);

void  vot_skipHdr (FILE *fd);
char *vot_getTableCol (char *line, int col, int span);
int   vot_isNumericField (handle_t field);
int   vot_fileType (char *fname);

char *vot_urlFname (char *url);
void  vot_printAttrs (char *fname, Query query, char *id);

char *vot_parseSvcType (char *svctype, int exact);
char *vot_parseBandpass (char *bpass);
char *vot_parseSubject (char *subject);
char *vot_parseCLevel (char *level);
char *vot_mktemp (char *root);
char *vot_copyStdin (void);

/* Utility procedures.
*/
int    isVOTable (char *fname);
int    isSexagesimal (char *str);
int    isDecimal (char *str);
float  sexa (char *s);
char   *toSexa (double pos);
char   *toSexaTime (int nsec);
char   *xmlEncode (char *in);

int	vot_atoi (char *val);
long	vot_atol (char *val);
double	vot_atof (char *val);

void   vot_setArg (char **argv, int *argc, char *value);
void   vot_printRegVOTableHdr (FILE *fd);
void   vot_printRegVOTableRec (FILE *fd, RegResult resource, int recnum);
void   vot_printRegVOTableTail (FILE *fd);

extern char *strcasestr ();




/****************************************************************************
**  Resolve a (presumed) ShortName or Identifier string to one or more
**  Registry resource attributes.  By default we assume we are interested
**  only in the ServiceURL, however optional arguments allow us to narrow
**  the search to particular service types or individual records.  Examples:
**
**  1) Find the ServiceURL for the GSC2.2 catalog
**
**	cl> =regResolver ("GSC2.2")
** 	http://chart.stsci.edu/GSCVO/GSC22VO.jsp?
**	cl> =nresolved()
**	2				# found more than one resource
**
**  2) Print the Title and ServiceType for each record found for USNO-B1:
**
**	cl> print (regResolver ("USNO-B1","","ServiceType,Title",-1))
**	CONE    USNO-B1 Catalogue
**	SKYNODE USNO-B1 SkyNode from VizieR
**
**	Note that in usage such as this we are still limited by the length
**	of output permitted by the print() function (currently 32*SZ_LINE).
**
**  3) Get the ServiceURL for the USNO-B1 Skynode service:
**
**	cl> =regResolver ("USNO-B1","skynode")
**	http://cdsws.u-strasbg.fr/USNO-B1BasicSkyNode/services/BasicSkyNode
**
*/

int 
vot_regResolver (char *term, char *svctype, char *bpass, char *subject, 
		 char *clevel, char *fields, int index, int exact, 
		 int dalOnly, char **res)
{
    int       i, j, nreturns=0, nattrs=0, ret_attr_start=0, try_again=1;
    int	      match=-1, istart, iend, recnum, alen, blen, bsize;
    char      *ip, *field_str = (char *)NULL;
    char      *attr_val=NULL, *attr_list[MAX_ATTRS], id[SZ_LINE];
    char      qstring[SZ_QSTRING], *svcstr, *bpstr, *substr, *conlev, *buf;
    char      attr_str[SZ_LINE], sbuf[SZ_RESBUF];
    RegQuery  query = 0;
    RegResult resource = 0;


    if (term[0]) {
	bzero (id, SZ_LINE);
	strcpy (id, term);
    }
    if (debug)
	fprintf (stderr, "regResolver init: id='%s' type='%s'\n", 
	        (id[0] ? id : "null"), (svctype ? svctype : "null"));

    /*  Sanity checks.
    */
    if (!id[0] && !svctype && !svctype[0]) {
	fprintf (stderr, "regResolver():  Must specify 'id' or 'svctype'\n");
	return (0);
    }

    bzero (qstring, SZ_QSTRING);
    bzero (sbuf, SZ_RESBUF);
    strcpy (attr_str, DEF_RESATTR);


    /* Set the default attribute list we want from the query.
     */
    attr_list[0] = "ShortName";
    attr_list[1] = "Identifier";
    attr_list[2] = "Title";
    attr_list[3] = "AccessUrl";
    nattrs = 4;

    /* Parse the fields argument if any.
    */
    if (fields && fields[0]) {
	/* If we were given a list of fields, assume we want only those
	** and in that particular order.
	*/
	ret_attr_start = nattrs;

	/* Split up the input 'fields' string for use in the attr_list.
	*/
	field_str = strdup (fields);
        for (ip=field_str; *ip && nattrs < MAX_ATTRS; ) {
            attr_list[nattrs++] = ip;
            while (*ip && *ip != ',')
                ip++;
            if (*ip == ',')
                *ip++ = '\0';
            else
                break;
        }
    } else
	ret_attr_start = nattrs - 1;		/* return ServiceURL only  */


    /* Now parse the service type and bandpass for extra constraints.
    */
    svcstr = vot_parseSvcType (svctype, !res_all);
    bpstr  = vot_parseBandpass (bpass);
    substr = vot_parseSubject (subject);
    conlev = vot_parseCLevel (clevel);

    if (strcmp (id, "%") == 0) {
	if (bpass || svctype || subject) {

	    /* Protect against a French overload.
    	     */
	    if (!(subject || bpass) &&
		(strcasestr(svctype,"table") || 
		strcasestr(svctype,"tabular") ||
		strcasestr(svctype,"vizier"))) {
	            fprintf (stderr, "Warning: Query too large to process.\n");
	 	    return (0);
	    }

	    bzero (qstring, SZ_QSTRING);
    	    if (bpstr) {  			/* add bandpass constraint  */
        	strcpy (qstring, bpstr);
	    }
    	    if (svcstr) {			/* add service constraint   */
		if (qstring[0]) 
		    strcat (qstring,  " AND ");
        	strcat (qstring, svcstr);
    	    }
    	    if (substr) {			/* add subject constraint   */
		if (qstring[0]) 
		    strcat (qstring,  " AND ");
        	strcat (qstring, substr);
    	    }
    	    if (conlev) {			/* add content constraint   */
		if (qstring[0]) 
		    strcat (qstring,  " AND ");
        	strcat (qstring, conlev);
    	    }

    	    /*  Execute the query.  */
	    if (strcasestr (svcstr, "catalog"))
    	        resource = voc_regSearch (qstring, "catalog", 0);
	    else
    	        resource = voc_regSearch (qstring, NULL, 0);

	} else {
	    fprintf (stderr, "Warning: Query too large to process.\n");
	    return (0);
	}

    } else { 

        /* Lastly, set the query string we'll be using.
        */
retry:
        if (group) {
	    for (i=0; i < nterms; i++) {
	        strcat (qstring, terms[i]);
	        if (i < (nterms -1))
	            strcat (qstring, " OR ");
	    }
        } else if (id[0]) {
	    char  term[SZ_LINE], *ip, *op;

	    memset (term, 0, SZ_LINE);
	    for (ip=id, op=term; *ip && *ip != '#'; )
		*op++ = *ip++;

            if (exact) {
                sprintf (qstring,
	           "((Identifier like '%s') OR (ShortName like '%s'))", 
			term, term);
            } else {
                sprintf (qstring,
		   "((Identifier like '%%%s%%') OR (ShortName like '%%%s%%'))",
		   term, term);
            }
        }

        /*  Do the registry query.  Add the service-specific part of the query
        **  once we get a handle.
        */
        query = voc_regQuery (qstring, 0);
        if (bpass && bpass[0]) {
	    voc_regAddSearchTerm (query, bpstr, 0);
            voc_regConstWaveband (query, bpass);
	}
        if (svctype && svctype[0]) {
	    voc_regAddSearchTerm (query, svcstr, 0);
            voc_regConstSvcType (query, svctype);
	}
        if (substr && substr[0])
	    voc_regAddSearchTerm (query, substr, 0);
        if (conlev && conlev[0])
	    voc_regAddSearchTerm (query, conlev, 0);
	if (dalOnly) 
	    voc_regDALOnly (query, dalOnly);

        if (debug) {
	    printf ("regResolver: id='%s' type='%s' fields='%s' index=%d\n", 
	        (id[0] ? id : "null"), 
	        (svctype ? svctype : "null"), 
	        (fields ? fields : "null"), index);
	    printf ("query string:\n\n%s\n\n", voc_regGetQueryString (query));
        }
	    
        /*  Execute the query.
         */
        resource = voc_regExecute (query);
    }

    blen = 0;			/* length of result buffer used	*/

    /* Save the number of resolved resources and get the requested attribute
     * (or the default service URL).
     */
    reg_nresolved = voc_resGetCount (resource);
    if (reg_nresolved > 0) {
	int isVizier = 0;

        bsize = (reg_nresolved * SZ_LINE);   /* max size of result buffer */
        buf = (char *) calloc (1, bsize);

        if (do_votable) {
            for (i=0; i < reg_nresolved; i++)
                vot_printRegVOTableRec (stdout, resource, i);

            return (reg_nresolved);	/* return number of matches found */
        }

	nreturns = (index >= 0 ? 1 : reg_nresolved);

	match = 0;
	if (index >= 0 && reg_nresolved > 1) {
	    /* We didn't specify a record number but have more than one
	     * result.  Look for an exact match in the ShortName or
	     * Identified which was a hidden part of the query.
	     */
	    for (i=0; i < reg_nresolved; i++) {
	        for (j=0; j < 2; j++) {
                    if ((attr_val = voc_resGetStr(resource, attr_list[j], i))) {
		        if (strncasecmp (id, attr_val, strlen(attr_val)) == 0) {
			    reg_nresolved = 1;
			    nreturns = 1;
		            match = i;
		            break;
		        }
		    }
		}
	    }
	}

	if (index >= 0) {
	    istart = index;
	    iend   = index + 1;
	    reg_nresolved = 1;
	} else if (match >= 0 && nreturns == 1) {
	    istart = match;
	    iend   = match + 1;
	    reg_nresolved = 1;
	} else {
	    istart = 0;
	    iend   = nreturns;
	}

	/*  For a negative index we list the attr for all records.
	 */
	recnum = 0;
	if (dalOnly)
	    reg_nresolved = 0;
	for (i=istart; i < iend; i++) {
	    isVizier = 0;

	    if (dalOnly) {
		int valid = 1;
	        for (j=ret_attr_start; j < nattrs; j++) {
		    if (strcmp (attr_list[j], "CapabilityStandardID") == 0) {
                	attr_val = voc_resGetStr (resource, attr_list[j], i);
			if (attr_val) {
			    char *a = attr_val;
			    if (! (strncasecmp (a, "cone", 4) == 0 ||
				   strncasecmp (a, "sia", 3) == 0 ||
				   strncasecmp (a, "simpleimage", 10) == 0 ||
				   strncasecmp (a, "ssap", 4) == 0 ||
				   strncasecmp (a, "simplespec", 9) == 0)) {
			    	 	valid = 0;
			    } else
				reg_nresolved++;
			}
			break;
		    }
	        }
		if (!valid)
		    continue;
	    }

	    for (j=ret_attr_start; j < nattrs; j++) {
                attr_val = voc_resGetStr (resource, attr_list[j], i);
		if (strncasecmp (attr_list[j], "identifier", 10) == 0)
		    isVizier = (attr_val && strcasestr (attr_val, "vizier"));
                if (attr_val) {
	 	    alen = strlen (attr_val);
		    strcat (buf, attr_val);
		    if (j < (nattrs-1))
			strcat (buf, "\t");
		    voc_freePointer ((char *) attr_val);
		} else {
		    alen = 0;
		    strcat (buf, (isVizier ? "Catalog\t" : "INDEF\t"));
		}

		blen += max (alen, 10);
	    }
	    //if (nreturns > 1 && i < (reg_nresolved-1))
	    if (nreturns > 1)
		strcat (buf, "\n");

	    recnum++;
	}
	
	attr_val = (char *) NULL;

    } else {

	if (try_again) {
             if (strncasecmp("ivo://CDS.VizieR",id,16) == 0) {
                char *ip, ivorn[SZ_LINE];
                int len = strlen (id);

                bzero (ivorn, SZ_LINE);
                strcpy (ivorn, id);
                for (ip = &ivorn[len-1]; *ip != '/'; ip--) 
		    *ip = '\0';
		*ip = '\0';

		strcpy (id, ivorn);
		try_again = 0;
		goto retry;
            }
	}

        buf = (char *) calloc (1, SZ_RESBUF);
	for (j=0; j < nattrs; j++) {
            strcpy (buf, "INDEF");
	    if (j < (nattrs-1))
		strcat (buf, "\t");
	}
    }

    if (field_str) free ((char *) field_str); 	/* free local memory	*/

    /* Push the result operand on the return variable.  We assume the result
    ** is large enough for the value.
    */
    *res = buf;

    if (bpstr)  free (bpstr);
    if (svcstr) free (svcstr);
    if (substr) free (substr);

    if (debug)
	printf ("regResolver: reg_nresolved = %d\n", reg_nresolved);

    return (reg_nresolved);		/* return number of matches found */
}


/****************************************************************************
**  Search the registry for the given phrase and constraints.  
*/
int 
vot_regSearch (char **ids, int nids, char *svctype, char *bpass, 
		   char *subject, char *clevel, int orValues, int votable, 
		   FILE *vot_fd, int dalOnly, int sortRes, int terse)
{
    int       i, nresults=0, nattrs=0, keywOnly = 1, haveADQL;
    char      *attr_val=NULL, *attr_list[MAX_ATTRS];
    char      qstring[SZ_QUERY], keyws[SZ_RESBUF], *svcstr, *bpstr, *substr;
    char      term2[SZ_QUERY], *conlev;
    char      attr_str[SZ_LINE], *results, buf[SZ_LINE], cname[SZ_LINE];
    RegResult res = 0;


    strcpy (attr_str, DEF_RESATTR);

    /* Set the default attribute list we want from the query.
    */
    attr_list[0] = "Title";
    attr_list[1] = "CapabilityStandardID";
    attr_list[2] = "ShortName";
    attr_list[3] = "Subject";
    attr_list[4] = "Identifier";
    attr_list[5] = "ServiceUrl";
    attr_list[6] = "Description";

    switch (verbose) {
    case 0: 	nattrs = 4; 	break;
    case 1: 	nattrs = 5; 	break;
    case 2: 	nattrs = 7; 	break;
    }


    /* Now parse the service type and bandpass for extra constraints.
    */
    svcstr = vot_parseSvcType (svctype, !res_all);
    bpstr  = vot_parseBandpass (bpass);
    substr = vot_parseSubject (subject);
    conlev = vot_parseCLevel (clevel);

    /* Begin forming the SQL query term we'll use
    */
    bzero (qstring, SZ_QUERY);
    bzero (term2, SZ_QUERY);
    bzero (keyws, SZ_RESBUF);
    

    /* Extract any terms that may be ADQL strings.
    */
    for (i=0; i < nids; i++) {
	if (strcasestr (ids[i], "like") ||
	    strcasestr (ids[i], "<") ||
	    strcasestr (ids[i], ">") ||
	    strcasestr (ids[i], "=")) {
		if (qstring[0] && orValues)
	    	    strcat (qstring, " OR ");
		else if (qstring[0])
	    	    strcat (qstring, " AND ");
                sprintf (buf, "(%s)", ids[i]);
                strcat (qstring, buf);
		keywOnly = 0;
		haveADQL = 1;
	}
    }
    for (i=1; i < nids; i++) {
	strcat (term2, ids[i]);
	if (i < (nids-1))
            strcat (term2, " ");
    }

    if (nids == 0 || (nids == 1 && strcmp ("any", *ids) == 0)) {
	strcpy (qstring, "(Identifier like '%')");
	keywOnly = 0;
    }


    if (!keywOnly || nids == 0) {
        if (svcstr) {			/* add service constraint	*/
	    if (qstring[0]) {
                sprintf (buf, "(%s) AND (%s)", qstring, svcstr);
                strcpy (qstring, buf);
	    } else {
	        strcat (qstring, svcstr);
	    }
        }
        if (bpstr) {			/* add bandpass constraint	*/
	    if (qstring[0]) {
                sprintf (buf, "(%s) AND (%s)", qstring, bpstr);
                strcpy (qstring, buf);
	    } else {
	        strcat (qstring, bpstr);
	    }
        }
        if (substr) {			/* add subject constraint	*/
	    if (qstring[0]) {
                sprintf (buf, "(%s) AND (%s)", qstring, substr);
                strcpy (qstring, buf);
	    } else {
	        strcat (qstring, substr);
	    }
        }
        if (conlev) {			/* add subject constraint	*/
	    if (qstring[0]) {
                sprintf (buf, "(%s) AND (%s)", qstring, conlev);
                strcpy (qstring, buf);
	    } else {
	        strcat (qstring, conlev);
	    }
        }
    }


    /* Build a string arrays of the keyword terms.
    */
    if (ids[0] && keywOnly) {
	RegQuery query = (RegQuery) 0;

        if (nids == 0 || (nids == 1 && strcmp ("any", *ids) == 0)) {
	    strcpy (qstring, "(Identifier like '%')");
            query = voc_regQuery (qstring, FALSE); 	/* get a query object */

        } else {
            for (i=0; i < nids; i++) {
	        if (keyws[0])
	            strcat (keyws, " ");
	        if (!strcasestr (ids[i], "like") &&
	            !strcasestr (ids[i], "<") &&
	            !strcasestr (ids[i], ">") &&
	            !strcasestr (ids[i], "="))
            	        strcat (keyws, ids[i]);
            }
            query = voc_regQuery (keyws, orValues); 	/* get a query object */
        }
		
#ifdef USE_CONSTRAINTS
	if (svctype) 					/* set constraints    */
	    voc_regConstSvcType (query, svctype);
	if (bpass)
	    voc_regConstWaveband (query, bpass);
#else
        if (svcstr && svcstr[0])
	    voc_regAddSearchTerm (query, svcstr, 0);
        if (bpstr && bpstr[0])
	    voc_regAddSearchTerm (query, bpstr, 0);
#endif

        if (substr && substr[0])
	    voc_regAddSearchTerm (query, substr, 0);
        if (conlev && conlev[0])
	    voc_regAddSearchTerm (query, conlev, 0);
	if (dalOnly) 
	    voc_regDALOnly (query, dalOnly);
	voc_regSortRes (query, sortRes);
        
        res = voc_regExecute (query);			/* execute it         */

    } else {
/*
        res = voc_regSearch (qstring, NULL, orValues);
*/
        res = voc_regSearch (qstring, term2, orValues);
	bzero (keyws, SZ_RESBUF);
    }
    nresults = voc_resGetCount (res);
	

    /* If no response, see if it was a ShortName used as a keyword term...
    */
    if (nresults == 0 && !(svcstr || bpstr || substr || conlev) && !haveADQL) {
	bzero (qstring, SZ_QUERY);
        if (ids[0]) {
            for (i=0; i < nids; i++) {
		if (qstring[0])
	            strcat (qstring, " OR ");
	        if (!strcasestr (ids[i], "like") &&
	            !strcasestr (ids[i], "<") &&
	            !strcasestr (ids[i], ">") &&
	            !strcasestr (ids[i], "=")) {
                	sprintf (buf, 
			    "(ShortName like '%s') OR (Identifier like '%s')",
			    ids[i], ids[i]);
                	strcat (qstring, buf);
		}
            }
        }

        res = voc_regSearch (qstring, (nids ? keyws : NULL), 1);

        nresults = voc_resGetCount (res);
	if (nresults && !count) {
	    verbose = 2;
        } else if (nresults == 0) {
            res = voc_regSearch (qstring, NULL, orValues);
            nresults = voc_resGetCount (res);
        }
    }

    if (debug)
	printf ("regSearch: qstr='%s' keyws='%s' nres = %d\n",
	    qstring, keyws, nresults);
	

    /* No longer need the query buffers so free them here.
    */
    if (svcstr) free (svcstr);
    if (bpstr)  free (bpstr);
    if (substr) free (substr);


    /* Return the result count if that's all we wanted.
    */
    if (count) {
	results = (char *)calloc (1, (nresults * 30));
	strcpy (results, " \t");
        for (i=0; i < nresults; i++) {
            attr_val = voc_resGetStr (res, attr_list[1], i);	/* SvcType    */
	    strcat (results, (attr_val ? attr_val : " "));
	    strcat (results, "\t \n\t");
	    voc_freePointer ((char *) attr_val);
        }
	if (keyws[0])
            printf ("%-20s      %3d\t", keyws, nresults);
	else
            printf ("%d  ", nresults);
        if (verbose && !bpstr)
            ppResSummary (results, nresults);
        printf ("\n");

	free ((char *) results);
	return (nresults);
    }


    if (votable) {
        for (i=0; i < nresults; i++) {
	    vot_printRegVOTableRec (vot_fd, res, i);
	}
    	return (nresults);		/* return number of matches found */
    }

    /* Print out the results for each resource based on the verbosity
    ** level.
    **
    **     Title:  <title>	   ServiceType: <type>		verb = 0
    **     ShortName:  <name>      Subject:  <id>		verb = 0
    **     Identifier:  <id>       ServiceURL:  <url>		verb = 1
    **     Description:  <descr>				verb = 2
    **
    */
    for (i=0; i < nresults; i++) {

	if (i > 0 && !terse)
	    printf ("-----------------------------------------------\n");

	if (dalOnly) {						/* CapName    */
            attr_val = voc_resGetStr (res, "CapabilityName", i);
	    bzero (cname, SZ_LINE);
	    strcpy (cname, (attr_val ? attr_val : ""));
	    voc_freePointer ((char *) attr_val);
	}

	if (terse) {
	    /*
	    int idx  = voc_resGetInt (res, "index", i),
            	rank = voc_resGetInt (res, "rank",  i);

	    printf ("%2d(%2d) ", rank, idx);
	    */
	    if (sortRes)
	        printf ("%3d %3d ", i, voc_resGetInt(res, "index", i) );

	    if (terse > 1) {
		/* "Tweet" format.
		*/
		char *ip;

	        printf ("New VO Resource: ");

                attr_val = voc_resGetStr (res, "Title", i);
	        printf ("\"%-s\" ", attr_val);
	        voc_freePointer ((char *) attr_val);

                attr_val = voc_resGetStr (res, "Waveband", i);
	        printf ("W:%s ", attr_val);
	        voc_freePointer ((char *) attr_val);

                attr_val = voc_resGetStr (res, "CapabilityStandardID", i);
	        printf ("T:%s ", attr_val);
	        voc_freePointer ((char *) attr_val);

                attr_val = voc_resGetStr (res, "Subject", i);
		if (attr_val && (ip = strchr (attr_val, (int)':')))
		    *ip = '\0';		/* kill qualifiers	*/
	        printf ("S:%-s\n", attr_val);
	        voc_freePointer ((char *) attr_val);

	    } else {
                attr_val = voc_resGetStr (res, attr_list[1], i);/* SvcType    */
	        printf ("%-7.7s ", attr_val);
	        voc_freePointer ((char *) attr_val);

                attr_val = voc_resGetStr (res, attr_list[0], i);/* Title      */
	        printf ((sortRes ? "%-63.63s\n" : "%-71.71s\n"), attr_val);
	        voc_freePointer ((char *) attr_val);
	    }

	    continue;

	} else {
            attr_val = voc_resGetStr (res, attr_list[1], i);	/* SvcType    */
	    printf ("       Type: %-s\n", attr_val);
	    voc_freePointer ((char *) attr_val);

            attr_val = voc_resGetStr (res, attr_list[0], i);	/* Title      */
	    printf ("      Title: ");
            ppMultiLine (attr_val, 13, 67, 1024);
	    printf ("\n");
	    voc_freePointer ((char *) attr_val);
	}

        attr_val = voc_resGetStr (res, attr_list[2], i);	/* ShortName  */
	if (dalOnly && verbose == 0) {
	    printf ("  ShortName: %-s\n", attr_val);
	    printf ("ServiceName: %s\n", cname);
	} else
	    printf ("  ShortName: %-s\n", attr_val);
	voc_freePointer ((char *) attr_val);

        attr_val = voc_resGetStr (res, attr_list[3], i);	/* Subject    */
	printf ("    Subject: ");
        ppMultiLine (attr_val, 13, 67, 1024);
	printf ("\n");
	voc_freePointer ((char *) attr_val);

	if (verbose == 0)
	    continue;

        attr_val = voc_resGetStr (res, attr_list[4], i);	/* Identifier */
	if (dalOnly)
	    printf (" Identifier: %-s#%s\n", attr_val, cname);
	else
	    printf (" Identifier: %-s\n", attr_val);
	voc_freePointer ((char *) attr_val);

        attr_val = voc_resGetStr (res, attr_list[5], i);	/* ServiceUrl */
	printf (" ServiceURL: %-s\n", attr_val);
	voc_freePointer ((char *) attr_val);

	if (verbose == 1)
	    continue;

        attr_val = voc_resGetStr (res, attr_list[6], i);	/* Descr.     */
	printf ("Description: ");
        ppMultiLine (attr_val, 13, 67, 1024);
	printf ("\n");
	voc_freePointer ((char *) attr_val);
    }

    return (nresults);			/* return number of matches found */
}


/************************************************************************
**  PRETTY_PRINT --   Pretty-print a table for verbose resolution output.
*/
void
pretty_print (char *result, int nresults)
{
    int   i, j, ncols, col;
    char  *ip, value[SZ_RESULT];


    /* pretty-print the table using the defined widths.
    */
    if (verbose) {
        ip = result;
	ncols = 3;
        for (i=0; i < nresults; i++) {
            j = 0;
	    for (col=0; col < ncols; ) {
                bzero (value, SZ_RESULT);
		while (*ip && (*ip != '\t' && *ip != '\n'))
		    value[j++] = *ip++;

		if (col == (ncols-1)) {
		    ppMultiLine (value, PP_OFFSET, PP_WIDTH, PP_MAXCHARS);
		    break;
		} else
		    printf ("%-20s", value);

		j = 0;
		col++;
		ip++;
	    }
	    printf ("\n");
	    ip++;
        }
    } else {
        printf ("%s\n", result);
	return;
    }

}


/************************************************************************
**  PPRESSUMMARY --  Print a summary of the resources found.  Assumes the
**  ServiceType is the second column of the result string.
*/

void
ppResSummary (char *result, int nresults)
{
    register int i, j;
    int    Ni = 0,			/* image count		*/ 
	   Nc = 0, 			/* catalog count	*/
	   Nt = 0, 			/* table count		*/
	   Ns = 0, 			/* spectra count	*/
	   Nsn = 0, 			/* SkyNode count	*/
	   Nother = 0;			/* 'other' count	*/
    char  *ip, value[SZ_RESULT];


    if (nresults == 0 || verbose == 1)
	return;

    ip = result;
    for (i=0; i < nresults; i++) {
	bzero (value, SZ_RESULT);
	while (*ip != '\t') ip++; 	/* skip first column	*/
	for (++ip, j=0; *ip != '\t';)   /* gather the value	*/
	    value[j++] = tolower (*ip++);
	while (*ip && *ip != '\n') ip++; 	/* skip last column	*/
	ip++;

	if (strstr (value, "siap"))
	    Ni++;
	else if (strstr (value, "cone"))
	    Nc++;
	else if (strstr (value, "tabular"))
	    Nt++;
	else if (strstr (value, "ssap"))
	    Ns++;
	else if (strstr (value, "skynode") || strstr (value, "skyservice"))
	    Nsn++;
	else
	    Nother++;
    }

    printf ("(");
    if (Nc) printf ("Cat: %-2d ", Nc);
    if (Nt) printf ("Tab: %-2d ", Nt);
    if (Ni) printf ("Img: %-2d ", Ni);
    if (Ns) printf ("Spec: %-2d ", Ns);
    if (Nsn) printf ("SNode: %-2d ", Nsn);
    if (Nother) printf ("Other: %-2d", Nother);
    printf (")");
}



/************************************************************************
**  PPMULTILINE --  Print a lengthy string on multiple lines.  Used to print
**  the last column of a table where we indent the carried-over lines to the
**  specified offset.  No effort is made to break lines at a 'nice' spot
**  since long URLs and such won't fit anyway, so we just cut the line and
**  continue.
*/

void
ppMultiLine (char *result, int poffset, int pwidth, int maxchars)
{
    register int i, j, ellipses = 0;
    int len = strlen((result ? result : ""));
    char *ip;
    extern int longlines;

    if (result)
	len = strlen (result);
    else
	return;

	
    for (i=0; i < len-1; i++ ) {
	if (result[i] == '\n' && result[i+1] != '\n')
	    result[i] = ' ';
    }

    ip = &result[len-1]; 			/* strip trailing w/s	*/
    while ((isspace(*ip) || *ip == '\n') && ip > result)
        *ip-- = '\0';

    if (longlines) {
	printf ("%s", result);
	return;
    }

    if (len > maxchars) {
	result[maxchars] = '\0';
	len = maxchars;
	ellipses++;
    }

    if (len < pwidth) {
	for (ip=result; *ip && isspace(*ip); )
	    ip++;
        printf ("%s", ip);
    } else {
	j = pwidth;
	for (i=0; i < len; ) {
	    while (isspace (result[i])) i++;

            printf ("%-*.*s\n", pwidth, pwidth, &result[i]);
	    i = j + 1;
	    j += pwidth;
            printf ("%*s", poffset, " ");
	    if (j > len) {
	        while (isspace (result[i])) i++;
                printf ("%s", &result[i]);
		if (ellipses)
                    printf (" (read more)....");
		break;
	    }
	}
    }
}


/************************************************************************
**  PRETTY_PRINT_TABLE --  Pretty-print a table with computed column widths.
*/
void
pretty_print_table (char *result, int nresults, char *fields)
{
    int   i, j, w, ncols, col, width[256];
    char  *ip, value[SZ_RESULT];


    if (!result)
	return;    
    if (!verbose && !fields) {
        printf ("%s\n", result);
	return;
    }


    /* Figure out how many columns there are.
    */
    for (ip=result, ncols=1; *ip && *ip != '\n'; ip++) {/* do only 1st line */
	if (*ip && *ip == '\t')
	    ncols++;
    }

    /* Calculate the column widths needed to align the columns.
    */
    bzero (width, 256 * sizeof(int));
    ip = result;
    for (i=0; i < nresults; i++) {
	for (w=0, col=0; *ip; ) {
	    if (*ip == '\t' || *ip == '\n') {
		if (w >= width[col])
		    width[col] = w + 3;
		w = 0;
	   	col++;
	    } else
	        w++;
	    if (*ip++ == '\n')
	         break;
	}
    }

	
    /* Now print the table using the computed widths.
    */
    ip = result;
    for (i=0; i < nresults; i++) {
        j = 0;
        bzero (value, SZ_RESULT);
	for (col=0; col < ncols; ) {
	    if (!*ip || (*ip == '\t' || *ip == '\n')) {
		value[j++] = '\0';
		if (col == (ncols-1))
		    printf ("%-s", value);
		else
		    printf ("%-*s", width[col], value);
		j = 0;
		col++;
	    } else
		value[j++] = *ip;
	    if (*ip && *ip == '\n')
	        break;
	    ip++;
	}
	printf ("\n");
	ip++;
    }
}



/****************************************************************************
**  VOT_PARSESVCTYPE -- Parse the service type specification.  Allow for
**  synonyms like 'catalog' for 'cone', 'image' for 'siap', etc.  
*/
char *
vot_parseSvcType (char *svctype, int exact)
{
    char   *ip, *op, val[SZ_FNAME], buf[SZ_FNAME];
    char   *svcstr = NULL, *like = NULL;
    int	   not = 0, more = 0;


    like = (exact ? "" : "%");			/* initialize		*/

    if (svctype && svctype[0]) {
	svcstr = calloc (1, 4*SZ_LINE);

	strcat (svcstr, "(");
        for (ip=svctype; *ip; ) {
	    more = 0;				/* re-initialize	*/
	    not  = 0;
	    bzero (val, SZ_FNAME);

	    for (op=val; *ip; ) {
		if (*ip == '-') {
		    not++, ip++;
		} else if (*ip == ',') {
		    more++, ip++;
		    break;
		} else
		    *op++ = *ip++;
	    }

	    /* Aliases for service type include:
	    **
	    **    Cone			-> catalog
	    **    SIAP			-> image
	    **    TABULARSKYSERVICE	-> table
	    **    SSAP			-> spectrum
	    **    TAP			-> query
	    */
	    if (not) strcat (svcstr, "(!");

	    if (strncasecmp (val,"catalog",3) == 0)
	        strcat (svcstr, "(Tag like '%catalog%')");
	    else if (strncasecmp (val,"image",5) == 0)
	        strcat (svcstr, "(Tag like '%image%')");
	    else if (strncasecmp (val,"spectr",6) == 0)
	        strcat (svcstr, "(Tag like '%spec%')");
	    else if (strncasecmp (val,"table",5) == 0)
	        strcat (svcstr, "(Identifier like '%Vizier%')");
	    else {
		bzero (buf, SZ_FNAME);
	        sprintf (buf, "(Tag like '%s%s%s')", like, val, like);
	        strcat (svcstr, buf);
	    }

	    if (not)  strcat (svcstr, ")");
	    if (more) strcat (svcstr, " OR ");
	}
	strcat (svcstr, ")");

	return (svcstr);
    }

    return ((char *)NULL);
}



/****************************************************************************
**  VOT_PARSEBANDPASS -- Convert a bandpass specification to an ADQL query.  We 
**  map things like 'x-ray' and 'xray' here to expand the query on behalf of 
**  the user. 
** 
**  Permitted values include: 
** 	"Radio", "Millimeter", "Infrared" (IR), "Optical", "Ultraviolet" (UV), 
** 	"X-Ray" (XRay), and "Gamma-Ray" (GR). 
*/
char *
vot_parseBandpass (char *bpass)
{
    char   *ip, *op, val[SZ_FNAME], buf[SZ_FNAME];
    char   *bpstr = NULL;
    int	   not = 0, more = 0;


    if (bpass && bpass[0]) {
        bpstr = calloc (1, 6*SZ_LINE);

	strcat (bpstr, "(");
        for (ip=bpass; *ip; ) {
	    more = 0;				/* re-initialize	*/
	    not  = 0;
	    bzero (val, SZ_FNAME);

	    for (op=val; *ip; ) {
		if (*ip == '-' && strncasecmp(ip,"-ray",4)) {
		    not++, ip++;
		} else if (*ip == ',') {
		    more++, ip++;
		    break;
		} else
		    *op++ = *ip++;
	    }

	    if (not) strcat (bpstr, "(!");

	    if (strncasecmp (val, "radio",3) == 0)
	        strcat (bpstr, "([coverage/waveband] like '%radio%')");
	    else if (strncasecmp (val,"millimeter",9) == 0)
	        strcat (bpstr, "([coverage/waveband] like '%millimeter%')");
	    else if (strncasecmp (val,"infrared",5) == 0 ||
	         strncasecmp (val,"ir",2) == 0)
	    	    strcat (bpstr, "([coverage/waveband] like '%infrared%')");
	    else if (strncasecmp (val,"optical",8) == 0)
	        strcat (bpstr, "([coverage/waveband] like '%optical%')");
	    else if (strncasecmp (val,"ultraviolet",5) == 0 ||
	         strncasecmp (val,"uv",2) == 0)
	    	    strcat (bpstr,"([coverage/waveband] like '%ultraviolet%')");
	    else if (strncasecmp (val,"x-ray",5) == 0 ||
	         strncasecmp (val,"xray",4) == 0)
	    	    strcat (bpstr, "([coverage/waveband] like '%x-ray%')");
	    else if (strncasecmp (val,"gamma-ray",9) == 0 ||
	         strncasecmp (val,"gammaray",8) == 0)
	    	    strcat (bpstr, "([coverage/waveband] like '%gamma-ray%')");
	    else {
		bzero (buf, SZ_FNAME);
	        sprintf (buf, "([coverage/waveband] like '%%%s%%')", val);
	        strcat (bpstr, buf);
	    }

	    if (not)  strcat (bpstr, ")");
	    if (more) strcat (bpstr, " OR ");
	}
	strcat (bpstr, ")");

	return (bpstr);
    }

    return ((char *)NULL);
}


/****************************************************************************
**  VOT_PARSESUBJECT -- Convert a subject specification to an ADQL query
**  string.  We don't impose a vocabulary on the allowed Subject string, but
**  do allow the string to be a comma-delimited list of subjects.
*/
char *
vot_parseSubject (char *subject)
{
    char   *ip, *op, val[SZ_FNAME], buf[SZ_FNAME];
    char   *substr;
    int	   more = 0;


    substr = calloc (1, 6*SZ_LINE);
    if (subject && subject[0]) {
	strcat (substr, "(");
        for (ip=subject; *ip; ) {
	    more = 0;				/* re-initialize	*/
	    bzero (val, SZ_FNAME);

	    for (op=val; *ip; ) {
		if (*ip == ',') {
		    more++, ip++;
		    break;
		} else
		    *op++ = *ip++;
	    }

	    bzero (buf, SZ_FNAME);
	    sprintf (buf, "([content/subject] like '%%%s%%')", val);
	    strcat (substr, buf);

	    if (more) strcat (substr, " OR ");
	}
	strcat (substr, ")");

	return (substr);
    }

    return ((char *)NULL);
}


/****************************************************************************
**  VOT_PARSECLEVEL -- Convert a ContentLevel specification to an ADQL query
**  string.
*/
char *
vot_parseCLevel (char *level)
{
    char   *ip, *op, *str, val[SZ_FNAME], buf[SZ_FNAME];
    int	   more = 0;

    str = calloc (1, 6*SZ_LINE);
    if (level && level[0]) {
	strcat (str, "(");
        for (ip=level; *ip; ) {
	    more = 0;				/* re-initialize	*/
	    memset (val, 0, SZ_FNAME);

	    for (op=val; *ip; ) {
		if (*ip == ',') {
		    more++, ip++;
		    break;
		} else
		    *op++ = *ip++;
	    }

	    memset (buf, 0, SZ_FNAME);
	    sprintf (buf, "([content/contentLevel] like '%%%s%%')", val);
	    strcat (str, buf);
	    if (more) 
		strcat (str, " OR ");
	}
	strcat (str, ")");
	return (str);
    }

    return ((char *)NULL);
}


/************************************************************************
**  Return the filename part of a URL.
*/
char *
vot_urlFname (char *url)
{
    char *ip;
    int  len = strlen (url);

    for (ip=&url[len-1]; ip >= url; ip--) {
	if (*ip == '&' || *ip == '?')
	    *ip = '\0';
	if (*ip == '/')
	    return (ip+1);
    }

    return (ip);
}


/******************************************************************************
** Print the resource attributes.
*/
void
vot_printAttrs (char *fname, Query query, char *ident)
{
    char      *result = (char *) NULL, *qstring = (char *) NULL;
    int       i=0, nrec, nattr;
#ifdef OLD_ATTRS
    char      *ip, *attrList = (char *) NULL;
    QRecord   rec = (QRecord) NULL;             /* Query record          */
    QResponse qr  = (QResponse) NULL;           /* Query response        */
#else
    int       vot = 0, nbytes = 0;
    FILE      *fd = stdout;
    char      *ip, *op;
#endif



    if (!query) {
        fprintf (stderr, "# Query failed, returning\n");
        return;
    }

#ifdef OLD_ATTRS
    if ((qr = voc_executeQuery (query))) {
        rec =  voc_getRecord (qr, 0);           /* get a row in the table */
        nrec = voc_getRecordCount (qr);
    }
    nattr = (rec ? voc_getAttrCount (rec) : 0);

    if (nattr > 0) {
        printf ("\n# --- Identifier:   %s\n#\n", ident);

	if (!meta)
            printf ("# returns %d records containing %d attributes each\n",
                nrec, nattr);

        attrList = voc_getAttrList (rec);
        printf ("#   Col  UCD\n");
        printf ("#   ---  ---\n#    %2d  ", (i=1));
        for (i++, ip=attrList; *ip; ip++) {
            if (isspace (*ip))
                printf ("\n#    %2d  ", i++);
            else
                putchar (*ip);
        }
        printf ("\n#\n");

        if (attrList) free ((void *) attrList);
    } else
        printf ("# --- No Attributes Found ---\n#\n");

#else

    if (query > 0) {
        if (! (qstring = voc_getQueryString (query, DAL_CONN, 0)))
	    return;
    } else
	return;

    while (isspace (*qstring))
	qstring++;

    if ((ip = strcasestr (qstring, "format=image"))) {
	op = ip;
	while (*ip && *ip != '&') 
	    ip++;
	if (*ip) {
	    for (ip++; *ip ;)
	        *op++ = *ip++;
	}
	*op = '\0';
    }

    if ((result = voc_getRawURL (qstring, &nbytes)) == NULL)
	return;

    /*  FIXME -- rawURL still returns garbage at the end.....
    result[nbytes] = '\0';
    */

    if ( nbytes > 0 && (vot = vot_openVOTABLE (result)) > 0 ) {

	char ucd[SZ_FNAME], name[SZ_FNAME], desc[SZ_LINE], id[SZ_FNAME], *at;
	extern char *vot_getFieldName(), *vot_getFieldDesc();
	extern char *vot_getFieldUCD(), *vot_getFieldID();;

        handle_t  res, tab, data, tdata, field, handle;


        res = vot_getRESOURCE (vot);      	/* get handles          */
        if ((tab = vot_getTABLE (res)) <= 0) {
            if ((data = vot_getDATA (tab)) <= 0) {
                if ((tdata = vot_getTABLEDATA (data))) {
                    nrec = vot_getNRows (tdata);
                    nattr = vot_getNCols (tdata);
                }
            }
        }


	if (!fname || strcmp (fname, "-") == 0) {
	    fd = stdout;
	} else {
	    if ((fd = fopen (fname, "w+")) == (FILE *) NULL)
	        return ;
	}

        fprintf (fd, "\n Service:  %s\n  NAttrs:  %d\n\n",
	    (ident ? ident : "N/A"), nattr);
        fprintf (fd, 
	    " Col  UCD              Name       Description\n");
        fprintf (fd, 
	    " ---  ---              ----       -----------------------\n");

        for (i=1, field=vot_getFIELD (tab); field; field=vot_getNext (field)) {

            fprintf (fd, " %3d  ", i++);

	    memset (ucd,  0, SZ_FNAME);  
	    memset (name, 0, SZ_FNAME);  
	    memset (id,   0, SZ_FNAME);  
	    memset (desc, 0, SZ_LINE);   

	    strcpy (ucd,  ((at=vot_getAttr (field, "ucd")) ? at : ""));
	    strcpy (name, ((at=vot_getAttr (field, "name")) ? at : ""));
	    strcpy (id,   ((at=vot_getAttr (field, "id")) ? at : ""));

	    if ((handle = vot_getDESCRIPTION (field)))
	    	strcpy (desc, vot_getValue (handle));
	    else
	    	strcpy (desc, " ");

/*
	    strcpy (desc, ((at=vot_getAttr (field, "description")) ? at : ""));

            fprintf (fd, "%-27.27s ", (ucd[0] ? ucd : 
					(name[0] ? name : 
					    (id[0] ? id : " "))) );
*/
            fprintf (fd, "%-16.16s %-10.10s ", ucd, name);

	    if (desc[0]) {
		ppMultiLine (desc, 34, 45, 1024);
	    } else {
	      if (name[0] && strcmp (name, id) == 0)
                fprintf (fd, "name=id='%s'", name);
	      else {
	        if (name[0])
                    fprintf (fd, "name='%s'%s", name, (id[0] ? ", ":" "));
	        if (id[0])
                    fprintf (fd, "id='%s'", id);
	      }
	    }
	    

            fprintf (fd, "\n");
	}
        vot_closeVOTABLE (vot);

	if (fd != stdout)
	    fclose (fd);

    } else if (!quiet)
	fprintf (stderr, "   %-50s    Error\n", ident);

	
/*  FIXME -- causes a segfault
    if (result) 
	voc_freePointer ((char *) result);
*/
#endif
}


/**
 *  VOT_SETARG -- Set a value in an argv vector, update the count.
 */
void   
vot_setArg (char **argv, int *argc, char *value)
{
    int  i = *argc;

    argv[i] = strdup (value);
    *argc += 1;
}


/************************************************************************
**  PRINTVOTABLEHDR -- Print the prolog to the VOTable output.
*/
void
vot_printRegVOTableHdr (FILE *fd)
{
fprintf (fd, "\
<?xml version=\"1.0\" encoding=\"utf-8\"?>\
<VOTABLE ID=\"ID\" xmlns=\"http://www.ivoa.net/xml/VOTable/v1.1\">\
<DESCRIPTION>REGISTRY SEARCH RESULTS</DESCRIPTION>\
<RESOURCE>\
<INFO name=\"QUERY_STATUS\" value=\"OK\"></INFO>\
<TABLE>");

if (verbose == 0) {

fprintf (fd, "\
<FIELD datatype=\"char\" name=\"title\" arraysize=\"*\"/>\
<FIELD datatype=\"char\" name=\"shortName\" arraysize=\"*\"/>\
<FIELD datatype=\"char\" name=\"identifier\" arraysize=\"*\"/>\
<FIELD datatype=\"char\" name=\"accessURL\" arraysize=\"*\"/>\
<FIELD datatype=\"char\" name=\"referenceURL\" arraysize=\"*\"/>\
<FIELD datatype=\"char\" name=\"capabilityClass\" arraysize=\"*\"/>\
<FIELD datatype=\"char\" name=\"contentLevel\" arraysize=\"*\"/>\
<FIELD datatype=\"char\" name=\"type\" arraysize=\"*\"/>\
<FIELD datatype=\"char\" name=\"waveband\" arraysize=\"*\"/>\
<FIELD datatype=\"char\" name=\"publisher\" arraysize=\"*\"/>\
<FIELD datatype=\"char\" name=\"subject\" arraysize=\"*\"/>\
<FIELD datatype=\"char\" name=\"version\" arraysize=\"*\"/>");

} else {

fprintf (fd, "\
<FIELD ID=\"tags\" datatype=\"char\" name=\"categories\" arraysize=\"*\"/>\
<FIELD ID=\"shortName\" datatype=\"char\" name=\"shortName\" arraysize=\"*\"/>\
<FIELD ID=\"title\" datatype=\"char\" name=\"title\" arraysize=\"*\"/>\
<FIELD ID=\"description\" datatype=\"char\" name=\"description\" arraysize=\"*\"/>\
<FIELD ID=\"publisher\" datatype=\"char\" name=\"publisher\" arraysize=\"*\"/>\
<FIELD ID=\"waveband\" datatype=\"char\" name=\"waveband\" arraysize=\"*\"/>\
<FIELD ID=\"identifier\" datatype=\"char\" name=\"identifier\" ucd=\"ID_MAIN\" arraysize=\"*\"/>\
<FIELD ID=\"updated\" datatype=\"char\" name=\"descriptionUpdated\" arraysize=\"*\"/>\
<FIELD ID=\"subject\" datatype=\"char\" name=\"subject\" arraysize=\"*\"/>\
<FIELD ID=\"type\" datatype=\"char\" name=\"type\" arraysize=\"*\"/>\
<FIELD ID=\"contentLevel\" datatype=\"char\" name=\"contentLevel\" arraysize=\"*\"/>\
<FIELD ID=\"regionOfRegard\" unit=\"arcsec\" datatype=\"int\" name=\"typicalRegionSize\"/>\
<FIELD ID=\"version\" datatype=\"char\" name=\"version\" arraysize=\"*\"/>\
<FIELD ID=\"capabilityClass\" datatype=\"char\" name=\"capabilityClass\" arraysize=\"*\"/>\
<FIELD ID=\"capabilityID\" datatype=\"char\" name=\"capabilityStandardID\" arraysize=\"*\"/>\
<FIELD ID=\"capabilityValidationLevel\" datatype=\"char\" name=\"capabilityValidationLevel\" arraysize=\"*\"/>\
<FIELD ID=\"interfaceClass\" datatype=\"char\" name=\"interfaceClass\" arraysize=\"*\"/>\
<FIELD ID=\"interfaceVersion\" datatype=\"char\" name=\"interfaceVersion\" arraysize=\"*\"/>\
<FIELD ID=\"interfaceRole\" datatype=\"char\" name=\"interfaceRole\" arraysize=\"*\"/>\
<FIELD ID=\"accessURL\" datatype=\"char\" name=\"accessURL\" arraysize=\"*\"/>\
<FIELD ID=\"supportedInputParam\" datatype=\"char\" name=\"supportedInputParam\" arraysize=\"*\"/>\
<FIELD ID=\"maxRadius\" datatype=\"int\" name=\"maxSearchRadius\"/>\
<FIELD ID=\"maxRecords\" datatype=\"int\" name=\"maxRecords\"/>\
<FIELD ID=\"publisherID\" datatype=\"char\" name=\"publisherIdentifier\" arraysize=\"*\"/>\
<FIELD ID=\"referenceURL\" datatype=\"char\" name=\"referenceURL\" arraysize=\"*\"/>");
}
fprintf (fd, "<DATA><TABLEDATA>");

fflush (fd);
}


/************************************************************************
**  PRINTVOTABLEREC -- Print an individual record in the search results
**  as a VOTable row.
*/
void
vot_printRegVOTableRec (FILE *fd, RegResult resource, int recnum)
{
    register int i;
    char *attr_val, *fmt, *attr;

    static char *resAttr[] = {
        "Title",          "ShortName",      "Identifier",
        "AccessURL",      "ReferenceURL",   "CapabilityClass",
        "ContentLevel",   "Type",           "Waveband",
        "Creator",        "Subject",        "Version",
        NULL
    };

    static char *resAttrVerbose[] = {
	"tags",			"shortName",		"title",
	"description",		"publisher",		"waveband",
	"identifier",		"updated",		"subject",
	"type",			"contentLevel",		"regionOfRegard",
	"version",		"capabilityClass",	"capabilityID",
	"capabilityValidationLevel",			"interfaceClass",	
	"interfaceVersion", 	"interfaceRole",	"accessURL",	
	"supportedInputParam", 	"maxRadius",		"maxRecords",	
	"publisherID", 		"referenceURL",	
	NULL
    };



    fprintf (fd, "<TR>");
    for (i=0; 1; i++) {
        attr = (verbose ? resAttrVerbose[i] : resAttr[i]);
	if (attr == NULL)
	    break;

        attr_val = xmlEncode (voc_resGetStr (resource, attr, recnum));

	/* Escape any URLs to take care of special chars.
	*/
	fmt = (*attr_val && strstr(attr,"URL") ? 
	    "<TD><![CDATA[%s]]></TD>" : "<TD>%s</TD>");

	if (!(*attr_val)  &&
	    (strcmp (attr, "regionOfRegard") == 0 ||
	     strcmp (attr, "maxRadius") == 0  ||
	     strcmp (attr, "maxRecords") == 0)) {
                fprintf (fd, fmt, "0");

	} else if (attr_val && *attr_val) {
            fprintf (fd, fmt, attr_val);

	} else
            fprintf (fd, "<TD/>");

	if (attr_val)
	    voc_freePointer ((char *) attr_val);
    }
    fprintf (fd, "</TR>");
    fflush (fd);
}



/************************************************************************
**  PRINTVOTABLETAIL -- Print the epilog to the VOTable output.
*/
void
vot_printRegVOTableTail (FILE *fd)
{
fprintf (fd, "\
</TABLEDATA>\
</DATA>\
</TABLE>\
</RESOURCE>\
</VOTABLE>\n");
}



/****************************************************************************
** Lexical utility procedures.
*/

char *
xmlEncode (char *in)
{
    char *ip, *op, *out;

    if (in == (char *) NULL) 
	return (calloc(1,2));		/* caller will free this pointer */

    out = calloc (1, strlen(in) * 2);
    ip  = in;
    op  = out;

    while (*ip) {
	if (*ip == '<') {
	    strncpy (op, "&gt;", 4);
	    op += 4;
	} else if (*ip == '>') {
	    strncpy (op, "&gt;", 4);
	    op += 4;
	} else if (*ip == '&') {
	    strncpy (op, "&amp;", 5);
	    op += 5;
	} else if (*ip == '\'') {
	    strncpy (op, "&quot;", 6);
	    op += 6;
	} else if (*ip == '\"') {
	    strncpy (op, "&dquot;", 7);
	    op += 7;
	} else 
	    *op++ = *ip;

	ip++;
    }


    if (in)
	voc_freePointer ((char *) in);

    return (out);
}



/************************************************************************
**  VOT_GETLINE -- Get the next line in a file/stdin.  We use this procedure
**  to allow for parsing input that may be piped in where newlines are
**  expressed as the string "\n".
*/
char *
vot_getline (FILE *fd)
{                               
    int   i, ch, peek;
    static char  cmdline[SZ_LINE];
                        
                    
    bzero (cmdline, SZ_LINE);
                
    for (i=ch=0;  ch != EOF; i++) {
        cmdline[i] = ch = (char) fgetc (fd);
        if (i == 0 && ch == EOF)
            return (NULL);                       
        else if (i > 0 && (ch == EOF || ch == '\n')) {
            cmdline[i] = '\0';
            break;  
        } else if (ch == (int) '\\') {
            if ((peek = fgetc (fd)) == (int)'n') {          
                cmdline[i] = '\0';
                break;
            } else  
                ungetc (peek, fd);
        }       
    }

    return (cmdline);
}


/****************************************************************************
**  NORMALIZECOORD -- Normalize a coordinate string, i.e. strip whitespace
**  from the ends and replace internal whitespace with a ':' in the case of
**  sexagesimal values.
*/
char *
vot_normalizeCoord (char *coord)
{
    static char *ip, *op, norm[SZ_LINE];


    bzero (norm, SZ_LINE);

    /* Remove trailing whitespace       */
    for (ip=&coord[strlen(coord)-1]; isspace(*ip) && ip > coord; )
        *ip = '\0';

    /* Skip leading whitespace          */
    for (ip=coord; *ip && isspace(*ip); ip++)
        ;

    for (op=norm; *ip; ) {
        if (isspace (*ip)) {
            *op++ = ':';
            while (*ip && isspace(*ip)) /* collapse multiple space      */
                ip++;
        } else
            *op++ = *ip++;
    }

    return (norm);
}


/****************************************************************************
** Normalize the names, i.e. replace anything other than [.+-] with '-'.
*/
char *
vot_normalize (char *str)
{
    char *ip, *op;
    static char name[SZ_FNAME];

    if (str == (char *)NULL)
        return ("");

    bzero (name, SZ_FNAME);
    for (ip=str, op=name; *ip; ) {
        if (strchr (".+-", (int)*ip))
            *op++ = *ip++;
        else if (!isalnum ((int)*ip))
            *op++ = '-', ip++;
        else
            *op++ = *ip++;
    }

    return (name);
}



/* UTILITY FUNCTIONS. 
*/

/****************************************************************************
**  ISVOTABLE --  Test a file or string to see if it's a VOTable.
*/
int
isVOTable (char *fname)
{
    int  i;
    FILE *fd = (FILE *) NULL;
    char line[SZ_LINE];

    if (fname[0] == '-') {
        (void) fgets (line, SZ_LINE, stdin);
        if (line[0] == '<')
            return (1);

    } else if (access (fname, R_OK) == 0) {

        /* Process the file contents.
        */
        if ((fd = fopen (fname, "r")) == (FILE *) NULL) {
            fprintf (stderr, "ERROR: Cannot open file '%s'\n", fname);
            return (0);
        }

        for (i=0; i < 10 && fgets (line, SZ_LINE, fd); i++) {
            if (strcasestr (line, "VOTABLE")) {
                fclose (fd);
                return (1);
            }
        }

        if (fd)
            fclose (fd);
    }

    return (0);
}


int
isSexagesimal (char *str)
{
    register int  i;

    /* Allow only numbers, colons, decimal point, whitespace, and sign.  */
    for (i=(strlen(str)-1); i >= 0; i--)
	if (!isdigit(str[i]) && strchr("/:+- .\t,", (int)str[i])==(char *)NULL)
	    return (0);

    return (1);
}

int
isDecimal (char *str)
{
    register int  i;

    /* Allow only numbers, decimal point, whitespace, and sign.  */
    for (i=(strlen(str)-1); i >= 0; i--)
	if (!isdigit(str[i]) && strchr("/+- .\t,", (int)str[i])==(char *)NULL)
	    return (0);

    return (1);
}

float
sexa (char *s)
{
    int     n, sign;
    int     hr, minutes;
    float   sec, val;
    extern double atof();

    while (isspace (*s)) 		/* skip leading whitespace	*/
	s++;
    sign = (*s == '-') ? (s++, -1) : 1; /* get the sign			*/

    minutes = 0;
    sec = 0.;
    n = sscanf (s, "%d:%d:%f", &hr, &minutes, &sec);
    if (n < 1 || minutes < 0 || sec < 0)
        val = -999.0;
    else
        /*  Beware: Evaluation here can produce roundoff errors! 
	*/
        val = sign * (hr + ((float)minutes)/60. + sec/3600.);

    return (val);
}


char *
toSexa (double pos)
{
    static char str[SZ_LINE];
    int   d, m;
    float s, frac;
    char sign = (pos < 0.0 ? '-' : 0);


    pos = (pos < 0.0 ? -pos : pos);

    d = (int) pos;
    frac = (pos - d);
    m = frac * 60.0;
    s = ((frac * 60.0) - m) * 60.0;

    if (sign)
        sprintf (str, "%c%02d:%02d:%04.1f", sign, d, m, s);
    else
        sprintf (str, "%02d:%02d:%04.1f", d, m, s);

    return (str);
}


char *
toSexaTime (int nsec)
{
    char  tstr[SZ_LINE];
    int   m, s;

    m = nsec / 60;
    s = nsec % 60;

    sprintf (tstr, "%02d:%02d", m, s);

    return (strdup (tstr));		/* note potential memory leak!	*/
}


char *
vot_mktemp (char *root)
{
    char *tmp;
    static char tmpfile[SZ_LINE];
    char *tmpdir = "/tmp";


    /* Get a temporary file name based on the pid.
    */
    bzero (tmpfile, SZ_LINE);
    if ((tmp = getenv ("TMP")) != NULL)
        tmpdir = tmp;
    sprintf (tmpfile, "%s/%s%d", tmpdir, root, (int)(getpid()+time((time_t)0)));

    return (tmpfile);
}


/* Copy the standard input to a temp file we can parse more easily.
*/
char *
vot_copyStdin ()
{
    static char *line, tmpfile[SZ_FNAME];
    FILE *fd;
    extern char *vot_getline();


    /* Open a temp file and copy the stdin to it.
    */
    strcpy (tmpfile, vot_mktemp ("vodo"));
    if ((fd = fopen (tmpfile, "w+")) == (FILE *) NULL) {
        fprintf (stderr, "Error opening tmp file '%s'\n", tmpfile);
        return ((char *)NULL);
    }

    while ( (line = vot_getline (stdin)) )
        fprintf (fd, "%s\n", line);
    fclose (fd);

    return (tmpfile);
}


/* Skip header lines of a table.
*/
void
vot_skipHdr (FILE *fd)
{
    register int i;
    char line[SZ_LINE];


    if (fd && table_hskip) {                    /* Skip header lines    */
        rewind (fd);
        for (i=0; i < table_hskip; i++) {
            if (fgets (line, SZ_LINE, fd) == NULL) {
                break;
            }
        }
    }
}



/****************************************************************************
**  GETTABLECOL -- Get the requested column value from the table line.
**  Column numbers are assumed to be one-indexed.
*/
char *
vot_getTableCol (char *line, int col, int span)
{
    int    i, nsp = span;
    char   sep[6], *ip, *op, *del = (char *)NULL;
    static char value[SZ_LINE];


    bzero (value, SZ_LINE);
    bzero (sep, 6);

    /*  If we're doing exact columns, copy whatever is in those columns
    **  to the output file.  Otherwise, parse the line based on delimiters.
    */
    if (ecols) {
	strncpy (value, (char *)&line[col-1], span);
	for (i=span-1; i && isspace(value[i]); i--) /* trailing space 	*/
	    value[i] = '\0';
	for (i=0; isspace(value[i]); i++)	/* leading space 	*/
	    ;
	return (&value[i]);
    }

    if ((del = strpbrk(line, " \t,|;")))	/* get delimiter	*/
        sep[0] = del[0];
    else
        strcpy (sep, delim);
    
    op = value;
    for (i=1, ip=line; *ip; ip++) {
	if (strchr (sep, (int)*ip) || *ip == '\n') {
	    if (sep[0] == ' ') {
	        while ( *ip == sep[0] ) 	/* skip multiple spaces	*/
		    ip++;
	        if (*ip != '\n')
		    ip--;			/* reposition for next	*/
	    }
	    if (i++ == col) {
		if (--nsp == 0) {
		    return (value);
		} else {
		    *op++ = ' ';		/* add a space for span */
		    i--;
		}
	    } else 
		bzero ((op=value), SZ_LINE);
	} else
	    *op++ = *ip;
    }

    return ( (i == col) ? value : (char *)NULL );
}


/**
 *  VOT_ISNUMERICFIELD -- Determine if a <FIELD> is a numeric datatype.
 */
int
vot_isNumericField (handle_t field)
{
    char *dtype = vot_getAttr (field, "datatype");
    char *asize = vot_getAttr (field, "arraysize");
                

    if (asize && asize[0]) {
        return (0);

    } else {
        if ((strncasecmp (dtype, "floatComplex", 12) == 0) ||
            (strncasecmp (dtype, "doubleComplex", 13) == 0))
                return (0);

        if ((strncasecmp (dtype, "short", 5) == 0) ||
            (strncasecmp (dtype, "int", 3) == 0) ||
            (strncasecmp (dtype, "long", 4) == 0) ||
            (strncasecmp (dtype, "float", 5) == 0) ||
            (strncasecmp (dtype, "double", 6) == 0))
                return (1);
    }
    return (0);
}


/**
 *  VOT_FILETYPE -- Determine what type of file we have.
 */
int
vot_fileType (char *fname)
{
    FILE  *fd = (FILE *) NULL;
    char  buf[1024];
    int   nread, ftype = -1;

    if ((fd = fopen (fname, "r")) != NULL) {
	memset (buf, 0, 1024);
	nread = fread (buf, 1023, 1, fd);
	if (strncasecmp ("SIMPLE", buf, 6) == 0) {
	    /* FIXME -- Need to add spectrum serialization. */
	    ftype = VOT_FITS;
	} else if (strcasestr (buf, "VOTABLE")) {
	    /* FIXME -- Need to add spectrum serialization. */
	    ftype = VOT_VOTABLE;
	}
	fclose (fd);
    } else 
	fprintf (stderr, "fileType: cannot open '%s'\n", fname);

    return (ftype);
}


/**
 *  VOT_SUM32 -- Internet checksum, 32 bit unsigned integer version.
 */
int
vot_sum32 (char *str)
{
    register int i;
    unsigned int *iarray;
    unsigned long lsum = 0;
    int      sum = 0;
    int      len, carry=0, newcarry=0;

    iarray = (unsigned int *) str;
    len = strlen (str) / 4;

    for (i=0; i<len; i++) {
        if (iarray[i] > ~ lsum)
            carry++;
        lsum += iarray[i];
    }

    while (carry) {
        if (carry > ~ lsum)
            newcarry++;
        lsum += carry;
        carry = newcarry;
        newcarry = 0;
    }

    return (abs(sum = lsum));
}


/**
 *  STRDIC -- Search a dictionary string for a match with an input string.
 *  The input string may be an abbreviation of a dictionary entry, however,
 *  it is an error if the abbreviation is not unique.  The entries in the
 *  dictionary string are separated by a delimiter character which is the
 *  first character of the dictionary string.  The full name of the matched 
 *  dictionary entry found is returned in out_str; the function value is 
 *  the word index of the dictionary entry.  The output string may be the 
 *  same as the input string.
 */

#include <ctype.h>

int strdic (
  char	*in_str,		/* Input string, always lower case	*/
  char	*out_str,		/* Output string as found in dictionary */
  int	 maxchars,		/* Maximum length of output string	*/
  char	*dict 			/* Dictionary string			*/
)
{
    char  ch, fch;
    int	  start, len, ip, i, match, entry;


    if (dict == NULL || dict[0] == '\0')
	return (0);

    for (i=0; isspace(in_str[i]); i++)
	;

    start = i;
    match = -1;
    ip    = 1;
    len   = strlen (&in_str[start]);
    fch   = in_str[start];


    /*  Search the dictionary string.  If the input string matches a
     *  dictionary entry it is either an exact match (len = dictionary
     *  entry length) or a legal abbreviation.  If an abbreviation
     *  matches two entries it is ambiguous and an error.
     */
    for (entry=0;  dict[ip] != '\0';  entry=entry+1) {
	if (dict[ip] == fch) {
	    if (strncmp (&dict[ip], &in_str[start], len) == 0) {
		for (i=0;  i < maxchars;  i++) {
		    ch = dict[ip+i-1];
		    if ((ch == dict[0]) || (ch == '\0'))
			break;
		    out_str[i] = ch;
		}
		out_str[i] = '\0';

		if ((dict[ip+len] == dict[0]) || (dict[ip+len] == '\0'))
		    return (entry);		/* exact match		*/
		else {
		    /* If we already have a match and the new match is not
		     * exact, then the abbreviation is ambiguous.
		     */
		    if (match != 0)
		        return (0);
		    else
		        match = entry;
		}
	    }
	}

	do {
	    ip = ip + 1;
	} while (dict[ip-1] != dict[0] && dict[ip] != '\0');
    }

    if (match <= 0)
	strcpy (out_str, in_str);
    return (match);
}


/*****************************************************************************
******  		URL String Encode / Decode			******
*****************************************************************************/

/* Converts a hex character to its integer value */
static char from_hex (char ch)
{
    return (isdigit(ch) ? ch - '0' : tolower(ch) - 'a' + 10);
}

/* Converts an integer value to its hex character*/
static char to_hex (char code)
{
    static char hex[] = "0123456789abcdef";
    return (hex[code & 15]);
}


/**
 *  VO_URLENCODE -- Returns a url-encoded version of str.  Call must free() 
 *  the pointer that is returned.
 */
char *
vo_urlEncode (char *str)
{
    char *pstr = str, *buf = malloc(strlen(str) * 3 + 1), *pbuf = buf;

    while (*pstr) {
        if (isalnum(*pstr) || 
	    *pstr == '-' || 
	    *pstr == '_' || 
	    *pstr == '.' || 
	    *pstr == '~') 
                *pbuf++ = *pstr;
        else if (*pstr == ' ') 
            *pbuf++ = '+';
        else {
            *pbuf++ = '%';
	    *pbuf++ = to_hex (*pstr >> 4);
	    *pbuf++ = to_hex (*pstr & 15);
	}
        pstr++;
    }
    *pbuf = '\0';
    return buf;
}


/**
 *   VO_URLDECODE -- Returns a url-decoded version of str.  Call must free() 
 *  the pointer that is returned.
 */
char *
vo_urlDecode (char *str)
{
    char *pstr = str, *buf = malloc(strlen(str) + 1), *pbuf = buf;

    while (*pstr) {
        if (*pstr == '%') {
            if (pstr[1] && pstr[2]) {
                *pbuf++ = from_hex(pstr[1]) << 4 | from_hex(pstr[2]);
                pstr += 2;
            }
        } else if (*pstr == '+') { 
            *pbuf++ = ' ';
        } else {
            *pbuf++ = *pstr;
        }
        pstr++;
    }
    *pbuf = '\0';

    return buf;
}


/**
 *  VOT_TOURL -- Convert a filename to a URL.
 */
char *
vot_toURL (char *arg)
{
    /*  If we have an existing protocol simply return the argument.
     */
    if ((strncmp (arg, "http:", 5) == 0) ||
        (strncmp (arg, "file:", 5) == 0) ||
        (strncmp (arg, "ftp:", 4) == 0))
            return (arg);

    if (access (arg, F_OK) == 0) {
        char  cwd[SZ_LINE];
        static char buf[SZ_LINE];

        memset (cwd, 0, SZ_LINE);
        getcwd (cwd, (unsigned long) SZ_LINE);

        memset (buf, 0, SZ_LINE);
        if (arg[0] == '/')
            sprintf (buf, "file://%s", arg);
        else
            sprintf (buf, "file://%s/%s", cwd, arg);

        return (buf);
    }

    return (arg);
}


/**
 *  VOT_ISVALIDFORMAT -- Check whether we have a supported format request.
 */
int
vot_isValidFormat (char *fmt)
{
    char format[SZ_LINE];
    extern int strdic ();

    return ( (strdic (fmt, format, SZ_FORMAT, FORMATS) >= 0) );
}


/**
 *  VOT_ATOI -- System atoi() with lexical argument checking.
 */
int
vot_atoi (char *val)
{
    char *ip;

    for (ip = val; *ip; ip++) {
	if (isalpha ((int) *ip)) {
	    fprintf (stderr, "Warning: value '%s' is not an integer\n", val);
	    break;
	}
    }
    return (atoi (val));
}


/**
 *  VOT_ATOL -- System atol() with lexical argument checking.
 */
long
vot_atol (char *val)
{
    char *ip;

    for (ip = val; *ip; ip++) {
	if (isalpha ((int) *ip) && *ip != '-' && *ip != '+') {
	    fprintf (stderr, "Warning: value '%s' is not an integer\n", val);
	    break;
	}
    }
    return (atol (val));
}


/**
 *  VOT_ATOF -- System atoi() with lexical argument checking.
 */
double
vot_atof (char *val)
{
    char *ip, c;

    for (ip = val; *ip; ip++) {
	c = *ip;
	if (! (tolower(c) == 'e' || tolower(c) == 'd' || isspace(c) ||
	      (c == '-' || c == '+' || c == '.') || isdigit(c))) {
	    	fprintf (stderr, 
		    "Warning: value '%s' is not a floating point value\n", val);
	    break;
	}
    }
    return (atof (val));
}
