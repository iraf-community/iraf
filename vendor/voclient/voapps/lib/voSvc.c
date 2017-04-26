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


#define SVC_DEBUG	0


extern int   nobjects, nservices, inventory;
extern int   verbose, quiet, debug, errno, force_svc, meta;
extern int   all_data, use_name, all_named, url_proc, svc_list;
extern int   force_read, table_hskip, table_nlines, table_sample;
#ifdef REG10_KLUDGE
extern int   reg10;
#endif
extern char *typestr, *bpass, *delim, *cols, *ecols, *output;

extern Service *svcList, *svcTail;

int svcIndex	= 0;


int    vot_parseServiceList (char *list, int dalOnly);
int    vot_printServiceList (FILE *fd);
int    vot_printServiceVOTable (FILE *fd);
int    vot_isSupportedSvc (char *type);

void   vot_addToSvcList (char *name, char *ident,char *url, char *type,
				char *title);
void   vot_freeServiceList (void);
void   vot_resetServiceCounters (void);
void   vot_readSvcFile (char *fname, int dalOnly);

static int vot_serviceResolver (char *idlist, int dalOnly);
static int isResourceVOTable (char *fname);
static int vot_loadResourceVOTable (char *fname);

static void  vot_regCacheResults (char *fname, char *results, int nres);

static char *vot_regGetCacheResults (char *fname, int *nres);
static char *vot_regIsCached (char *id, char *type, char *bpass);
static char *vot_regCacheName (char *id, char *type, char *bpass);

extern int   vot_callConeSvc (svcParams *pars);
extern int   vot_callSiapSvc (svcParams *pars);
extern int   vot_callSsapSvc (svcParams *pars);
extern void  vot_addToAclist (char *url, char *fname);
extern char *vot_urlFname (char *url);
extern char *vot_normalizeCoord (char *coord);
extern char *vot_normalize (char *str);



/* Utility procedures.
*/
extern char  *strcasestr ();




/****************************************************************************
**  Parse a string containing service names.  The string may be a single
**  name, a comma-delimited list, or the name of a file containing the same.
*/
int
vot_parseServiceList (char *list, int dalOnly) 
{
    FILE  *fd;
    /*char  name[SZ_FNAME];*/
    char  *name;

    extern char *vot_getline (FILE *fd);


    if (access (list, R_OK) == 0) {

	if (isResourceVOTable (list)) {
	    nservices += vot_loadResourceVOTable (list);

	} else {

	    /* Process the file contents. 
	    */
	    if ((fd = fopen (list, "r")) == (FILE *) NULL) {
	        fprintf (stderr, "ERROR: Cannot open file '%s'\n", list);
	        return (1);
	    }

	    while ((name = vot_getline (fd)))
	        vot_serviceResolver (name, dalOnly);

	    fclose (fd);
	}

    } else 
	vot_serviceResolver (list, dalOnly);

    if (debug) 
	 vot_printServiceList (stderr);
    if (svc_list) {
	FILE *fd;
	char  listfile[SZ_FNAME];

	memset (listfile, 0, SZ_FNAME);
	sprintf (listfile, "%s.services", output);
	if ((fd = fopen (listfile, "w+")) != (FILE *) NULL) {
	    vot_printServiceList (fd);
	    fclose (fd);
	}
    }

    return (0);
}


/****************************************************************************
**  LOADRESOURCEVOTABLE --  Read a Resource VOTable and load the Service list
**  with its contents.
*/
static int
vot_loadResourceVOTable (char *fname)
{
    int  vot = 0, i;
    int  nr = 0, nc = 0, nresults = 0;
    int  c_url, c_name, c_id, c_type, c_title;
    char url[SZ_LINE], name[SZ_FNAME], id[SZ_FNAME];
    char type[SZ_FNAME], title[SZ_LINE];


    if ((vot = vot_openVOTABLE (fname)) ) {

	int   res, tab, data, tdata;

        res   = vot_getRESOURCE (vot);      /* get handles          */
        if ((tab   = vot_getTABLE (res)) <= 0)
            return (0);
        if ((data  = vot_getDATA (tab)) <= 0)
            return (0);
        if ((tdata = vot_getTABLEDATA (data)) <= 0)
            return (0);
        nr    = vot_getNRows (tdata);
        nc    = vot_getNCols (tdata);

	c_name  = vot_colByName (tab, "shortName", "");
	c_url   = vot_colByName (tab, "serviceURL", "accessURL");
	c_id    = vot_colByName (tab, "identifier", "");
	c_type  = vot_colByName (tab, "type", "capabilityclass");
	c_title = vot_colByName (tab, "title", "");

	if (SVC_DEBUG)
	    fprintf (stderr, "name:%d  url:%d  id:%d  type:%d  title:%d\n",
    		c_name, c_url, c_id, c_type, c_title);
	    
	for (i=0; i < nr; i++) {
	    bzero (name,  SZ_FNAME); 
	    strcpy (name,  vot_getTableCell (tdata, i, c_name));

	    bzero (url,   SZ_LINE); 
	    strcpy (url,   vot_getTableCell (tdata, i, c_url));

	    bzero (id,    SZ_FNAME); 
	    strcpy (id,    vot_getTableCell (tdata, i, c_id));

	    bzero (type,  SZ_FNAME); 
	    strcpy (type,  vot_getTableCell (tdata, i, c_type));

	    if (strcasestr ("SimpleImageAccess", type))
	        strcpy (type, "SIAP");
	    if (strcasestr ("ConeSearch", type))
	        strcpy (type, "CONE");
	    if (strcasestr ("skyservice", type))
	        strcpy (type, "TABULARSKYSERVICE");

	    bzero (title, SZ_LINE); 
	    strcpy (title, vot_getTableCell (tdata, i, c_title));

            /* Skip services we don't yet support.
            */
            if (! vot_isSupportedSvc (type) && !meta && !inventory) {
		if (!quiet && verbose > 1)
                  fprintf (stderr,
		    "# Unsupported type %s for '%s', skipping...\n", type,name);
            } else {
            	vot_addToSvcList (name, id, url, type, title);
		nresults++;
	    }
	}
    }

    if (vot)
	vot_closeVOTABLE (vot);
    		
    return (nresults);
}


/****************************************************************************
**  ISRESOURCEVOTABLE --  Test a file or string to see if it's a Resource 
**  VOTable by looking for a <FIELD> element with a 'ShortName' attribute.
*/
static int
isResourceVOTable (char *fname)
{
    int   vot = 0, res, tab, field;
    char  name[SZ_FNAME], *attr = NULL;
    extern  int isVOTable (char *fname);


    /* First, make sure that it's at least a VOTable.
    */
    if (isVOTable (fname) == 0)
	return (0);


    /* Open it, looking for something we'd only expect from a
    ** Resource VOTable like the 'ShortName' field.
    */
    if ((vot = vot_openVOTABLE (fname)) ) {
	res   = vot_getRESOURCE (vot);
        if ((tab   = vot_getTABLE (res)) <= 0)
            return (0);

        for (field=vot_getFIELD(tab); field; field=vot_getNext(field)) {
	    bzero (name, SZ_FNAME);
	    if ((attr = vot_getAttr (field, "name"))) 
	        strcpy (name, attr);

	    if (name[0] && strcasecmp (name, "ShortName") == 0) {
    		vot_closeVOTABLE (vot);
		return (1);
	    }
	}
    }

    if (vot > 0)
	vot_closeVOTABLE (vot);

    return (0);
}



/****************************************************************************
**  Free the service list and reset the counter.
*/
void
vot_freeServiceList ()
{
    Service *cur, *next;

    for (cur=svcList; cur; cur=next) {
	next = cur->next;
	if (cur)
	    free ((void *) cur);
    }
    nservices = 0;
    svcList = svcTail = (Service *) NULL;
}


/****************************************************************************
**  Reset the service counters.
*/
void
vot_resetServiceCounters ()
{
    Service *svc;

    for (svc=svcList; svc; svc=svc->next)
	svc->count = svc->nfailed = svc->nnodata = 0;
}


/****************************************************************************
**  Resolve a service name/list to the proper service URL and store the
**  result in the 'svcList' global which we assume is declared in the caller.
*/
static int
vot_serviceResolver (char *idlist, int dalOnly)
{
    char    *ip, *rp, *np, *id;
    char    sname[SZ_LINE], ident[SZ_LINE], title[SZ_LINE];
    char    name[SZ_LINE], url[SZ_URL], type[SZ_LINE], *result;
    int     i, len, use_any = 0, nres=0, vot_regResolver ();
		
    extern  int svcNumber;


    /* Resolve the (list) of service names and add them to the
    ** service list to be processed.
    */
    ip = idlist; 

    while (*ip) {

	/* Break up the input list into a single ID to resolve.
	*/
	id = ip;
	while (*ip) {
	    if (*ip == ',' || *ip == '\n') {
		*ip++ = '\0';
		break;
	    } else
		ip++;
	}

        if (access (id, R_OK) == 0) {			/* file 	*/
	    vot_parseServiceList (id, dalOnly);
	    continue;
	} else {					/* service name	*/
	   
	    if (url_proc && strncmp (id, "http", 4) == 0) {
		/* A URL.  This may be a user-defined service URL if the
		** the -s flag was given, otherwise we assume it's an 
		** access reference we want to download.
		*/
		if (force_svc) {
		    sprintf (result, "%s\tUserSvc%d\tivo://user\t%s\n",
			id, nres++, typestr);

		} else {
		    /* Argument was a URL but we're not asked to treat it
		    ** as a service URL, so just add it to the access list
		    ** and move on.  When given on the command-line this is
		    ** handled by the argument parsing, here we do it mostly
		    ** to process URLs given in a file.
		    */
		    vot_addToAclist (id, NULL);
		    url_proc++;
		    break;
		}

	    } else {
		/* Try to resolve the id to a service URL.  This may be
		** a substring of a ShortName or Identifier field and in
		** some cases may resolve to more than one resource.  For
		** 'all_data' mode we assume the id is a ShortName that may
		** resolve to multiple tables having unique IVORNs so we
		** require an exact match of the name and add expand the 
		** service list with each resolved identifier.  
		*/

	        char *c_name = vot_regIsCached (id, typestr, bpass);	

		if (c_name) {
		    result = vot_regGetCacheResults (c_name, &nres);
		    nservices += nres;
		    if (strcasecmp ("any", id) == 0) {
		 	if (inventory)
			    nservices = -1;
			else if (all_data) 
			    use_any = 1;
		    }

		} else {
		  char *fields = 
		    "AccessURL,ShortName,Identifier,CapabilityStandardID,Title";

		    if (strcasecmp ("any", id) == 0) {
		        if (typestr) {
		            use_any = 1;
	                    nres = vot_regResolver ("%", typestr, bpass, "",
		                NULL, fields, -1, all_data, dalOnly, &result);
			    nservices += nres;
		        } else if (inventory) {
			    nservices = -1;
		        } else {
			    fprintf (stderr,
			      "Must specify service type for 'any' query\n");
			    break;
		        }
		    } else {
			/* If we're supporting Registry 1.0 then we need to
			** transform the VizieR ivorns before doing to search.
			*/
			if (strncasecmp("ivo://CDS.VizieR",id,16) == 0)
			   all_data++;
#ifdef REG10_KLUDGE
			if (reg10 || 
			    strncasecmp("ivo://CDS.VizieR",id,16) == 0) {
				char ivorn[SZ_LINE];

			        bzero (ivorn, SZ_LINE);
			        strcpy (ivorn, id);
			        strcat (ivorn, "%");
			        ivorn[9] = '/';
			        all_data++;
	                    nres = vot_regResolver (ivorn, typestr, bpass, "",
		                NULL, fields, -1, !all_data, dalOnly, &result);

			} else {
#endif
			    if (strncasecmp("ivo://CDS/VizieR",id,16) == 0) {
				char ivorn[SZ_LINE];

			        bzero (ivorn, SZ_LINE);
			        strcpy (ivorn, id);
			        strcat (ivorn, "%");
			        ivorn[9] = '.';
	                        nres = vot_regResolver (ivorn, typestr, bpass,
				   NULL, "", fields, -1, !all_data, 0, &result);
			        if (nres == 0) {
				    int len = strlen (ivorn);
				    char *ip = &ivorn[len-1];

				    for ( ; *ip != '/'; ip--) *ip = '\0';
	                            nres = vot_regResolver (ivorn, typestr,
					bpass, "", NULL, fields, -1, !all_data,
					0, &result);
			        }

			    } else {
	                        nres = vot_regResolver (id, typestr, bpass, "",
		                    NULL, fields, -1, !all_data, dalOnly, 
				    &result);
			        if (nres == 0 && !all_data) {
				    /* No results for exact match, try again by
				    ** being a little more liberal with matching
				    */
				    all_data++;
	                            nres = vot_regResolver (id, typestr, bpass,
					NULL, "", fields, -1, 0, dalOnly, 
					&result);
			        }
			    }
#ifdef REG10_KLUDGE
			}
#endif
	    		if (nres == 0) {
			    /* For no results from the registry, assume
			    ** any 'http' URI is instead a file to download.
			    */
			    if (!url_proc && strncmp (id, "http", 4) == 0) {
            		        vot_addToAclist (id, NULL);
			        url_proc++;
				break;
	    		    }
	    		}
			nservices += nres;
		    }
	            if ((nres > 1 && verbose) && !all_data && !use_any) {
	                fprintf (stderr,
			    "# Service query '%s' non-unique (%d found)...\n",
			    id, nres);
	            }

		    /* Cache the result.
		    */
		    c_name = vot_regCacheName (id, typestr, bpass);
		    vot_regCacheResults (c_name, result, nres);
		}
	    }
	}

	/* Replace problem characters in the name so it can be used in
	** a filename.
	*/
	for (np=id; *np; np++)
	    if (*np == ' ' || *np == '/' || *np == '(' || *np == ')')
		*np = '_';


	/* Loop over each of the resources found, parsing the result string
	** and adding each service in turn.  If we're not doing all the data,
	** use the first result found and break.
	*/
	rp = &result[0];
	for (i=0; i < nres; i++) {

	    /* Split the resolved string to the url and type.
	    */
	    memset (url,  0, SZ_URL);
	    memset (sname, 0, SZ_LINE);
	    memset (ident, 0, SZ_LINE);
	    memset (type, 0, SZ_LINE);
	    memset (title, 0, SZ_LINE);

	    for (np=url, len=SZ_URL; *rp && *rp != '\t' && len; len--)   
		*np++ = *rp++;
	    rp++;
	    for (np=sname, len=SZ_LINE; *rp && *rp != '\t' && len; len--) 
		*np++ = *rp++;
	    rp++;
	    for (np=ident, len=SZ_LINE; *rp && *rp != '\t' && len; len--) 
		*np++ = *rp++;
	    rp++;
	    for (np=type, len=SZ_LINE; *rp && *rp != '\t' && len; len--)  
		*np++ = *rp++;
	    rp++;
	    for (np=title, len=SZ_LINE; *rp && *rp != '\n' && len; len--)  
		*np++ = *rp++;
	    rp++;

	    /* Skip services we don't yet support.
	    if (! vot_isSupportedSvc (type) && !meta) {
	    */
	    if (! vot_isSupportedSvc (type)) {
		if (!quiet && verbose > 1)
		  fprintf (stderr,
		    "# Unsupported service type '%s' for '%s', skipping...\n",
		      type, sname);
		continue;
	    }

	    /* Check for a specifically requested service number.
	     */
	    if (svcNumber > 0) {
		char  *ip, *op, num[SZ_FNAME];

		bzero (num, SZ_FNAME);
		for (ip=sname; *ip && ! isdigit(*ip); ip++) ;
		for (op=num; *ip && isdigit(*ip); ip++) 
		    *op++ = *ip;

		if (atoi(num) != svcNumber)
		    continue;
	    }

            if (verbose && !quiet && !use_any && nres > 1) {
		if (all_data)
		    fprintf (stderr, "# Using %s Resource %s_%s -> %s\n", 
			type, id, vot_urlFname(url), ident);
		else
		    fprintf (stderr, "# Using %s Resource %s -> %s\n",
			type, sname, ident);
	        if (debug) {
		    fprintf (stderr, "%d url = '%s'\n", i, url);
		    fprintf (stderr, "%d type = '%s'\n", i, type);
	        }
	    }

            /* Save results in the service list.
            */
	    bzero (name, SZ_LINE);
	    strcpy (name, (use_any || all_data ? sname : id));

	    vot_addToSvcList (name, ident, url, type, title);

	    if (!all_data && !use_any)
		break;
	}

	if (result) 
	    free ((char *)result);
    }

    return (0);
}


/****************************************************************************
**  Utility routine to add a URL to the service list.
*/
void
vot_addToSvcList (char *name, char *ident, char *url, char *type, char *title)
{
    Service *svc;
    char    *ip, *op;


    if (url == (char *)NULL || strcasecmp ("NOT PROVIDED", url) == 0) {
	fprintf (stderr,
	    "Warning: Empty ServiceURL for '%s', skipping...\n", name);
	return;
    }

    /* Save results in the service list.
    */
    svc =  (Service *)calloc (1,sizeof(Service));
    if (!svcList)
        svcList = svcTail = svc;
    else
        svcTail->next = (Service *)svc;


    /* Clean up any '&amp;' encodings in the URL.
    */
    for (ip = op = url; *ip; ) {
	if (*ip == '&' && strncmp (ip, "&amp", 4) == 0) {
	    ip += ((strncmp (ip, "&amp;", 5) == 0) ? 5 : 4);
	    *op++ = '&';
 	} else
	    *op++ = *ip++;
    }
    *op = '\0';

    
    strcpy (svc->name, name);
    strcpy (svc->service_url, url);
    strcpy (svc->identifier, ident);
    strcpy (svc->title, title);
    if (strncasecmp (type, "cone", 4) == 0 ||
	strncasecmp (type, "catalog", 7) == 0) {
            svc->func = &vot_callConeSvc;
	    if (strstr (url, "vizier")) {
            	svc->type = SVC_VIZIER;
	    } else 
                svc->type = SVC_CONE;
    } else if (strncasecmp (type, "sia", 3) == 0 ||
	strncasecmp (type, "simpleimage", 9) == 0) {
            svc->func = &vot_callSiapSvc;
            svc->type = SVC_SIAP;
    } else if (strncasecmp (type, "ssap", 4) == 0 ||
	strncasecmp (type, "simplespec", 9) == 0) {
            svc->func = &vot_callSsapSvc;
            svc->type = SVC_SSAP;
    } else if (strncasecmp (type, "tabularsky", 8) == 0 ||
	       strstr (url, "vizier")) {
            	   svc->func = &vot_callConeSvc;
            	   svc->type = SVC_VIZIER;
    } else {
         svc->func = NULL;
         svc->type = SVC_OTHER;
    }
    svc->index = svcIndex++;

    svcTail = svc;
}


int
vot_isSupportedSvc (char *type)
{
    if (strncasecmp (type, "cone", 4) == 0 || 
	strncasecmp (type, "catalog", 7) == 0)
            return (1);
    else if (strncasecmp (type, "sia", 3) == 0 || 
	     strncasecmp (type, "simpleimage", 10) == 0)
                return (1);
    else if (strncasecmp (type, "ssap", 4) == 0 ||
	     strncasecmp (type, "simplespec", 9) == 0)
                return (1);
    else if (strncasecmp (type, "tabularsky", 10) == 0)
        return (1);

    if (type && (*type) == (char) 0)
	return (0);
         
    return (0);
}


/****************************************************************************
**  Utility routines to print and count the service list.
*/

int
vot_countServiceList ()
{
    register int i = 0;
    Service *svc = svcList;

    if (nservices < 0)
	return (nservices);
    else if (!svc)
	return (0);
    else
        while ((svc = svc->next)) i++;

    return (i+1);
}

int
vot_printServiceList (FILE *fd)
{
    register int i = 0;
    Service *svc = svcList;

    if (!svc)
	return (0);

    fprintf (fd, "# Services queried:  %d\n#\n", vot_countServiceList());
    fprintf (fd, "# %4s\t%-16.16s\t%s\n", "Id", "Name", "Title");
    do {
	fprintf (fd, "%4d\t%-16.16s\t%s\n", i++, svc->name, svc->title);
    } while ((svc = svc->next));

    return (0);
}

int 
vot_printServiceVOTable (FILE *fd)
{
    Service *svc = svcList;

    if (!svc)
	return (0);


    /* Print the VOTable header.
    */
    fprintf (fd, "\
        <?xml version=\"1.0\" encoding=\"utf-8\"?>\
        <VOTABLE ID=\"ID\" xmlns=\"http://www.ivoa.net/xml/VOTable/v1.1\">\
        <RESOURCE><TABLE>");

    fprintf (fd, "\
        <FIELD datatype=\"char\" name=\"shortName\" arraysize=\"*\"/>\
        <FIELD datatype=\"char\" name=\"identifier\" arraysize=\"*\"/>\
        <FIELD datatype=\"char\" name=\"accessURL\" arraysize=\"*\"/>\
        <FIELD datatype=\"char\" name=\"title\" arraysize=\"*\"/>");

    fprintf (fd, "<DATA><TABLEDATA>");

    do {
	fprintf(stderr,"<TR><TD>%s</TD><TD>%s</TD><TD>%s</TD><TD>%s</TD></TR>",
	    svc->name, svc->service_url, svc->identifier, svc->title);
    } while ((svc = svc->next));

    /* Close off the table output.
    */
    fprintf (fd, "</TABLEDATA></DATA></TABLE></RESOURCE></VOTABLE>\n");

    return (0);
}



/******************************************************************************
** Registry cache handling routines.
**
**     ***********  FIXME  *************
*/

#define C_EXPIRE		2592000		/* 30 days		*/


/* Create the cache name from the resolver terms.
*/
static char *
vot_regIsCached (char *id, char *type, char *bpass)
{
    char *fname = vot_regCacheName (id, type, bpass);
    char *s, *line[SZ_LINE];
    struct stat st;
    time_t now = time ((time_t)NULL);
    FILE *fd;


return ((char *) NULL);		/** FIXME -- CURRENTLY DISABLED */

    if ((s = getenv("VOC_NO_CACHE")))
        return ((char *) NULL);

    if (access (fname, F_OK) == 0) {
	/* Check first for an expired entry.
	*/
	stat (fname, &st);
	if ((st.st_ctime - now) > C_EXPIRE) {
	    unlink (fname);
    	    return ((char *) NULL);
	}

	/* Now see whether the contents look valid.
	*/
        if ((fd = fopen (fname, "r")) != (FILE *) NULL) {
	    (void) fgets ((char *)line, SZ_LINE, fd);
	    (void) fgets ((char *)line, SZ_LINE, fd);
	    if (strncasecmp ("INDEF", (char *)line, 5) == 0) {
	        fclose (fd);
	        unlink (fname);
		return ((char *)NULL);
	    }
	    fclose (fd);
        } 

	return ((char *) fname);
    }

    return ((char *) NULL);
}


/* Fetch the cached data.
*/
static char *
vot_regGetCacheResults (char *fname, int *nres)
{
    FILE *fd;
    int  nr, size;
    char line[SZ_LINE], *results;

    memset (line, 0, SZ_LINE);
    if ((fd = fopen (fname, "r")) != (FILE *) NULL) {
	(void) fgets ((char *)line, SZ_LINE, fd);
	sscanf ((char *)line, "%d %d\n", &nr, &size);
	results = (char *) calloc (1, size+2);

	while (fgets ((char *)line, SZ_LINE, fd))
	    strcat ((char *)results, (char *)line);
    } 
    fclose (fd);

    *nres = nr;
    return ((char *)results);
}


/* Create the cache name from the resolver terms.
*/
static void
vot_regCacheResults (char *fname, char *results, int nres)
{
    FILE *fd;

    if (access (fname, F_OK) == 0)
	unlink (fname);
    if ((fd = fopen (fname, "w+")) != (FILE *) NULL) {
	fprintf (fd, "%d %d\n%s", nres, (int)strlen(results), results);
	fclose (fd);
    } 
}


/****************************************************************************
** Create the cache name from the resolver terms.
*/
static char *
vot_regCacheName (char *id, char *type, char *bpass)
{
    static char name[SZ_FNAME];
    char s1[SZ_FNAME], s2[SZ_FNAME], s3[SZ_FNAME];
    extern char *voc_getCacheDir (char *s);

    bzero (name, SZ_FNAME);		/* Initialize strings		*/

    strcpy (s1, vot_normalize (id));
    strcpy (s2, vot_normalize (type));
    strcpy (s3, vot_normalize (bpass));

    sprintf (name, "%s/%s_%s_%s", voc_getCacheDir("regResolver"), s1, s2, s3);

    return (name);
}



void
vot_readSvcFile (char *fname, int dalOnly)
{
    char tmpfile[SZ_FNAME];
    extern char *vot_copyStdin();


    bzero (tmpfile, SZ_FNAME);
    
    if (strcmp (fname, "-") == 0) {             /* read from stdin      */

        strcpy (tmpfile, vot_copyStdin ());

        /* Parse the input and unlink the temp file.
        */
        vot_parseServiceList (tmpfile, dalOnly);
        unlink (tmpfile);

    } else if (access (fname, R_OK) == 0) 
        vot_parseServiceList (fname, dalOnly);
}

