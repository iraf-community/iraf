/************************************************************************
**  VOSIAP.C -- Worker procedure to make a query to an SIAP service.
**
**  M. Fitzpatrick, NOAO, July 2007
*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include "VOClient.h"
#include "voAppsP.h"
#include "samp.h"


extern int  errno;
extern int  debug, verbose, all_named, all_data, save_res, extract, quiet;
extern int  meta, dverbose, count, count_only, file_get, use_name, format;
extern int  id_col, samp, samp_p;

extern char *output;


int     vot_callSiapSvc (svcParams *pars);
char   *vot_validateFile (char *fname);

extern int    vot_extractResults (char *result, char delim, svcParams *pars);
extern int    vot_printCount (Query query, svcParams *pars, int *count);
extern int    vot_countResults (char *result);
extern void   vot_printCountHdr (void);
extern void   vot_printCountLine (int nrec, svcParams *pars);
extern void   vot_dalExit (int code, int count);
extern void   vot_printHdr (int fd, svcParams *pars);
extern void   vot_printAttrs (char *fname, Query query, char *id);
extern char   vot_svcTypeCode (int type);
extern char  *vot_normalize (char *str);
extern char  *vot_getOFName (svcParams *pars, char *extn, int pid);
extern char  *vot_getOFIndex (svcParams *pars, char *extn, int pid);



/************************************************************************
**  VOT_CALLSIAPSVC -- Call a Simple Image Access service.
*/
int 
vot_callSiapSvc (svcParams *pars)
{
    char  *result = (char *)NULL;
    char  *extn, fname[SZ_LINE];
    DAL	   siap;				/* DAL Connection handle */
    Query  query;				/* Query handle		 */
    pid_t  cpid;
    int    fd, code, res_count = 0;

	

    if (debug)
	fprintf (stderr, "siapCaller(%s:%d): ra = %f  dec = %f\n", 
  	    pars->name, getpid(), pars->ra, pars->dec);


    if ((cpid = fork()) < 0) {
	fprintf (stderr, 
	    "vot_callSiapSvc: Unable to create child process, exiting\n");
	exit (-1);

    } else if (cpid > 0) { 			/* Parent process	*/
	return (cpid);

    } else { 					/* Child process 	*/

	if (getenv("VOC_NO_NETWORK"))
	    return (OK);

        /*  Initialize the VOClient code.  Error messages are printed by the
        **  interface so we just quit if there is a problem.
        */
        if (voc_initVOClient ((char *) NULL) == ERR) 
            vot_dalExit (E_VOCINIT, 0);

        /*  Get a new connection to the named service and form the query.
        */
        siap = voc_openSiapConnection (pars->service_url);

	if (meta)
            query = voc_getSiapQuery (siap, 0.0, 0.0, 0.0, 0.0, "METADATA");
	else
            query = voc_getSiapQuery (siap, pars->ra, pars->dec,
	        pars->sr, pars->sr, (char *)NULL);

	/* Not all SIAP services support VERB, leave it out for now....
	*/
        if (verbose > 1)
            (void) voc_addIntParam (query, "VERB", (all_data ? 3 : verbose));


        /* Execute the query.
        */
	if (debug) {
	    fprintf (stderr, "siapCaller(%s:%d): executing query....%d\n",
		pars->name, getpid(), pars->fmt);
	    fprintf (stderr, "Executing SIAP Query(%d):\n  %s\n\n", query,
        	voc_getQueryString (query, SIAP_CONN, 0));
	}

	    
	if (count_only) {
	    if ((code = vot_printCount (query, pars, &res_count)) != E_NONE) {
		vot_dalExit (code, res_count);
	    }

	} else if (meta) {
            memset (fname, 0, SZ_LINE);
            if (output)
                strcpy (fname, output);
            else
                sprintf (fname, "%s_%c.meta", vot_normalize (pars->name),
                    vot_svcTypeCode(pars->type));
	    vot_printAttrs (fname, query, pars->identifier);

	} else {
	    char delim;

            switch (pars->fmt) {
            case F_ASCII:
	        extn = "asv"; delim = ' ';
		result = voc_executeASCII (query);
		break;
            case F_RAW:
	        extn = "xml"; delim = '\0';
		result = voc_executeVOTable (query);
		break;
            case F_RAW | F_XML:
		extract |= EX_XML;
	        extn = "xml"; delim = '\0';
		result = voc_executeVOTable (query);
		break;
            case F_CSV:
	        extn = "csv"; delim = ',';
		result = voc_executeCSV (query);
		break;
            case F_CSV | F_HTML:
		extract |= EX_HTML;
	        extn = "csv"; delim = ',';
		result = voc_executeCSV (query);
		break;
            case F_CSV | F_KML:
		extract |= EX_KML;
	        extn = "csv"; delim = ',';
		result = voc_executeCSV (query);
		break;
            case F_TSV:
	        extn = "tsv"; delim = '\t';
		result = voc_executeTSV (query);
		break;
            default:
	        fprintf (stderr, "siapCaller: Unknown format: %d\n", pars->fmt);
	        exit (-1);
            }

 	    if (!result) {
   	        vot_dalExit (E_NODATA, 0);
	    } else if (pars->fmt != F_RAW) {
		res_count = vot_extractResults (result, delim, pars);
	    } else if (pars->fmt == F_RAW) {
		res_count = vot_countResults (result);
	    }

            if (count && (!output || (output && output[0] != '-')))
	    	vot_printCountLine (res_count, pars);
#ifdef EARLY_EXIT
 	    if (res_count == 0) {
        	voc_closeConnection (siap);
        	voc_closeVOClient (0);
		if (result) free ((char *) result);
   	        vot_dalExit (E_NODATA, res_count);
	    }
#endif

	    /* Check for a NULL result indicating an error in the call.
	    */
	    if (result == (char *)NULL) {
		char *err;
		extern char *voc_getErrMsg();

		err = voc_getErrMsg ();
		if (err && strncmp (err, "ERROR", 5) == 0) {
		    if (verbose > 1)
			fprintf (stderr, "Pid %d: %s\n", getpid(), err);
   	            vot_dalExit (E_REQFAIL, res_count);
		} else
   	            vot_dalExit (E_NODATA, res_count);
	    }

            if (use_name || all_named || id_col)
                strcpy (fname, vot_getOFName (pars, extn, (int)getpid()));
            else
                strcpy (fname, vot_getOFIndex (pars, extn, (int)getpid()));


            if (output && output[0] == '-' && (! extract & EX_COLLECT)) {
                write (fileno(stdout), result, strlen (result)-1);

            } else if (format != (F_CSV|F_HTML) && format != (F_CSV|F_KML)) {
	        if ((fd = open (fname, O_WRONLY|O_CREAT, 0644)) < 0){
	            fprintf (stderr, "Error opening file '%s'\n", fname);
   	            vot_dalExit (E_FILOPEN, res_count);
	        }

                /* Output the result.
                if (pars->fmt = F_CSV || pars->fmt == F_TSV)
		    vot_printHdr (fd, pars);
                */
	        if (result)
	            write (fd, result, strlen (result)-1);
	        close (fd);

	    }
	}

        voc_closeConnection (siap);		/* close the siap connection */
        voc_closeVOClient (0);			/* close VOClient connection */
 
        if (result) free ((void *) result);	/* free local storage	     */

	if (debug) {
	    fprintf (stderr, "siapCaller(%s:%d): exiting....\n",
		pars->name, getpid());
	}

	if (samp) {
	    char  url[SZ_FNAME], cwd[SZ_FNAME];
	    extern int samp_tableLoadVOTable ();

            samp_p = sampInit ("VOData", "VOClient Data Access");
            samp_setSyncMode (samp_p);
            sampStartup (samp_p);

            memset (cwd, 0, SZ_FNAME);
            if (getcwd (cwd, SZ_FNAME) < 0)
                strcpy (cwd, "./");

	    memset (url, 0, SZ_FNAME);
	    sprintf (url, "file://%s/%s", cwd, fname);
            (void) samp_tableLoadVOTable (samp_p, "all", url, NULL, NULL);

	    samp_UnRegister (samp_p);
	}

	if (res_count == 0)
	    unlink (fname);
        vot_dalExit (E_NONE, res_count);	/* no error		*/
    }

    return (OK);
}

		
/************************************************************************
**  
*/
char *
vot_validateFile (char *fname)
{
    int  fd, size;
    static char buf[10], new[SZ_FNAME], *extn;


    if ((fd = open (fname, O_RDONLY, 0777)) <= 0) {
	unlink (fname);
	return (0);
    }

    if ((size = read (fd, buf, 10)))
        close (fd);

    if (strncmp ("SIMPLE", buf, 6) == 0) {
	extn = "fits";
    } else if (strncmp ("GIF8", buf, 4) == 0) {
	extn = "gif";
    } else if (strncmp ("PNG\r\n", &buf[1], 5) == 0) {
	extn = "png";
    } else if (buf[0] == '\377' && buf[1] == '\330' && buf[2] == '\377' &&
        buf[3] == '\340') {
	    extn = "jpg";
    } else if (buf[0] == '\037' && buf[1] == '\213') {
	extn = "gz";
    } else
	return (fname);

    sprintf (new, "%s.%s", fname, extn);
    rename (fname, new);

    return (new);
}
