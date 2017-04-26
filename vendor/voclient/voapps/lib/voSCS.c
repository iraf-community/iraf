/************************************************************************
**  VOSCS.C -- Worker procedure to query a Simple Cone Search service.
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

int     vot_callConeSvc (svcParams *pars);

extern int    vot_extractResults (char *result, char delim, svcParams *pars);
extern int    vot_printCount (Query query, svcParams *pars, int *count);
extern int    vot_countResults (char *result);
extern char   vot_svcTypeCode (int type);
extern void   vot_printCountHdr (void);
extern void   vot_printCountLine (int nrec, svcParams *pars);
extern void   vot_dalExit (int code, int count);
extern void   vot_printHdr (int fd, svcParams *pars);

extern  void  vot_printAttrs (char *fname, Query query, char *id);
extern  char *vot_normalize (char *str);
extern  char *vot_getOFName (svcParams *pars, char *extn, int pid);
extern  char *vot_getOFIndex (svcParams *pars, char *extn, int pid);


/************************************************************************
**  VOT_CALLCONESVC -- Call a Cone Search service.
*/
int 
vot_callConeSvc (svcParams *pars)
{
    char  *result = (char *)NULL;
    char  *extn, fname[SZ_LINE], delim;
    DAL	   cone;				/* DAL Connection handle */
    Query  query;				/* Query handle		 */
    pid_t  cpid;
    int    fd, res_count=0, code;
	

    if (debug)
	fprintf (stderr, "coneCaller(%s:%d): ra = %f  dec = %f\n", 
  	    pars->name, getpid(), pars->ra, pars->dec);


    if ((cpid = fork()) < 0) {
	fprintf (stderr, 
	    "vot_callConeSvc: Unable to create child process, exiting\n");
	vot_dalExit (-1, 0);

    } else if (cpid > 0) { 			/* Parent process	*/
	return (cpid);

    } else { 					/* Child process 	*/

	if (getenv ("VOC_NO_NETWORK"))
    	    return (OK);

        /*  Initialize the VOClient code.  Error messages are printed by the
        **  interface so we just quit if there is a problem.
        */
        if (voc_initVOClient ((char *) NULL) == ERR) 
            vot_dalExit (E_VOCINIT, 0);

        /*  Get a new connection to the named service and form the query.
	if (all_data || verbose > 1)
        */
        cone = voc_openConeConnection (pars->service_url);
        query = voc_getConeQuery (cone, pars->ra, pars->dec, 
	    (meta ? 0.0 : pars->sr));
	if (verbose > 1)
	    (void) voc_addIntParam (query, "VERB", (all_data ? 3 : verbose));


        /* Execute the query.
        */
	if (debug) {
	    fprintf (stderr, "coneCaller(%s:%d): executing query....\n",
		pars->name, getpid());
	    fprintf (stderr, "Executing Cone Query:\n  %s\n\n", 
        	voc_getQueryString (query, CONE_CONN, 0));
	}
	    
	if (count_only) {
/*
	    if ((code = vot_printCount (query, pars, &res_count)) != E_NONE) {
		vot_dalExit (code, res_count);
	    }
*/
	    code = vot_printCount (query, pars, &res_count);
	    vot_dalExit (code, res_count);

	} else if (meta) {
	    bzero (fname, SZ_FNAME);
	    if (output)
		strcpy (fname, output);
	    else
                sprintf (fname, "%s_%c.meta", vot_normalize (pars->name), 
                    vot_svcTypeCode(pars->type));
	    vot_printAttrs (fname, query, pars->identifier);

	} else {

            switch (pars->fmt) {
            case F_ASCII:
	        extn = "asv", delim = ' ';
		result = voc_executeASCII (query);
		break;
            case F_RAW:
	        extn = "xml", delim = '\0';
		result = voc_executeVOTable (query);
		break;
            case F_RAW | F_XML:
                extract |= EX_XML;
                extn = "xml"; delim = '\0';
                result = voc_executeVOTable (query);
                break;
            case F_CSV:
	        extn = "csv", delim = ',';
		result = voc_executeCSV (query);
		break;
            case F_CSV | F_HTML:
		extract |= EX_HTML;
	        extn = "csv", delim = ',';
		result = voc_executeCSV (query);      
		break;
            case F_CSV | F_KML:
		extract |= EX_KML;
	        extn = "csv", delim = ',';
		result = voc_executeCSV (query);      
		break;
            case F_TSV:
	        extn = "tsv", delim = '\t';
		result = voc_executeTSV (query);      
		break;
            default:
	        fprintf (stderr, "coneCaller: Unknown format: %d\n", pars->fmt);
	        vot_dalExit (-1, 0);
            }

 	    if (!result)
   	        vot_dalExit (E_NODATA, 0);
	    else if (pars->fmt != F_RAW)
		res_count = vot_extractResults (result, delim, pars);
	    else 
		res_count = vot_countResults (result);

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

		if (strncmp ((err = voc_getErrMsg()), "ERROR", 5) == 0) {
		    if (verbose > 1)
			fprintf (stderr, "Pid %d: %s\n", getpid(), err);
   	            vot_dalExit (E_REQFAIL, res_count);

		} else
   	            vot_dalExit (E_NODATA, res_count);
	    }

	    if (all_data && pars->type == SVC_VIZIER) {
		char  *vot_urlFname (char *url);

		if (output) {
		    /*
		    */
		    if (output[0] == '-')
			strcpy (fname, vot_getOFName(pars,extn,(int)getpid()));
		    else
	                sprintf (fname, "%s_%s.%s",
		            output, vot_urlFname(pars->service_url), extn);

		} else
	            sprintf (fname, "%s%s.%s",
		        vot_normalize(pars->name),
		        vot_urlFname(pars->service_url), extn);

	    } else if (use_name || all_named || id_col) {
		strcpy (fname, vot_getOFName (pars, extn, (int)getpid()));

	    } else {
		strcpy (fname, vot_getOFIndex (pars, extn, (int)getpid()));
	    }

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
	        write (fd, result, strlen (result)-1);
	        close (fd);
	    }
	}

        voc_closeConnection (cone);		/* close the cone connection */
        voc_closeVOClient (0);			/* close VOClient connection */
 
        if (result) free ((void *) result);	/* free local storage	     */

	if (debug)
	    fprintf (stderr, "coneCaller(%s:%d): exiting....\n",
		pars->name, getpid());

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
        vot_dalExit (0, res_count);
    }

    return (OK);
}
