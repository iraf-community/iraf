/************************************************************************
**  VOSSAP.C -- Worker procedure to make a query to an SSAP service.
**
**  M. Fitzpatrick, NOAO, Februaary 2009
*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include "VOClient.h"
#include "voAppsP.h"


extern int  errno;
extern int  debug, verbose, all_named, all_data, save_res, extract, quiet;
extern int  meta, dverbose, count, count_only, file_get, use_name, format;
extern int  id_col;

extern char *output, *d2_band, *d2_time, *d2_format, *d2_version;


int     vot_callSsapSvc (svcParams *pars);

extern int    vot_extractResults (char *result, char delim, svcParams *pars);
extern int    vot_printCount (Query query, svcParams *pars, int *count);
extern int    vot_countResults (char *result);
extern char   vot_svcTypeCode (int type);
extern char  *vot_normalize (char *str);
extern char  *vot_getOFName (svcParams *pars, char *extn, int pid);
extern char  *vot_getOFIndex (svcParams *pars, char *extn, int pid);
extern void   vot_printCountHdr (void);
extern void   vot_printCountLine (int nrec, svcParams *pars);
extern void   vot_dalExit (int code, int count);
extern void   vot_printHdr (int fd, svcParams *pars);
extern void   vot_printAttrs (char *fname, Query query, char *id);



/************************************************************************
**  VOT_CALLSSAPSVC -- Call a Simple Image Access service.
*/
int 
vot_callSsapSvc (svcParams *pars)
{
    char  *result = (char *)NULL;
    char  *extn, fname[SZ_LINE];
    DAL	   ssap;				/* DAL Connection handle */
    Query  query;				/* Query handle		 */
    pid_t  cpid;
    int    fd, code, res_count = 0;

	

    if (debug)
	fprintf (stderr, "ssapCaller(%s:%d): ra = %f  dec = %f\n", 
  	    pars->name, getpid(), pars->ra, pars->dec);


    if ((cpid = fork()) < 0) {
	fprintf (stderr, 
	    "vot_callSsapSvc: Unable to create child process, exiting\n");
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
        ssap = voc_openSsapConnection (pars->service_url);
	if (meta) {
            query = voc_getSsapQuery (ssap, 0.0, 0.0, 0.0, 
		(char *) NULL, (char *) NULL, "METADATA");
	} else {
            query = voc_getSsapQuery (ssap, pars->ra, pars->dec, pars->sr, 
		(char *) d2_band, 		/* BAND 	*/
		(char *) d2_time, 		/* TIME 	*/
		(char *) d2_format);		/* FORMAT 	*/

	    if (d2_version)
		(void) voc_addStringParam (query, "VERSION", d2_version);
	    (void) voc_addStringParam (query, "REQUEST", "queryData");
	}


        /* Execute the query.
        */
	if (debug) {
	    fprintf (stderr, "ssapCaller(%s:%d): executing query....%d\n",
		pars->name, getpid(), pars->fmt);
	    fprintf (stderr, "Executing SSAP Query(%d):\n  %s\n\n", query,
        	voc_getQueryString (query, SSAP_CONN, 0));
	}


	if (count_only) {
	    if ((code = vot_printCount (query, pars, &res_count)) != E_NONE) {
		vot_dalExit (code, res_count);
	    }

	} else if (meta) {
            bzero (fname, SZ_FNAME);
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
	        fprintf (stderr, "ssapCaller: Unknown format: %d\n", pars->fmt);
	        exit (-1);
            }

 	    if (!result)
   	        vot_dalExit (E_NODATA, 0);
	    else if (pars->fmt != F_RAW)
		res_count = vot_extractResults (result, delim, pars);
	    else  if (pars->fmt == F_RAW)
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

        voc_closeConnection (ssap);		/* close the ssap connection */
        voc_closeVOClient (0);			/* close VOClient connection */
 
        if (result) free ((void *) result);	/* free local storage	     */

	if (debug) {
	    fprintf (stderr, "ssapCaller(%s:%d): exiting....\n",
		pars->name, getpid());
	}

        vot_dalExit (E_NONE, res_count);	/* no error		*/
    }

    return (OK);
}
