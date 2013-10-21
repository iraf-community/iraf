/**
 *  PKG_MAIN -- VOPackage task main().
 *
 *  @file       pkgMain.c
 *  @author     Mike Fitzpatrick
 *  @date       12/13/12
 *
 *  @brief      VOPackage task main().
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "votools.h"			/* package task declarations	    */


static int help		 = 0;		/* help flag			    */
static int debug	 = 0;		/* debug flag			    */
static int verbose	 = 0;		/* verbose flag			    */
static int interactive	 = 0;		/* interactive		    	    */
static int connected	 = 0;		/* connected subprocess		    */
static int detached	 = 0;		/* detached subprocess		    */

static int do_attrs	 = 0;		/* print package attrs		    */
static int do_taskList	 = 0;		/* print task list		    */
static int do_taskParams = 0;		/* print package attrs		    */

static char *taskName	 = NULL;	/* name of task to execute	    */
static char *cbsocket	 = NULL;	/* callback socket		    */


static void vopHelp(void);


static char  *opts      = "hNno:r%:";
static struct option long_opts[] = {
        { "Number",       2, 0,   'N'},         /* task option          */
        { "number",       2, 0,   'n'},         /* task option          */
        { "output",       1, 0,   'o'},         /* task option          */
        { "return",       2, 0,   'r'},         /* task option          */
        { "help",         2, 0,   'h'},         /* required             */
        { "test",         1, 0,   '%'},         /* required             */
        { NULL,           0, 0,    0 }
};



/**
 *   Application entry point.
 */
int main (int argc, char *argv[])
{
    char **pargv, optval[SZ_FNAME], ch;
    int    pos = 0;


    /* Process arguments.
     */
    pargv = vo_paramInit (argc, argv);
    while ((ch = vo_paramNext (opts,long_opts,argc,pargv,optval,&pos)) != 0) {
        if (ch > 0) {
            switch (ch) {
	    case 'h': 	vopHelp (); break;
	    case 'd': 	debug++;    break;
	    case 'v': 	verbose++;  break;

	    case 'a':			/* package attributes		    */
	        do_attrs++; 
	        break;
	    case 't':			/* package task list		    */
	        do_taskList++; 
	        break;
	    case 'p':			/* task parameters ("-p <task>")    */
	        do_taskParams++; 
	        break;
	    case 'S':			/* callback socket ("-S <sock>")    */
	        cbsocket = strdup (optval);
	        break;
            }
        } else {
	    taskName = strdup (optval);/* execute task .... 	    */
        }
    }

    if (!cbsocket) {
        cbsocket = (char *) calloc (1, SZ_FNAME);
	sprintf (cbsocket, "/tmp/voPkg_%d", (int) getuid());
    }

    if (!connected && !detached) { 	/* run interactively 		    */
	interactive++;
    } else if (connected) { 		/* run as a connected process 	    */

    } else if (detached) { 		/* run as a detached (bkg) process  */
    }


    if (cbsocket)
	free ((void *) cbsocket);
    return (0);
}


/**
 *  VOPHELP -- Task help summary.
 */
static void
vopHelp ()
{
  printf ("\n\
    Usage:   %s [-dhiv] [-a] | [-t] | [-p <task>] -S <cbsock> [<task>]]\n\
  \n\
    ", PKG_NAME);
}


