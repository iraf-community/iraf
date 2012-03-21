#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "samp.h"


int	interactive	= 0;
int	verbose		= 0;
int	debug		= 0;
int	generics	= 0;		/* run generic handlers		*/
int	trace		= 0;
int	nscan		= 0;
char	*name		= "zztest";
char	*descr		= "Test Application";
    
int	registered	= 0;
int     cmdNum 		= 0;

static int    proc_loop (FILE *ifd, int samp);
static int    proc_cmd (int samp, char *method, 
			    char *a1, char *a2, char *a3, char *a4);
static int    make_msg (char *mtype);
static void   cmdUsage (char *s);
static void   help_summary (void);
static void   print_result (handle_t samp, int stat);



/****************************************************************************
 *  Simple user-methods to install as message handlers.  All we do is
 *  print the arguments received to ensure the values are passed and that
 *  the function gets called properly.  Optional parameters are printed as
 *  INDEF values.
 ***************************************************************************/

#define	opt(s)		(s?s:"INDEF")


void generic_handler (char *sender, char *msg_id, int params) {
    printf ("user generic_handler('%s','%s',%d) .....\n", 
	sender, msg_id, params);
}

void hub_handler (char *sender, char *msg_id, int params) {
    printf ("user hub_handler('%s','%s',%d) .....\n", 
	sender, msg_id, params);
}


						    /* samp.app.ping          */
void ping_handler (char *sender) {
    printf ("[%d] user ping_handler('%s') .....\n", cmdNum++, sender);
}
						    /* samp.app.status        */
void status_handler (char *sender) {
    printf ("user status_handler()'%s' .....\n", sender);
}
						    /* table.load.*           */
void tblload_handler (char *url, char *tblId, char *name) {
    printf ("user tblload_handler(%s,%s,%s)\n", url, opt(tblId), opt(name));
}
						    /* table.load.fits        */
void tblFITS_handler (char *url, char *tblId, char *name) {
    printf ("user tblFITS_handler(%s,%s,%s)\n", url, opt(tblId), opt(name));
}
						    /* table.load.votable     */
void tblVOT_handler (char *url, char *tblId, char *name) {
    printf ("user tblVOT_handler(%s,%s,%s)\n", url, opt(tblId), opt(name));
}
						    /* table.highlight.row    */
void tbrow_handler (char *url, char *tblId, int row) {
    printf ("user tbrow_handler(%s,%s,%d)\n", url, opt(tblId), row);
}
						    /* table.select.rowList   */
void tbsel_handler (char *url, char *tblId, int rowList[], int nrows) {
    printf ("user tbsel_handler(%s,%s,[%d])\n", url, opt(tblId), nrows);
}
						    /* image.load.fits        */
void imload_handler (char *url, char *imId, char *name) {
    printf ("user imload_handler(%s,%s,%s)\n", url, opt(imId), opt(name));
}
						    /* coord.pointAt.sky      */
void pointat_handler (float ra, float dec) {
    printf ("user pointat_handler(%g,%g)\n", ra, dec);
}
						    /* bibcode.load           */
void bibcode_handler (char *bibcode) {
    printf ("user bibcode_handler(%s)\n", bibcode);
}
						    /* client.env.get */
void envGet_handler (char *name, char *value, int maxch) {
    char *s = getenv (name);

    printf ("user envget_handler(%s)\n", name);
    if (s)
	strncpy (value, s, maxch);
    else
	strncpy (value, "INDEF", maxch);
    printf ("user envget  '%s' = '%s'\n", name, value);
}
						    /* client.cmd.exec */
void cmdExec_handler (char *cmd) {
    printf ("user cmdexec_handler(%s)\n", cmd);
}
						    /* client.env.set */
void envSet_handler (char *name, char *value) {
    printf ("user envset_handler(%s,%s)\n", name, value);
}
						    /* client.param.get */
void paramGet_handler (char *name, char *value, int maxch) {
    printf ("user paramget_handler(%s)\n", name);
}
						    /* client.param.set */
void paramSet_handler (char *name, char *value) {
    printf ("user paramset_handler(%s,%s)\n", name, value);
}






/****************************************************************************
 *  Program entry point.
 */
int
main (int argc, char **argv)
{
    int	   i, j, len, samp;
    FILE    *ifd 		= stdin;
    char   ch, cmdline[SZ_CMD], method[SZ_CMD];
    char   arg1[SZ_CMD], arg2[SZ_CMD], arg3[SZ_CMD], arg4[SZ_CMD];


    /* Process commandline arguments.
    */
    memset (cmdline, 0, SZ_CMD);
    for (i=1; i < argc; i++) {
        if (argv[i][0] == '-' && !(isdigit(argv[i][1]))) {
            len = strlen (argv[i]);
            for (j=1; j < len; j++) {
                ch = argv[i][j];

                switch (ch) {
                case 'c':  
		    if ((ifd = fopen (argv[++i], "r")) == (FILE *) NULL) {
			fprintf (stderr, "Error opening '%s'\n", argv[i]);
			exit (1);
		    }
		    break;
                case 'D':  debug++;   		break;
                case 'g':  generics++; 		break;
                case 'd':  descr = argv[++i];   break;
                case 'n':  name = argv[++i];   	break;
                case 's':  interactive++;	break;
                case 'v':  verbose++;		break;
                default:
                    fprintf (stderr, "Unknown option '%c'\n", ch);
                    break;
                }
                j = len;        /* skip remainder       */
            }
        } else {
	    if (access (argv[i], F_OK) == 0) {
		if (! (ifd = fopen (argv[i], "r"))) {  /* argument is a file  */
		    fprintf (stderr, "Error opening '%s'\n", argv[i]);
		    exit (1);
		}
	    } else {
	        strcat (cmdline, argv[i]);
	        strcat (cmdline, " ");
	    }
	    break;
	}
    }


    /* Initialize the SAMP interface.
    */
    samp = sampInit (name, descr);

    /*  Set up some local metadata values.
     */
    samp_Metadata (samp, "samp.description.html", "<none/>");
    samp_Metadata (samp, "samp.icon.url", "http://iraf.noao.edu/icon.png");
    samp_Metadata (samp, "samp.documentation.url", "");
    samp_Metadata (samp, "author.email", "Will E Coyote");
    samp_Metadata (samp, "author.name", "rascal@acme.com");

    /*  Subscribe to various message types, we also install the local
     *  message handlers defined above.
     */
    samp_Subscribe (samp, "samp.app.ping",         ping_handler);
    samp_Subscribe (samp, "samp.app.status",       status_handler);
    samp_Subscribe (samp, "image.load.fits",       imload_handler);
    if (generics)
        samp_Subscribe (samp, "table.load.*",      tblload_handler);
    samp_Subscribe (samp, "table.load.fits",       tblFITS_handler);
    samp_Subscribe (samp, "table.load.votable",    tblVOT_handler);

    samp_Subscribe (samp, "table.highlight.row",   tbrow_handler);
    samp_Subscribe (samp, "table.select.rowList",  tbsel_handler);
    samp_Subscribe (samp, "coord.pointAt.sky",     pointat_handler);
    samp_Subscribe (samp, "client.cmd.exec",       cmdExec_handler);
    samp_Subscribe (samp, "client.env.set",        envSet_handler);
    samp_Subscribe (samp, "client.env.get",        envGet_handler);
    samp_Subscribe (samp, "client.param.set",      paramSet_handler);
    samp_Subscribe (samp, "client.param.get",      paramGet_handler);
    samp_Subscribe (samp, "bibcode.load",          bibcode_handler);


/*
    samp_Subscribe (samp, "spectrum.load.*",       NULL);
    samp_Subscribe (samp, "voresource.loadlist.*", NULL);

    samp_Subscribe (samp, "samp.app.event.*",      NULL);
    samp_Subscribe (samp, "*",      		   generic_handler);
*/
    samp_Subscribe (samp, "samp.hub.event.*",      hub_handler);

    sampStartup (samp);



    /* Process the commands.
    */
    if (cmdline[0] == 0) {
	proc_loop (ifd, samp);

    } else {
	nscan = sscanf (cmdline, "%s %s %s %s %s", 
			    method, arg1, arg2, arg3, arg4);
	proc_cmd (samp, method, arg1, arg2, arg3, arg4);
    }

    /*  Clean up.
     */
    if (registered && samp_UnRegister (samp) != OK)
	fprintf (stderr, "UnRegistration fails\n");

    return (0);
}


/* PROC_LOOP -- Process the commands interactively.
 */
static int
proc_loop (FILE *ifd, int samp)
{
    char  cmd[SZ_CMD], method[SZ_CMD];
    char  arg1[SZ_CMD], arg2[SZ_CMD], arg3[SZ_CMD], arg4[SZ_CMD];


    memset (cmd, 0, SZ_CMD);
    memset (method, 0, SZ_CMD);

    if (ifd == (FILE *) stdin)
        fprintf (stderr, "samp> ");

    while (fgets (cmd, SZ_CMD, ifd)) {

	memset (method, 0, SZ_CMD);
	memset (arg1,   0, SZ_CMD);
	memset (arg2,   0, SZ_CMD);
	memset (arg3,   0, SZ_CMD);
	memset (arg4,   0, SZ_CMD);

	nscan = sscanf (cmd, "%s %s %s %s %s", method, arg1, arg2, arg3, arg4);

        if (strcasecmp (method, "load") == 0) {		/* LOAD  */
	    if (access (arg1, F_OK) == 0) {
	        if (! (ifd = fopen (arg1, "r"))) {
		    fprintf (stderr, "Error opening '%s'\n", arg1);
		    exit (1);
	        }
	    }
	} else if (proc_cmd (samp, method, arg1, arg2, arg3, arg4) < 0)
	    break;

	memset (cmd, 0, SZ_CMD);
        if (ifd == (FILE *) stdin)
            fprintf (stderr, "samp> ");
    }

    return (0);
}


/* PROC_CMD -- Process a single command.
 */
static int
proc_cmd (int samp, char *method, 
	char *arg1, char *arg2, char *arg3, char *arg4)
{
    register int   i = 0, stat = 0;
    char  *appName = NULL;
    char   rstr[SZ_RESSTR], *sres = rstr;
    Map    resp;


    if (debug)
	fprintf (stderr, "Proc_Cmd Method '%s'  Args: '%s' '%s' '%s'\n", 
				method,arg1,arg2,arg3);

    if (strcasecmp (method, "quit") == 0) {			/* QUIT */
	printf ("Quitting....\n\n");
        if (sampShutdown (samp) != 0)
            fprintf (stderr, "Shutdown fails\n");
        sampClose (samp);

	return (0);

    } else if (strcasecmp (method, "start") == 0) {		/* START */
	printf ("Starting SAMP interface .....\n");
        sampStartup (samp);

    } else if (strcasecmp (method, "stop") == 0) {		/* STOP */
	printf ("Stopping SAMP interface .....\n");
        sampShutdown (samp);

    } else if (strcasecmp (method, "help") == 0) {		/* HELP */
	help_summary ();

    } else if (strcasecmp (method, "listClient") == 0) {	/* LISTCLIENT */
	samp_listClients (samp);

    } else if (strcasecmp (method, "trace") == 0) {		/* TRACE */
	trace++;
	if (trace % 2)
            setenv ("XMLRPC_TRACE_XML", "1", 1);
	else
	    unsetenv ("XMLRPC_TRACE_XML");

    } else if (strcasecmp (method, "target") == 0) {		/* TARGET */
	appName = arg1;



    } else if (strcasecmp (method, "envSet") == 0) {		/* ENVSET */
	if (nscan < 4) {
	    cmdUsage ("envSet appName keyw value");
	} else {
	    stat = samp_envSet (samp, arg1, arg2, arg3);
	    print_result (samp, stat);
	}

    } else if (strcasecmp (method, "envGet") == 0) {		/* ENVGET */
	if (nscan < 3) {
	    cmdUsage ("envGet appName keyw");
	} else {
	    char *v = samp_envGet (samp, arg1, arg2);
	    if (v) 
	 	printf ("OK  '%s' = '%s'\n", arg2, v);
	    else
	 	printf ("ERR  '%s' not found\n", arg2);
	}
        ;

    } else if (strcasecmp (method, "paramSet") == 0) {		/* PARAMSET */
	if (nscan < 4) {
	    cmdUsage ("envSet appName keyw value");
	} else {
	    stat = samp_paramSet (samp, arg1, arg2, arg3);
	    print_result (samp, stat);
	}

    } else if (strcasecmp (method, "paramGet") == 0) {		/* PARAMGET */
        ;



    } else if (strcasecmp (method, "notify") == 0) {		/* NOTIFY */
	if (nscan < 3) {
	    cmdUsage ("notify appName mtype");
	} else {
	    int msg = make_msg (arg2);
	    samp_notify (samp, arg1, msg);
	    print_result (samp, SAMP_OK);
	}

    } else if (strcasecmp (method, "notifyAll") == 0) {		/* NOTIFYALL */
	if (nscan < 2) {
	    cmdUsage ("notifyAll mtype");
	} else {
	    int msg = make_msg (arg1);
	    stat = samp_notifyAll (samp, msg);
	    print_result (samp, stat);
	}

    } else if (strcasecmp (method, "call") == 0) {		/* CALL */
	if (nscan < 3) {
	    cmdUsage ("call appName mtype");
	} else {
	    int msg = make_msg (arg2);
	    char *id = samp_call (samp, arg1, "testTag", msg);
	    fprintf (stderr, "msgId = '%s'\n", id);
	    print_result (samp, stat);
	}

    } else if (strcasecmp (method, "callAll") == 0) {		/* CALLALL */
	if (nscan < 2) {
	    cmdUsage ("callAll mtype");
	} else {
	    int msg = make_msg (arg1);
	    stat = samp_callAll (samp, "allTag", msg);
	    print_result (samp, stat);
	}

    } else if (strcasecmp (method, "callAndWait") == 0) {	/* CALLANDWAIT*/
	if (nscan < 3) {
	    cmdUsage ("callAndWait appName mtype");
	} else {
	    int msg = make_msg (arg2);
	    stat = samp_callAndWait (samp, arg1, "waitTag", msg);
	    print_result (samp, stat);
	}



    } else if (strcasecmp (method, "Register") == 0) {		/* REGISTER */
	if (nscan < 3) {
	    cmdUsage ("Register appName appDesc");
	} else {
    	    if (samp_Register (samp) != OK)
		fprintf (stderr, "Registration fails\n");
    	    else {
		registered = 1;
		fprintf (stderr, "Registration OK\n");
	    }
	}

    } else if (strcasecmp (method, "Unregister") == 0) {  	/* UNREGISTER */
	if (samp == 0) {
	    fprintf (stderr, "Samp Hub not connected\n");
	} else {
    	    if (samp_UnRegister (samp) != OK)
		fprintf (stderr, "UnRegistration fails\n");
    	    else {
		registered = 0;
		fprintf (stderr, "UnRegistration OK\n");
	    }
	}

    } else if (strcasecmp (method, "DeclareMetadata") == 0) {	/* METADATA */
	if (samp == 0) {
	    fprintf (stderr, "Samp Hub not connected\n");
	} else {
    	    if (samp_DeclareMetadata (samp) != OK)
		fprintf (stderr, "Metadata declaration fails\n");
    	    else
		fprintf (stderr, "Metadata declaration OK\n");
	}

    } else if (strcasecmp (method, "Ping") == 0) {		/* PING */
	if (nscan < 2) {
	    cmdUsage ("Ping appName");

	} else {
	    if ((samp_Ping (samp, arg1)) < 0)
		fprintf (stderr, "%s responds w/ ERR\n", arg1);
	    else
		fprintf (stderr, "%s responds OK [%d]\n", arg1, cmdNum++);
	}

    } else if (strcasecmp (method, "GetMetadata") == 0) {      /* GETMETADATA */
	if (nscan < 2) {
	    cmdUsage ("GetMetadata appName");
	} else {
    	    if ((resp = samp_GetMetadata (samp, arg1)) > 0) {
	        xr_getStringFromStruct (resp, "samp.name", &sres);
	            printf ("  Name  = '%s'\n", sres);
	        xr_getStringFromStruct (resp, "author.name", &sres);
	            printf (" Author = '%s'\n", sres);
	        xr_getStringFromStruct (resp, "samp.description.text", &sres);
	            printf ("  Descr = '%s'\n", sres);
	        xr_getStringFromStruct (resp, "samp.icon.url", &sres);
	            printf ("IconURL = '%s'\n", sres);
	        xr_getStringFromStruct (resp, "samp.documentation.url", &sres);
	            printf (" DocURL = '%s'\n", sres);
	    }
	}
	    
							     /* SUBSCRIPTIONS */
    } else if (strcasecmp (method, "DeclareSubscriptions") == 0) {
	if ((samp_DeclareSubscriptions (samp)) < 0)
	    fprintf (stderr, "DeclareSubscriptions responds w/ ERR\n");
	else
	    fprintf (stderr, "DeclareSubscriptions OK\n");

    } else if (strcasecmp (method, "GetSubscriptions") == 0) {
	Map subs = (Map) 0;

	if ((subs = samp_GetSubscriptions (samp, arg1)) > 0) {
	    /*  Need xmlrpc_struct_read_member() to scan map names ....
	     */
	}

							     /* CLIENTS       */
    } else if (strcasecmp (method, "GetRegisteredClients") == 0) {
	/*  Print the list of registered clients.
	*/
	List clients = samp_GetRegisteredClients (samp);

	for (i=0; i < samp_listLen (clients); i++)
	    printf ("%s\n", samp_getStringFromList (clients, i));
	samp_freeList (clients);

    } else if (strcasecmp (method, "GetSubscribedClients") == 0) {
	if (nscan < 2) {
	    cmdUsage ("GetSubscribedClients mtype");

	} else {
	    /*  Print the list of clients subscribed to a specific mtype.
	     */
	    List clients = samp_GetSubscribedClients (samp, arg1);

	    for (i=0; i < samp_listLen (clients); i++)
	        printf ("%s\n", samp_getStringFromList (clients, i));
	    samp_freeList (clients);
	}


			      /* table.load.votable (url, [table-id], [name]) */
    } else if (strcasecmp (method, "tableLoadVOTable") == 0) {
	if (nscan < 2) {
	    cmdUsage ("tableLoadVOTable appName url [table-id] [name]");
	} else {
	    if (samp_tableLoadVOTable (samp, arg1, arg2, arg3, arg4) == SAMP_OK)
	        printf ("OK\n");
	    else
	        printf ("Error: '%s'\n", samp_getErr (samp));
	}

			         /* table.load.fits (url, [table-id], [name]) */
    } else if (strcasecmp (method, "tableLoadFITS") == 0) {
	if (nscan < 2) {
	    cmdUsage ("tableLoadFITS appName url [table-id] [name]");
	} else {
	    if (samp_tableLoadFITS (samp, arg1, arg2, arg3, arg4) == SAMP_OK)
	        printf ("OK\n");
	    else
	        printf ("Error: '%s'\n", samp_getErr (samp));
	}

			    /* table.highlight.row (table-id, url, (int) row) */
    } else if (strcasecmp (method, "tableHighlightRow") == 0) {
	;

			/* table.highlight.rowList (table-id, url, (List)row) */
    } else if (strcasecmp (method, "tableSelectRowList") == 0) {
	;

				 /* image.load.fits (url, [table-id], [name]) */
    } else if (strcasecmp (method, "imageLoadFits") == 0) {
	if (nscan < 2) {
	    cmdUsage ("imageLoadFITS appName url [image-id] [name]");
	} else {
	    if (samp_imageLoadFITS (samp, arg1, arg2, arg3, arg4) == SAMP_OK)
	        printf ("OK\n");
	    else
	        printf ("Error: '%s'\n", samp_getErr (samp));
	}

				 /* coord.pointAt.sky ((float)ra, (float)dec) */
    } else if (strcasecmp (method, "coordPointAtSky") == 0) {
	;

	/* spectrum.load.ssa-generic (url, (Map)meta, [spectrum-id], [name])  */
    } else if (strcasecmp (method, "specLoadSSAGeneric") == 0) {
	;

						     /* send (appName, mtype) */
    } else if (strcasecmp (method, "send") == 0) {
	;

    } else if (method[0])
	fprintf (stderr, "Unknown command '%s'\n", method);


    return (0);
}


static void
cmdUsage (char *s)
{ 
    fprintf (stderr, "Usage:  %s\n", s);
}


static void
help_summary (void)
{ 
 fprintf (stderr, "Commands:\n"
  "    help			print help summary\n"
  "    quit			quit the task\n"
  "    start			start SAMP (connect to Hub)\n"
  "    stop			start SAMP (disconnect from Hub)\n"
  "    set <var> <value>		set variable\n"
  "    get <var>			get variable\n"
  "\n"
  "    notify			NYI\n"
  "    notifyAll			NYI\n"
  "    call			NYI\n"
  "    callAll			NYI\n"
  "    callAndWait			NYI\n"
  "\n"
  "    Register <name> <descr>	register with the hub\n"
  "    Unregister			unregister from hub\n"
  "    DeclareMetadata		NYI\n"
  "    Ping <appName>		ping named app\n"
  "    GetMetadata 		NYI\n"
  "    DeclareSubscriptions 	NYI\n"
  "    GetSubscriptions 		NYI\n"
  "    GetRegisteredClients 	NYI\n"
  "\n"
  "    tableLoadVotable 		NYI\n"
  "    tableLoadFits 		NYI\n"
  "    tableHighlightRow 		NYI\n"
  "    tableSelectRowList 		NYI\n"
  "    imageLoadFits 		NYI\n"
  "    coordPointAtSky 		NYI\n"
  "    specLoadSSAGeneric 		NYI\n"
 );
}


static void
print_result (handle_t samp, int stat)
{
    if (stat == SAMP_OK)
	printf ("OK\n");
    else
	printf ("Error: '%s'\n", samp_getErr (samp));
}


static int
make_msg (char *mtype)
{
    static Map msg = 0, param = 0;
    
    if (msg == 0) {
	msg = samp_newMsg ();
	param = samp_newMsg ();
    }

    samp_msgMType (msg, mtype);
    samp_msgParam (msg, param);

    return (msg);
}

