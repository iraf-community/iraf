/**
 *  XRSERVER.C
 *
 *  Procedures used to implement an XML-RPC server in an application.  To 
 *  do this, we keep a local registry of method names and functions and 
 *  rely on a default method to invoke the procedure using a common function 
 *  prototype.
 * 
 *  Server methods:
 * 
 *                   xr_createServer  (path, port, logfile)
 *                xr_addServerMethod  (name, *method, *userData)
 *             xr_removeServerMethod  (name)
 *                 xr_setServerParam  (param, *value)
 * 
 *              xr_startServerThread  ()		    // never returns
 *                 xr_shutdownServer  ()
 *
 *
 *  @brief      Procedures used to implement an XML-RPC server.
 *
 *  @file       xrServer.c
 *  @author     Mike Fitzpatrick
 *  @date       6/10/09
 */


#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <signal.h>
#include <string.h>

#include <xmlrpc-c/base.h>
#include <xmlrpc-c/client.h>
#include <xmlrpc-c/server.h>
#include <xmlrpc-c/server_abyss.h>
#include <xmlrpc-c/abyss.h>

#include "xrpcP.h"

#define	 DEBUG			0
#define	 SRVR_DEBUG		0

/*
#define GLOBAL_ABYSS_SERVER	1
#define SINGLE_RUN_SERVER	1
*/
#define NEW_ABYSS_SERVER	1

#define SZ_CALL_RING		256


int  	res_anum = -1;			/* result array num		*/
int	res_snum = -1;			/* result struct num		*/
int     svr_done = 0;			/* shutdown requested		*/


static Caller cs[SZ_CALL_RING];
static int cindex = 0;

static pthread_mutex_t svr_mutex = PTHREAD_MUTEX_INITIALIZER;

static void *xr_rpcListener (Server *svr);

static xmlrpc_value *xr_defaultMethod (xmlrpc_env *envP, char *host, 
			    char *methodName, xmlrpc_value *paramArrayP, 
			    void *serverInfo);

#ifdef USE_REQUEST_SHUTDOWN
static xmlrpc_server_shutdown_fn xr_requestShutdown;
static void  xr_requestShutdown (xmlrpc_env *const faultP, void *const context,
                const char *const comment, void *const callInfo);
#endif

static ServerP  svr = (Server *) NULL;



/*************************************************************************
** CREATESERVER - Create an instance of the RPC server.
*************************************************************************/

int
xr_createServer (char *path, int port, char *logfile)
{
	
    if (svr) {
	perror ("createServer:  Server already allocated");
	return (ERR);
    }
    if (! (svr = (ServerP) calloc ((size_t) 1, sizeof (Server)))) {
	perror ("createServer:  Cannot allocate server");
	return (ERR);
    }


    /* Initialize the structure.
    */
    strcpy (svr->path, (path ? path : ""));
    strcpy (svr->logfile, (logfile ? logfile : ""));
    svr->port = port;


    /* Initialize XML-RPC interface.
    */
    xmlrpc_env_init (&svr->env);             
    bzero (svr->url, SZ_PATH);
    sprintf (svr->url, "http://localhost:%d/RPC2", port);


    /* Create the service registry and initialize server params.
    */
    svr->registry = xmlrpc_registry_new (&svr->env);


    /* Set the default shutdown method.
    xmlrpc_registry_set_shutdown (svr->registry, &xr_requestShutdown,
	&shutdown);
    */


    /* In the "modern" form of the Abyss API, we supply parameters in memory
    ** like a normal API.  We select the modern form by setting
    ** config_file_name to NULL: 
    */
    svr->serverparm.config_file_name 	= NULL;
    svr->serverparm.registryP        	= svr->registry;
    svr->serverparm.port_number      	= svr->port;
    if (svr->logfile[0])
        svr->serverparm.log_file_name	= svr->logfile;

    svr->trace = 0;


    return (OK);
}



/*************************************************************************
** ADDSERVERMETHOD - Add a callback method to the RPC server.
*************************************************************************/

int
xr_addServerMethod (char *name, void *method, void *userData)
{
    char *arg_signature, *ret_signature;


    if (DEBUG)
	fprintf (stderr, "addMethod: '%s'\n", name);

    /* Create a method in the server.  We also use this to let the 
    ** user set the default and shutdown methods.
    */
    if (strcmp (name, "default") == 0) {

	/* Set the default method.
	*/
	xmlrpc_registry_set_default_method (&svr->env, svr->registry, 
	    (xmlrpc_default_method) &method, NULL);

    } else if (strcmp (name, "shutdown") == 0) {
	int shutdown;

        xmlrpc_registry_set_shutdown (svr->registry, method, &shutdown);

    } else {

	/*  All we really do at this stage is register the method with
	**  the interface registry.  When a method is called, the default
	**  method is responsible for parsing the arguments and invoking
	**  the client code using the standard prototype.
	*/ 
	MethodP	m = calloc (1, sizeof (Method));

	if (svr->method_head == (MethodP) NULL) {
	    /* Initialize the list of methods;
	    */
	    svr->method_head = svr->method_tail = m;

	} else {
	    /* Add the mthod to the tail of the list.
	    */
	    svr->method_tail->next = svr->method_tail = m;
	}

	/* Save information about the method.  We force the argument 
	** signature to always be an array to simply the process of getting
	** the parameters in the method.  On return, we use the signature
	** provided by the user and rely on the implementation to extract
	** the results appropriately.
	**
	*/
	arg_signature = ret_signature = "not_currently_used";
	strcpy (m->name, name);
	strcpy (m->ret_signature, ret_signature);
        if (arg_signature[0] != '(') {
            sprintf (m->arg_signature, "(%s)", arg_signature);
        } else 
            strcpy (m->arg_signature, arg_signature);


	m->methodFunc = method;
	m->serverInfo = userData;

	svr->num_methods++;
    }

    return (OK);
}


/*************************************************************************
** REMOVESERVERMETHOD - Delete a callback method to the RPC server.
*************************************************************************/

int
xr_removeServerMethod (char *name)
{
    MethodP last = (MethodP) NULL;
    MethodP m = svr->method_head;

    while (m) {
	if (strcmp (m->name, name) == 0) {
	    if (last) 				/* drop method from the list  */
		last->next = m->next;
	    else
		svr->method_head = m->next;

	    free ((void *) m);			/* free the struct	*/
	    if (svr->num_methods > 0)
	        svr->num_methods--;

	    return (OK);
	}
	last = m;
	m = m->next;
    }

    return (ERR);
}


/*************************************************************************
** SETSERVERPARAM - Set a parameter for the RPC server.
*************************************************************************/

void
xr_setServerParam (char *param, void *value)
{
    xmlrpc_env env;                                 /* local env            */
    xmlrpc_value *t = xmlrpc_bool_new (&env, (xmlrpc_bool) 1);


    if (strcmp (param, "port") == 0)
	svr->serverparm.port_number = (unsigned int) value;

    else if (strcmp (param, "logfile") == 0)
	svr->serverparm.log_file_name = (char *) value;

    else if (strcmp (param, "keepalive_timeout") == 0)
	svr->serverparm.keepalive_timeout = (unsigned int) value;

    else if (strcmp (param, "keepalive_max_conn") == 0)
	svr->serverparm.keepalive_max_conn = (unsigned int) value;

    else if (strcmp (param, "timeout") == 0)
	svr->serverparm.timeout = (unsigned int) value;

    else if (strcmp (param, "shutdown") == 0)
	svr->serverparm.enable_shutdown = (xmlrpc_bool) t;

    else if (strcmp (param, "trace") == 0)
	svr->trace = *((int *)value);
}


/*************************************************************************
** STARTSERVERTHREAD -- Start the server on a separate thread.
*************************************************************************/

pthread_t
xr_startServerThread ()
{
    /* Create a detatched thread in which to run.
    */
    pthread_attr_init (&svr->attr);
    pthread_attr_setdetachstate (&svr->attr, PTHREAD_CREATE_DETACHED);

    if (svr->trace)
        fprintf (stderr, "Starting server thread on port %d....\n", svr->port);

    /* Start the listener thread, i.e. the XML-RPC "server".
    */
    svr->thread = 0;
    if (pthread_create (&svr->thread, NULL, (void *)xr_rpcListener,
	(void *) svr)) {
            perror ("Cannot start listener thread");
            exit (-1);
    }

    return (svr->thread);
}


void
xr_startServer ()
{
    pthread_t tid = 0;
    sigset_t mask, oldmask;


    tid = xr_startServerThread ();

    /*  Set up the mask of signals to temporarily block. 
     */
    sigemptyset (&mask);
    sigaddset (&mask, SIGUSR1);
    sigaddset (&mask, SIGUSR2);
     
    /*  Wait for a shutdown signal to arrive. 
     */
    pthread_sigmask (SIG_BLOCK, &mask, &oldmask);
    while (! svr->shutdown )
        sigsuspend (&oldmask);
    pthread_sigmask (SIG_UNBLOCK, &mask, NULL);

    pthread_kill (svr->thread, SIGTERM);
}


/* ***********************************************************************
** SHUTDOWNSERVER -- Stop the RPC server.
*************************************************************************/

int
xr_shutdownServer ()
{
    /* Not yet implemented.  */
    if (! svr) {
	perror ("destroyServer:  Server already allocated");
	return (ERR);
    }

    free ((void *) svr);		/* free the server struct	*/
    svr = (ServerP) NULL;

    return (OK);
}

void
xr_setShutdownLevel ( int level )
{
    svr->shutdown = svr_done = level;
}



/************************************************************************ */
/*  PRIVATE METHODS							  */
/************************************************************************ */

/**
 *  XR_REQUESTSHUTDOWN -- Issue a shutdown request.  You make this run by 
 *  executing the system method 'system.shutdown'.
 */
#ifdef USE_REQUEST_SHUTDOWN
static void
xr_requestShutdown (xmlrpc_env * const faultP,
                    void       * const context,
                    const char * const comment,
                    void       * const callInfo) 
{
    int *terminationRequestedP = context;
    pid_t  ppid = getppid ();

    xmlrpc_env_init(faultP);
    *terminationRequestedP = 1;

    /* Send parent a signal we're shutting down.
     */
    if (ppid)
       kill (ppid, SIGUSR1);
}
#endif


/**
 *  XR_REQUESTABORT -- Issue an abort request.  You make this run by executing
 *  the local system method 'sys.abort'.
 */
int
xr_requestAbort (void *data)
{
    pid_t  ppid = getppid ();

    /* Send parent a signal we're shutting down.
     */
    if (ppid)
       kill (ppid, SIGUSR2);

    xr_setIntInResult (data, (int) OK);  /* set result      */

    return (OK);
}


/* The method to be called in the server.
*/

static xmlrpc_value *
xr_defaultMethod (xmlrpc_env *envP, char *host, char *methodName,
               xmlrpc_value *paramArrayP, void *serverInfo)
{
    int      status;
    ServerP  svr = (Server *) serverInfo; 
    MethodP  m   = (Method *) NULL;

    extern int  xr_errstat;



    if (DEBUG)
        fprintf (stderr, "DEFAULT:  method='%s' data= 0x%lx\n",
	    methodName, (long) serverInfo);
        
        
    /* Now look for the method in the interface registry.
    */
    for (m=svr->method_head; m; m = m->next) {

	if (strcmp (m->name, methodName) == 0) {

	    Caller *c = (Caller *) NULL;
	    xmlrpc_value *result = (xmlrpc_value *) NULL;


	    (void) pthread_mutex_lock (&svr_mutex);
	    c = &cs[(cindex = (cindex + 1) % SZ_CALL_RING)];
	    (void) pthread_mutex_unlock (&svr_mutex);

#ifdef FREE_RES
	    /*  Free old result values.
	     */
	    if (res_anum >= 0) {
		xr_freeArray (res_anum);
		res_anum = -1;
	    }
	    if (res_snum >= 0) {
		xr_freeStruct (res_snum);
		res_snum = -1;
	    }
#endif

	    memset (c, 0, sizeof(Caller));
	    c->env   = envP; 		/* setup the calling parameters */
	    c->host  = host;
	    c->name  = methodName;
	    c->param = paramArrayP;
	    c->info  = serverInfo;


	    /* Call the function.
	    */
	    if ( (status = (*(PFI)(*m->methodFunc))((void *)c)) ) {
		fprintf (stderr, "Match failed '%s'...\n", m->name);
		xr_errstat = ERR;
	    } else {
	        xr_errstat = OK;

		/*  FIXME -- paramArrayP is freed in caller .....
	  	if (paramArrayP)
		    xmlrpc_DECREF(paramArrayP);
		*/
	        result = (c->result ? c->result : (xmlrpc_value *) NULL);;
	    }

	    if (result)
	        xmlrpc_INCREF(result);
    	    return ( result ); 			/* return our result	*/
	}
    }


    if (!m) {
	char  msg[256];

	memset (msg, 0, 256);
	sprintf (msg, "No such method '%s'", methodName);
	xmlrpc_value *result = xmlrpc_string_new (envP, msg);

        return ( result );

    } else {
        return ((xmlrpc_value *) NULL);		/* should never happen	*/
    }
}



/****************************************************************************/


#ifdef GLOBAL_ABYSS_SERVER

/*****************************************************************************
 *  Original stab at the Abyss server implementation
 ****************************************************************************/

/**
 * Thread process created to run a  listener for the methods being called.
 */
static void *
xr_rpcListener (Server *svr)
{
    xmlrpc_server_abyss_parms serverparm;
    

    /*  Initialize the environment and install the default dispatcher method.
    */
    xmlrpc_env_init (&svr->env);	/* initialize XML-RPC interface */

    if (SRVR_DEBUG)
	fprintf (stderr, "GLOBAL_ABYSS_SERVER rpcListener ....\n");
    svr->registry = xmlrpc_registry_new (&svr->env);
    xmlrpc_registry_set_default_method (&svr->env, 
	(svr->serverparm.registryP = svr->registry), 
        (xmlrpc_default_method) &xr_defaultMethod, svr);

    serverparm.config_file_name   = NULL;
    serverparm.registryP          = svr->serverparm.registryP;
    serverparm.port_number        = svr->serverparm.port_number;
    serverparm.log_file_name      = svr->serverparm.log_file_name;
    serverparm.keepalive_timeout  = 0;
    serverparm.keepalive_max_conn = 0;
    /*
    serverparm.keepalive_timeout  = 30;
    serverparm.keepalive_max_conn = 128;
    */


    /*  Never returns .....
    */
#ifndef OLD_METHOD
    xmlrpc_server_abyss (&svr->env, &serverparm, XMLRPC_APSIZE(log_file_name));

#else
  {
    xmlrpc_server_abyss_t   *serverP;
    xmlrpc_server_abyss_sig *oldHandlersP;

    xmlrpc_server_abyss_create (&svr->env, &serverparm, 
	XMLRPC_APSIZE(log_file_name), &serverP);
    xmlrpc_server_abyss_setup_sig (&svr->env, serverP, &oldHandlersP);

    printf ("Starting XML-RPC server...\n");

    xmlrpc_server_abyss_run_server (&svr->env,  &serverP);

    xmlrpc_server_abyss_restore_sig (oldHandlersP);
    free (oldHandlersP);
    xmlrpc_server_abyss_destroy (serverP);
    xmlrpc_server_abyss_global_term ();
  }
#endif


    /*  Should never get here.
     */
    if (svr->env.fault_occurred) {
        fprintf (stderr, "xmlrpc_server_abyss terminates.....\n");
        exit (1);
    } else
	return ((void *) OK);
}

#endif		/* GLOBAL_ABYSS_SERVER */


/****************************************************************************/


#ifdef NEW_ABYSS_SERVER

/*****************************************************************************
 *  For XMLRPC-C v1.14 or newer 
 ****************************************************************************/

/**
 * Thread process created to run a  listener for the methods being called.
 */
static void *
xr_rpcListener (Server *svr)
{
    xmlrpc_server_abyss_parms serverparm;
    xmlrpc_server_abyss_t    *serverP;
    xmlrpc_server_abyss_sig  *oldHandlersP;


    xmlrpc_env_init (&svr->env);

    if (SRVR_DEBUG)
	fprintf (stderr, "NEW_ABYSS_SERVER rpcListener ....\n");
    xmlrpc_server_abyss_global_init (&svr->env);
    xr_dieIfFailed ("xmlrpc_server_abyss_global_init", svr->env);
    
    svr->registry = xmlrpc_registry_new (&svr->env);
    xr_dieIfFailed ("xmlrpc_registry_new", svr->env);

    xmlrpc_registry_set_default_method (&svr->env, 
        (svr->serverparm.registryP = svr->registry), 
        (xmlrpc_default_method) &xr_defaultMethod, svr);

    serverparm.config_file_name   = NULL;
    serverparm.registryP          = svr->serverparm.registryP;
    serverparm.port_number        = svr->serverparm.port_number;
    serverparm.log_file_name      = svr->serverparm.log_file_name;
    /*
    serverparm.keepalive_timeout  = 15;
    serverparm.keepalive_max_conn = 4;

    xmlrpc_server_abyss_create (&svr->env, &serverparm, 
				XMLRPC_APSIZE(keepalive_max_conn), &serverP);
    */
    xmlrpc_server_abyss_create (&svr->env, &serverparm, 
				XMLRPC_APSIZE(log_file_name), &serverP);
    xr_dieIfFailed ("xmlrpc_server_abyss_create", svr->env);
    
    xmlrpc_server_abyss_setup_sig (&svr->env, serverP, &oldHandlersP);
    xr_dieIfFailed ("xmlrpc_server_abyss_setup_sig", svr->env);

    xr_setupSigtermHandler (serverP);

    /*  Launch the server.
     */  
    xmlrpc_server_abyss_run_server (&svr->env, serverP);
    xr_dieIfFailed ("xmlrpc_server_abyss_run_server", svr->env);


    /*  We should never get here ....
     */  
    fprintf (stderr, "Server has terminated\n");

    xr_restoreSigtermHandler ();
    xmlrpc_server_abyss_restore_sig (oldHandlersP);
    xmlrpc_server_abyss_destroy (serverP);
    xmlrpc_registry_free (svr->registry);
    xmlrpc_server_abyss_global_term ();
    xmlrpc_env_clean (&svr->env);

    return ((void *) ERR);;
}

#endif		/* NEW_ABYSS_SERVER */


/****************************************************************************/


#ifdef SINGLE_RUN_SERVER

/*****************************************************************************
 *  A simple standalone XML-RPC server based on Abyss that contains a
 *  simple one-thread request processing loop.
 ****************************************************************************/

xmlrpc_value *
test_ping(xmlrpc_env *const envP, xmlrpc_value *const paramArrayP,
           void *const serverInfo, void *const channelInfo)
{
    static long count = 0;

    fprintf (stderr, "svrping: count = %ld\n", (long) count++);

    sleep (30);
    return xmlrpc_build_value(envP, "i", 0);
}


#ifdef ZZ
#define	TraceMsg	printf
#define	TraceExit	printf

void
zz_xr_serverRunOnce (TServer * const serverP) {
/*----------------------------------------------------------------------------
   Accept a connection from the channel switch and do the HTTP
   transaction that comes over it.

   If no connection is presently waiting at the switch, wait for one.
   But return immediately if we receive a signal during the wait.
-----------------------------------------------------------------------------*/
    struct _TServer * const srvP = serverP->srvP;

    const char * error;
    TChannel *   channelP;
    void *       channelInfoP;

    extern void  ChanSwitchAccept ();
    extern void  ChannelFormatPeerInfo ();
    extern void  ServerSetKeepaliveMaxConn ();
    extern void  serverRunChannel ();
    extern void  xmlrpc_strfree ();


/*
    ServerSetKeepaliveMaxConn (serverP, 1);
    ChanSwitchAccept (srvP->chanSwitchP, &channelP, &channelInfoP, &error);
*/

    if (error) {
        TraceMsg ("Failed to accept the next connection from a client "
                  "at the channel level.  %s", error);
        xmlrpc_strfree (error);
    } else {

	/* Handle connection in the background.
    	 */
    	switch (fork ()) {
    	case 0:
            break; 				/* We are the child 	*/
    	default:
            return; 				/* We are the parent 	*/
    	}

        if (channelP) {
            const char * error;

            serverRunChannel (serverP, channelP, channelInfoP, &error);

            if (error) {
                const char * peerDesc;
                ChannelFormatPeerInfo (channelP, &peerDesc);
                TraceExit ("Got a connection from '%s', but failed to "
                           "run server on it.  %s", peerDesc, error);
                xmlrpc_strfree (peerDesc);
                xmlrpc_strfree (error);
            }
            ChannelDestroy (channelP);
            free (channelInfoP);
        }
    }

    exit (0);
}
#endif


/**
 *  Server methods to handle shutdown requests.
 *
typedef void (*sighandler_t)(int);
 */


void xr_sigShutdown (int sig) { svr->shutdown = 1; /* svr_done = 1; */ }
void xr_sigAbort    (int sig) { svr->shutdown = 2; /* svr_done = 2; */ }

void xr_setSigHandler (int sig, sighandler_t handler)
{
    struct sigaction usr_act;
    sigset_t block_mask;


    /*  Establish the shutdown request signal handlers.
     */
#ifdef USE_SIGNAL
    signal (signal, handler);
#else
    sigfillset (&block_mask);
    usr_act.sa_mask    = block_mask;
    usr_act.sa_flags   = 0;
    usr_act.sa_flags   = SA_RESTART;
    usr_act.sa_handler = handler; 

    sigaction (sig, &usr_act, NULL);
#endif
}

static void *
xr_rpcListener (Server *svr)
{
    struct xmlrpc_method_info3 const methodInfo[] = {
       /* .methodName   	.methodFunction  	*/ 
	{ "svrping", 		&test_ping,		},
	/*
	{ "sys.abort", 		&xr_requestAbort,	},
	{ "sys.shutdown", 	&xr_requestShutdown,	}
	*/
    };
    register int i = 0;
    TServer abyssServer;
    xmlrpc_registry * registryP;
    xmlrpc_env env;
    const char * error;

    extern void xr_serverRunOnce ();
    extern int  xr_requestAbort ();


    AbyssInit (&error);				/* initialize		*/
    xmlrpc_env_init (&env);
    registryP = xmlrpc_registry_new (&env);


    if (SRVR_DEBUG)
	fprintf (stderr, "single_run_server rpcListener ....\n");

    /*  Setup a test ping method and install the special shutdown handlers.
     */
    for (i=0; i < 1; i++)
    	xmlrpc_registry_add_method3 (&env, registryP, &methodInfo[i]);

    /*  Set default shutdown method.
     */
    xmlrpc_registry_set_shutdown (registryP, &xr_requestShutdown, &svr_done);
    xr_addServerMethod ("sys.abort", xr_requestAbort, NULL);


    /*  Set the default method we'll use to dispatch calls.
     */
    svr->serverparm.enable_shutdown = 
	(xmlrpc_bool) xmlrpc_bool_new (&env, (xmlrpc_bool) 1);
    xmlrpc_registry_set_default_method (&svr->env, 
        (svr->serverparm.registryP = svr->registry = registryP), 
        (xmlrpc_default_method) &xr_defaultMethod, svr);


    /*  Create the server instance.
     */
    ServerCreate (&abyssServer, "XmlRpcServer", 
	svr->serverparm.port_number, NULL, NULL);

    xmlrpc_server_abyss_set_handlers2 (&abyssServer, "/RPC2",
	svr->serverparm.registryP);

    ServerInit (&abyssServer);
    xr_setupSigpipeHandlers ();


    while (! svr_done ) {
        /*  This waits for the next connection, accepts it, reads the
         *  HTTP POST request, executes the indicated RPC, and closes
         *  the connection.
        ServerRunOnce (&abyssServer);
         */
        (void) xr_serverRunOnce (&abyssServer);
    }

    ServerFree (&abyssServer);			/* shut down		*/
    AbyssTerm ();

    return ((void *) ERR);
}

#endif		/* SINGLE_RUN_SERVER */
