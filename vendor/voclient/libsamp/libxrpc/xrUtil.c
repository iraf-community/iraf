/** 
 *  XRUTIL.C
 *
 *  Utility procedures.
 *
 *
 *  @brief      Utility procedures.
 *
 *  @file       xrUtil.c
 *  @author     Mike Fitzpatrick
 *  @date       6/10/09
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <signal.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include <xmlrpc-c/base.h>
#include <xmlrpc-c/client.h>
#include <xmlrpc-c/server.h>
#include <xmlrpc-c/server_abyss.h>

#include "xrpc.h"


extern	int  client_verbose;




/** ************************************************************************
 *  SIGTERM HANDLERS -- Setup the SIGTERM handler.
 */
xmlrpc_server_abyss_t *serverToTerminateP;

void
xr_setupSigtermHandler (xmlrpc_server_abyss_t *serverP)
{
    struct sigaction mysigaction;

    sigemptyset (&mysigaction.sa_mask);
    mysigaction.sa_flags = 0;
    mysigaction.sa_handler = xr_svrSigtermHandler;
    sigaction (SIGTERM, &mysigaction, NULL);
}

void
xr_svrSigtermHandler (int signalClass)
{
    xmlrpc_env env;

    xmlrpc_env_init (&env);
    xmlrpc_server_abyss_terminate (&env, serverToTerminateP);
    xr_dieIfFailed ("xmlrpc_server_abyss_terminate", env);

    xmlrpc_env_clean (&env);
}

void
xr_restoreSigtermHandler (void)
{
    struct sigaction mysigaction;

    sigemptyset (&mysigaction.sa_mask);
    mysigaction.sa_flags = 0;
    mysigaction.sa_handler = SIG_DFL;
    sigaction (SIGTERM, &mysigaction, NULL);
}



/** ************************************************************************
 *  SIGPIPE HANDLERS -- Setup the SIGPIPE handlers.
 */
void
xr_setupSigpipeHandlers (void)
{
    struct sigaction mysigaction;

    sigemptyset (&mysigaction.sa_mask);
    mysigaction.sa_flags = 0;
    mysigaction.sa_handler = SIG_IGN;
    sigaction (SIGPIPE, &mysigaction, NULL);
}



/** ************************************************************************
 *  Utility procedures.
 */

/**
 *  XR_GETPEERIPADDR -- Get the IP addr of the host on the server channel.
 */
char *
xr_getPeerIpAddr (TSession * const abyssSessionP)
{
    struct abyss_unix_chaninfo * channelInfoP;
    struct sockaddr_in * sockAddrInP;
    static unsigned char *ipAddr, buf[32];


    SessionGetChannelInfo (abyssSessionP, (void*)&channelInfoP);

    sockAddrInP = (struct sockaddr_in *) &channelInfoP->peerAddr;
    ipAddr      = (unsigned char *)&sockAddrInP->sin_addr.s_addr;

    memset (buf, 0, 32);
    sprintf ((char *)buf, "%u.%u.%u.%u", 
	(unsigned int)ipAddr[0], (unsigned int)ipAddr[1], 
	(unsigned int)ipAddr[2], (unsigned int)ipAddr[3]);

    return ((char *)buf);
}


void
xr_dieIfFailed (char *description, xmlrpc_env env)
{
    if (env.fault_occurred) {
        fprintf (stderr, "%s failed. %s\n", description, env.fault_string);
        exit (1);
    }
}


void die_on_error (xmlrpc_env *env) 
{
    if (env->fault_occurred) {
        fprintf(stderr, 
	    "XML-RPC Fault: %s (%d)\n", env->fault_string, env->fault_code);
        exit(1);
    }
}


void warn_on_error (xmlrpc_env *env) 
{
    if (env->fault_occurred && client_verbose)
        fprintf(stderr, 
	    "XML-RPC Fault: %s (%d)\n", env->fault_string, env->fault_code);
}


void
xr_dbgPrintParams ( xmlrpc_server_abyss_parms s )
{
    fprintf (stderr, "Server Params:\n");
    fprintf (stderr, "\tport = %d\n", s.port_number);
    fprintf (stderr, "\tlogfile = %s\n", s.log_file_name);
    fprintf (stderr, "\tkeepalive_timeout = %d\n", s.keepalive_timeout);
    fprintf (stderr, "\tkeepalive_max_conn = %d\n", s.keepalive_max_conn);
    fprintf (stderr, "\ttimeout = %d\n", s.timeout);
}

