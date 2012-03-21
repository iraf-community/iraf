/* A simple standalone XML-RPC server based on Abyss that contains a
   simple one-thread request processing loop.  

   xmlrpc_sample_add_server.c is a server that does the same thing, but
   does it by running a full Abyss daemon in the background, so it has
   less control over how the requests are served.
*/

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include <xmlrpc-c/base.h>
#include <xmlrpc-c/abyss.h>
#include <xmlrpc-c/server.h>
#include <xmlrpc-c/server_abyss.h>

#include "config.h"  /* information about this build environment */

#define	DEBUG		0



/** ************************************************************************ **/

xmlrpc_value *
test_ping(xmlrpc_env *const envP, xmlrpc_value *const paramArrayP,
           void *const serverInfo, void *const channelInfo)
{
    static long count = 0;

    fprintf (stderr, "count = %ld\n", (long) count++);
    return xmlrpc_build_value(envP, "i", 0);
}


int 
main(int           const argc, 
     const char ** const argv) {

    struct xmlrpc_method_info3 const methodInfo = {
        /* .methodName      	= */ "ping",
        /* .methodFunction  	= */ &test_ping,
    };
    TServer abyssServer;
    xmlrpc_registry * registryP;
    xmlrpc_env env;
    int terminationRequested;  /* A boolean value */
    const char * error;


    AbyssInit (&error);
    
    xmlrpc_env_init (&env);
    registryP = xmlrpc_registry_new (&env);

    xmlrpc_registry_add_method3 (&env, registryP, &methodInfo);


    xmlrpc_registry_set_shutdown (registryP,
                                 &xr_requestShutdown, &terminationRequested);

    ServerCreate (&abyssServer, "XmlRpcServer", 3000, NULL, NULL);
    
    xmlrpc_server_abyss_set_handlers2 (&abyssServer, "/RPC2", registryP);

    ServerInit (&abyssServer);

    xr_setupSigHandlers ();

    terminationRequested = 0;

    while (!terminationRequested) {
        printf ("Waiting for next RPC...\n");

        /*  This waits for the next connection, accepts it, reads the
         *  HTTP POST request, executes the indicated RPC, and closes
         *  the connection.
         */
        ServerRunOnce (&abyssServer);
    }

    ServerFree (&abyssServer);

    AbyssTerm ();

    return 0;
}
