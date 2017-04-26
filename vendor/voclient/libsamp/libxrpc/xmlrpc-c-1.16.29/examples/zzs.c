/* A simple standalone XML-RPC server written in C. */

#include <stdlib.h>
#include <stdio.h>
#ifdef WIN32
#  include <windows.h>
#else
#  include <unistd.h>
#endif

#include <xmlrpc-c/base.h>
#include <xmlrpc-c/server.h>
#include <xmlrpc-c/server_abyss.h>

#include "config.h"  /* information about this build environment */


#ifdef WIN32
  #define SLEEP(seconds) SleepEx(seconds * 1000, 1);
#else
  #define SLEEP(seconds) sleep(seconds);
#endif


static xmlrpc_value *
ping(xmlrpc_env *   const envP,
           xmlrpc_value * const paramArrayP,
           void *         const serverInfo ATTR_UNUSED,
           void *         const channelInfo ATTR_UNUSED) {

    xmlrpc_int32 x, y, z;
    static long count = 0;


    /* Parse our argument array. */
    xmlrpc_decompose_value(envP, paramArrayP, "(i)", &x);
    if (envP->fault_occurred)
        return NULL;

    /* Add our two numbers. */
    z = 0;

    /* Sometimes, make it look hard (so client can see what it's like
       to do an RPC that takes a while).
    if (y == 1)
        SLEEP(3);
    */
    fprintf (stderr, "count = %ld\n", (long) count++);

    /* Return our result. */
    return xmlrpc_build_value(envP, "i", z);
}



int 
main(int           const argc, 
     const char ** const argv) {

    struct xmlrpc_method_info3 const methodInfo = {
        /* .methodName     = */ "ping",
        /* .methodFunction = */ &ping,
    };
    xmlrpc_server_abyss_parms serverparm;
    xmlrpc_registry * registryP;
    xmlrpc_env env;

    
    xmlrpc_env_init(&env);
    registryP = xmlrpc_registry_new(&env);

    xmlrpc_registry_add_method3(&env, registryP, &methodInfo);

    /* In the modern form of the Abyss API, we supply parameters in memory
       like a normal API.  We select the modern form by setting
       config_file_name to NULL: 
    */
    serverparm.config_file_name = NULL;
    serverparm.registryP        = registryP;
    serverparm.port_number      = 3000;
    serverparm.log_file_name    = "/tmp/xmlrpc_log";

    printf("Running XML-RPC server...\n");

    xmlrpc_server_abyss(&env, &serverparm, XMLRPC_APSIZE(log_file_name));

    /* xmlrpc_server_abyss() never returns */

    return 0;
}
