/* A simple synchronous XML-RPC client written in C. */

#include <stdlib.h>
#include <stdio.h>

#include <xmlrpc-c/base.h>
#include <xmlrpc-c/client.h>

#include "config.h"  /* information about this build environment */

#define NAME "XML-RPC C Test Client synch_client"
#define VERSION "1.0"

static void
die_if_fault_occurred(xmlrpc_env * const envP) {
    if (envP->fault_occurred) {
        fprintf(stderr, "XML-RPC Fault: %s (%d)\n",
                envP->fault_string, envP->fault_code);
        exit(1);
    }
}


#define ORIG

int 
main(int           const argc, 
     const char ** const argv ATTR_UNUSED) {

    xmlrpc_env env;
    xmlrpc_value  *resultP;
    xmlrpc_client *rpc_client;
    const char    *stateName;
    int   val;
    static int count = 8;


    /* Start up our XML-RPC client library. 
     */
    xmlrpc_client_init(XMLRPC_CLIENT_NO_FLAGS, NAME, VERSION);

  while (count < 2000) {

    xmlrpc_client_setup_global_const (&env);
    xmlrpc_client_create (&env, XMLRPC_CLIENT_NO_FLAGS, "client", "v1.0",
                          NULL, 0, &rpc_client);

    xmlrpc_env_init(&env); 		/* init our error-handling env	*/


    					/* call the server 		*/
#ifdef ORIG
    resultP = xmlrpc_client_call(&env, "http://tucana.tuc.noao.edu:3000/RPC2",
                                 "ping", "(i)", (xmlrpc_int32) count);
    die_if_fault_occurred (&env);

#else
   {
     xmlrpc_server_info *info =
          xmlrpc_server_info_new (&env, "http://tucana.tuc.noao.edu:3000/RPC2");
     xmlrpc_value *param = xmlrpc_array_new (&env);
     xmlrpc_value *v     = xmlrpc_int_new (&env, count);

     xmlrpc_array_append_item (&env, param, v);

fprintf (stderr, "4\n");
     xmlrpc_env_init(&env);
/*
     xmlrpc_client_call2 (&env, rpc_client, info, "ping", param, &resultP);
     die_if_fault_occurred (&env);
*/
fprintf (stderr, "5\n");
     xmlrpc_DECREF (v);
   }
#endif
    

    /* Read return value.  */
fprintf (stderr, "6  0x%x\n", (int) resultP);
    xmlrpc_read_int (&env, resultP, &val);
    die_if_fault_occurred (&env);
    printf ("%d %d\n", val, count++);
   
fprintf (stderr, "7\n");
    /* Dispose of our result value. */
    xmlrpc_DECREF (resultP);


    /* Clean up our error-handling environment. */
    xmlrpc_env_clean (&env);
    die_if_fault_occurred (&env);
    xmlrpc_env_init(&env);

/*
*/
    xmlrpc_client_destroy (rpc_client);
    die_if_fault_occurred (&env);

    xmlrpc_client_teardown_global_const ();
    die_if_fault_occurred (&env);
  }
    
    /* Shutdown our XML-RPC client library. */
    xmlrpc_client_cleanup();

    return 0;
}
