/*  
**  XRPC.h -- Private include file for the XRPC interface.
*/


#define SZ_LINE         	128
#define SZ_PATH         	128
#define SZ_CMD          	32
#define SZ_SIG          	64
#define SZ_NAME          	64

#define	MAX_CLIENTS		512	/* max clients or async msgs	*/
#define	MAX_STRUCTS		32768
#define	MAX_ARRAYS		32768

#define	OK			0
#define ERR			1

#define TY_INT                  1
#define TY_DOUBLE               2
#define TY_BOOL                 3
#define TY_STRING               4
#define TY_DATETIME             5
#define TY_STRUCT               6
#define TY_ARRAY                7

#define	XR_NAME			"xrLib"
#define	XR_VERSION		"v1.0"



/**
 *  Type for pointer to function return integer.
 */
typedef int (*PFI)();               	/* ptr to func returning an int */


/**
 *  XML-RPC method data struct.
 */
typedef struct {
    char   name[SZ_PATH];               /** method name                  */
    char   arg_signature[SZ_SIG];       /** argument signature           */
    char   ret_signature[SZ_SIG];       /** return signature             */

    int   (*methodFunc)(void *p);      	/** function to call             */
    void  *serverInfo;         		/** user data                    */

    void  *next;                        /** list pointer                 */

} Method, *MethodP;


/**
 *  XML-RPC caller data struct.
 */
typedef struct {
    xmlrpc_env   *env;			/** calling environment		 */
    char         *host;			/** caller host name (not used)  */
    char         *name;			/** method name			 */
    xmlrpc_value *param;		/** calling parameter array	 */
    void         *info;			/** user data ('svr' pointer)	 */
    xmlrpc_value *result;		/** result array		 */
 
    int   (*handlerFunc)(void *p);     	/** asynch handler function      */
    int		 nparams;		/** number of calling params	 */
    int		 nresults;		/** number of result values	 */

    int		 client_num;		/** client number		 */
    int		 rpc_shutdown;		/** shutdown request		 */

} Caller, *CallerP;


/**
 *  XML-RPC server data struct.
 */
typedef struct {
    int    port;                        /** server port numnber          */
    char   path[SZ_PATH];               /** path e.g. "/RPC2"            */
    char   url[SZ_PATH];                /** full server URL              */
    char   config[SZ_PATH];             /** server config file           */
    char   logfile[SZ_PATH];            /** server log file              */


    /* Runtime structure
    */
    Method *method_head;                /** server method list           */
    Method *method_tail;                /** server method tail           */
    int    num_methods;                 /** number of methods            */

    Caller caller;			/** method calling struct	 */

    pthread_attr_t attr;                /** server thread attribute      */
    pthread_t     thread;               /** server thread                */

    xmlrpc_default_method  defMethod;   /** dispatcher method            */

    xmlrpc_server_abyss_parms serverparm;
    xmlrpc_registry     *registry;
    xmlrpc_env          env;

    int     shutdown;                   /** shutdown requested           */
    int     trace;                      /** trace execution?             */

} Server, *ServerP;


/**
 *  XML-RPC client data struct.
 */
typedef struct {
    char   url[SZ_PATH];                /** URL to service 		 */
    xmlrpc_client *rpc_client;		/** client struct		 */
    xmlrpc_env     env;			/** env context			 */

    int    in_use;			/** struct in use?		 */

    int   (*handlerFunc)(void *p);     	/** asynch handler function      */
    xmlrpc_value  *param;		/** method parameters		 */
    xmlrpc_value  *result;		/** result values		 */

    int    faultCode;                   /** error code 			 */
    char   faultString[SZ_PATH];        /** error string 		 */

} Client, *ClientP;




/* Private function prototypes.
*/
void 	die_on_error (xmlrpc_env *env);
void 	warn_on_error (xmlrpc_env *env);


#include "xrpc.h"


