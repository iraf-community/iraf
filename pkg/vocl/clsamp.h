/**
 *  CLSAMP.H -- Definition for the SAMP/CL interface.
 */

#include "proto.h"
#include "sampDecl.h"


#define MAX_HANDLERS            32      /* max user-defined handlers    */

typedef struct {
    char   mtype[SZ_FNAME];             /* message type string          */
    char   cmd[SZ_FNAME];               /* message handler command      */
} Handler, *HandlerP;



/*  samp.c
 */
int 	cl_sampStart ();
int 	cl_sampStop ();

void 	cl_Samp (void);

void 	sampio_handler (int signum);
int 	samp_rl_hook (void);
int 	get_samp_command (char *cmdbuf, int maxch);
int 	sampop (int opcode, int op_index, int nargs);


/*  sampCmd.c
 */
int 	cmd_sampDbg (int nargs);
int 	cmd_sampAddHandler (int nargs);
int 	cmd_sampAccess (int nargs);
int 	cmd_sampMetadata (int nargs);
void 	cmd_sampRestart (void);
void 	cmd_sampStart (void);
void 	cmd_sampStop (void);

int 	cmd_sampExec (int nargs);
char   *cmd_sampEnvGet (int nargs);
int 	cmd_sampEnvSet (int nargs);
char   *cmd_sampParamGet (int nargs);
int 	cmd_sampParamSet (int nargs);

int 	cmd_sampSend (int nargs);
int 	cmd_sampLoadImage (int nargs);
int 	cmd_sampLoadFITS (int nargs);
int 	cmd_sampLoadVOTable (int nargs);

int 	cmd_sampShowRow (int nargs);
int 	cmd_sampSelectRowList (int nargs);
int 	cmd_sampPointAt (int nargs);
int 	cmd_sampSpecLoad (int nargs);


/*  sampFuncs.c
 */
void 	func_sampDbg (void);
void 	func_sampStatus (int nargs);
void 	func_sampHubAccess (int nargs);
void 	func_sampAccess (int nargs);
void 	func_sampMetadata (int nargs);
void 	func_sampRestart (void);
void 	func_sampStart (void);
void 	func_sampStop (void);
void 	func_sampSend (void);
void 	func_sampAddHandler (int nargs);
void 	func_sampLoadImage (int nargs);
void 	func_sampLoadFITS (int nargs);
void 	func_sampLoadVOTable (int nargs);

void 	func_sampPointAt (int nargs);
void 	func_sampShowRow (int nargs);
void 	func_sampSelectRowList (int nargs);
void 	func_sampSpecLoad (int nargs);
void 	func_sampBibcodeLoad (int nargs);


/* sampHandlers.c
 */
int 	cl_addUserHandler (char *mtype, char *cmd);
int 	cl_delUserHandler (char *mtype);
char   *cl_getUserHandler (char *mtype);

int 	cl_genericHandler (char *sender, char *mtype, char *msg_id, Map map);
int 	cl_cmdExecHandler (char *cmd);
int 	cl_envSetHandler (char *name, char *value);
int 	cl_envGetHandler (char *name, char *value, int maxch);
int 	cl_paramSetHandler (char *name, char *value);
int 	cl_paramGetHandler (char *name, char *value, int maxch);
int 	cl_pingHandler (char *sender);
int 	cl_imgLoadHandler (char *url, char *imgId, char *name);
int 	cl_tblLoadHandler (char *url, char *tblId, char *name);

void 	str_replace (char **string, char *substr, char *replacement );
int 	is_stdMType (char *mtype);


