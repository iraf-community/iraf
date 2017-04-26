/***************************************************************************
 *
 *  VOCLient Library --  Client interface library to the VOClient daemon
 *  application.  This API allows non-Java programs to make use of the  DAL
 *  client interface by means of a remote procedure call between this
 *  interface and the VOClient daemon.  State is maintained in the server and
 *  values passed back are language-neutral integer 'handles' on the remote
 *  objects, or simple int/double/string values.
 *
 *  All tasks must initialize the VO Client and establish a connection to
 *  the VOClient daemon by calling voc_initVOClient() before making any service
 *  connections, thereafter new connections may be opened/closed at will.
 *  Convenience procedures allow for easy use of specific services, e.g. Cone
 *  or Siap.  Service-specific parameters may be added to a query using the
 *  voc_add<type>Param() calls.  No action is taken until an execute of the
 *  query is performed, applications may get back a handle on the result and
 *  interrogate attributes directly, the raw VOTable or a CSV representation
 *  of the result may also be returned.
 *
 *  High-Level Functions:
 *  ---------------------
 *
 *               voc_initVOClient (config_opts)
 *              voc_closeVOClient (shutdown_flag)
 *              voc_abortVOClient (errcode, errmsg)
 * 
 *        string = voc_coneCaller (url, ra, dec, sr, otype)
 *  status = voc_coneCallerToFile (url, ra, dec, sr, otype, file)
 *        string = voc_siapCaller (url, ra, dec, rsize, dsize, fmt, otype)
 *  status = voc_siapCallerToFile (url, ra, dec, rsize, dsize, fmt, otype, file)
 * 
 *
 *  Interface Utility Procedures:
 *  ------------------------------
 * 
 *         flag = voc_validateObj (hcode)
 *		   voc_debugLevel (level)
 *		  voc_freePointer (ptr)
 *
 * 
 *  Main DAL Interface Procedures:
 *  ------------------------------
 *
 *       dal = voc_openConnection (svc_url, type)
 *   dal = voc_openConeConnection (svc_url)		    # Utility aliases
 *   dal = voc_openSiapConnection (svc_url)
 *            voc_closeConnection (dal)
 * 
 *    count = voc_getServiceCount (dal)
 *              voc_addServiceURL (dal, svc_url)
 *        url = voc_getServiceURL (dal, index)
 * 
 *           query = voc_getQuery (dal, type)
 *       query = voc_getConeQuery (dal, ra, dec, sr)
 *       query = voc_getSiapQuery (dal, ra, dec, ra_size, dec_size, format)
 *
 *         stat = voc_addIntParam (query, pname, ival)
 *       stat = voc_addFloatParam (query, pname, dval)
 *      stat = voc_addStringParam (query, pname, str)
 * 
 *   url_str = voc_getQueryString (query, type, index)
 *
 *          qr = voc_executeQuery (query)
 *      stat = voc_executeQueryAs (query, fname, type)
 *       csv_str = voc_executeCSV (query)
 *   vot_str = voc_executeVOTable (query)
 *
 *     count = voc_getRecordCount (qr)
 *            rec = voc_getRecord (qr, recnum)
 *
 *        attr = voc_getAttribute (rec, char *attrname)
 *       count = voc_getAttrCount (rec)                   
 *     list_str = voc_getAttrList (rec)                   
 *
 *            ival = voc_intValue (attr)
 *          dval = voc_floatValue (attr)
 *          str = voc_stringValue (attr)
 *
 *                 voc_setIntAttr (rec, attrname, ival)   (Not Yet Implemented)
 *               voc_setFloatAttr (rec, attrname, dval)   ( "   "      "      )
 *              voc_setStringAttr (rec, attrname, str)    ( "   "      "      )
 *
 *          stat = voc_getDataset (rec, acref, fname) 
 *
 *
 *  Sesame Name Resolver Interface:
 *  -------------------------------
 *
 *          sr = voc_nameResolver (target)
 *      pos_str = voc_resolverPos (sr)
 *         radeg = voc_resolverRA (sr)
 *       decdeg = voc_resolverDEC (sr)
 * 
 *
 *	Client programs may be written in any language that can interface to
 *  C code.  Sample programs using the interface are provided as is a SWIG
 *  interface definition file.  This inferface is based closely on the DAL
 *  client code produced for the 2005 NVOSS, as that interface evolves 
 * 
 *
 *  Michael Fitzpatrick, NOAO, June 2006
 *
 **************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>


#define _VOCLIENT_LIB_
#include "VOClient.h"


VOClient *vo = (VOClient *) NULL;	/* Interface runtime struct	*/

#define SVR_MAXTRY      	5
#define DEF_VOCSERVER_PORT	6200


typedef struct {
    char  server[SZ_FNAME];		/* VOClient server daemon	*/
    char  proxy[SZ_FNAME];		/* VOClient server proxy	*/
    char  config[SZ_FNAME];		/* VOClient config file		*/
    char  path[SZ_FNAME];		/* VOClientd path		*/
    char  runid[SZ_FNAME];		/* RUNID logging string	        */

    int   port;				/* Server port			*/
    int   spawn;			/* spawn a local daemon?	*/
    int   console;			/* initiate a console?		*/
    int   onetrip;			/* force a single instance?	*/
    int   use_cache;			/* use object/registry cache?   */
    int   use_runid;			/* use RUNID parameter?		*/
    int   quiet;			/* suppress API output?		*/
} vocOpt;




/*************************************
 *  Private procedure declarations.
 *************************************/
static int  voc_openVOCServer (char *dev);
static int  voc_parseDev (char *dev, unsigned short *host_port,
                unsigned long *host_addr);

static vocOpt *voc_initOpts ();
static vocOpt *voc_parseOpts (char *opts);
static vocOpt *voc_readConfig (char *config);

static char   *voc_cacheCreate (char *home, char *cache, char *subdir);

vocOpt *vopt  = (vocOpt *) NULL;


void	voc_exitHandler();


/******************************************************************************
**  OPENVOCLIENT --  Open and initialize the VOClient interface.
*/
int
voc_initVOClient (char *opts)
{
    register int   i;
    char  *home, *s, config[128], *opt_str;

    char  *VOCServer = (char *)NULL;
    char  *VOCProxy = (char *)NULL;
    int   port    = DEF_VOCSERVER_PORT;
    int   spawn   = TRUE;
    int   console = FALSE;
    int   onetrip = FALSE;


    /* Initialize the options.
    */
    vopt  = (vocOpt *) voc_initOpts ();


    /*  Configuration order (should be):
    **
    **		- defaults				(default)
    **		- $home/.voclient/config file		(user pref default)
    **		- application settings			(app requirements)
    **		- environment settings			(app-specific req.)
    */

    /* Get a debug flag from the opt string or the environment if we can.
     */
    opt_str = ( opts ? opts : getenv ("VOC_OPTS"));
    if (opt_str) {
	if (! (vopt = voc_parseOpts (opt_str))) {
	    if (!vopt->quiet)
	        fprintf (stderr, "ERROR: Invalid opt string '%s'!\n", opt_str);
	    return ERR;

	} else {
	    /* Allow an opt string to specify a config file to be used.
	    */
	    if (vopt->config && access (vopt->config, R_OK) == 0)
	        vopt = voc_readConfig (vopt->config);
	}

    } else {
	/* Attempt to open the config file.  This may be specified using
	** either and environment variable or the default path.
	*/
	memset (config, 0, 128);
	if ((s = getenv ("VOC_CONFIG")))
	    strcpy (config, s);
	else if ((home = getenv ("HOME")))
            sprintf (config, "%s/.voclient/config", home);

	/* Use the config from the environment, then try the user's
	** config file before using the default.
	*/  
	if (access (config, R_OK) == 0)
	    vopt = voc_readConfig (config);
    }


    /* Use defaults if we didn't get any config options.
    */
    if (vopt == (vocOpt *)NULL) {
	vopt = (vocOpt *) calloc (1, sizeof (vocOpt));
        strcpy (vopt->server, DEF_SERVER);
        strcpy (vopt->proxy,  DEF_NET_SERVER);
        strcpy (vopt->runid,  DEF_RUNID);
        vopt->port      =  DEF_VOCSERVER_PORT;
        vopt->spawn     =  TRUE;
        vopt->quiet     =  TRUE;
        vopt->use_cache =  TRUE;
        vopt->use_runid =  TRUE;
        vopt->console   =  FALSE;
        vopt->onetrip   =  onetrip;
    }


    /* Allow an environment variable to override a config file setting or
    ** task option string.
    */
    VOCServer = ((s = getenv("VOC_SERVER")) ? s : vopt->server);
    VOCProxy  = ((s = getenv("VOC_PROXY")) ? s : vopt->proxy);
	    
    if (0) {
	fprintf (stderr, "Server: '%s' Proxy: '%s' ", VOCServer, VOCProxy);
	fprintf (stderr, "spawn=%d  console=%d  port=%d  onetrip=%d\n",
	    spawn, console, port, onetrip);
    }

    if (vo) {
	if (VOC_DEBUG)
	    fprintf (stderr, "Warning: VOClient already initialized!\n");
	return OK;

    } else {
        /* Allocate the struct.
	*/
        vo = calloc (1, sizeof (VOClient));

	/* Get a channel to a server.  If we can't start our own daemon, try
	** connecting to the proxy server.
	*/
	if (!vopt->spawn) {
	    if ((vo->io_chan = voc_openVOCServer(VOCProxy)) == (int) VOC_NULL) {
                if (!vopt->quiet)
	            fprintf (stderr,
			"Cannot connect to local or proxy server.\n");
	        return ERR;
	    }


	} else if ((vo->io_chan=voc_openVOCServer(VOCServer))==(int)VOC_NULL) {
	    char *cmd = calloc (1, 128);

	    if (VOC_DEBUG)
		fprintf (stderr, "Couldn't open server, starting private...\n");

	    /*  FIXME -- 
	    //system ("voclientd -private >& /dev/null &");
	    if (opts && strstr (opts, "console")) {
	        system ("voclientd -gui &");
	    } else {
	        system ("voclientd >& /dev/null &");
	    }
	     */
	    if (vopt->console)
	        sprintf (cmd, "%s -gui -port %d &", 
		    (vopt->path[0] ? vopt->path : "voclientd"), vopt->port);
	    else
	        sprintf (cmd, "%s -port %d > /dev/null 2>&1 &", 
		    (vopt->path[0] ? vopt->path : "voclientd"), vopt->port);

	    system (cmd);

	    /* We couldn't connect to the default server, so try to start
	     * a private version for this session.  Timeout after SVR_MAXTRY
	     * attempts.
	     */
	    for (i=SVR_MAXTRY; i && vo->io_chan == (int) VOC_NULL; i--) {
		sleep (1);
	        vo->io_chan = voc_openVOCServer ("6200");
	    }
	    if (i == 0) {
	        if ((vo->io_chan=voc_openVOCServer(VOCProxy))==(int)VOC_NULL) {
		    if (!vopt->quiet)
	              fprintf (stderr,
		        "ERROR: Cannot connect to or create server process\n");
	            return ERR;
	        }
	    }
	}
    }

    /* Get a debug flag from the environment if we can.
     */
    vo->debug = ((s = getenv("VOC_DEBUG")) != NULL);
    if (s && (i = atoi(s)))
        vo->debug = i;

    vo->quiet     = vopt->quiet;
    vo->use_cache = vopt->use_cache;
    vo->use_runid = vopt->use_runid;
    vo->onetrip   = vopt->onetrip;
    vo->runid     = vopt->runid;

    /* Post an exit handler so we clean up properly.
     */
    (void) atexit (voc_exitHandler);

    return OK;
}


/******************************************************************************
**  CLOSEVOCLIENT -- Close and free the VOClient interface.
*/
void
voc_closeVOClient (int shutdown)
{
    vocMsg_t *msg = (vocMsg_t *) msg_quitMsg ();

    
    if (vo == (VOClient *) NULL)		/* no-op on null pointer */
	return;

    if (vo->onetrip || shutdown) {
        vocMsg_t *msg = (vocMsg_t *) msg_shutdownMsg ();

	/* Send the shutdown request.  We don't expect a reply so send a
	 * raw message and assume it got there.  Use an asynchrnous write
	 * so we don't block.
	 */
	(void) msg_sendRawMsg (vo->io_chan, msg);

    } else {
        vocRes_t *result;

    	/* Send the quit request. */
	if (msg_resultStatus ((result = msg_sendMsg (vo->io_chan, msg))) == ERR)
	    if (vo->debug)
	    	fprintf (stderr, "ERROR quitting voclientd.\n");

        free ((vocRes_t *) result);
    }

    /* Close the VOClient connection.
     */
    if (vo->io_chan >= 0) {
        close (vo->io_chan);
	vo->io_chan = -1;
    }

    /* Free the structure.
     */
    if (msg) free ((vocMsg_t *) msg);
    if (vo)  free ((void *) vo);
    vo = (VOClient *) NULL;
}



/******************************************************************************
**  ABORTVOCLIENT -- Close the VOClient interface and abort the application.
*/
void
voc_abortVOClient (int code, char *msg)
{
    if (msg && !vo->quiet)
	fprintf (stderr, "ABORT: %s\n", msg);
    voc_closeVOClient (1);
    exit (code);
}



/******************************************************************************
**  EXITHANDLER -- Automatically disconnect clients that forget to do so.
*/
void
voc_exitHandler()
{
    if (vo)
        voc_closeVOClient (0);
}


/******************************************************************************
**  VALIDATEOBJECT -- Given the hashcode hand for an object, verify it is 
**  still valid in the daemon hasmap.  Returns 0 if not valid, 1 if valid.
*/
int
voc_validateObject (int hcode)
{
    vocRes_t *result = (vocRes_t *) NULL;
    vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (hcode, "validateObject", 0);
    int       flag = 0;

    /* Send message and read result.  */
    if (msg_resultStatus ((result = msg_sendMsg (vo->io_chan, msg))) != ERR)
        flag = msg_getIntResult (result, 0);

    if (msg)    free ((void *)msg);         /* free the pointers            */
    if (result) free ((void *)result);

    return (flag);
}


/******************************************************************************
**  VOCREADY -- Verify that the VO Client daemon is alive and well.
*/
int
voc_ready ()
{
    vocRes_t *result = (vocRes_t *) NULL;
    vocMsg_t *msg = (vocMsg_t *) msg_ackMsg ();
    int       flag = 0;

    /* Send message and read result.  */
    if (msg_resultStatus ((result = msg_sendMsg (vo->io_chan, msg))) != ERR)
        flag = msg_getIntResult (result, 0);

    if (msg)    free ((void *)msg);         /* free the pointers            */
    if (result) free ((void *)result);

    return (flag);
}



/******************************************************************************
**  FREEPOINTER -- Free a pointer allocated in the interface.  For Client
**  apps like the IRAF CL a pointer returned for a string cannot be freed
**  properly because of a proprietary memory i/o interface.  So, we'll pass
**  it back into the interface to free it.
*/
void
voc_freePointer (char *ptr)
{
    if (ptr) 
	free ((void *) ptr);
}


/******************************************************************************
** DEBUGLEVEL -- Set the package debugging output level.
*/
void
voc_debugLevel (int level) { vo->debug = level; }






/*****************************************************************************
 *****************************************************************************
 ***
 ***   PRIVATE PROCEDURES
 ***
 *****************************************************************************
 ****************************************************************************/


/******************************************************************************
**  VOC_OPENVOCSERVER -- Open a connection to the VOClient server.  The caller
**  may either specify a connection at device open time, or the procedure will
**  attempt to connect on a unix socket or local port if that fails.
**  The syntax for the imtdev argument is as follows:
**
**      	<domain> : <address>
**
**  where <domain> is one of "inet" (internet tcp/ip socket), "unix" (unix
**  domain socket) or "fifo" (named pipe).  The form of the address depends
**  upon the domain, as illustrated in the examples below.
**
**  inet:6200 (or '6200')       Server connection to port 6200 on the local
**                              host.  For a client, a connection to the
**                              given port on the local host.
**
**  inet:6200:foo.bar.edu       Client connection to port 6200 on internet
**                              host foo.bar.edu.  The dotted form of address
**                              may also be used.
*/

static int voc_openVOCServer (char *dev)
{
    int	 fd;
    unsigned short host_port;
    unsigned long  host_addr;
    struct   sockaddr_in sockaddr;


    if (dev == (char *) NULL)
	dev = DEF_SERVER;

    if (voc_parseDev (dev, &host_port, &host_addr) == ERR) {
	if (!vo->quiet)
            fprintf (stderr, "Cannot parse device: '%s'.\n", dev);
    	return ((int ) VOC_NULL);

    } else {
        /* Get a socket. 
	 */
        if ((fd = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
            if (!vo->quiet)
                fprintf (stderr, "Cannot create inet socket on '%s'.\n", dev);
    	    return ((int ) VOC_NULL);
	}

        /* Compose a network address. 
	 */
        memset ((char *)&sockaddr, 0, sizeof(sockaddr));
        sockaddr.sin_family = AF_INET;
        sockaddr.sin_port = host_port;		/* htons() done above */
        bcopy ((char *)&host_addr,(char *)&sockaddr.sin_addr,sizeof(host_addr));

        /* Connect to the server. 
	 */
        if (connect(fd,(struct sockaddr *)&sockaddr,sizeof(sockaddr)) < 0) {
            close (fd);
            /*
            if (!vo->quiet)
                fprintf (stderr, "Cannot open server connection on '%s'.\n",
		    dev);
            */
    	    return ((int ) VOC_NULL);
	}
    }
    if (VOC_DEBUG) fprintf (stderr, "Connection established on '%s'\n", dev);

    return (fd);
}
	


/******************************************************************************
**  VOC_PARSEDEV -- Parse the VOClient Server device string to extract the host
**  and port number information.
*/

static int 
voc_parseDev (char *dev, unsigned short *hport, unsigned long *host_addr)
{
    char   *ip, osfn[128], buf[128], host_str[128];
    unsigned short port;
    register int i;
    struct hostent *hp;


    if (dev == NULL)
	return ERR;

    memset (osfn, 0, 128);
    memset (host_str, 0, 128);

    /* Expand any %d fields in the network address to the UID. */
    sprintf (osfn, (char *)dev, getuid(), getuid());
 
    /* Form is a port number with an optional node name.  First extract
     * the port number.
     */
    if (isdigit (osfn[0]))
        ip = osfn;
    else if (strncmp (osfn, "inet:", 5) == 0)
        ip = osfn + 5;

    /* Get the port number. */
    for (i=0; isdigit (*ip); i++, ip++)
        buf[i] = (char) *ip;

    port = atoi (buf);		/* set port number	*/
    *hport = htons (port);

    /* Get host address.  This may be specified either has a host
     * name or as an Internet address in dot notation.  If no host
     * name is specified default to the local host.
     */
    if (*ip++ == ':') {
	strcpy (host_str, ip);
    } else
        sprintf (host_str, "%s", "127.0.0.1");

    if (isdigit (host_str[0])) {
        *host_addr = inet_addr (host_str);
        if ((int)*host_addr == -1)
            return ERR;

    } else if ((hp = gethostbyname (host_str))) {
        bcopy (hp->h_addr, (char *)host_addr, sizeof(*host_addr));

    } else
        return ERR;

    if (VOC_DEBUG)
	fprintf (stderr, "parseDev:  '%s' --> port=%d  node='%s'\n",
	    dev, port, host_str);

    return OK;
}


/******************************************************************************
**  VOC_INITOPTS -- Initialize the VOClient configuration options.
*/
static vocOpt *
voc_initOpts ()
{
    /* Use defaults if we didn't get any config options.
    */
    if (vopt == (vocOpt *)NULL)
	vopt = (vocOpt *) calloc (1, sizeof (vocOpt));

    strcpy (vopt->server, DEF_SERVER);
    strcpy (vopt->proxy,  DEF_NET_SERVER);
    strcpy (vopt->runid,  DEF_RUNID);
    vopt->port      =  DEF_VOCSERVER_PORT;
    vopt->spawn     =  TRUE;
    vopt->quiet     =  TRUE;
    vopt->use_cache =  TRUE;
    vopt->use_runid =  TRUE;
    vopt->console   =  FALSE;
    vopt->onetrip   =  FALSE;

    return (vopt);
}



/******************************************************************************
**  VOC_PARSEOPTS -- Parse the VOClient Server option string.  Options are
**  encoded as a comma-delimited list of options that include:
**
**      spawn           If set, will attempt to spawn a local instance of the
**                      voclient daemon if not already running.
**
**      port            Default port to use for a local voclient daemon.
**
**      console         Create a GUI console window for the local daemon?
**
**      quiet           Client API shouldn't produce any error output, it
**       		will be handled by the application.
**
**      runid           Set the RUNID logging string
**
**      use_cache       Use the object/registry cache?
**
**      use_runid       Use the RUNID parameter?
**
**      server          VOClient daemon server address.  The address is of
**                      the form
**                                   <port> ':' <host>
**
**                      where <host> can be a local machine name, a fully-
**                      qualified domain name, or an IP address.  <port> is
**                      the socket number on which the 'voclientd' is running
**                      on that machine.  A server will be started if the
**                      'use_local' flag is set and <host> is 'localhost' or
**                      the current machine name, the <port> specified here
**                      will override the 'port' setting
**
**  Keyword names may be abbreviated to the smallest unique word.
** 
**  An example opt string would be
** 
** 		server=8081:localhost,spawn=yes,console=yes,runid=test
** 
**  to spawn a voclientd running on the local machine at port 8081 with a
**  GUI console panel.
*/

#define	SZ_OPT_BUF	64


static vocOpt *  
voc_parseOpts (char *opts)
{
    register int i;
    char *ip, keyw[SZ_OPT_BUF], val[SZ_OPT_BUF];
    char  lserver[SZ_OPT_BUF];
    char  lproxy[SZ_OPT_BUF];
    char  lconfig[SZ_OPT_BUF];
    char  lpath[SZ_OPT_BUF];
    char  lrunid[SZ_OPT_BUF];
    int   lport      = DEF_VOCSERVER_PORT;
    int   lspawn     = TRUE;
    int   lcons      = FALSE;
    int   lone       = FALSE;
    int   lquiet     = TRUE;
    int   luse_cache = TRUE;
    int   luse_runid = TRUE;
    int   len;

    vocOpt *vopt  = (vocOpt *) NULL;


    memset (lserver, 0, SZ_OPT_BUF);	strcpy (lserver, DEF_SERVER);
    memset (lproxy, 0, SZ_OPT_BUF);	strcpy (lproxy, DEF_NET_SERVER);
    memset (lconfig, 0, SZ_OPT_BUF);
    memset (lpath, 0, SZ_OPT_BUF);

    /* Loop through the option string, setting values.
    */
    ip = opts;
    while (*ip) {
	memset (keyw, 0, SZ_OPT_BUF);
	memset (val, 0, SZ_OPT_BUF);

	for (i=0; *ip && *ip != '=' && i < SZ_OPT_BUF; ) 
	    keyw[i++] = tolower (*ip++);

 	if (*ip != '=') {
	    /* The presence of a keyword implies it's turned on.
	    */
	    val[0] = '1';
	} else {
	    ip++;
	    for (i=0; *ip && *ip != ',' && i < SZ_OPT_BUF; ) 
	        val[i++] = tolower (*ip++);
	}
	len = strlen (val);


	/*  Ugly but effective parsing.
	*/
	if (strncmp ("config", keyw, 4) == 0)
	    strcpy (lconfig, val);

	else if (strncmp ("console", keyw, 4) == 0)
	    lcons = (val[0] == 'y' || val[0] == '1');

	else if (strncmp ("onetrip", keyw, 2) == 0)
	    lone = (val[0] == 'y' || val[0] == '1');

	else if (strncmp ("port", keyw, 2) == 0)
	    lport = atoi (val);

	else if (strncmp ("proxy", keyw, 2) == 0)
	    strcpy (lproxy, val);

	else if (strncmp ("runid", keyw, 3) == 0)
	    strcpy (lrunid, val);

	/*
	else if (strncmp ("voc_path", keyw, 8) == 0)
	    strcpy (lpath, val);
            if (val[len-1] != '/')
                strcat (lpath, "/");
	*/

	else if (strncmp ("spawn", keyw, 2) == 0)
	    lspawn = (val[0] == 'y' || val[0] == '1');

	else if (strncmp ("use_cache", keyw, 5) == 0)
	    luse_cache = (val[0] == 'y' || val[0] == '1');

	else if (strncmp ("use_runid", keyw, 5) == 0)
	    luse_runid = (val[0] == 'y' || val[0] == '1');

	else if (strncmp ("server", keyw, 2) == 0)
	    strcpy (lserver, val);

	else if (strncmp ("quiet", keyw, 2) == 0)
	    lquiet = atoi (val);

	else if (!lquiet)
	    fprintf (stderr, "parseOpt: invalid option '%s'='%s'\n", keyw, val);

	/*  Skip to next option or break.
	*/
	if (*ip && *ip == ',')
	    ip++;
    }


    /* Save the values and return.
    */
    vopt = calloc (1, sizeof (vocOpt));
    strcpy (vopt->server, lserver);
    strcpy (vopt->proxy, lproxy);
    strcpy (vopt->config, lconfig);
    strcpy (vopt->path, lpath);
    strcpy (vopt->runid, lrunid);
    vopt->onetrip   = lone;
    vopt->port      = lport;
    vopt->spawn     = lspawn;
    vopt->quiet     = lquiet;
    vopt->use_cache = luse_cache;
    vopt->use_runid = luse_runid;
    vopt->console   = lcons;

    return (vopt);
}


static vocOpt *
voc_readConfig (char *config)
{
    return ((vocOpt *)NULL);
}



/******************************************************************************
**  Cache Utility procedures.
*/

char *
voc_getCacheDir (char *subdir)
{
    char *home, cache[SZ_FNAME], fullpath[SZ_FNAME], *path;
    DIR  *dir;


    if ((home = getenv ("HOME")) ) {
        memset (cache, 0, SZ_FNAME);
        sprintf (cache, "%s/.voclient/cache", home);
        sprintf (fullpath, "%s/%s", cache, subdir);
    } else
	return ((char *) NULL);


    /* Create the cache dir if it doesn't already exist.
    */
    if (!(dir = opendir (fullpath)) )
        path = voc_cacheCreate (home, cache, subdir);
    else {
	closedir (dir);
	path = calloc (1, strlen (fullpath)+2);
	strcpy (path, fullpath);
    }

    if ((dir = opendir (path)) ) {
	closedir (dir);
        return (path);
    } else 
	return ((char *)NULL);
}


/*  Create any needed cache directories.
*/
static char *
voc_cacheCreate (char *home, char *cache, char *subdir)
{
    char  *path, fname[SZ_FNAME];
    DIR   *dir;


    /* Be sure we can open the HOME dir. */
    if ((dir = opendir (home)) ) {
        closedir (dir);
        sprintf (fname, "%s/.voclient/", home);

        /* Check for access to the $HOME/.voclient dir, if it doesn't exist
        ** then create it.
        */
        if (!(dir = opendir (fname)) ) {
            mkdir (fname, (mode_t) 0777);
            sprintf (fname, "%s/.voclient/cache", home);
            mkdir (fname, (mode_t) 0777);       /* create cache dir     */

        } else {
            /* The $HOME/.voclient exists, now check for a cache dir.
            */
            sprintf (fname, "%s/.voclient/cache", home);
            if (!(dir = opendir (fname)) )
                mkdir (fname, (mode_t) 0777);
        }
        if (dir)
            closedir (dir);

	/* We now have the cache dir, see what's to be done about a subdir.
	*/
	if (subdir) {
	    path = calloc (1, SZ_FNAME);

            sprintf (path, "%s/%s", cache, subdir);
            if (!(dir = opendir (path)) )
                mkdir (path, (mode_t) 0777);
	    else
                closedir (dir);
    	    return (path);

	} else
    	    return (strdup (cache));

    }

    return ((char *) NULL);
}


int voc_dbg (void) { static int count = 0; count++; return (count); }
