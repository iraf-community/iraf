/**
 *  SAMP.I -- SWIG Interface definition file for SAMP library.
 *
 *  @brief      SWIG Interface definition file for SAMP library.
 *
 *  @file       samp.i
 *  @author     Mike FItzpatrick
 *  @date       7/20/11
 */


%module libsamp
%{
#define handle_t        int
#define Param           int
#define Map             int
#define Msg             int
#define List            int


/*  samp.c
 */
extern handle_t sampInit (char *appName, char *description);
extern void 	sampClose (handle_t handle);
extern int  	sampStartup (handle_t handle);
extern int  	sampShutdown (handle_t handle);

extern void 	samp_setMetadata (handle_t handle, char *field, char *value);
extern void 	samp_setSubscription(handle_t handle, char *mtype, void *func);
extern void 	samp_Unsubscribe (handle_t handle, char *mtype);
extern void 	samp_setCallMode (handle_t handle, int mode);
extern void 	samp_setReplyCallback (handle_t handle, int *func);
extern void 	samp_setResponseCallback (handle_t handle, int *func);
extern void 	samp_defaultReplyHandler (handle_t handle);
extern void 	samp_deaultfResponseHandler (handle_t handle);
extern int 	samp_replyStatus (handle_t handle);

/*  sampHub.c
 */
extern handle_t samp_hubOpen (Samp *samp);
extern int 	samp_hubClose (handle_t handle);
extern List 	samp_getAvailableHubs (handle_t handle);
extern char    *samp_getActiveHubName (handle_t handle);
extern int 	samp_getActiveHub (handle_t handle);
extern int 	samp_hubInit (handle_t samp, char *appName, char *descr);

extern int 	samp_hubRegister (Hub *hub);
extern int 	samp_hubUnRegister (Hub *hub);
extern int	samp_hubSetXmlrpcCallback (Hub *hub);
extern int 	samp_hubPing (Hub *hub);
extern int 	samp_hubDeclareMetadata (Hub *hub);
extern int 	samp_hubDeclareSubscriptions (Hub *hub);

/*  sampCommands.c
 */
extern int 	samp_Register (handle_t samp);
extern int 	samp_UnRegister (handle_t samp);
extern int 	samp_DeclareMetadata (handle_t samp);
extern int 	samp_Ping (handle_t samp, char *appName);
extern Map 	samp_GetMetadata (handle_t samp, char * pubId);
extern void 	samp_DeclareSubscriptions (handle_t samp, Map subscriptions);
extern Map 	samp_GetSubscriptions (handle_t samp, char * pubId);
extern List 	samp_GetRegisteredClients (handle_t samp);

extern int 	samp_tableLoadVOTable (handle_t samp, char *recip, char *url, 
		    char *tableId, char *name);
extern int 	samp_tableLoadFITS (handle_t samp, char *recip, char *url, 
		    char *tableId, char *name);
extern int 	samp_imageLoadFITS (handle_t samp, char *recip, char *url, 
		    char *imageId, char *name);

extern int 	samp_tableHighlightRow (handle_t samp, char *recip, 
		    char *tableId, char *url, int row);
extern int 	samp_tableSelectRowList (handle_t samp, char *recip, 
		    char *tableId, char *url, List rows);
extern int 	samp_coordPointAtSky (handle_t samp, char *recip, float ra, 
		    float dec);
extern int 	samp_specLoadSSAGeneric (handle_t samp, char *recip, char *url, 
		    Map meta, char *spectrumId, char *name);

/*  sampClient.c
 */
extern void    	samp_notify (handle_t samp, char * recipId, Map msg);
extern List    	samp_notifyAll (handle_t samp, Map msg);
extern char *  	samp_call (handle_t samp, char * recipId, char * tag, Map msg);
extern Map     	samp_callAll (handle_t samp, char * msg_tag, Map msg);
extern Map     	samp_callAndWait (handle_t samp, char * recipId, 
		    char * msg_tag, Map msg);

extern char * 	samp_clientName (handle_t samp, char * pubId);
extern int	samp_setErr (handle_t samp, Map resp);
extern char *  	samp_getErr (handle_t samp);

/*  sampHandlers.c
 */
extern int 	samp_receiveCall (void *data);
extern int 	samp_receiveNotification (void *data);
extern int 	samp_receiveResponse (void *data);

extern int 	samp_PingHandler (void *data);

extern int 	samp_Reply (handle_t samp, char *msg_id, Map response);

/* sampList.c
*/
extern handle_t	samp_newList ();
extern void 	samp_freeList (List list);
extern int 	samp_listLen (List list);

extern void 	samp_setchar *InList (List list, char *value);
extern void 	samp_setMapInList (List list, Map map);
extern void 	samp_setListInList (List list1, List list2);

extern char    *samp_getchar *FromList (List list, int index);
extern Map 	samp_getMapFromList (List list, int index);
extern List 	samp_getListFromList (List list, int index);

/* sampMap.c
*/
extern handle_t samp_newMap (void);
extern void 	samp_freeMap (Map map);

extern void 	samp_setchar *InMap (Map map, char *key, char *value);
extern void 	samp_setMapInMap (Map map1, char *key, Map map2);
extern void 	samp_setListInMap (Map map, char *key, List list);

extern char    *samp_getchar *FromMap (Map map, char *key);
extern Map 	samp_getMapFromMap (Map map, char *key);
extern List 	samp_getListFromMap (Map map, char *key);

/* sampMsg.c
 */
extern Msg 	samp_newMsg (void);
extern void 	samp_freeMsg (Msg msg);
extern void 	samp_msgMType (Msg msg, char * mtype);
extern void 	samp_msgParam (Msg msg, Param param);
extern char    *samp_msgTag (void);

/* sampParam.c
 */
extern Param	samp_newParam (void);
extern void 	samp_freeParam (Param param);
extern Param 	samp_paramInit (Msg msg);
extern void 	samp_addchar *Param (Msg msg, char *keyw, char * val);
extern void 	samp_addMapParam (Msg msg, char *keyw, Map val);
extern void 	samp_addListParam (Msg msg, char *keyw, List val);
extern int 	samp_paramLen (Msg msg);

/* sampLog.c
*/
extern void 	sampLog (handle_t handle, char *format, ...);
extern void 	sampTrace (handle_t handle, char *format, ...);

/*  sampUtil.c
 */
extern handle_t samp_newHandle (void *ptr);
extern void 	samp_freeHandle (handle_t handle);
extern handle_t samp_P2H (void *ptr);
extern void    *samp_H2P (handle_t handle); 
%}




#define handle_t        int
#define Param           int
#define Map             int
#define Msg             int
#define List            int


/*  samp.c
 */
extern handle_t sampInit (char *appName, char *description);
extern void 	sampClose (handle_t handle);
extern int  	sampStartup (handle_t handle);
extern int  	sampShutdown (handle_t handle);

extern void 	samp_setMetadata (handle_t handle, char *field, char *value);
extern void 	samp_setSubscription(handle_t handle, char *mtype, void *func);
extern void 	samp_Unsubscribe (handle_t handle, char *mtype);
extern void 	samp_setCallMode (handle_t handle, int mode);
extern void 	samp_setReplyCallback (handle_t handle, int *func);
extern void 	samp_setResponseCallback (handle_t handle, int *func);
extern void 	samp_defaultReplyHandler (handle_t handle);
extern void 	samp_deaultfResponseHandler (handle_t handle);
extern int 	samp_replyStatus (handle_t handle);

/*  sampHub.c
 */
extern handle_t samp_hubOpen (Samp *samp);
extern int 	samp_hubClose (handle_t handle);
extern List 	samp_getAvailableHubs (handle_t handle);
extern char    *samp_getActiveHubName (handle_t handle);
extern int 	samp_getActiveHub (handle_t handle);
extern int 	samp_hubInit (handle_t samp, char *appName, char *descr);

extern int 	samp_hubRegister (Hub *hub);
extern int 	samp_hubUnRegister (Hub *hub);
extern int	samp_hubSetXmlrpcCallback (Hub *hub);
extern int 	samp_hubPing (Hub *hub);
extern int 	samp_hubDeclareMetadata (Hub *hub);
extern int 	samp_hubDeclareSubscriptions (Hub *hub);

/*  sampCommands.c
 */
extern int 	samp_Register (handle_t samp);
extern int 	samp_UnRegister (handle_t samp);
extern int 	samp_DeclareMetadata (handle_t samp);
extern int 	samp_Ping (handle_t samp, char *appName);
extern Map 	samp_GetMetadata (handle_t samp, char * pubId);
extern void 	samp_DeclareSubscriptions (handle_t samp, Map subscriptions);
extern Map 	samp_GetSubscriptions (handle_t samp, char * pubId);
extern List 	samp_GetRegisteredClients (handle_t samp);

extern int 	samp_tableLoadVOTable (handle_t samp, char *recip, char *url, 
		    char *tableId, char *name);
extern int 	samp_tableLoadFITS (handle_t samp, char *recip, char *url, 
		    char *tableId, char *name);
extern int 	samp_imageLoadFITS (handle_t samp, char *recip, char *url, 
		    char *imageId, char *name);

extern int 	samp_tableHighlightRow (handle_t samp, char *recip, 
		    char *tableId, char *url, int row);
extern int 	samp_tableSelectRowList (handle_t samp, char *recip, 
		    char *tableId, char *url, List rows);
extern int 	samp_coordPointAtSky (handle_t samp, char *recip, float ra, 
		    float dec);
extern int 	samp_specLoadSSAGeneric (handle_t samp, char *recip, char *url, 
		    Map meta, char *spectrumId, char *name);

/*  sampClient.c
 */
extern void    	samp_notify (handle_t samp, char * recipId, Map msg);
extern List    	samp_notifyAll (handle_t samp, Map msg);
extern char *  	samp_call (handle_t samp, char * recipId, char * tag, Map msg);
extern Map     	samp_callAll (handle_t samp, char * msg_tag, Map msg);
extern Map     	samp_callAndWait (handle_t samp, char * recipId, 
		    char * msg_tag, Map msg);

extern char * 	samp_clientName (handle_t samp, char * pubId);
extern int	samp_setErr (handle_t samp, Map resp);
extern char *  	samp_getErr (handle_t samp);

/*  sampHandlers.c
 */
extern int 	samp_receiveCall (void *data);
extern int 	samp_receiveNotification (void *data);
extern int 	samp_receiveResponse (void *data);

extern int 	samp_PingHandler (void *data);

extern int 	samp_Reply (handle_t samp, char *msg_id, Map response);

/* sampList.c
*/
extern handle_t	samp_newList ();
extern void 	samp_freeList (List list);
extern int 	samp_listLen (List list);

extern void 	samp_setchar *InList (List list, char *value);
extern void 	samp_setMapInList (List list, Map map);
extern void 	samp_setListInList (List list1, List list2);

extern char    *samp_getchar *FromList (List list, int index);
extern Map 	samp_getMapFromList (List list, int index);
extern List 	samp_getListFromList (List list, int index);

/* sampMap.c
*/
extern handle_t samp_newMap (void);
extern void 	samp_freeMap (Map map);

extern void 	samp_setchar *InMap (Map map, char *key, char *value);
extern void 	samp_setMapInMap (Map map1, char *key, Map map2);
extern void 	samp_setListInMap (Map map, char *key, List list);

extern char    *samp_getchar *FromMap (Map map, char *key);
extern Map 	samp_getMapFromMap (Map map, char *key);
extern List 	samp_getListFromMap (Map map, char *key);

/* sampMsg.c
 */
extern Msg 	samp_newMsg (void);
extern void 	samp_freeMsg (Msg msg);
extern void 	samp_msgMType (Msg msg, char * mtype);
extern void 	samp_msgParam (Msg msg, Param param);
extern char    *samp_msgTag (void);

/* sampParam.c
 */
extern Param	samp_newParam (void);
extern void 	samp_freeParam (Param param);
extern Param 	samp_paramInit (Msg msg);
extern void 	samp_addchar *Param (Msg msg, char *keyw, char * val);
extern void 	samp_addMapParam (Msg msg, char *keyw, Map val);
extern void 	samp_addListParam (Msg msg, char *keyw, List val);
extern int 	samp_paramLen (Msg msg);

/* sampLog.c
*/
extern void 	sampLog (handle_t handle, char *format, ...);
extern void 	sampTrace (handle_t handle, char *format, ...);

/*  sampUtil.c
 */
extern handle_t samp_newHandle (void *ptr);
extern void 	samp_freeHandle (handle_t handle);
extern handle_t samp_P2H (void *ptr);
extern void    *samp_H2P (handle_t handle); 
