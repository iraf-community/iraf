/**
 *  SAMPMSG.C -- (Internal) Interface to Message objects.
 *
 *             msg = samp_newMsg  ()
 *                 samp_msgMType  (Msg msg, String mtype)
 *                 samp_msgParam  (Msg msg, Param param)
 *                   samp_msgTag  ()
 *                  samp_freeMsg  (Msg msg)
 *  
 *  @brief      (Internal)  Interface to Message objects.
 *  
 *  @file       sampMsg.c
 *  @author     Mike Fitzpatrick
 *  @date       7/10/11
 */


#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "samp.h"



/**
 *  SAMP_NEWMSG -- Create a new Msg object
 *
 *  @brief	Create a new Msg object
 *  @fn		int samp_newMsg (void)
 *
 *  @return 		handle to new Msg
 */
Msg
samp_newMsg ()
{
    Map    msg = (Map) xr_newStruct();
    return ( (Map) msg );
}


/**
 *  SAMP_FREEMSG -- Free the given Msg object
 *
 *  @brief	Free the given Msg object
 *  @fn		void samp_freeMsg (Msg msg)
 *
 *  @param  msg		Msg object to free
 *  @return 		nothing
 */
void
samp_freeMsg (Msg msg)
{
    xr_freeStruct (msg);
}


/**
 *  SAMP_SETMTYPE -- Set the MType of a Msg.
 *
 *  @brief	Set the MType of a Msg.
 *  @fn		void samp_setMtype (Msg msg, String mtype)
 *
 *  @param  msg		handle to Msg object
 *  @param  mtype	MType string
 *  @return 		nothing
 */
void
samp_msgMType (Msg msg, String mtype)
{
    xr_setStringInStruct (msg, "samp.mtype", mtype);
}


/**
 *  SAMP_NEWMSG -- Create a new Msg object
 *
 *  @brief	Create a new Msg object
 *  @fn		int samp_newMsg (void)
 *
 *  @return 		handle to new Msg
 */
char *
samp_msgTag ()
{
    static int  msgnum = 0;
    static char tag[128];

    memset (tag, 0, 128);
    sprintf (tag, "msg%06d", msgnum++);

    return ( tag );
}


/**
 *  SAMP_MSGPARAM -- Add a parameter to the Msg.
 *
 *  @brief	Add a parameter to the Msg.
 *  @fn		void samp_msgParam (Msg msg, Param param)
 *
 *  @param  msg		handle to Msg object
 *  @param  keyw	map keyword
 *  @param  param	parameter map
 *  @return 		nothing
 */
void
samp_msgParam (Msg msg, Param param)
{
    xr_setStructInStruct (msg, "samp.params", param);
}
