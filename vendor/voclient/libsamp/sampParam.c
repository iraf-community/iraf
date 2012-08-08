/**
 *  SAMPPARAM.C -- (Internal) Interface to Param objects.
 *
 *         param = samp_newParam  ()
 *                samp_freeParam  (Param param)
 *  
 *        param = samp_paramInit  (Msg msg);
 *           samp_addStringParam  (Msg msg, char *keyw, String val)
 *             samp_addListParam  (Msg msg, char *keyw, List val)
 *              samp_addMapParam  (Msg msg, char *keyw, Map val)
 *             N = samp_paramLen  (Msg msg)
 *  
 *  @brief      (Internal)  Interface to Param objects.
 *  
 *  @file       sampParam.c
 *  @author     Mike Fitzpatrick
 *  @date       7/10/11
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "samp.h"


#define	USE_STRING_PARAMS



/**
 *  SAMP_NEWPARAM -- Create a new Param object
 *
 *  @brief	Create a new Param object
 *  @fn		int samp_newParam (void)
 *
 *  @return 		handle to new Param
 */
Param
samp_newParam ()
{
    return ((Param) xr_newStruct());
}


/**
 *  SAMP_FREEPARAM -- Free the given Param object
 *
 *  @brief	Free the given Msg object
 *  @fn		void samp_freeParam (Param param)
 *
 *  @param  param	Param object to free
 *  @return 		nothing
 */
void
samp_freeParam (Param param)
{
    xr_freeStruct (param);
}


/**
 *  SAMP_PARAMINIT -- Get number of Params.
 *
 *  @brief	Get number of Params.
 *  @fn		nparam = samp_paramInit (Msg msg)
 *
 *  @param  msg		handle to Msg object
 *  @return 		nothing
 */
Param
samp_paramInit (Msg msg)
{
    Param  param;

    xr_getStructFromStruct (msg, "samp.params", &param);
    if (param == 0)
        param = (Map) xr_newStruct();

    return (param);
}


/**
 *  SAMP_ADDSTRINGPARAM -- Add a String parameter to the Param.
 *
 *  @brief	Add a String parameter to the Param.
 *  @fn		void samp_addStringParam (Msg msg, char *keyw, Map val)
 *
 *  @param  msg		handle to Msg object
 *  @param  keyw	map keyword
 *  @param  val		value string
 *  @return 		nothing
 */
void
samp_addStringParam (Msg msg, char *keyw, String val)
{
    Param  param = samp_paramInit (msg);
    xr_setStringInStruct (param, keyw, (val ? val :  ""));
}


/**
 *  SAMP_ADDMAPPARAM -- Add a Map parameter to the Param.
 *
 *  @brief	Add a parameter to the Param.
 *  @fn		void samp_addMapParam (Msg msg, char *keyw, Map val)
 *
 *  @param  msg		handle to Msg object
 *  @param  keyw	map keyword
 *  @param  val		value map
 *  @return 		nothing
 */
void
samp_addMapParam (Msg msg, char *keyw, Map val)
{
    Param  param = samp_paramInit (msg);
    xr_setStructInStruct (param, keyw, val);
}


/**
 *  SAMP_ADDLISTPARAM -- Add a List parameter to the Param.
 *
 *  @brief	Add a parameter to the Param.
 *  @fn		void samp_addListParam (Msg msg, char *keyw, List val)
 *
 *  @param  msg		handle to Msg object
 *  @param  keyw	map keyword
 *  @param  val		value list
 *  @return 		nothing
 */
void
samp_addListParam (Msg msg, char *keyw, List val)
{
    Param  param = samp_paramInit (msg);
    xr_setArrayInStruct (param, keyw, val);
}


/**
 *  SAMP_ADDINTPARAM -- Add a <SAMP int> parameter to the Param.
 *
 *  @brief	Add a <SAMP int> parameter to the Param.
 *  @fn		void samp_addIntParam (Msg msg, char *keyw, int val)
 *
 *  @param  msg		handle to Msg object
 *  @param  keyw	map keyword
 *  @param  val		integer value
 *  @return 		nothing
 */
void
samp_addIntParam (Msg msg, char *keyw, int val)
{
    Param  param = samp_paramInit (msg);
#ifdef USE_STRING_PARAMS
    char  sval[32];

    memset (sval, 0, 32);
    sprintf (sval, "%d", val);
    xr_setStringInStruct (param, keyw, sval);
#else
    xr_setIntInStruct (param, keyw, val);
#endif
}


/**
 *  SAMP_ADDFLOATPARAM -- Add a <SAMP float> parameter to the Param.
 *
 *  @brief	Add a <SAMP float> parameter to the Param.
 *  @fn		void samp_addFloatParam (Msg msg, char *keyw, float val)
 *
 *  @param  msg		handle to Msg object
 *  @param  keyw	map keyword
 *  @param  val		floating point value
 *  @return 		nothing
 */
void
samp_addFloatParam (Msg msg, char *keyw, float val)
{
    Param  param = samp_paramInit (msg);
#ifdef USE_STRING_PARAMS
    char   sval[32];

    memset (sval, 0, 32);
    sprintf (sval, "%f", val);
    xr_setStringInStruct (param, keyw, sval);
#else
    /*xr_setDoubleInStruct (param, keyw, (double) val);*/
    xr_setFloatInStruct (param, keyw, val);
#endif
}


/**
 *  SAMP_PARAMLEN -- Get number of Params.
 *
 *  @brief	Get number of Params.
 *  @fn		nparam = samp_paramLen (Param param)
 *
 *  @param  msg		handle to Msg object
 *  @return 		nothing
 */
int
samp_paramLen (Msg msg)
{
    return (0); 					/*  NYI  */
}
