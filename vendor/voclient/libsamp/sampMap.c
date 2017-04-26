/**
 *  SAMPMAP.C -- (Internal) Interface to support the Map structure
 *
 *             map = samp_newMap  ()
 *                  samp_freeMap  (Map map)
 *  
 *       nelem = samp_getMapSize  (Map map)
 *          key = samp_getMapKey  (Map map, int index)
 *          val = samp_getMapVal  (Map map, int index)
 *  
 *           samp_setStringInMap  (Map map, char *value)
 *              samp_setMapInMap  (Map map1, Map map2)
 *             samp_setListInMap  (Map map, List list)
 *              samp_setIntInMap  (Map map, int ival)
 *            samp_setFloatInMap  (Map map, float rval)
 *  
 *   str = samp_getStringFromMap  (Map map, char *key)
 *      map = samp_getMapFromMap  (Map map, char *key)
 *    list = samp_getListFromMap  (Map map, char *key)
 *     ival = samp_getIntFromMap  (Map map, char *key)
 *   rval = samp_getFloatFromMap  (Map map, char *key)
 *
 *
 *  @brief      (Internal) Interface to support the Map structure
 *  
 *  @file       sampMap.c
 *  @author     Mike Fitzpatrick
 *  @date       7/10/09
 */


#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "samp.h"




/**
 *  SAMP_NEWMAP -- Create a new Map object
 *
 *  @brief	Create a new Map object
 *  @fn		int samp_newMap (void)
 *
 *  @return 		handle to new Map
 */
handle_t
samp_newMap ()
{
    return ( (handle_t) xr_newStruct() );
}


/**
 *  SAMP_FREEMAP -- Free the given Map object
 *
 *  @brief	Free the given Map object
 *  @fn		void samp_freeMap (Map map)
 *
 *  @param  map		Map object to free
 *  @return 		nothing
 */
void
samp_freeMap (Map map)
{
    if (map > 0)
        xr_freeStruct (map);
}


/*
 *  SAMP_GETMAPSIZE -- Get the number of elements in a Map.
 *
 *  @brief	Get the number of elements in a Map.
 *  @fn		int nelem = samp_getMapSize (Map map)
 *
 *  @param  map		handle to Map object
 *  @return 		number of elements
 */
int samp_getMapSize (Map map)
{
    return (xr_structSize (map));
}


/*
 *  SAMP_GETMAPKEY -- Get a Map keyword by index.
 *
 *  @brief	Get a Map keyword by index.
 *  @fn		char *key = samp_getMapKey (Map map, int index)
 *
 *  @param  map		handle to Map object
 *  @param  index	Map element index
 *  @return 		Map keyword
 */
char *
samp_getMapKey (Map map, int index)
{
    return (xr_getStructKey (map, index));
}


/*
 *  SAMP_GETMAPVAL -- Get a Map value by index.
 *
 *  @brief	Get a Map value by index.
 *  @fn		char *val = samp_getMapVal (Map map, int index)
 *
 *  @param  map		handle to Map object
 *  @param  index	Map element index
 *  @return 		Map value
 */
char *
samp_getMapVal (Map map, int index)
{
    return (xr_getStructVal (map, index));
}


/**
 *  SAMP_SETSTRINGINMAP -- Set a string in a Map (append)
 *
 *  @brief	Set a string in a Map (append)
 *  @fn		void samp_setStringInMap (Map map, char *key, char *value)
 *
 *  @param  map		handle to Map object
 *  @param  key		Map key
 *  @param  value	Map value
 *  @return 		nothing
 */
void
samp_setStringInMap (Map map, char *key, char *value)
{
    xr_setStringInStruct (map, key, value);
}


/**
 *  SAMP_SETMAPINMAP -- Set a Map in a Map (append)
 *
 *  @brief	Set a Map in a Map (append)
 *  @fn		void samp_setMapInMap (Map map1, char *key, Map map2)
 *
 *  @param  map1	handle to Map object
 *  @param  key		Map key
 *  @param  map2	handle to Map object to set
 *  @return 		nothing
 */
void
samp_setMapInMap (Map map1, char *key, Map map2)
{
    xr_setStructInStruct (map1, key, map2);
}


/**
 *  SAMP_SETLISTINMAP -- Set a List in a Map (append)
 *
 *  @brief	Set a List in a Map (append)
 *  @fn		void samp_setListInMap (Map map, char *key, List list)
 *
 *  @param  map		handle to Map object
 *  @param  key		Map key
 *  @param  list	handle to List object to set
 *  @return 		nothing
 */
void
samp_setListInMap (Map map, char *key, List list)
{
    xr_setArrayInStruct (map, key, list);
}


/**
 *  SAMP_SETINTINMAP -- Set a Int in a Map (append)
 *
 *  @brief	Set a Int in a Map (append)
 *  @fn		void samp_setIntInMap (Map map, char *key, int value)
 *
 *  @param  map		handle to Map object
 *  @param  key		Map key
 *  @param  value	value
 *  @return 		nothing
 */
void
samp_setIntInMap (Map map, char *key, int value)
{
    xr_setIntInStruct (map, key, value);
}


/**
 *  SAMP_SETFLOATINMAP -- Set a Float in a Map (append)
 *
 *  @brief	Set a string in a Map (append)
 *  @fn		void samp_setFloatInMap (Map map, char *key, float value)
 *
 *  @param  map		handle to Map object
 *  @param  key		Map key
 *  @param  value	value
 *  @return 		nothing
 */
void
samp_setFloatInMap (Map map, char *key, float value)
{
    xr_setDoubleInStruct (map, key, (double)value);
}


/****************************************************************************/

/**
 *  SAMP_GETSTRINGFROMMAP -- Get a string from the Map
 *
 *  @brief	Get a string from the Map
 *  @fn		char *samp_getStringFromMap (Map map, char *key)  
 *
 *  @param  map		handle to Map object
 *  @param  key		Map key
 *  @return 		string value from Map
 */
char *
samp_getStringFromMap (Map map, char *key)  
{
    static char  buf[SZ_SBUF], *res = buf;

    memset (buf, 0, SZ_SBUF);
    xr_getStringFromStruct (map, key, &res);

    return (res);
}


/**
 *  SAMP_GETMAPFROMMAP -- Get a Map from the Map
 *
 *  @brief	Get a Map from the Map
 *  @fn		Map samp_getMapFromMap (Map map, char *key)  
 *
 *  @param  map		handle to Map object
 *  @param  key		Map key
 *  @return 		handle to Map value
 */
Map
samp_getMapFromMap (Map map, char *key)  
{
    Map m = 0;
    xr_getStructFromStruct (map, key, &m);

    return (m);
}


/**
 *  SAMP_GETLISTFROMMAP -- Get a LIST from the Map
 *
 *  @brief	Get a LIST from the Map
 *  @fn		List samp_getListFromMap (Map map, char *key)  
 *
 *  @param  map		handle to Map object
 *  @param  key		Map key
 *  @return 		handle to List value
 */
List
samp_getListFromMap (Map map, char *key)  
{
    List list = 0;
    xr_getArrayFromStruct (map, key, &list);

    return (list);
}


/**
 *  SAMP_GETINTFROMMAP -- Get a integer from the Map
 *
 *  @brief	Get a integer from the Map
 *  @fn		ival = samp_getIntFromMap (Map map, char *key)  
 *
 *  @param  map		handle to Map object
 *  @param  key		Map key
 *  @return 		integer value
 */
int
samp_getIntFromMap (Map map, char *key)  
{
    int ival = 0;
    xr_getIntFromStruct (map, key, &ival);

    return (ival);
}


/**
 *  SAMP_GETFLOATFROMMAP -- Get a float from the Map
 *
 *  @brief	Get a float from the Map
 *  @fn		rval = samp_getFloatFromMap (Map map, char *key)  
 *
 *  @param  map		handle to Map object
 *  @param  key		Map key
 *  @return 		floating point value
 */
float
samp_getFloatFromMap (Map map, char *key)  
{
    double dval = 0.0;
    xr_getDoubleFromStruct (map, key, &dval);

    return ((float) dval);
}
