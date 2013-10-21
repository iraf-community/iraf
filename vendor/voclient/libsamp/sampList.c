/**
 *  SAMPLIST.C -- (Internal) interface to support the List structure.
 *
 *                list = samp_newList  ()
 *                      samp_freeList  (List list)
 *		   len = samp_listLen  (List list)
 *        
 *               samp_setStringInList  (List list, char *value)
 *                  samp_setMapInList  (List list, Map map)
 *                 samp_setListInList  (List list1, List list2)
 *                  samp_setIntInList  (List list, int val)
 *                samp_setFloatInList  (List list, float val)
 *  
 *       str = samp_getStringFromList  (List list, int index)  
 *          map = samp_getMapFromList  (List list, int index)  
 *        list = samp_getListFromList  (List list, int index)  
 *         ival = samp_getIntFromList  (List list, int index)  
 *       rval = samp_getFloatFromList  (List list, int index)  
 *
 *  @brief      (Internal) interface to support the List structure.
 *  
 *  @file       sampList.c
 *  @author     Mike Fitzpatrick
 *  @date       7/10/09
 */


#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "samp.h"




/**
 *  SAMP_NEWLIST -- Create a new List object
 *
 *  @brief	Create a new List object
 *  @fn		int samp_newList (void)
 *
 *  @return		handle to List object
 */
handle_t
samp_newList ()
{
    return ( (handle_t) xr_newArray () );
}


/**
 *  SAMP_FREELIST -- Free the given List object
 *
 *  @brief	Free the given List object
 *  @fn		void samp_freeList (List list)
 *
 *  @param  list	List object handle
 *  @return		nothing
 */
void
samp_freeList (List list)
{
    if (list >= 0)
        xr_freeArray (list);
}


/**
 *  SAMP_LISTLEN -- Get number of elements in a List.
 *
 *  @brief	Get number of elements in a List.
 *  @fn		int  = samp_listLen (List list)
 *
 *  @param  list	List object handle
 *  @return		nothing
 */
int
samp_listLen (List list)
{
    return ( xr_arrayLen (list) );
}


/********************************
 *  SET Methods
 ********************************/

/**
 *  SAMP_SETSTRINGINLIST -- Set a string in a List (append)
 *
 *  @brief	Set a string in a List (append)
 *  @fn		void samp_setStringInList (List list, char *value)
 *
 *  @param  list	List object handle
 *  @param  value	string value to set
 *  @return		nothing
 */
void
samp_setStringInList (List list, char *value)
{
    xr_setStringInArray (list, (value ? value : ""));
}


/**
 *  SAMP_SETMAPINLIST -- Set a Map in a List (append)
 *
 *  @brief	Set a Map in a List (append)
 *  @fn		void samp_setMapInList (List list, Map map)
 *
 *  @param  list	List object handle
 *  @param  map		Map object to be set
 *  @return		nothing
 */
void
samp_setMapInList (List list, Map map)
{
    xr_setStructInArray (list, map);
}


/**
 *  SAMP_SETLISTINLIST -- Set a List in another List (append)
 *
 *  @brief	Set a List in another List (append)
 *  @fn		void samp_setListInList (List list1, List list2)
 *
 *  @param  list1	List object handle
 *  @param  list2	List to be appended
 *  @return		nothing
 */
void
samp_setListInList (List list1, List list2)
{
    xr_setArrayInArray (list1, list2);
}


/**
 *  SAMP_SETINTINLIST -- Set an Int in a List (append)
 *
 *  @brief	Set an Int in a List (append)
 *  @fn		void samp_setIntInList (List list, int ival)
 *
 *  @param  list1	List object handle
 *  @param  ival	Integer value to be appended
 *  @return		nothing
 */
void
samp_setIntInList (List list, int ival)
{
    xr_setIntInArray (list, ival);
}


/**
 *  SAMP_SETFLOATINLIST -- Set a Float in a List (append)
 *
 *  @brief	Set a Float in a List (append)
 *  @fn		void samp_setFloatInList (List list, float rval)
 *
 *  @param  list1	List object handle
 *  @param  rval	Float value to be appended
 *  @return		nothing
 */
void
samp_setFloatInList (List list, float rval)
{
    xr_setDoubleInArray (list, (double) rval);
}




/********************************
 *  GET Methods
 ********************************/

/**
 *  SAMP_GETSTRINGFROMLIST -- Get a string from the List
 *
 *  @brief	Get a string from the List
 *  @fn		char *samp_getStringFromList (List list, int index)  
 *
 *  @param  list	List object handle
 *  @param  index	List index containing the string
 *  @return		character string
 */
char *
samp_getStringFromList (List list, int index)  
{
    static char  buf[SZ_SBUF], *res = buf;

    memset (buf, 0, SZ_SBUF);
    xr_getStringFromArray (list, index, &res);

    return ((res));
}


/**
 *  SAMP_GETMAPFROMLIST -- Get a Map from the List
 *
 *  @brief	Get a Map from the List
 *  @fn		Map samp_getMapFromList (List list, int index)  
 *
 *  @param  list	List object handle
 *  @param  index	List index containing the Map
 *  @return		Map handle
 */
Map
samp_getMapFromList (List list, int index)  
{
    Map map = 0;
    xr_getStructFromArray (list, index, &map);

    return (map);
}


/**
 *  SAMP_GETLISTFROMLIST -- Get a List from the List
 *
 *  @brief	Get a List from the List
 *  @fn		List samp_getListFromList (List list, int index)  
 *
 *  @param  list	List object handle
 *  @param  index	List index containing the List
 *  @return		List handle
 */
List
samp_getListFromList (List list, int index)  
{
    List l = 0;
    xr_getArrayFromArray (list, index, &l);

    return (l);
}


/**
 *  SAMP_GETINTFROMLIST -- Get an Int from the List
 *
 *  @brief	Get an Int from the List
 *  @fn		ival =  samp_getListFromList (List list, int index)  
 *
 *  @param  list	List object handle
 *  @param  index	List index containing the desired value
 *  @return		int value
 */
int
samp_getIntFromList (List list, int index)  
{
    int i = 0;
    xr_getIntFromArray (list, index, &i);

    return (i);
}


/**
 *  SAMP_GETFLOATFROMLIST -- Get a Float from the List
 *
 *  @brief	Get a Float from the List
 *  @fn		ival =  samp_getFloatFromList (List list, int index)  
 *
 *  @param  list	List object handle
 *  @param  index	List index containing the desired value
 *  @return		int value
 */
float
samp_getFloatFromList (List list, int index)  
{
    double x = 0.0;
    xr_getDoubleFromArray (list, index, &x);

    return ((float) x);
}
