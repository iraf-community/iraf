/* ALLOC.H -- Status returns for ZDVALL, ZDVOWN.
 */
#define DV_DEVFREE	1	/* device is free and can be allocated	*/
#define DV_DEVALLOC	2	/* device is already allocated		*/
#define DV_DEVINUSE	3	/* device is in use by someone else	*/
#define	DV_ERROR	9	/* software error from alloc.e		*/

#define	D_alloc
