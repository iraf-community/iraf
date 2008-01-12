# XALLOC.H -- Defines for xalloc.x (the device allocation package).

define DV_DEVFREE	1	# device is free and can be allocated
define DV_DEVALLOC	2	# device is already allocated
define DV_DEVINUSE	3	# device is in use by someone else
define DV_DEVNOTFOUND	4	# device not in device table
