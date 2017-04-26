/* GTERM.H -- Global definitions for GTERM.
 */
#define	CURSOR_OFF		3	/* turn cursor off entirely	*/
#define	CURSOR_ON		4	/* turn it back on		*/

#define	GRAPHICS_OFF		0
#define	GRAPHICS_ON		1
#define	GRAPHICS_DISCARD	2


struct	fonttab {		/* Gterm font descriptor. */
	short	pointsize;
	char	ch_xsize, ch_ysize;
	short	win_xsize, win_ysize;
	struct	pixfont *pixfont;
	char	*path;
	char	*label;
};
