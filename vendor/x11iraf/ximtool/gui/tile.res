
set Resources(tile_panel) { \

    *tileOpts.frameType:		    	chiseled
    *tileOpts.frameWidth:		    	2
    *tileOpts.outerOffset:			5
    *tileOpts.innerOffset:			7
    *tileOpts*shrinkToFit:			True
    *tileOpts*borderWidth:		      	0

    *toptLayout.layout:	vertical { \
	horizontal { -6 tFramesG < +inf -inf * > -6 } \
	horizontal { \
	    -1 \
	    horizontal { -6 tileMode < * +inf -inf > -6 } \
	    vertical { \
		horizontal { -3 userOrientG < +inf -inf * > -3 } \
		horizontal { -3 fillStyle   < +inf -inf * > -3 } \
		horizontal { -3 tileLabel   < +inf -inf * > -3 } \
		-7 \
		horizontal { -3 geomFrame   < +inf -inf * > -3 } \
	    } \
	    -1 \
	} \
	-4 \
    }
    *tileMode.label:				Tile Mode
    *tileMode.outerOffset:			7
    *tileMode.innerOffset:			5
    *tileMode*location:				0 0 150 20
    *tileMode*TextToggle.outerOffset:		2
    *tileMode*TextToggle.innerOffset:		1
    *tileMode*TextToggle.frameWidth:		0
    *tileMode*TextToggle.leftMargin:		10
    *tileMode*TextToggle.rightMargin:		20
    *tileMode*TextToggle.onIcon:		diamond1s
    *tileMode*TextToggle.offIcon:		diamond0s
    *tileMode*TextToggle.highlightColor:	yellow

    *tileDisabled.label:			Disabled
    *tileManual.label:				Manual
    *tileBest.label:				Best
    *tileSquare.label:				Square
    *tileHorizontal.label:			Horizontal
    *tileVertical.label:			Vertical
    *tileRow.label:				One Row
    *tileCol.label:				One Column

    *fillStyle.label:				Fill Style
    *fillStyle.location:			0 0 160 30
    *fillStyle.outerOffset:			7
    *fillStyle.innerOffset:			5
    *fillStyle.rows:				1
    *fillStyle*selectionStyle:			multi
    *fillStyle*outerOffset:			0
    *fillStyle*innerOffset:			1
    *fillStyle*leftMargin:			7
    *fillStyle*onIcon:				square1s
    *fillStyle*offIcon:				square0s
    *fillStyle*highlightColor:			yellow
    *fillStyle.TextToggle.frameWidth:		0
    *fillStyle.TextToggle.location:		0 0 85 23
    *byCols.label:				Fill by Columns
    *bottomUp.label:				Fill from Bottom

    *tileLabel.label:				Tile Labels
    *tileLabel.location:			0 0 175 30
    *tileLabel.outerOffset:			7
    *tileLabel.innerOffset:			5
    *tileLabel.rows:				1
    *tileLabel*selection:			-1
    *tileLabel*outerOffset:			0
    *tileLabel*innerOffset:			1
    *tileLabel*leftMargin:			7
    *tileLabel*onIcon:				square1s
    *tileLabel*offIcon:				square0s
    *tileLabel*highlightColor:			yellow
    *tileLabel.TextToggle.frameWidth:		0
    *tileLabel.TextToggle.location:		0 0 85 23
    *labelFrames.label:				Frameno
    *labelImname.label:				Img Name
    *labelTitles.label:				Img Title

    *geomFrame.frameWidth:			0
    *geomFrame.frameType:			sunken
    *geomFrame.outerOffset:			7
    *geomFrame.innerOffset:			4
    *tileGeometry.width:			220
    *tileGeometry.height;			37
    *tileGeometry.background:			#c4c4c4
    *tileGeometry.font:				7x13bold
    *tileGeometry.label:			Tile Geometry:  1 x 2

    *userOrientG.label:				Manual Configuration
    *userOrientG.height:			90
    *userOrientG.width:				220
    *userOrientG.outerOffset:			7
    *userOrientG.innerOffset:			5
    *userOrientG.shrinkToFit:			True
    *userOrientG*Frame.frameWidth:		1
    *userOrientG*Frame.frameType:		sunken
    *userOrientG*Text.height:			21
    *userOrientG*Text*editType:			edit
    *userOrientL.borderWidth:			0
    *userOrientL.layout: vertical { \
	1 \
	horizontal { 18 nrowLab < +50% -inf * >  nrFrame < +inf -inf * > 5 } \
	1 \
	horizontal { ncolLab < +50% -inf * > ncFrame < +inf -inf * > 5 } \
    }
    *nrowLab.justify:				right
    *ncolLab.justify:				right

    *nrLayout.layout: horizontal {nrdecrease nrtext < +inf -inf * > nrincrease}
    *nrdecrease.direction:			left
    *nrincrease.direction:			right
    *nrowLab.label:				Tile Rows:
    *nrtext.background:				#adadad
    *nrtext.justify:				center
    *nrtext.font:				7x13bold
    *nrtext.label:				1

    *ncLayout.layout: horizontal {ncdecrease nctext < +inf -inf * > ncincrease}
    *ncdecrease.direction:			left
    *ncincrease.direction:			right
    *ncolLab.label:				Tile Columns:
    *nctext.background:				#adadad
    *nctext.justify:				center
    *nctext.font:				7x13bold
    *nctext.label:				2

    *tFramesG.label:				Tile Frames
    *tFramesG.outerOffset:			7
    *tFramesG.innerOffset:			7
    *tFrames.borderWidth:			0
    *tFrames*Toggle.height:			17
    *tFrames.layout: horizontal	{ \
	tAll \
	2 \
	tFrame1  tFrame2  tFrame3  tFrame4  tFrame5  \
	tFrame6  tFrame7  tFrame8  tFrame9  tFrame10 \
	tFrame11 tFrame12 tFrame13 tFrame14 tFrame15 tFrame16 \
	2 \
	tNone \
    }
    *tAll.label:				All\ 
    *tNone.label:				None
}

