
set Resources(warning) { \

    ! ---------------------
    ! WARNING dialog.
    ! ---------------------
    *warning.geometry:				+400+300
    *warning*borderWidth:			0
    *warning*TextBox.frameWidth:		0

    *warn.layout: vertical { \
	5 < -5	> \
	horizontal { 5 warnFrame < +inf * +inf > 5 } \
	5 < -5	> \
	horizontal { \
	    10 \
	    warnOk < +inf * > \
	    5 < +inf -5 > \
	    warnCancel < +inf * > \
	    10 \
	} \
	5 < -5	> \
    }
    *warnOk.label:				Okay
    *warnCancel.label:				Cancel

    *WFlayout.layout: horizontal { \
	5 < -5	> \
	vertical { 5 < +inf -5 > warnIcon 5 < +inf -5 > } \
	5 < -5	> \
	warnText < +inf -inf *	+inf -inf > \
	5 < -5	> \
    }

    *warnLabel.label:				Warning
    *warnLabel.width:				300
    *warnLabel.height:				20
    *warnFrame.frameType:			sunken
    *warnFrame.frameWidth:			2
    *warnIcon.location:				0 0 40 40
    *warnIcon.image:				WARNING
    *warnText.label:				generic warning text
    *warnText.width:				270
    *warnText.height:				60
    *warnText.background:			gray77
}



