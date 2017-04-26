
################################################################################
#  Header Display Callbacks.
################################################################################

set hdrImage		""
set hdrImageId		""
set hdrKeywords		"*"


proc fitsHdrClose args \
{
    global hdr_up

    send hdr_panel unmap
    send imageHeader set on False
    set hdr_up 0
} ; send hdrClose addCallback fitsHdrClose

proc ptFitsHeader {widget type state args} \
{
    global hdr_up

    set hdr_up $state
    if {$hdr_up == 1} {
	send imageHeader set on True
	send hdr_panel map
    } else {
	send imageHeader set on False
	send hdr_panel unmap
    }
}; send imageHeader addCallback ptFitsHeader

proc getHeader { name id } \
{
    global hdrImage hdrImageId hdrKeywords

    set hdrImage $name
    set hdrImageId $id
    send hdrObjMenu set label $name
    send hdrText   setText ""
    send hdrIGText set string ""
    send hdrKGText set string ""

    catch { send wcspix objinfo $hdrImageId $hdrKeywords }
}

proc hdrKeywFilter args \
{
    global hdrImageId hdrKeywords

    set str [ send hFindEntry get string ]
    if {$str != ""} {
	set hdrKeywords $str
	send hdrText setText ""
	catch { send wcspix objinfo $hdrImageId $hdrKeywords }
    }
} ; send hdrFilter addCallback hdrKeywFilter


# Set the image menu in the header panel.
proc setHdrObjMenu { frame args } \
{
    global frameCache ism_enable

    if {! $ism_enable} \
	return

    # Create the menu of images for the header panel.
    set items { }
    foreach i [list $frameCache($frame)] {
        set l [lindex $i 0]
        set lid [lindex $i 1]
        regsub -all {[\[]} $l  "\\\[" l2
	catch {
            lappend items [format "%s f.exec \{getHeader %s %d\}" $l $l2 $lid]
	}
    }
    editMenu objMenu hdrObjMenu $items
}


# Search box for the header.
proc hdrFind args \
{
    set phrase  [send hFindEntry get string]

    if { $phrase != "" } {
        if {[send hdrText searchText $phrase start end forward caseless] > 0} {
            set elid [lindex [lindex $start 0] 0]
            set id   [max 1 [expr $elid - 10] ]
            send hdrText gotoId $id
            send hdrText setSelection $start $end
        } else {
            send warnText set label "Search string not found."
            send warning map
        }
    } else {
        send warnText set label "Warning: No search phrase entered."
        send warning map
    }
} ; foreach w { hFindEntry hdrFind } { send $w addCallback hdrFind }

send hdrClear addCallback { send hFindEntry set string "" }

