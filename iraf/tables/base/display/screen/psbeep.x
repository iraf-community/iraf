# PS_BEEP -- Sound the bell
#
# B.Simon	19-Jan-89	Original

procedure ps_beep ()

begin
	call ps_sendcap ("bl", 1)
end
