Sample layout resources from various app-defaults files.
----------------------------

*layout: vertical { \
		NormalSpace = (25 % of height cards) \
		NormalAdjust = 25% \
		horizontal {\
			$NormalSpace < +infinity -$NormalSpace > \
			label1 label2 label3 \
			$NormalSpace < +infinity -$NormalSpace > \
		} \
		horizontal {\
			cards < * +infinity -$NormalAdjust > \
			0 < +infinity > \
		} \
	}

*menuBar.layout: vertical { \
	5 < -5 >\
	horizontal { \
		5 < -5 > \
		fileMenuButton \
		5 < -5 > \
		newGame \
		5 < -5 > \
		undo \
		5 < -5 > \
		hint \
		5 < -5 > \
		score \
		5 < -5 > \
		pileAll \
		5 <+inf -inf> \
		baseRank \
		5 < -5 > \
	} \
	5 < -5 > \
}

*frame.layout: vertical {\
	-1 \
	horizontal {\
		-1 \
		menuBar < +inff -100% * >\
		-1 \
	} \
	10 < -inf > \
	horizontal { \
		0 < +inf -inf > \
		vertical { \
			0 < +inf -inf > \
			logo \
			0 < +inf -inf > \
		} \
		0 < +inf -inf > \
		foundation < -100% * -90% > \
		10 < -inf > \
	} \
	10 < -inf > \
	horizontal {\
		10 < -inf > \
		vertical { \
			horizontal { \
				stock < -75% * -90% > \
				0 < +inf > \
			} \
			10 < -inf > \
			talon < -75% * -90% > \
		} \
		10 < +inf -inf > \
		tableau < -50% * +inf -50% > \
		10 < -inf > \
	} \
	horizontal { \
		-1 \
		message < +inff -100% * > \
		-1 \
	} \
	-1 \
}

*menuBar.layout: vertical { \
	5 < -5 >\
	horizontal { \
		5 < -5 > \
		fileMenuButton \
		5 < -5 > \
		newGame \
		5 < -5 > \
		undo \
		5 < -5 > \
		hint \
		5 < -5 > \
		score \
		5 < -5 > \
		pileAll \
		0 <+inf -inf> \
	} \
	5 < -5 > \
}

*frame.layout: vertical {\
	-1 \
	horizontal {\
		-1 \
		menuBar < +inff -100% * >\
		-1 \
	} \
	10 < -inf > \
	horizontal { \
		10 < -inf > \
		deck < -75% * -90% > \
		10 < +inf -inf > \
		piles < -100% * -90% > \
		10 < -inf > \
	} \
	10 < -inf > \
	stacks < -50% * +inf -50% > \
	horizontal { \
		-1 \
		message < +inff -100% * > \
		-1 \
	} \
	-1 \
}

*menuBar.layout: vertical { \
	5 < -5 >\
	horizontal { \
		5 < -5 > \
		fileMenuButton \
		5 < -5 > \
		deal \
		5 < -5 > \
		newGame \
		5 < -5 > \
		undo \
		5 < -5 > \
		score \
		0 <+inf -inf> \
	} \
	5 < -5 > \
}

*frame.layout: vertical {\
	-1 \
	horizontal {\
		-1 \
		menuBar < +inff -100% * >\
		-1 \
	} \
	10 < -inf > \
	cards < +100% -100% * +100% -100% > \
	horizontal { \
		-1 \
		dealDisplay < -100% * > \
		-1 \
		message < +inff -100% * > \
		-1 \
	} \
	-1 \
}

*menuBar.layout: vertical { \
	5 < -5 >\
	horizontal { \
		5 < -5 > \
		fileMenuButton \
		5 < -5 > \
		newGame \
		5 < -5 > \
		undo \
		5 < -5 > \
		hint \
		5 < -5 > \
		score \
		0 <+inf -inf> \
	} \
	5 < -5 > \
}

*frame.layout: vertical {\
	-1 \
	horizontal {\
		-1 \
		menuBar < +inff -100% * >\
		-1 \
	} \
	10 < -inf > \
	horizontal { \
		10 < -inf > \
		deck < -75% * -90% > \
		10 < +inf -inf > \
		piles < -100% * -90% > \
		10 < -inf > \
	} \
	10 < -inf > \
	stacks < -50% * +inf -50% > \
	horizontal { \
		-1 \
		message < +inff -100% * > \
		-1 \
	} \
	-1 \
}

*menuBar.layout: vertical { \
	5 < -5 >\
	horizontal { \
		5 < -5 > \
		fileMenuButton \
		5 < -5 > \
		newGame \
		5 < -5 > \
		undo \
		5 < -5 > \
		hint \
		5 < -5 > \
		score \
		0 <+inf -inf> \
	} \
	5 < -5 > \
}

*frame.layout: vertical {\
	-1 \
	horizontal {\
		-1 \
		menuBar < +inff -100% * >\
		-1 \
	} \
	10 < -inf > \
	horizontal { \
		10 < -inf > \
		vertical { \
			stacks < -50% * +inf -50% > \
			10 < -inf > \
			horizontal { \
				vertical { \
					10 < +inf -inf > \
					draw < +inf -100% * -90% > \
				} \
				10 < -inf > \
				vertical { \
					deckCount < +inf -inf * > \
					deck < -75% * -90% > \
				} \
			} \
		} \
		10 < -inf > \
		vertical { \
			piles < -50% * -50% > \
			0 < +inf > \
		} \
		10 < -inf > \
	} \
	horizontal { \
		-1 \
		message < +inff -100% * > \
		-1 \
	} \
	-1 \
}

*menuBar.layout: vertical { \
	5 < -5 >\
	horizontal { \
		5 < -5 > \
		fileMenuButton \
		5 < -5 > \
		newGame \
		5 < -5 > \
		undo \
		5 < -5 > \
		hint \
		5 < -5 > \
		score \
		5 < -5 > \
		pileAll \
		0 <+inf -inf> \
	} \
	5 < -5 > \
}

*frame.layout: vertical {\
	-1 \
	horizontal {\
		-1 \
		menuBar < +inff -100% * >\
		-1 \
	} \
	10 < -inf > \
	horizontal { \
		10 < -inf > \
		suits < -75% * -90% > \
		10 < +inf -inf > \
		piles < -100% * -90% > \
		10 < -inf > \
	} \
	10 < -inf > \
	stacks < -50% * +inf -50% > \
	horizontal { \
		-1 \
		message < +inff -100% * > \
		-1 \
	} \
	-1 \
}

*layout.layout: horizontal {\n \
	-1 \n \
	vertical { \n \
		-1 \n \
		reversi < +inff -100% * +inff -100% > \n \
		-1 \n \
		error < +inff -100% * > \
		-1 \n \
	} \n \
	vertical { \n \
		10 < +10 -100% > \n \
		horizontal { \
			5 < +inf -100% > \n \
			quit \n \
			5 < +inf -100% > \n \
			hint \n \
			5 < +inf -100% > \n \
			undo \n \
			5 < +inf -100% > \n \
			restart \n \
			5 < +inf -100% > \n \
		} \n \
		10 < +10 -100% > \n \
		horizontal { \n \
			5 < -100% > \n \
			vertical { \n \
				horizontal { \n \
					playerLabel \n \
					0 < +inf > \n \
				} \
				10 < +10 - 100% > \n \
				horizontal { \n \
					playWhite \n \
					5 < +inf -100% > \n \
					playBlack \n \
					5 < +inf -100% > \n \
					playBoth \n \
					5 < +inf -100% > \n \
					playNeither \n \
				} \n \
			} \n \
			5 < +inf -100% > \n \
		} \n \
		10 < +10 -100% > \n \
		horizontal { \n \
			5 < -100% > \n \
			vertical { \
				0 < +inf > \n \
				levelLabel \n \
				0 < +inf > \n \
			} \n \
			10 < -100% > \n \
			levelValue \n \
			5 < +inf -100% > \n \
		} \n \
		10 < +10 -100% > \n \
		horizontal { \n \
			5 < -100% > \n \
			turn \n \
			5 < +inf -100% > \n \
		} \n \
		10 < +1000 -100% > \n \
	} \n \
}

*layout.layout: vertical {\
MenuBarBorderWidth = 1\
HumanHandBorderWidth = 1\
-$MenuBarBorderWidth \
horizontal { \
	-$MenuBarBorderWidth menuBar < +inf -100% * > -$MenuBarBorderWidth \
} \
horizontal { \
	vertical { \
		MinPlayHeight = 20 \
		MinPlayWidth = 550 \
		PlayVertShrink = (100% - $MinPlayHeight) \
		PlayHorzShrink = (100% - $MinPlayWidth) \
		computerPlay < -$PlayHorzShrink * +inf -$PlayVertShrink > \
		horizontal { \
			-1 \
			vertical { \
				computerMiles < +1 -100% * >\
				horizontal { \
					0 < +inf > \
					vertical { \
						message \
						errors \
						0 < +1 > \
					}\
					vertical { \
						deckCount < +inf -100% * >\
						deck < -100% * -100% > \
						0 < +1 > \
					} \
					(width deck / 4) < -inf > \
				} \
				humanMiles < +1 -100% * > \
			} \
			10 < -inf > \
			score < +1 * +1 > \
		} \
		humanPlay < -$PlayHorzShrink * +inf -$PlayVertShrink > \
		horizontal { \
			-$HumanHandBorderWidth \
			humanHand < -$PlayHorzShrink * -$PlayVertShrink > \
		} \
		-$HumanHandBorderWidth \
	} \
	vertical { \
		computerSafeties < -150% * -100% > \
		computerSafetyLabel < +inf -inf * > \
		0 < +inf >\
		humanSafeties < -150% * -100% > \
		humanSafetyLabel < +inf -inf * >\
	} \
	0 < +inf > \
} \
}

*yesOrNoDialog.layout: vertical {\
	Spacing = (50 % of height yesOrNoLabel) \
	$Spacing < +inf -inf > \
	yesOrNoLabel \
	$Spacing < +inf -inf > \
	horizontal { \
		$Spacing < -inf > \
		yesOrNoOk \
		$Spacing < +inf -inf > \
		yesOrNoNo \
		$Spacing < -inf > \
	} \
	$Spacing < +inf -inf > \
}

*promptedDialog.layout: vertical {\
	Spacing = (20 % of height promptedLabel) \
	$Spacing < +inf -inf > \
	promptedLabel \
	$Spacing < +inf -inf > \
	horizontal { \
		$Spacing < -inf > \
		promptedValue \
		$Spacing < +inf -inf > \
	} \
	horizontal { \
		$Spacing < -inf > \
		promptedOk \
		$Spacing < +inf -inf > \
		promptedCancel \
		$Spacing < -inf > \
	} \
	$Spacing < +inf -inf > \
}
