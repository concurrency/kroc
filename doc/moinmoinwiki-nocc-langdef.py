# -*- coding: iso-8859-1 -*-
"""
	MoinMoin - NOCC language definition file parser

    @copyright: 2007 by Fred Barnes <F.R.M.Barnes@kent.ac.uk>
    @license: GNU GPL, see COPYING for details.


    based on java.py by Taesu Pyo, 2002

"""

from MoinMoin.util.ParserBase import ParserBase

Dependencies = []

class Parser (ParserBase):

    parsername = "ColorizedNOCCLangDef"
    extensions = ['.ldef']
    Dependencies = []

    def setupRules (self):
        ParserBase.setupRules (self)

	self.addRulePair ("String", '"', r'$|[^\*]"')
	self.addRule ("Comment", "#.*$")
	reserved_words = ['INVALID', 'BEFORE', 'AFTER', 'EXPECTED', 'CODE', 'STUCK', 'NONE']
	self.addRule("Preprc", r"^.[A-Z]*")
	self.addRule ("ID", "[a-zA-Z\.][0-9a-zA-Z\.]*")
	self.addRule ("SPChar", r"[><=~&:;\[\]+-?!]")

        self.addReserved(reserved_words)



