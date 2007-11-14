# -*- coding: iso-8859-1 -*-
"""
	MoinMoin - occam-pi Source Parser

    @copyright: 2006 by Fred Barnes <F.R.M.Barnes@kent.ac.uk>
    @license: GNU GPL, see COPYING for details.

    based on java.py by Taesu Pyo, 2002

css:

pre.occampiarea		{font-style: sans-serif; color: #000000; }

pre.occampiarea span.ID		{ color: #000000; }
pre.occampiarea span.Char	{ color: #004080; }
pre.occampiarea span.Comment	{ color: #808080; }
pre.occampiarea span.Number	{ color: #008080; font-weight: bold; }
pre.occampiarea span.String	{ color: #004080; }
pre.occampiarea span.SPChar	{ color: #0000C0; }
pre.occampiarea span.ResWord	{ color: #4040ff; font-weight: bold; }
pre.occampiarea span.ConsWord	{ color: #008080; font-weight: bold; }
pre.occampiarea span.ResWord2	{ color: #0080ff; font-weight: bold; }
pre.occampiarea span.Special	{ color: #0000ff; }
pre.occampiarea span.Preprc	{ color: #804000; }
pre.occampiarea span.Folded	{ color: #a0f0f0; background-color: #202020; }


"""

from MoinMoin.util.ParserBase import ParserBase

Dependencies = []

class Parser (ParserBase):

    parsername = "ColorizedOccampi"
    extensions = ['.occ']
    Dependencies = []

    def setupRules (self):
        ParserBase.setupRules (self)

	self.addRule ("Comment","--.*$")
	self.addRulePair ("String", '"', r'$|[^\*]"')
	self.addRule ("Char", r"'\*.'|'[^\*]'")
	self.addRule ("Number", r"[0-9](\.[0-9]*)?(E[+-][0-9])?")
	self.addRule ("Preprc", r"#[A-Z]*")
	reserved_words = ['PROC', 'FUNCTION', 'VAL', 'BOOL', 'BYTE', 'INT16', 'INT32', 'INT64',
	'INT', 'REAL32', 'REAL64', 'CHAR', 'SKIP', 'STOP', 'PAR', 'SEQ', 'ALT', 'IF', 'CASE', 'ELSE',
	'FOR', 'TO', 'SIZE', 'IN', 'AT', 'WORKSPACE', 'VECTORSPACE', 'MOBILESPACE', 'WHILE', 'CHAN', 'PORT',
	'INCLUDE', 'PRAGMA', 'EXTERNAL', 'USE', 'PLUS', 'MINUS', 'TIMES']
	self.addRule ("ID", "[a-zA-Z\.][0-9a-zA-Z\.]*")
	self.addRule ("SPChar", r"[><=~&:;\[\]+-?!]")
	self.addRule ("Folded", r"\.\.\.  .*$")

        self.addReserved(reserved_words)
	constant_words = ['TRUE', 'FALSE']
	special_words = ['NATIVELIB', 'USES', 'INCLUDES', 'NAMESPACE']
        self.addConstant(constant_words)
	self.addWords(special_words, 'Special')

