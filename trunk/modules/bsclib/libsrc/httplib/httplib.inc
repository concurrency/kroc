--
--	httplib.inc -- #INCLUDE this for the occam HTTP library
--	Copyright (C) 2000 Fred Barnes <frmb2@ukc.ac.uk>
--
--	This program is free software; you can redistribute it and/or modify
--	it under the terms of the GNU General Public License as published by
--	the Free Software Foundation; either version 2 of the License, or
--	(at your option) any later version.
--
--	This program is distributed in the hope that it will be useful,
--	but WITHOUT ANY WARRANTY; without even the implied warranty of
--	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--	GNU General Public License for more details.
--
--	You should have received a copy of the GNU General Public License
--	along with this program; if not, write to the Free Software
--	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
--

VAL HTML.NONE IS 0:
VAL HTML.H1 IS 1:
VAL HTML.H2 IS 2:
VAL HTML.H3 IS 3:
VAL HTML.H4 IS 4:
VAL HTML.TT IS 5:

PROTOCOL HTTP.PROTO
  CASE
    init.ok				-- sends 200 header
    init.redirect			-- sends 302 header
    redirect.to; INT::[]BYTE		-- sends redirect-header (URL)
    content.type; INT::[]BYTE           -- sends content-type string
    end.headers				-- sends blank-line (marks end of headers)
    finish				-- kills the HTMLiser process

    begin.html				-- <HTML>
    end.html				-- </HTML>
    begin.head				-- <HEAD>
    title; INT::[]BYTE			-- <TITLE>...</TITLE>
    refresh; INT::[]BYTE		-- <META HTTP_EQUIV=REFRESH CONTENT="...">
    end.head				-- </HEAD>
    begin.body				-- <BODY>
    begin.body.params; INT::[]BYTE	-- <BODY ...>
    end.body				-- </BODY>
    begin.pre				-- <PRE>
    end.pre				-- </PRE>
    begin.tag; INT::[]BYTE		-- <...>
    end.tag; INT::[]BYTE		-- </...>
    par					-- <P>

    html; INT; INT::[]BYTE		-- html style; text
    strong; INT::[]BYTE			-- <STRONG>...</STRONG>
    line.break				-- <BR>
    sendfile; INT::[]BYTE               -- filename to dump down socket
:

