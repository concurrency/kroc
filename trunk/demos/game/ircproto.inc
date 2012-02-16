--
--	ircproto.inc -- IRC protocol definition for occam
--	Copyright (C) 2002 Fred Barnes <frmb@kent.ac.uk>
--	derived largely from RFC-1459.  I.Oikarien, D.Reed, May 1993
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


--{{{  some basic protocols
PROTOCOL ICB IS INT::[]BYTE:
--}}}  
--{{{  constants
VAL INT DEF.IRC.PORT IS 6667:
VAL INT MAX.MESSAGE.SIZE IS 512:
--}}}  
--{{{  protocol components used in IRC client->server messages
-- where a piece of protocol ICB is optional (denoted []), send 0::""
PROTOCOL IRC.CS.PROTO
  CASE
    --{{{  connection/server related
    connect; ICB; INT			-- hostname; port
    isconnected
    --}}}  
    --{{{  registration
    pass; ICB				-- password
    nick; ICB; INT			-- nickname; hop-count
    user; ICB; ICB; ICB; ICB		-- username; hostname; servername; realname
    --}}}  
    --{{{  IRC operator
    oper; ICB; ICB			-- username; password
    --}}}  
    --{{{  quit
    quit; ICB				-- message
    --}}}  
    --{{{  channel operations
    join; ICB; ICB			-- channel[; key]
    part; ICB				-- channel
    mode; ICB; ICB			-- channel/user; mode-data
    topic; ICB; ICB			-- channel[; topic]
    names; ICB				-- [channel]
    list; ICB				-- [channel]
    invite; ICB; ICB			-- nickname; channel
    kick; ICB; ICB; ICB			-- channel; user[; message]
    --}}}  
    --{{{  server operations
    version; ICB			-- [server]
    stats; ICB; ICB			-- [query[; server]]
    links; ICB; ICB			-- [[remote server;] server mask]
    time; ICB				-- [server]
    sconnect; ICB; ICB			-- server[; port]
    trace; ICB				-- [server]
    admin; ICB				-- [server]
    info; ICB				-- [server]
    --}}}  
    --{{{  messages
    privmsg; ICB; ICB			-- recipient; text
    notice; ICB; ICB			-- nickname; text
    --}}}  
    --{{{  user queries
    who; ICB; BOOL			-- [name[; opers-only?]]
    whois; ICB; ICB			-- [server]; nickmask
    whowas; ICB; ICB; ICB		-- nickname[; count[; server]]
    --}}}  
    --{{{  miscellaneous
    kill; ICB; ICB			-- nickname; comment
    ping; ICB; ICB			-- server[; server2]     (things deviate here..)
    pong; ICB; ICB			-- daemon[; daemon2]
    error; ICB				-- message
    --}}}  
    --{{{  optionals
    away; ICB				-- [message]
    rehash
    restart
    summon; ICB; ICB			-- user[; server]
    users; ICB				-- [server]
    wallops; ICB			-- message
    userhost; ICB			-- nickname
    ison; ICB				-- nickname
    --}}}  
:
--}}}  
--{{{  protocol components used in IRC server->client messages
--{{{  error codes
VAL INT ERR.NOSUCHNICK IS 401:
VAL INT ERR.NOSUCHSERVER IS 402:
VAL INT ERR.NOSUCHCHANNEL IS 403:
VAL INT ERR.CANNOTSENDTOCHAN IS 404:
VAL INT ERR.TOOMANYCHANNELS IS 405:
VAL INT ERR.WASNOSUCHNICK IS 406:
VAL INT ERR.TOOMANYTARGETS IS 407:
VAL INT ERR.NOORIGIN IS 409:
VAL INT ERR.NORECIPIENT IS 411:
VAL INT ERR.NOTEXTTOSEND IS 412:
VAL INT ERR.NOTOPLEVEL IS 413:
VAL INT ERR.WILDTOPLEVEL IS 414:
VAL INT ERR.UNKNOWNCOMMAND IS 421:
VAL INT ERR.NOMOTD IS 422:
VAL INT ERR.NOADMININFO IS 423:
VAL INT ERR.FILEERROR IS 424:
VAL INT ERR.NONICKNAMEGIVEN IS 431:
VAL INT ERR.ERRONOUSNICKNAME IS 432:
VAL INT ERR.NICKNAMEINUSE IS 433:
VAL INT ERR.NICKCOLLISION IS 436:
VAL INT ERR.USERNOTINCHANNEL IS 441:
VAL INT ERR.NOTONCHANNEL IS 442:
VAL INT ERR.USERONCHANNEL IS 443:
VAL INT ERR.NOLOGIN IS 444:
VAL INT ERR.SUMMONDISABLED IS 445:
VAL INT ERR.USERDISABLED IS 446:
VAL INT ERR.NOTREGISTERED IS 451:
VAL INT ERR.NEEDMOREPARAMS IS 461:
VAL INT ERR.ALREADYREGISTERED IS 462:
VAL INT ERR.NOPERMFORHOST IS 463:
VAL INT ERR.PASSWDMISPATCH IS 464:
VAL INT ERR.YOURBANNEDCREEP IS 465:
VAL INT ERR.KEYSET IS 467:
VAL INT ERR.CHANNELISFULL IS 471:
VAL INT ERR.UNKNOWNMODE IS 472:
VAL INT ERR.INVITEONLYCHAN IS 473:
VAL INT ERR.BANNEDFROMCHAN IS 474:
VAL INT ERR.BADCHANNELKEY IS 475:
VAL INT ERR.NOPRIVILEGES IS 481:
VAL INT ERR.CHANOPRIVNEEDED IS 482:
VAL INT ERR.CANTKILLSERVER IS 483:
VAL INT ERR.NOOPERHOST IS 491:
VAL INT ERR.UMODEUNKNOWNFLAG IS 501:
VAL INT ERR.USERSDONTMATCH IS 502:
--}}}  
--{{{  command responses
VAL INT RPL.NONE IS 300:
VAL INT RPL.USERHOST IS 302:
VAL INT RPL.ISON IS 303:
VAL INT RPL.AWAY IS 304:
VAL INT RPL.UNAWAY IS 305:
VAL INT RPL.NOWAWAY IS 306:
VAL INT RPL.WHOISUSER IS 311:
VAL INT RPL.WHOISSERVER IS 312:
VAL INT RPL.WHOISOPERATOR IS 313:
VAL INT RPL.WHOISIDLE IS 317:
VAL INT RPL.ENDOFWHOIS IS 318:
VAL INT RPL.WHOISCHANNELS IS 319:
VAL INT RPL.WHOWASUSER IS 314:
VAL INT RPL.ENDOFWHOWAS IS 369:
VAL INT RPL.LISTSTART IS 321:
VAL INT RPL.LIST IS 322:
VAL INT RPL.LISTEND IS 323:
VAL INT RPL.CHANNELMODEIS IS 324:
VAL INT RPL.NOTOPIC IS 331:
VAL INT RPL.TOPIC IS 332:
VAL INT RPL.INVITING IS 341:
VAL INT RPL.SUMMONING IS 342:
VAL INT RPL.VERSION IS 351:
VAL INT RPL.WHOREPLY IS 352:
VAL INT RPL.ENDOFWHO IS 315:
VAL INT RPL.NAMREPLY IS 353:
VAL INT RPL.ENDOFNAMES IS 366:
VAL INT RPL.LINKS IS 364:
VAL INT RPL.ENDOFLINKS IS 365:
VAL INT RPL.BANLIST IS 367:
VAL INT RPL.ENDOFBANLIST IS 368:
VAL INT RPL.INFO IS 371:
VAL INT RPL.ENDOFINFO IS 374:
VAL INT RPL.MOTDSTART IS 375:
VAL INT RPL.MOTD IS 372:
VAL INT RPL.ENDOFMOTD IS 376:
VAL INT RPL.YOUREOPER IS 381:
VAL INT RPL.REHASHING IS 382:
VAL INT RPL.TIME IS 391:
VAL INT RPL.USERSSTART IS 392:
VAL INT RPL.USERS IS 393:
VAL INT RPL.ENDOFUSERS IS 394:
VAL INT RPL.NOUSERS IS 395:
VAL INT RPL.TRACELINK IS 200:
VAL INT RPL.TRACECONNECTING IS 201:
VAL INT RPL.TRACEHANDSHAKE IS 202:
VAL INT RPL.TRACEUNKNOWN IS 203:
VAL INT RPL.TRACEOPERATOR IS 204:
VAL INT RPL.TRACEUSER IS 205:
VAL INT RPL.TRACESERVER IS 206:
VAL INT RPL.TRACENEWTYPE IS 208:
VAL INT RPL.TRACELOG IS 261:
VAL INT RPL.STATSLINKINFO IS 211:
VAL INT RPL.STATSCOMMANDS IS 212:
VAL INT RPL.STATSCLINE IS 213:
VAL INT RPL.STATSNLINE IS 214:
VAL INT RPL.STATSILINE IS 215:
VAL INT RPL.STATSKLINE IS 216:
VAL INT RPL.STATSYLINE IS 218:
VAL INT RPL.ENDOFSTATS IS 219:
VAL INT RPL.STATSLLINE IS 241:
VAL INT RPL.STATSUPTIME IS 242:
VAL INT RPL.STATSOLINE IS 243:
VAL INT RPL.STATSHLINE IS 244:
VAL INT RPL.UMODEIS IS 221:
VAL INT RPL.LUSERCLIENT IS 251:
VAL INT RPL.LUSEROP IS 252:
VAL INT RPL.LUSERUNKNOWN IS 253:
VAL INT RPL.LUSERCHANNELS IS 254:
VAL INT RPL.LUSERME IS 255:
VAL INT RPL.ADMINME IS 256:
VAL INT RPL.ADMINLOC1 IS 257:
VAL INT RPL.ADMINLOC2 IS 258:
VAL INT RPL.ADMINEMAIL IS 259:
--}}}  
PROTOCOL IRC.SC.PROTO
  CASE
    --{{{  connection replies
    connected; BOOL			-- successful-connection ?
    isconnected; BOOL			-- is-connected ?
    disconnect
    --}}}  
    --{{{  error replies
    error; ICB; INT; ICB		-- originator; code; message
    --}}}  
    --{{{  command responses
    repl; ICB; INT; ICB			-- originator; code; message
    --}}}  
    --{{{  other numeric replies
    unknown; ICB; INT; ICB		-- originator; code; message
    --}}}  
    --{{{  things happening on the channel
    notice; ICB; ICB; ICB		-- originator; target; message
    privmsg; ICB; ICB; ICB		-- originator; target; message
    join; ICB; ICB			-- originator; channel
    part; ICB; ICB			-- originator; channel
    ping; ICB; ICB			-- server[; server]  (on ircd, this is a number)
    --}}}  
    --{{{  other replies
    unprocessed; ICB; ICB		-- originator; data
    --}}}  
:
--}}}  

