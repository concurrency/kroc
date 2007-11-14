/*
 *	Filename functions
 *	Copyright (C) 1990 Inmos Limited
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* Copyright 1990 INMOS Limited */
/* CMSIDENTIFIER
   PLAY:POPEN_C@514.AAAA-FILE;1(20-FEB-92)[FROZEN] */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <string.h>
#ifdef __STDC__
#include <stdlib.h>
#endif
#include "toolkit.h"

/*{{{   PRIVATE int CheckBaseName (FileName)   */
/*{{{   comment   */
/*
-- ----------------------------------------------------------------------------
--
--     Function Name : CheckBaseName
--
--     Input Parameters :
--         char *FileName - file name to be tested for.
--
--     Output Parameters :
--         None.
--
--     Result :
--         int           - TRUE if found specifier, otherwise FALSE.
--
--     DESCRIPTION
--         Returns TRUE if the file name does not have any sort of directory
--         specification in it, otherwise FALSE is returned.
--
-- ----------------------------------------------------------------------------
*/
/*}}}  */
PRIVATE int CheckBaseName (FileName)
    char const *FileName;
{
    while (*FileName != '\0')
    {
        switch (*FileName)
        {
            case '/':
            case '\\':
            case ':':
            case ']':
                return(FALSE);
                break;
            default:
                FileName++;
                break;
        }
    }
    return(TRUE);
}
/*}}}  */
/*{{{   PRIVATE char *FindNextName (PathString, FileName, FullName)   */
/*{{{   comment   */
/*
-- ----------------------------------------------------------------------------
--
--     Function Name : FindNextName
--
--     Input Parameters :
--         char *PathString - start of path string to be searched along.
--         char *FileName   - file name to be added to the path.
--
--     Output Parameters :
--         char *FullName   - full path name of the file to be tested.
--
--     Result :
--         char *           - new start of the path string.
--
--     DESCRIPTION
--         Finds the next directory section from the path string and then
--         concatenates the directory section and file name together to create
--         a new full path name for the file. If there are no more directory
--         sections in the path string then no new full path name is created.
--
-- ----------------------------------------------------------------------------
*/
/*}}}  */
PRIVATE char *FindNextName (PathString, FileName, FullName)
    char *PathString, *FileName, *FullName;
{
    if (*PathString != '\0')
    {
        int FoundDirectory = FALSE;

        while ((! FoundDirectory) && (*PathString != '\0'))
        {
            switch (*PathString)
            {
                case ';':
                case ' ':
                    FoundDirectory = TRUE;
                    while ((*PathString == ';') || (*PathString == ' '))
                        PathString++;
                    break;
                default:
                    *FullName++ = *PathString++;
                    break;
            }
        }
        *FullName = '\0';
        (void) strcat(FullName, FileName);
    }
    return(PathString);
}
/*}}}  */
/*{{{   PUBLIC FILE *pathopen (FileName, PathName, FullName, OpenMode)*/
/*{{{   comment   */
/*
-- ----------------------------------------------------------------------------
--
--     Function Name : pathopen
--
--     Input Parameters :
--         char *FileName - file name to be searched for and opened.
--         char *PathName - name of the path environment variable.
--         int OpenMode   - mode for the file to be opened in.
--
--     Output Parameters :
--         char *FullName - full path name of the file found on the path.
--
--     Result :
--         FILE *         - pointer to file stream, otherwise NULL.
--
--     DESCRIPTION
--         A routine for opening a file in the occam toolset, where you search
--         an environment variable for a list of directories to look in. It
--         first simply tries to open the basic file name. If it cannot and the
--         file name contains a directory specification in it then no path
--         searching is performed.
--
--         Otherwise it translates the environment variable and it scans
--         through the directories on the list specified by the variable (each
--         separated by spaces or semicolons). It adds the file name onto the
--         end of each directory specification in turn, and then trying to open
--         the full path name just created for the file.
--
--         If it succeeds in opening the file (on the path or otherwise), it
--         returns the full path name for the file and a FILE pointer to it. If
--         it cannot find the file (on the path or otherwise) then it returns
--         NULL and no full path name.
--
--         The open mode parameter specifies text, value 0 and binary value 1.
--
-- ----------------------------------------------------------------------------
*/
/*}}}  */
PUBLIC FILE *pathopen (FileName, PathName, FullName, OpenMode)
    char const *FileName, *PathName, *OpenMode;
    char *FullName;
{
  FILE *Stream;
  char *PathString;
  Stream = NULL;
  PathString = NULL;
  if ((FullName != NULL) && (strlen(FileName) != 0))
  {
    if ((Stream = fopen (FileName, OpenMode)) == NULL)
    /*{{{   search path   */
    {
      if ((PathName != NULL) && (strlen(PathName) != 0))
      {
        if (CheckBaseName (FileName))
        {
          if ((PathString = (char *) getenv(PathName)) != NULL)
          {
            while ((Stream == NULL) && (strlen(PathString) != 0))
            {
              PathString = FindNextName(PathString, (char *) FileName, FullName);
              if (strlen(FullName) != 0) Stream = fopen (FullName, OpenMode);
            }
          }
        }
      }
    }
    /*}}}  */
    else (void) strcpy(FullName, FileName);
  }
  if (Stream == NULL) *FullName = '\0';
  return(Stream);
}
/*}}}  */
