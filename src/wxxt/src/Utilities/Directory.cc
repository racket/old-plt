/*								-*- C++ -*-
 * $Id: Directory.cc,v 1.1 1996/01/10 14:56:50 markus Exp $
 *
 * Purpose: basic file and directory handling
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "wx.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

Bool wxConcatFiles(char *src1, char *src2, char *dest)
{
    char *outfile = wxGetTempFileName("cat");

    FILE *f1, *f2, *fd;
    int ch;

    if (!(f1 = fopen(src1, "rb"))) {
	return FALSE;
    } else if (!(f2 = fopen(src2, "rb"))) {
	fclose(f1); return FALSE;
    } else if (!(fd = fopen(outfile, "wb"))) {
	fclose(f1); fclose(f2); return FALSE;
    }

    while ((ch = getc (f1)) != EOF)
	putc (ch, fd);
    while ((ch = getc (f2)) != EOF)
	putc (ch, fd);

    fclose (f1); fclose (f2); fclose (fd);

    Bool result = wxRenameFile(outfile, dest);
    if (!result) {
      /* Maybe tried to move across filesystems. */
      wxCopyFile(outfile, dest);
      wxRemoveFile(outfile);
    }
    delete[] outfile;
    return result;
}

Bool wxCopyFile(char *src, char *dest)
{
    FILE *fs, *fd;
    int ch;

    if (!(fs = fopen(src, "rb"))) {
	return FALSE;
    } else if (!(fd = fopen(dest, "wb"))) {
	fclose(fs); return FALSE;
    }

    while ((ch = getc (fs)) != EOF)
	putc (ch, fd);

    fclose (fs); fclose (fd);

    return TRUE;
}

Bool wxDirExists(char *dir)
{
    struct stat sbuf;
    return ((stat(dir, &sbuf) != -1) && S_ISDIR(sbuf.st_mode) ? TRUE : FALSE);
}

char *wxGetWorkingDirectory(char *buf, int sz)
{
    if (!buf)
	buf = new char[sz+1];
    if (getcwd(buf, sz) == NULL) {
	buf[0] = '.';
	buf[1] = '\0';
    }
    return buf;
}

Bool wxMkdir(char *dir)
{
    return (mkdir(dir,S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
	    == 0);
}

Bool wxFileExists(char *filename)
{
    struct stat stbuf;

    return (filename && stat((char *)filename, &stbuf) == 0);
}

Bool wxRemoveFile(char *file)
{
    return Bool((unlink(file) == 0));
}

Bool wxRenameFile(char *old, char *_new)
{
    if (0 == rename(old, _new))
	return TRUE;
    return FALSE;
}

Bool wxRmdir(char *dir)
{
    return (rmdir(dir) == 0);
}

Bool wxSetWorkingDirectory(char *dir)
{
    return (chdir(dir) == 0);
}
