
MFUTILS
-------

Julian Smart, October 1994
--------------------------

This library is a *start* at reading Windows metafiles
into wxWindows-compatible format, independent of
the platform the code is running on. It's not
properly documented yet, so this and the source
code is all you have to go on I'm afraid...

The main class is wxXMetaFile, which contains a list
of records, and a list of pointers to those records
that are GDI objects and which are used in the
metafile (see mfutils.cc for a diagram and explanation).

Each record is stored in wxMetaRecord, which caters
for all kinds of metafile records (probably an
inefficient and hacky way to do it).

Only a subset of metafile functionality is handled, and
the library will probably crash if primitives are
read which are not yet handled.

The wxXMetaFileDC device context has not been implemented
but the skeleton class definition might be useful to someone
wishing to (partially) duplicate Windows metafile functionality
on different platforms.

The wxWindows application ClockWorks, soon to be made available on
skye.aiai.ed.ac.uk, shows how this library can be used to read in
simple metafiles and use wxWindows primitives to draw the
image (see mfload.h, mfload.cc).

The Windows SDK sample application, wmfdecode, was useful
in analysing the composition of metafiles and displaying them.

More details
------------

Windows metafiles are quite tricky to read, not least because the
Microsoft documentation is incomplete, and because of the funny way in
which object creation functions and DeleteObject allocate and destroy
identifiers for GDI objects, such as pens and brushes.

In a file, GDI objects cannot be referred to by GDI memory handles, so they
are referred to by their position in a list of GDI handles that
dynamically grows and shrinks as the metafile is played. A strategy
for allocating and deallocating these handle identifiers is implicit
in the file, and so mfutils simulates this with an array called
HandleTable.

When an object is created, a free slot in the handle table is found
(or created by incrementing the size of the table), and this number is
stored in the metarecord. When an object is deleted, the slot is freed
up for possible use by another GDI object. The record that is stored
in the handle table is the record that created the GDI object, and in
this record there is stored a number that indexes into the GDI record
list, giving its position in the GDI record list for when the handle
table is discarded (after the metafile has been read).

When mfutils finds a SelectObject record, the only record apart from
creation/deletion records to access the HandleTable, the integer read
from the metafile indexes into the handle table. The record found in
the table is the GDI object creation record, which, you'll remember,
contains another integer indexing into the GDI object list.

At this point, the SelectObject record is given the index into the GDI
object list, and all knowledge of the position in the handle table can
be forgotten. Since the GDI object list never shrinks or grows after
the metafile is read (all GDI objects being created ahead of time),
this index is all we need to find the GDI object.

So we've converted Microsoft's dynamic way of (de)allocating GDI
object handles into a static method, saving ourselves GDI object
creation time every time the metafile is drawn by always pre-creating
GDI objects (that should be particularly OK once WIN32 removes GDI handle
restrictions!)

