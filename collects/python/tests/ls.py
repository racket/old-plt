#######
##  No imports in our compiler yet
#from os import listdir
#from os import stat
#from stat import S_ISDIR
#from stat import S_ISREG

# a FileTree is one of:
#  file(string)
#  directory( string, listof(FileTree) )


class directory(object):
    def __init__(this, name, files):
        this.name = name
        this.files = files

class file(object):
    def __init__(this, name):
        this.name = name



####
##  No Python FFI yet, so isdir and isfile are provided by the runtime
#def isdir(path): return S_ISDIR(stat(path).st_mode)
#def isfile(path): return S_ISREG(stat(path).st_mode)

###
##  Sort already provided by the runtime
#def sort(lst):
#    copy = map(lambda x:x,lst)
#    copy.sort()
#    return copy


# build: string -> FileTree
# builds a directory tree starting with path
def build(path):
    def buildtree(fullpath):
        if isdir(fullpath):
            return directory(fullpath, filter(lambda x:x,
                                              map(buildtree,
                                                  map(lambda s: fullpath + "/" + s,
                                                      listdir(fullpath)))))
        elif isfile(fullpath):
            return file(fullpath)
        else: return None
    t = buildtree(path)
    if not t: raise "Invalid Path", path
    return t

  
# printtree: FileTree number ->
# print out the tree, starting with offset spaces
def printtree(t, offset = 0):
    if isinstance(t, directory):
        print offset * " ", t.name, ":"
        for f in t.files: printtree(f, offset + 3)
    else: print offset * " ", t.name


dir = 'c:/temp/plt/collects/python'
#dir = '/home/dsilva/python'

printtree(build(dir))

