import sys
import os
import errno
import shutil
import glob
import string

# from: 
# http://stackoverflow.com/questions/600268/mkdir-p-functionality-in-python
def mkdirs(path):
    try:
        os.makedirs(path)
    except OSError, exc: # Python >2.5
        if exc.errno == errno.EEXIST:
            pass
        else: raise

def copy_file(src, dst, substitute=None):
    print '%s -> %s' % (src, dst)
    if substitute and src.endswith('.in'):
        s = open(src, 'r')
        contents = string.Template(s.read())
        s.close()
        contents = contents.safe_substitute(substitute)
        if os.path.isdir(dst):
            dst = os.path.join(dst, os.path.basename(src[:-3]))
        d = open(dst, 'w')
        d.write(contents)
        d.close()
    else:
        shutil.copy2(src, dst)

def copy_tree(src_dir, dest_dir, excludes=[], substitute=None):
    for root, dirs, files in os.walk(src_dir):
        path = root[len(src_dir) + 1:]
        dest_path = os.path.join(dest_dir, path)
        mkdirs(dest_path)
        for f in files:
            skip = False
            src_file = os.path.join(root, f)
            for f in excludes:
                if src_file.endswith(f): 
                    skip = True
            if not skip:
                if os.path.islink(src_file):
                    link_dest = os.readlink(src_file)
                    dest = os.path.join(dest_path, os.path.basename(src_file))
                    if os.path.lexists(dest):
                        os.unlink(dest)
                    print 'symlink: %s pointing to  %s' % (dest, link_dest)
                    os.symlink(link_dest, dest)
                else:
                    copy_file(src_file, dest_path, substitute)
        if '.svn' in dirs:
            dirs.remove('.svn')  # don't visit .svn directories 

def copy_files(src, dest_path, substitute=None):
    files = glob.iglob(src)
    for f in files:
        copy_file(f, dest_path, substitute)


