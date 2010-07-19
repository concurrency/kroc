import zipfile
import tarfile
import os

import command

def check_sanity(namelist):
    names = [name.split('/')[0] for name in namelist]
    sane = True
    first_n = names[0]
    for n in names:
        if n != first_n:
            sane = False
            break
    if sane:
        return (True, first_n)
    return (False, None)

def extract(archive, dest='', force=False):
    if archive.endswith('.zip') or archive.endswith('.jar'):
        a = zipfile.ZipFile(archive, 'r')
        sane, internal_dest = check_sanity(a.namelist())
        if sane:
            exists = os.path.exists(os.path.join(dest, internal_dest))
        else:
            dest = os.path.join(dest, 
                    os.path.splitext(os.path.basename(archive))[0])
            exists = os.path.exists(dest)
        if not exists:
            os.makedirs(dest)
            print 'extracting %s to %s' % (archive, dest)
            command.execute(['unzip', '-q', archive, '-d', dest])
        else:
            print 'extracting %s (already extracted)' % (archive, )
    elif archive.endswith('.tgz') or archive.endswith('.tar.gz'):
        a = tarfile.open(archive, 'r:*')
        sane, internal_dest = check_sanity(a.getnames())
        if sane:
            exists = os.path.exists(os.path.join(dest, internal_dest))
        else:
            dest = os.path.join(dest, 
                    os.path.splitext(os.path.basename(archive))[0])
            exists = os.path.exists(dest)
        if not exists:
            if not os.path.exists(dest): os.makedirs(dest)
            print 'extracting %s to %s' % (archive, dest)
            command.execute(['tar', '-xzf', archive, '-C', dest])
        else:
            print 'extracting %s (already extracted)' % (archive, )
    else:
        raise Exception('Archive format (%s) not recognised' % (archive, ))
