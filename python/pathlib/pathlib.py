# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# pathlib.py
# Lars Yencken <lljy@csse.unimelb.edu.au>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Wed Apr 26 11:59:58 2006
#
# Originally: Copyright (c) CNRI 1997.
# With minor modifications made by me.
#----------------------------------------------------------------------------#

"""Path management for large projects.

As a side effect of initializing this module, sys.path is modified to
include all relevant directories.

In order to bootstrap this, the environment variable PYTHONPATH may
include the directory containing the pathlib module, *or* the script
may contain a line like this to allow scripts to locate pathlib:

    import sys; sys.path.insert(0, '../../common')

The number of "../" segments in the first line should reflect the depth of
the script file relative to the project root.

This line may be ommitted if pathlib is installed in the standard
Python library or placement along PYTHONPATH is required.

A line like this should be used to initialize pathlib:

    import pathlib; pathlib.boot('packageA/packageB')

You should pass the path from the root to your module, and the root directory
should have a .pathlib file attached. The configuration file used by this
module uses lines similar to RFC822 headers, with blank lines and comment
lines beginning with `#' also supported.  The header names are case
insensitive.

The `project-root' header specifies the location of the project root
relative to the location of the configuration file.  The `project-root-var'
header specifies the name of an environment variable which, if present in
the user's environment, overrides the computed location of the root, and
causes an alternate configuration file to be read.  The altenate file
has the same name and relative location to the specified root as the
initial file has to the computed root.

More interestingly, the `library' header is used to specify a location,
relative to the root, which should be added to sys.path if it exists.  This
header may be used repeatedly in the configuration file to specify multiple
directories.  Each occurrance specifies exactly one directory.  All
directories specified using the `library' header are placed on the path
before the original entries on sys.path and in the order in which they are
encountered in the configuration file.

Each path specified by a `library' header is also interpreted as a Python
formatting specification which will be formatted across a dictionary
containing a single entry, 'plat'.  The value of the entry will be the name
of the platform, as returned by `get_platform_name()'.

All path information read from the file should be specified in POSIX
syntax; it will be converted to the native syntax if needed.


Functions (see individual doc strings for more information):

    boot():
        Initialize the pathlib module with a path to our config file;
        path is relative to sys.argv[0] and is always specified as a POSIX
        path; conversion to a non-POSIX systax is performed if needed.

    get_rootdir():
        Return the root of the project source tree.

    get_moduledir():
        Return the directory of the module that called boot().

    get_platform_name():
        Return a name for a directory containing architecture-specific
        files.

"""
__copyright__ = "Copyright (c) CNRI 1997."

import os
import string
import sys

import warnings
warnings.filterwarnings(
    action = 'ignore',
    message='.*regex module is deprecated',
    category=DeprecationWarning
)
DEFAULT_CONFIG_FILE = ".pathlib"
 
_platform_name = None

def get_platform_name():
    """Calculate our platform name, for use as a subdirectory of common/.

    Usually sys.platform will give us the right thing, however Python
    is broken in that sys.platform is 'sunos5' for both Solaris/Sparc
    and Solaris/Intel (and presumably Solaris/PPC).  Do some
    additional hackery in those cases.  If these hacks fail (e.g. when
    imported in an applet), return sys.platform.

    XXX Should eventually do this for Linux and NT as well.

    """
    global _platform_name
    if _platform_name:
        return _platform_name
    #
    plat = sys.platform
    if plat != 'sunos5': return plat
    try:
        arch = os.uname()[4]
        if len(arch) >= 4 and arch[:4] == 'sun4':
            arch = 'sparc'
    except (AttributeError, os.error):
        _platform_name = plat
    else:
        _platform_name = plat + '-' + arch
    return _platform_name
    

def get_rootdir():
    """Get the root of the project tree."""
    return _root
 
def _get_script_path():
    """Return the real home of the control script."""
    script_name = os.path.join(os.getcwd(), sys.argv[0])
    if not script_name:
        # get it in the interactive case as well....
        script_name = os.path.join(os.getcwd(), ".")
    while 1:
        script_dir = os.path.dirname(script_name)
        if not os.path.islink(script_name):
            break
        script_name = os.path.join(script_dir, os.readlink(script_name))
        script_dir = os.path.join(os.getcwd(), script_dir)
    return os.path.normpath(script_dir)

import regex
_validpat = "^\([-a-z0-9_]*\):\(.*\)$"
_valid = regex.compile(_validpat, regex.casefold)

def _read_config(filename):
    """Read a pathlib config file.

    Lines in the file may be commments (begin line with a hash ('#')
    character), blank, or a path configuration parameter.  Parameters
    are specified on lines which resemble RFC822 headers.  Continuation is
    not supported or needed.

    Returns a dictionary of names to a list of values.  Names are normalized
    to lower case.
    """
    values = {}
    try:
        fp = open(filename)
    except IOError:
        return values
    while 1:
        line = fp.readline()
        if not line:
            break
        line = string.rstrip(line)
        if _valid.match(line) >= 0:
            name, value = _valid.group(1, 2)
            name = string.lower(name)
            if not values.has_key(name):
                values[name] = []
            values[name].append(string.strip(value))
    fp.close()
    return values


def _initialize(root, values):
    """Initialize module with final set of loaded values."""
    global _root, _values
    _root = root
    _values = values
    platdir = get_platform_name()
    # set up a reasonable set of defaults to look for:
    if not values.has_key("library"):
        values["library"] = [
            "pythonlib", "pythonlib/%(plat)s", "common", "common/%(plat)s"]
    # now locate the ones that exist, and make sure they're on the front
    # of sys.path:
    found = []
    for p in values["library"]:
        p = _convert_path(p % {"plat": platdir})
        ap = os.path.normpath(os.path.join(root, p))
        if os.path.isdir(ap) and not ap in found:
            found.append(ap)
    for p in found:
        while p in sys.path:
            sys.path.remove(p)
    sys.path[:0] = found


def _absolutize_path():
    """Make all sys.path entries absolute."""
    cwd = os.getcwd()
    i = 0
    while i < len(sys.path):
        sys.path[i] = os.path.normpath(os.path.join(cwd, sys.path[i]))
        if os.path.exists(sys.path[i]):
            i = i + 1
        else:
#           sys.stderr.write("%s: found non-existent path on sys.path:\n  %s\n"
#                            % (__name__, sys.path[i]))
            del sys.path[i]


def _get_relative_dir(dirname, relative_to):
    """Get relative path to `dirname' from `relative_to'."""
    dirpath = string.splitfields(dirname, os.sep)
    relative_to = string.splitfields(relative_to, os.sep)
    changed = 0
    while dirpath and relative_to and dirpath[0] == relative_to[0]:
        del dirpath[0]
        del relative_to[0]
        changed = changed + 1
    if changed < 2:
        return dirname
    dirpath = (len(relative_to) * [os.pardir]) + dirpath
    return string.joinfields(dirpath, os.sep)


def _convert_path(path):
    """Convert a POSIX pathname to a 'local' pathname."""
    if os.name == "posix":
        return path
    pardir = os.pardir
    if os.name == "mac":
        pardir = ''
    parts = string.splitfields(path, '/')
    for pos in range(len(parts)):
        if parts[pos] == "..":
            parts[pos] = pardir
        elif parts[pos] == ".":
            parts[pos] = os.curdir
    return string.joinfields(parts, os.sep)


_inited = 0

def _reversePath(somepath):
    dirList = os.path.split(somepath)
    dirList.reverse()
    return os.path.join(dirList)

def get_moduledir():
    return os.path.join(_root, _moduleRelPath)

def boot(pathToHere):
    """Initialize path configuration if not already initialized.

    `conffile' is either the path to the directory containing the
    configuration file relative to the script location, or the full file name
    of the config file, also relative to the script location.  If only the
    directory is specified, the filename '.pathlib' is used.
    """
    global _inited, _moduleRelPath
    if _inited:
        return
    _inited = 1
    _absolutize_path()

    # safe the relative path from root to module
    _moduleRelPath = pathToHere

    conffile = _reversePath(pathToHere)
    script_dir = _get_script_path()
    conffile = _convert_path(conffile)
    my_conf = os.path.join(script_dir, conffile)

    if not os.path.isdir(my_conf):
        # XXX only except directory paths
        raise Exception, "Only directory paths are accepted now"
    else:
        my_conf = os.path.join(my_conf, DEFAULT_CONFIG_FILE)

    my_conf = os.path.normpath(my_conf)
    my_dir = os.path.dirname(my_conf)
    my_values = _read_config(my_conf)
    my_root = script_dir
    if my_values.has_key('project-root'):
        part = _convert_path(my_values['project-root'][0])
        my_root = os.path.normpath(os.path.join(my_dir, part))
    if my_values.has_key('project-root-var'):
        my_var = my_values['project-root-var'][0]
        if os.environ.has_key(my_var):
            new_root = os.environ[my_var]
            if new_root != my_root:
                # need to locate new config file to re-load values
                my_conf_dir, my_conf_name = os.path.split(my_conf)
                rel_path_to_conffile = _get_relative_dir(my_conf_dir, my_root)
                new_conf = os.path.normpath(
                    os.path.join(new_root, rel_path_to_conffile))
                new_conffile = os.path.join(new_conf, my_conf_name)
                if os.path.exists(new_conffile):
                    my_values = _read_config(new_conffile)
                my_root = new_root
    _initialize(my_root, my_values)
