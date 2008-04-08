#!/usr/bin/env python
# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# notify.py
# Lars Yencken <lljy@csse.unimelb.edu.au>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Tue Apr  8 16:15:50 EST 2008
#
#----------------------------------------------------------------------------#

"Uses either pynotify or Growl to leave a transient note for the user."

import os, sys, optparse

#----------------------------------------------------------------------------#
# PUBLIC
#----------------------------------------------------------------------------#
 
def notify(title=None, message=None, appName='python shell'):
    "Provide the user with a notification or fail silently."
    try:
        notifyUsingPynotify(title=title, message=message, appName=appName)
        return
    except ImportError:
        pass

    try:
        notifyUsingGrowl(title=title, message=message, appName=appName)
    except ImportError:
        pass

#----------------------------------------------------------------------------#

def notifyUsingPynotify(appName=None, title=None, message=None):
    import pynotify
    pynotify.init(appName)
    n = pynotify.Notification(title, message, "dialog-info")
    n.set_urgency(pynotify.URGENCY_NORMAL)
    n.set_timeout(pynotify.EXPIRES_DEFAULT)
    n.show()
    return

#----------------------------------------------------------------------------#

def notifyUsingGrowl(appName=None, title=None, message=None):
    import Growl
    n = Growl.GrowlNotifier(applicationName=appName, notifications=['info'])
    n.register()
    n.notify('info', title, message)
    return

#----------------------------------------------------------------------------#
# PRIVATE
#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
# MODULE EPILOGUE
#----------------------------------------------------------------------------#

def _createOptionParser():
    usage = \
"""%prog [options] title message

Uses either pynotify or Growl to display a status message to the user for a
few seconds. If neither are available, this script fails silently."""

    parser = optparse.OptionParser(usage)

    parser.add_option('--debug', action='store_true', dest='debug',
            default=False, help='Enables debugging mode [False]')

    return parser

#----------------------------------------------------------------------------#

def main(argv):
    parser = _createOptionParser()
    (options, args) = parser.parse_args(argv)

    try:
        [title, message] = args
    except:
        parser.print_help()
        sys.exit(1)

    # Avoid psyco in debugging mode, since it merges stack frames.
    if not options.debug:
        try:
            import psyco
            psyco.profile()
        except:
            pass

    notify(title=title, message=message, appName="Python shell")
    
    return

#----------------------------------------------------------------------------#

if __name__ == '__main__':
    main(sys.argv[1:])

#----------------------------------------------------------------------------#
  
# vim: ts=4 sw=4 sts=4 et tw=78:
