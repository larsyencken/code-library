# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# setup.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 sts=4 et tw=78:
# Tue Apr  8 16:35:12 2008
#
#----------------------------------------------------------------------------#

"Setup script for fetchEpisodes module."

#----------------------------------------------------------------------------#

from distutils.core import setup

#----------------------------------------------------------------------------#

setup(
        name='fetchEpisodes',
        version='0.1a',
        scripts=['fetchEpisodes.py']
    )
