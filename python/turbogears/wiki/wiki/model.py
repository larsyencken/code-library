from turbogears.database import PackageHub
from sqlobject import *

hub = PackageHub('wiki')
__connection__ = hub

# class YourDataClass(SQLObject):
#     pass

class Page(SQLObject):
    pagename = UnicodeCol(alternateID=True, length=30)
    data = UnicodeCol()
