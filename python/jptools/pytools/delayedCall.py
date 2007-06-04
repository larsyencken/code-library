# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# delayedCall.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 noet tw=78:
# Thu Nov 17 18:29:38 EST 2005
#
#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#

class DelayedCall:
	""" Represents a delayed function call.
	"""
	#------------------------------------------------------------------------#
	# PUBLIC METHODS
	#

	def __init__(self, *args, **kwargs):
		""" Creates a new delayed call. The args must include the method to be
			called.
		"""
		self.method = args[0]
		self.args = list(args[1:])
		self.kwargs = kwargs

		return

	#------------------------------------------------------------------------#

	def render(self):
		""" Perform the function call which was delayed.
		"""
		return apply(self.method, self.args, self.kwargs)

	#------------------------------------------------------------------------#

	def renderArgs(self):
		""" As for render(), but also render any function arguments which are 
			also DelayedCall instances.
		"""
		for i in range(len(self.args)):
			if isinstance(self.args[i], DelayedCall):
				self.args[i] = self.args[i].renderArgs()

		for key, value in self.kwargs.iteritems():
			if isinstance(value, DelayedCall):
				self.kwargs[key] = value.renderArgs()

		return apply(self.method, self.args, self.kwargs)

	#------------------------------------------------------------------------#
	# PRIVATE METHODS
	#
	
	#------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
