# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# testSql.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 noet tw=78:
# Mon Jan 23 13:48:08 EST 2006
#
#----------------------------------------------------------------------------#

import sys
sys.path.append('..')

import unittest
from jptools import sql

#----------------------------------------------------------------------------#

dbUser = 'lars'
dbDB = 'pythonTest'
dbPasswd = 'crookshanks'

#----------------------------------------------------------------------------#

def suite():
	testSuite = unittest.TestSuite((
			unittest.makeSuite(ConnectionTestCase),
			unittest.makeSuite(SimpleTable),
			unittest.makeSuite(SelectTestCase)
		))
	return testSuite

#----------------------------------------------------------------------------#

class SqlTestCase(unittest.TestCase):
	def setUp(self):
		self._db = sql.SqlDB(user=dbUser, db=dbDB, passwd=dbPasswd)
		return

	def tearDown(self):
		return

#----------------------------------------------------------------------------#

class ConnectionTestCase(SqlTestCase):
	""" Test that the db can make simple connections.
	"""
	def testConnection(self):
		self._db.open()
		self._db.commit()

		self._db.open()
		self._db.rollback()
		return

#----------------------------------------------------------------------------#

class SimpleTable(SqlTestCase):
	""" Tests the creation of a simple table, inserting a few values.
	"""
	def testCreateTable(self):
		""" Tests that a table can be created inserted into and dropped.
		"""
		self._db.open()
		self._db.execute(\
""" CREATE TABLE testTable (
id INT PRIMARY KEY NOT NULL AUTO_INCREMENT,
floatVal FLOAT,
stringVal VARCHAR(100)
) ENGINE=InnoDB """
			)
		self._db.commit()

		self._db.open()

		self.assertEqual(
				self._db.execute('SHOW TABLES', 
						returnStyle=sql.ReturnStyle.Many),
				(('testTable',),)
			)

		self._db.execute('INSERT INTO testTable (floatVal, stringVal) VALUES (1.0, "super")')
		self._db.execute('INSERT INTO testTable (floatVal, stringVal) VALUES (2.0, "cool")')
		self._db.execute('INSERT INTO testTable (floatVal, stringVal) VALUES (3.0, "test")')

		data = self._db.execute('SELECT * FROM testTable')
	
		assert len(data) == 3
		assert data[0] == (1, 1.0, 'super')
		assert data[1] == (2, 2.0, 'cool')
		assert data[2] == (3, 3.0, 'test')
	
		self._db.execute('DROP TABLE testTable')

		self._db.commit()

		return
	
	def testRollbackTableCreation(self):
		""" Determines if rollback works properly for table creation.
		"""
		self._db.open()
		self._db.execute(\
""" CREATE TABLE IF NOT EXISTS testTable (
	id INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
	floatVal FLOAT,
	stringVal VARCHAR(100)
) ENGINE=InnoDB """)
		self._db.rollback()

		self._db.open()
		try:
			assert 'testTable' not in self._db.execute('SHOW TABLES')
		except:
			self._db.commit()
			raise
		else:
			self._db.commit()

		return

	def tearDown(self):
		if self._db.isOpen():
			self._db.rollback()
		self._db.open()
		self._db.execute('DROP TABLE IF EXISTS testTable')
		self._db.commit()
		SqlTestCase.tearDown(self)
		return

#----------------------------------------------------------------------------#

class SelectTestCase(SqlTestCase):
	def setUp(self):
		SqlTestCase.setUp(self)

		self._db.open()
		self._db.execute(\
""" CREATE TABLE selectTests (
		id INT PRIMARY KEY NOT NULL AUTO_INCREMENT,
		tag VARCHAR(100)
	) ENGINE=InnoDB
""")
		self._db.commit()

		self._db = sql.SqlDB(user=dbUser, db=dbDB, passwd=dbPasswd)
		return

	def myTestInsertAndSelect(self):
		""" Tests that insert and select are working properly in simple cases.
		"""
		self._db['selectTests'].insert(
				[
					("try"),
					("this"),
					("out")
				],
				columnNames=("tag")
			)

		data = self._db.selectTests.select()
		self.assertEqual(data, [(1, "try"), (2, "this"), (3, "out")])

		return

	def tearDown(self):
		self._db = sql.SqlDB(user=dbUser, db=dbDB, passwd=dbPasswd)
		self._db.open()
		self._db.execute("DROP TABLE IF EXISTS selectTests")
		self._db.commit()
		SqlTestCase.tearDown(self)
		return

#----------------------------------------------------------------------------#

if __name__ == "__main__":
	unittest.TextTestRunner(verbosity=1).run(suite())
