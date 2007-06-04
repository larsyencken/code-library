# -*- coding: utf-8 -*-
#----------------------------------------------------------------------------#
# sql.py
# Lars Yencken <lars.yencken@gmail.com>
# vim: ts=4 sw=4 noet tw=78:
# Wed Jul  6 16:38:49 EST 2005
#
#----------------------------------------------------------------------------#

import sys
import exceptions
from sets import Set

import enum
import MySQLdb
import copy

#----------------------------------------------------------------------------#

ReturnStyle = enum.Enum('All', 'Single', 'Many', 'Id', 'None')

#----------------------------------------------------------------------------#

class SqlDB:
	""" A db specification which allows opening and closing of transactions
		and general execution of sql code.
	"""

	def __init__(self, db, user, passwd, encoding='utf8'):
		""" Creates a new class instance, with all the information needed to
			connect to the database at will.
		"""
		self._dbName = db
		self._user = user
		self._passwd = passwd
		self._encoding = encoding

		self._db = None
		self._cursor = None
		self._serverVersion = None

		self._tables = {}

		self.refreshTables()

		return

	def __getitem__(self, tableName):
		""" Returns the table instance matching the table name, creating and
			storing one if one does not already exist.
		"""
		if not tableName in self._tables.keys():
			raise exceptions.KeyError, tableName

		return self._tables[tableName]
	
	def open(self):
		""" Starts a new database transaction. This must be done before any
			operation is performed.
		"""
		if self._db or self._cursor:
			raise Exception, "Can't open a new db connection; one exists"

		self._db = MySQLdb.connect(
				user=self._user,
				passwd=self._passwd,
				db = self._dbName,
				use_unicode = self._encoding
			)
		self._cursor = self._db.cursor()
		self._serverVersion = self._db.get_server_info()
		if self._serverVersion.startswith('5'):
			self._cursor.execute("SET NAMES %s" % self._encoding)

		return
	
	def isOpen(self):
		""" Returns True if the database connection is open, False otherwise.
		"""
		return self._db is not None
	
	def refreshTables(self):
		""" Detects which tables are present in the db. Should
		"""
		if self.isOpen():
			raise Exception, "Should be called with a closed DB"

		# remove the current dictionary
		for tableName in self._tables.iterkeys():
			del self.__dict__['key']

		self._tables = {}

		self.open()
		tableList = map(lambda x: x[0], list(
				self.execute('SHOW TABLES', ReturnStyle.All)
			))
		
		for tableName in tableList:
			self._tables[tableName] = BaseTable(tableName, self)
		self.commit()

		self.__dict__.update(self._tables)

		return

	def updateTable(self, tableName):
		""" Updates the table information for a given table. Should be called
			with an open DB.
		"""
		if not self.isOpen():
			raise Exception, "Update table should be called with a open DB"

		newTable = BaseTable(tableName, self)

		self._tables[tableName] = newTable
		self.__dict__.update(_tables)

		return

	def tables(self):
		""" Returns a list of table names which are present in the db.

			@return A list of table names
		"""
		return self._tables.keys()

	def execute(self, statement, args=None, returnStyle=ReturnStyle.All,
			limit=1):
		""" Executes an sql statement, possibly returning results of that
			statement.

			@param statement: An sql statement to execute.
			@type statement: str
			@param args: Possible args for the statement.
			@type args: list
			@param returnStyle: The type and number of results to return.
			@type returnStyle: ReturnStyle
			@param limit: The number to return if ReturnStyle.Many
				is used, ignored otherwise.
			@type limit: int
			@return A row/list of rows/id/None
		"""
		if not self._db or not self._cursor:
			raise Exception, 'DB has not yet been opened'

		if isinstance(args, enum.Enum):
			raise Exception, "Pass of return style without explicit ref"

		try:
			if args:
				self._cursor.execute(statement, args)
			else:
				self._cursor.execute(statement)
		except:
			self.rollback()
			print >> sys.stderr, 'Error in statement: %s' % `statement`
			if args:
				print >> sys.stderr, '              args: %s' % `args`
			raise

		if returnStyle == ReturnStyle.All:
			return self._cursor.fetchall()
		elif returnStyle == ReturnStyle.Many:
			return self._cursor.fetchmany(limit)
		elif returnStyle == ReturnStyle.Single:
			return self._cursor.fetchone()
		elif returnStyle == ReturnStyle.Id:
			return self._cursor.lastrowid

		return

	def getColumns(self, tableName):
		""" Gets the list of columns for the table name from the db.

			@param tableName: The name of the table to get columns for.
			@type tableName: str
			@return: A (name, type) list of columns
		"""
		rawColumns = self.execute('DESC %s' % tableName, ReturnStyle.All)
		columnSpec = []
		for name, type, _blah1, _blah2, _blah3, _blah4 in rawColumns:
			columnSpec.append((tableName, name, type))

		return columnSpec

	def rollback(self):
		""" Rolls back all changes since the last open() call (i.e. for this
			transaction), and closes the connection.
		"""
		if self.isOpen():
			self._db.rollback()
			self._db.close()

		self._cursor = None
		self._db = None
		self._serverVersion = None

		return

	def forceClose(self):
		self._cursor.close()
		self._db.close()

		self._cursor = None
		self._db = None
		self._serverVersion = None

		return

	def commit(self):
		""" Commits the changes for the current transaction, and closes the
			connection to the db.
		"""
		if not self._db:
			raise Exception, "No db transaction to commit!!!"

		self._db.commit()
		self._db.close()
		
		self._cursor = None
		self._db = None
		self._serverVersion = None

		return

	def __del__(self):
		if self._db:
			self.rollback()
			raise Exception, "Had to rollback an open connection on delete!" 

		return

	def autoConnect(self):
		autoId = self._nextAutoId
		self._nextAutoId += 1
		self._autoConnections.add(autoId)

		if not self._db:
			self.open()

		return autoId

	def autoCleanup(self, autoId):
		if not autoId in self._autoConnections:
			raise Exception, "Trying to clean up a non-existent autoconnection"

		self._autoConnections.remove(autoId)

		if not self._autoConnections:
			self.commit()

		return

	def __repr__(self):
		if self._db is None:
			return "<closed SqlDB object %s@%s>" % (self._user, self._dbName)
		else:
			return "<open SqlDB object %s@%s>" % (self._user, self._dbName)

	def copy(self):
		""" Creates a new copy of the current database object this facilitates
			creating multiple connections.
		"""
		if self._db is not None:
			raise Exception, 'Cannot copy a live database connection'

		return copy.deepcopy(self)

#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

JoinType = enum.Enum('inner', 'leftOuter')

Op = enum.Enum('eq', 'le', 'ge', 'lt', 'gt')

#----------------------------------------------------------------------------#

class BaseTable:
	""" This class represents a single existing table in MySQL.
	"""

	def __init__(self, name, db):
		""" Creates a new instance repesenting an existing table.

			@param name: The table name
			@type name: str
			@param db: The database to use
			@type db: SqlDB
		"""
		self._name = name
		self._db = db
		self._columnSpec = db.getColumns(self._name)

		return
	
	def join(self, tableInstance, constraints, joinType=JoinType.inner):
		""" Creates a new virtual table based on this table and the instance
			passed in, identifying the pairs of columns provided.

			@param tableInstance: The table to join to.
			@type tableInstance: BaseTable
			@param constraints: Triples of (leftCol, op, rightCol), where op
				is part of the Op enum. 
			@type constraints: (str, Op, str)
			@param joinType: The method used to join.
			@type joinType: JoinType
			@return A new BaseTable instance
		"""

		qualified = []
		rightName = tableInstance.name()
		for left, op, right in constraints:
			if not '.' in left:
				left = self._name + '.' + left

			if not '.' in right:
				right = rightName + '.' + right

			qualified.append((left, op, right))

		newTable = JoinTable(self._db, self, tableInstance, qualified,
				joinType)

		return newTable
	
	def insert(self, rows, columnNames=None):
		""" Inserts the rows into this table.
		"""
		if columnNames:
			columnSql = '(%s)' % ', '.join(['%s' % c for c in columnNames])
			numColumns = len(columnNames)
		else:
			columnSql = ''
			numColumns = len(self._columnSpec)

		sqlString = 'INSERT INTO %s %s VALUES ' % (self._name, columnSql)
		sqlString += '(%s)' % ', '.join(['%s'] * numColumns)

		rowIds = []
		for row in rows:
			rowIds.append(self._db.execute(sqlString, row, \
					returnStyle=ReturnStyle.Id))

		return rowIds

	def update(self, values, valueColumns, constraints):
		""" Updates the values of these rows into this table.
		"""
		sqlString = 'UPDATE %s SET ' % self._name
		sqlString += ', '.join(['%s = %%s' % name for name in valueColumns])
		sqlString += ' WHERE ' + _constraintSql(constraints)

		self._db.execute(sqlString, values)

		return

	def name(self):
		return self._name

	def columns(self):
		return self._columnSpec[:]

	def select(self, constraints=None, columnNames=None, limit=None):
		""" Performs an sql select, with optional constraints and columns
			selected.
		"""
		columnSql = ''
		if columnNames:
			columnSql = ', '.join(columnNames)
		else:
			columnSql = '*'

		sqlString = 'SELECT %s FROM %s' % (columnSql, self._name)

		if constraints:
			sqlString += ' WHERE %s' % _constraintSql(constraints)

		if limit:
			sqlString += ' LIMIT %d' % limit

		results = self._db.execute(sqlString)

		return results
	
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

class JoinTable:
	""" This table represents multiple tables joined together.
	"""

	def __init__(self, db, leftSource, rightSource, constraints, joinType):
		self._db = db
		self._joinType = joinType

		leftName = leftSource.name()
		rightName = rightSource.name()

		if not isinstance(rightSource, BaseTable):
			raise Exception, "Right source must ALWAYS be a base table"

		self._name = leftSource.name()

		if joinType == JoinType.inner:
			self._name += ' INNER JOIN %s ON ' % rightName
		else:
			self._name += ' LEFT OUTER JOIN %s ON ' % rightName
		self._name += _constraintSql(constraints)

		# populate our columns and links to base tables
		self._columnSpec = []
		self._columnSpec.extend(leftSource.columns())
		self._columnSpec.extend(rightSource.columns())

		return

	def join(self, tableInstance, constraints, joinType=JoinType.inner):
		""" Creates a new virtual table based on this table and the instance
			passed in, identifying the pairs of columns provided.

			@param tableInstance: The table to join to.
			@type tableInstance: BaseTable
			@param constraints: Triples of (leftCol, op, rightCol), where op
				is part of the Op enum. 
			@type constraints: (str, Op, str)
			@param joinType: The method used to join.
			@type joinType: JoinType
			@return A new BaseTable instance
		"""
		name = '(%s-%s-%s)' % (self._name,str(joinType),tableInstance.name())
		newTable = BaseTable(name, self._db, TableType.virtual)

		return
	
	def insert(self, blah):
		raise Exception, "INSERTs into joined tables are not yet supported"

	def name(self):
		return self._name

	def columns(self):
		return self._columnSpec[:]

	def select(self, constraints=None, columnNames=None, limit=None):
		""" Performs an sql select, with optional constraints and columns
			selected.
		"""
		if columnNames:
			columnSql = ', '.join(['%s' % name for name in columnNames])
		else:
			columnSql = '*'

		if constraints:
			sqlConstraint = _constraintSql(constraints)
			sqlString = 'SELECT %s FROM %s WHERE %s' % \
					(columnSql, self._name, sqlConstraint)
		else:
			sqlString = 'SELECT %s FROM %s' % (columnSql, self._name)

		if limit:
			sqlString += ' LIMIT %d' % limit

		return self._db.execute(sqlString)
	
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

_opToString = {
		Op.eq: '=',
		Op.lt: '<',
		Op.gt: '>',
		Op.le: '<=',
		Op.ge: '>='
	}

def _constraintSql(constraints):
	assert constraints

	left, op, right = constraints[0]
	sqlString = "%s %s %s" % (`left`, _opToString[op], `right`)

	for left, op, right in constraints[1:]:
		sqlString += " AND %s %s %s" % (`left`, _opToString[op], `right`)
	
	return sqlString

#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

