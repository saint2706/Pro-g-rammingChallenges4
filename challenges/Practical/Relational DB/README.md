# Relational DB Challenge

This project implements a lightweight educational relational database engine in Python. It focuses on illustrating how SQL parsing, storage, and execution layers interact without relying on third-party database libraries.

## Supported SQL subset

The interactive shell and executor accept a compact subset of ANSI SQL tailored for CRUD operations and schema management:

| Statement | Capabilities |
|-----------|--------------|
| `CREATE TABLE` | Defines tables with `INT` and `TEXT` columns, single-column primary keys, and `REFERENCES` clauses for foreign keys. |
| `DROP TABLE` | Removes a table definition and its data. |
| `INSERT` | Inserts rows via column lists or full table inserts. Multiple row values are accepted in a single statement. |
| `SELECT` | Projects a column list or `*` with optional `WHERE` filters that combine equality predicates using `AND`. |
| `UPDATE` | Updates one or more columns with optional filtered predicates. |
| `DELETE` | Deletes rows optionally filtered by a `WHERE` clause. |
| `BEGIN`, `COMMIT`, `ROLLBACK` | Provides basic transaction semantics using optimistic copy-on-write snapshots. |

All identifiers are case-insensitive. String literals use single quotes (`'value'`). Numeric literals are parsed as integers. Expressions beyond equality predicates (e.g., arithmetic, joins, ordering, or aggregation) are out of scope for this challenge implementation.

## Architecture overview

The project is organised into three main layers:

1. **Storage engine (`storage.py`)** – Manages table schemas, row storage, and constraint enforcement. Tables are backed by heap storage (lists of Python dictionaries) augmented with hash indexes for primary keys to speed up lookups. Foreign-key metadata is enforced on inserts, updates, and deletes to maintain referential integrity between tables.
2. **SQL parser (`parser.py`)** – Tokenises SQL strings and produces intermediate command objects describing the requested operations. The parser recognises the subset listed above and supplies structured payloads to the executor.
3. **Executor (`executor.py`)** – Binds parsed statements to storage operations, returning result sets for `SELECT` statements and row-count summaries for DML statements. The executor also controls transaction boundaries by coordinating snapshot creation and restoration in the storage engine.

A simple command-line interface (`cli.py`) wraps the executor, offering a REPL experience with multiline statements, `.quit` to exit, and `.tables` to list defined schemas. Automated tests under `tests/` cover schema creation, CRUD flows, transaction handling, and foreign-key behaviour.

## Usage

1. **Install dependencies** – The project only relies on the Python standard library and `pytest` for testing. Create a virtual environment and install development tooling if desired.
2. **Run the interactive shell**:

   ```bash
   python -m relational_db.cli
   ```

   Example session:

   ```sql
   db> CREATE TABLE authors (id INT PRIMARY KEY, name TEXT);
   db> CREATE TABLE books (id INT PRIMARY KEY, title TEXT, author_id INT REFERENCES authors(id));
   db> INSERT INTO authors (id, name) VALUES (1, 'Octavia Butler');
   db> INSERT INTO books VALUES (1, 'Kindred', 1);
   db> SELECT * FROM books;
   id | title   | author_id
   1  | Kindred | 1
   db> BEGIN;
   db> UPDATE books SET title = 'Parable of the Sower' WHERE id = 1;
   db> ROLLBACK;
   db> SELECT title FROM books WHERE id = 1;
   title
   Kindred
   ```

3. **Run tests**:

   ```bash
   pytest
   ```

## Performance notes

The engine is optimised for clarity rather than raw performance:

- Heap-backed tables keep data in memory only; persistence is not implemented.
- Primary keys are indexed with in-memory hash maps, offering O(1) lookups for equality predicates on primary keys.
- Foreign-key validation executes against referencing tables on each mutation, which is acceptable for small datasets but becomes expensive as table counts grow.
- Transactions copy table data lazily when entering `BEGIN`, making short-lived operations efficient while larger datasets incur increased memory usage.

These trade-offs make the project suitable for educational exploration and unit-testing sized workloads.
