import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from relational_db.executor import SQLExecutor
from relational_db.storage import Column, Database, ForeignKey


def execute_single(executor: SQLExecutor, sql: str):
    result = executor.execute(sql)
    assert len(result) == 1
    return result[0]


def test_schema_and_crud_operations():
    db = Database()
    db.create_table(
        "authors",
        [
            Column("id", "INT", primary_key=True),
            Column("name", "TEXT"),
        ],
    )
    db.create_table(
        "books",
        [
            Column("id", "INT", primary_key=True),
            Column("title", "TEXT"),
            Column(
                "author_id",
                "INT",
                foreign_key=ForeignKey("author_id", "authors", "id"),
            ),
        ],
    )
    db.insert("authors", [{"id": 1, "name": "Octavia Butler"}])
    db.insert(
        "books",
        [
            {"id": 1, "title": "Kindred", "author_id": 1},
            {"id": 2, "title": "Fledgling", "author_id": 1},
        ],
    )
    cols, rows = db.select("books", ["id", "title"], [("author_id", 1)])
    assert cols == ["id", "title"]
    assert rows == [(1, "Kindred"), (2, "Fledgling")]


def test_foreign_key_enforcement_via_sql():
    executor = SQLExecutor()
    execute_single(
        executor,
        """
        CREATE TABLE parents (id INT PRIMARY KEY, name TEXT);
        """,
    )
    execute_single(
        executor,
        """
        CREATE TABLE children (
            id INT PRIMARY KEY,
            parent_id INT REFERENCES parents(id),
            name TEXT
        );
        """,
    )
    execute_single(
        executor,
        "INSERT INTO parents VALUES (1, 'Alice');",
    )
    execute_single(
        executor,
        "INSERT INTO children VALUES (1, 1, 'Charlie');",
    )
    with pytest.raises(ValueError):
        executor.execute("INSERT INTO children VALUES (2, 99, 'Orphan');")
    with pytest.raises(ValueError):
        executor.execute("DELETE FROM parents WHERE id = 1;")


def test_update_delete_and_transactions():
    executor = SQLExecutor()
    execute_single(
        executor,
        "CREATE TABLE items (id INT PRIMARY KEY, name TEXT);",
    )
    execute_single(
        executor,
        "INSERT INTO items VALUES (1, 'Widget');",
    )
    execute_single(executor, "BEGIN;")
    execute_single(
        executor,
        "UPDATE items SET name = 'Gadget' WHERE id = 1;",
    )
    # Update visible inside transaction
    result = execute_single(
        executor,
        "SELECT name FROM items WHERE id = 1;",
    )
    assert result["rows"] == [("Gadget",)]
    execute_single(executor, "ROLLBACK;")
    # Value restored after rollback
    result = execute_single(
        executor,
        "SELECT name FROM items WHERE id = 1;",
    )
    assert result["rows"] == [("Widget",)]
    execute_single(executor, "BEGIN;")
    execute_single(
        executor,
        "UPDATE items SET name = 'Tool' WHERE id = 1;",
    )
    execute_single(executor, "COMMIT;")
    result = execute_single(
        executor,
        "SELECT name FROM items WHERE id = 1;",
    )
    assert result["rows"] == [("Tool",)]
    execute_single(
        executor,
        "DELETE FROM items WHERE id = 1;",
    )
    result = execute_single(executor, "SELECT * FROM items;")
    assert result["rows"] == []


def test_drop_table_rejects_referenced_parent():
    executor = SQLExecutor()
    execute_single(
        executor,
        "CREATE TABLE a (id INT PRIMARY KEY);",
    )
    execute_single(
        executor,
        "CREATE TABLE b (id INT PRIMARY KEY, a_id INT REFERENCES a(id));",
    )
    with pytest.raises(ValueError):
        executor.execute("DROP TABLE a;")
    execute_single(executor, "DROP TABLE b;")
    execute_single(executor, "DROP TABLE a;")
