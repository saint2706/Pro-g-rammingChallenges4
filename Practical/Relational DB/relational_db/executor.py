"""SQL executor that ties the parser to the storage engine."""

from __future__ import annotations

from typing import Any, Dict, List

from .parser import SQLParser, Statement
from .storage import Column, Database


class SQLExecutor:
    """Executes SQL statements against an in-memory :class:`Database`."""

    def __init__(self, database: Database | None = None) -> None:
        self.database = database or Database()
        self.parser = SQLParser()

    def execute(self, sql: str) -> List[Dict[str, Any]]:
        statements = self.parser.parse(sql)
        results: List[Dict[str, Any]] = []
        for statement in statements:
            handler = getattr(self, f"_exec_{statement.kind}", None)
            if handler is None:
                raise ValueError(f"Unsupported statement '{statement.kind}'")
            results.append(handler(statement.payload))
        return results

    # ------------------------------------------------------------------
    def _exec_create_table(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        name: str = payload["name"]
        columns: List[Column] = payload["columns"]
        self.database.create_table(name, columns)
        return {"type": "message", "message": f"Table '{name}' created"}

    def _exec_drop_table(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        name: str = payload["name"]
        self.database.drop_table(name)
        return {"type": "message", "message": f"Table '{name}' dropped"}

    def _exec_insert(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        table_name: str = payload["table"]
        columns = payload["columns"]
        values = payload["values"]
        prepared_rows = []
        if columns is None:
            table = self.database._get_table(table_name)  # internal use
            if any(len(row) != len(table.normalised_column_names()) for row in values):
                raise ValueError("Inserted row has incorrect number of values")
            for row in values:
                prepared_rows.append(
                    {
                        column: row[idx]
                        for idx, column in enumerate(table.normalised_column_names())
                    }
                )
        else:
            normalised_columns = [column.lower() for column in columns]
            table = self.database._get_table(table_name)
            for column in normalised_columns:
                table.get_column(column)
            for row in values:
                if len(row) != len(normalised_columns):
                    raise ValueError("Inserted row has incorrect number of values")
                record = {
                    column: row[idx] for idx, column in enumerate(normalised_columns)
                }
                for column in table.normalised_column_names():
                    if column not in record:
                        record[column] = None
                prepared_rows.append(record)
        count = self.database.insert(table_name, prepared_rows)
        return {"type": "rowcount", "count": count}

    def _exec_select(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        table_name: str = payload["table"]
        columns: List[str] = payload["columns"]
        predicate = payload["where"]
        result_columns, rows = self.database.select(table_name, columns, predicate)
        return {"type": "result_set", "columns": result_columns, "rows": rows}

    def _exec_update(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        table_name: str = payload["table"]
        values: Dict[str, Any] = payload["values"]
        predicate = payload["where"]
        count = self.database.update(table_name, values, predicate)
        return {"type": "rowcount", "count": count}

    def _exec_delete(self, payload: Dict[str, Any]) -> Dict[str, Any]:
        table_name: str = payload["table"]
        predicate = payload["where"]
        count = self.database.delete(table_name, predicate)
        return {"type": "rowcount", "count": count}

    def _exec_begin(self, _: Dict[str, Any]) -> Dict[str, Any]:
        self.database.begin()
        return {"type": "message", "message": "Transaction started"}

    def _exec_commit(self, _: Dict[str, Any]) -> Dict[str, Any]:
        self.database.commit()
        return {"type": "message", "message": "Transaction committed"}

    def _exec_rollback(self, _: Dict[str, Any]) -> Dict[str, Any]:
        self.database.rollback()
        return {"type": "message", "message": "Transaction rolled back"}


__all__ = ["SQLExecutor"]
