"""Core storage structures for the educational relational database."""

from __future__ import annotations

import copy
from dataclasses import dataclass
from typing import Any, Dict, Iterable, List, Optional, Tuple


SUPPORTED_TYPES = {"INT", "TEXT"}


def _normalise(name: str) -> str:
    return name.lower()


@dataclass(frozen=True)
class ForeignKey:
    """Represents a simple foreign-key relationship."""

    column: str
    referenced_table: str
    referenced_column: str

    def normalised(self) -> Tuple[str, str, str]:
        return (
            _normalise(self.column),
            _normalise(self.referenced_table),
            _normalise(self.referenced_column),
        )


@dataclass(frozen=True)
class Column:
    """Column metadata for a table."""

    name: str
    col_type: str
    primary_key: bool = False
    foreign_key: Optional[ForeignKey] = None

    def __post_init__(self) -> None:
        if self.col_type.upper() not in SUPPORTED_TYPES:
            raise ValueError(f"Unsupported column type: {self.col_type}")

    @property
    def normalised_name(self) -> str:
        return _normalise(self.name)


class Table:
    """In-memory heap table backed by Python dictionaries."""

    def __init__(self, name: str, columns: Iterable[Column]):
        self.name = name
        self._columns: List[Column] = list(columns)
        self._column_order = [col.normalised_name for col in self._columns]
        self._column_map: Dict[str, Column] = {
            col.normalised_name: col for col in self._columns
        }
        self._rows: List[Dict[str, Any]] = []
        pk_cols = [col for col in self._columns if col.primary_key]
        self._primary_key: Optional[str] = (
            pk_cols[0].normalised_name if pk_cols else None
        )
        self._pk_index: Dict[Any, Dict[str, Any]] = {}

    def column_names(self) -> List[str]:
        return [col.name for col in self._columns]

    def normalised_column_names(self) -> List[str]:
        return list(self._column_order)

    def get_column(self, name: str) -> Column:
        try:
            return self._column_map[_normalise(name)]
        except KeyError as exc:
            raise KeyError(f"Unknown column '{name}' in table '{self.name}'") from exc

    def rows(self) -> List[Dict[str, Any]]:
        return self._rows

    def primary_key(self) -> Optional[str]:
        return self._primary_key

    def insert_row(self, row: Dict[str, Any]) -> None:
        if self._primary_key is not None:
            pk_value = row[self._primary_key]
            if pk_value in self._pk_index:
                raise ValueError(
                    f"Duplicate primary key value '{pk_value}' for table '{self.name}'"
                )
            self._pk_index[pk_value] = row
        self._rows.append(row)

    def delete_row(self, row: Dict[str, Any]) -> None:
        if self._primary_key is not None:
            pk_value = row[self._primary_key]
            self._pk_index.pop(pk_value, None)
        self._rows.remove(row)

    def update_row(self, row: Dict[str, Any], values: Dict[str, Any]) -> None:
        if self._primary_key is not None and self._primary_key in values:
            new_pk = values[self._primary_key]
            if new_pk != row[self._primary_key] and new_pk in self._pk_index:
                raise ValueError(
                    f"Duplicate primary key value '{new_pk}' for table '{self.name}'"
                )
            old_pk = row[self._primary_key]
            self._pk_index.pop(old_pk, None)
            self._pk_index[new_pk] = row
        row.update(values)

    def fetch_by_pk(self, value: Any) -> Optional[Dict[str, Any]]:
        if self._primary_key is None:
            return None
        return self._pk_index.get(value)

    def snapshot(self) -> "Table":
        cloned = Table(self.name, self._columns)
        cloned._rows = [row.copy() for row in self._rows]
        cloned._pk_index = {
            k: cloned._rows[self._rows.index(v)] for k, v in self._pk_index.items()
        }
        return cloned


class Database:
    """Container for tables and transaction state."""

    def __init__(self) -> None:
        self._tables: Dict[str, Table] = {}
        self._fk_children: Dict[str, List[Tuple[str, str]]] = {}
        self._snapshots: List[
            Tuple[Dict[str, Table], Dict[str, List[Tuple[str, str]]]]
        ] = []

    # ------------------------------------------------------------------
    # Schema management
    # ------------------------------------------------------------------
    def create_table(self, name: str, columns: Iterable[Column]) -> None:
        norm_name = _normalise(name)
        if norm_name in self._tables:
            raise ValueError(f"Table '{name}' already exists")
        cols = list(columns)
        pk_columns = [col for col in cols if col.primary_key]
        if len(pk_columns) > 1:
            raise ValueError("Only single-column primary keys are supported")
        table = Table(name, cols)
        self._tables[norm_name] = table
        self._register_foreign_keys(norm_name, cols)

    def drop_table(self, name: str) -> None:
        norm_name = _normalise(name)
        if norm_name not in self._tables:
            raise ValueError(f"Table '{name}' does not exist")
        if self._fk_children.get(norm_name):
            children = ", ".join({child for child, _ in self._fk_children[norm_name]})
            raise ValueError(
                f"Cannot drop table '{name}' because it is referenced by: {children}"
            )
        self._tables.pop(norm_name)
        # Remove as child references as well
        for parent, references in list(self._fk_children.items()):
            filtered = [ref for ref in references if ref[0] != norm_name]
            if filtered:
                self._fk_children[parent] = filtered
            else:
                self._fk_children.pop(parent, None)

    def list_tables(self) -> List[str]:
        return [table.name for table in self._tables.values()]

    # ------------------------------------------------------------------
    # Data manipulation
    # ------------------------------------------------------------------
    def insert(self, table_name: str, rows: Iterable[Dict[str, Any]]) -> int:
        table = self._get_table(table_name)
        count = 0
        for row in rows:
            prepared = self._prepare_row(table, row)
            self._enforce_foreign_keys(table, prepared)
            table.insert_row(prepared)
            count += 1
        return count

    def select(
        self,
        table_name: str,
        columns: Optional[List[str]] = None,
        predicate: Optional[List[Tuple[str, Any]]] = None,
    ) -> Tuple[List[str], List[Tuple[Any, ...]]]:
        table = self._get_table(table_name)
        result_columns = (
            table.column_names() if columns is None or columns == ["*"] else columns
        )
        norm_columns = [
            col if col == "*" else table.get_column(col).name for col in result_columns
        ]
        norm_cols_lower = (
            table.normalised_column_names()
            if columns is None or columns == ["*"]
            else [_normalise(col) for col in result_columns]
        )

        def matches(row: Dict[str, Any]) -> bool:
            if not predicate:
                return True
            for col, value in predicate:
                if row[_normalise(col)] != value:
                    return False
            return True

        matched_rows = [row for row in table.rows() if matches(row)]
        ordered = []
        for row in matched_rows:
            ordered.append(tuple(row[col] for col in norm_cols_lower))
        return norm_columns, ordered

    def update(
        self,
        table_name: str,
        values: Dict[str, Any],
        predicate: Optional[List[Tuple[str, Any]]] = None,
    ) -> int:
        table = self._get_table(table_name)
        prepared_updates = {
            _normalise(col): self._coerce_value(table.get_column(col), value)
            for col, value in values.items()
        }
        updated = 0

        def matches(row: Dict[str, Any]) -> bool:
            if not predicate:
                return True
            for col, value in predicate:
                if row[_normalise(col)] != value:
                    return False
            return True

        for row in list(table.rows()):
            if not matches(row):
                continue
            new_row = row.copy()
            new_row.update(prepared_updates)
            self._enforce_foreign_keys(table, new_row)
            pk_col = table.primary_key()
            if pk_col and pk_col in prepared_updates:
                if row[pk_col] != prepared_updates[pk_col]:
                    self._ensure_no_dependent_rows(table.name, row[pk_col])
            table.update_row(row, prepared_updates)
            updated += 1
        return updated

    def delete(
        self,
        table_name: str,
        predicate: Optional[List[Tuple[str, Any]]] = None,
    ) -> int:
        table = self._get_table(table_name)

        def matches(row: Dict[str, Any]) -> bool:
            if not predicate:
                return True
            for col, value in predicate:
                if row[_normalise(col)] != value:
                    return False
            return True

        deleted = 0
        for row in list(table.rows()):
            if matches(row):
                if table.primary_key() is not None:
                    self._ensure_no_dependent_rows(table.name, row[table.primary_key()])
                table.delete_row(row)
                deleted += 1
        return deleted

    # ------------------------------------------------------------------
    # Transactions
    # ------------------------------------------------------------------
    def begin(self) -> None:
        table_snapshot = {
            name: copy.deepcopy(table) for name, table in self._tables.items()
        }
        fk_snapshot = copy.deepcopy(self._fk_children)
        self._snapshots.append((table_snapshot, fk_snapshot))

    def commit(self) -> None:
        if not self._snapshots:
            raise RuntimeError("No active transaction")
        self._snapshots.pop()

    def rollback(self) -> None:
        if not self._snapshots:
            raise RuntimeError("No active transaction")
        tables, fk_children = self._snapshots.pop()
        self._tables = tables
        self._fk_children = fk_children

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------
    def _get_table(self, name: str) -> Table:
        norm_name = _normalise(name)
        try:
            return self._tables[norm_name]
        except KeyError as exc:
            raise ValueError(f"Table '{name}' does not exist") from exc

    def _prepare_row(self, table: Table, data: Dict[str, Any]) -> Dict[str, Any]:
        prepared: Dict[str, Any] = {}
        for column in table.normalised_column_names():
            if column not in data:
                raise ValueError(
                    f"Missing value for column '{table.get_column(column).name}'"
                )
            prepared[column] = self._coerce_value(
                table.get_column(column), data[column]
            )
        return prepared

    def _coerce_value(self, column: Column, value: Any) -> Any:
        if value is None:
            return None
        if column.col_type.upper() == "INT":
            if isinstance(value, bool):  # bool is subclass of int
                raise ValueError("Booleans are not valid INT values")
            if isinstance(value, int):
                return value
            try:
                return int(value)
            except (ValueError, TypeError) as exc:
                raise ValueError(
                    f"Value '{value}' for column '{column.name}' is not a valid INT"
                ) from exc
        if column.col_type.upper() == "TEXT":
            if isinstance(value, str):
                return value
            return str(value)
        raise ValueError(f"Unsupported column type: {column.col_type}")

    def _register_foreign_keys(
        self, table_name: str, columns: Iterable[Column]
    ) -> None:
        for column in columns:
            if column.foreign_key is None:
                continue
            _, ref_table, ref_column = column.foreign_key.normalised()
            if ref_table not in self._tables:
                raise ValueError(
                    f"Referenced table '{column.foreign_key.referenced_table}' does not exist"
                )
            ref_table_obj = self._tables[ref_table]
            if ref_table_obj.primary_key() != _normalise(
                column.foreign_key.referenced_column
            ):
                raise ValueError(
                    "Foreign keys must reference the primary key of the parent table"
                )
            self._fk_children.setdefault(ref_table, []).append(
                (table_name, column.normalised_name)
            )

    def _enforce_foreign_keys(self, table: Table, row: Dict[str, Any]) -> None:
        for column in table.normalised_column_names():
            col_meta = table.get_column(column)
            if col_meta.foreign_key is None:
                continue
            fk_column, fk_table_name, fk_ref_column = col_meta.foreign_key.normalised()
            if row[fk_column] is None:
                continue
            parent_table = self._tables.get(fk_table_name)
            if parent_table is None:
                raise ValueError(
                    f"Referenced table '{col_meta.foreign_key.referenced_table}' does not exist"
                )
            parent_row = parent_table.fetch_by_pk(row[fk_column])
            if parent_row is None:
                raise ValueError(
                    f"Foreign key constraint failed on column '{col_meta.name}'"
                )

    def _ensure_no_dependent_rows(self, table_name: str, pk_value: Any) -> None:
        norm_name = _normalise(table_name)
        for child_table_name, child_column in self._fk_children.get(norm_name, []):
            child_table = self._tables.get(child_table_name)
            if child_table is None:
                continue
            for row in child_table.rows():
                if row[child_column] == pk_value:
                    raise ValueError(
                        f"Cannot modify or delete row because of existing references in table '{child_table.name}'"
                    )
