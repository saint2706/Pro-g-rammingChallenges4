"""Small SQL parser for the supported subset."""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any, Dict, Iterable, List, Optional, Tuple

from .storage import Column, ForeignKey

TOKEN_PATTERN = re.compile(
    r"\s*("  # leading whitespace
    r"'[^']*'"  # single quoted string
    r"|\*"  # star
    r"|\(|\)"  # parentheses
    r"|,|=|;"  # punctuation
    r"|[A-Za-z_][A-Za-z0-9_]*"  # identifiers
    r"|\d+"  # integers
    r")"
)


@dataclass
class Statement:
    kind: str
    payload: Dict[str, Any]


class TokenStream:
    def __init__(self, tokens: Iterable[str]):
        self._tokens = list(tokens)
        self._index = 0

    def peek(self) -> Optional[str]:
        if self._index >= len(self._tokens):
            return None
        return self._tokens[self._index]

    def consume(self, expected: Optional[str] = None) -> str:
        token = self.peek()
        if token is None:
            raise ValueError("Unexpected end of input")
        if expected is not None and token.upper() != expected.upper():
            raise ValueError(f"Expected '{expected}' but found '{token}'")
        self._index += 1
        return token

    def match(self, value: str) -> bool:
        token = self.peek()
        if token is None:
            return False
        if token.upper() == value.upper():
            self._index += 1
            return True
        return False

    def eof(self) -> bool:
        return self.peek() is None


class SQLParser:
    """Parses SQL text into structured statements."""

    def parse(self, sql: str) -> List[Statement]:
        tokens = [token for token in TOKEN_PATTERN.findall(sql) if token.strip()]
        stream = TokenStream(tokens)
        statements: List[Statement] = []
        while not stream.eof():
            if stream.match(";"):
                continue
            statements.append(self._parse_statement(stream))
            if stream.peek() == ";":
                stream.consume(";")
        return statements

    # ------------------------------------------------------------------
    def _parse_statement(self, stream: TokenStream) -> Statement:
        token = stream.peek()
        if token is None:
            raise ValueError("Empty statement")
        keyword = token.upper()
        if keyword == "CREATE":
            return self._parse_create_table(stream)
        if keyword == "DROP":
            return self._parse_drop_table(stream)
        if keyword == "INSERT":
            return self._parse_insert(stream)
        if keyword == "SELECT":
            return self._parse_select(stream)
        if keyword == "UPDATE":
            return self._parse_update(stream)
        if keyword == "DELETE":
            return self._parse_delete(stream)
        if keyword in {"BEGIN", "COMMIT", "ROLLBACK"}:
            stream.consume()
            return Statement(kind=keyword.lower(), payload={})
        raise ValueError(f"Unsupported statement starting with '{token}'")

    def _parse_create_table(self, stream: TokenStream) -> Statement:
        stream.consume("CREATE")
        stream.consume("TABLE")
        table_name = stream.consume()
        stream.consume("(")
        columns: List[Column] = []
        while True:
            column_name = stream.consume()
            column_type = stream.consume()
            col_kwargs: Dict[str, Any] = {}
            if stream.match("PRIMARY"):
                stream.consume("KEY")
                col_kwargs["primary_key"] = True
            fk: Optional[ForeignKey] = None
            if stream.match("REFERENCES"):
                ref_table = stream.consume()
                stream.consume("(")
                ref_column = stream.consume()
                stream.consume(")")
                fk = ForeignKey(
                    column=column_name,
                    referenced_table=ref_table,
                    referenced_column=ref_column,
                )
                col_kwargs["foreign_key"] = fk
            columns.append(Column(name=column_name, col_type=column_type, **col_kwargs))
            if stream.match(","):
                continue
            break
        stream.consume(")")
        return Statement(
            kind="create_table",
            payload={"name": table_name, "columns": columns},
        )

    def _parse_drop_table(self, stream: TokenStream) -> Statement:
        stream.consume("DROP")
        stream.consume("TABLE")
        name = stream.consume()
        return Statement(kind="drop_table", payload={"name": name})

    def _parse_insert(self, stream: TokenStream) -> Statement:
        stream.consume("INSERT")
        stream.consume("INTO")
        table_name = stream.consume()
        columns: Optional[List[str]] = None
        if stream.match("("):
            columns = []
            while True:
                columns.append(stream.consume())
                if stream.match(","):
                    continue
                break
            stream.consume(")")
        stream.consume("VALUES")
        values: List[List[Any]] = []
        while True:
            stream.consume("(")
            row: List[Any] = []
            while True:
                row.append(self._parse_value(stream.consume()))
                if stream.match(","):
                    continue
                break
            stream.consume(")")
            values.append(row)
            if stream.match(","):
                continue
            break
        return Statement(
            kind="insert",
            payload={"table": table_name, "columns": columns, "values": values},
        )

    def _parse_select(self, stream: TokenStream) -> Statement:
        stream.consume("SELECT")
        columns: List[str] = []
        while True:
            columns.append(stream.consume())
            if stream.match(","):
                continue
            break
        stream.consume("FROM")
        table_name = stream.consume()
        predicate = self._parse_where(stream)
        return Statement(
            kind="select",
            payload={"table": table_name, "columns": columns, "where": predicate},
        )

    def _parse_update(self, stream: TokenStream) -> Statement:
        stream.consume("UPDATE")
        table_name = stream.consume()
        stream.consume("SET")
        assignments: Dict[str, Any] = {}
        while True:
            column = stream.consume()
            stream.consume("=")
            assignments[column] = self._parse_value(stream.consume())
            if stream.match(","):
                continue
            break
        predicate = self._parse_where(stream)
        return Statement(
            kind="update",
            payload={"table": table_name, "values": assignments, "where": predicate},
        )

    def _parse_delete(self, stream: TokenStream) -> Statement:
        stream.consume("DELETE")
        stream.consume("FROM")
        table_name = stream.consume()
        predicate = self._parse_where(stream)
        return Statement(
            kind="delete",
            payload={"table": table_name, "where": predicate},
        )

    def _parse_where(self, stream: TokenStream) -> Optional[List[Tuple[str, Any]]]:
        if not stream.match("WHERE"):
            return None
        predicates: List[Tuple[str, Any]] = []
        while True:
            column = stream.consume()
            stream.consume("=")
            predicates.append((column, self._parse_value(stream.consume())))
            if stream.match("AND"):
                continue
            break
        return predicates

    def _parse_value(self, token: str) -> Any:
        if token.startswith("'") and token.endswith("'"):
            return token[1:-1]
        if token.upper() == "NULL":
            return None
        if token.isdigit():
            return int(token)
        return token


__all__ = ["SQLParser", "Statement"]
