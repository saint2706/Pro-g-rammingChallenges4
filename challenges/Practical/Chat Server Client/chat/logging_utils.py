from __future__ import annotations

import logging
from logging.handlers import RotatingFileHandler
from pathlib import Path
from typing import Optional


def setup_rotating_log(
    path: Path, max_bytes: int = 1_000_000, backups: int = 5
) -> logging.Logger:
    path.parent.mkdir(parents=True, exist_ok=True)
    logger = logging.getLogger(str(path))
    if logger.handlers:
        return logger

    logger.setLevel(logging.INFO)
    handler = RotatingFileHandler(
        path, maxBytes=max_bytes, backupCount=backups, encoding="utf-8"
    )
    formatter = logging.Formatter("%(asctime)s %(levelname)s %(message)s")
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    return logger


def configure_root_logger(
    level: int = logging.INFO, stream: Optional[logging.Handler] = None
) -> None:
    logging.basicConfig(level=level, handlers=[stream] if stream else None)
