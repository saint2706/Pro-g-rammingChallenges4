@echo off
REM Wrapper for Windows to call the Python todo CLI.
SETLOCAL ENABLEDELAYEDEXPANSION
CD /D "%~dp0"
python todo.py %*
ENDLOCAL