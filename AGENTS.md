# AGENTS.md

This file provides guidelines for contributors about the repository and develop smoothly.

## 1. Repository Overview
- This is a generator of auto differentiation (AD) code from Fortran code.

## 2. Setup Steps
- `pip install fparser`.

## 3. Code Style and Development Policy
- Python code is recommended to follow [PEP 8](https://www.python.org/dev/peps/pep-0008/).
- The generated automatic differentiation code maintains the structure of the original source code as much as possible.
- The names of modules, functions, subroutines, variables, etc. in the generated code should be the original code names followed by “_ad” as much as possible.

## 4. Commit and PR Guidelines
- Aim for one topic per commit and write clear messages.
- When opening a PR, explain the changes and test steps.
- Run the tests before pushing.
- Messages such as commits and PRs, as well as branch names, should be in English.

## 5. Tests and Examples
- Place the original Fortran code samples under “examples” and the test scripts under “tests.”
