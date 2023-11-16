# haskell-propositional

# Haskell SAT Solver and Formula Manipulation

## Overview

This project is a Haskell implementation of a SAT (Boolean satisfiability problem) solver, along with modules for handling logical formulas and their conversion to CNF (Conjunctive Normal Form). The primary objective is to determine if a logical formula is satisfiable. The project comprises three main modules: `Formula`, `Literal`, and `NormalForm`, which collectively handle the creation, manipulation, and evaluation of logical formulas.

### Modules

- **Formula.hs**: Defines the data structure for logical formulas. It includes functions for creating formulas and performing logical operations such as conjunction, disjunction, and negation.

- **Literal.hs**: Manages literals, the basic elements of formulas. It provides functionality for creating literals from boolean values and logical variables, along with their negations.

- **NormalForm.hs**: Responsible for converting logical formulas into their Conjunctive Normal Form (CNF). This transformation is a prerequisite for the SAT solving process.

### SAT Solver

The core of the project is a basic implementation of the DPLL (Davis–Putnam–Logemann–Loveland) algorithm. This algorithm is a recursive, backtracking-based search method used for determining the satisfiability of boolean formulas. The solver simplifies a given formula based on variable assignments and recursively checks for satisfiability.

## Features

- Creation and manipulation of logical formulas.
- Conversion of formulas to CNF.
- A rudimentary SAT solver based on the DPLL algorithm.
- Evaluation of formula satisfiability.

## Usage

To use the modules, import them into your Haskell script. You can create formulas using the `Formula.hs` module, convert them to CNF using `NormalForm.hs`, and then check for satisfiability using the SAT solver.

Example usage:

```haskell
import Formula
import NormalForm
import Literal

-- Creating a formula
let formula = conj (Var "x") (neg (Var "y"))

-- Converting to CNF
let cnf = toCNF formula

-- Checking for satisfiability
let isSatisfiable = isSAT formula
```

## Limitations

This implementation of the SAT solver is basic and not optimized for complex or large formulas. For practical applications, using established SAT solver libraries is recommended.

## Contributing

Contributions to enhance the functionality, improve efficiency, or extend the capabilities of the solver and formula manipulation modules are welcome.

