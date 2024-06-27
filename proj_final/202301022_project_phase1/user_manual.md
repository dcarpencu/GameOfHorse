# Artificial Intelligence User Manual

# Game of horse

<h1>Student: David-Ioan Carpencu 202301022 </h1>

## Table of Contents

1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [Features](#features)
   - [1. Game Overview](#game-overview)
   - [2. Implemented Algorithms](#implemented-algorithms)
   - [3. User Interaction](#user-interaction)
4. [Algorithms](#alg)

## Introduction <a name="introduction"></a>

This document serves as the user manual for the Horse Game project developed as part of the Artificial Intelligence course. It provides instructions on how to install and interact with the game.

## Getting Started <a name="getting-started"></a>

To get started with the Horse Game project, follow these steps:

1. Compile the `project.lisp` file in the LispWorks IDE.
2. Run the program by typing `(start)` in the console.

## Features <a name="features"></a>

### 1. Game Overview <a name="game-overview"></a>

The Horse Game is implemented in LISP and consists of the following files:

- `search.lisp`: Implementation of search methods and efficiency analysis metrics.
- `puzzle.lisp`: Solves specific problems, defining operators and heuristics for the application domain.
- `project.lisp`: Handles user interaction, file reading and writing.

### 2. Implemented Algorithms <a name="implemented-algorithms"></a>

Two algorithms are available for solving the game:

- Breadth-First Search (BFS)
- Depth-First Search (DFS)
- A\*

### 3. User Interaction <a name="user-interaction"></a>

Users can interact with the Horse Game by following the instructions provided in the console.

### Algorithms <a name="alg"></a>

The Horse Game implements Breadth-First Search (BFS), Depth-First Search (DFS) and A\*. The user can choose the prefered algorithm to use.
