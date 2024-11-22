Functional vs. Imperative Data Structures Project
This project provides a comprehensive analysis and comparison of purely functional data structures (lists and trees) versus their imperative counterparts. We focus on evaluating memory usage, performance, and the benefits of parallelization in both programming paradigms.

Project Overview
The aim is to assess how purely functional data structures are implemented in languages like Haskell and compare them with imperative implementations in languages such as C++. The project explores:

Memory Usage: Analyzing the memory footprint of data structures in both paradigms.
Performance: Benchmarking time complexity for operations like insertion, deletion, and lookup.
Parallelization Performance: Evaluating the effectiveness of parallelizing operations in functional versus imperative structures.
Added Values: Discussing the benefits of immutability, ease of reasoning, and other functional advantages.
Project Structure
/Cpp:
bst.cpp: C++ implementation of binary search trees and benchmarking code.
/Haskell:
BST.hs: Haskell implementation of binary search trees with benchmarking functionalities.
Getting Started
These instructions will help you get a copy of the project up and running on your local machine for development, testing, and contribution purposes.

Prerequisites
Ensure you have the following installed:

C++ Compiler (e.g., g++, Clang) for C++ code
GHC and Cabal for Haskell code
Installing and Running
C++ Implementation
bash
Copy code
g++ -o bst bst.cpp
./bst
Haskell Implementation
bash
Copy code
cabal build
cabal run
Built With
C++ - Used for imperative data structure implementations.
Haskell - Used for functional data structure implementations.
Analysis and Results
Documentation: Detailed findings on performance, memory usage, and parallelization are documented in the docs folder.
Comparative Analysis: Comparative graphs and charts are available in the analysis folder.
Authors
Your Name
License
This project is licensed under the MIT License - see the LICENSE.md file for details.

Acknowledgments
Inspiration from Chris Okasaki's "Purely Functional Data Structures"
Community contributions and discussions
