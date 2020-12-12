Ocaml project on Ford-FUlkerson.

Features implemented in the project: 
-Ford-Fulkerson algorithm which can be applied on graph to find max flow from one node to another
-Resolution of Bipartite Matching (unweighted) using Ford-Fulkerson with input files that are easier for the user, as well as an interpretation of the output 

What has yet to be implemented:
-Max Flow min Cost algorithm using Bellman Ford to find negative cycles
-Using this same algorithm to resolve weighted Bipartite matching (to account for user's preferences)

How to use the project: 
Compile it using ctrl+shift+b in vscode or just make clean then make build in terminal

To run project use command ./ftest.native input_file mode output source sink

2 modes are implemented right now:
1: using max flow algorithm on graph (input_file) with source and sink which are chosen by user - mode=1 in command
2: solving bipartite matching with custom input file (input_file) -mode= 2 in command

Explanation of the custom input file system:
Each item which is meant to be associated with an item of the other group (so all the items) is given a unique number by the user (except 0 or -1 which would make the program brake).
To create an association between an item x and an item y, the user has to write x y on a new line in the input file.
All items of the same group have to be written on same side of input file.
For example you write x y to associate x with y, all of the items which are in the same group as x have to be written on the left side.
An example is given in ./graphs/graph3.txt 

In both modes to visualize output graph write command dot -Tsvg (your output) > (choose name).svg