Drawing graphs using graphviz.
==============================
In this chapter of the book, the idea was to work in depth with alists, lambda
functions and other new concepts that were being placed in the way.

As a result, the functions graph->png and ugraph->png has been generated.

To use it is really simple, first load graph-util.lisp in clisp:
   
   clisp -i graphs-util.lisp

And then try to call, the functions:

   (graph->png "directed-graph.png" *wizzard-nodes* *wizzard-edges*)

and
   
   (ugraph->png "undirected-graph.png" *wizzard-nodes* *wizzard-edges*)
   
To check the difference of the png files generated use your favorite browser,
and voila.

These functions, are useful to paint the map of the wizzard, done in the
previous chapters, but, is can be reused, if one have a par of similar
structures, so that the first one is an alist with the nodes of the graph and
their description; and the second one is an alist of alist, that represents the
edges.
In the example I'm using this alists:

   defparameter *wizzard-nodes* '(
         (living-room (You are in the living-room. A wizzard is snoring loudly on the couch.))
         (garden (You are in a beatiful garden. There is a well in front of you))
         (attic (You are in the attic. There is a gigant welding torch in the corner.))))
   
   (defparameter *wizzard-edges* '(
         (living-room (garden west door) (attic upstairs ladder))
         (garden (living-room east door))
         (attic (living-room downstairs ladder))))
