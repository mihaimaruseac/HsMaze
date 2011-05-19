Robot in a Maze
===============

A. About
........

This is a homework for the Machine Learning Course that I am taking right now
at my University.

The homework uses genetic algorithms in order to enable a robot to find its way
out of a randomly generated maze.

The assignment is done in Haskell.

B. Usage
........

Run ``make run`` to build and start the application. The GUI is pretty
simple to use.

The GUI shows a maze and each robot as it tries to go out of the maze. It is
very funny to watch how the robot gets stuck then a random mutation evolves it
to a new strategy and so on.

The application will also dump several informations to a file. Use
``./plot.sh`` to plot them nicely or to learn what is dumped.

The maze was generated with Sidewinder's algorithm. The maximum fitness is
somewhere around ``100 * (n^2 - t) + 50 * t + 20 * n`` where ``n`` is the size
of the maze and ``t`` is the length of the escape path.

