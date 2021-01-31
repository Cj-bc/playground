from maze import Maze
from solver import Solver

class Bot:
    maze: Maze
    solver: Solver

    def __init__(self, maze):
        self.maze = maze
        self.solver = Solver(self.maze)

    def run(self):
        """ Run auto solving
        """
        for p in self.solver.solve():
            self.maze.movePlayer(p)
