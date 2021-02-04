from maze import Maze
from solver import Solver

class Bot:
    maze: Maze
    solver: Solver
    step: int

    def __init__(self, maze):
        self.maze = maze
        self.solver = Solver(self.maze)
        self.step = 0

    def run(self):
        """ Run auto solving
        """
        for p in self.solver.solve():
            self.maze.movePlayer(p)
            self.step += 1
