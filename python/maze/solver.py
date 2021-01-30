from typing import List
import random
from maze import Maze, Coord, CellType
import logging

class Solver:
    maze: Maze

    logger: logging.Logger

    def __init__(self, m):
        solverLogger = logging.getLogger(__name__)
        solverLogger.setLevel(logging.DEBUG)
        h = logging.StreamHandler()
        h.setLevel(logging.DEBUG)
        solverLogger.addHandler(h)

        self.logger = solverLogger
        self.maze = m

    def solve(self):
        ret = self.loop(self.maze.start)
        if ret is None:
            print("見つかりませんでした")
        else:
            print(ret)

    def loop(self, c) -> List[Coord]:
        self.logger.debug(f"Looking around {c}...")
        for n in [c.up(), c.down(), c.right(), c.left()]:
            if n == self.maze.goal:
                self.logger.debug(f"{n} is goal. Return")
                return [n, c]

            if self.maze.isCellType(n, CellType.PATH):
                self.logger.debug(f"Look for {n}...")
                ret = self.loop(n)
                if ret != None:
                    self.logger.debug(f"Back...")
                    return ret.append(c)

        return None

