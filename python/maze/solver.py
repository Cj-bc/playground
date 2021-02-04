from typing import List
import random
from maze import Maze, Coord, CellType, Direction
import logging

class Solver:
    maze: Maze

    logger: logging.Logger

    def __init__(self, m):
        solverLogger = logging.getLogger(__name__)
        solverLogger.setLevel(logging.DEBUG)
        h = logging.StreamHandler()
        h.setLevel(logging.ERROR)
        solverLogger.addHandler(h)

        self.logger = solverLogger
        self.maze = m

    def solve(self):
        ret = self.loop(self.maze.start, self.maze.start)
        if ret is None:
            self.logger.info("見つかりませんでした")
            return
        ret.reverse()
        return ret

    def loop(self, prev, c) -> List[Direction]:
        self.logger.debug(f"Looking around {c}...")
        for d,c_looking in filter(lambda s: s[1] != prev, [(Direction.UP,     c.up())
                                                          , (Direction.DOWN,  c.down())
                                                          , (Direction.RIGHT, c.right())
                                                          , (Direction.LEFT,  c.left())]):
            if c_looking == self.maze.goal:
                self.logger.debug(f"{c_looking} is goal. Return")
                return [d]

            if self.maze.isCellType(c_looking, CellType.PATH):
                self.logger.debug(f"Dig into {c_looking}...")
                ret = self.loop(c, c_looking)
                if ret != None:
                    self.logger.debug(f"Back...")
                    ret.append(d)
                    return ret

        return None
