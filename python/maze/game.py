from maze import Maze

class GameState:
    """ ゲームの状態を保存するクラス。
        本来はeHandlerでstepsを常時インクリメントしたいが、状態を値渡しできなさそうなので
        オブジェクトに保存して更新させる。
    """
    maze: Maze
    steps: int

    def __init__(self, m):
        self.maze = m
        self.steps = 0

    def step(self):
        self.steps += 1
