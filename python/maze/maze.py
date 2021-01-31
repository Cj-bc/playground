from typing import List, Tuple
import sys
import random
import logging
from enum import Enum
import copy
import rich

class InvalidMazeSizeError(Exception):
  pass

class Direction(Enum):
    UP = 0
    DOWN = 1
    LEFT = 2
    RIGHT = 3

class CellType(Enum):
    PATH = 0
    WALL = 1
    PLAYER = 2
    GOAL = 3
    START = 4
    FOOTPRINT = 5

# 配列だと引数の順番がこんがらがってきたので作成
class Coord:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __str__(self):
        return f"({self.x}, {self.y})"

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def right(self):
      return Coord(self.x+1, self.y)

    def left(self):
      return Coord(self.x-1, self.y)

    def up(self):
      return Coord(self.x, self.y-1)

    def down(self):
      return Coord(self.x, self.y+1)


class Maze:
    _width = 0
    _height = 0

    _data = [0][0] # _data[y][x]。 [x][y]ではないので注意
    start: Coord = None
    goal: Coord = None
    _goalProposalList: List[Coord] = [] # 生成途中用

    # 変数名がわかりづらいので改名_startPath
    _nextDigProposals: List[Coord] = []

    _isCreated = False

    _playerPoint: Coord = None
    _playerMovementHistory: List[Coord] = []


    logger: logging.Logger

    # Utilities {{{
    def isValidCoord(self, c: Coord) -> bool:
        """ この迷路内に存在する座標かどうかを判定する
        """
        if c.x < 0 or self._width - 1 < c.x:
            return False
        if c.y < 0 or self._height - 1 < c.y:
            return False

        return True

    def isCellType(self, c: Coord, cellType: CellType) -> bool:
        # andは、前者が失敗していれば後者を実行しない(はず)
        return self.isValidCoord(c) and self._data[c.y][c.x] == cellType

    def isGoal(self) -> bool:
        return self._playerPoint == self.goal
    # }}}

    # Initializations {{{
    def __init__(self, width: int, height: int, logger=None) -> None:
        if logger is None:
            self.logger = logging.getLogger("Maze.{__name__}")
            logging.Formatter
            _handler = logging.StreamHandler()
            _handler.setLevel(logging.INFO)
            self.logger.setLevel(logging.INFO)
            self.logger.addHandler(_handler)
        else:
            self.logger = logger
            self.logger.info("logger was replaced")

        if width < 5 or height < 5 or width % 2 == 0 or height % 2 == 0:
            self.logger.error("迷路の縦・横は5以上の奇数である必要があります")
            raise InvalidMazeSizeError


        self._width = width
        self._height = height

    def create(self):
        """迷路を生成する
        """
        self._isCreated = False
        self._data = [[CellType.WALL for _ in range(0, self._width)] for _ in range(0, self._height)]

        firstPos = Coord(random.randint(1, self._width-2), random.randint(1, self._height-2))
        self.dig(firstPos)
        self.setStart(firstPos)
        self.setPlayer(firstPos)
        self._setGoal()


    def _setGoal(self):
        """ ゴール地点をself._goalProposalListからランダムに設定する
        """
        if self.goal is not None:
            self.logger.debug("ゴール地点が既に設定されているため、設定しませんでした")
            return
        if len(self._goalProposalList) == 0:
            self.logger.debug("ゴール地点の候補リストが存在しないため、設定できませんでした")
            return
        self.goal = random.choice(self._goalProposalList)

    def setStart(self, c: Coord) -> None:
        """ スタート地点を設定する。
            先に迷路を生成すること。
        """
        if not self._isCreated:
            self.logger.info("迷路が生成されていないため、スタート地点が設定できませんでした")
            return
        if not self.isValidCoord(c):
            self.logger.info("座標{c}は有効な座標ではないため、スタート地点が設定できませんでした")
            return

        self.start = c

    def dig(self, c):
        """
        終了するまで掘る。一マスのみ掘るのではない。
        再帰だと上限に引っかかる可能性があるため、ループで処理する

        1. 上下左右の方向で、どの方向が掘り進められるのか判定する。掘り進められるかどうかの定義は、現在の座標から1マス先及び2マス先の座標まで壁となっていることである
        2. 掘り進めることが可能な方向をランダムに選択して、2マス先まで道として、現在位置を更新する
        3. 現在位置をチェックポイントとしてリストに追加し、保存する
        4. 掘り進めることができなくなるまで1.~3.を繰り返す
        5. 掘り進めることができなくなったら、これまで3.に記録していた座標からどこかしらかに掘り進めることが可能な座標を取得してまた1.からの作業を行う
        6. どこにも掘ることが出来る座標がなくなった場合は、その時点で処理を終了する
        """

        if not self.isValidCoord(c):
            self.logger.info(f"座標({c.x}, {c.y})はこの迷路の有効範囲外のため、digに失敗しました")
            return

        self._data[c.y][c.x] = CellType.PATH
        # TODO: この無限ループは最大4回しか呼び出されないはず。
        # Forなどを使った方がわかりやすくなるかもしれない
        #
        # ここの無限ループの趣旨は、「digに与えられた座標の上下左右それぞれについて掘れるか確認し、掘れるなら掘る」
        # なので実際はそこまで無限にループしない
        while True:
            digDirections = []
            self.logger.debug("start checking diggable directions")
            # 必ず2マス同時に掘り進めるため、一マス横が掘れるかどうかは確認しなくて良い
            # (ﾁｮｯﾄしかわからん)
            if self.isValidCoord(Coord(c.x, c.y - 2)) and self.isCellType(Coord(c.x, c.y-2), CellType.WALL):
                digDirections.append(Direction.UP)
            if self.isValidCoord(Coord(c.x, c.y + 2)) and self.isCellType(Coord(c.x, c.y+2), CellType.WALL):
                digDirections.append(Direction.DOWN)
            if self.isValidCoord(Coord(c.x - 2, c.y)) and self.isCellType(Coord(c.x-2, c.y), CellType.WALL):
                digDirections.append(Direction.LEFT)
            if self.isValidCoord(Coord(c.x + 2, c.y)) and self.isCellType(Coord(c.x+2, c.y), CellType.WALL):
                digDirections.append(Direction.RIGHT)

            if len(digDirections) == 0:
                # このif分岐は元の座標から4方向全て掘った後に通る場所なので、
                # ここにくる時点では行き止まりかはわからない
                #
                # 行き止まりならゴールの候補地にする
                if len(list(filter(lambda n: n == True
                                  , [self.isCellType(Coord(c.x-1,c.y), CellType.WALL)
                                    ,self.isCellType(Coord(c.x+1,c.y), CellType.WALL)
                                    ,self.isCellType(Coord(c.x,c.y-1), CellType.WALL)
                                    ,self.isCellType(Coord(c.x,c.y+1), CellType.WALL)]))) == 3:
                    self._goalProposalList.append(c)
                break

            direction = random.choice(digDirections)
            self.logger.debug(f"direction {direction} is chosen by random choice")

            if direction == Direction.UP:
                self._data[c.y - 1][c.x] = CellType.PATH
                self._data[c.y - 2][c.x] = CellType.PATH
                self._nextDigProposals.append(Coord(c.x, c.y - 2))
            elif direction == Direction.DOWN:
                self._data[c.y + 1][c.x] = CellType.PATH
                self._data[c.y + 2][c.x] = CellType.PATH
                self._nextDigProposals.append(Coord(c.x, c.y + 2))
            elif direction == Direction.LEFT:
                self._data[c.y][c.x - 1] = CellType.PATH
                self._data[c.y][c.x - 2] = CellType.PATH
                self._nextDigProposals.append(Coord(c.x - 2, c.y))
            elif direction == Direction.RIGHT:
                self._data[c.y][c.x + 1] = CellType.PATH
                self._data[c.y][c.x + 2] = CellType.PATH
                self._nextDigProposals.append(Coord(c.x + 2, c.y))

        nextDigProposalsLen = len(self._nextDigProposals)
        self.logger.debug(f"nextDigProposalsLen: {nextDigProposalsLen}")
        if nextDigProposalsLen > 0:
            path = self._nextDigProposals.pop(random.randint(0, nextDigProposalsLen - 1))
            self.dig(path)
        else:
            self._isCreated = True
    # }}}

    # Player movements {{{
    def setPlayer(self, c: Coord):
        if c.x < 0 or self._width < c.x:
            self.logger.debug(f"x座標'{c.x}'は有効範囲外のため、動かせません")
            return
        if c.y < 0 or self._height < c.y:
            self.logger.debug(f"y座標'{c.y}'は有効範囲外のため、動かせません")
            return

        if not self.isCellType(c, CellType.PATH):
            self.logger.debug(f"座標({c.x}, {c.y})は道ではないため、動かせません")
            return

        prevPlayerCoord = self._playerPoint
        self._playerPoint = c

        if c in self._playerMovementHistory:
            self._playerMovementHistory.remove(c)
            if prevPlayerCoord in self._playerMovementHistory:
                self._playerMovementHistory.remove(prevPlayerCoord)

        if not c in self._playerMovementHistory:
            self._playerMovementHistory.append(c)


    def movePlayer(self, direction):
        if self._playerPoint is None:
            self.logger.debug("プレイヤーが迷路内に存在しないため、動かせません")
            return
        pos = self._playerPoint
        checkpos: Coord
        if direction == Direction.UP:
            checkpos = Coord(pos.x, pos.y - 1)
        elif direction == Direction.DOWN:
            checkpos = Coord(pos.x, pos.y + 1)
        elif direction == Direction.LEFT:
            checkpos = Coord(pos.x - 1, pos.y)
        elif direction == Direction.RIGHT:
            checkpos = Coord(pos.x + 1, pos.y)
        else:
            self.logger.warn(f"direction: '{direction}' is not recognized")

        self.setPlayer(checkpos)
    # }}}

    def draw(self):
        if not self._isCreated:
            self.logger.info("迷路は生成されていないため描画できません")
            return

        result = ""

        # PLAYER,GOAL, STARTは_data内に存在しないため、突っ込む
        # PLAYERを最後に入れることで、スタート時点やゴール地点にいる際もプレイヤーを表示させる。
        self.logger.debug(f"data is {self._data}")
        rendering = copy.deepcopy(self._data)
        for c in self._playerMovementHistory:
            rendering[c.y][c.x] = CellType.FOOTPRINT
        rendering[self.goal.y][self.goal.x] = CellType.GOAL
        rendering[self.start.y][self.start.x] = CellType.START
        if self._playerPoint is not None:
            self.logger.debug("Add player to the rendering")
            rendering[self._playerPoint.y][self._playerPoint.x] = CellType.PLAYER

        for row in rendering:
            for cell in row:
                if cell == CellType.PATH:
                    result += " "
                elif cell == CellType.WALL:
                    result += "#"
                elif cell == CellType.PLAYER:
                    result += "[red]@[/red]"
                elif cell == CellType.GOAL:
                    result += "G"
                elif cell == CellType.START:
                    result += "S"
                elif cell == CellType.FOOTPRINT:
                    result += "[on green] [/on green]"
                else:
                    self.logger.error(f"row: '{row}' is neither PATH nor WALL nor PLAYER")
            result += "\n"

        rich.print(result)
