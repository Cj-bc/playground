from typing import List, Tuple
import sys
import random
import logging
from enum import Enum
import copy


class Direction(Enum):
    UP = 0
    DOWN = 1
    LEFT = 2
    RIGHT = 3

# 配列だと引数の順番がこんがらがってきたので作成
class Coord:
    def __init__(self, x, y):
        self.x = x
        self.y = y

class Maze:
    _width = 0
    _height = 0

    _data = [0][0] # _data[y][x]。 [x][y]ではないので注意
    start: Coord = None
    goal: Coord = None
    _goalProposalList: List[Coord] = [] # 生成途中用

    # 変数名がわかりづらいので改名_startPath
    _breadcrumb: List[Coord] = []

    _isCreated = False

    _playerPoint = None

    PATH = 0
    WALL = 1
    PLAYER = 2
    GOAL = 3
    START = 4

    logger: logging.Logger

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
            return

        self._width = width
        self._height = height

    def isValidCoord(self, x: int, y: int) -> bool:
        """ この迷路内に存在する座標かどうかを判定する
        """
        if x < 0 or self._width - 1 < x:
            return False
        if y < 0 or self._height - 1 < y:
            return False

        return True

    def isCellType(self, x: int, y: int, cellType) -> bool:
        # andは、前者が失敗していれば後者を実行しない(はず)
        return self.isValidCoord(x, y) and self._data[y][x] == cellType

    def create(self):
        """迷路を生成する
        """
        self._isCreated = False
        self._data = [[Maze.WALL for _ in range(0, self._width)] for _ in range(0, self._height)]


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
        if not self.isValidCoord(c.x,c.y):
            self.logger.info("座標{c}は有効な座標ではないため、スタート地点が設定できませんでした")
            return

        self.start = c

    def dig(self, x, y):
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

        if not self.isValidCoord(x, y):
            self.logger.info(f"座標({x}, {y})はこの迷路の有効範囲外のため、digに失敗しました")
            return

        self._data[y][x] = Maze.PATH
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
            if self.isValidCoord(x, y - 2) and self.isCellType(x, y-2, Maze.WALL):
                digDirections.append(Direction.UP)
            if self.isValidCoord(x, y + 2) and self.isCellType(x, y+2, Maze.WALL):
                digDirections.append(Direction.DOWN)
            if self.isValidCoord(x - 2, y) and self.isCellType(x-2, y, Maze.WALL):
                digDirections.append(Direction.LEFT)
            if self.isValidCoord(x + 2, y) and self.isCellType(x+2, y, Maze.WALL):
                digDirections.append(Direction.RIGHT)

            if len(digDirections) == 0:
                # このif分岐は元の座標から4方向全て掘った後に通る場所なので、
                # ここにくる時点では行き止まりかはわからない
                #
                # 行き止まりならゴールの候補地にする
                if len(list(filter(lambda x: x == True
                                  , [self.isCellType(x-1,y, Maze.WALL)
                                    ,self.isCellType(x+1,y, Maze.WALL)
                                    ,self.isCellType(x,y-1, Maze.WALL)
                                    ,self.isCellType(x,y+1, Maze.WALL)]))) == 3:
                    self._goalProposalList.append(Coord(x,y))
                break

            direction = random.choice(digDirections)
            self.logger.debug(f"direction {direction} is chosen by random choice")

            if direction == Direction.UP:
                self._data[y - 1][x] = Maze.PATH
                self._data[y - 2][x] = Maze.PATH
                self._breadcrumb.append(Coord(x, y - 2))
            elif direction == Direction.DOWN:
                self._data[y + 1][x] = Maze.PATH
                self._data[y + 2][x] = Maze.PATH
                self._breadcrumb.append(Coord(x, y + 2))
            elif direction == Direction.LEFT:
                self._data[y][x - 1] = Maze.PATH
                self._data[y][x - 2] = Maze.PATH
                self._breadcrumb.append(Coord(x - 2, y))
            elif direction == Direction.RIGHT:
                self._data[y][x + 1] = Maze.PATH
                self._data[y][x + 2] = Maze.PATH
                self._breadcrumb.append(Coord(x + 2, y))

        breadcrumbLen = len(self._breadcrumb)
        self.logger.debug(f"breadcrumbLen: {breadcrumbLen}")
        if breadcrumbLen > 0:
            path = self._breadcrumb.pop(random.randint(0, breadcrumbLen - 1))
            self.dig(path.x, path.y)
        else:
            self._isCreated = True


    def setPlayer(self, x, y):
        if x < 0 or self._width < x:
            self.logger.debug(f"x座標'{x}'は有効範囲外のため、動かせません")
            return
        if y < 0 or self._height < y:
            self.logger.debug(f"y座標'{y}'は有効範囲外のため、動かせません")
            return

        if self._data[y][x] == Maze.PATH:
            self._playerPoint = Coord(x, y)


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

        self.setPlayer(checkpos.x, checkpos.y)

    def draw(self):
        if not self._isCreated:
            self.logger.info("迷路は生成されていないため描画できません")
            return

        result = ""

        # PLAYER,GOAL, STARTは_data内に存在しないため、突っ込む
        # PLAYERを最後に入れることで、スタート時点やゴール地点にいる際もプレイヤーを表示させる。
        self.logger.debug(f"data is {self._data}")
        rendering = copy.deepcopy(self._data)
        rendering[self.goal.y][self.goal.x] = Maze.GOAL
        rendering[self.start.y][self.start.x] = Maze.START
        if self._playerPoint is not None:
            self.logger.debug("Add player to the rendering")
            rendering[self._playerPoint.y][self._playerPoint.x] = Maze.PLAYER

        for row in rendering:
            for cell in row:
                if cell == Maze.PATH:
                    result += " "
                elif cell == Maze.WALL:
                    result += "#"
                elif cell == Maze.PLAYER:
                    result += "@"
                elif cell == Maze.GOAL:
                    result += "%"
                else:
                    self.logger.error(f"row: '{row}' is neither PATH nor WALL nor PLAYER")
            result += "\n"

        print(result)

if __name__ == '__main__':
    # logger {{{
    mainlogger = logging.getLogger("__main__")
    mainlogger.setLevel(logging.INFO)
    h = logging.StreamHandler()
    h.setLevel(logging.DEBUG)
    # TODO: formatterの設定をする
    # _formatter = logging.Formatter("%(name)s: [%(levelname)s]: %(funcName)s %(message)s")
    # _handler.setFormatter(_formatter)
    mainlogger.addHandler(h)
    # }}}

    try:
        width = int(input("迷路の横幅を入力してください"))
        height = int(input("迷路の縦幅を入力してください"))
    except TypeError:
        mainlogger.critical("横幅・縦幅は整数で入力してください")
        sys.exit(-1)

    maze = Maze(width, height, logger=mainlogger)
    maze.create()
    firstPos = Coord(random.randint(0, width-1), random.randint(0, height-1))
    maze.dig(firstPos.x, firstPos.y)
    maze.setStart(firstPos)
    maze.setPlayer(firstPos.x, firstPos.y)
    maze._setGoal()
    maze.draw()

    while True:
        i = input("左: h, 下: j, 上: k, 右: l, 終了: q")
        if i == "h":
            maze.movePlayer(Direction.LEFT)
        elif i == "j":
            maze.movePlayer(Direction.DOWN)
        elif i == "k":
            maze.movePlayer(Direction.UP)
        elif i == "l":
            maze.movePlayer(Direction.RIGHT)
        elif i == "q":
            break
        else:
            mainlogger.info("予期しない入力のため、なにもしませんでした")

        maze.draw()
