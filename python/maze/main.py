from maze import Maze, Direction
from solver import Solver
import click
import logging

@click.group()
def cli():
    pass

@cli.command()
def play():
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
    maze.draw()

    steps = 0
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

        steps+= 1

        if maze.isGoal():
            click.echo(f"おめでとうございます！{steps}手でゴールしました！")
            break

if __name__ == '__main__':
    cli()
