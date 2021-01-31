from maze import Maze, Direction, InvalidMazeSizeError
from solver import Solver
import click
import logging


@click.group()
def cli():
    pass

@click.option('--width', default=31, help='迷路の横幅')
@click.option('--height', default=31, help='迷路の縦幅')
@cli.command()
def play(width, height):
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


@click.option('--width', default=31, help='迷路の横幅')
@click.option('--height', default=31, help='迷路の縦幅')
@cli.command()
def solve(width, height):
    mainlogger = logging.getLogger("__main__")
    mainlogger.setLevel(logging.INFO)
    h = logging.StreamHandler()
    h.setLevel(logging.DEBUG)
    mainlogger.addHandler(h)

    maze = Maze(width, height)
    maze.create()
    maze.draw()

    s = Solver(maze)
    answer = s.solve()
    maze.draw()


if __name__ == '__main__':
    cli()
