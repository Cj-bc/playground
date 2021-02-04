from maze import Maze, Direction, InvalidMazeSizeError
from solver import Solver
from bot import Bot
from game import GameState
import click
import logging
import rich
import rich.panel
import urwid

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
    rich.print(rich.panel.Panel.fit(maze.draw()))

    urwid_maze  = urwid.Text(maze.draw())
    urwid_usage = urwid.Text("左: h, 下: j, 上: k, 右: l, 終了: q")

    state = GameState(maze)
    def eHandler(key):
        """ Playで使われるurwid用のイベントハンドラ関数 """
        if key == "h":
            maze.movePlayer(Direction.LEFT)
            urwid_maze.set_text(maze.draw())
            state.step()
        elif key == "j":
            maze.movePlayer(Direction.DOWN)
            urwid_maze.set_text(maze.draw())
            state.step()
        elif key == "k":
            maze.movePlayer(Direction.UP)
            urwid_maze.set_text(maze.draw())
            state.step()
        elif key == "l":
            maze.movePlayer(Direction.RIGHT)
            urwid_maze.set_text(maze.draw())
            state.step()
        elif key == "q":
            raise urwid.ExitMainLoop()
        else:
            mainlogger.info("予期しない入力のため、なにもしませんでした")

        if maze.isGoal():
            raise urwid.ExitMainLoop()


    loop = urwid.MainLoop(urwid.ListBox(urwid.SimpleListWalker([urwid_maze, urwid_usage]))
                         , unhandled_input=eHandler)
    loop.run()
    if maze.isGoal():
        click.echo(f"おめでとうございます！{state.steps}手でゴールしました！")





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

    bot = Bot(maze)
    bot.run()
    rich.print(maze.draw())
    print(f"{bot.step}手でゴールしました")
if __name__ == '__main__':
    try:
        cli()
    except InvalidMazeSizeError:
        pass
