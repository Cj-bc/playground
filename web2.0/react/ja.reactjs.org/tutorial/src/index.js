import React from 'react';
import ReactDOM from 'react-dom/client';
import './index.css'

function Square(props) {
    return (
	<button className="square" onClick={props.handleClick}>
        {props.value}
      </button>
    );
}

class Board extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
	cells: Array(9).fill(null),
	IsNextX: true
    };
  }

  renderSquare(i) {
    return <Square value={this.state.cells[i]} handleClick={() => this.handleClick(i)} />;
  }

  handleClick(index) {
      console.log("handle click called!");
      const cells  = this.state.cells.slice();
      cells[index] = this.state.IsNextX ? 'X' : 'O';
      this.setState({cells: cells
		     , IsNextX: !this.state.IsNextX});
  }

  render() {
    const status = 'Next player: ' + (this.state.IsNextX ? 'X' : 'O');

    return (
      <div>
        <div className="status">{status}</div>
        <div className="board-row">
          {this.renderSquare(0)}
          {this.renderSquare(1)}
          {this.renderSquare(2)}
        </div>
        <div className="board-row">
          {this.renderSquare(3)}
          {this.renderSquare(4)}
          {this.renderSquare(5)}
        </div>
        <div className="board-row">
          {this.renderSquare(6)}
          {this.renderSquare(7)}
          {this.renderSquare(8)}
        </div>
      </div>
    );
  }
}

class Game extends React.Component {
  render() {
    return (
      <div className="game">
        <div className="game-board">
          <Board />
        </div>
        <div className="game-info">
          <div>{/* status */}</div>
          <ol>{/* TODO */}</ol>
        </div>
      </div>
    );
  }
}

// ========================================

const root = ReactDOM.createRoot(document.getElementById("root"));
root.render(<Game />);
