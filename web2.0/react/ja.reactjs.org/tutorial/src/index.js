import React from 'react';
import ReactDOM from 'react-dom/client';
import './index.css'

function calculateWinner(squares) {
  const lines = [
    [0, 1, 2],
    [3, 4, 5],
    [6, 7, 8],
    [0, 3, 6],
    [1, 4, 7],
    [2, 5, 8],
    [0, 4, 8],
    [2, 4, 6],
  ];
  for (let i = 0; i < lines.length; i++) {
    const [a, b, c] = lines[i];
    if (squares[a] && squares[a] === squares[b] && squares[a] === squares[c]) {
      return squares[a];
    }
  }
  return null;
}


function Square(props) {
    return (
	<button className="square" onClick={props.handleClick}>
        {props.value}
      </button>
    );
}

class Board extends React.Component {
  renderSquare(i) {
    return <Square value={this.props.cells[i]} handleClick={() => this.props.handleClick(i)} />;
  }

  render() {
    return (
      <div>
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
    constructor(props) {
	super(props);

	this.state = {
	    history: [{cells: Array(9).fill(null)}],
	    isNextX: true
	};
    }

    render() {
	let history = this.state.history;
	let current = history[history.length - 1]
	let winner = calculateWinner(current.cells);
	const status = winner ? ('Winner: ' + winner)
	      : 'Next player: ' + (this.props.isNextX ? 'X' : 'O');

	return (
	    <div className="game">
		<div className="game-board">
		    <Board cells={current.cells}
			   handleClick={(i) => this.handleClick(i)}
		    />
		</div>
		<div className="game-info">
		    <div>{status}</div>
		    <ol>{/* TODO */}</ol>
		</div>
	    </div>
	);
    }

    handleClick(index) {
	console.log("handle click called!");
	const history = this.state.history
	const cells   = history[history.length - 1].cells.slice();
	if (calculateWinner(cells) || cells[index])
	    return;

	cells[index] = this.state.isNextX ? 'X' : 'O';
	this.setState({history: history.concat([{cells: cells}]),
		       isNextX: !this.state.isNextX});
    }

}

// ========================================

const root = ReactDOM.createRoot(document.getElementById("root"));
root.render(<Game />);
