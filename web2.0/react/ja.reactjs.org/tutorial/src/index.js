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

function indexToCellCoordinate(index) {
    let x = index % 3 + 1;
    let y = Math.floor(index/3) + 1;
    return {x: x, y: y};
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
	      {Array.from([0,1,2]).map((n) => 
		  <div className="board-row">
		      {Array.from([0,1,2]).map((m) =>
			  this.renderSquare((n*3)+m))}
		  </div>
	      )}
	  </div>);
  }
}

class Game extends React.Component {
    constructor(props) {
	super(props);

	this.state = {
	    history: [{cells: Array(9).fill(null), newCell: null}
		      ],
	    stepNumber: 0,
	    isNextX: true,
	    historyIncremental: true,
	};
    }

    render() {
	let history = this.state.history;
	let current = history[this.state.stepNumber]
	let winner = calculateWinner(current.cells);
	const status = winner ? ('Winner: ' + winner)
	      : 'Next player: ' + (this.props.isNextX ? 'X' : 'O');
	const historyIncremental = this.state.historyIncremental;

	let historyList = history.map((histEntry, idx) => {
	    let c = indexToCellCoordinate(histEntry.newCell)
	    let descStr = (idx == 0) ? "Go to game start"
		: "added: ("+c.x+","+c.y+")"+"; Go to #"+idx+" move"

	    let desc = (idx === this.state.stepNumber) ? <b>{descStr}</b>
		: descStr
	    
	    return (<li key={idx}>
			<button onClick={() => this.jumpTo(idx)}>
			    {desc}
			</button>
		    </li>);
	});
	return (
	    <div className="game">
		<div className="game-board">
		    <Board cells={current.cells}
			   handleClick={(i) => this.handleClick(i)}
		    />
		</div>
		<div className="game-info">
		    <div>{status}</div>
		    <div>
			<button onClick={() => this.setState({historyIncremental: !historyIncremental})}>
			    {historyIncremental ? "show history decrementally" : "show history incrementally"}
			</button>
		    </div>
		    <ol>{historyIncremental ? historyList : historyList.reverse()}</ol>
		</div>
	    </div>
	);
    }

    handleClick(index) {
	console.log("handle click called!");
	const history = this.state.history.slice(0, this.state.stepNumber + 1);
	const cells   = history[history.length - 1].cells.slice();
	if (calculateWinner(cells) || cells[index])
	    return;

	cells[index] = this.state.isNextX ? 'X' : 'O';
	this.setState({history: history.concat([{cells: cells, newCell: index}]),
		       isNextX: !this.state.isNextX,
		       stepNumber: history.length,
		      });
    }

    jumpTo(index) {
	this.setState({stepNumber: index,
		       isNextX: (index % 2) === 0
		      })
    }

}

// ========================================

const root = ReactDOM.createRoot(document.getElementById("root"));
root.render(<Game />);
