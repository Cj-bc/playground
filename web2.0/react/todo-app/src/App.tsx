import React from 'react';
import logo from './logo.svg';
import './App.css';

interface TodoItem {
    id: number,
    name: string,
    isDone: boolean,
}


function TodoItem(props: {entry: TodoItem, toggle: () => void}) {
    const entry = props.entry;
    return (<div className="Todo-Item">
		<button className="Todo-done" onClick={props.toggle}> {entry.isDone ? "DONE" : "TODO"} </button>
		<div className="Todo-Name"> {entry.name} </div>

	    </div>)
}

function TodoList(props: TodoItem[]) {

}

class App extends React.Component<{}, {todoes: TodoItem[]}> {
    constructor(props: any) {
	super(props);

	const todoes: TodoItem[] = [{id: 0, name: "Example", isDone: false },
		     {id: 1, name: "2nd", isDone: false }
		    ]
 
	this.state = {
	    todoes: todoes
	};
    }
    
    render() {
	const todoes = this.state.todoes;
	const items = todoes.map((item, idx) => {
	    return (<li>
			<TodoItem entry={item} toggle={() => this.toggleStateAt(item.id)} />
		    </li>)})
	
	return (
	    <div className="App">
		<ol>
		    {items}
		</ol>
	    </div>
	);
    }

    toggleStateAt(id: number) {
	const todoes = this.state.todoes;

	this.setState({todoes: todoes.map((item) =>
	    (item.id === id) ? {...item, isDone: !item.isDone} : item
	)
		      });
	
    }
}

export default App;
