import React from 'react';
import {FormEvent, ChangeEvent, useState} from 'react';
import logo from './logo.svg';
import './App.css';

interface TodoItem {
    id: number,
    name: string,
    isDone: boolean,
}


function TodoItem(props: {entry: TodoItem, toggle: () => void}) {
    const entry = props.entry;
    return (<div className={`Todo-Item ${entry.isDone ? "done" : ""}`}>
		<button className="Todo-status-Toggle" onClick={props.toggle}> {entry.isDone ? "DONE" : "TODO"} </button>
		<div className="Todo-Name"> {entry.name} </div>
	    </div>)
}

function TodoList(props: {todoes: TodoItem[], toggle: (arg0: number) => void}) {
    const items = props.todoes.map((item) =>
	<li> <TodoItem entry={item} toggle={() => props.toggle(item.id)} /> </li>)

    return (<div className="Todo-List">
		<p> Todoes </p>
		<ol> {items} </ol>
	    </div>)
}

interface TodoFieldProps {
    adder: (arg0: string) => void
}

const TodoField = (props: TodoFieldProps) => {
    const [value, setValue] = useState("");

    const handleChange = (event: ChangeEvent<HTMLInputElement>) => {
	setValue(event.target.value);
    }

    const handleSubmit = (event: FormEvent) => {
	event.preventDefault();
	props.adder(value);
	setValue("");
    }

    return (<form onSubmit={handleSubmit}>
		<input type="text" value={value} onChange={handleChange} />
	    </form>)

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
		    <TodoList todoes={todoes} toggle={(id) => this.toggleStateAt(id)} />
		</ol>
		<div className="Todo-Field">
		    <TodoField adder={(s) => this.createNewTodo(s)} />
		</div>
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

    createNewTodo(name: string) {
	this.setState({todoes: this.state.todoes.concat({id: new Date().getTime(), name: name, isDone: false})});
    }
}

export default App;
