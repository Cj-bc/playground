import React, {useState, useEffect, FormEvent, ChangeEvent} from 'react';
import './App.css';

interface UserActorProperty {
    id: string,
    name: string,
    inbox: string,
    outbox: string,
    following: string,
    followers: string,
    liked: string | null,
    streams: string[] | null,
    preferredUsername: string | null,
    summary: string,
}

const UserActor = (props: {user: UserActorProperty | null}) => {
    if (props.user == null) {
	return (<div> loading ... </div>);
    } else {
	return (<div className="UserProfile">
		    <div className="UserName">{props.user.name}</div>
		    <ul>
			<li> inbox: {props.user.inbox} </li>
			<li> outbox: {props.user.outbox} </li>
		    </ul>
		</div>
	);
    }
}

function App() {
    const [user, setUser] = useState(null as (UserActorProperty | null));
    const [userIdPartial, setUserIdPartial] = useState("");
    const [userId, setUserId] = useState(null as (string | null))

    useEffect(() => {
	if (userId === null) return;
	if (user) return;

	fetch(userId, {headers: {'Accept': 'application/ld+json; profile="https://www.w3.org/ns/activitystreams'}})
	    .then((res) => res.json())
	    .then((json) => setUser(json));
    });

    const onSubmit = (e: FormEvent) => {
	e.preventDefault();
	setUserId(userIdPartial);
    }

    const onChange = (e: ChangeEvent<HTMLInputElement>) => {
	setUserIdPartial(e.target.value);
    }

    return (
	<div className="App">
	    <UserActor user={user} />
	    <form onSubmit={onSubmit}>
		<input type="text" onChange={onChange} value={userIdPartial} />
	    </form>
	</div>
    );
}

export default App;
