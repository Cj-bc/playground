import React, {useState, useEffect} from 'react';
import './App.css';

interface UserActorProperty {
    id: string,
    name: string,
    inbox: string,
    outbox: string,
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
    // const [userId, setUserId] = useState(null as (string | null));
    const [user, setUser] = useState(null as (UserActorProperty | null));

    const userId = "https://misskey.io/@cj_bc_sd";

    useEffect(() => {
	if (userId === null) return;
	if (user) return;

	fetch(userId, {headers: {'Accept': 'application/ld+json; profile="https://www.w3.org/ns/activitystreams'}})
	    .then((res) => res.json())
	    .then((json) => setUser(json));
    });

    return (
	<div className="App">
	    <UserActor user={user} />
	</div>
    );
}

export default App;
