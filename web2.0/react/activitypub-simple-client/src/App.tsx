import React, {useState, useEffect} from 'react';
import './App.css';

interface UserActorProps {
    id: string,
}

interface UserActorProperty {
    id: string,
    name: string,
    inbox: string,
    outbox: string,
}

const UserActor = (props: UserActorProps) => {
    const [user, setUser] = useState(null as (UserActorProperty | null));

    useEffect(() => {
	if (user) { return; }

	fetch(props.id,
	      {headers: new Headers({'Accept': 'application/ld+json; profile="https://www.w3.org/ns/activitystreams"',
				    }),
	       method: 'GET'
	      }
	     ).then((response) => {
		 console.log(response);
		 if (response.ok) {
		     response.json().then((json) => setUser(json));
		 } else {
		     console.log(`Couldn't success user calling: ${response.status}`);
		 }

	     });
    });

    if (!user) {
	return (<div> loading ... </div>);
    } else {
	return (<div>
		    <ul>
			<li> name: {user.name} </li>
			<li> inbox: {user.inbox} </li>
			<li> outbox: {user.outbox} </li>
		    </ul>
		</div>
	);
    }
}

function App() {
  return (
    <div className="App">
	<UserActor id="https://misskey.io/@cj_bc_sd" />
    </div>
  );
}

export default App;
