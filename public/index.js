var gameId = window.location.pathname;

// TODO what to do when gameId is nothing?

firebase.initializeApp(firebaseConfig);
firebase.analytics();

var db = firebase.firestore();

var app = Elm.Main.init({
	node: document.getElementById('elm')
});

db.collection('games').doc(gameId)
	.onSnapshot(snap => {
		app.ports.pullState.send(snap.data);
	});

app.ports.pushState.subscribe(data => {
	db.collection('games').doc(gameId).set(data);
	// TODO success & fail callback
});
