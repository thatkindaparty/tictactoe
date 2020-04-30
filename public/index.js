var gameId = window.location.pathname;

// TODO what to do when gameId is nothing?

firebase.initializeApp(firebaseConfig);
firebase.analytics();

var db = firebase.firestore();

var app = Elm.Main.init({
	node: document.getElementById('elm')
});

// TODO subscribe to collection('games').document(gameId)
// TODO update collection('games').document(gameId)
