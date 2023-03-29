let elem = document.getElementById("timestamp-string");
chrome.runtime.onMessage.addListener((msg, sender, responseFunc) => {
	if (msg.name == "updateTimestamp") {
	    elem.textContent = msg.value;
	}
});

window.addEventListener("load", (event) =>
    chrome.runtime.sendMessage(null, {name: "mirrorReady"})
);
