chrome.runtime.onInstalled.addListener(() => {
    chrome.action.disable();

    chrome.declarativeContent.onPageChanged.removeRules(undefined, () => {
	let enableActionRule = {
	    conditions: [new chrome.declarativeContent.PageStateMatcher({pageUrl: {hostSuffix: "youtube.com"}})],
	    actions: [new chrome.declarativeContent.ShowAction()]
	}
	chrome.declarativeContent.onPageChanged.addRules([enableActionRule]);
    });
});

// This function temporary returns dummy timestamp. It should reads from actual page.
readTimstamp = () => {
    return "0:0:0"
};

chrome.action.onClicked.addListener(async (playerTab) => {
    let newTab = await chrome.windows.create({url: 'mirrored.html'}).then((w) => w.tabs[0].id);

    chrome.runtime.onMessage.addListener((msg, sender, responseFunc) => {
	if (msg.name == "mirrorReady") {
	    chrome.scripting.executeScript({
		target: {tabId: playerTab.id},
		func: readTimstamp
	    }).then((results) =>
		chrome.tabs.sendMessage(newTab, {name: "updateTimestamp", value: results[0].result})
	    );
	}
    });
});
