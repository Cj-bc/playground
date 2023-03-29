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

chrome.action.onClicked.addListener(async (tab) => {
    chrome.windows.create({url: 'mirrored.html'});
});
