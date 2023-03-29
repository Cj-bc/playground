chrome.action.onClicked.addListener(async (tab) => {
    chrome.windows.create({url: 'mirrored.html'});
});
