function checkForValidUrl(tabId, changeInfo, tab) {
//if we could find "photo" in the url, we will process this page;
  if (tab.url.indexOf("photo") > -1 && tab.url.indexOf("renren") > -1) {
    // ... show the page action.
    chrome.pageAction.show(tabId);
  }
  else
  	return;
  chrome.tabs.executeScript(null, {file: "extractURLBack.js"});
};

// Listen for any changes to the URL of any tab.
chrome.tabs.onUpdated.addListener(checkForValidUrl);