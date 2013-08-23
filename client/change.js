function checkForValidUrl(tabId, changeInfo, tab) {
//if we could find "photo" in the url, we will process this page;
  var url = tab.url;

  //The repition problem for URL should be considered when receivng data.
  //++++++++

  if (tab.url.indexOf("photo") > -1 && tab.url.indexOf("renren") > -1) {
    // ... show the page action.
    chrome.pageAction.show(tabId);
  }
  else
  	return;
  if(url != oldURL){
     chrome.tabs.executeScript(null, {file: "extractURL.js"});
     oldURL = url;
  }
};

// Listen for any changes to the URL of any tab.
var oldURL = "wtf";
chrome.tabs.onUpdated.addListener(checkForValidUrl);
chrome.tabs.onCreated.addListener(checkForValidUrl);