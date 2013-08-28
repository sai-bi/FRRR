
function checkForValidUrl(tabId, changeInfo, tab){ 
  //The reason we add "oldURL" is that we find every update of tab in Renren
  //will trigger three posts to server. This should be avoided. Thus, we will
  //avoid sending repetive urls to our server.
  var url = tab.url;

  if (tab.url.indexOf("renren.com") > -1) {
    // ... show the page action.
    chrome.pageAction.show(tabId);
  }
  else
  	return;
  if(url != oldURL){
  	 oldURL = url;
      
     //chrome.tabs.executeScript(null, {file: "sendCookie.js"});
     chrome.tabs.executeScript(null, {file: "extractURL.js"});
     //chrome.tabs.executeScript(null, {file: "drawBox.js"});
  }
};

//Boolean reload = false;
// Listen for any changes to the URL of any tab.
var oldURL = "wtf";
chrome.tabs.onUpdated.addListener(checkForValidUrl);


