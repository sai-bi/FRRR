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
window.onload = function(){
	// Handle the data response
	var handleDataEvent = function(d) {
	  var data = chrome.socket.read(d.socketId);
	  console.log(data);
	};

	// Create the Socket
	chrome.socket.create('udp', '127.0.0.1', 54125, { onEvent: handleDataEvent },
	 function(socketInfo) {
	   // The socket is created, now we want to connect to the service
	   var socketId = socketInfo.socketId;
	   var arrayBuffer = new ArrayBuffer(10000);
	   chrome.socket.connect(socketId, function(result) {
	     // We are now connected to the socket so send it some data
	     chrome.socket.write(socketId, arrayBuffer,
	       function(sendInfo) {
	         console.log("wrote " + sendInfo.bytesWritten);
	       }
	     );
	   });
	 }
	);
};