var photoContainer = document.getElementById('photoContainer');

if(photoContainer == null)
	return;

var anchorA = photoContainer.childNodes[1]; 

var img = anchorA.childNodes[0];

var url = img.getAttribute('src'); //url stores the link to the image we want

//Following should I to sent url to 'Backgournd Page' or 'Server'?
//I am not sure.
//alert(url);
chrome.socket.create('udp', '127.0.0.1', 54123, {},
 function(socketInfo) {
   // The socket is created, now we want to connect to the service
   var socketId = socketInfo.socketId;
   chrome.socket.connect(socketId, function(result) {
     // We are now connected to the socket so send it some data
     
     //write url in array buffer
     var arrayBuffer = str2ab(url);
     
     chrome.socket.write(socketId, arrayBuffer,
       function(sendInfo) {
         console.log("wrote " + sendInfo.bytesWritten);
       }
     );
   });
 }
);


var str2ab = function(str){
  var buf = new ArrayBuffer(str.length*2); // 2 bytes for each char
  var bufView = new Uint16Array(buf);
  for (var i=0, strLen=str.length; i<strLen; i++) {
    bufView[i] = str.charCodeAt(i);
  }
  return buf;
};