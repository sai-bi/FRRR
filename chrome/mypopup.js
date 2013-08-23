function click(color) {
  chrome.tabs.executeScript(null, 
        {code:"document.body.style.backgroundColor='" + color.target.id + "'"});
  window.close();
}



document.addEventListener('DOMContentLoaded', function () {
  var divs = document.querySelectorAll('div');
  for (var i = 0; i < divs.length; i++) {
    divs[i].addEventListener('click', click);
  }
});