import "./styles/main.css";
import { Elm } from "./Main.elm";

var highestLevel = localStorage.getItem("highestLevel");
if ( highestLevel === null) {
  highestLevel = 0;
}

var app = Elm.Main.init({
    node: document.getElementById("main"), 
    flags: highestLevel
});

app.ports.sendMessage.subscribe(function(msg) {
  var jsMsg = msg.jsMsg;
  switch (jsMsg) {
    case "getHighestLevel":
      var highestLevel = localStorage.getItem("highestLevel");
      if (highestLevel === null) {
        localStorage.setItem("highestLevel", "1");
        app.ports.messageReceiver.send(1);
      } else {
        var highestLevelInt = parseInt(highestLevel, 10);
        app.ports.messageReceiver.send(highestLevelInt);
      }
      break;
    case "setHighestLevel":
        localStorage.setItem("highestLevel", msg.value);
    default:
      break;
  }
})