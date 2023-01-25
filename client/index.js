const socket = new WebSocket("ws://localhost:8082")

socket.addEventListener("open", (event) =>
  setInterval(() =>
    socket.send("ping"), 60000))

socket.addEventListener("message", (event) => {
  if (event.data === "pong") {
    return
  }

  const msg = JSON.parse(event.data)

  switch (msg.notification) {
    case "feed-added":
      fetch("/articles", { headers: { Accept: "application/json" } })
        .then((res) =>
          res.json())
        .then((res) =>
          document.querySelector("main").innerHTML = res.html)
      break

    default:
      console.log(msg)
      break
  }
})
