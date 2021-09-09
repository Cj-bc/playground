import { explore_hello } from "../../declarations/explore_hello";

document.getElementById("clickMeBtn").addEventListener("click", async () => {
  const name = document.getElementById("name").value.toString();
  // Interact with explore_hello actor, calling the greet method
  const greeting = await explore_hello.greet(name);

  document.getElementById("greeting").innerText = greeting;
});
