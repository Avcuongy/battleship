// AI Setup Page
let playerData = null;

function init() {
  const gameMode = sessionStorage.getItem("battleship-game-mode");
  if (gameMode !== "ai") {
    window.location.href = "../home.html";
    return;
  }

  playerData = Storage.getPlayer();
  if (!playerData || !playerData.playerId) {
    window.location.href = "../home.html";
    return;
  }

  const nameEl = document.getElementById("player1Name");
  if (nameEl) nameEl.textContent = playerData.playerName;

  Board.render("gameBoard", true);
  Ships.init("gameBoard", "shipsSection", onFleetChange);

  document.getElementById("resetBtn")?.addEventListener("click", handleReset);
  document.getElementById("readyBtn")?.addEventListener("click", handleReady);
}

function onFleetChange(fleet) {
  const btn = document.getElementById("readyBtn");
  if (btn) btn.disabled = fleet.length !== 5;
}

function handleReset(e) {
  e.preventDefault();
  Ships.reset();
}

async function handleReady(e) {
  e.preventDefault();
  console.log("READY");

  const btn = document.getElementById("readyBtn");

  try {
    const fleet = Ships.getFleet();
    if (fleet.length !== 5) {
      alert("Not enough ships placed");
      return;
    }

    const v = Validation.validateFleet(fleet);
    if (!v.valid) {
      alert(v.errors.join("\n"));
      return;
    }

    btn.disabled = true;
    btn.textContent = "Ready";

    const res = await API.startAIGame(
      playerData.playerId,
      playerData.playerName,
      fleet
    );
    if (!res || res.asrStatus !== "success")
      throw new Error(res?.asrMessage || "API error");

    Storage.saveGameId(res.asrGameId);
    Storage.saveFleet(fleet);

    console.log("Navigate");
    setTimeout(() => {
      window.location.href = "./game.html";
    }, 100);
  } catch (err) {
    alert("Error: " + err.message);
    btn.disabled = false;
    btn.textContent = "Ready";
  }
}

document.addEventListener("DOMContentLoaded", init);
