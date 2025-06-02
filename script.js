var WORD = "";
const MAX_ATTEMPTS = 6;
let currentGuess = "";
let currentRow = 0;

const grid = document.getElementById("grid");
const keyboard = document.getElementById("keyboard");
const rows = [];

fetchWord();
setupTheme(); // Init theme toggle

// 1️⃣ Pre-build grid
for (let r = 0; r < MAX_ATTEMPTS; r++) {
  const row = document.createElement("div");
  row.className = "d-flex";
  const boxes = [];
  for (let c = 0; c < 5; c++) {
    const box = document.createElement("div");
    box.className = "box";
    row.appendChild(box);
    boxes.push(box);
  }
  grid.appendChild(row);
  rows.push(boxes);
}

// 2️⃣ Add Restart + Theme Toggle
const restartRow = document.createElement("div");
restartRow.className = "w-100 d-flex justify-content-between mb-2";

const restartBtn = document.createElement("button");
restartBtn.className = "btn btn-outline-danger";
restartBtn.innerHTML = '<i class="fas fa-redo-alt"></i>';
restartBtn.onclick = () => location.reload();

// Theme toggle button
const themeToggle = document.createElement("button");
themeToggle.id = "theme-toggle";
themeToggle.className = "btn btn-outline-secondary";
themeToggle.textContent = "🌙"; // default

themeToggle.onclick = () => {
  const isDark = document.body.classList.contains("dark");
  const newTheme = isDark ? "light" : "dark";
  setTheme(newTheme);
};

restartRow.appendChild(restartBtn);
restartRow.appendChild(themeToggle);
keyboard.appendChild(restartRow);

// 3️⃣ Keyboard layout
const layout = [
  ["Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P"],
  ["A", "S", "D", "F", "G", "H", "J", "K", "L"],
  ["Enter", "Z", "X", "C", "V", "B", "N", "M", "Back"]
];

const keyStates = {}; // Track keyboard color states

// 4️⃣ Render virtual keyboard
layout.forEach(row => {
  const rowDiv = document.createElement("div");
  rowDiv.className = "d-flex flex-wrap justify-content-center mb-2";

  row.forEach(key => {
    const button = document.createElement("button");
    button.textContent = key;
    button.className = "btn btn-secondary m-1";
    button.style.minWidth = key.length > 1 ? "60px" : "40px";
    button.onclick = () => handleKey(key);
    button.id = `key-${key}`;
    rowDiv.appendChild(button);
  });

  keyboard.appendChild(rowDiv);
});

// 5️⃣ Handle keyboard input
function handleKey(key) {
  if (currentRow >= MAX_ATTEMPTS) return;

  if (key === "Enter") {
    if (currentGuess.length === 5) {
      submitGuess();
    }
    return;
  }

  if (key === "Back") {
    if (currentGuess.length > 0) {
      currentGuess = currentGuess.slice(0, -1);
      updateRow();
    }
    return;
  }

  if (/^[A-Z]$/.test(key) && currentGuess.length < 5) {
    currentGuess += key;
    updateRow();
  }
}

// 6️⃣ Update row boxes
function updateRow() {
  const boxes = rows[currentRow];
  for (let i = 0; i < 5; i++) {
    boxes[i].textContent = currentGuess[i] || "";
  }
}

// 7️⃣ Submit guess
function submitGuess() {
  const guess = currentGuess.toUpperCase();
  const target = WORD.split("");
  const guessLetters = guess.split("");
  const colors = Array(5).fill("grey");

  // First pass: Green
  for (let i = 0; i < 5; i++) {
    if (guess[i] === WORD[i]) {
      colors[i] = "green";
      target[i] = null;
      guessLetters[i] = null;
    }
  }

  // Second pass: Yellow
  for (let i = 0; i < 5; i++) {
    if (guessLetters[i] && target.includes(guessLetters[i])) {
      colors[i] = "yellow";
      target[target.indexOf(guessLetters[i])] = null;
    }
  }

  // Apply color classes
  for (let i = 0; i < 5; i++) {
    const box = rows[currentRow][i];
    box.classList.add(colors[i]);
    updateKeyColor(guess[i], colors[i]);
  }

  let confirmed = false;

  if (guess === WORD) {
    confirmed = confirm(`🎉 Congrats! The word was: ${WORD}.\n\nPlay again?`);
    if (confirmed) location.reload();
  } else if (currentRow === MAX_ATTEMPTS - 1) {
    confirmed = confirm(`😢 Out of tries. Word was: ${WORD}.\n\nPlay again?`);
    if (confirmed) location.reload();
  }

  currentRow++;
  currentGuess = "";
}

// 8️⃣ Update key color (preserve best state)
function updateKeyColor(letter, color) {
  const btn = document.getElementById(`key-${letter}`);
  if (!btn) return;

  const current = keyStates[letter];
  if (current === "green") return;
  if (current === "yellow" && color === "grey") return;

  keyStates[letter] = color;
  btn.classList.remove("btn-secondary", "green", "yellow", "grey");
  btn.classList.add(color);
}

// 9️⃣ Real keyboard input
document.addEventListener("keydown", (e) => {
  const key = e.key.toUpperCase();
  if (key === "BACKSPACE") handleKey("Back");
  else if (key === "ENTER") handleKey("Enter");
  else if (/^[A-Z]$/.test(key) && key.length === 1) handleKey(key);
});

// 🔟 Fetch word from API
function fetchWord() {
  fetch('https://random-word-api.herokuapp.com/word?length=5')
    .then(response => response.json())
    .then(json => {
      takeWord(json);
    })
    .catch(error => console.error(error));
}

function takeWord(data) {
  WORD = data[0].toUpperCase();
  console.log("Word to guess:", WORD);
}

// 🌙 Theme Handling
function setupTheme() {
  const stored = localStorage.getItem("theme");
  const defaultTheme = stored || "light";
  setTheme(defaultTheme);
}

function setTheme(theme) {
  if (theme === "dark") {
    document.body.classList.add("dark");
    document.getElementById("theme-toggle").textContent = "☀️";
  } else {
    document.body.classList.remove("dark");
    document.getElementById("theme-toggle").textContent = "🌙";
  }
  localStorage.setItem("theme", theme);
}
