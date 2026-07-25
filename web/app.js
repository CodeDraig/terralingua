// TerraLingua Phase 9 Interactive Canvas & Event Log Parser

let events = [];
let maxSteps = 0;
let currentStep = 0;
let isPlaying = false;
let playInterval = null;
let worldHistory = [];
let gridSize = 50;       // pulled from the run-started event; default 50
let visionRadius = 2;     // pulled from the run-started event; default 2
let phylogeny = {};       // name -> {creator, versions: [{v, ts, payload}]}

const canvas = typeof document !== 'undefined' ? document.getElementById('grid-canvas') : null;
const ctx = canvas ? canvas.getContext('2d') : null;
const btnPlay = typeof document !== 'undefined' ? document.getElementById('btn-play') : null;
const slider = typeof document !== 'undefined' ? document.getElementById('step-slider') : null;
const stepDisplay = typeof document !== 'undefined' ? document.getElementById('step-display') : null;
const fileInput = typeof document !== 'undefined' ? document.getElementById('log-file-input') : null;

if (fileInput) {
  fileInput.addEventListener('change', (e) => {
    const file = e.target.files[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = (evt) => {
      parseJSONLEvents(evt.target.result);
    };
    reader.readAsText(file);
  });
}

function parseJSONLEvents(text) {
  const lines = text.split('\n');
  events = [];
  let badLines = 0;
  for (const line of lines) {
    if (!line.trim()) continue;
    try {
      events.push(JSON.parse(line));
    } catch (err) {
      badLines++;
      console.warn('JSON parse error:', err);
    }
  }

  // Surface a status warning so the user knows the timeline may be incomplete.
  const status = document.getElementById('status-line');
  if (status) {
    if (badLines > 0) {
      status.textContent = `⚠ ${badLines} unparseable line(s) — log may be malformed`;
      status.style.color = 'var(--accent-pink)';
    } else {
      status.textContent = '';
    }
  }

  // Sort by ts so out-of-order events (hand-merged logs, reorders) are
  // still reconstructed correctly. The runner emits in order; this is
  // defensive.
  events.sort((a, b) => (a.ts || 0) - (b.ts || 0));

  maxSteps = events.reduce((max, e) => Math.max(max, e.ts || 0), 0);
  slider.max = maxSteps;
  currentStep = 0;
  // Pull grid-size / vision-radius from the first run-started event so
  // the canvas matches the actual config (defaults remain 50 / 2).
  for (const evt of events) {
    if (evt.type === 'run-started') {
      if (typeof evt['grid-size'] === 'number') gridSize = evt['grid-size'];
      if (typeof evt['vision-radius'] === 'number') visionRadius = evt['vision-radius'];
      break;
    }
  }
  buildPhylogeny();
  worldHistory = reconstructWorldHistoryFromEvents(events, maxSteps);
  updateUI();
}

function reconstructWorldHistoryFromEvents(eventList, maxStep) {
  const history = [];
  for (let s = 0; s <= maxStep; s++) {
    history.push({
      beings: {},
      food: [],
      artifactsByName: {}
    });
  }

  // Apply every timestamp's deltas before storing that timestamp's state.
  // This makes the slider represent the state after that simulation step,
  // including changes at the final logged step.
  let cumBeings = {};
  let cumArts = {};
  const ordered = [...eventList].sort((a, b) => (a.ts || 0) - (b.ts || 0));
  let eventIndex = 0;
  function apply(evt) {
    if (evt.type === 'agent-added') {
      cumBeings[evt.tag] = { name: evt.name, pos: evt.pos || { x: 0, y: 0 } };
    } else if (evt.type === 'agent-moved') {
      const b = cumBeings[evt.tag];
      if (b) cumBeings[evt.tag] = { ...b, pos: evt.pos || b.pos };
    } else if (evt.type === 'agent-died') {
      delete cumBeings[evt.tag];
    } else if (evt.type === 'artifact-added') {
      cumArts[evt.name] = {
        name: evt.name,
        payload: evt.payload,
        pos: evt.pos || { x: 0, y: 0 }
      };
    } else if (evt.type === 'artifact-removed') {
      delete cumArts[evt.name];
    } else if (evt.type === 'artifact-pickup' || evt.type === 'artifact-drop') {
      // Inventory movement: pos=null hides from grid; drop restores it.
      const a = cumArts[evt.artifact];
      if (a) {
        cumArts[evt.artifact] = {
          ...a,
          pos: (evt.type === 'artifact-drop') ? evt.pos : null
        };
      }
    }
  }
  for (let s = 0; s <= maxStep; s++) {
    while (eventIndex < ordered.length && (ordered[eventIndex].ts || 0) <= s) {
      apply(ordered[eventIndex]);
      eventIndex++;
    }
    history[s].beings = { ...cumBeings };
    history[s].artifactsByName = { ...cumArts };
  }

  // Materialize artifactsByName into the array the renderer reads.
  // Hide artifacts currently in an inventory (pos == null).
  for (const s of history) {
    s.artifacts = Object.values(s.artifactsByName).filter(a => a.pos);
  }
  return history;
}

function renderGrid() {
  if (!ctx) return;
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  const cellSize = canvas.width / gridSize;

  // Background grid
  ctx.strokeStyle = 'rgba(255, 255, 255, 0.05)';
  ctx.lineWidth = 1;
  for (let i = 0; i <= gridSize; i++) {
    ctx.beginPath();
    ctx.moveTo(i * cellSize, 0);
    ctx.lineTo(i * cellSize, canvas.height);
    ctx.stroke();

    ctx.beginPath();
    ctx.moveTo(0, i * cellSize);
    ctx.lineTo(canvas.width, i * cellSize);
    ctx.stroke();
  }

  const state = worldHistory[currentStep];
  if (!state) return;

  // Render Beings
  for (const tag in state.beings) {
    const b = state.beings[tag];
    if (!b) continue;
    const x = (b.pos.x || 0) * cellSize + cellSize / 2;
    const y = (b.pos.y || 0) * cellSize + cellSize / 2;

    ctx.fillStyle = '#38bdf8';
    ctx.beginPath();
    ctx.arc(x, y, cellSize * 0.4, 0, Math.PI * 2);
    ctx.fill();
  }

  // Render Artifacts
  for (const art of state.artifacts) {
    const x = (art.pos.x || 0) * cellSize + cellSize / 2;
    const y = (art.pos.y || 0) * cellSize + cellSize / 2;

    ctx.fillStyle = '#f472b6';
    ctx.fillRect(x - cellSize * 0.3, y - cellSize * 0.3, cellSize * 0.6, cellSize * 0.6);
  }
}

function updateUI() {
  if (stepDisplay) stepDisplay.textContent = `${currentStep} / ${maxSteps}`;
  if (slider) slider.value = currentStep;

  const state = worldHistory[currentStep];
  if (state) {
    const livingCount = Object.values(state.beings).filter(Boolean).length;
    const statLiving = document.getElementById('stat-living');
    if (statLiving) statLiving.textContent = livingCount;

    const statArts = document.getElementById('stat-artifacts');
    if (statArts) statArts.textContent = state.artifacts.length;

    // Food isn't carried in the event log; show a placeholder so the
    // dashboard doesn't pretend to know a value of 0.
    const statFood = document.getElementById('stat-food');
    if (statFood) statFood.textContent = '--';
  }

  renderGrid();
  renderPhylogeny();
}

// SPEC §6: artifact phylogeny = creator + version history per artifact.
// Reads the same event stream as reconstructWorldHistory, so no extra
// Racket support is required.
function buildPhylogeny() {
  phylogeny = {};
  for (const evt of events) {
    if (evt.type === 'artifact-added') {
      phylogeny[evt.name] = {
        name: evt.name,
        creator: evt.creator || evt['agent-type'] || '?',
        createdAt: evt.ts || 0,
        versions: [{ v: 1, ts: evt.ts || 0, payload: evt.payload || '' }]
      };
    } else if (evt.type === 'artifact-interaction' &&
               phylogeny[evt.artifact] &&
               typeof evt.payload === 'string') {
      const entry = phylogeny[evt.artifact];
      const nextV = entry.versions.length + 1;
      entry.versions.push({
        v: nextV,
        ts: evt.ts || 0,
        payload: evt.payload,
        action: evt.action || ''
      });
    }
  }
}

function renderPhylogeny() {
  const el = document.getElementById('phylogeny-container');
  if (!el) return;
  const names = Object.keys(phylogeny);
  if (names.length === 0) {
    el.innerHTML = '<p style="color: var(--text-muted);">Load events.jsonl to explore artifacts.</p>';
    return;
  }
  names.sort();
  const items = names.map(name => {
    const p = phylogeny[name];
    const versions = p.versions
      .map(v => `<div>v${v.v} @ step ${v.ts}${v.action ? ' (' + v.action + ')' : ''}: <code>${escapeHtml(truncate(v.payload, 80))}</code></div>`)
      .join('');
    return `<div class="artifact-item"><strong>${escapeHtml(name)}</strong> <span style="color: var(--text-muted);">by ${escapeHtml(p.creator)} @ step ${p.createdAt}</span>${versions}</div>`;
  });
  el.innerHTML = items.join('');
}

function escapeHtml(s) {
  return String(s).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
}

function truncate(s, n) {
  s = String(s);
  return s.length > n ? s.slice(0, n) + '…' : s;
}

if (slider) {
  slider.addEventListener('input', (e) => {
    currentStep = parseInt(e.target.value, 10);
    updateUI();
  });
}

if (btnPlay) {
  btnPlay.addEventListener('click', () => {
    isPlaying = !isPlaying;
    btnPlay.textContent = isPlaying ? 'Pause' : 'Play';
    if (isPlaying) {
      playInterval = setInterval(() => {
        if (currentStep < maxSteps) {
          currentStep++;
          updateUI();
        } else {
          isPlaying = false;
          btnPlay.textContent = 'Play';
          clearInterval(playInterval);
        }
      }, 200);
    } else {
      clearInterval(playInterval);
    }
  });
}

if (typeof module !== 'undefined') {
  module.exports = { reconstructWorldHistoryFromEvents };
}
