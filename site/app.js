const STORAGE_KEY = "physics_graph_project_v1";

function setStatus(msg) {
  document.getElementById("status").textContent = msg;
}

function safeJsonParse(s) {
  try { return JSON.parse(s); } catch { return null; }
}

function loadProject() {
  const raw = localStorage.getItem(STORAGE_KEY);
  if (!raw) return null;
  return safeJsonParse(raw);
}

function saveProject(project) {
  localStorage.setItem(STORAGE_KEY, JSON.stringify(project, null, 2));
}

function downloadJson(filename, obj) {
  const blob = new Blob([JSON.stringify(obj, null, 2)], { type: "application/json" });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  a.remove();
  URL.revokeObjectURL(url);
}

async function fetchJson(path) {
  const res = await fetch(path, { cache: "no-store" });
  if (!res.ok) throw new Error(`failed to load: ${path}`);
  return await res.json();
}

function getOrCreateSymbolColors(project) {
  project.ui ??= {};
  project.ui.symbol_colors ??= {};
  return project.ui.symbol_colors;
}

function colorForSymbol(project, latexSym) {
  const map = getOrCreateSymbolColors(project);
  if (map[latexSym] == null) {
    // deterministic-ish palette index
    const seed = [...latexSym].reduce((a, c) => a + c.charCodeAt(0), 0);
    map[latexSym] = (seed % 12) + 1;
  }
  const idx = map[latexSym];

  // simple palette, dark-friendly
  const palette = [
    "#7aa2ff", "#2fe38c", "#ffd166", "#ff5c7a",
    "#a78bfa", "#22d3ee", "#fb7185", "#fbbf24",
    "#34d399", "#60a5fa", "#f472b6", "#c084fc"
  ];
  return palette[(idx - 1) % palette.length];
}

function extractSymbols(latex) {
  // MVP symbol extraction: grabs common single-letter symbols and common Greek commands.
  // This is intentionally simple. We'll improve it later.
  const greek = latex.match(/\\[a-zA-Z]+/g) ?? [];
  const letters = latex.match(/\b[a-zA-Z]\b/g) ?? [];
  const subs = latex.match(/[a-zA-Z](?=_{)/g) ?? [];
  const all = [...greek, ...letters, ...subs];

  // filter out common function commands
  const blacklist = new Set([
    "\\frac","\\cdot","\\times","\\left","\\right","\\sqrt","\\nabla",
    "\\sin","\\cos","\\tan","\\log","\\ln","\\exp","\\sum","\\int",
    "\\mathrm","\\mathbf","\\vec","\\hat","\\bar","\\dot","\\ddot"
  ]);
  const uniq = [];
  const seen = new Set();
  for (const s of all) {
    if (blacklist.has(s)) continue;
    if (!seen.has(s)) { seen.add(s); uniq.push(s); }
  }
  return uniq;
}

function buildUsageIndex(project) {
  const usage = {}; // latexSym -> [eqId...]
  for (const eq of project.eqs) {
    const syms = extractSymbols(eq.latex);
    eq._symbols = syms;
    for (const s of syms) {
      usage[s] ??= [];
      usage[s].push(eq.id);
    }
  }
  return usage;
}

function renderInspector(project, usage, symbol) {
  const el = document.getElementById("inspector");
  if (!symbol) {
    el.innerHTML = "click any symbol to see where it appears";
    return;
  }
  const eqIds = usage[symbol] ?? [];
  const titleById = Object.fromEntries(project.eqs.map(e => [e.id, e.title]));
  el.innerHTML = `
    <div><strong>symbol:</strong> <span style="color:${colorForSymbol(project, symbol)}">${escapeHtml(symbol)}</span></div>
    <div><strong>used in:</strong> ${eqIds.length} equation(s)</div>
    <ul>
      ${eqIds.map(id => `<li>${escapeHtml(titleById[id] ?? id)}</li>`).join("")}
    </ul>
    <div style="margin-top:8px;color:#9aa3b2;">
      later we’ll link symbols to full variable definitions (units, type, bounds)
    </div>
  `;
}

function escapeHtml(s) {
  return s.replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;");
}

function badgeClass(status) {
  if (status === "good") return "badge good";
  if (status === "bad") return "badge bad";
  return "badge warn";
}

function renderCards(project) {
  const cards = document.getElementById("cards");
  cards.innerHTML = "";

  const usage = buildUsageIndex(project);
  renderInspector(project, usage, null);

  for (const eq of project.eqs) {
    const card = document.createElement("div");
    card.className = "card";

    const tags = (eq.tags ?? []).join(", ");
    const status = eq.status ?? "unknown";

    card.innerHTML = `
      <div class="cardTop">
        <div>
          <div class="cardTitle">${escapeHtml(eq.title)}</div>
          <div class="cardTags">${escapeHtml(tags)}</div>
        </div>
        <div class="${badgeClass(status)}">${escapeHtml(status)}</div>
      </div>

      <div class="mathBox">
        <div class="mathDisplay" id="disp_${eq.id}"></div>
        <div class="mathEdit" id="edit_${eq.id}">
          <math-field id="mf_${eq.id}"></math-field>
        </div>
      </div>

      <div class="row" id="sym_${eq.id}"></div>

      <div class="row">
        <button class="small" id="btnEdit_${eq.id}">edit</button>
        <button class="small" id="btnCopy_${eq.id}">copy latex</button>
        <button class="small" id="btnWhy_${eq.id}">why?</button>
      </div>

      <div class="hint" id="why_${eq.id}" style="display:none;"></div>
    `;

    cards.appendChild(card);

    // Render KaTeX display
    const disp = document.getElementById(`disp_${eq.id}`);
    try {
      katex.render(eq.latex, disp, { throwOnError: false, displayMode: true });
    } catch (e) {
      disp.textContent = eq.latex;
    }

    // Symbols row
    const symRow = document.getElementById(`sym_${eq.id}`);
    const syms = eq._symbols ?? extractSymbols(eq.latex);
    for (const s of syms) {
      const pill = document.createElement("span");
      pill.className = "symbol";
      pill.textContent = s;
      pill.style.borderColor = colorForSymbol(project, s);
      pill.style.color = colorForSymbol(project, s);
      pill.addEventListener("click", () => {
        renderInspector(project, usage, s);
        highlightSymbol(project, s);
      });
      symRow.appendChild(pill);
    }

    // Edit toggle
    const btnEdit = document.getElementById(`btnEdit_${eq.id}`);
    const btnCopy = document.getElementById(`btnCopy_${eq.id}`);
    const btnWhy  = document.getElementById(`btnWhy_${eq.id}`);
    const whyBox  = document.getElementById(`why_${eq.id}`);
    const editWrap = document.getElementById(`edit_${eq.id}`);
    const mf = document.getElementById(`mf_${eq.id}`);

    btnEdit.addEventListener("click", () => {
      const isEditing = editWrap.style.display === "block";
      if (!isEditing) {
        editWrap.style.display = "block";
        disp.style.display = "none";
        mf.value = eq.latex;
        mf.focus();
      }
    });

    mf.addEventListener("blur", () => {
      eq.latex = mf.value || eq.latex;
      eq.status = "unknown";
      eq.diagnostics = [];
      saveProject(project);
      renderCards(project);
    });

    btnCopy.addEventListener("click", async () => {
      await navigator.clipboard.writeText(eq.latex);
      setStatus("copied LaTeX");
      setTimeout(() => setStatus("ready"), 800);
    });

    btnWhy.addEventListener("click", () => {
      whyBox.style.display = (whyBox.style.display === "none") ? "block" : "none";
      const diag = (eq.diagnostics && eq.diagnostics.length) ? eq.diagnostics.join("<br>") : "mvp: checks not enabled yet";
      whyBox.innerHTML = diag;
    });
  }
}

function highlightSymbol(project, symbol) {
  // MVP: just re-render cards so pills stand out later if you want.
  // Leaving as a hook for next step.
}

async function bootstrap() {
  setStatus("loading…");

  let project = loadProject();
  if (!project) {
    project = {
      project: { id: "proj_001", name: "physics graph", version: 1 },
      vars: [],
      eqs: [],
      assumptions: [],
      values: [],
      ui: {}
    };
    saveProject(project);
  }

  document.getElementById("btnExport").addEventListener("click", () => {
    downloadJson("physics-graph-export.json", project);
  });

  document.getElementById("btnReset").addEventListener("click", () => {
    localStorage.removeItem(STORAGE_KEY);
    location.reload();
  });

  document.getElementById("fileImport").addEventListener("change", async (ev) => {
    const file = ev.target.files?.[0];
    if (!file) return;
    const text = await file.text();
    const obj = safeJsonParse(text);
    if (!obj || !obj.eqs) {
      setStatus("import failed");
      return;
    }
    project = obj;
    saveProject(project);
    renderCards(project);
    setStatus("imported");
    setTimeout(() => setStatus("ready"), 800);
  });

  document.getElementById("loadThermo").addEventListener("click", async () => {
    const starter = await fetchJson("../models/thermo.json");
    project.vars = starter.vars;
    project.eqs = starter.eqs;
    project.assumptions = starter.assumptions ?? [];
    project.values = starter.values ?? [];
    project.ui = project.ui ?? {};
    saveProject(project);
    renderCards(project);
    setStatus("loaded thermo starter");
    setTimeout(() => setStatus("ready"), 800);
  });

  renderCards(project);
  setStatus("ready");
}

bootstrap().catch(err => {
  console.error(err);
  setStatus("error loading app");
});
