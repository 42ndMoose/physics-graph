const STORAGE_KEY = "physics_graph_project_v1";

// --------------------
// Units: parsing + dimensional arithmetic
// --------------------
const UNIT_DEFS = {
  "1": {},
  "m":  { L: 1 },
  "s":  { T: 1 },
  "kg": { M: 1 },
  "K":  { Th: 1 },
  "A":  { I: 1 },
  "mol":{ N: 1 },
  "cd": { J: 1 },

  "N":  { M: 1, L: 1, T: -2 },
  "J":  { M: 1, L: 2, T: -2 },
  "W":  { M: 1, L: 2, T: -3 },
  "Pa": { M: 1, L: -1, T: -2 }
};

function dimAdd(a, b, k = 1) {
  const out = { ...(a || {}) };
  for (const key of Object.keys(b || {})) {
    out[key] = (out[key] || 0) + k * b[key];
    if (Math.abs(out[key]) < 1e-12) delete out[key];
  }
  return out;
}
function dimMul(a, b) { return dimAdd(a, b, 1); }
function dimDiv(a, b) { return dimAdd(a, b, -1); }
function dimPow(a, p) {
  if (!Number.isFinite(p)) return null;
  const out = {};
  for (const k of Object.keys(a || {})) out[k] = a[k] * p;
  return out;
}
function dimEq(a, b) {
  const ak = Object.keys(a || {}).sort();
  const bk = Object.keys(b || {}).sort();
  if (ak.length !== bk.length) return false;
  for (let i = 0; i < ak.length; i++) {
    if (ak[i] !== bk[i]) return false;
    if (Math.abs(a[ak[i]] - b[bk[i]]) > 1e-12) return false;
  }
  return true;
}
function dimToString(d) {
  if (!d || Object.keys(d).length === 0) return "1";
  return Object.entries(d)
    .sort((x, y) => x[0].localeCompare(y[0]))
    .map(([k, v]) => v === 1 ? k : `${k}^${v}`)
    .join(" ");
}

function normalizeUnitString(u) {
  return (u || "")
    .replaceAll("\\cdot", "*")
    .replaceAll("·", "*")
    .replaceAll(" ", "")
    .replaceAll("{", "(")
    .replaceAll("}", ")")
    .trim();
}

function parseUnitExpr(unitStr) {
  const s = normalizeUnitString(unitStr);
  if (!s) return null;

  let i = 0;
  function peek() { return s[i] || ""; }
  function eat(ch) { if (s[i] === ch) { i++; return true; } return false; }

  function parseNumber() {
    let start = i;
    while (/[0-9.]/.test(peek())) i++;
    if (start === i) return null;
    return Number(s.slice(start, i));
  }

  function parseIdent() {
    let start = i;
    while (/[a-zA-Z]/.test(peek())) i++;
    if (start === i) return null;
    return s.slice(start, i);
  }

  function parsePrimary() {
    if (eat("(")) {
      const d = parseExpr();
      if (!eat(")")) return null;
      return d;
    }
    const id = parseIdent();
    if (id) {
      const def = UNIT_DEFS[id];
      return def ? def : null;
    }
    if (peek() === "1") { i++; return {}; }
    return null;
  }

  function parseFactor() {
    let base = parsePrimary();
    if (!base) return null;
    if (eat("^")) {
      const p = parseNumber();
      if (!Number.isFinite(p)) return null;
      base = dimPow(base, p);
      if (!base) return null;
    }
    return base;
  }

  function parseTerm() {
    let d = parseFactor();
    if (!d) return null;
    while (true) {
      if (eat("*")) {
        const r = parseFactor(); if (!r) return null;
        d = dimMul(d, r);
        continue;
      }
      if (eat("/")) {
        const r = parseFactor(); if (!r) return null;
        d = dimDiv(d, r);
        continue;
      }
      break;
    }
    return d;
  }

  function parseExpr() { return parseTerm(); }

  const out = parseExpr();
  if (!out) return null;
  if (i !== s.length) return null;
  return out;
}

function buildVarDimMap(project) {
  const map = {};
  for (const v of (project.vars || [])) {
    const d = parseUnitExpr(v.units);
    if (d) map[v.latex] = d;
  }
  return map;
}

// --------------------
// LaTeX expression -> unit dims (MVP)
// --------------------
function latexHasUnsupportedOps(lx) {
  const bad = ["\\partial","\\nabla","\\int","\\sum","\\sin","\\cos","\\tan","\\log","\\ln","\\exp"];
  return bad.some(x => (lx || "").includes(x));
}

function tokenizeLatex(lx) {
  lx = (lx || "")
    .replaceAll("\\left", "")
    .replaceAll("\\right", "")
    .replaceAll("\\cdot", "*")
    .replaceAll("\\times", "*");

  const tokens = [];
  let i = 0;

  while (i < lx.length) {
    const c = lx[i];

    if (c === " " || c === "\n" || c === "\t") { i++; continue; }

    if (c === "\\") {
      let j = i + 1;
      while (j < lx.length && /[a-zA-Z]/.test(lx[j])) j++;
      tokens.push(lx.slice(i, j));
      i = j;
      continue;
    }

    if (/[0-9.]/.test(c)) {
      let j = i;
      while (j < lx.length && /[0-9.]/.test(lx[j])) j++;
      tokens.push(lx.slice(i, j));
      i = j;
      continue;
    }

    if (/[a-zA-Z]/.test(c)) {
      tokens.push(c);
      i++;
      continue;
    }

    if ("=+-*/^(){}".includes(c)) {
      tokens.push(c);
      i++;
      continue;
    }

    if (c === "_") {
      tokens.push("_");
      i++;
      continue;
    }

    i++;
  }
  return tokens;
}

function readBraceGroup(tokens, idx) {
  if (tokens[idx] !== "{") return { expr: null, next: idx };
  let depth = 0;
  let out = [];
  let i = idx;
  while (i < tokens.length) {
    const t = tokens[i];
    if (t === "{") { depth++; if (depth > 1) out.push(t); i++; continue; }
    if (t === "}") { depth--; if (depth === 0) { i++; break; } out.push(t); i++; continue; }
    out.push(t);
    i++;
  }
  return { expr: out, next: i };
}

function unitsOfLatexExpr(latex, varDimMap) {
  if (!latex) return { dim: null, ok: false, why: "empty" };
  if (latexHasUnsupportedOps(latex)) return { dim: null, ok: false, why: "unsupported operator in MVP" };

  const tokens = tokenizeLatex(latex);
  let i = 0;

  function peek() { return tokens[i] || null; }
  function eat(x) { if (tokens[i] === x) { i++; return true; } return false; }

  function skipSubscriptIfAny() {
    if (eat("_")) {
      if (peek() === "{") {
        const g = readBraceGroup(tokens, i);
        i = g.next;
      } else {
        i++;
      }
    }
  }

  function unitsFromTokenList(tokList) {
    const subLatex = tokList.join("");
    const res = unitsOfLatexExpr(subLatex, varDimMap);
    return res.ok ? res.dim : null;
  }

  function parsePrimary() {
    const t = peek();
    if (!t) return null;

    if (eat("(")) {
      const d = parseExpr();
      if (!eat(")")) return null;
      return d;
    }

    if (t === "\\frac") {
      i++;
      if (peek() !== "{") return null;
      const a = readBraceGroup(tokens, i); i = a.next;
      if (peek() !== "{") return null;
      const b = readBraceGroup(tokens, i); i = b.next;
      const da = unitsFromTokenList(a.expr);
      const db = unitsFromTokenList(b.expr);
      if (!da || !db) return null;
      return dimDiv(da, db);
    }

    if (/^\\[a-zA-Z]+$/.test(t) || /^[a-zA-Z]$/.test(t)) {
      i++;
      skipSubscriptIfAny();
      const d = varDimMap[t];
      return d ? d : null;
    }

    if (/^[0-9.]+$/.test(t)) {
      i++;
      return {};
    }

    if (eat("{")) {
      const inner = [];
      let depth = 1;
      while (i < tokens.length && depth > 0) {
        if (tokens[i] === "{") depth++;
        else if (tokens[i] === "}") depth--;
        if (depth > 0) inner.push(tokens[i]);
        i++;
      }
      return unitsFromTokenList(inner);
    }

    return null;
  }

  function parseFactor() {
    let base = parsePrimary();
    if (!base) return null;

    if (eat("^")) {
      const t = peek();
      if (!t) return null;

      if (/^[0-9.]+$/.test(t)) {
        i++;
        const p = Number(t);
        base = dimPow(base, p);
        if (!base) return null;
      } else if (t === "{") {
        const g = readBraceGroup(tokens, i);
        i = g.next;
        const raw = g.expr.join("");
        const p = Number(raw);
        if (!Number.isFinite(p)) return null;
        base = dimPow(base, p);
        if (!base) return null;
      } else {
        return null;
      }
    }
    return base;
  }

  function isFactorStart(t) {
    if (!t) return false;
    if (t === "(" || t === "{") return true;
    if (t === "\\frac") return true;
    if (/^\\[a-zA-Z]+$/.test(t)) return true;
    if (/^[a-zA-Z]$/.test(t)) return true;
    if (/^[0-9.]+$/.test(t)) return true;
    return false;
  }

  function parseTerm() {
    let d = parseFactor();
    if (!d) return null;

    while (true) {
      const t = peek();
      if (t === "*") { i++; const r = parseFactor(); if (!r) return null; d = dimMul(d, r); continue; }
      if (t === "/") { i++; const r = parseFactor(); if (!r) return null; d = dimDiv(d, r); continue; }

      if (isFactorStart(t)) {
        const r = parseFactor();
        if (!r) return null;
        d = dimMul(d, r);
        continue;
      }
      break;
    }
    return d;
  }

  function parseExpr() {
    let d = parseTerm();
    if (!d) return null;

    while (true) {
      const t = peek();
      if (t === "+" || t === "-") {
        i++;
        const r = parseTerm();
        if (!r) return null;
        if (!dimEq(d, r)) return null;
        continue;
      }
      break;
    }
    return d;
  }

  const dim = parseExpr();
  if (!dim) return { dim: null, ok: false, why: "could not infer units" };
  if (i !== tokens.length) return { dim: null, ok: false, why: "unsupported structure" };
  return { dim, ok: true, why: "" };
}

function unitCheckEquation(eqLatex, varDimMap) {
  const parts = (eqLatex || "").split("=");
  if (parts.length !== 2) return { status: "unknown", diag: "unit check: expected one '=' " };

  const lhs = parts[0].trim();
  const rhs = parts[1].trim();

  const ul = unitsOfLatexExpr(lhs, varDimMap);
  const ur = unitsOfLatexExpr(rhs, varDimMap);

  if (!ul.ok || !ur.ok) {
    return { status: "unknown", diag: `unit check: ${!ul.ok ? ul.why : ur.why}` };
  }
  if (dimEq(ul.dim, ur.dim)) {
    return { status: "good", diag: `units ok: ${dimToString(ul.dim)}` };
  }
  return { status: "bad", diag: `unit mismatch: LHS ${dimToString(ul.dim)} vs RHS ${dimToString(ur.dim)}` };
}

// --------------------
// Storage + helpers
// --------------------
function setStatus(msg) {
  const el = document.getElementById("status");
  if (el) el.textContent = msg;
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
    const seed = [...latexSym].reduce((a, c) => a + c.charCodeAt(0), 0);
    map[latexSym] = (seed % 12) + 1;
  }
  const idx = map[latexSym];
  const palette = [
    "#7aa2ff", "#2fe38c", "#ffd166", "#ff5c7a",
    "#a78bfa", "#22d3ee", "#fb7185", "#fbbf24",
    "#34d399", "#60a5fa", "#f472b6", "#c084fc"
  ];
  return palette[(idx - 1) % palette.length];
}

function extractSymbols(latex) {
  const greek = (latex || "").match(/\\[a-zA-Z]+/g) ?? [];
  const letters = (latex || "").match(/\b[a-zA-Z]\b/g) ?? [];
  const subs = (latex || "").match(/[a-zA-Z](?=_{)/g) ?? [];
  const all = [...greek, ...letters, ...subs];

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
  const usage = {};
  for (const eq of (project.eqs || [])) {
    const syms = extractSymbols(eq.latex || "");
    eq._symbols = syms;
    for (const s of syms) {
      usage[s] ??= [];
      usage[s].push(eq.id);
    }
  }
  return usage;
}

function escapeHtml(s) {
  return (s || "")
    .replaceAll("&","&amp;")
    .replaceAll("<","&lt;")
    .replaceAll(">","&gt;");
}

function badgeClass(status) {
  if (status === "good") return "badge good";
  if (status === "bad") return "badge bad";
  return "badge warn";
}

// --------------------
// Explorer canvas (zoom + pan)
// --------------------
const appState = {
  explorerFilterEqIds: null,
  explorerSelectedSymbol: null
};

function buildGraphFromProject(project) {
  const eqs = project.eqs || [];
  const vars = project.vars || [];
  const symbols = vars.length ? vars.map(v => v.latex) : extractSymbols(eqs.map(e => e.latex).join(" "));

  const nodes = [];
  const edges = [];

  nodes.push({
    id: "core",
    type: "core",
    label: project.project?.name || "physics core",
    level: 0,
    x: 0,
    y: 0,
    summary: "Root identity that all branches inherit from.",
    eqIds: []
  });

  const symbolNodes = symbols.map((sym, idx) => {
    const angle = (idx / Math.max(1, symbols.length)) * Math.PI * 2;
    return {
      id: `sym_${sym}`,
      type: "symbol",
      symbol: sym,
      label: sym,
      level: 1,
      x: Math.cos(angle) * 260,
      y: Math.sin(angle) * 200,
      summary: "Choose which equation defines this symbol.",
      eqIds: []
    };
  });

  nodes.push(...symbolNodes);
  for (const symNode of symbolNodes) {
    edges.push(["core", symNode.id]);
  }

  const equationNodes = eqs.map((eq, idx) => {
    const angle = (idx / Math.max(1, eqs.length)) * Math.PI * 2;
    return {
      id: `eq_${eq.id}`,
      type: "equation",
      eqId: eq.id,
      label: eq.title || eq.id,
      level: 2,
      x: Math.cos(angle) * 420,
      y: Math.sin(angle) * 320,
      summary: eq.description || "",
      latex: eq.latex || "",
      theory: eq.theory || "",
      doi: eq.doi || "",
      eqIds: [eq.id]
    };
  });
  nodes.push(...equationNodes);

  const symbolIndex = {};
  for (const symNode of symbolNodes) {
    symbolIndex[symNode.symbol] = symNode;
  }

  for (const eq of eqs) {
    const usedSyms = extractSymbols(eq.latex || "");
    for (const sym of usedSyms) {
      const symNode = symbolIndex[sym];
      if (!symNode) continue;
      symNode.eqIds.push(eq.id);
      edges.push([symNode.id, `eq_${eq.id}`]);
    }
  }

  return { nodes, edges };
}

function setupExplorer(project) {
  let canvas = document.getElementById("graphCanvas");
  if (!canvas) return;
  if (canvas.dataset.bound === "true") {
    const clone = canvas.cloneNode(true);
    canvas.parentNode.replaceChild(clone, canvas);
    canvas = clone;
  }
  canvas.dataset.bound = "true";
  const ctx = canvas.getContext("2d");
  const depthLabel = document.getElementById("depthLabel");
  const focusCard = document.getElementById("focusCard");
  const graph = buildGraphFromProject(project);

  const state = {
    scale: 0.95,
    offset: { x: canvas.width / 2, y: canvas.height / 2 },
    dragging: false,
    moved: false,
    last: { x: 0, y: 0 },
    hoverId: null,
    selectedId: "core",
    sized: false
  };

  function getDepth() {
    if (state.scale < 1.05) return 1;
    if (state.scale < 1.55) return 2;
    return 3;
  }

  function visibleNodes() {
    const depth = getDepth() - 1;
    return graph.nodes.filter((n) => n.level <= depth);
  }

  function screenPos(node) {
    return {
      x: node.x * state.scale + state.offset.x,
      y: node.y * state.scale + state.offset.y
    };
  }

  function nodeAtPoint(x, y) {
    const nodes = visibleNodes();
    for (const node of nodes) {
      const p = screenPos(node);
      const r = node.level === 0 ? 26 : node.level === 1 ? 20 : 16;
      const dx = x - p.x;
      const dy = y - p.y;
      if (dx * dx + dy * dy <= (r + 6) * (r + 6)) return node;
    }
    return null;
  }

  function renderFocus(node) {
    if (!focusCard) return;
    if (!node) {
      focusCard.textContent = "click a node to explore its linked equations.";
      return;
    }
    const eqsForSymbol = node.type === "symbol" ? node.eqIds : [];
    const selectedEqBySymbol = project.ui?.selectedEqBySymbol || {};
    const selectedEqId = node.type === "symbol" ? selectedEqBySymbol[node.symbol] : null;
    const eqChoices = eqsForSymbol.map((eqId) => {
      const eq = (project.eqs || []).find(e => e.id === eqId);
      if (!eq) return "";
      const isActive = selectedEqId === eqId;
      const theory = eq.theory ? `<div class="detailChoiceMeta">${escapeHtml(eq.theory)}</div>` : "";
      const doi = eq.doi ? `<div class="detailChoiceMeta"><a href="${eq.doi}" target="_blank" rel="noreferrer">${escapeHtml(eq.doi)}</a></div>` : "";
      return `
        <div class="detailChoice ${isActive ? "detailChoiceActive" : ""}" data-eqid="${eqId}">
          <div class="detailChoiceTitle">${escapeHtml(eq.title || eq.id)}</div>
          <div class="detailChoiceEq">${escapeHtml(eq.latex || "")}</div>
          ${theory}
          ${doi}
        </div>
      `;
    }).join("");

    focusCard.innerHTML = `
      <div class="detailBadge">level ${node.level + 1}</div>
      <strong>${escapeHtml(node.label)}</strong>
      <span>${escapeHtml(node.summary || "No summary yet.")}</span>
      ${node.type === "equation" ? `<div class="detailChoiceEq">${escapeHtml(node.latex || "")}</div>` : ""}
      ${eqChoices ? `<div>${eqChoices}</div>` : ""}
    `;

    if (node.type === "symbol") {
      focusCard.querySelectorAll("[data-eqid]").forEach((el) => {
        el.addEventListener("click", () => {
          const eqId = el.getAttribute("data-eqid");
          project.ui ??= {};
          project.ui.selectedEqBySymbol ??= {};
          project.ui.selectedEqBySymbol[node.symbol] = eqId;
          saveProject(project);
          renderAll(project);
          renderFocus(node);
        });
      });
    }
  }

  function draw() {
    const depth = getDepth();
    const nodes = visibleNodes();

    ctx.clearRect(0, 0, canvas.width, canvas.height);

    if (nodes.length === 1 && nodes[0].id === "core") {
      ctx.fillStyle = "rgba(154,163,178,0.8)";
      ctx.font = "600 14px system-ui";
      ctx.textAlign = "center";
      ctx.textBaseline = "middle";
      ctx.fillText("Load a model to populate the map", canvas.width / 2, canvas.height / 2 - 18);
      ctx.font = "12px system-ui";
      ctx.fillText("Try the thermo or mechanics starter on the left", canvas.width / 2, canvas.height / 2 + 6);
    }

    ctx.lineWidth = 1;
    ctx.strokeStyle = "rgba(122,162,255,0.3)";
    for (const [a, b] of graph.edges) {
      const na = graph.nodes.find(n => n.id === a);
      const nb = graph.nodes.find(n => n.id === b);
      if (!na || !nb) continue;
      if (!nodes.includes(na) || !nodes.includes(nb)) continue;
      const pa = screenPos(na);
      const pb = screenPos(nb);
      ctx.beginPath();
      ctx.moveTo(pa.x, pa.y);
      ctx.lineTo(pb.x, pb.y);
      ctx.stroke();
    }

    for (const node of nodes) {
      const p = screenPos(node);
      const r = node.level === 0 ? 26 : node.level === 1 ? 20 : 16;
      const isSelected = node.id === state.selectedId;
      const isHover = node.id === state.hoverId;
      const fill = node.level === 0 ? "#7aa2ff" : node.level === 1 ? "#2fe38c" : "#ffd166";

      ctx.beginPath();
      ctx.arc(p.x, p.y, r + (isHover ? 3 : 0), 0, Math.PI * 2);
      ctx.fillStyle = "rgba(15,19,26,0.9)";
      ctx.fill();
      ctx.lineWidth = isSelected ? 3 : 1.5;
      ctx.strokeStyle = isSelected ? "#ffffff" : fill;
      ctx.stroke();

      ctx.beginPath();
      ctx.arc(p.x, p.y, r - 4, 0, Math.PI * 2);
      ctx.fillStyle = fill;
      ctx.fill();

      ctx.fillStyle = "#0b0d10";
      ctx.font = "600 12px system-ui";
      ctx.textAlign = "center";
      ctx.textBaseline = "middle";
      ctx.fillText(node.label, p.x, p.y);
    }

    if (depthLabel) depthLabel.textContent = `depth ${depth} / 3`;
  }

  function resize() {
    const rect = canvas.getBoundingClientRect();
    canvas.width = Math.max(320, Math.floor(rect.width));
    canvas.height = Math.max(260, Math.floor(rect.height));
    if (!state.sized) {
      state.offset.x = rect.width / 2;
      state.offset.y = rect.height / 2;
      state.sized = true;
    }
    draw();
  }

  canvas.addEventListener("mousedown", (ev) => {
    state.dragging = true;
    state.moved = false;
    state.last = { x: ev.offsetX, y: ev.offsetY };
  });
  canvas.addEventListener("mousemove", (ev) => {
    if (state.dragging) {
      const dx = ev.offsetX - state.last.x;
      const dy = ev.offsetY - state.last.y;
      if (Math.abs(dx) + Math.abs(dy) > 2) state.moved = true;
      state.offset.x += dx;
      state.offset.y += dy;
      state.last = { x: ev.offsetX, y: ev.offsetY };
      draw();
      return;
    }
    const node = nodeAtPoint(ev.offsetX, ev.offsetY);
    state.hoverId = node ? node.id : null;
    canvas.style.cursor = node ? "pointer" : "grab";
    draw();
  });
  canvas.addEventListener("mouseup", (ev) => {
    state.dragging = false;
    if (state.moved) return;
    const node = nodeAtPoint(ev.offsetX, ev.offsetY);
    if (node) {
      state.selectedId = node.id;
      renderFocus(node);
      if (node.type === "symbol") {
        appState.explorerFilterEqIds = node.eqIds;
        appState.explorerSelectedSymbol = node.symbol;
      } else if (node.type === "equation") {
        appState.explorerFilterEqIds = node.eqIds;
        appState.explorerSelectedSymbol = null;
      } else {
        appState.explorerFilterEqIds = null;
        appState.explorerSelectedSymbol = null;
      }
      renderAll(project);
      draw();
    }
  });
  canvas.addEventListener("mouseleave", () => {
    state.dragging = false;
    state.hoverId = null;
    canvas.style.cursor = "grab";
    draw();
  });
  canvas.addEventListener("wheel", (ev) => {
    ev.preventDefault();
    const scaleBy = ev.deltaY < 0 ? 1.08 : 0.92;
    const nextScale = Math.min(2.4, Math.max(0.7, state.scale * scaleBy));
    const rect = canvas.getBoundingClientRect();
    const mx = ev.clientX - rect.left;
    const my = ev.clientY - rect.top;
    const wx = (mx - state.offset.x) / state.scale;
    const wy = (my - state.offset.y) / state.scale;
    state.scale = nextScale;
    state.offset.x = mx - wx * state.scale;
    state.offset.y = my - wy * state.scale;
    draw();
  }, { passive: false });

  window.addEventListener("resize", resize);
  renderFocus(graph.nodes.find(n => n.id === state.selectedId));
  resize();
}

// --------------------
// Rendering
// --------------------
function renderInspector(project, usage, symbol) {
  const el = document.getElementById("inspector");
  if (!el) return;

  if (!symbol) {
    el.innerHTML = "click any symbol to see where it appears";
    return;
  }

  const eqIds = usage[symbol] ?? [];
  const titleById = Object.fromEntries((project.eqs || []).map(e => [e.id, e.title]));

  el.innerHTML = `
    <div><strong>symbol:</strong> <span style="color:${colorForSymbol(project, symbol)}">${escapeHtml(symbol)}</span></div>
    <div><strong>used in:</strong> ${eqIds.length} equation(s)</div>
    <ul>
      ${eqIds.map(id => `<li>${escapeHtml(titleById[id] ?? id)}</li>`).join("")}
    </ul>
  `;
}

function hideKeyboardNow() {
  try {
    if (window.mathVirtualKeyboard) window.mathVirtualKeyboard.hide();
  } catch {}
}

function renderVarsTable(project) {
  const el = document.getElementById("varsTable");
  if (!el) return;

  const usage = buildUsageIndex(project);

  const header = `
    <div class="varsRow varsHead">
      <div>symbol</div>
      <div>units</div>
      <div>used</div>
    </div>
  `;

  const rows = (project.vars || []).map(v => {
    const usedIn = usage[v.latex] || [];
    const dot = colorForSymbol(project, v.latex);
    return `
      <div class="varsRow">
        <div class="varsSym">
          <span class="varsDot" style="background:${dot}"></span>
          <span style="color:${dot};font-weight:800;">${escapeHtml(v.latex)}</span>
          <span style="color:#9aa3b2;">${escapeHtml(v.name || "")}</span>
        </div>
        <div class="varsUnits">${escapeHtml(v.units || "")}</div>
        <div class="varsUsed">${usedIn.length ? usedIn.length + " eq(s)" : "unused"}</div>
      </div>
    `;
  }).join("");

  el.innerHTML = header + rows;
}

function renderCards(project) {
  const cards = document.getElementById("cards");
  if (!cards) return;
  cards.innerHTML = "";

  const usage = buildUsageIndex(project);
  const varDimMap = buildVarDimMap(project);
  const selectedEqBySymbol = project.ui?.selectedEqBySymbol || {};
  const selectedEqIds = new Set(Object.values(selectedEqBySymbol || {}));

  for (const eq of (project.eqs || [])) {
    const res = unitCheckEquation(eq.latex || "", varDimMap);
    eq._status = res.status;
    eq._diag = res.diag;
  }

  renderInspector(project, usage, null);

  for (const eq of (project.eqs || [])) {
    if (appState.explorerFilterEqIds && !appState.explorerFilterEqIds.includes(eq.id)) {
      continue;
    }
    const card = document.createElement("div");
    card.className = "card";

    const tags = (eq.tags ?? []).join(", ");
    const status = eq._status ?? "unknown";
    const isSelected = selectedEqIds.has(eq.id);

    card.innerHTML = `
      <div class="cardTop">
        <div>
          <div class="cardTitle">${escapeHtml(eq.title)}</div>
          <div class="cardTags">${escapeHtml(tags)}</div>
        </div>
        <div class="row" style="justify-content:flex-end;">
          ${isSelected ? `<div class="badge good">selected</div>` : ""}
          <div class="${badgeClass(status)}">${escapeHtml(status)}</div>
        </div>
      </div>

      <div class="mathBox">
        <div class="mathDisplay" id="disp_${eq.id}"></div>
        <div class="mathEdit" id="edit_${eq.id}" style="display:none;">
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

    const disp = document.getElementById(`disp_${eq.id}`);
    try {
      katex.render(eq.latex, disp, { throwOnError: false, displayMode: true });
    } catch {
      disp.textContent = eq.latex;
    }

    const symRow = document.getElementById(`sym_${eq.id}`);
    const syms = eq._symbols ?? extractSymbols(eq.latex || "");
    for (const s of syms) {
      const pill = document.createElement("span");
      pill.className = "symbol";
      pill.textContent = s;
      pill.style.borderColor = colorForSymbol(project, s);
      pill.style.color = colorForSymbol(project, s);
      pill.addEventListener("click", () => {
        renderInspector(project, usage, s);
      });
      symRow.appendChild(pill);
    }

    const btnEdit = document.getElementById(`btnEdit_${eq.id}`);
    const btnCopy = document.getElementById(`btnCopy_${eq.id}`);
    const btnWhy  = document.getElementById(`btnWhy_${eq.id}`);
    const whyBox  = document.getElementById(`why_${eq.id}`);
    const editWrap = document.getElementById(`edit_${eq.id}`);
    const mf = document.getElementById(`mf_${eq.id}`);

    btnEdit.addEventListener("click", () => {
      hideKeyboardNow();
      editWrap.style.display = "block";
      disp.style.display = "none";
      mf.value = eq.latex || "";
      mf.focus();
    });

    mf.addEventListener("blur", () => {
      hideKeyboardNow();
      eq.latex = mf.value || eq.latex;
      saveProject(project);
      renderAll(project);
    });

    btnCopy.addEventListener("click", async () => {
      await navigator.clipboard.writeText(eq.latex || "");
      setStatus("copied LaTeX");
      setTimeout(() => setStatus("ready"), 800);
    });

    btnWhy.addEventListener("click", () => {
      const open = whyBox.style.display !== "none";
      whyBox.style.display = open ? "none" : "block";
      whyBox.innerHTML = escapeHtml(eq._diag || "no diagnostics");
    });
  }
}

function renderAll(project) {
  renderCards(project);
  renderVarsTable(project);
}

// --------------------
// Bootstrap
// --------------------
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

  const btnExport = document.getElementById("btnExport");
  if (btnExport) {
    btnExport.addEventListener("click", () => {
      downloadJson("physics-graph-export.json", project);
    });
  }

  const btnReset = document.getElementById("btnReset");
  if (btnReset) {
    btnReset.addEventListener("click", () => {
      localStorage.removeItem(STORAGE_KEY);
      location.reload();
    });
  }

  const fileImport = document.getElementById("fileImport");
  if (fileImport) {
    fileImport.addEventListener("change", async (ev) => {
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
      renderAll(project);
      setupExplorer(project);
      setStatus("imported");
      setTimeout(() => setStatus("ready"), 800);
    });
  }

  const loadThermo = document.getElementById("loadThermo");
  if (loadThermo) {
    loadThermo.addEventListener("click", async () => {
      try {
        const starter = await fetchJson("./models/thermo.json");
        project.vars = starter.vars || [];
        project.eqs = starter.eqs || [];
        project.assumptions = starter.assumptions ?? [];
        project.values = starter.values ?? [];
        saveProject(project);
        renderAll(project);
        setupExplorer(project);
        setStatus("loaded thermo starter");
        setTimeout(() => setStatus("ready"), 900);
      } catch (e) {
        console.error(e);
        setStatus("failed to load thermo.json");
      }
    });
  }

  const loadMech = document.getElementById("loadMechanics");
  if (loadMech) {
    loadMech.addEventListener("click", async () => {
      try {
        const starter = await fetchJson("./models/mechanics.json");
        project.vars = starter.vars || [];
        project.eqs = starter.eqs || [];
        project.assumptions = starter.assumptions ?? [];
        project.values = starter.values ?? [];
        saveProject(project);
        renderAll(project);
        setupExplorer(project);
        setStatus("loaded mechanics starter");
        setTimeout(() => setStatus("ready"), 900);
      } catch (e) {
        console.error(e);
        setStatus("failed to load mechanics.json");
      }
    });
  }

  // click outside math-field hides keyboard
  document.addEventListener("mousedown", (ev) => {
    const target = ev.target;
    const insideMathField = target && target.closest && target.closest("math-field");
    if (!insideMathField) hideKeyboardNow();
  });

  renderAll(project);
  setupExplorer(project);
  setStatus("ready");
}

bootstrap().catch(err => {
  console.error(err);
  setStatus("error loading app");
});
