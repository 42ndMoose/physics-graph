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

function safeStorageGet(key) {
  try { return localStorage.getItem(key); } catch { return null; }
}

function safeStorageSet(key, value) {
  try { localStorage.setItem(key, value); } catch {}
}

function safeStorageRemove(key) {
  try { localStorage.removeItem(key); } catch {}
}

function loadProject() {
  const raw = safeStorageGet(STORAGE_KEY);
  if (!raw) return null;
  return safeJsonParse(raw);
}

function saveProject(project) {
  safeStorageSet(STORAGE_KEY, JSON.stringify(project, null, 2));
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
  const candidates = [path];
  if (path.startsWith("./models/")) {
    candidates.push(`./docs/models/${path.split("/").pop()}`);
  }
  if (path.startsWith("./docs/models/")) {
    candidates.push(`./models/${path.split("/").pop()}`);
  }
  let lastErr = null;
  for (const candidate of candidates) {
    try {
      const res = await fetch(candidate, { cache: "no-store" });
      if (res.ok) return await res.json();
      lastErr = new Error(`failed to load: ${candidate}`);
    } catch (err) {
      lastErr = err;
    }
  }
  throw lastErr || new Error(`failed to load: ${path}`);
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



function getAppState() {
  globalThis.__physicsGraphAppState ??= {
    explorerFilterEqIds: null,
    explorerSelectedSymbol: null,
    explorerNodeId: null
  };
  return globalThis.__physicsGraphAppState;
}

function normalizeEquationText(latex) {
  return (latex || "").replaceAll(" ", "").trim();
}

function equationIdsForNode(project, node) {
  if (!node || !Array.isArray(node.equations) || !node.equations.length) return null;
  const wanted = new Set(node.equations.map(normalizeEquationText));
  const matched = (project.eqs || [])
    .filter((eq) => wanted.has(normalizeEquationText(eq.latex)))
    .map((eq) => eq.id);
  return matched.length ? matched : null;
}

function pickDefiningSymbol(eq) {
  const latex = eq?.latex || "";
  const lhs = latex.includes("=") ? latex.split("=")[0] : latex;
  const syms = extractSymbols(lhs || "");
  return syms[0] || null;
}

function equationAlternativesForSymbol(project, symbol) {
  if (!symbol) return [];
  return (project.eqs || []).filter((eq) => pickDefiningSymbol(eq) === symbol);
}

function selectEquationForSymbol(project, symbol, eqId) {
  if (!symbol || !eqId) return;
  project.ui ??= {};
  project.ui.selectedEqBySymbol ??= {};
  project.ui.selectedEqBySymbol[symbol] = eqId;
}

function normalizeProject(project) {
  const base = project && typeof project === "object" ? project : {};
  base.project ??= { id: "proj_001", name: "physics graph", version: 1 };
  base.vars = Array.isArray(base.vars) ? base.vars : [];
  base.eqs = Array.isArray(base.eqs) ? base.eqs : [];
  base.assumptions = Array.isArray(base.assumptions) ? base.assumptions : [];
  base.values = Array.isArray(base.values) ? base.values : [];
  base.ui ??= {};
  return base;
}

// --------------------
// Explorer canvas (zoom + pan)
// --------------------
const GRAPH_NODES = [
  {
    id: "core",
    label: "Physics = 1",
    level: 0,
    x: 0,
    y: 0,
    summary: "The core conservation-style identity we want to explain through everything else."
  },
  {
    id: "mechanics",
    label: "Mechanics",
    level: 1,
    x: -260,
    y: -90,
    summary: "Mass, momentum, energy, and force relationships.",
    equations: ["F = ma", "p = mv", "K = \\frac{1}{2}mv^2"],
    refs: [{ label: "Newton (1687)", url: "https://doi.org/10.5479/sil.52126.39088015628375" }]
  },
  {
    id: "thermo",
    label: "Thermo",
    level: 1,
    x: 250,
    y: -120,
    summary: "Heat, work, entropy, and state variables.",
    equations: ["\\Delta U = Q - W", "PV = nRT"],
    refs: [{ label: "Clausius (1850)", url: "https://doi.org/10.1002/andp.18501550405" }]
  },
  {
    id: "em",
    label: "Electromagnetism",
    level: 1,
    x: 200,
    y: 160,
    summary: "Fields, charge, and light.",
    equations: ["\\nabla \\cdot E = \\frac{\\rho}{\\epsilon_0}", "F = q(E + v \\times B)"],
    refs: [{ label: "Maxwell (1865)", url: "https://doi.org/10.1098/rstl.1865.0008" }]
  },
  {
    id: "momentum",
    label: "Momentum",
    level: 2,
    x: -420,
    y: -200,
    summary: "Momentum continuity and collisions.",
    equations: ["p = mv", "J = \\Delta p"]
  },
  {
    id: "force",
    label: "Force",
    level: 2,
    x: -360,
    y: 20,
    summary: "Force definitions mapped to various theories.",
    equations: ["F = ma", "F = -\\nabla U", "F = qE"],
    refs: [{ label: "Classical mechanics notes", url: "https://doi.org/10.1119/1.197" }]
  },
  {
    id: "entropy",
    label: "Entropy",
    level: 2,
    x: 360,
    y: -240,
    summary: "Entropy links microstates to macrostates.",
    equations: ["S = k \\ln \\Omega", "\\Delta S \\geq 0"]
  },
  {
    id: "state",
    label: "State equations",
    level: 2,
    x: 420,
    y: -20,
    summary: "Equation-of-state relationships for matter.",
    equations: ["PV = nRT"]
  },
  {
    id: "fields",
    label: "Field laws",
    level: 2,
    x: 340,
    y: 240,
    summary: "Maxwell and derived field equations.",
    equations: ["\\nabla \\times B = \\mu_0 J + \\mu_0\\epsilon_0 \\frac{\\partial E}{\\partial t}"]
  }
];

const GRAPH_EDGES = [
  ["core", "mechanics"],
  ["core", "thermo"],
  ["core", "em"],
  ["mechanics", "momentum"],
  ["mechanics", "force"],
  ["thermo", "entropy"],
  ["thermo", "state"],
  ["em", "fields"]
];

function findEquationByLatex(project, latex) {
  const target = normalizeEquationText(latex);
  return (project.eqs || []).find((eq) => normalizeEquationText(eq.latex) === target) || null;
}

function equationForNode(project, node) {
  const appState = getAppState();
  const nodeEquations = node?.equations || [];
  for (const latex of nodeEquations) {
    const eq = findEquationByLatex(project, latex);
    if (eq) {
      const symbol = pickDefiningSymbol(eq);
      const selected = symbol ? project.ui?.selectedEqBySymbol?.[symbol] : null;
      if (!selected || selected === eq.id) return eq;
    }
  }
  if (appState.explorerSelectedSymbol) {
    const selectedId = project.ui?.selectedEqBySymbol?.[appState.explorerSelectedSymbol];
    if (selectedId) return (project.eqs || []).find((eq) => eq.id === selectedId) || null;
  }
  return nodeEquations.length ? findEquationByLatex(project, nodeEquations[0]) : null;
}

function inferBalancedEquationDim(eq, varDimMap) {
  if (!eq?.latex || !eq.latex.includes('=')) return null;
  const lhs = eq.latex.split('=')[0].trim();
  const res = unitsOfLatexExpr(lhs, varDimMap);
  return res.ok ? res.dim : null;
}

function setupExplorer(project) {
  const viewport = document.getElementById('graphViewport');
  const nodesLayer = document.getElementById('graphNodesLayer');
  const edgesSvg = document.getElementById('graphEdges');
  if (!viewport || !nodesLayer || !edgesSvg) return;

  const depthLabel = document.getElementById('depthLabel');
  const focusCard = document.getElementById('focusCard');

  const state = {
    scale: 1,
    offset: { x: 0, y: 0 },
    dragging: false,
    moved: false,
    hoverId: null,
    selectedId: getAppState().explorerNodeId || 'core',
    last: { x: 0, y: 0 }
  };

  const nodesById = new Map(GRAPH_NODES.map((node) => [node.id, node]));
  const neighbors = {};
  for (const [a, b] of GRAPH_EDGES) {
    neighbors[a] ??= [];
    neighbors[b] ??= [];
    neighbors[a].push(b);
    neighbors[b].push(a);
  }

  function getDepth() {
    if (state.scale < 1.1) return 1;
    if (state.scale < 1.6) return 2;
    return 3;
  }

  function visibleNodes() {
    const maxLevel = getDepth() - 1;
    return GRAPH_NODES.filter((node) => node.level <= maxLevel);
  }

  function descendants(nodeId) {
    const depthCap = getDepth() - 1;
    const out = [];
    const queue = [nodeId];
    const seen = new Set(queue);
    while (queue.length) {
      const curr = queue.shift();
      for (const nextId of (neighbors[curr] || [])) {
        if (seen.has(nextId)) continue;
        const nextNode = nodesById.get(nextId);
        if (!nextNode || nextNode.level <= (nodesById.get(curr)?.level ?? 0) || nextNode.level > depthCap) continue;
        seen.add(nextId);
        queue.push(nextId);
        out.push(nextNode);
      }
    }
    return out;
  }

  function projectPoint(node) {
    return {
      x: node.x * state.scale + state.offset.x,
      y: node.y * state.scale + state.offset.y
    };
  }

  function clampOffset() {
    const visible = visibleNodes();
    if (!visible.length) return;
    const xs = visible.map((n) => n.x * state.scale);
    const ys = visible.map((n) => n.y * state.scale);
    const pad = 160;
    const minX = Math.min(...xs) - pad;
    const maxX = Math.max(...xs) + pad;
    const minY = Math.min(...ys) - pad;
    const maxY = Math.max(...ys) + pad;
    const width = viewport.clientWidth;
    const height = viewport.clientHeight;

    const xMinAllowed = width - maxX;
    const xMaxAllowed = -minX;
    const yMinAllowed = height - maxY;
    const yMaxAllowed = -minY;

    state.offset.x = Math.min(xMaxAllowed, Math.max(xMinAllowed, state.offset.x));
    state.offset.y = Math.min(yMaxAllowed, Math.max(yMinAllowed, state.offset.y));
  }

  function renderFocus(node) {
    if (!focusCard) return;
    if (!node) {
      focusCard.textContent = 'hover or click an item to inspect equations.';
      return;
    }
    const appState = getAppState();
    const usage = buildUsageIndex(project);
    const varDimMap = buildVarDimMap(project);
    const nodeEquation = equationForNode(project, node);
    const eqs = (node.equations || []).map((eq) => `<li>${escapeHtml(eq)}</li>`).join('');
    const symbols = Array.from(new Set((node.equations || []).flatMap((eq) => extractSymbols(eq))));
    const symbolButtons = symbols.map((symbol) => {
      const active = appState.explorerSelectedSymbol === symbol;
      return `<button class="small symbolJump ${active ? 'isActive' : ''}" data-symbol="${escapeHtml(symbol)}">${escapeHtml(symbol)}</button>`;
    }).join('');

    const alternatives = equationAlternativesForSymbol(project, appState.explorerSelectedSymbol);
    const selectedEqBySymbol = project.ui?.selectedEqBySymbol || {};
    const selectedForSymbol = appState.explorerSelectedSymbol ? selectedEqBySymbol[appState.explorerSelectedSymbol] : null;
    const alternativesHtml = alternatives.map((eq) => {
      const picked = selectedForSymbol === eq.id;
      const theory = eq.theory ? `<div class="altMeta">${escapeHtml(eq.theory)}</div>` : '';
      const doi = eq.doi ? `<a href="https://doi.org/${encodeURIComponent(eq.doi)}" target="_blank" rel="noreferrer">doi:${escapeHtml(eq.doi)}</a>` : '';
      const source = eq.sourceUrl ? `<a href="${eq.sourceUrl}" target="_blank" rel="noreferrer">reference</a>` : '';
      return `<button class="altEq ${picked ? 'isActive' : ''}" data-eqid="${eq.id}" data-symbol="${escapeHtml(appState.explorerSelectedSymbol || '')}"><div class="altTop"><strong>${escapeHtml(eq.title || eq.id)}</strong>${picked ? '<span class="badge good">selected</span>' : ''}</div><div class="altLatex">${escapeHtml(eq.latex || '')}</div>${theory}${(doi || source) ? `<div class="altLinks">${doi}${source}</div>` : ''}</button>`;
    }).join('');

    const nodeDim = inferBalancedEquationDim(nodeEquation, varDimMap);
    const compatibility = (neighbors[node.id] || [])
      .map((id) => nodesById.get(id))
      .filter(Boolean)
      .map((peer) => {
        const peerEq = equationForNode(project, peer);
        const peerDim = inferBalancedEquationDim(peerEq, varDimMap);
        const ok = nodeDim && peerDim ? dimEq(nodeDim, peerDim) : null;
        return `<li>${escapeHtml(peer.label)}: ${ok === null ? 'unknown' : ok ? 'compatible' : 'mismatch'} ${ok === false ? '⚠️' : ''}</li>`;
      }).join('');

    const nodeRefs = [];
    if (nodeEquation?.doi) nodeRefs.push(`<a href="https://doi.org/${encodeURIComponent(nodeEquation.doi)}" target="_blank" rel="noreferrer">doi:${escapeHtml(nodeEquation.doi)}</a>`);
    if (nodeEquation?.sourceUrl) nodeRefs.push(`<a href="${nodeEquation.sourceUrl}" target="_blank" rel="noreferrer">reference</a>`);
    for (const ref of (node.refs || [])) nodeRefs.push(`<a href="${ref.url}" target="_blank" rel="noreferrer">${escapeHtml(ref.label)}</a>`);

    const expanded = descendants(node.id).map((n) => `<li>${escapeHtml(n.label)}</li>`).join('');
    focusCard.innerHTML = `
      <div class="detailBadge">level ${node.level + 1}</div>
      <strong>${escapeHtml(node.label)}</strong>
      <span>${escapeHtml(node.summary || 'No summary yet.')}</span>
      ${nodeEquation ? `<div class="focusEq" id="focusEq"></div><div class="focusEqLabel">${escapeHtml(nodeEquation.title || 'equation')}</div>` : ''}
      ${eqs ? `<ul class="detailList">${eqs}</ul>` : ''}
      ${expanded ? `<div class="focusSection"><div class="detailTitle">expanded downstream items</div><ul class="detailList">${expanded}</ul></div>` : ''}
      ${symbolButtons ? `<div class="focusSection"><div class="detailTitle">variables in this branch</div><div class="row">${symbolButtons}</div></div>` : ''}
      ${alternativesHtml ? `<div class="focusSection"><div class="detailTitle">candidate equations for ${escapeHtml(appState.explorerSelectedSymbol)}</div><div class="altList">${alternativesHtml}</div></div>` : ''}
      ${compatibility ? `<div class="focusSection"><div class="detailTitle">adjacent consistency</div><ul class="detailList">${compatibility}</ul></div>` : ''}
      ${nodeRefs.length ? `<div class="focusSection"><div class="detailTitle">citations</div><div class="altLinks">${nodeRefs.join('')}</div></div>` : ''}
    `;

    if (nodeEquation) {
      const eqEl = focusCard.querySelector('#focusEq');
      if (eqEl) {
        try { katex.render(nodeEquation.latex, eqEl, { throwOnError: false, displayMode: true }); }
        catch { eqEl.textContent = nodeEquation.latex; }
      }
    }

    for (const button of focusCard.querySelectorAll('.symbolJump')) {
      button.addEventListener('click', () => {
        appState.explorerSelectedSymbol = button.dataset.symbol || null;
        appState.explorerFilterEqIds = appState.explorerSelectedSymbol ? (usage[appState.explorerSelectedSymbol] || []) : equationIdsForNode(project, node);
        renderAll(project);
        renderFocus(node);
      });
    }

    for (const button of focusCard.querySelectorAll('.altEq')) {
      button.addEventListener('click', () => {
        const eqId = button.dataset.eqid;
        const symbol = button.dataset.symbol;
        if (!eqId || !symbol) return;
        selectEquationForSymbol(project, symbol, eqId);
        saveProject(project);
        renderAll(project);
        renderFocus(node);
        renderGraph();
      });
    }
  }

  function renderGraph() {
    const nodes = visibleNodes();
    clampOffset();
    edgesSvg.setAttribute('viewBox', `0 0 ${viewport.clientWidth} ${viewport.clientHeight}`);

    const edgeMarkup = GRAPH_EDGES
      .map(([a, b]) => [nodesById.get(a), nodesById.get(b)])
      .filter(([a, b]) => a && b && nodes.includes(a) && nodes.includes(b))
      .map(([a, b]) => {
        const pa = projectPoint(a);
        const pb = projectPoint(b);
        const active = a.id === state.selectedId || b.id === state.selectedId;
        return `<line x1="${pa.x}" y1="${pa.y}" x2="${pb.x}" y2="${pb.y}" class="mapEdge ${active ? 'isActive' : ''}" />`;
      }).join('');
    edgesSvg.innerHTML = edgeMarkup;

    nodesLayer.innerHTML = '';
    for (const node of nodes) {
      const p = projectPoint(node);
      const card = document.createElement('button');
      const nodeEq = equationForNode(project, node);
      const isHover = node.id === state.hoverId;
      const isSelected = node.id === state.selectedId;
      card.className = `mapNode level${node.level} ${isHover ? 'isHover' : ''} ${isSelected ? 'isSelected' : ''}`;
      card.style.left = `${p.x}px`;
      card.style.top = `${p.y}px`;
      card.dataset.nodeid = node.id;
      card.innerHTML = `<div class="mapEquation" id="map_eq_${node.id}"></div><div class="mapLabel">${escapeHtml(node.label)}</div>${nodeEq?.doi ? `<div class="mapRef">doi:${escapeHtml(nodeEq.doi)}</div>` : '<div class="mapRef">reference tagged</div>'}`;
      card.addEventListener('mouseenter', () => {
        state.hoverId = node.id;
        renderFocus(node);
        renderGraph();
      });
      card.addEventListener('mouseleave', () => {
        state.hoverId = null;
        renderGraph();
      });
      card.addEventListener('click', () => {
        state.selectedId = node.id;
        const appState = getAppState();
        appState.explorerNodeId = node.id;
        if (!appState.explorerSelectedSymbol) appState.explorerFilterEqIds = equationIdsForNode(project, node);
        renderFocus(node);
        renderAll(project);
        renderGraph();
      });
      nodesLayer.appendChild(card);

      const eqEl = card.querySelector(`#map_eq_${node.id}`);
      const fallback = nodeEq?.latex || (node.equations?.[0] || node.label);
      if (eqEl) {
        try { katex.render(fallback, eqEl, { throwOnError: false, displayMode: true }); }
        catch { eqEl.textContent = fallback; }
      }
    }

    if (depthLabel) depthLabel.textContent = `depth ${getDepth()} / 3`;
  }

  function zoomAt(multiplier, centerX, centerY) {
    const next = Math.min(2.6, Math.max(0.85, state.scale * multiplier));
    const wx = (centerX - state.offset.x) / state.scale;
    const wy = (centerY - state.offset.y) / state.scale;
    state.scale = next;
    state.offset.x = centerX - wx * state.scale;
    state.offset.y = centerY - wy * state.scale;
    clampOffset();
  }

  function onWheel(ev) {
    ev.preventDefault();
    const rect = viewport.getBoundingClientRect();
    const x = ev.clientX - rect.left;
    const y = ev.clientY - rect.top;
    zoomAt(ev.deltaY < 0 ? 1.1 : 0.9, x, y);
    if (state.hoverId && ev.deltaY < 0) {
      state.selectedId = state.hoverId;
      const node = nodesById.get(state.selectedId);
      const appState = getAppState();
      appState.explorerNodeId = state.selectedId;
      if (node && !appState.explorerSelectedSymbol) appState.explorerFilterEqIds = equationIdsForNode(project, node);
      renderFocus(node);
      renderAll(project);
    }
    renderGraph();
  }

  viewport.onmousedown = (ev) => {
    state.dragging = true;
    state.moved = false;
    state.last = { x: ev.clientX, y: ev.clientY };
  };
  window.onmouseup = () => { state.dragging = false; };
  viewport.onmousemove = (ev) => {
    if (!state.dragging) return;
    const dx = ev.clientX - state.last.x;
    const dy = ev.clientY - state.last.y;
    if (Math.abs(dx) + Math.abs(dy) > 2) state.moved = true;
    state.offset.x += dx;
    state.offset.y += dy;
    state.last = { x: ev.clientX, y: ev.clientY };
    clampOffset();
    renderGraph();
  };
  viewport.onwheel = onWheel;

  function centerInitial() {
    state.offset.x = viewport.clientWidth / 2;
    state.offset.y = viewport.clientHeight / 2;
    clampOffset();
  }

  window.addEventListener('resize', () => {
    clampOffset();
    renderGraph();
  });

  centerInitial();
  renderFocus(nodesById.get(state.selectedId));
  renderGraph();
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

  const stateRef = getAppState();
  for (const eq of (project.eqs || [])) {
    if (stateRef.explorerFilterEqIds && !stateRef.explorerFilterEqIds.includes(eq.id)) {
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
        ${pickDefiningSymbol(eq) ? `<button class="small" id="btnUse_${eq.id}">use for ${escapeHtml(pickDefiningSymbol(eq))}</button>` : ""}
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
    const btnUse  = document.getElementById(`btnUse_${eq.id}`);
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

    if (btnUse) {
      btnUse.addEventListener("click", () => {
        const symbol = pickDefiningSymbol(eq);
        if (!symbol) return;
        selectEquationForSymbol(project, symbol, eq.id);
        const stateRef = getAppState();
        stateRef.explorerSelectedSymbol = symbol;
        stateRef.explorerFilterEqIds = usage[symbol] || null;
        saveProject(project);
        renderAll(project);
      });
    }
  }
}

function renderAll(project) {
  renderCards(project);
  renderVarsTable(project);
}

function safeSetupExplorer(project) {
  try {
    setupExplorer(project);
  } catch (err) {
    console.error("explorer init failed", err);
    setStatus("explorer unavailable");
  }
}

// --------------------
// Bootstrap
// --------------------
async function bootstrap() {
  setStatus("loading…");

  const loadedProject = loadProject();
  let project = normalizeProject(loadedProject);
  if (!loadedProject) {
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
      safeStorageRemove(STORAGE_KEY);
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
      project = normalizeProject(obj);
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
