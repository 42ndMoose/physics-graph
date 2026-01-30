const STORAGE_KEY = "physics_graph_project_v1";

// --------------------
// Units: parsing + dimensional arithmetic
// --------------------
const BASE_DIMS = ["M","L","T","Th","I","N","J"]; // mass, length, time, temperature, current, amount, luminous

const UNIT_DEFS = {
  "1": {},
  "m":  { L: 1 },
  "s":  { T: 1 },
  "kg": { M: 1 },
  "K":  { Th: 1 },
  "A":  { I: 1 },
  "mol":{ N: 1 },
  "cd": { J: 1 },

  "N":  { M: 1, L: 1, T: -2 },                 // kg*m/s^2
  "J":  { M: 1, L: 2, T: -2 },                 // N*m
  "W":  { M: 1, L: 2, T: -3 },                 // J/s
  "Pa": { M: 1, L: -1, T: -2 }                 // N/m^2
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
  for (let i=0;i<ak.length;i++) {
    if (ak[i] !== bk[i]) return false;
    if (Math.abs(a[ak[i]] - b[bk[i]]) > 1e-12) return false;
  }
  return true;
}

function dimToString(d) {
  if (!d || Object.keys(d).length === 0) return "1";
  return Object.entries(d)
    .sort((x,y) => x[0].localeCompare(y[0]))
    .map(([k,v]) => v === 1 ? k : `${k}^${v}`)
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

// Very small unit expression parser: supports *, /, ^, parentheses, and known unit symbols.
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
    // allow plain "1"
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

// Map variable latex symbol -> dim vector
function buildVarDimMap(project) {
  const map = {};
  for (const v of (project.vars || [])) {
    const d = parseUnitExpr(v.units);
    if (d) map[v.latex] = d;
  }
  return map;
}

// --------------------
// LaTeX expression -> unit dims (MVP parser)
// Supports: identifiers, numbers, implicit multiplication, \frac{a}{b}, +/-, parentheses, ^integer
// Anything with derivatives, sums, integrals, trig etc => "unknown" for now.
// --------------------
function latexHasUnsupportedOps(lx) {
  const bad = ["\\partial","\\nabla","\\int","\\sum","\\sin","\\cos","\\tan","\\log","\\ln","\\exp"];
  return bad.some(x => lx.includes(x));
}

function tokenizeLatex(lx) {
  // strip some wrappers
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

    // commands like \rho, \frac
    if (c === "\\") {
      let j = i + 1;
      while (j < lx.length && /[a-zA-Z]/.test(lx[j])) j++;
      const cmd = lx.slice(i, j);
      tokens.push(cmd);
      i = j;
      continue;
    }

    // numbers
    if (/[0-9.]/.test(c)) {
      let j = i;
      while (j < lx.length && /[0-9.]/.test(lx[j])) j++;
      tokens.push(lx.slice(i, j));
      i = j;
      continue;
    }

    // single-letter identifiers
    if (/[a-zA-Z]/.test(c)) {
      tokens.push(c);
      i++;
      continue;
    }

    // operators / punctuation
    if ("=+-*/^(){}".includes(c)) {
      tokens.push(c);
      i++;
      continue;
    }

    // underscore: skip subscript payload (treat as part of same symbol)
    if (c === "_") {
      tokens.push("_");
      i++;
      continue;
    }

    // everything else ignored
    i++;
  }
  return tokens;
}

function readBraceGroup(tokens, idx) {
  // expects '{' at idx
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
  if (latexHasUnsupportedOps(latex)) return { dim: null, ok: false, why: "unsupported operator (derivatives/integrals/etc) in MVP" };

  const tokens = tokenizeLatex(latex);
  let i = 0;

  function peek() { return tokens[i] || null; }
  function eat(x) { if (tokens[i] === x) { i++; return true; } return false; }

  function skipSubscriptIfAny() {
    if (eat("_")) {
      // skip either { ... } or one token
      if (peek() === "{") {
        const g = readBraceGroup(tokens, i);
        i = g.next;
      } else {
        i++;
      }
    }
  }

  function parsePrimary() {
    const t = peek();
    if (!t) return null;

    if (eat("(")) {
      const d = parseExpr();
      if (!eat(")")) return null;
      return d;
    }

    // \frac{a}{b}
    if (t === "\\frac") {
      i++;
      if (peek() !== "{") return null;
      const a = readBraceGroup(tokens, i); i = a.next;
      if (peek() !== "{") return null;
      const b = readBraceGroup(tokens, i); i = b.next;
      const da = unitsFromTokens(a.expr, varDimMap);
      const db = unitsFromTokens(b.expr, varDimMap);
      if (!da || !db) return null;
      return dimDiv(da, db);
    }

    // identifier or command
    if (/^\\[a-zA-Z]+$/.test(t) || /^[a-zA-Z]$/.test(t)) {
      i++;
      skipSubscriptIfAny();
      const d = varDimMap[t];
      // numbers like "e" would be caught here, but we treat unknown id as dimensionless for now? better: unknown => null
      return d ? d : null;
    }

    // number => dimensionless
    if (/^[0-9.]+$/.test(t)) {
      i++;
      return {};
    }

    // braces used for grouping sometimes: treat {expr} as expr
    if (eat("{")) {
      const inner = [];
      let depth = 1;
      while (i < tokens.length && depth > 0) {
        if (tokens[i] === "{") depth++;
        else if (tokens[i] === "}") depth--;
        if (depth > 0) inner.push(tokens[i]);
        i++;
      }
      const d = unitsFromTokens(inner, varDimMap);
      return d;
    }

    return null;
  }

  function parseFactor() {
    let base = parsePrimary();
    if (!base) return null;

    if (eat("^")) {
      const t = peek();
      if (!t) return null;
      // only allow simple numeric exponent in MVP
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
      if (t === "*" ) { i++; const r = parseFactor(); if (!r) return null; d = dimMul(d, r); continue; }
      if (t === "/" ) { i++; const r = parseFactor(); if (!r) return null; d = dimDiv(d, r); continue; }

      // implicit multiplication: two factors adjacent
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
        // for +/-, units must match
        if (!dimEq(d, r)) return null;
        continue;
      }
      break;
    }
    return d;
  }

  function unitsFromTokens(tokList, varDimMap) {
    const savedTokens = tokens;
    const savedI = i;
    try {
      // hack: temporarily parse a sub-token list by swapping globals
      // we'll just parse by joining back into latex and calling unitsOfLatexExpr recursively (safe enough for MVP)
      const subLatex = tokList.join("");
      const res = unitsOfLatexExpr(subLatex, varDimMap);
      return res.dim;
    } finally {
      // restore (no-op because we used recursion that does not touch current tokens)
    }
  }

  const dim = parseExpr();
  if (!dim) return { dim: null, ok: false, why: "could not infer units (unknown symbol or unsupported structure)" };
  if (i !== tokens.length) return { dim: null, ok: false, why: "trailing tokens (unsupported structure)" };
  return { dim, ok: true, why: "" };
}

function unitCheckEquation(eqLatex, varDimMap) {
  const parts = (eqLatex || "").split("=");
  if (parts.length !== 2) return { status: "unknown", diag: "unit check: expected a single '=' in equation" };
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

  return {
    status: "bad",
    diag: `unit mismatch: LHS ${dimToString(ul.dim)} vs RHS ${dimToString(ur.dim)}`
  };
}

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

    // Unit checks
  const varDimMap = buildVarDimMap(project);
  for (const eq of project.eqs) {
    const res = unitCheckEquation(eq.latex, varDimMap);
    eq.status = res.status;
    eq.diagnostics = [res.diag];
  }
  saveProject(project);

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

        // apply keyboard setting to this field
    const enabled = (loadProject()?.ui?.keyboardEnabled ?? true);
    mf.setOptions({ virtualKeyboardMode: enabled ? "onfocus" : "off" });

    mf.addEventListener("focus", () => {
      const enabledNow = (loadProject()?.ui?.keyboardEnabled ?? true);
      const x = document.getElementById("btnKbHide");
      if (x) x.style.display = enabledNow ? "block" : "none";
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

// --------------------
// Virtual keyboard controls (MathLive)
// --------------------
function setKeyboardEnabled(enabled) {
  const project = loadProject();
  if (!project) return;

  project.ui ??= {};
  project.ui.keyboardEnabled = enabled;
  saveProject(project);

  const btn = document.getElementById("btnKbToggle");
  if (btn) btn.textContent = enabled ? "kb: on" : "kb: off";

  // Apply to all math-fields currently in DOM
  document.querySelectorAll("math-field").forEach(mf => {
    mf.setOptions({ virtualKeyboardMode: enabled ? "onfocus" : "off" });
  });

  if (!enabled) hideKeyboardNow();
}

function hideKeyboardNow() {
  try {
    // MathLive global keyboard object exists when loaded
    if (window.mathVirtualKeyboard) window.mathVirtualKeyboard.hide();
  } catch {}
  const x = document.getElementById("btnKbHide");
  if (x) x.style.display = "none";
}

function renderVarsTable(project) {
  const el = document.getElementById("varsTable");
  if (!el) return;

  const usage = buildUsageIndex(project);
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
  });

  el.innerHTML = `
    <div class="varsRow varsHead">
      <div>symbol</div>
      <div>units</div>
      <div>used</div>
    </div>
    ${rows.join("")}
  `;
    renderVarsTable(project);
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
    const starter = await fetchJson("./models/thermo.json");
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

  // kb toggle button
  const kbBtn = document.getElementById("btnKbToggle");
  if (kbBtn) {
    const p = loadProject();
    const enabled = (p?.ui?.keyboardEnabled ?? true);
    kbBtn.textContent = enabled ? "kb: on" : "kb: off";
    kbBtn.addEventListener("click", () => {
      const cur = (loadProject()?.ui?.keyboardEnabled ?? true);
      setKeyboardEnabled(!cur);
    });
  }

  // kb hide X
  const kbHide = document.getElementById("btnKbHide");
  if (kbHide) {
    kbHide.addEventListener("click", () => {
      hideKeyboardNow();
      // also blur active mathfield
      const active = document.activeElement;
      if (active && active.tagName && active.tagName.toLowerCase() === "math-field") active.blur();
    });
  }

  // clicking outside math-field hides keyboard
  document.addEventListener("mousedown", (ev) => {
    const target = ev.target;
    const insideMathField = target && (target.closest && target.closest("math-field"));
    const clickedX = target && target.id === "btnKbHide";
    if (!insideMathField && !clickedX) hideKeyboardNow();
  });

bootstrap().catch(err => {
  console.error(err);
  setStatus("error loading app");
});
