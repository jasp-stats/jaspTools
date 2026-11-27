(() => {
  "use strict";
  const {items} = JSON.parse(document.getElementById("review-data").textContent);
  const byId = id => document.getElementById(id);
  const decisions = new Map(), drafts = new Map(), history = [];
  const labels = {pending: "Awaiting decision", accept: "Accepted", decline: "Declined", comment: "Commented · Unresolved", reference: "Reference only"};
  const status = item => item.candidateHash == null ? "reference" : decisions.get(item.id)?.action || "pending";
  let current = (items.find(item => status(item) === "pending") || items[0])?.id;
  const modeLabels = {diff: "Visual differences", candidate: "Candidate", baseline: "Baseline"};
  const tableModeLabels = {diff: "Differences (formatted)", precision: "Differences (full precision)",
    candidate: "Candidate (formatted)", baseline: "Baseline (formatted)"};
  let drawn, mode;

  function el(tag, text, className) {
    const node = document.createElement(tag);
    if (text !== undefined) node.textContent = text;
    if (className) node.className = className;
    return node;
  }

  function visible() {
    const query = byId("search").value.toLowerCase();
    const filter = byId("filter").value;
    return items.filter(item => (filter === "all" || status(item) === filter || (filter === "pending" && status(item) === "comment")) &&
      [item.id, item.kind, item.new?.title, item.old?.title].join(" ").toLowerCase().includes(query));
  }

  function draw() {
    const list = visible();
    let index = list.findIndex(item => item.id === current);
    if (index < 0 && list.length) { index = 0; current = list[0].id; }
    const item = list[index];
    const candidates = items.filter(entry => entry.candidateHash != null);
    const resolved = candidates.filter(entry => ["accept", "decline"].includes(status(entry))).length;
    byId("progress").textContent = resolved + " of " + candidates.length + " resolved";
    byId("export").disabled = !decisions.size;
    const sidebar = byId("items"), sidebarFocused = sidebar.contains(document.activeElement);
    sidebar.replaceChildren(...list.map(entry => {
      const button = el("button", entry.id, "item");
      button.setAttribute("aria-current", String(entry.id === current));
      button.append(el("small", labels[status(entry)]));
      button.onclick = () => { current = entry.id; draw(); };
      return button;
    }));
    if (sidebarFocused) sidebar.querySelector('[aria-current="true"]')?.focus();
    byId("empty").hidden = Boolean(item);
    byId("review").hidden = !item;
    byId("undo").disabled = !history.length;
    byId("accept").disabled = byId("decline").disabled = !item || item.candidateHash == null;
    if (!item) return;
    const reference = status(item) === "reference";
    byId("previous").disabled = index === 0;
    byId("next").disabled = index === list.length - 1;
    byId("status").textContent = item.kind + " · " + labels[status(item)] + (item.old == null ? " · New snapshot" : "");
    byId("path").textContent = item.id;
    byId("comment").value = drafts.get(item.id) ?? decisions.get(item.id)?.comment ?? "";
    byId("comment-only").disabled = !byId("comment").value.trim();
    byId("decision").hidden = reference;
    if (drawn !== item) {
      mode = reference ? "baseline" : item.old == null ? "candidate" : "diff";
      document.querySelector("main").scrollTop = 0;
      drawn = item;
      drawPreview(item);
    }
  }

  function previewModes(item) {
    const labels = item.kind === "table" ? tableModeLabels : modeLabels;
    if (item.old != null && item.new != null) return labels;
    const side = item.new == null ? "baseline" : "candidate";
    const modes = {[side]: labels[side]};
    if (item.kind === "table") modes.raw = modeLabels[side] + " (full precision)";
    return modes;
  }

  function nextMode(labels) {
    const keys = Object.keys(labels);
    return keys[(keys.indexOf(mode) + 1) % keys.length];
  }

  function drawPreview(item, scrollLeft = 0) {
    const target = byId("preview"), cycle = byId("cycle");
    const shown = mode === "baseline" || item.new == null ? item.old : item.new;
    byId("title").textContent = plainText(shown?.title) || item.id.split("/").pop();
    target.replaceChildren();
    byId("preview-details").replaceChildren();
    const labels = previewModes(item), next = nextMode(labels);
    target.classList.toggle("swappable", next !== mode);
    byId("view-label").textContent = labels[mode];
    cycle.hidden = next === mode;
    cycle.title = "Show " + labels[next].toLowerCase() + " (T)";
    target.title = cycle.hidden ? "" : cycle.title;
    if (item.kind === "table") showTable(item, mode, shown, target);
    if (item.kind === "figure") {
      void showFigure(item, mode, target);
      if (item.removeStructure)
        byId("preview-details").append(el("p", "Accepting this figure also removes its previous plot structure reference.", "caption"));
    }
    if (item.kind === "html") showHtml(item, mode, target);
    if (scrollLeft && target.querySelector(".scroll")) target.querySelector(".scroll").scrollLeft = scrollLeft;
  }

  function cyclePreview() {
    const item = visible().find(entry => entry.id === current);
    if (!item || byId("cycle").hidden) return;
    mode = nextMode(previewModes(item));
    drawPreview(item, byId("preview").querySelector(".scroll")?.scrollLeft);
  }

  function valueText(value) {
    if (value === undefined) return "—";
    if (value === "") return '""';
    if (Object.is(value, -0)) return "-0";
    return typeof value === "string" ? value : JSON.stringify(value);
  }

  function plainText(value) {
    const template = el("template");
    template.innerHTML = String(value ?? "");
    return template.content.textContent;
  }

  function noteSymbol(note, index) {
    const symbol = note?.symbol ?? index;
    return typeof symbol === "number" && symbol >= 0 && symbol < 26 ?
      String.fromCharCode(97 + symbol) : plainText(symbol);
  }

  function noteRefs(node, indices, table) {
    if (indices == null) return;
    for (const index of [].concat(indices)) {
      const note = typeof index === "number" ? table?.footnotes?.[index] : null;
      const sup = el("sup", noteSymbol(note, index));
      sup.title = plainText(note?.text ?? note ?? index);
      node.append(sup);
    }
  }

  function decimalText(change) {
    if (change.decimalPlace == null) return change.delta;
    const place = change.decimalPlace;
    const precision = place > 0 ? "decimal place " + place : place === 0 ? "units place" : "place 10^" + -place;
    return [change.delta, precision].filter(Boolean).join(" · ");
  }

  function changesTable(changes) {
    const scroll = el("div", undefined, "scroll"), table = el("table");
    const head = table.createTHead().insertRow();
    ["Change", "Row", "Column / field", "Baseline", "Candidate", "Difference"].forEach(name => head.append(el("th", name)));
    const body = table.createTBody();
    for (const change of changes) {
      const tr = body.insertRow();
      const row = change.rowLabel || [change.oldRow, change.newRow].filter(x => x != null).join(" → ");
      const path = [change.column, change.path].filter(Boolean).join(" / ");
      [change.kind.replaceAll("_", " "), row, path, change.old, change.new, decimalText(change)].forEach(text => {
        tr.append(el("td", text || "—"));
      });
    }
    scroll.append(table);
    return scroll;
  }

  // Candidate entries retain their order. Put each deletion immediately
  // after its previous surviving baseline neighbor (or at the beginning).
  function insertRemovedEntries(oldLength, oldForNew) {
    const after = Array.from({length: oldForNew.length + 1}, () => []);
    const toNew = new Map(oldForNew.map((i, j) => [i, j]));
    let neighbor = 0;
    for (let i = 0; i < oldLength; i++) {
      if (toNew.has(i)) neighbor = toNew.get(i) + 1;
      else after[neighbor].push({i, j: null});
    }
    const entries = after[0].slice();
    oldForNew.forEach((i, j) => { entries.push({i, j}, ...after[j + 1]); });
    return entries;
  }

  function alignTableEntries(old, current, keys) {
    const used = new Set(), matches = current.map(() => null);
    for (const key of keys) {
      const available = new Map();
      old.forEach((entry, i) => {
        if (used.has(i)) return;
        const id = key(entry);
        if (!available.has(id)) available.set(id, []);
        available.get(id).push(i);
      });
      current.forEach((entry, j) => {
        if (matches[j] != null) return;
        const bucket = available.get(key(entry));
        if (bucket?.length) { matches[j] = bucket.shift(); used.add(matches[j]); }
      });
    }
    return insertRemovedEntries(old.length, matches);
  }

  function markTableNumber(cell, before, after) {
    // A change below the displayed precision still needs a highlighted cell.
    cell.classList.add("changed-value");
    if (before === after) { cell.append(after); return; }
    const scientific = /([eE](?=[+-]?\d)|×\s*10)/;
    const [a, , oldExponent = ""] = before.split(scientific);
    const [b, separator = "", exponent = ""] = after.split(scientific);
    const point = text => text.match(/^[^\d.]*[\d,]*/)[0].length;
    function markDigits(text, old, offset) {
      for (let i = 0; i < text.length; i++) {
        const digit = text[i];
        if (/[\d⁰¹²³⁴⁵⁶⁷⁸⁹]/.test(digit) && digit !== old[i + offset])
          cell.append(el("span", digit, "changed-digit"));
        else cell.append(digit);
      }
    }
    markDigits(b, a, point(a) - point(b));
    cell.append(separator);
    markDigits(exponent, oldExponent, oldExponent.length - exponent.length);
  }

  function tableFootnotes(target, baseline, candidate, compare) {
    const old = baseline?.footnotes || [], notes = candidate?.footnotes || [];
    if (!old.length && !notes.length) return;
    const noteKey = n => JSON.stringify([n?.symbol, n?.text ?? n, n?.cols, n?.rows]);
    const anchorKey = n => JSON.stringify([n?.symbol, n?.cols, n?.rows]);
    const text = n => plainText(n?.text ?? n);
    const entries = compare ? alignTableEntries(old, notes, [noteKey, anchorKey]) : notes.map((_, j) => ({i: null, j}));
    const list = el("div", undefined, "table-footnotes");
    for (const {i, j} of entries) {
      const removed = j == null, added = compare && i == null, note = removed ? old[i] : notes[j];
      const line = el("p", undefined, "footnote");
      if (removed || added) line.classList.add(removed ? "removed" : "added");
      line.append(el("sup", noteSymbol(note, removed ? i : j)), " ");
      if (compare && i != null && j != null && noteKey(old[i]) !== noteKey(notes[j])) {
        line.append(el("del", text(old[i]), "removed"), " ", el("span", text(notes[j]), "changed"));
      } else line.append(text(note));
      list.append(line);
    }
    target.append(list);
  }

  function showTable(item, mode, current, target) {
    const compare = mode === "diff" || mode === "precision";
    const previous = compare ? item.old : null;
    let display = [], oldDisplay = [];
    if (mode !== "precision" && mode !== "raw") {
      try {
        display = window.snapshotTableDisplay(current);
        oldDisplay = window.snapshotTableDisplay(previous);
      } catch (error) {
        display = []; oldDisplay = [];
        target.append(el("p", "JASP could not format this table. Showing full-precision values. " + error.message, "caption"));
      }
    }
    const oldFields = previous?.schema?.fields || [], fields = current?.schema?.fields || [];
    const oldData = previous?.data || [], data = current?.data || [];
    const changes = compare ? item.diff?.changes || [] : [];
    const columns = compare ? alignTableEntries(oldFields, fields, [field => field.name]) :
      fields.map((_, j) => ({i: null, j}));
    const rows = compare ? item.diff.rows.map(({oldRow, newRow}) => ({
      i: oldRow == null ? null : oldRow - 1, j: newRow == null ? null : newRow - 1
    })) : data.map((_, j) => ({i: null, j}));
    const cellChanges = new Map(), columnChanges = new Set(), rowChanges = new Set();
    for (const change of changes) {
      if (change.kind === "column_metadata") columnChanges.add(change.column);
      if (change.kind === "row_metadata") rowChanges.add(change.newRow);
      if (change.kind === "cell") {
        const key = JSON.stringify([change.newRow, change.column]);
        if (!cellChanges.has(key)) cellChanges.set(key, []);
        cellChanges.get(key).push(change);
      }
    }

    function headerCell(col) {
      const field = col.j == null ? oldFields[col.i] : fields[col.j];
      const type = !compare ? "" : col.j == null ? "removed" : col.i == null ? "added" :
        columnChanges.has(field.name) ? "changed" : "";
      const label = (field.overTitle ? plainText(field.overTitle) + " / " : "") + plainText(field.title ?? field.name);
      const cell = el("th", label, type);
      cell.title = field.name;
      noteRefs(cell, field[".footnotes"], col.j == null ? previous : current);
      return cell;
    }

    const fieldValue = (record, name) => record != null && Object.hasOwn(record, name) ? record[name] : undefined;
    const payload = raw => raw != null && typeof raw === "object" && Object.hasOwn(raw, "value") ? raw.value : raw;
    function dataCell(row, col, tag = "td") {
      const removed = compare && (row.j == null || col.j == null);
      const added = compare && !removed && (row.i == null || col.i == null);
      const snapshot = removed ? previous : current;
      const record = removed ? oldData[row.i] : data[row.j];
      const field = removed ? oldFields[col.i] : fields[col.j];
      const cell = el(tag, undefined, removed ? "removed" : added ? "added" : "");
      // New-only columns have no value at their intersection with deleted rows.
      if (!record || !field) return cell;
      const name = field.name;
      const raw = fieldValue(record, name), value = payload(raw);
      const shown = fieldValue(removed ? oldDisplay[row.i] : display[row.j], name) ?? valueText(value);
      const paired = compare && !removed && !added;
      const before = paired ? fieldValue(oldDisplay[row.i], name) : undefined;
      const displayChanged = before != null && before !== shown;
      const changesHere = paired ? cellChanges.get(JSON.stringify([row.j + 1, name])) : null;
      const oldValue = paired ? payload(fieldValue(oldData[row.i], name)) : undefined;
      if (typeof value === "number" && typeof oldValue === "number" && (displayChanged || !Object.is(oldValue, value))) {
        markTableNumber(cell, before ?? valueText(oldValue), shown);
      } else {
        cell.append(shown);
        if (changesHere || displayChanged) cell.classList.add("changed");
      }
      if (changesHere) cell.title = changesHere.map(c => [c.path, c.old + " → " + c.new, decimalText(c)].filter(Boolean).join(" · ")).join("\n");
      else if (value !== raw) cell.title = valueText(raw);
      noteRefs(cell, fieldValue(record[".footnotes"], name), snapshot);
      return cell;
    }

    if (columns.length) {
      const table = el("table", undefined, "snapshot-table"), head = table.createTHead(), body = table.createTBody();
      if (current.casesAcrossColumns) {
        const titleRows = current.overTitle && columns.length > 1 ? 2 : 1;
        for (let k = 0; k < titleRows; k++) {
          const tr = head.insertRow(); tr.append(headerCell(columns[k]));
          for (const row of rows) tr.append(dataCell(row, columns[k], "th"));
        }
        for (const col of columns.slice(titleRows)) {
          const tr = body.insertRow(); tr.append(headerCell(col));
          for (const row of rows) tr.append(dataCell(row, col));
        }
      } else {
        const headRow = head.insertRow();
        for (const col of columns) headRow.append(headerCell(col));
        for (const row of rows) {
          const tr = body.insertRow(), record = row.j == null ? oldData[row.i] : data[row.j];
          if (record?.[".isNewGroup"]) tr.classList.add("group-start");
          if (record?.[".isNewSubGroup"]) tr.classList.add("subgroup-start");
          for (const col of columns) tr.append(dataCell(row, col));
          if (compare && row.j != null && rowChanges.has(row.j + 1) && tr.firstElementChild)
            tr.firstElementChild.classList.add("changed");
        }
      }
      const scroll = el("div", undefined, "scroll");
      scroll.append(table); target.append(scroll);
    }
    const error = current?.error;
    if (error) target.append(el("p", error.errorMessage ?? valueText(error), "table-error"));
    tableFootnotes(target, previous, current, compare);
    if (changes.length) {
      const details = el("details");
      details.append(el("summary", "Change details (" + changes.length + ")"), changesTable(changes));
      byId("preview-details").append(details);
    }
  }

  async function showFigure(item, view, target) {
    function figureImage(url, label) {
      const image = el("img");
      image.alt = label + " figure";
      image.src = url;
      return image;
    }
    if (view !== "diff") {
      target.append(figureImage(view === "candidate" ? item.new : item.old, modeLabels[view]));
      return;
    }
    const images = [figureImage(item.old, "Baseline"), figureImage(item.new, "Candidate")];
    const canvas = el("canvas"), caption = el("p", "Comparing figures…", "caption");
    canvas.setAttribute("aria-label", "Visual differences between the candidate and baseline figures");
    target.append(canvas, caption);
    try {
      await Promise.all(images.map(image => image.decode()));
      if (!canvas.isConnected) return;
      const maxW = Math.max(...images.map(image => image.naturalWidth));
      const maxH = Math.max(...images.map(image => image.naturalHeight));
      const scale = Math.min(1, 1600 / Math.max(maxW, maxH));
      canvas.width = Math.max(1, Math.round(maxW * scale)); canvas.height = Math.max(1, Math.round(maxH * scale));
      const ctx = canvas.getContext("2d", {willReadFrequently: true});
      const frames = images.map(image => {
        ctx.fillStyle = "white"; ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.drawImage(image, 0, 0, image.naturalWidth * scale, image.naturalHeight * scale);
        return ctx.getImageData(0, 0, canvas.width, canvas.height);
      });
      const old = frames[0].data, updated = frames[1].data;
      const luminance = (pixels, i) => pixels[i] * .299 + pixels[i + 1] * .587 + pixels[i + 2] * .114;
      let count = 0;
      for (let i = 0; i < old.length; i += 4) {
        if (old[i] === updated[i] && old[i + 1] === updated[i + 1] && old[i + 2] === updated[i + 2]) {
          for (let j = i; j < i + 3; j++) updated[j] = 170 + updated[j] / 3;
        } else {
          count++;
          updated.set(luminance(updated, i) <= luminance(old, i) ? [190, 49, 63] : [29, 108, 168], i);
        }
      }
      ctx.putImageData(frames[1], 0, 0);
      caption.textContent = (100 * count / (canvas.width * canvas.height)).toFixed(2) +
        "% of rendered pixels differ. Red marks darker candidate pixels. Blue marks lighter candidate pixels.";
    } catch {
      caption.textContent = "The overlay could not be rendered. Click to inspect the candidate and baseline.";
    }
  }

  // Parse in an inert template. Rebuild the static elements before placing the
  // fragment in its sandbox. Resource and navigation attributes stay out.
  function staticHtml(source) {
    const input = el("template"), output = el("div");
    input.innerHTML = source;
    const safe = new Set(("a abbr address article aside b bdi bdo blockquote br caption cite code col colgroup dd del details " +
      "dfn div dl dt em figcaption figure footer h1 h2 h3 h4 h5 h6 header hr i img ins kbd li main mark ol p pre q s samp " +
      "section small span strong style sub summary sup table tbody td tfoot th thead time tr u ul var wbr").split(" "));
    const omit = new Set(["script", "noscript", "template", "iframe", "frame", "object", "embed", "title"]);
    const attrs = new Set(["class", "id", "dir", "lang", "style", "title", "colspan", "rowspan", "span", "scope", "alt",
      "width", "height", "align", "valign", "border", "cellpadding", "cellspacing", "start", "type", "open"]);
    function copy(parent, node) {
      if (node.nodeType === Node.TEXT_NODE) { parent.append(node.textContent); return; }
      const tag = node.localName;
      if (!tag || omit.has(tag)) return;
      let container = parent;
      if (safe.has(tag)) {
        container = el(tag === "a" ? "span" : tag);
        for (const attr of node.attributes) {
          if (attrs.has(attr.name) || (tag === "img" && attr.name === "src" &&
            /^data:image\/(?:png|gif|jpeg|webp);base64,/i.test(attr.value))) {
            container.setAttribute(attr.name, attr.value);
          }
        }
        parent.append(container);
      }
      for (const child of node.childNodes) copy(container, child);
    }
    for (const child of input.content.childNodes) copy(output, child);
    return output;
  }

  // Find matching subsequences at both levels: sibling elements, then words
  // within changed text. Prefer exact matches and use common words to align
  // edited paragraphs when other same-tag paragraphs were inserted or removed.
  function htmlMatches(a, b, key, similarity) {
    let start = 0, endA = a.length, endB = b.length;
    const pairs = [];
    while (start < endA && start < endB && key(a[start]) === key(b[start])) {
      pairs.push([start, start]); start++;
    }
    while (endA > start && endB > start && key(a[endA - 1]) === key(b[endB - 1])) { endA--; endB--; }
    const width = endB - start + 1;
    if ((endA - start + 1) * width <= 300000) {
      const scores = new Float64Array((endA - start + 1) * width);
      const choices = new Uint8Array(scores.length);
      for (let i = endA - 1; i >= start; i--) {
        for (let j = endB - 1; j >= start; j--) {
          const at = (i - start) * width + j - start;
          const match = similarity(a[i], b[j]);
          const take = match > 0 ? match + scores[at + width + 1] : -1;
          scores[at] = Math.max(take, scores[at + width], scores[at + 1]);
          choices[at] = scores[at] === take ? 1 : scores[at] === scores[at + width] ? 2 : 3;
        }
      }
      let i = start, j = start;
      while (i < endA && j < endB) {
        const choice = choices[(i - start) * width + j - start];
        if (choice === 1) { pairs.push([i, j]); i++; j++; }
        else if (choice === 2) i++;
        else j++;
      }
    } else {
      // Bound memory for long preformatted output. Index successive exact
      // matches rather than allocating the full quadratic alignment matrix.
      const positions = new Map();
      for (let j = endB - 1; j >= start; j--) {
        const value = key(b[j]);
        if (!positions.has(value)) positions.set(value, []);
        positions.get(value).push(j);
      }
      let next = start;
      for (let i = start; i < endA; i++) {
        const hits = positions.get(key(a[i]));
        while (hits?.length && hits.at(-1) < next) hits.pop();
        if (hits?.length) { next = hits.pop() + 1; pairs.push([i, next - 1]); }
      }
    }
    for (let i = endA, j = endB; i < a.length; i++, j++) pairs.push([i, j]);
    return pairs;
  }

  function htmlChange(node, change) {
    const marked = node.nodeType === Node.TEXT_NODE ? el("span", node.data) : node.cloneNode(true);
    marked.dataset.reviewEdit = change;
    return marked;
  }

  function htmlTextDiff(before, after) {
    const tokens = value => value.match(/[\p{L}\p{N}_]+|\s+|[^\p{L}\p{N}_\s]/gu) || [];
    const a = tokens(before), b = tokens(after), fragment = document.createDocumentFragment();
    const pairs = htmlMatches(a, b, token => token, (x, y) => x === y ? (/\S/u.test(x) ? 4 : 1) : 0);
    let oldAt = 0, newAt = 0;
    for (const [i, j] of pairs.concat([[a.length, b.length]])) {
      const old = a.slice(oldAt, i).join(""), updated = b.slice(newAt, j).join("");
      let prefix = 0, suffix = 0, limit = Math.min(old.length, updated.length);
      while (prefix < limit && old[prefix] === updated[prefix]) prefix++;
      while (suffix < limit - prefix && old[old.length - 1 - suffix] === updated[updated.length - 1 - suffix]) suffix++;
      fragment.append(old.slice(0, prefix));
      const removed = old.slice(prefix, old.length - suffix), inserted = updated.slice(prefix, updated.length - suffix);
      if (removed) fragment.append(htmlChange(document.createTextNode(removed), "removed"));
      if (inserted) fragment.append(htmlChange(document.createTextNode(inserted), removed ? "replaced" : "added"));
      fragment.append(old.slice(old.length - suffix), b[j] || "");
      oldAt = i + 1; newAt = j + 1;
    }
    return fragment;
  }

  function htmlChildrenDiff(parent, before, after) {
    const describe = node => ({
      node, key: node.outerHTML ?? "#" + node.data,
      words: new Set((node.textContent.slice(0, 500).toLowerCase().match(/[\p{L}\p{N}_]+/gu) || []))
    });
    const a = Array.from(before.childNodes, describe), b = Array.from(after.childNodes, describe);
    const pairs = htmlMatches(a, b, x => x.key, (x, y) => {
      if (x.node.nodeName !== y.node.nodeName) return 0;
      if (x.key === y.key) return x.node.textContent.trim() || x.node.nodeType === Node.ELEMENT_NODE ? 12 : .1;
      let common = 0;
      for (const word of x.words) if (y.words.has(word)) common++;
      return .2 + 4 * common / Math.max(1, x.words.size + y.words.size);
    });
    let oldAt = 0, newAt = 0;
    for (const [i, j] of pairs.concat([[a.length, b.length]])) {
      const removed = a.slice(oldAt, i), inserted = b.slice(newAt, j);
      removed.forEach(x => parent.append(htmlChange(x.node, "removed")));
      inserted.forEach((x, index) => parent.append(htmlChange(x.node, index < removed.length ? "replaced" : "added")));
      if (i < a.length) {
        const old = a[i].node, updated = b[j].node;
        if (a[i].key === b[j].key) parent.append(updated.cloneNode(true));
        else if (old.nodeType === Node.TEXT_NODE) parent.append(htmlTextDiff(old.data, updated.data));
        else {
          const child = updated.cloneNode(false);
          if (!old.cloneNode(false).isEqualNode(child)) child.dataset.reviewEdit = "replaced";
          htmlChildrenDiff(child, old, updated);
          parent.append(child);
        }
      }
      oldAt = i + 1; newAt = j + 1;
    }
  }

  function showHtml(item, mode, target) {
    // Styles affect rendering but have no visible text to mark up. Use the
    // chosen side's CSS and retain both original sources in the disclosure.
    const chosen = staticHtml(mode === "baseline" ? item.old : item.new);
    const styles = [...chosen.querySelectorAll("style")];
    styles.forEach(node => node.remove());
    let content = chosen;
    if (mode === "diff") {
      const old = staticHtml(item.old);
      old.querySelectorAll("style").forEach(node => node.remove());
      content = el("div");
      htmlChildrenDiff(content, old, chosen);
    }
    content.dataset.reviewDocument = "";
    const frame = el("iframe");
    frame.title = (mode === "diff" ? "Changes" : mode === "candidate" ? "Candidate" : "Baseline") + " in HTML output";
    // The parent measures the document after layout. Scripts and outbound
    // requests remain disabled, and the preview contains only static markup.
    frame.setAttribute("sandbox", "allow-same-origin");
    frame.setAttribute("tabindex", "-1");
    frame.referrerPolicy = "no-referrer";
    frame.addEventListener("load", () => {
      const doc = frame.contentDocument;
      if (!doc?.body) return;
      doc.head.lastElementChild.before(...styles);
      doc.body.append(content);
      const size = () => { frame.style.height = Math.ceil(Math.max(1, content.scrollHeight, content.getBoundingClientRect().height)) + "px"; };
      size();
      new ResizeObserver(size).observe(content);
    }, {once: true});
    frame.srcdoc = '<!doctype html><html><head><meta charset="utf-8"><meta http-equiv="Content-Security-Policy" ' +
      'content="default-src \'none\'; style-src \'unsafe-inline\'; img-src data:; form-action \'none\'; base-uri \'none\'">' +
      '<style>body{font:15px/1.45 system-ui,sans-serif}img{max-width:100%}table{border-collapse:collapse}</style>' +
      '<style>html,body{margin:0!important;height:auto!important;min-height:0!important;overflow:hidden!important}' +
      '[data-review-document]{display:flow-root;padding:12px;overflow-wrap:anywhere}' +
      '[data-review-edit="added"]{background:#def4e4!important;color:#145739!important}' +
      '[data-review-edit="removed"],[data-review-edit="replaced"]{background:#fbe4e1!important;color:#922c31!important}' +
      '[data-review-edit="removed"]{text-decoration:line-through!important}</style></head><body></body></html>';
    target.append(frame);
    const source = el("details");
    source.append(el("summary", "HTML source"));
    for (const [side, text] of [["Baseline", item.old], ["Candidate", item.new]]) {
      if (text != null) source.append(el("h3", side), el("pre", text));
    }
    byId("preview-details").append(source);
  }

  function move(step) {
    const list = visible(), i = list.findIndex(item => item.id === current);
    if (list[i + step]) { current = list[i + step].id; draw(); }
  }

  function decide(action) {
    const list = visible(), i = list.findIndex(item => item.id === current);
    if (i < 0 || list[i].candidateHash == null) return;
    const previous = decisions.get(current), comment = byId("comment").value;
    if (action === "comment" && !comment.trim()) return;
    if (previous?.action !== action || previous?.comment !== comment) {
      history.push({id: current, previous});
      decisions.set(current, {action, comment});
    }
    drafts.delete(current);
    const next = list.slice(i + 1).concat(list.slice(0, i)).find(item => status(item) === "pending");
    if (next) current = next.id;
    draw();
  }

  function undo() {
    const last = history.pop();
    if (!last) return;
    if (last.previous) {
      decisions.set(last.id, last.previous);
      drafts.delete(last.id);
    } else {
      drafts.set(last.id, decisions.get(last.id).comment);
      decisions.delete(last.id);
    }
    current = last.id;
    if (!visible().some(item => item.id === current)) {
      byId("filter").value = "all";
      byId("search").value = "";
    }
    draw();
  }

  function exportDecisions() {
    const reviewed = items.filter(item => decisions.has(item.id));
    const entries = reviewed.map(item => ({
      id: item.id, baselineHash: item.baselineHash, candidateHash: item.candidateHash,
      structureBaselineHash: item.structureBaselineHash ?? null, structureCandidateHash: item.structureCandidateHash ?? null,
      ...(item.removeStructure && decisions.get(item.id).action === "accept" ? {removeStructure: true} : {}),
      ...decisions.get(item.id),
      ...(item.diff ? {summary: item.diff.summary, changes: item.diff.changes} : {})
    }));
    const version = entries.some(entry => entry.removeStructure) ? 2 : 1;
    const blob = new Blob([JSON.stringify({version, decisions: entries}, null, 2)], {type: "application/json"});
    const url = URL.createObjectURL(blob), link = el("a");
    link.href = url; link.download = "snapshot-decisions.json";
    document.body.append(link); link.click(); link.remove();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
    byId("message").textContent = "Exported " + entries.length + " decision(s). Pass snapshot-decisions.json to the agent running applySnapshotReview().";
  }

  byId("search").oninput = draw;
  byId("filter").onchange = draw;
  byId("previous").onclick = () => move(-1);
  byId("next").onclick = () => move(1);
  byId("cycle").onclick = cyclePreview;
  byId("preview").onclick = () => { if (!String(window.getSelection())) cyclePreview(); };
  byId("accept").onclick = () => decide("accept");
  byId("decline").onclick = () => decide("decline");
  byId("comment-only").onclick = () => decide("comment");
  byId("undo").onclick = undo;
  byId("export").onclick = exportDecisions;
  byId("comment").oninput = event => {
    drafts.set(current, event.target.value);
    byId("comment-only").disabled = !event.target.value.trim();
  };
  document.addEventListener("keydown", event => {
    if (event.altKey || event.metaKey || event.ctrlKey || event.target.matches("input, select, textarea") || event.repeat) return;
    switch (event.key.toLowerCase()) {
    case "arrowleft": move(-1); break;
    case "arrowright": move(1); break;
    case "a": decide("accept"); break;
    case "r": decide("decline"); break;
    case "t": cyclePreview(); break;
    case "u": undo(); break;
    default: return;
    }
    event.preventDefault();
  });
  draw();
})();
