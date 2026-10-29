/**
 * The inbox of the comments of a BareBlog blog. It is bound to the sheet that holds them.
 * `bareblog comments connect` says how it is set up. The numbers below are written in by
 * BareBlog: change a number there, not here.
 * @OnlyCurrentDoc
 */

var VERSION = {{version}};
var NAME_MAX = {{name-max}};
var TEXT_MAX = {{text-max}};
var OPEN_MIN_MS = {{open-min-ms}};

// One address for both: a reader's comment, and the author's actions with a secret.
function doPost(e) {
  try {
    var data = JSON.parse(e.postData.contents);
    return json(data.action ? authorAction(data) : comment(data));
  } catch (err) {
    return json({ ok: false, error: String(err) });
  }
}

// A trap is answered as a comment is, and nothing is written, so a bot is not told.
function comment(data) {
  if (props().getProperty('open') !== 'yes') return { ok: false, error: 'comments are closed' };
  if (data.trap !== '' || typeof data.open !== 'number' || data.open < OPEN_MIN_MS) {
    return { ok: true, id: Utilities.getUuid() };
  }
  if (!filled(data.name, NAME_MAX)) return { ok: false, error: 'a name of at most ' + NAME_MAX + ' characters is needed' };
  if (!filled(data.text, TEXT_MAX)) return { ok: false, error: 'a comment of at most ' + TEXT_MAX + ' characters is needed' };
  var id = Utilities.getUuid();
  sheet().appendRow([id, new Date().toISOString(), String(data.post || ''),
                     String(data.replyTo || ''), data.name, data.text]);
  return { ok: true, id: id };
}

function filled(s, max) {
  return typeof s === 'string' && s.trim() !== '' && s.length <= max;
}

// The first secret the script is given becomes its secret. Later calls must send the same one.
function authorAction(data) {
  var secret = props().getProperty('secret');
  if (!secret && data.action === 'connect' && data.secret) {
    props().setProperty('secret', data.secret);
    return answer({ connected: true });
  }
  if (!secret || data.secret !== secret) return { ok: false, version: VERSION, error: 'wrong secret' };
  if (data.action === 'connect') return answer({ connected: true });
  if (data.action === 'rows') return answer({ rows: rows() });
  if (data.action === 'delete') return answer({ deleted: deleteRow(data.id) });
  if (data.action === 'state') {
    props().setProperty('open', data.open ? 'yes' : 'no');
    return answer({});
  }
  return { ok: false, version: VERSION, error: 'unknown action' };
}

function answer(o) {
  o.ok = true;
  o.version = VERSION;
  o.open = props().getProperty('open') === 'yes';
  return o;
}

function rows() {
  return sheet().getDataRange().getValues()
    .filter(function (r) { return r[0] !== ''; })
    .map(function (r) {
      return { id: r[0], received: r[1], post: r[2], replyTo: r[3], name: r[4], text: r[5] };
    });
}

function deleteRow(id) {
  var values = sheet().getDataRange().getValues();
  for (var i = values.length - 1; i >= 0; i--) {
    if (values[i][0] === id) { sheet().deleteRow(i + 1); return true; }
  }
  return false;
}

function props() {
  return PropertiesService.getScriptProperties();
}

function sheet() {
  return SpreadsheetApp.getActiveSpreadsheet().getSheets()[0];
}

function json(o) {
  return ContentService.createTextOutput(JSON.stringify(o))
    .setMimeType(ContentService.MimeType.JSON);
}
