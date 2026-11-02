/**
 * The inbox of the comments of a BareBlog blog. It is bound to the sheet that holds them.
 * `bareblog comments connect` says how it is set up. The numbers below are written in by
 * BareBlog: change a number there, not here. What the script may do is named in
 * appsscript.json.
 */

var VERSION = {{version}};
var NAME_MAX = {{name-max}};
var TEXT_MAX = {{text-max}};
var OPEN_MIN_MS = {{open-min-ms}};
var WAITING_MAX = {{waiting-max}};
var NOTICE_GAP_MS = {{notice-gap-ms}};
var EMAIL = /^(?:{{email-pattern}})$/;
var FOLDER = /^(?:{{folder-pattern}})?$/;
// The shape of a UUID, as Google and BareBlog make them.
var ID = /^(?:[0-9a-f-]{36})?$/;

// One address for both: a reader's comment, and the author's actions with a secret.
function doPost(e) {
  try {
    var data = JSON.parse(e.postData.contents);
    return json(data.action ? authorAction(data) : comment(data));
  } catch (err) {
    return json({ ok: false, error: String(err) });
  }
}

// A trap is answered as a comment is, and nothing is written, so a bot is not told. A full
// inbox stops a flood at what the author can read.
function comment(data) {
  if (props().getProperty('open') !== 'yes') return { ok: false, error: 'comments are closed' };
  if (data.trap !== '' || typeof data.open !== 'number' || data.open < OPEN_MIN_MS ||
      !addressed(data)) {
    return { ok: true, id: Utilities.getUuid() };
  }
  if (sheet().getLastRow() >= WAITING_MAX) return { ok: false, error: 'the inbox is full' };
  if (!filled(data.name, NAME_MAX)) return { ok: false, error: 'a name of at most ' + NAME_MAX + ' characters is needed' };
  if (!filled(data.text, TEXT_MAX)) return { ok: false, error: 'a comment of at most ' + TEXT_MAX + ' characters is needed' };
  var id = Utilities.getUuid();
  sheet().appendRow([id, new Date().toISOString(), data.post || '', data.replyTo || '',
                     data.name, data.text].map(asText));
  notice(data.post || '');
  return { ok: true, id: id };
}

// The author is told by email that comments wait, at most once in a gap, and with nothing a
// reader wrote. A notice that cannot be sent is tried again with the next comment, and never
// refuses the comment.
function notice(post) {
  var last = Number(props().getProperty('noticed') || 0);
  if (props().getProperty('notify') !== 'yes' || Date.now() - last < NOTICE_GAP_MS) return;
  try {
    MailApp.sendEmail(props().getProperty('to'), 'A comment waits on your blog',
                      (post ? 'A new comment is on the post ' + post + '. ' : '') +
                      'Comments that wait: ' + sheet().getLastRow() + '.\n\n' +
                      'Read them with: bareblog comments');
    props().setProperty('noticed', String(Date.now()));
  } catch (err) {
    console.error(err);
  }
}

// A cell keeps what starts with a quote as the text after it, so that a comment such as +1 or
// =A1 is not read as a number or a formula.
function asText(s) {
  return "'" + s;
}

// A post and the comment it answers, as a page sends them.
function addressed(data) {
  return FOLDER.test(data.post || '') && ID.test(data.replyTo || '');
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
  if (data.action === 'notify') return notify(data);
  return { ok: false, version: VERSION, error: 'unknown action' };
}

function notify(data) {
  if (data.on === true) return notifyTo(data.to || '');
  if (data.on === false) props().setProperty('notify', 'no');
  return answer({});
}

// Notices go on only once Google took a test email to the address, so that the author learns of
// a wrong address while setting it.
function notifyTo(to) {
  if (!EMAIL.test(to)) {
    return { ok: false, version: VERSION, error: 'an address to email is needed' };
  }
  try {
    MailApp.sendEmail(to, 'Notices of your blog come here',
                      'BareBlog emails this address when a comment waits on your blog, at most ' +
                      'once an hour.\n\nTurn the notices off with: bareblog comments notify --off');
  } catch (err) {
    return { ok: false, version: VERSION,
             error: 'the test email could not be sent: ' + err.message };
  }
  props().setProperty('to', to);
  props().setProperty('notify', 'yes');
  return answer({});
}

function answer(o) {
  o.ok = true;
  o.version = VERSION;
  o.open = props().getProperty('open') === 'yes';
  o.notify = props().getProperty('notify') === 'yes';
  o.to = props().getProperty('to') || '';
  o.authorized = ScriptApp.getAuthorizationInfo(ScriptApp.AuthMode.FULL)
    .getAuthorizationStatus() === ScriptApp.AuthorizationStatus.NOT_REQUIRED;
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
