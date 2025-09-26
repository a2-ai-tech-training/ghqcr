// Button click handler
$(document).on('click', 'button[id^="ghqc_status_app-modal_btn_"]', function() {
  Shiny.setInputValue(this.id, Math.random());
});

// Checkbox change handler for `show_qcer`
$(document).on('shiny:connected', function() {
  var checkbox = document.getElementById('show_qcer');
  if (checkbox) {
    Shiny.setInputValue('show_qcer', checkbox.checked);
    checkbox.addEventListener('change', function() {
      Shiny.setInputValue('show_qcer', checkbox.checked);
    });
  }
});

function triggerDefaultAction(id, action) {
  console.log("triggerDefaultAction called with:", id, action);

  let labelMap = {
    'Notify file changes': 'btn-info',
    'Approve': 'btn-success',
    'Repost last QC notification': 'btn-plum',
    'Notify last remote commit': 'btn-plum',
    'Unapprove (danger)': 'btn-danger',
    'Unapprove (light)': 'btn-light'
  };

  // Fallback to btn-default if not listed
  let btnClass = labelMap[action] || 'btn-default';

  // Strip variant from label: "Unapprove (danger)" => "Unapprove"
  let buttonLabel = action.replace(/\s?\(.*?\)$/, '');

  let btnMain = document.getElementById('main-btn-' + id);
  let btnCaret = document.getElementById('caret-btn-' + id);

  if (btnMain) {
    btnMain.innerText = buttonLabel;
    btnMain.className = 'btn btn-sm ' + btnClass;
  }

  if (btnCaret) {
    btnCaret.className = 'btn btn-sm dropdown-toggle ' + btnClass;
  }

  if (buttonLabel === 'Approve') {
    console.log("Setting input value:", ns_prefix + 'show_approve_modal_row');
    Shiny.setInputValue(ns_prefix + 'show_approve_modal_row', { row: parseInt(id), nonce: Math.random() });
  } else if (
    buttonLabel === 'Notify file changes' ||
    buttonLabel === 'Notify last remote commit' ||
    buttonLabel === 'Repost last QC notification'
  ) {
    Shiny.setInputValue(ns_prefix + 'show_notify_modal_row', { row: parseInt(id), action: buttonLabel, nonce: Math.random() });
  } else if (buttonLabel === 'Unapprove') {
    Shiny.setInputValue(ns_prefix + 'show_unapprove_modal_row', { row: parseInt(id), nonce: Math.random() });
  } else {
    Shiny.setInputValue(ns_prefix + 'action_' + id, buttonLabel, {priority: 'event'});
  }
}
