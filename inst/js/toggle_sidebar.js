import { adjust_grid } from './adjust_grid.js';

$('button[id$="-toggle_sidebar"]').on('click', function() {
  var sidebar = $('div[id$="-sidebar"]');
  sidebar.toggleClass('collapsed');
  // Change the icon based on the sidebar state
  var icon = $(this).find('i'); // find icon
  if (sidebar.hasClass('collapsed')) {
    icon.removeClass('fas fa-angles-left').addClass('fas fa-angles-right');
  } else {
    icon.removeClass('fas fa-angles-right').addClass('fas fa-angles-left');
  }
  // call adjust_grid after toggle
  setTimeout(adjust_grid, 100)
});
