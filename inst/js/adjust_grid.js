export function adjust_grid(ns) {
  // Function to get the maximum width of the first column across all grid-items
  function getMaxWidth() {
    const rows = document.querySelectorAll('.grid-items');
    let maxWidth = 0;

    console.log('Adjusting grid layout'); // Log to check if the function is executed

    rows.forEach((row, index) => {
      const firstColumn = row.querySelector('.item-a');
      if (firstColumn) {
        const width = firstColumn.scrollWidth;
        if (width > maxWidth) {
          maxWidth = width;
        }
      } else {
        console.warn(`First column not found in row ${index}`);
      }
    });

    console.log(`Max width: ${maxWidth}px`); // Log the maximum width found
    return maxWidth;
  }

  // Function to apply the maximum width to all grid-items with a constraint of 2fr
  function applyMaxWidth(maxWidth) {
    if (maxWidth === 0) {
      console.warn("Max width is 0px, no adjustments made.");
      return;
    }

    const container = document.querySelector('.grid-container-depth-0');
    const containerWidth = container ? container.clientWidth : 0;

    console.log(`Container width: ${containerWidth}px`);

    const sidebarCollapsed = document.querySelector("div[id$='-sidebar']").classList.contains('collapsed');
    const maxAllowableWidth = sidebarCollapsed ? containerWidth * 1 / 4 : containerWidth * 1 / 4;

    if (maxWidth > maxAllowableWidth) {
      maxWidth = Math.max(maxWidth, 100);
    } else {
      maxWidth = maxAllowableWidth;
    }

    console.log(`Adjusted max width (constrained to '2fr'}): ${maxWidth}px`);

    const rows = document.querySelectorAll('.grid-items');
    rows.forEach((row, index) => {
      if (row) {
        row.style.transition = 'all 0.5s ease';
        //row.style.gridTemplateColumns = `67px minmax(125px, 1fr) minmax(125px, 1fr) minmax(50px, auto)`; // ${maxWidth}px
      } else {
        console.warn(`Row ${index} is undefined`);
      }
    });
  }

  // Get the maximum width and apply it
  const maxWidth = getMaxWidth();
  applyMaxWidth(maxWidth);

  if (typeof Shiny !== 'undefined') {
    const inputValue = ns + '-adjust_grid_finished';
    Shiny.setInputValue(inputValue, true);
  }
}

    Shiny.addCustomMessageHandler('adjust_grid', function(ns) {
        const inputValue = ns + '-adjust_grid_finished';
        Shiny.setInputValue(inputValue, false);
        $(document).on('shiny:idle', function() {
          setTimeout(adjust_grid(ns), 100);
        });
    });
