// Couldn't just apply the color to the nodes themselves
// had to call to redraw the tree every time d/t way tree applied it
// now redraws when
// 1. new milestone/new path is selected/sent in
// 2. when a new node is created (ie when an unvisited dir is opened) - within tree.js
// 3. when a previously visited node is opened
export function redrawTreeWithHighlights() {
  console.log("Redrawing tree with highlights...");

  const treeId = $("div[id$='-treeNavigator']").attr("id");
  const treeInstance = $.jstree.reference("#" + treeId);
  const rootNode = treeInstance.get_node("#");

  function visitNode(node, parentPath) {
    if (!node || node.id === "#") {
      // If the node is the root or invalid, recursively visit its children
      if (node.children && node.children.length > 0) {
        node.children.forEach(function (childId) {
          const childNode = treeInstance.get_node(childId);
          visitNode(childNode, parentPath);
        });
      }
      return;
    }

    // Get the node text directly from the <a class="jstree-anchor">
    const nodeElement = $("#" + $.escapeSelector(node.id) + "_anchor");  // Target the anchor element
    const nodeText = $.trim(nodeElement.contents().filter(function() {
      return this.nodeType === 3; // NodeType 3 is the text node/needed specifically the text node
    }).text());

    // Construct the nodePath using parentPath and nodeText
    const nodePath = parentPath ? parentPath + "/" + nodeText : nodeText;

    applyHighlight(node, nodePath);

    // Recursively visit all children of the current node
    if (Array.isArray(node.children) && node.children.length > 0) {
      node.children.forEach(function (childId) {
        const childNode = treeInstance.get_node(childId);
        visitNode(childNode, nodePath);  // Pass down the updated nodePath
      });
    }
  }

  function applyHighlight(node, nodePath) {
    if (highlightPaths.includes(nodePath)) {
      console.log("Highlighting node:", node.id, "with path:", nodePath);
      const nodeElement = $("#" + $.escapeSelector(node.id) + "_anchor");
      nodeElement.css("color", "#999");
    } else {
      const nodeElement = $("#" + $.escapeSelector(node.id) + "_anchor");
      nodeElement.css("color", "");
    }
  }

  // Start the recursive node visiting and highlighting from the root node
  visitNode(rootNode, "");
}

let highlightPaths = [];

Shiny.addCustomMessageHandler("highlightPaths", function (x) {
  console.log("Received highlightPaths message:", x);

  // More than 1 chr in the vector auto converts to array but single string
  // doesn't so need to convert single string to an array
  if (Array.isArray(x)) {
    highlightPaths = x;
    console.log("highlightPaths set as an array:", highlightPaths);
  } else if (typeof x === 'string') {
    highlightPaths = [x];
    console.log("highlightPaths set as a single-item array:", highlightPaths);
  } else {
    console.error("Error: highlightPaths data is missing or not an array.");
    highlightPaths = [];
  }

  // Redraw the tree to apply highlights after setting the paths
  redrawTreeWithHighlights();
});

// call fxn again when opening any nodes as the highlighting doesn't persist/traverse
// previously opened nodes
$(document).ready(function () {
  const $navigator = $("div[id$='-treeNavigator']");

  $navigator.on("ready.jstree", function (e, data) {
    const treeId = e.target.id;
    const treeInstance = $.jstree.reference(`#${treeId}`);

    $(`#${treeId}`).on("after_open.jstree", function (e, data) {
     // console.log(`Node ${data.node.id} opened. Reapplying highlights.`);
      redrawTreeWithHighlights();
    });
  });
});
