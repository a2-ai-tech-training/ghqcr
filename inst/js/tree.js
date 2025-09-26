import { redrawTreeWithHighlights } from './highlight_files.js';

var Trees = {};
var Ids = {};
var Children = null;
var childrenFlag = true;
var lastNodeState = {};
var lastChildrenState = {};
var lastNodeIndex = {};

Shiny.addCustomMessageHandler("getChildren", function (x) {
  console.log("Received getChildren message:", x);
  Children = x;
  childrenFlag = true;
});

Shiny.addCustomMessageHandler('expandRoot', function(message) {
  var tree = $('#treeNavigator').jstree(true);
  if (tree) {
    // Expand the root node (node with ID '#')
    tree.open_node('#');
  }
});

Shiny.addCustomMessageHandler("noChildrenFound", function (x) {
  console.log("childrenFlag set to false.");
  childrenFlag = false;
});

var $navigator = $("div[id$='-treeNavigator']");
console.time("Initial tree rendering time");

$navigator.on("ready.jstree", function (e, data) {
  console.timeEnd("Initial tree rendering time");
  var id = e.target.id;
  Ids[id] = id.split("-")[0];
  Trees[id] = data.instance;
  var tree = Trees[id];
  var li_id = $("#" + id + ">ul>li").attr("id");
  tree.disable_checkbox(li_id);
  tree.disable_node(li_id);
});

$navigator.on("after_open.jstree", function (e, data) {
  var tree = Trees[e.target.id];
  tree.enable_checkbox(data.node);
  tree.enable_node(data.node);
});

$navigator.on("after_close.jstree", function (e, data) {
  var tree = Trees[e.target.id];
  tree.disable_checkbox(data.node);
  tree.disable_node(data.node);
});

$navigator.on("click", "li.jstree-x > i", function (e) {
  var $li = $(this).parent();
  if (!$li.hasClass("jstree-x")) {
    alert("That should not happen...");
    return;
  }

  var div_id = $li.closest("div").attr("id");
  var tree = Trees[div_id];
  var ns = Ids[div_id];
  var id = $li.attr("id");
  var node = tree.get_node(id);
  //console.log("Leaf node clicked with id:", id, "Node:", node);

  if (tree.is_leaf(node) && node.original.type === "folder") {
    var path = tree.get_path(node, "/");

    console.log("Fetching children for path:", path);

    Shiny.setInputValue(ns + "-path_from_js", path);

    // Introduce a delay to check if the flag changes
    // store node state, children state, and node index when clicking a node
    // if children is invalid (ie it contains only the excluded items), restore a node's
    // state within the tree and reset flags to "as before" state and stop further
    // node creation (it will disrupt tree state otherwise; explored reverting to tree state
    // instead of node states but ran into issues and i couldn't find a way to only reset/redraw
    // the portion of the tree/parent effected rather than the whole tree) and send a null value
    // to app to allow for reselection of invalid child because otherwise observeEvent
    // won't pick up the reselection as it is technically no change from prev selection
    // used delete/create node because re-enabling node state did not work and trying
    // to write the node state did not work
    var checkInterval = setInterval(function () {
      if (Children !== null || !childrenFlag) {
        lastNodeState[div_id] = tree.get_json(node);
        lastChildrenState[div_id] = Children;
        lastNodeIndex[div_id] = $("#" + id).index();
        clearInterval(checkInterval);
        if (!childrenFlag) {
          tree.delete_node(node);
          tree.create_node(node.parent, lastNodeState[div_id], lastNodeIndex[div_id]);
          Children = lastChildrenState[div_id];
          childrenFlag = true;
          Shiny.setInputValue(ns + "-path_from_js", null);
          return;
        }

        console.time("Node rendering time");
        console.log("Starting node rendering for:", Children.elem.length, "elements");
        for (var i = 0; i < Children.elem.length; i++) {
          var isdir = Children.folder[i];
          var newnode = tree.create_node(id, {
            text: Children.elem[i],
            type: isdir ? "folder" : "file",
            children: false,
            li_attr: isdir ? { class: "jstree-x" } : null
          });
          if (isdir) {
            tree.disable_checkbox(newnode);
            tree.disable_node(newnode);
          }
        }
        console.timeEnd("Node rendering time");
        Children = null;
        setTimeout(function () {
          tree.open_node(id);
          redrawTreeWithHighlights();
        }, 10);
      }
    }, 100);
  }
});

