exports.proseMirror = function(the_doc_json){
  return function(parentElement){
    return function(creator){
      return function(codeEmitter){
        return function(){
          // console.log('proseMirror: %o', the_doc_json);
          return (function (prosemirrorModel, prosemirrorSchemaBasic, prosemirrorMenu, prosemirrorExampleSetup, prosemirrorState, prosemirrorView) {
            'use strict';

            // nodespec{
            // The supported types of dinosaurs.
            var dinos = ["brontosaurus", "stegosaurus", "triceratops",
                         "tyrannosaurus", "pterodactyl"];

            var dinoNodeSpec = {
              // Dinosaurs have one attribute, their type, which must be one of
              // the types defined above.
              // Brontosaurs are still the default dino.
              attrs: {
                type: {default: "brontosaurus"},
                cell_uuid: {default: "UNPOPULATED CELL_UUID"}
              },
              inline: true,
              group: "inline",
              draggable: true,

              // These nodes are rendered as images with a `dino-type` attribute.
              // There are pictures for all dino types under /img/dino/.
              toDOM: function (node) { return ["img", {"dino-type": node.attrs.type,
                                                       src: "https://prosemirror.net/img/dino/" + node.attrs.type + ".png",
                                                       title: node.attrs.type,
                                                       class: "dinosaur"}]; },
              // When parsing, such an image, if its type matches one of the known
              // types, is converted to a dino node.
              parseDOM: [{
                tag: "img[dino-type]",
                getAttrs: function (dom) {
                  var type = dom.getAttribute("dino-type");
                  return dinos.indexOf(type) > -1 ? {type: type} : false
                }
              }]
            };

            var dinoSchema = new prosemirrorModel.Schema({
              nodes: prosemirrorSchemaBasic.schema.spec.nodes.addBefore("image", "dino", dinoNodeSpec),
              marks: prosemirrorSchemaBasic.schema.spec.marks
            });

            // }

            // command{
            var dinoType = dinoSchema.nodes.dino;

            function insertDino(type) {
              return function(state, dispatch) {
                var ref = state.selection;
                var $from = ref.$from;
                var index = $from.index();
                if (!$from.parent.canReplaceWith(index, index, dinoType))
                { return false }
                if (dispatch)
                {
                  dispatch(state.tr.replaceSelectionWith(dinoType.create({type: type}))); }
                return true
              }
            }

            function uuidv4() {
              return ([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g, c =>
                (c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16)
              );
            }

            // Ask example-setup to build its basic menu
            var menu = prosemirrorExampleSetup.buildMenuItems(dinoSchema);
            // Add a dino-inserting item for each type of dino
            dinos.forEach(function (name) { return menu.insertMenu.content.push(new prosemirrorMenu.MenuItem({
              title: "Insert " + name,
              label: name.charAt(0).toUpperCase() + name.slice(1),
              enable: function enable(state) { return insertDino(name)(state) },
              run: insertDino(name)
            })); });

            let startDoc = dinoSchema.nodeFromJSON(the_doc_json);
            let state = prosemirrorState.EditorState.create({
              doc: startDoc,
              // Pass exampleSetup our schema and the menu we created
              plugins: prosemirrorExampleSetup.exampleSetup({schema: dinoSchema, menuContent: menu.fullMenu})
            });
            let idleTimer = null;
            let previousCode = arrayToCode(startDoc.content.toJSON());
            let view = new prosemirrorView.EditorView(parentElement, {
              state: state,
              dispatchTransaction: function(transaction) {
                // Apply the update to the editor
                let newState = view.state.apply(transaction);
                view.updateState(newState);

                // Set an idle timer to emit the code
                let code = arrayToCode(transaction.doc.content.toJSON());
                if (code == previousCode) return;
                previousCode = code;
                clearTimeout(idleTimer);
                idleTimer = setTimeout(function(){
                  // console.log("ProseMirror.js: Idle timer triggered. Emitting code.", code);
                  codeEmitter.onCode(code)();
                }, 1000);
              },
              nodeViews: {
                dino: function(node){
                  var n = document.createElement('span');
                  var uuid = uuidv4();
                  // console.log('onCreate(%o, %o, %o)', n, uuid, node.attrs.cell_uuid);
                  creator.onCreate(uuid)(node.attrs.cell_uuid)(n)();

                  return {
                    dom: n,
                    destroy: function(){
                      console.error('TOOD: destroy()');
                    }
                  };
                }
              }
            });

            return {
              setInput: function(json){
                // console.log('ProseMirror.js: setInput(%o)', json);
                let code = arrayToCode(json.content);
                if (code == previousCode) {
                  // console.log('ProseMirror.js: ignoring, no change.');
                  return;
                }
                let startDoc = dinoSchema.nodeFromJSON(json);
                let newState = prosemirrorState.EditorState.create({
                  doc: startDoc,
                  // Pass exampleSetup our schema and the menu we created
                  plugins: prosemirrorExampleSetup.exampleSetup({schema: dinoSchema, menuContent: menu.fullMenu})
                });
                view.updateState(newState);
              }
            };
            // }

          }(PM.model, PM.schema_basic, PM.menu, PM.example_setup, PM.state, PM.view));
        };
      };
    };
  };

  function arrayToCode(array){
    let buffer = [];
    buffer.push('@prim:rich_doc([');
    for (var i = 0; i < array.length; i++) {
      if (i > 0) buffer.push(',');
      toCode(array[i], buffer);
    }
    buffer.push('])');
    return buffer.join("");
  }

  function toCode(tree, buffer){
    if (tree.type == 'paragraph') {
      buffer.push('@prim:rich_paragraph([');
      if (tree.content) {
        for (var i = 0; i < tree.content.length; i++) {
          if (i > 0) buffer.push(',');
          toCode(tree.content[i], buffer);
        }
      }
      buffer.push('])');
    } else if (tree.type == 'text') {
      if (tree.marks) {
        for (var i = 0; i < tree.marks.length; i++) {
          if (tree.marks[i].type == 'strong') {
            buffer.push('@prim:rich_bold(');
          } else if (tree.marks[i].type == 'em') {
            buffer.push('@prim:rich_italic(');
          }
        }
      }
      buffer.push('@prim:rich_text(');
      buffer.push('"' + tree.text.replace(/"/, '""') + '"');
      buffer.push(')');
      if (tree.marks) {
        for (var i = 0; i < tree.marks.length; i++) {
          if (tree.marks[i].type == 'strong' || tree.marks[i].type == 'em') {
            buffer.push(')');
          }
        }
      }
    } else if (tree.type == 'dino') {
      buffer.push('@prim:rich_cell(');
      buffer.push('@cell:uuid:' + tree.attrs.cell_uuid);
      buffer.push(')');
    } else {
      throw new Exception('tree type unknown: ' + tree.type);
    }
  }
};

exports.newCreator = function(){
  return {};
}

exports.setOnCreate = function(creator){
  return function(f){
    return function(){
      creator.onCreate = f;
    }
  }
}


exports.newCodeEmitter = function(){
  return {};
}

exports.setOnCode = function(codeEmitter){
  return function(f){
    return function(){
      codeEmitter.onCode = f;
    }
  }
}

exports.setProseMirrorInput = function(json){
  return function(thing){
    return function(){
      thing.setInput(json);
    }
  }
}
