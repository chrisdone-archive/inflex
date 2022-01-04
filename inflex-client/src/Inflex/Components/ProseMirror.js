exports.proseMirror = function(parentElement){
  return function(){
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

      let startDoc = dinoSchema.nodeFromJSON({
        "type": "doc",
        "content":
        [
          {
            "type": "paragraph",
            "content": [
              {
                "type": "text",
                "text": "This is your cell-enabled editor! Tada: "
              },
              {
                "type": "dino",
                "attrs": {
                  "cell_uuid": "7190914a-9a54-477b-b5dc-da6b73edb7c9",
                  "type": "stegosaurus"
                }
              }
            ]
          }
        ]
        //   {
        //     "type": "paragraph",
        //     "content": [
        //       {
        //         "type": "text",
        //         "text": "This paragraph "
        //       },
        //       {
        //         "type": "dino",
        //         "attrs": {
        //           "type": "stegosaurus"
        //         }
        //       },
        //       {
        //         "type": "text",
        //         "text": ", for example, "
        //       },
        //       {
        //         "type": "dino",
        //         "attrs": {
        //           "type": "triceratops"
        //         }
        //       },
        //       {
        //         "type": "text",
        //         "text": " is full "
        //       },
        //       {
        //         "type": "dino",
        //         "attrs": {
        //           "type": "tyrannosaurus"
        //         }
        //       },
        //       {
        //         "type": "text",
        //         "text": " of dinosaurs."
        //       }
        //     ]
        //   },
        //   {
        //     "type": "paragraph",
        //     "content": [
        //       {
        //         "type": "text",
        //         "text": "Dinosaur nodes can be selected, copied, pasted, dragged, and so on."
        //       }
        //     ]
        //   }
        // ]
      });
      let state = prosemirrorState.EditorState.create({
        doc: startDoc,
        // Pass exampleSetup our schema and the menu we created
        plugins: prosemirrorExampleSetup.exampleSetup({schema: dinoSchema, menuContent: menu.fullMenu})
      });
      window.view = new prosemirrorView.EditorView(parentElement, {
        state: state,
        nodeViews: {
          dino: function(node){
            var n = document.createElement('span');
            n.innerText = node.attrs.cell_uuid;
            n.style.border = '1px solid black';
            var uuid = uuidv4();
            console.log('create: cell:%o ==> %o', node.attrs.cell_uuid, uuid);
            return {
              dom: n,
              destroy: function(){
                console.log('destroy: %o', uuid);
                console.log('parent: %o', n.parentNode);
                setTimeout(function(){
                  console.log('parent-after: %o', n.parentNode);
                }, 1000);
              }
            };
          }
        }
      });

      // }

    }(PM.model, PM.schema_basic, PM.menu, PM.example_setup, PM.state, PM.view));
  };
};
