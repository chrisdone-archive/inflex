{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
-- |

module Inflex.Server.View.Tour where

import           Data.Text (Text)
import           Lucid hiding (step_, section_)

tour :: Monad m => HtmlT m ()
tour = do
  section_
    "Familiar formulas like in spreadsheets"
    (do explain_
          (do p_
                "Cells are boxes that compute things, like arithmetic. You can click them to change the formula and hit Enter to see the result again."
              p_
                "Where's the grid? There is no grid. We use cells on an as-needed basis.")
        show_
          (do step_
                "Type in some arithmetic"
                (do cell_
                      "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"\"><input value=\"150+100\" class=\"form-control\" placeholder=\"Type code here\" style=\"width: 10ch;\"></div></div></div></div>"
                    cell_
                      "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">250</div></div></div></div></div>")))
  section_
    "Built-in records"
    (do explain_
          (do p_
                "Inflex natively understands records with built-in syntax. Records model the idea of a single thing, like a person or a stock item."
              p_
                "Records can be worked with either as code or as structured elements in the interface."
              p_
                "In a spreadsheet you couldn't express this in a deep way; only superficially with visual cues.")
        show_
          (do step_
                "Type in a record"
                (do cell_
                      "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"\" title=\"\"><input value='{name:\"Emily\",age:20}' class=\"form-control\" placeholder=\"Type code here\" style=\"width: 22ch;\"></div></div></div></div>"
                    cell_
                      "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap\" title=\"\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><table class=\"record\"><button class=\"wip-button\">Add field</button><tr class=\"record-field\"><td class=\"record-field-name\"><button class=\"wip-button\">-</button><div class=\"cell-name\" title=\"Click to edit field name\">name</div></td><td class=\"record-field-value\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><div class=\"text\"><div class=\"cell-name\" title=\"Click to edit text\">Emily</div></div></div></td></tr><tr class=\"record-field\"><td class=\"record-field-name\"><button class=\"wip-button\">-</button><div class=\"cell-name\" title=\"Click to edit field name\">age</div></td><td class=\"record-field-value\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">20</div></div></td></tr></table></div></div></div></div>")
              step_
                "Edit fields of the record"
                (do cell_
                      "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap\" title=\"\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><table class=\"record\"><button class=\"wip-button\">Add field</button><tr class=\"record-field\"><td class=\"record-field-name\"><button class=\"wip-button\">-</button><div class=\"cell-name\" title=\"Click to edit field name\">name</div></td><td class=\"record-field-value\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><div class=\"text\"><div class=\"\" title=\"\"><input class=\"form-control\" placeholder=\"Type text here\" value='Emily'></div></div></div></td></tr><tr class=\"record-field\"><td class=\"record-field-name\"><button class=\"wip-button\">-</button><div class=\"cell-name\" title=\"Click to edit field name\">age</div></td><td class=\"record-field-value\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">20</div></div></td></tr></table></div></div></div></div>")))
  section_
    "Lists"
    (do explain_
          (do p_
                "Lists are also built-in with special syntax, which can be edited either graphically or as a formula."
              p_
                "They contain one value inside, and each item in the list has the same type, so it's always safe to apply transformations or filters on them.")
        show_
          (do step_
                "Create a list cell"
                (cell_
                   "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><table class=\"array\"><tbody class=\"array-body\"><tr><td colspan=\"3\" class=\"array-empty\">↙ Hit the bottom-left button to add rows!</td></tr><tr><td class=\"add-row\"><button class=\"add-row-button \" title=\"Add row\">+</button></td><td class=\"bottom-blank\"></td></tr></tbody></table></div></div></div></div>")
              step_
                "Add items to the list"
                (cell_
                   "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><table class=\"array\"><tbody class=\"array-body\"><tr><td colspan=\"1\" class=\"row-number\">0</td><td class=\"array-datum-value\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">123</div></div></td></tr><tr><td class=\"row-number\">1</td><td class=\"array-datum-value\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">456</div></div></td></tr><tr><td class=\"row-number\">2</td><td class=\"array-datum-value\"><div class=\"\" title=\"\"><input class=\"form-control\" placeholder=\"Type code here\" style=\"width: 10ch;\"></div></td></tr><tr><td class=\"add-row\"><button class=\"add-row-button \" title=\"Add row\">+</button></td><td class=\"bottom-blank\"></td></tr></tbody></table></div></div></div></div>")
              step_
                "Edit list as code"
                (cell_
                   "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"\"><input class=\"form-control\" placeholder=\"Type code here\" style=\"width: 14ch;\" value=\"[123,456,789]\"></div></div></div></div>")))
  section_
    "Tables"
    (do explain_
          (do p_
                "We can create a table in the graphical interface piece by piece. Adding data to tables is as easy as adding data to lists."
              p_
                "We've seen records and lists above: In Inflex, tables are simply lists of records! That means \
                \you can re-use the same intuitions and concepts, a filter on a table is just a filter on a list.")
        show_
          (do step_
                "Create a table cell"
                (cell_
                   "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><table class=\"table\"><thead class=\"table-header\"><th class=\"table-column\" title=\"\"></th><th></th><th class=\"add-column\"><button class=\"add-column-button\" title=\"Add column to this table\">+</button></th></thead><tbody class=\"table-body\"><tr><td colspan=\"3\" class=\"table-empty\">Hit the top-right button to add columns! ↗</td></tr><tr><td class=\"add-row\"><button class=\"add-row-button disabled\" title=\"Add row\">+</button></td><td class=\"bottom-blank\" colspan=\"2\"></td></tr></tbody></table></div></div></div></div>")
              step_
                "Add a column"
                (cell_
                   "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><table class=\"table\"><thead class=\"table-header\"><th class=\"table-column\" title=\"\"></th><th class=\"table-column\" title=\"Click to edit\"><div class=\"table-column-content\"><div class=\"cell-name\" title=\"Click to edit column name\">column1</div><button class=\"remove-column-button\">×</button></div></th><th class=\"add-column\"><button class=\"add-column-button\" title=\"Add column to this table\">+</button></th></thead><tbody class=\"table-body\"><tr><td colspan=\"3\" class=\"table-empty\">↙ Hit the bottom-left button to add rows!</td></tr><tr><td class=\"add-row\"><button class=\"add-row-button \" title=\"Add row\">+</button></td><td class=\"bottom-blank\" colspan=\"2\"></td></tr></tbody></table></div></div></div></div>")
              step_
                "Add data"
                (cell_
                   "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><table class=\"table\"><thead class=\"table-header\"><th class=\"table-column\" title=\"\"></th><th class=\"table-column\" title=\"Click to edit\"><div class=\"table-column-content\"><div class=\"cell-name\" title=\"Click to edit column name\">column1</div><button class=\"remove-column-button\">×</button></div></th><th class=\"add-column\" title=\"\"><button class=\"add-column-button\" title=\"Add column to this table\">+</button></th></thead><tbody class=\"table-body\"><tr><td colspan=\"1\" class=\"row-number\">0</td><td class=\"table-datum-value\"><div class=\"\" title=\"\"><input value=\"100\" class=\"form-control\" placeholder=\"Type code here\" style=\"width: 10ch;\"></div></td><td class=\"add-column-blank\"></td></tr><tr><td class=\"add-row\"><button class=\"add-row-button \" title=\"Add row\">+</button></td><td class=\"bottom-blank\" colspan=\"2\"></td></tr></tbody></table></div></div></div></div>")
              step_
                "View as code"
                (cell_
                   "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"\"><input class=\"form-control\" placeholder=\"Type code here\" value='[{\"column1\": 100}]' style=\"width: 19ch;\"></div></div></div></div>")))
  section_
    "Functions"
    (do explain_
          (do p_
                "Functions let us specify computations to be done on some input. Here we show how the function n:n>300 is used as a condition to filter in only items in the list that match."
              p_ "")
        show_
          (do step_
                "Name your cell"
                (do cell_
                      "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"\" title=\"\"><input class=\"form-control\" placeholder=\"Type a name here\"></div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><table class=\"array\"><tbody class=\"array-body\"><tr><td colspan=\"1\" class=\"row-number\">0</td><td class=\"array-datum-value\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">123</div></div></td></tr><tr><td class=\"row-number\">1</td><td class=\"array-datum-value\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">456</div></div></td></tr><tr><td class=\"row-number\">2</td><td class=\"array-datum-value\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">789</div></div></td></tr><tr><td class=\"add-row\"><button class=\"add-row-button \" title=\"Add row\">+</button></td><td class=\"bottom-blank\"></td></tr></tbody></table></div></div></div></div>"
                    cell_
                      "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">numbers</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><table class=\"array\"><tbody class=\"array-body\"><tr><td colspan=\"1\" class=\"row-number\">0</td><td class=\"array-datum-value\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">123</div></div></td></tr><tr><td class=\"row-number\">1</td><td class=\"array-datum-value\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">456</div></div></td></tr><tr><td class=\"row-number\">2</td><td class=\"array-datum-value\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">789</div></div></td></tr><tr><td class=\"add-row\"><button class=\"add-row-button \" title=\"Add row\">+</button></td><td class=\"bottom-blank\"></td></tr></tbody></table></div></div></div></div>")
              step_
                "Make a cell that filters only numbers greater than 300"
                (do cell_
                      "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"\"><input value='filter(n:n>300,numbers)' class=\"form-control\" placeholder=\"Type code here\" style=\"width: 24ch;\"></div></div></div></div>"
                    cell_
                      "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><table class=\"array\"><tbody class=\"array-body\"><tr><td class=\"row-number\">0</td><td class=\"array-datum-value\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">456</div></div></td></tr><tr><td class=\"row-number\">1</td><td class=\"array-datum-value\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">789</div></div></td></tr><tr><td class=\"add-row\"><button class=\"add-row-button \" title=\"Add row\">+</button></td><td class=\"bottom-blank\"></td></tr></tbody></table></div></div></div></div>")))
  section_
    "Re-using functions"
    (do explain_
          (do p_
                "Functions are normal values like numbers or text, so they can be put in a cell and passed around."
              p_
                "Here we put the function in another cell and then use it in our original filter. This way, we\
                 \ separated concerns and gained re-use.")
        show_
          (do step_
                "Take the condition from the last example"
                (cell_
                   "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"\"><input value='filter(n:n>300,numbers)' class=\"form-control\" placeholder=\"Type code here\" style=\"width: 24ch;\"></div></div></div></div>")
              step_
                "Cut and paste it to its own cell"
                (cell_
                   "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">myfilter</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"\"><input value=\"n:n>300\" class=\"form-control\" placeholder=\"Type code here\" style=\"width: 10ch;\"></div></div></div></div>")
              step_
                "Use that cell in original formula"
                (do cell_
                      "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"\"><input value=\"filter(myfilter,numbers)\" class=\"form-control\" placeholder=\"Type code here\" style=\"width: 25ch;\"></div></div></div></div>"
                    cell_
                      "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">(unnamed)</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><table class=\"array\"><tbody class=\"array-body\"><tr><td class=\"row-number\">0</td><td class=\"array-datum-value\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">456</div></div></td></tr><tr><td class=\"row-number\">1</td><td class=\"array-datum-value\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">789</div></div></td></tr><tr><td class=\"add-row\"><button class=\"add-row-button \" title=\"Add row\">+</button></td><td class=\"bottom-blank\"></td></tr></tbody></table></div></div></div></div>")))
  section_
    "Type system to prevent mistakes"
    (do explain_
          (do p_
                "Inflex has a static type system, which means that any attempt to combine the wrong types of data together will stop you and loudly complain."
              p_
                "This helps prevent correctness errors and bugs at the time of writing rather than later on at the time of inspecting results.")
        show_
          (do step_
                "Accidentally add two incompatible types of things"
                (do cell_
                      "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">telephone</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><div class=\"text\"><div class=\"cell-name\" title=\"Click to edit text\">07123123</div></div></div></div></div></div>"
                    div_
                      (cell_
                         "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">stock</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">42</div></div></div></div></div>")
                    cell_
                      "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">mistake</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"\"><input value=\"telephone+stock\" class=\"form-control\" placeholder=\"Type code here\" style=\"width: 16ch;\"></div></div></div></div>"
                    cell_
                      "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">mistake</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><div class=\"error-message\">types of values don't match up</div></div></div></div></div>")))
  blanks
  nested

nested :: Monad m => HtmlT m ()
nested =
  section_
    "Nest data inside other data freely"
    (do explain_
          (do p_
                "Not all data fits neatly, or correctly, into a grid, or a table. \
                \In Inflex, you can put any type of value anywhere else."
              p_ "If you want a table inside a table, go for it!")
        show_
          (step_
             "Each recipe has a list of ingredients"
             (do cell_
                   "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">recipes</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><table class=\"table\"><thead class=\"table-header\"><th class=\"table-column\" title=\"\"></th><th class=\"table-column\" title=\"Click to edit\"><div class=\"table-column-content\"><div class=\"cell-name\" title=\"Click to edit column name\">name</div><button class=\"remove-column-button\">×</button></div></th><th class=\"table-column\" title=\"Click to edit\"><div class=\"table-column-content\"><div class=\"cell-name\" title=\"Click to edit column name\">ingredients</div><button class=\"remove-column-button\">×</button></div></th><th class=\"add-column\"><button class=\"add-column-button\" title=\"Add column to this table\">+</button></th></thead><tbody class=\"table-body\"><tr><td class=\"row-number\">0</td><td class=\"table-datum-value\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><div class=\"text\"><div class=\"cell-name\" title=\"Click to edit text\">cake</div></div></div></td><td class=\"table-datum-value\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><table class=\"table\"><thead class=\"table-header\"><th class=\"table-column\" title=\"\"></th><th class=\"table-column\" title=\"Click to edit\"><div class=\"table-column-content\"><div class=\"cell-name\" title=\"Click to edit column name\">name</div><button class=\"remove-column-button\">×</button></div></th><th class=\"table-column\" title=\"Click to edit\"><div class=\"table-column-content\"><div class=\"cell-name\" title=\"Click to edit column name\">amount</div><button class=\"remove-column-button\">×</button></div></th><th class=\"add-column\"><button class=\"add-column-button\" title=\"Add column to this table\">+</button></th></thead><tbody class=\"table-body\"><tr><td class=\"row-number\">0</td><td class=\"table-datum-value\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><div class=\"text\"><div class=\"cell-name\" title=\"Click to edit text\">eggs</div></div></div></td><td class=\"table-datum-value\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><div class=\"variant\"><div class=\"variant-tag\">#unit</div><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">2</div></div></div></div></td><td class=\"add-column-blank\"></td></tr><tr><td class=\"add-row\"><button class=\"add-row-button \" title=\"Add row\">+</button></td><td class=\"bottom-blank\" colspan=\"3\"></td></tr></tbody></table></div></td><td class=\"add-column-blank\"></td></tr><tr><td class=\"row-number\">1</td><td class=\"table-datum-value\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><div class=\"text\"><div class=\"cell-name\" title=\"Click to edit text\">donuts</div></div></div></td><td class=\"table-datum-value\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><table class=\"table\"><thead class=\"table-header\"><th class=\"table-column\" title=\"\"></th><th class=\"table-column\" title=\"Click to edit\"><div class=\"table-column-content\"><div class=\"cell-name\" title=\"Click to edit column name\">name</div><button class=\"remove-column-button\">×</button></div></th><th class=\"table-column\" title=\"Click to edit\"><div class=\"table-column-content\"><div class=\"cell-name\" title=\"Click to edit column name\">amount</div><button class=\"remove-column-button\">×</button></div></th><th class=\"add-column\"><button class=\"add-column-button\" title=\"Add column to this table\">+</button></th></thead><tbody class=\"table-body\"><tr><td class=\"row-number\">0</td><td class=\"table-datum-value\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><div class=\"text\"><div class=\"cell-name\" title=\"Click to edit text\">eggs</div></div></div></td><td class=\"table-datum-value\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><div class=\"variant\"><div class=\"variant-tag\">#unit</div><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">1</div></div></div></div></td><td class=\"add-column-blank\"></td></tr><tr><td class=\"row-number\">1</td><td class=\"table-datum-value\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><div class=\"text\"><div class=\"cell-name\" title=\"Click to edit text\">butter</div></div></div></td><td class=\"table-datum-value\"><div class=\"editor-boundary-wrap\"><div class=\"ellipsis-button\" title=\"Edit this as code\"></div><div class=\"variant\"><div class=\"variant-tag\">#kg</div><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">0.2</div></div></div></div></td><td class=\"add-column-blank\"></td></tr><tr><td class=\"add-row\"><button class=\"add-row-button \" title=\"Add row\">+</button></td><td class=\"bottom-blank\" colspan=\"3\"></td></tr></tbody></table></div></td><td class=\"add-column-blank\"></td></tr><tr><td class=\"add-row\"><button class=\"add-row-button \" title=\"Add row\">+</button></td><td class=\"bottom-blank\" colspan=\"3\"></td></tr></tbody></table></div></div></div></div>")))

blanks :: Monad m => HtmlT m ()
blanks =
  section_
    "Use blanks when you're not done"
    (do explain_
          (do p_
                "If you don't know yet what to put for a part of a formula, just leave it blank with _."
              p_
                "The formula engine will compute the rest of your formula and leave the blanks as-is. This can be an excellent tool for dealing with partial data (tables use this), and for seeing how an equation works.")
        show_
          (step_
             "Use _ when you don't know what to put yet"
             (do cell_
                   "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">mistake</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"\" title=\"\"><input value=\"5*4 + _\" class=\"form-control\" placeholder=\"Type code here\" style=\"width: 10ch;\"></div></div></div></div>"
                 cell_
                   "<div class=\"cell-wrapper\" style=\"\"><div class=\"cell\"><div class=\"cell-header\"><div class=\"cell-name\" title=\"Click to edit cell's name\">mistake</div><button class=\"delete-cell\" title=\"Delete this cell\">×</button></div><div class=\"cell-body\"><div class=\"editor-boundary-wrap clickable-to-edit\" title=\"Click to edit\"><div class=\"misc\">(20 + _)</div></div></div></div></div>")))


cell_ :: Monad m => Text -> HtmlT m ()
cell_ raw = div_ [class_ "example-cell"] (do toHtmlRaw raw)

section_ :: Monad m => Text -> HtmlT m () -> HtmlT m ()
section_ title stuff = do
  div_ [class_ "margin-wrapper"] (h1_ (toHtml title))
  div_ [class_ "margin-wrapper"] stuff

explain_ ::Monad m => HtmlT m () -> HtmlT m ()
explain_ t = div_ [class_ "explain"] t

show_ ::Monad m => HtmlT m () -> HtmlT m ()
show_ inner = div_ [class_ "show"] inner

step_ :: Monad m => Text -> HtmlT m () -> HtmlT m ()
step_ label inner =
  div_
    [class_ "step"]
    (do div_ [class_ "step-label"] (toHtml label)
        inner)
