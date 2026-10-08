--- Make tables usable with a screen reader and on a narrow screen.
---
--- Each header cell gets a scope, so a screen reader links every data cell
--- to its headers: "col" in the table head, "row" in the header columns of
--- the body rows.
--- Each table goes in a box that scrolls sideways when the table is wider
--- than the page; the box takes the keyboard focus so it can scroll too, and
--- its label is the table caption, or "Table" and the table number.

local count = 0

local function set_scope(cells, scope)
  for _, cell in ipairs(cells) do
    cell.attributes.scope = scope
  end
end

function Table(tbl)
  count = count + 1
  for _, row in ipairs(tbl.head.rows) do
    set_scope(row.cells, "col")
  end
  for _, body in ipairs(tbl.bodies) do
    local columns = body.row_head_columns
    for _, row in ipairs(body.body) do
      for index, cell in ipairs(row.cells) do
        if index <= columns then
          cell.attributes.scope = "row"
        end
      end
    end
  end
  local label = pandoc.utils.stringify(tbl.caption.long)
  if label == "" then
    label = "Table " .. count
  end
  return pandoc.Div(
    { tbl },
    pandoc.Attr("", { "nacho-table-scroll" }, {
      tabindex = "0",
      role = "region",
      ["aria-label"] = label,
    })
  )
end
