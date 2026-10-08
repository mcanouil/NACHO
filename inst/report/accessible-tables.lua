--- Make tables usable with a screen reader and on a narrow screen.
---
--- Each header cell gets the column scope, so a screen reader links every
--- data cell to its column header.
--- Each table goes in a box that scrolls sideways when the table is wider
--- than the page; the box takes the keyboard focus so it can scroll too.
function Table(tbl)
  for _, row in ipairs(tbl.head.rows) do
    for _, cell in ipairs(row.cells) do
      cell.attributes.scope = "col"
    end
  end
  return pandoc.Div(
    { tbl },
    pandoc.Attr("", { "nacho-table-scroll" }, {
      tabindex = "0",
      role = "region",
      ["aria-label"] = "Table",
    })
  )
end
