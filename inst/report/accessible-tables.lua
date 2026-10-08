--- Make tables usable with a screen reader and on a narrow screen.
---
--- Each header cell gets a scope, so a screen reader links every data cell
--- to its headers: "col" in the table head, "row" in the header columns of
--- the body rows.
--- Each table goes in a box that scrolls sideways when the table is wider
--- than the page; the box takes the keyboard focus so it can scroll too, and
--- its label is the table caption, or "Table" and the table number.
--- Quarto moves the caption of a cross-referenced table to the float around
--- it, so a first pass copies that caption to the table.

local count = 0

local function set_scope(cells, scope)
  for _, cell in ipairs(cells) do
    cell.attributes.scope = scope
  end
end

local function caption_floats(float)
  if float.type ~= "Table" then
    return nil
  end
  local label = pandoc.utils.stringify(float.caption_long or {})
  local function set_label(tbl)
    tbl.attributes["nacho-label"] = label
    return tbl
  end
  if float.content.t == "Table" then
    float.content = set_label(float.content)
  else
    float.content = float.content:walk({ Table = set_label })
  end
  return float
end

local function wrap_table(tbl)
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
  local label = tbl.attributes["nacho-label"] or pandoc.utils.stringify(tbl.caption.long)
  tbl.attributes["nacho-label"] = nil
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

return {
  { FloatRefTarget = caption_floats },
  { Table = wrap_table },
}
