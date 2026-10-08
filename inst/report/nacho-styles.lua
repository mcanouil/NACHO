--- Apply the style hooks of the report in both formats.
---
--- The decision summary writes its three counts as paragraphs in a
--- "nacho-verdict" div, each starting with a span of class "n", and the
--- flagged count also has the class "flag".
--- The parameter table marks each source with a span of class "tag", and a
--- source the user chose also has the class "user".
--- A value cell is a cell in a column headed "Value" or "Limit", or in a
--- right-aligned column.
---
--- In HTML, the classes stay for nacho-report.scss, and each value cell gets
--- the class "num".
--- In Typst, the verdict becomes a call to `nacho-verdict()`, each tag a call
--- to `nacho-tag()` and the content of each value cell a call to
--- `nacho-num()`, all from partials/typst-template.typ.
--- Typst cannot break a long sample id inside a table cell, so each long word
--- in a cell is split after each `_`, `.`, `-` and `/` into boxes: Typst can
--- break a line between two boxes, and the PDF text stays as typed.

local typst = quarto.doc.is_format("typst")

local value_headers = { Value = true, Limit = true }

local function typst_markup(inlines)
  local text = pandoc.write(pandoc.Pandoc({ pandoc.Plain(inlines) }), "typst")
  return (text:gsub("%s+$", ""))
end

local function wrap_inlines(open, inlines)
  local wrapped = pandoc.List({ pandoc.RawInline("typst", open) })
  wrapped:extend(inlines)
  wrapped:insert(pandoc.RawInline("typst", "]"))
  return wrapped
end

local function verdict_box(block)
  local count = block.content[1]
  if block.t ~= "Para" or count == nil or count.t ~= "Span" or not count.classes:includes("n") then
    error("nacho-styles.lua: each paragraph of a nacho-verdict div must start with a span of class n.")
  end
  local label = pandoc.List()
  for index = 2, #block.content do
    if #label > 0 or block.content[index].t ~= "Space" then
      label:insert(block.content[index])
    end
  end
  return string.format(
    "([%s], [%s], %s)",
    typst_markup(count.content),
    typst_markup(label),
    count.classes:includes("flag") and "true" or "false"
  )
end

function Div(div)
  if not typst or not div.classes:includes("nacho-verdict") then
    return nil
  end
  local boxes = pandoc.List()
  for _, block in ipairs(div.content) do
    boxes:insert(verdict_box(block))
  end
  return pandoc.RawBlock("typst", "#nacho-verdict(" .. table.concat(boxes, ", ") .. ")")
end

function Span(span)
  if not typst or not span.classes:includes("tag") then
    return nil
  end
  local open = span.classes:includes("user") and "#nacho-tag(user: true)[" or "#nacho-tag["
  return wrap_inlines(open, span.content)
end

local function value_columns(tbl)
  local columns = {}
  local header = tbl.head.rows[1]
  for index, colspec in ipairs(tbl.colspecs) do
    local title = header and header.cells[index] and pandoc.utils.stringify(header.cells[index].contents) or ""
    columns[index] = colspec[1] == "AlignRight" or value_headers[title] == true
  end
  return columns
end

local long_word = 20

local function breakable_word(str)
  if #str.text <= long_word then
    return nil
  end
  local pieces = pandoc.List()
  for piece in str.text:gmatch("[^_%.%-/]*[_%.%-/]?") do
    if piece ~= "" then
      pieces:extend(wrap_inlines("#box[", { pandoc.Str(piece) }))
    end
  end
  return pieces
end

local function mark_value_cell(cell)
  if not typst then
    cell.classes:insert("num")
    return
  end
  for _, block in ipairs(cell.contents) do
    if block.t == "Plain" or block.t == "Para" then
      block.content = wrap_inlines("#nacho-num[", block.content)
    end
  end
end

function Table(tbl)
  local columns = value_columns(tbl)
  for _, body in ipairs(tbl.bodies) do
    for _, row in ipairs(body.body) do
      for index, cell in ipairs(row.cells) do
        if typst then
          cell.contents = cell.contents:walk({ Str = breakable_word })
        end
        if columns[index] then
          mark_value_cell(cell)
        end
      end
    end
  end
  return tbl
end
