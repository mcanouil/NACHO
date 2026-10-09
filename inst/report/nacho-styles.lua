--- Apply the style hooks of the report in Typst.
---
--- The decision summary writes its three counts as paragraphs in a
--- "nacho-verdict" div, each starting with a span of class "n", and the
--- flagged count also has the class "flag".
--- The parameter table marks each source with a span of class "tag", and a
--- source the user chose also has the class "user".
--- The R code marks each value and limit in a table with a span of class
--- "num".
---
--- HTML does not need this filter: nacho-report.scss styles the classes.
--- In Typst, the verdict becomes a call to `nacho-verdict()`, each tag a call
--- to `nacho-tag()` and each value a call to `nacho-num()`, all from
--- partials/typst-template.typ.
--- Typst tables also get these changes:
--- - A table with no column widths gets equal widths, so it fills the page
---   width like the tables that have widths.
--- - Typst cannot break a long sample id inside a table cell, so each word of
---   more than 12 characters in the text of a cell is split into boxes, after
---   each `_`, `.`, `-` and `/`, and then every 12 characters.
---   Typst can break a line between two boxes, and the PDF text stays as typed.
---   Code in backticks is not split.

local long_word = 12

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

local function verdict(div)
  if not div.classes:includes("nacho-verdict") then
    return nil
  end
  local boxes = pandoc.List()
  for _, block in ipairs(div.content) do
    boxes:insert(verdict_box(block))
  end
  return pandoc.RawBlock("typst", "#nacho-verdict(" .. table.concat(boxes, ", ") .. ")")
end

local function styled_span(span)
  if span.classes:includes("tag") then
    local open = span.classes:includes("user") and "#nacho-tag(user: true)[" or "#nacho-tag["
    return wrap_inlines(open, span.content)
  end
  if span.classes:includes("num") then
    return wrap_inlines("#nacho-num[", span.content)
  end
  return nil
end

local function word_pieces(text)
  local pieces = pandoc.List()
  for piece in text:gmatch("[^_%.%-/]*[_%.%-/]?") do
    while pandoc.text.len(piece) > long_word do
      pieces:insert(pandoc.text.sub(piece, 1, long_word))
      piece = pandoc.text.sub(piece, long_word + 1)
    end
    if piece ~= "" then
      pieces:insert(piece)
    end
  end
  return pieces
end

local function breakable_word(str)
  if pandoc.text.len(str.text) <= long_word then
    return nil
  end
  local boxes = pandoc.List()
  for _, piece in ipairs(word_pieces(str.text)) do
    boxes:extend(wrap_inlines("#box[", { pandoc.Str(piece) }))
  end
  return boxes
end

local function typst_table(tbl)
  local has_widths = false
  for _, colspec in ipairs(tbl.colspecs) do
    if colspec[2] ~= nil and colspec[2] > 0 then
      has_widths = true
    end
  end
  if not has_widths then
    for index, colspec in ipairs(tbl.colspecs) do
      tbl.colspecs[index] = { colspec[1], 1 / #tbl.colspecs }
    end
  end
  for _, body in ipairs(tbl.bodies) do
    for _, row in ipairs(body.body) do
      for _, cell in ipairs(row.cells) do
        cell.contents = cell.contents:walk({ Str = breakable_word })
      end
    end
  end
  return tbl
end

return {
  {
    Div = verdict,
    Span = styled_span,
    Table = typst_table,
  },
}
