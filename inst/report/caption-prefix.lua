--- Set the "Figure n." and "Table n." prefix of each HTML caption in bold.
---
--- Quarto adds the prefix when it renders the cross-referenced floats, so this
--- filter runs after that step (`at: post-render`).
--- A caption then starts with the inlines "Figure", a non-breaking space, the
--- number and the delimiter "." from `crossref: title-delim`.
--- The Typst template sets the same prefix in bold itself.

local prefixes = { Figure = true, Table = true }

local function is_prefix(inlines)
  return #inlines >= 4
    and inlines[1].t == "Str"
    and prefixes[inlines[1].text]
    and inlines[2].t == "Str"
    and inlines[2].text == "\u{a0}"
    and inlines[3].t == "Str"
    and inlines[3].text:match("^%d+$") ~= nil
    and inlines[4].t == "Str"
    and inlines[4].text == "."
end

function Plain(plain)
  if not is_prefix(plain.content) then
    return nil
  end
  local content = pandoc.List({ pandoc.Strong({ table.unpack(plain.content, 1, 4) }) })
  for index = 5, #plain.content do
    content:insert(plain.content[index])
  end
  plain.content = content
  return plain
end
