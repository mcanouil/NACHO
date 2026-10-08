// Tortilla-chip decoration echoing the NACHO hex logo: rounded, translucent triangles.
#let chip-colours = (
  rgb("#D85460"),
  rgb("#E45430"),
  rgb("#FCB448"),
  rgb("#F0D83C"),
  rgb("#E8EE9A"),
)

#let lerp(a, b, t) = (a.at(0) + (b.at(0) - a.at(0)) * t, a.at(1) + (b.at(1) - a.at(1)) * t)

// One rounded triangle of the given size (pt), rotation and colour.
#let chip(size: 60pt, angle: 0deg, colour: red, alpha: 30%, round: 0.18) = {
  let s = size / 1pt
  let pts = range(3).map(i => {
    let a = angle + i * 120deg - 90deg
    (s / 2 + s / 2 * calc.cos(a), s / 2 + s / 2 * calc.sin(a))
  })
  let segs = ()
  for i in range(3) {
    let p = pts.at(i)
    let prev = pts.at(calc.rem(i + 2, 3))
    let next = pts.at(calc.rem(i + 1, 3))
    let a = lerp(p, prev, round)
    let b = lerp(p, next, round)
    if i == 0 {
      segs.push(curve.move((a.at(0) * 1pt, a.at(1) * 1pt)))
    } else {
      segs.push(curve.line((a.at(0) * 1pt, a.at(1) * 1pt)))
    }
    segs.push(curve.quad((p.at(0) * 1pt, p.at(1) * 1pt), (b.at(0) * 1pt, b.at(1) * 1pt)))
  }
  segs.push(curve.close())
  box(width: size, height: size, curve(fill: colour.transparentize(alpha), ..segs))
}

// A pile of chips placed relative to an anchor, for the cover and corners.
#let chip-pile(scale: 1, alpha: 30%) = {
  let spec = (
    (0pt, 30pt, 150pt, 10deg, 0),
    (70pt, 0pt, 140pt, 45deg, 1),
    (40pt, 70pt, 130pt, -20deg, 3),
    (120pt, 60pt, 120pt, 75deg, 2),
    (10pt, 120pt, 110pt, 30deg, 4),
  )
  box(width: 260pt * scale, height: 240pt * scale, {
    for (dx, dy, size, a, c) in spec {
      place(top + left, dx: dx * scale, dy: dy * scale,
        chip(size: size * scale, angle: a, colour: chip-colours.at(c), alpha: alpha))
    }
  })
}
