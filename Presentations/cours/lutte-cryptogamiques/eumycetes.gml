graph [
  version 2
  directed 1
  bgcolor "transparent"
  nodesep 1.2
  pad "0.212,0.055"
  rankdir "TB"
  splines "ortho"
  node [
    id 0
    name "EUMYCOTA"
    fixedsize "true"
    graphics [
      w 100.8
      H 21.6
      hasFill 1
      type "box"
      fill "#833a94"
      outline "transparent"
    ]
    LabelGraphics [
      fontColor "white"
      fontSize 12
      fontName "Arial"
    ]
  ]
  node [
    id 1
    name "Chytridiomycota"
    fixedsize "true"
    graphics [
      w 100.8
      H 21.6
      type "box"
      fill "transparent"
      outline "transparent"
    ]
    LabelGraphics [
      fontSize 12
      fontName "Arial"
    ]
  ]
  node [
    id 2
    name "Chytridiomycètes"
    fixedsize "true"
    graphics [
      w 100.8
      H 21.6
      type "box"
      fill "transparent"
      outline "transparent"
    ]
    LabelGraphics [
      fontSize 12
      fontName "Arial"
    ]
  ]
  node [
    id 3
    name "invisible1"
    fixedsize "false"
    graphics [
      w 0
      H 0
      type "none"
      fill "transparent"
      outline "transparent"
    ]
    LabelGraphics [
      fontSize 12
      fontName "Arial"
    ]
  ]
  node [
    id 4
    name "invisible2"
    fixedsize "false"
    graphics [
      w 0
      H 0
      type "none"
      fill "transparent"
      outline "transparent"
    ]
    LabelGraphics [
      fontSize 12
      fontName "Arial"
    ]
  ]
  node [
    id 5
    name "Mucoromycètes"
    fixedsize "true"
    graphics [
      w 100.8
      H 21.6
      type "box"
      fill "transparent"
      outline "transparent"
    ]
    LabelGraphics [
      fontSize 12
      fontName "Arial"
    ]
  ]
  node [
    id 6
    name "Symbiomycota"
    fixedsize "true"
    graphics [
      w 100.8
      H 21.6
      type "box"
      fill "transparent"
      outline "transparent"
    ]
    LabelGraphics [
      fontSize 12
      fontName "Arial"
    ]
  ]
  node [
    id 7
    name "Gloméromycètes"
    fixedsize "true"
    graphics [
      w 100.8
      H 21.6
      type "box"
      fill "transparent"
      outline "transparent"
    ]
    LabelGraphics [
      fontSize 12
      fontName "Arial"
    ]
  ]
  node [
    id 8
    name "Dikarya"
    fixedsize "true"
    graphics [
      w 100.8
      H 21.6
      hasFill 1
      type "box"
      fill "#94563a"
      outline "transparent"
    ]
    LabelGraphics [
      fontColor "white"
      fontSize 12
      fontName "Arial"
    ]
  ]
  node [
    id 9
    name "Entorrhizomycètes"
    fixedsize "true"
    graphics [
      w 100.8
      H 21.6
      type "box"
      fill "transparent"
      outline "transparent"
    ]
    LabelGraphics [
      fontSize 12
      fontName "Arial"
    ]
  ]
  node [
    id 10
    name "Basidiomycètes"
    fixedsize "true"
    graphics [
      w 100.8
      H 21.6
      hasFill 1
      type "box"
      fill "#3a7794"
      outline "transparent"
    ]
    LabelGraphics [
      fontColor "white"
      fontSize 12
      fontName "Arial"
    ]
  ]
  node [
    id 11
    name "Ascomycètes"
    fixedsize "true"
    graphics [
      w 100.8
      H 21.6
      hasFill 1
      type "box"
      fill "#4a943a"
      outline "transparent"
    ]
    LabelGraphics [
      fontColor "white"
      fontSize 12
      fontName "Arial"
    ]
  ]
  edge [
    id 1
    source 0
    target 1
  ]
  edge [
    id 2
    source 0
    target 3
    graphics [
      style "dashed"
    ]
  ]
  edge [
    id 3
    source 1
    target 2
    graphics [
      style "dashed"
    ]
  ]
  edge [
    id 4
    source 3
    target 5
  ]
  edge [
    id 5
    source 3
    target 6
  ]
  edge [
    id 8
    source 4
    target 8
  ]
  edge [
    id 7
    source 6
    target 4
    graphics [
      targetArrow "none"
    ]
  ]
  edge [
    id 6
    source 6
    target 7
    graphics [
      style "dashed"
    ]
  ]
  edge [
    id 9
    source 8
    target 9
  ]
  edge [
    id 10
    source 8
    target 10
  ]
  edge [
    id 11
    source 8
    target 11
  ]
]
