添加自动动画
mathematica
Manipulate[
  Plot[Sin[a x], {x, 0, 2 Pi}, PlotRange -> {-1.5, 1.5}],
  {{a, 1}, 1, 5, Animator}
]
