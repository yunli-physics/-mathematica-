(* ============================================================
   模型 34：杨氏双缝干涉（物理完整版）
   所属模块：光学（选修第一册·第五章 光的干涉和衍射）
   物理原理：
     光程差  δ = d sinθ ≈ d y / L
     干涉条纹间距  Δx = λ L / d
     合成光强（含单缝衍射包络）：
        I(y) = I0 · [ sin(π a y/(λ L)) / (π a y/(λ L)) ]² · cos²(π d y/(λ L))
     其中 d 为双缝间距，a 为单缝宽度。
   可调参数：缝间距 d(mm)、单缝宽度 a(mm)、波长 λ(nm)、缝屏距离 L(m)
   交互效果：观察干涉条纹、衍射包络与光强分布随参数的变化。
   ============================================================ *)

Manipulate[
 Module[{dm, am, lm, dx, yMin, Ymax, sinc, envelope, intensity, nCentral},
  dm = d/1000;                 (* mm → m *)
  am = a/1000;                 (* mm → m *)
  lm = lambda*10^-9;           (* nm → m *)
  dx = lm L/dm;                (* 干涉条纹间距 / m *)
  yMin = lm L/am;              (* 单缝衍射第一暗纹位置 / m *)
  Ymax = 2.2 yMin;             (* 显示范围：中央主极大 + 两侧次级亮斑 *)

  sinc[x_] := If[x == 0, 1, Sin[x]/x];
  envelope[y_] := sinc[Pi am y/(lm L)]^2;               (* 单缝衍射包络 *)
  intensity[y_] := envelope[y] Cos[Pi dm y/(lm L)]^2;   (* 合成光强 *)

  nCentral = Max[1, 2 Floor[dm/am] - 1];                (* 中央主极大内亮纹数 *)

  Column[{
    (* —— 二维干涉条纹图（含衍射包络，亮纹亮度向两侧递减）—— *)
    DensityPlot[intensity[x/1000], {x, -Ymax*1000, Ymax*1000}, {y, 0, 1},
     FrameTicks -> {{None, None}, {Automatic, None}},
     ColorFunction -> (RGBColor[#, 0, 0] &),
     ColorFunctionScaling -> False,
     PlotPoints -> ControlActive[{100, 3}, {300, 3}],
     MaxRecursion -> 2,
     AspectRatio -> 1/5,
     PlotLabel -> Style["杨氏双缝干涉条纹（含衍射包络）", 12, Bold],
     ImageSize -> 540],

    (* —— 一维光强分布图 —— *)
    Plot[intensity[y/1000], {y, -Ymax*1000, Ymax*1000},
     Frame -> True,
     FrameLabel -> {"屏幕位置 y / mm", "相对光强  I/I₀"},
     PlotStyle -> {Blue, Thick},
     PlotRange -> All,
     GridLines -> {Automatic, Automatic},
     GridLinesStyle -> Directive[GrayLevel[0.85]],
     ImageSize -> 540],

    Style[Row[{
       "条纹间距 Δx = λ L / d = ", NumberForm[dx*1000, {4, 2}], " mm   |   ",
       "中央主极大内亮纹约 ", nCentral, " 条"
       }], 13, Blue],

    If[a >= d,
     Style["⚠ 提示：单缝宽度 a 应小于缝间距 d，否则图案不再符合双缝干涉", 12, Red],
     ""]
   }, Spacings -> 1.4]
 ],
 {{d, 0.2, "缝间距 d (mm)"}, 0.05, 1, 0.05},
 {{a, 0.05, "单缝宽度 a (mm)"}, 0.01, 0.3, 0.01},
 {{lambda, 632.8, "波长 λ (nm)"}, 380, 700, 1},
 {{L, 2, "缝屏距离 L (m)"}, 0.5, 5, 0.1},
 ControlPlacement -> Left
]
