# 令和元 (2019) 年度ゴマサバ太平洋系群の資源評価　補足資料3
## 概要
* ゴマサバの卵密度は2018年に急増したが，この増加はマサバ卵が混在することでゴマサバの卵密度が過大推定されている可能性があったため，2018年以降の卵密度データはチューニング指数として使用できなかった
* このマサバ・ゴマサバ卵の誤同定問題を解決するために，VASTを用いた産卵量指標値の標準化を行った．
* 今回のVASTモデルの主な特徴は以下の2つ
  * "マサバ卵が多いとゴマサバ卵の採集率が高くなる"と考え，マサバの卵密度をcatchability covariateに入れたこと
  * 年と月の交互作用をoverdispersion configに入れたこと
* 解析の結果，2018年のゴマサバ卵の急増は下方修正された．

## コードの詳細
* estimation.R
  * vastのコード．catchability covariateや年と月の交互作用の入れ方の参考に
* figures.R
  * 補足資料の図を作成するために用いたコード．VASTとFishStatsUtilsでは書けないような図もggplot2を使って作成している．

## 参考文献 
* Y Kanamori, S Nishijima, H Okamura, R Yukami, M Watai, A Takasuka (2021) Spatio-temporal model reduces species misidentification bias of spawning eggs in stock assessment of spotted mackerel in the western North Pacific. [Fish Res 236:105825](https://www.sciencedirect.com/science/article/pii/S0165783620303428)  
* [令和元 (2019) 年度ゴマサバ太平洋系群の資源評価](http://www.fra.affrc.go.jp/shigen_hyoka/SCmeeting/2019-1/detail_goma_p_2.pdf) 
