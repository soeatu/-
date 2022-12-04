#データを読み込み
DF <- read.table("2016.csv",
                 sep = ",",                #カンマ区切りのファイル
                 header = TRUE,            #1行目はヘッダー(列名)
                 stringsAsFactors = FALSE, #文字列を文字列型で取込む
                 fileEncoding="UTF-8")     #文字コードはUTF-8

#行抽出 
DF_SelectAf <- DF[DF$Region=="Sub-Saharan Africa",]
DF_SelectEE <- DF[DF$Region=="Central and Eastern Europe",]
#行結合
DF_Selected <- rbind(DF_SelectAf, DF_SelectEE)
#不要な列を削除
DF_Selected <- DF_Selected[, -c(1, 3, 5, 6)]

#散布図マトリクスの表示

#標準のparis()を使う場合
pairs(DF_Selected[-1]) 

#ライブラリlatticeのsplom()を使う
library(lattice)
splom(DF_Selected[-1],   　　　　　　 #散布図からは区部(9列目)を除く
      groups=DF_Selected$Region,      #色分け
      axis.text.cex=.3,               #目盛りのフォントサイズ
      varname.cex=.5)                 #項目名のフォントサイズ

#ライブラリGGallyのggpairs()で散布図マトリクスを描く
library(ggplot2)
library(GGally)
ggpairs(DF_Selected,                        #区部(9列目)も含める
        aes( colour=as.factor(Region),      #色分け
             alpha=0.5),                    #透明度
        upper=list(continuous=wrap("cor", size=3)) ) +
  #相関係数の文字サイズ
  theme(axis.text =element_text(size=6),  #軸の文字サイズ
        strip.text=element_text(size=6))  #項目の文字サイズ



#(3)相関係数のグラフ表示

#相関行列を計算してオブジェクトCORに格納
COR <- cor(DF_Selected[-1])

#ライブラリ qgraph を使いr=.20を基準として視覚化
library(qgraph)
qgraph( COR, 
        minimum=.20,         #.20以上の相関関係を表示 
        labels=colnames(COR),#長い項目名を省略せずに表示
        edge.labels=T,       #辺に相関係数を表示
        label.scale=F,       #項目名を一定の大きさで表示  
        label.cex=0.8,       #項目名のフォントサイズ
        edge.label.cex=1.4 ) #辺のフォントサイズ


