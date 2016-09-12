# Star_R ver.2.9  2016-09-11~
# ファイル読み込み--
data <- read.csv(file.choose(), header=FALSE)

## 各種関数の定義
# library_visual_scenesの書き出し
# <instance_geometry> ～ </instance_geometry>
F_lib_vis_scene <- function (s_count) {
  lib_vis_sce2 = array(dim = c(9))
  for (i in 1:s_count) {
    # instance_geometry urlの設定
    ig_url = paste("\t\t\t\t<instance_geometry url=\"#ID", i, "\">", sep = "") 
    im_tar = paste("\t\t\t\t\t\t\t<instance_material symbol=\"Material2\" target=\"#ID", i, "-material\">", sep = "")
    lib_vis_sce2[1] = ig_url
    lib_vis_sce2[2] = "\t\t\t\t\t<bind_material>"
    lib_vis_sce2[3] = "\t\t\t\t\t\t<technique_common>"
    lib_vis_sce2[4] = im_tar
    lib_vis_sce2[5] = "\t\t\t\t\t\t\t\t<bind_vertex_input semantic=\"UVSET0\" input_semantic=\"TEXCOORD\" input_set=\"0\" />"
    lib_vis_sce2[6] = "\t\t\t\t\t\t\t</instance_material>"
    lib_vis_sce2[7] = "\t\t\t\t\t\t</technique_common>"
    lib_vis_sce2[8] = "\t\t\t\t\t</bind_material>"
    lib_vis_sce2[9] = "\t\t\t\t</instance_geometry>"
    for (j in 1:9){
      write (lib_vis_sce2[j], "fileoutput.dae", append = TRUE)
    }
  }
}

# library_geometries(Position)の書き出し
F_lib_geo_Pos <- function (id, count) {
  lib_geo_Pos = array(dim = c(4))
  # geometry idの設定
  geo_id = paste("\t\t<geometry id=\"ID", id, "\">", sep = "") 
  lib_geo_Pos[1] = geo_id
  lib_geo_Pos[2] = "\t\t\t<mesh>"
  # source idの設定
  sour_id = paste("\t\t\t\t<source id=\"ID", id, "-Pos\">", sep = "") 
  lib_geo_Pos[3] = sour_id
  fl_arr_id = paste("\t\t\t\t\t<float_array id=\"ID", id, "-Pos-array\" count=\"", count, "\">", sep = "") 
  lib_geo_Pos[4] = fl_arr_id
  for (j in 1:4){
    write (lib_geo_Pos[j], "fileoutput.dae", append = TRUE)
  }
}

# 底面頂点座標の書き出し
F_bottom_plate <- function (vertex, xb, yb, zb) {
  bottom = array(dim = c(vertex))
  for (j in 1:vertex) {
    bottom[j] = paste("\t\t\t\t\t\t", xb[j], yb[j], zb[j])
  }
  for (j in 1:vertex){
    write (bottom[j], "fileoutput.dae", append = TRUE)
  }
}
# 側面頂点座標の書き出し
F_side_plate <- function (vertex, xb, yb, zb, xt, yt, zt) {
  side = array(dim = c(vertex, 4))
  for (j in 1:vertex) {
    k = j + 1
    if (j == vertex){
      k = 1
    }
    side[j, 1] = paste("\t\t\t\t\t\t", xb[k], yb[k], zb[k])
    side[j, 2] = paste("\t\t\t\t\t\t", xb[j], yb[j], zb[j])
    side[j, 3] = paste("\t\t\t\t\t\t", xt[j], yt[j], zt[j])
    side[j, 4] = paste("\t\t\t\t\t\t", xt[k], yt[k], zt[k])
  }
  for (j in 1:vertex){
    for (k in 1:4) {
      write (side[j, k], "fileoutput.dae", append = TRUE)
    }
  }
}
# 上面頂点座標の書き出し
F_top_plate <- function (vertex, xt, yt, zt) {
  top = array(dim = c(vertex))
  for (j in 1:vertex) {
    top[j] = paste("\t\t\t\t\t\t", xt[vertex-j+1], yt[vertex-j+1], zt[vertex-j+1])
  }
  for (j in 1:vertex){
    write (top[j], "fileoutput.dae", append = TRUE)
  }
}

# Pos_technique_commonの書き出し
F_Pos_Tech_com <- function (id) {
  tec_com = array(dim = c(8))
  tec_com[1] = "\t\t\t\t\t</float_array>"
  tec_com[2] = "\t\t\t\t\t<technique_common>"
  # accessor coount, sourceの設定
  accsor = paste("\t\t\t\t\t\t<accessor count=\"", ver_num, "\" source=\"#ID", id, "-Pos-array\" stride=\"3\">", sep = "")
  tec_com[3] = accsor
  tec_com[4] = "\t\t\t\t\t\t\t<param name=\"X\" type=\"float\" />"
  tec_com[5] = "\t\t\t\t\t\t\t<param name=\"Y\" type=\"float\" />"
  tec_com[6] = "\t\t\t\t\t\t\t<param name=\"Z\" type=\"float\" />"
  tec_com[7] = "\t\t\t\t\t\t</accessor>"
  tec_com[8] = "\t\t\t\t\t</technique_common>"
  tec_com[9] = "\t\t\t\t</source>"
  for (j in 1:9){
    write (tec_com[j], "fileoutput.dae", append = TRUE)
  }
}

# library_geometries(Normal)の書き出し
F_lib_geo_Nor <- function (id, count) {
  sour_id = paste("\t\t\t\t<source id=\"ID", id, "-Normal\">", sep = "") 
  fl_arr_id = paste("\t\t\t\t\t<float_array id=\"ID", id, "-Normal-array\" count=\"", count, "\">", sep = "") 
  lib_geo_Pos = array(dim = c(2))
  lib_geo_Pos[1] = sour_id
  lib_geo_Pos[2] = fl_arr_id
  for (j in 1:2){
    write (lib_geo_Pos[j], "fileoutput.dae", append = TRUE)
  }
}

# 法線ベクトルを求める
F_normal_vector <- function (p1, p2, p3) {
  v1 = array(dim = c(3))
  v2 = array(dim = c(3))
  cross = array(dim = c(3))
  for (j in 1:3) {
    v1[j] = p1[j] - p2[j]
  }
  for (j in 1:3) {
    v2[j] = p3[j] - p2[j]
  }
  for (j in 1:3) {
    cross[j] = v2[(j+1)%%3+1]*v1[(j+2)%%3+1] - v2[(j+2)%%3+1]*v1[(j+1)%%3+1]
  }
  length = sqrt(cross[1] * cross[1] + cross[2] * cross[2] + cross[3] * cross[3])
  normal = array(dim = c(3))
  for (j in 1:3) {
    normal[j] = cross[j] / length
  }
  return(c(normal[3], normal[1], normal[2]))
}

# Nor_technique_commonの書き出し
F_Nor_Tech_com <- function (id) {
  tec_com = array(dim = c(8))
  tec_com[1] = "\t\t\t\t\t</float_array>"
  tec_com[2] = "\t\t\t\t\t<technique_common>"
  # accessor coount, sourceの設定
  accsor = paste("\t\t\t\t\t\t<accessor count=\"", ver_num, "\" source=\"#ID", id, "-Noamal-array\" stride=\"3\">", sep = "")
  tec_com[3] = accsor
  tec_com[4] = "\t\t\t\t\t\t\t<param name=\"X\" type=\"float\" />"
  tec_com[5] = "\t\t\t\t\t\t\t<param name=\"Y\" type=\"float\" />"
  tec_com[6] = "\t\t\t\t\t\t\t<param name=\"Z\" type=\"float\" />"
  tec_com[7] = "\t\t\t\t\t\t</accessor>"
  tec_com[8] = "\t\t\t\t\t</technique_common>"
  tec_com[9] = "\t\t\t\t</source>"
  for (j in 1:9){
    write (tec_com[j], "fileoutput.dae", append = TRUE)
  }
}

# vertices + polilystの書き出し
F_verti_polist <- function (id) {
  # verticesの書き出し
  verti = array(dim = c(4))
  id_vtx = paste("\t\t\t\t<vertices id=\"ID", id, "-Vtx\">", sep = "")
  id_pos = paste("\t\t\t\t\t<input semantic=\"POSITION\" source=\"#ID", id, "-Pos\" />", sep = "")
  id_nor = paste("\t\t\t\t\t<input semantic=\"NORMAL\" source=\"#ID", id, "-Normal\" />", sep = "")
  verti[1] = id_vtx
  verti[2] = id_pos
  verti[3] = id_nor
  verti[4] = "\t\t\t\t</vertices>"
  for (j in 1:4){
    write (verti[j], "fileoutput.dae", append = TRUE)
  }
  p_num = ver_num
  p_no = c(0)
  for (j in 1:(p_num-1)) {
    p_no = c(p_no, j)
  }
  polist = array(dim = c(5))
  poly_mate = paste("\t\t\t\t<polylist count=\"", poly_mate_cnt, "\" material=\"Material2\">", sep = "")
  polist[1] = poly_mate
  sour_id = paste("\t\t\t\t\t<input offset=\"0\" semantic=\"VERTEX\" source=\"#ID", id, "-Vtx\" />", sep = "")
  polist[2] = sour_id
  for (j in 1:2) {
    write (polist[j], "fileoutput.dae", append = TRUE)
  }
  sink(file = "fileoutput.dae", append = TRUE, split = TRUE)
  cat ("\t\t\t\t\t<vcount>", vcnt, "</vcount>\n")
  sink()
  sink(file = "fileoutput.dae", append = TRUE, split = TRUE)
  cat ("\t\t\t\t\t<p>", p_no, "</p>\n")
  sink()
  polist[3] = "\t\t\t\t</polylist>"
  polist[4] = "\t\t\t</mesh>"
  polist[5] = "\t\t</geometry>"
  for (j in 3:5) {
    write (polist[j], "fileoutput.dae", append = TRUE)
  }
}

# library_materialsの書き出し
F_lib_mate <- function (s_count) {
  lib_mate = array(dim = c(7))
  lib_mate[1] = "\t</library_geometries>"
  lib_mate[2] = "\t<library_materials>"
  for (j in 1:2){
    write (lib_mate[j], "fileoutput.dae", append = TRUE)
  }
  for (i in 1:s_count) {
    mate_id = paste("\t\t<material id=\"ID", i, "-material\">", sep = "")
    effe_url = paste("\t\t\t<instance_effect url=\"#ID", i, "-surface\" />", sep = "")
    lib_mate[3] = mate_id
    lib_mate[4] = effe_url
    lib_mate[5] = "\t\t</material>"
    for (j in 3:5){
      write (lib_mate[j], "fileoutput.dae", append = TRUE)
    }
  }
  lib_mate[6] = "\t</library_materials>"
  lib_mate[7] = "\t<library_effects>"
  for (j in 6:7){
    write (lib_mate[j], "fileoutput.dae", append = TRUE)
  }
}

# library_effectsの書き出し
F_lib_effe <- function (s_count, col_rgb) {
  for (i in 1:s_count) {
    lib_effe = array(dim = c(11))
    effe_id = paste("\t\t<effect id=\"ID", i, "-surface\">", sep = "")
    if (i > counter) {
      col_rgb[i] = "0.41 0.41 0.41 1"
    }
    effe_col = paste("\t\t\t\t\t\t\t<color>", col_rgb[i], "</color>", sep = "")
    lib_effe[1] = effe_id
    lib_effe[2] = "\t\t\t<profile_COMMON>"
    lib_effe[3] = "\t\t\t\t<technique sid=\"COMMON\">"
    lib_effe[4] = "\t\t\t\t\t<lambert>"
    lib_effe[5] = "\t\t\t\t\t\t<diffuse>"
    lib_effe[6] = effe_col
    lib_effe[7] = "\t\t\t\t\t\t</diffuse>"
    lib_effe[8] = "\t\t\t\t\t</lambert>"
    lib_effe[9] = "\t\t\t\t</technique>"
    lib_effe[10] = "\t\t\t</profile_COMMON>"
    lib_effe[11] = "\t\t</effect>"
    for (j in 1:11){
      write (lib_effe[j], "fileoutput.dae", append = TRUE)
    }
  }
}

# zoningのcolor設定
F_zone_color <- function (zoning, id) {
  if (zoning == 1) {
    col_rgb[i] = "0 0.647 0.408 1"
  } else if (zoning == 2) {
    col_rgb[i] = "0.467 0.718  0.620 1"
  } else if (zoning == 3) {
    col_rgb[i] = "0.314 0.686  0.424 1"
  } else if (zoning == 4) {
    col_rgb[i] = "0.745 0.804  0 1"
  } else if (zoning == 5) {
    col_rgb[i] = "1 0.937  0.267 1"
  } else if (zoning == 6) {
    col_rgb[i] = "0.976 0.698  0 1"
  } else if (zoning == 7) {
    col_rgb[i] = "0.933 0.498  0 1"
  } else if (zoning == 8) {
    col_rgb[i] = "0.941 0.569  0.604 1"
  } else if (zoning == 9) {
    col_rgb[i] = "0.910 0.345  0.522 1"
  } else if (zoning == 10) {
    col_rgb[i] = "0.820 0.741  0.851 1"
  } else if (zoning == 11) {
    col_rgb[i] = "0.749 0.886  0.906 1"
  } else if (zoning == 12) {
    col_rgb[i] = "0.314 0.420  0.678 1"
  }
}

## プログラム本体
# 変数の宣言
ncol(data)
nrow(data)
counter = nrow(data)
col_rgb = array(dim = c(counter))
kansan = 0.0254     # ｍをインチ換算
koutai2 = 0.6       # 町家の２階壁面の後退距離
matiya1 = 2.7 + 0.3 # 地下の根入れ部分30cmを追加
matiya2 = 1.8
roof_count = 0
yane_count = 0

# 傾斜屋根タイプのデータ数の算出
for (i in 1:counter) {
  yanetype = data[i, 10]
  vertex_data = data[i, 25]
  if ((yanetype != 0) && (vertex_data == 10)) {
    roof_count = roof_count + 1
  }
}

# COLLADAファイルの出力開始
# ファイル出力（ヘッダー部分の書き出し）
xml = "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
collada_1 = "<COLLADA xmlns=\"http://www.collada.org/2005/11/COLLADASchema\" version=\"1.4.1\">"
write (xml, "fileoutput.dae")
write (collada_1, "fileoutput.dae", append = TRUE)
asset = array(dim = c(4))
asset[1] = "\t<asset>"
asset[2] = "\t\t<unit meter=\"0.0254\" name=\"inch\" />"
asset[3] = "\t\t<up_axis>Z_UP</up_axis>"
asset[4] = "\t</asset>"
for (j in 1:4){
  write (asset[j], "fileoutput.dae", append = TRUE)
}

# library_visual_scenesの書き出し
# <library_visual_scenes> ～ <node>
lib_vis_sce1 = array(dim = c(3))
lib_vis_sce1[1] = "\t<library_visual_scenes>"
lib_vis_sce1[2] = "\t\t<visual_scene id=\"DefaultScene\">"
lib_vis_sce1[3] = "\t\t\t<node name=\"Building\">"
for (j in 1:3){
  write (lib_vis_sce1[j], "fileoutput.dae", append = TRUE)
}
# <instance_geometry> ～ </instance_geometry>
# データ毎に必要な部分（url(#ID）のみが変わる）
s_count = counter + roof_count
F_lib_vis_scene (s_count)   # 関数によるIDのみを変更した繰り返し部分の書き出し

# </node> ～ <library_geometries>
lib_vis_sce3 = array(dim = c(4))
lib_vis_sce3[1] = "\t\t\t</node>"
lib_vis_sce3[2] = "\t\t</visual_scene>"
lib_vis_sce3[3] = "\t</library_visual_scenes>"
lib_vis_sce3[4] = "\t<library_geometries>"
for (j in 1:4){
  write (lib_vis_sce3[j], "fileoutput.dae", append = TRUE)
}

# 逐次処理（１件ずつデータ読み込み → モデリングを行う）
for (i in 1:counter) {
  hyoukou = data[i, 2]    # 地盤高データの読み込み
  youto = data[i, 6]      # 建物用途データの読み込み
  story = data[i, 8]      # 階数データの読み込み
  basement = data[i, 10]  # 地下階数データの読み込み
  yanetype = data[i, 12]  # 屋根タイプの読み込み
  incline = data[i, 14]   # 屋根勾配データの読み込み
  hiratuma = data[i, 16]  # 平入り・妻入りデータの読み込み
  yanemuki = data[i, 18]  # 流れ方向の読み込み
  hisashi = data[i, 20]   # 軒庇の長さの読み込み
  keraba = data[i, 22]    # けらば幅の読み込み
  yaneatu = data[i, 24]   # 屋根厚さの読み込み
  zoning = data[i, 26]    # 用途地域の読み込み
  # col_rgb[i] = "1 1 1 1"  # colorのデフォルト設定
  col_rgb[i] = F_zone_color (zoning, i)   # 関数によるcolor値(RGB)の読み込み
  
  # 頂点座標の配列変数の準備と読み込み
  vertex = data[i, 27] / 2  # 頂点数（屋根形態に応じる）
  coordinate = array(dim = c(vertex, 3))
  dim(coordinate)
  for (j in 1:vertex) {
    x = 28 + 2 * (j - 1)
    coordinate[j, 1] = data[i, x]
    y = 29 + 2 * (j - 1)
    coordinate[j, 2] = data[i, y]
    coordinate[j, 3] = hyoukou
  }
  # 起点と終点の頂点座標が完全一致する場合は，頂点のデータ数を１つ減らす
  # 傾斜屋根モデルの場合は5頂点を4頂点に変更してモデリングする
  if ((coordinate[1, 1] == coordinate[vertex, 1]) && (coordinate[1, 2] == coordinate[vertex, 2])) {
    vertex = vertex - 1
  }
  botm_poly = vertex   # 底面の頂点数
  top_poly = vertex    # 上面の頂点数
  
  ver_num = vertex * 4 + botm_poly + top_poly    # 面頂点数の総和
  count = ver_num * 3                            # 座標データ数の総和
  poly_mate_cnt = vertex + 2
  
  ## モデリング用頂点データの準備
  # h = 3.3     # 階高3.3ｍ（仮設定）
  # height = h * story
  # 傾斜屋根家屋の高さの計算
  if ((yanetype != 0) && (story <= 3)) {
    height = 3.25 + 2.5 * (story - 1)
  }
  # 専門店・飲食店の高さの計算
  else if (youto == 2) {
    height = 3.2 * story
  }
  # 事務所ビルの高さの計算
  else if (youto == 3) {
    height = 4.2 + 3.5 * (story - 1)
  }
  # デパート・スーパーの高さの計算
  else if (youto == 4) {
    height = 4.0 + 3.5 * (story - 1)
  }
  # ホテルの高さの計算
  else if (youto == 5) {
    height = 4.2 + 3.2 * (story - 1)
  }
  # マンションの高さの計算
  else if (youto == 6) {
    height = 3.2 * story
  }
  # その他住宅の場合の高さの計算
  else
  {
    height = 3.3 * story
  }
  
  xb = array(dim = c(vertex))
  yb = array(dim = c(vertex))
  zb = array(dim = c(vertex))
  xt = array(dim = c(vertex))
  yt = array(dim = c(vertex))
  zt = array(dim = c(vertex))
  for (j in 1:vertex) {
    # 底面座標（ｍ→ inch）
    xb[j] = coordinate[j, 1] / kansan
    yb[j] = coordinate[j, 2] / kansan
    # 地下部分を追加するため，底面のZ座標を地下部分の深さだけ下げる
    # ３階建て以下は根入れ深さを30cmとし，４階建て以上は階高の1/3の1ｍとする
    if (story <= 3) {
      zb[j] = (coordinate[j, 3] - 0.3) / kansan
    } else if (story > 3) {
      zb[j] = (coordinate[j, 3] - 1.0) / kansan
    }
    
    # 上面座標（ｍ→ inch）
    xt[j] = coordinate[j, 1] / kansan
    yt[j] = coordinate[j, 2] / kansan
    zt[j] = (coordinate[j, 3] + height) / kansan
  }
  
  # ４頂点建物モデルのモデリング
  if (vertex == 4) {
    # 建物本体のモデリング
    xk = array(dim = c(8))    # 切妻屋根・町家１階の家形頂点座標
    yk = array(dim = c(8))
    zk = array(dim = c(8))
    xm = array(dim = c(8))    # 町家２階の家形頂点座標
    ym = array(dim = c(8))
    zm = array(dim = c(8))
    
    if (yanetype == 1) {
      ## 切妻屋根の家型のモデリング
      # 切妻屋根は平入りを基本とし，hiratumaが2の場合は妻入りに入れ替える
      if (hiratuma == 2) {
        xb2 = array(dim = c(4))
        yb2 = array(dim = c(4))
        xt2 = array(dim = c(4))
        yt2 = array(dim = c(4))
        for (j in 1:4) {
          xb2[j] = xb[j]
          yb2[j] = yb[j]
          xt2[j] = xt[j]
          yt2[j] = yt[j]
        }
        for (j in 1:3) {
          xb[j] = xb2[j+1]
          yb[j] = yb2[j+1]
          xt[j] = xt2[j+1]
          yt[j] = yt2[j+1]
        }
        xb[4] = xb2[1]
        yb[4] = yb2[1]
        xt[4] = xt2[1]
        yt[4] = yt2[1]
      }
      
      # 家型上面用の座標設定
      xk[1] = xt[1]
      xk[2] = xt[2]
      xk[5] = xt[3]
      xk[6] = xt[4]
      xk[3] = (xk[1] + xk[6]) / 2
      xk[4] = (xk[2] + xk[5]) / 2
      xk[7] = (xk[2] + xk[5]) / 2
      xk[8] = (xk[1] + xk[6]) / 2
      yk[1] = yt[1]
      yk[2] = yt[2]
      yk[5] = yt[3]
      yk[6] = yt[4]
      yk[3] = (yk[1] + yk[6]) / 2
      yk[4] = (yk[2] + yk[5]) / 2
      yk[7] = (yk[2] + yk[5]) / 2
      yk[8] = (yk[1] + yk[6]) / 2
      zk[1] = zt[1]
      zk[2] = zt[2]
      zk[5] = zt[3]
      zk[6] = zt[4]
      
      # 妻側頂点の高さ（Z座標）を計算する
      l_s2 = sqrt((xk[2] - xk[5])^2 + (yk[2] - yk[5])^2)
      l_s4 = sqrt((xk[1] - xk[6])^2 + (yk[1] - yk[6])^2)
      zk[3] = (l_s2 * incline / 2) + ((zk[1] + zk[6]) / 2)
      zk[4] = (l_s4 * incline / 2) + ((zk[2] + zk[5]) / 2)
      zk[7] = (l_s4 * incline / 2) + ((zk[2] + zk[5]) / 2)
      zk[8] = (l_s2 * incline / 2) + ((zk[1] + zk[6]) / 2)
      
      # ID番号の設定（iをidに変更 → counterまでが建物本体）
      id = i
      
      # 切妻屋根の建物モデリング
      ver_num = (1 + 2 + 2) * 4 + 2 * 5
      count = ver_num * 3
      F_lib_geo_Pos (id, count)
      
      # 底面頂点座標の書き出し
      F_bottom_plate (vertex, xb, yb, zb)
      
      # 側面頂点座標の書き出し
      side1 = array(dim = c(2, 4))
      side2 = array(dim = c(2, 5))
      for (j in 1:4) {
        k = j + 1
        if (j == 4) { k = 1 }
        if ((j == 1) || (j == 3)) {
          side1[j/2+1, 1] = paste("\t\t\t\t\t\t", xb[k], yb[k], zb[k])
          side1[j/2+1, 2] = paste("\t\t\t\t\t\t", xb[j], yb[j], zb[j])
          side1[j/2+1, 3] = paste("\t\t\t\t\t\t", xt[j], yt[j], zt[j])
          side1[j/2+1, 4] = paste("\t\t\t\t\t\t", xt[k], yt[k], zt[k])
        }
        if ((j == 2) || (j == 4)) {
          side2[j/2, 1] = paste("\t\t\t\t\t\t", xb[k], yb[k], zb[k])
          side2[j/2, 2] = paste("\t\t\t\t\t\t", xb[j], yb[j], zb[j])
          side2[j/2, 3] = paste("\t\t\t\t\t\t", xt[j], yt[j], zt[j])
          side2[j/2, 4] = paste("\t\t\t\t\t\t", xk[j*2], yk[j*2], zk[j*2])
          side2[j/2, 5] = paste("\t\t\t\t\t\t", xt[k], yt[k], zt[k])
        }
      }
      for (j in 1:2){
        for (k in 1:4) {
          write (side1[j, k], "fileoutput.dae", append = TRUE)
        }
        for (k in 1:5) {
          write (side2[j, k], "fileoutput.dae", append = TRUE)
        }
      }
      
      # 上面頂点座標の書き出し
      top = array(dim = c(2, 4))
      top[1, 1] = paste("\t\t\t\t\t\t", xk[8], yk[8], zk[8])
      top[1, 2] = paste("\t\t\t\t\t\t", xk[7], yk[7], zk[7])
      top[1, 3] = paste("\t\t\t\t\t\t", xk[2], yk[2], zk[2])
      top[1, 4] = paste("\t\t\t\t\t\t", xk[1], yk[1], zk[1])
      top[2, 1] = paste("\t\t\t\t\t\t", xk[4], yk[4], zk[4])
      top[2, 2] = paste("\t\t\t\t\t\t", xk[3], yk[3], zk[3])
      top[2, 3] = paste("\t\t\t\t\t\t", xk[6], yk[6], zk[6])
      top[2, 4] = paste("\t\t\t\t\t\t", xk[5], yk[5], zk[5])
      for (j in 1:2){
        for (k in 1:4) {
          write (top[j, k], "fileoutput.dae", append = TRUE)
        }
      }
      
      # Pos_technique_commonの書き出し
      F_Pos_Tech_com (id)
      
      # library_geometries(Normal)の書き出し
      F_lib_geo_Nor (id, count)
      
      ## 法線ベクトルの書き出し
      Nor = c()
      
      # 底面法線ベクトルの算出
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xb[k], yb[k], zb[k])
        p2 = c(xb[j], yb[j], zb[j])
        p3 = c(xb[l], yb[l], zb[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      # 側面法線ベクトルの算出
      for (j in 1:4) {
        l = j + 1
        if (l > 4) { l = 1 }
        # 平側の法線ベクトル
        if ((j == 1) || (j == 3)) {
          for (k in 1:4) {
            if (k == 1) {
              p1 = c(xt[j], yt[j], zt[j])
              p2 = c(xb[j], yb[j], zb[j])
              p3 = c(xb[l], yb[l], zb[l])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 2) {
              p1 = c(xb[j], yb[j], zb[j])
              p2 = c(xb[l], yb[l], zb[l])
              p3 = c(xt[l], yt[l], zt[l])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 3) {
              p1 = c(xb[l], yb[l], zb[l])
              p2 = c(xt[l], yt[l], zt[l])
              p3 = c(xt[j], yt[j], zt[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 4) {
              p1 = c(xt[l], yt[l], zt[l])
              p2 = c(xt[j], yt[j], zt[j])
              p3 = c(xb[j], yb[j], zb[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
          }
        }
        # 妻側の法線ベクトル
        if ((j == 2) || (j == 4)) {
          for (k in 1:5) {
            if (k == 1) {
              p1 = c(xt[j], yt[j], zt[j])
              p2 = c(xb[j], yb[j], zb[j])
              p3 = c(xb[l], yb[l], zb[l])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 2) {
              p1 = c(xb[j], yb[j], zb[j])
              p2 = c(xb[l], yb[l], zb[l])
              p3 = c(xt[l], yt[l], zt[l])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 3) {
              p1 = c(xb[l], yb[l], zb[l])
              p2 = c(xt[l], yt[l], zt[l])
              p3 = c(xk[j*2], yk[j*2], zk[j*2])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 4) {
              p1 = c(xt[l], yt[l], zt[l])
              p2 = c(xk[j*2], yk[j*2], zk[j*2])
              p3 = c(xt[j], yt[j], zt[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 5) {
              p1 = c(xk[j*2], yk[j*2], zk[j*2])
              p2 = c(xt[j], yt[j], zt[j])
              p3 = c(xb[j], yb[j], zb[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
          }
        }
      }
      
      # 上面１の法線ベクトルの算出
      for (j in 1:2) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 8}
        if (l > 2) {l =7}
        p1 = c(xk[k], yk[k], zk[k])
        p2 = c(xk[j], yk[j], zk[j])
        p3 = c(xk[l], yk[l], zk[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      for (j in 7:8) {
        k = j - 1
        l = j + 1
        if (k == 6) {k = 2}
        if (l > 8) {l = 1}
        p1 = c(xk[k], yk[k], zk[k])
        p2 = c(xk[j], yk[j], zk[j])
        p3 = c(xk[l], yk[l], zk[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      # 上面２の法線ベクトルの算出
      for (j in 3:6) {
        k = j - 1
        l = j + 1
        if (k == 2) {k = 6}
        if (l > 6) {l = 3}
        p1 = c(xk[k], yk[k], zk[k])
        p2 = c(xk[j], yk[j], zk[j])
        p3 = c(xk[l], yk[l], zk[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      sink(file = "fileoutput.dae", append = TRUE, split = TRUE)
      cat ("\t\t\t\t\t\t", Nor, "\n")
      sink()
      
      # Nor_technique_commonの書き出し
      F_Nor_Tech_com (id)
      
      # vertices + polilystの書き出し
      poly_mate_cnt = 1 + 4 + 2
      vcnt = c(4, 4, 5, 4, 5, 4, 4)
      F_verti_polist (id)
    }
    
    else if (yanetype == 8) {
      ## 町家タイプの家型のモデリング
      # 町家タイプは平入りを基本とし，hiratumaが2の場合は妻入りに入れ替える
      if (hiratuma == 2) {
        xb2 = array(dim = c(4))
        yb2 = array(dim = c(4))
        xt2 = array(dim = c(4))
        yt2 = array(dim = c(4))
        for (j in 1:4) {
          xb2[j] = xb[j]
          yb2[j] = yb[j]
          xt2[j] = xt[j]
          yt2[j] = yt[j]
        }
        for (j in 1:3) {
          xb[j] = xb2[j+1]
          yb[j] = yb2[j+1]
          xt[j] = xt2[j+1]
          yt[j] = yt2[j+1]
        }
        xb[4] = xb2[1]
        yb[4] = yb2[1]
        xt[4] = xt2[1]
        yt[4] = yt2[1]
      }
      
      # zt値の変更（町家１階軒高）
      zt[1] = zb[1] + matiya1 / kansan
      zt[2] = zb[2] + matiya1 / kansan
      zt[3] = zb[3] + matiya1 / kansan
      zt[4] = zb[4] + matiya1 / kansan
            
      # 町家２階モデルの座標設定
      xbm = array(dim = c(4))
      ybm = array(dim = c(4))
      zbm = array(dim = c(4))
      xtm = array(dim = c(4))
      ytm = array(dim = c(4))
      ztm = array(dim = c(4))
      
      # 直線（辺）の傾きと切片
      a = array(dim = c(4))   # 直線（辺）の傾き 
      b = array(dim = c(4))   # 直線（辺）の切片
      
      # 直線S1（頂点1→2）～S4（頂点4→1）の式（１階壁面線）
      for (j in 1:4) {
        if (j == 4) {
          if ((xb[j] - xb[1]) == 0) {
            xb[j] = xb[1] + 0.00001
          }
          a[j] = (yb[j] - yb[1]) / (xb[j] - xb[1])
        } else {
          if ((xb[j] - xb[j+1]) == 0) {
            xb[j] = xb[j+1] + 0.00001
          }
          a[j] = (yb[j] - yb[j+1]) / (xb[j] - xb[j+1])
        }
        b[j] = yb[j] - xb[j] * a[j]
      }
      
      # 仮の切片の値の算出（xMaxの時のｙの値）
      xMax = xt[1]
      for (j in 2:4) {
        if (xMax < xt[j]) {
          xMax = xt[j]
        }
      }
      br = array(dim = c(4))
      for (j in 1:4) {
        br[j] = xMax * a[j] + b[j]
      }
      
      # 向かい合う直線の切片間の距離
      D = array(dim = c(4))
      D[1] = abs(br[1] - br[3])
      D[2] = abs(br[2] - br[4])
      D[3] = abs(br[3] - br[1])
      D[4] = abs(br[4] - br[2])
      
      # 直線S1（頂点1→2）と直線S3（頂点3→4）に平行な直線の式の切片
      B = array(dim = c(4, 2))    # 平行な２直線の切片
      Br = array(dim = c(2))      # xMaxの仮の切片
      bo = array(dim = c(4))      # 新しい切片の値
      # 直線S1－仮の切片による切片の差の比較
      B[1, 1] = br[1] + koutai2 / kansan * sqrt(a[1]^2 + 1)
      B[1, 2] = br[1] - koutai2 / kansan * sqrt(a[1]^2 + 1)
      # 新しい切片の値の設定
      if (D[1] > abs(B[1, 1] - br[3])) {
        bo[1] = b[1] + koutai2 / kansan * sqrt(a[1]^2 + 1)
      } else {
        bo[1] = b[1] - koutai2 / kansan * sqrt(a[1]^2 + 1)
      }
      # 直線S3－仮の切片による切片の差の比較
      B[3, 1] = br[3] + koutai2 / kansan * sqrt(a[3]^2 + 1)
      B[3, 2] = br[3] - koutai2 / kansan * sqrt(a[3]^2 + 1)
      # 新しい切片の値の設定
      if (D[3] > abs(B[3, 1] - br[1])) {
        bo[3] = b[3] + koutai2 / kansan * sqrt(a[3]^2 + 1)
      } else {
        bo[3] = b[3] - koutai2 / kansan * sqrt(a[3]^2 + 1)
      }
      # 直線S2と直線s4はそのまま
      bo[2] = b[2]
      bo[4] = b[4]
      
      # 町家２階モデルの４頂点のX･Y座標の算出
      for (j in 1:4) {
        if (j == 1) {
          xbm[1] = (bo[1] - bo[4]) / (a[4] - a[1])
        } else {
          xbm[j] = (bo[j] - bo[j-1]) / (a[j-1] - a[j])
        }
        xtm[j] = xbm[j]
        ybm[j] = xbm[j] * a[j] + bo[j]
        ytm[j] = ybm[j]
      }
      
      # 町家２階モデルの４頂点のZ座標の算出
      zbm[1] = zt[1] + koutai2 * incline / kansan
      zbm[2] = zt[2] + koutai2 * incline / kansan
      zbm[3] = zt[3] + koutai2 * incline / kansan
      zbm[4] = zt[4] + koutai2 * incline / kansan
      ztm[1] = zt[1] + matiya2 / kansan
      ztm[2] = zt[2] + matiya2 / kansan
      ztm[3] = zt[3] + matiya2 / kansan
      ztm[4] = zt[4] + matiya2 / kansan
      
      # 町家２階上面（小屋伏せ）用の座標設定
      xm[1] = xtm[1]
      xm[2] = xtm[2]
      xm[5] = xtm[3]
      xm[6] = xtm[4]
      xm[3] = (xm[1] + xm[6]) / 2
      xm[4] = (xm[2] + xm[5]) / 2
      xm[7] = (xm[2] + xm[5]) / 2
      xm[8] = (xm[1] + xm[6]) / 2
      ym[1] = ytm[1]
      ym[2] = ytm[2]
      ym[5] = ytm[3]
      ym[6] = ytm[4]
      ym[3] = (ym[1] + ym[6]) / 2
      ym[4] = (ym[2] + ym[5]) / 2
      ym[7] = (ym[2] + ym[5]) / 2
      ym[8] = (ym[1] + ym[6]) / 2
      zm[1] = ztm[1]
      zm[2] = ztm[2]
      zm[5] = ztm[3]
      zm[6] = ztm[4]
      
      # ２階妻側頂点の高さ（Z座標）を計算する
      l_s2 = sqrt((xm[2] - xm[5])^2 + (ym[2] - ym[5])^2)
      l_s4 = sqrt((xm[1] - xm[6])^2 + (ym[1] - ym[6])^2)
      zm[3] = (l_s4 / 2 * incline) + ((zm[1] + zm[6]) / 2)
      zm[4] = (l_s2 / 2 * incline) + ((zm[2] + zm[5]) / 2)
      zm[7] = (l_s2 / 2 * incline) + ((zm[2] + zm[5]) / 2)
      zm[8] = (l_s4 / 2 * incline) + ((zm[1] + zm[6]) / 2)
      
      # 町家１階上面（天井伏せ）用の座標設定
      xk[1] = xt[1]
      xk[2] = xt[2]
      xk[5] = xt[3]
      xk[6] = xt[4]
      xk[3] = xm[6]
      xk[4] = xm[5]
      xk[7] = xm[2]
      xk[8] = xm[1]
      yk[1] = yt[1]
      yk[2] = yt[2]
      yk[5] = yt[3]
      yk[6] = yt[4]
      yk[3] = ym[6]
      yk[4] = ym[5]
      yk[7] = ym[2]
      yk[8] = ym[1]
      zk[1] = zt[1]
      zk[2] = zt[2]
      zk[5] = zt[3]
      zk[6] = zt[4]
      zk[3] = zbm[4]
      zk[4] = zbm[3]
      zk[7] = zbm[2]
      zk[8] = zbm[1]
      
      # ID番号の設定（iをidに変更 → counterまでが建物本体）
      id = i
      
      # 町家タイプの建物モデリング
      ver_num = ((1 + 2 + 3) * 4 + 2 * 6) + ((1 + 2 + 2) * 4 + 2 * 5)
      count = ver_num * 3
      F_lib_geo_Pos (id, count)
      
      # １階底面頂点座標の書き出し
      F_bottom_plate (4, xb, yb, zb)
      
      # １階側面頂点座標の書き出し
      side1 = array(dim = c(2, 4))
      side2 = array(dim = c(2, 6))
      for (j in 1:4) {
        k = j + 1
        if (j == 4) { k = 1 }
        if (j == 1) {
          side1[1, 1] = paste("\t\t\t\t\t\t", xb[k], yb[k], zb[k])
          side1[1, 2] = paste("\t\t\t\t\t\t", xb[j], yb[j], zb[j])
          side1[1, 3] = paste("\t\t\t\t\t\t", xt[j], yt[j], zt[j])
          side1[1, 4] = paste("\t\t\t\t\t\t", xt[k], yt[k], zt[k])
        }
        if (j == 2) {
          side2[1, 1] = paste("\t\t\t\t\t\t", xb[k], yb[k], zb[k])
          side2[1, 2] = paste("\t\t\t\t\t\t", xb[j], yb[j], zb[j])
          side2[1, 3] = paste("\t\t\t\t\t\t", xt[j], yt[j], zt[j])
          side2[1, 4] = paste("\t\t\t\t\t\t", xk[7], yk[7], zk[7])
          side2[1, 5] = paste("\t\t\t\t\t\t", xk[4], yk[4], zk[4])
          side2[1, 6] = paste("\t\t\t\t\t\t", xt[k], yt[k], zt[k])
        }
        if (j == 3) {
          side1[2, 1] = paste("\t\t\t\t\t\t", xb[k], yb[k], zb[k])
          side1[2, 2] = paste("\t\t\t\t\t\t", xb[j], yb[j], zb[j])
          side1[2, 3] = paste("\t\t\t\t\t\t", xt[j], yt[j], zt[j])
          side1[2, 4] = paste("\t\t\t\t\t\t", xt[k], yt[k], zt[k])
        }
        if (j == 4) {
          side2[2, 1] = paste("\t\t\t\t\t\t", xb[k], yb[k], zb[k])
          side2[2, 2] = paste("\t\t\t\t\t\t", xb[j], yb[j], zb[j])
          side2[2, 3] = paste("\t\t\t\t\t\t", xt[j], yt[j], zt[j])
          side2[2, 4] = paste("\t\t\t\t\t\t", xk[3], yk[3], zk[3])
          side2[2, 5] = paste("\t\t\t\t\t\t", xk[8], yk[8], zk[8])
          side2[2, 6] = paste("\t\t\t\t\t\t", xt[k], yt[k], zt[k])
        }
      }
      for (j in 1:2){
        for (k in 1:4) {
          write (side1[j, k], "fileoutput.dae", append = TRUE)
        }
        for (k in 1:6) {
          write (side2[j, k], "fileoutput.dae", append = TRUE)
        }
      }
      
      # １階上面頂点座標の書き出し
      top = array(dim = c(3, 4))
      top[1, 1] = paste("\t\t\t\t\t\t", xk[2], yk[2], zk[2])
      top[1, 2] = paste("\t\t\t\t\t\t", xk[7], yk[7], zk[7])
      top[1, 3] = paste("\t\t\t\t\t\t", xk[8], yk[8], zk[8])
      top[1, 4] = paste("\t\t\t\t\t\t", xk[1], yk[1], zk[1])
      top[2, 1] = paste("\t\t\t\t\t\t", xk[7], yk[7], zk[7])
      top[2, 2] = paste("\t\t\t\t\t\t", xk[4], yk[4], zk[4])
      top[2, 3] = paste("\t\t\t\t\t\t", xk[3], yk[3], zk[3])
      top[2, 4] = paste("\t\t\t\t\t\t", xk[8], yk[8], zk[8])
      top[3, 1] = paste("\t\t\t\t\t\t", xk[6], yk[6], zk[6])
      top[3, 2] = paste("\t\t\t\t\t\t", xk[3], yk[3], zk[3])
      top[3, 3] = paste("\t\t\t\t\t\t", xk[4], yk[4], zk[4])
      top[3, 4] = paste("\t\t\t\t\t\t", xk[5], yk[5], zk[5])
      for (j in 1:3){
        for (k in 1:4) {
          write (top[j, k], "fileoutput.dae", append = TRUE)
        }
      }
      
      # ２階底面頂点座標の書き出し
      F_bottom_plate (4, xbm, ybm, zbm)
      
      # ２階側面頂点座標の書き出し
      sidem1 = array(dim = c(2, 4))
      sidem2 = array(dim = c(2, 5))
      for (j in 1:4) {
        k = j + 1
        if (j == 4) { k = 1 }
        if (j == 1) {
          sidem1[1, 1] = paste("\t\t\t\t\t\t", xbm[k], ybm[k], zbm[k])
          sidem1[1, 2] = paste("\t\t\t\t\t\t", xbm[j], ybm[j], zbm[j])
          sidem1[1, 3] = paste("\t\t\t\t\t\t", xtm[j], ytm[j], ztm[j])
          sidem1[1, 4] = paste("\t\t\t\t\t\t", xtm[k], ytm[k], ztm[k])
        }
        if (j == 2) {
          sidem2[1, 1] = paste("\t\t\t\t\t\t", xbm[k], ybm[k], zbm[k])
          sidem2[1, 2] = paste("\t\t\t\t\t\t", xbm[j], ybm[j], zbm[j])
          sidem2[1, 3] = paste("\t\t\t\t\t\t", xtm[j], ytm[j], ztm[j])
          sidem2[1, 4] = paste("\t\t\t\t\t\t", xm[4], ym[4], zm[4])
          sidem2[1, 5] = paste("\t\t\t\t\t\t", xtm[k], ytm[k], ztm[k])
        }
        if (j == 3) {
          sidem1[2, 1] = paste("\t\t\t\t\t\t", xbm[k], ybm[k], zbm[k])
          sidem1[2, 2] = paste("\t\t\t\t\t\t", xbm[j], ybm[j], zbm[j])
          sidem1[2, 3] = paste("\t\t\t\t\t\t", xtm[j], ytm[j], ztm[j])
          sidem1[2, 4] = paste("\t\t\t\t\t\t", xtm[k], ytm[k], ztm[k])
        }
        if (j == 4) {
          sidem2[2, 1] = paste("\t\t\t\t\t\t", xbm[k], ybm[k], zbm[k])
          sidem2[2, 2] = paste("\t\t\t\t\t\t", xbm[j], ybm[j], zbm[j])
          sidem2[2, 3] = paste("\t\t\t\t\t\t", xtm[j], ytm[j], ztm[j])
          sidem2[2, 4] = paste("\t\t\t\t\t\t", xm[8], ym[8], zm[8])
          sidem2[2, 5] = paste("\t\t\t\t\t\t", xtm[k], ytm[k], ztm[k])
        }
      }
      for (j in 1:2){
        for (k in 1:4) {
          write (sidem1[j, k], "fileoutput.dae", append = TRUE)
        }
        for (k in 1:5) {
          write (sidem2[j, k], "fileoutput.dae", append = TRUE)
        }
      }
      
      # ２階上面頂点座標の書き出し
      topm = array(dim = c(2, 4))
      topm[1, 1] = paste("\t\t\t\t\t\t", xm[8], ym[8], zm[8])
      topm[1, 2] = paste("\t\t\t\t\t\t", xm[7], ym[7], zm[7])
      topm[1, 3] = paste("\t\t\t\t\t\t", xm[2], ym[2], zm[2])
      topm[1, 4] = paste("\t\t\t\t\t\t", xm[1], ym[1], zm[1])
      topm[2, 1] = paste("\t\t\t\t\t\t", xm[4], ym[4], zm[4])
      topm[2, 2] = paste("\t\t\t\t\t\t", xm[3], ym[3], zm[3])
      topm[2, 3] = paste("\t\t\t\t\t\t", xm[6], ym[6], zm[6])
      topm[2, 4] = paste("\t\t\t\t\t\t", xm[5], ym[5], zm[5])
      for (j in 1:2){
        for (k in 1:4) {
          write (topm[j, k], "fileoutput.dae", append = TRUE)
        }
      }
      
      # Pos_technique_commonの書き出し
      F_Pos_Tech_com (id)
      
      # library_geometries(Normal)の書き出し
      F_lib_geo_Nor (id, count)
      
      ## 法線ベクトルの書き出し
      Nor = c()
      
      # １階底面法線ベクトルの算出
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xb[k], yb[k], zb[k])
        p2 = c(xb[j], yb[j], zb[j])
        p3 = c(xb[l], yb[l], zb[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      # １階側面法線ベクトルの算出
      for (j in 1:4) {
        l = j + 1
        if (l > 4) { l = 1 }
        # 平側の法線ベクトル
        if ((j == 1) || (j == 3)) {
          for (k in 1:4) {
            if (k == 1) {
              p1 = c(xt[j], yt[j], zt[j])
              p2 = c(xb[j], yb[j], zb[j])
              p3 = c(xb[l], yb[l], zb[l])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 2) {
              p1 = c(xb[j], yb[j], zb[j])
              p2 = c(xb[l], yb[l], zb[l])
              p3 = c(xt[l], yt[l], zt[l])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 3) {
              p1 = c(xb[l], yb[l], zb[l])
              p2 = c(xt[l], yt[l], zt[l])
              p3 = c(xt[j], yt[j], zt[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 4) {
              p1 = c(xt[l], yt[l], zt[l])
              p2 = c(xt[j], yt[j], zt[j])
              p3 = c(xb[j], yb[j], zb[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
          }
        }
        # 妻側の法線ベクトル
        if ((j == 2) || (j == 4)) {
          for (k in 1:6) {
            if (j == 2) {j73 = 7}
            if (j == 4) {j73 = 3}
            if (k == 1) {
              p1 = c(xt[j], yt[j], zt[j])
              p2 = c(xb[j], yb[j], zb[j])
              p3 = c(xb[l], yb[l], zb[l])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 2) {
              p1 = c(xb[j], yb[j], zb[j])
              p2 = c(xb[l], yb[l], zb[l])
              p3 = c(xt[l], yt[l], zt[l])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 3) {
              p1 = c(xb[l], yb[l], zb[l])
              p2 = c(xt[l], yt[l], zt[l])
              p3 = c(xk[j*2], yk[j*2], zk[j*2])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 4) {
              p1 = c(xt[l], yt[l], zt[l])
              p2 = c(xk[j*2], yk[j*2], zk[j*2])
              p3 = c(xk[j73], yk[j73], zk[j73])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 5) {
              p1 = c(xk[j*2], yk[j*2], zk[j*2])
              p2 = c(xk[j73], yk[j73], zk[j73])
              p3 = c(xb[j], yb[j], zb[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 6) {
              p1 = c(xk[j73], yk[j73], zk[j73])
              p2 = c(xt[j], yt[j], zt[j])
              p3 = c(xb[j], yb[j], zb[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
          }
        }
      }
      
      # １階上面１の法線ベクトルの算出
      for (j in 1:2) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 8}
        if (l > 2) {l =7}
        p1 = c(xk[k], yk[k], zk[k])
        p2 = c(xk[j], yk[j], zk[j])
        p3 = c(xk[l], yk[l], zk[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      for (j in 7:8) {
        k = j - 1
        l = j + 1
        if (k == 6) {k = 2}
        if (l > 8) {l = 1}
        p1 = c(xk[k], yk[k], zk[k])
        p2 = c(xk[j], yk[j], zk[j])
        p3 = c(xk[l], yk[l], zk[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      # １階上面２の法線ベクトルの算出
      for (j in 3:6) {
        k = j - 1
        l = j + 1
        if (k == 2) {k = 6}
        if (l > 6) {l = 3}
        p1 = c(xk[k], yk[k], zk[k])
        p2 = c(xk[j], yk[j], zk[j])
        p3 = c(xk[l], yk[l], zk[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      # １階上面３の法線ベクトルの算出
      for (j in 3:4) {
        k = j - 1
        l = j + 1
        if (k == 2) {k = 8}
        if (l > 4) {l =7}
        p1 = c(xk[k], yk[k], zk[k])
        p2 = c(xk[j], yk[j], zk[j])
        p3 = c(xk[l], yk[l], zk[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      for (j in 7:8) {
        k = j - 1
        l = j + 1
        if (k == 6) {k = 4}
        if (l > 8) {l = 3}
        p1 = c(xk[k], yk[k], zk[k])
        p2 = c(xk[j], yk[j], zk[j])
        p3 = c(xk[l], yk[l], zk[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      # ２階底面法線ベクトルの算出
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xbm[k], ybm[k], zbm[k])
        p2 = c(xbm[j], ybm[j], zbm[j])
        p3 = c(xbm[l], ybm[l], zbm[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      # ２階側面法線ベクトルの算出
      for (j in 1:4) {
        l = j + 1
        if (l > 4) { l = 1 }
        # 平側の法線ベクトル
        if ((j == 1) || (j == 3)) {
          for (k in 1:4) {
            if (k == 1) {
              p1 = c(xtm[j], ytm[j], ztm[j])
              p2 = c(xbm[j], ybm[j], zbm[j])
              p3 = c(xbm[l], ybm[l], zbm[l])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 2) {
              p1 = c(xbm[j], ybm[j], zbm[j])
              p2 = c(xbm[l], ybm[l], zbm[l])
              p3 = c(xtm[l], ytm[l], ztm[l])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 3) {
              p1 = c(xbm[l], ybm[l], zbm[l])
              p2 = c(xtm[l], ytm[l], ztm[l])
              p3 = c(xtm[j], ytm[j], ztm[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 4) {
              p1 = c(xtm[l], ytm[l], ztm[l])
              p2 = c(xtm[j], ytm[j], ztm[j])
              p3 = c(xbm[j], ybm[j], zbm[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
          }
        }
        # 妻側の法線ベクトル
        if ((j == 2) || (j == 4)) {
          for (k in 1:5) {
            if (k == 1) {
              p1 = c(xtm[j], ytm[j], ztm[j])
              p2 = c(xbm[j], ybm[j], zbm[j])
              p3 = c(xbm[l], ybm[l], zbm[l])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 2) {
              p1 = c(xbm[j], ybm[j], zbm[j])
              p2 = c(xbm[l], ybm[l], zbm[l])
              p3 = c(xtm[l], ytm[l], ztm[l])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 3) {
              p1 = c(xbm[l], ybm[l], zbm[l])
              p2 = c(xtm[l], ytm[l], ztm[l])
              p3 = c(xm[j*2], ym[j*2], zm[j*2])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 4) {
              p1 = c(xtm[l], ytm[l], ztm[l])
              p2 = c(xm[j*2], ym[j*2], zm[j*2])
              p3 = c(xtm[j], ytm[j], ztm[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (k == 5) {
              p1 = c(xm[j*2], ym[j*2], zm[j*2])
              p2 = c(xtm[j], ytm[j], ztm[j])
              p3 = c(xbm[j], ybm[j], zbm[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
          }
        }
      }
      
      # ２階上面１の法線ベクトルの算出
      for (j in 1:2) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 8}
        if (l > 2) {l =7}
        p1 = c(xm[k], ym[k], zm[k])
        p2 = c(xm[j], ym[j], zm[j])
        p3 = c(xm[l], ym[l], zm[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      for (j in 7:8) {
        k = j - 1
        l = j + 1
        if (k == 6) {k = 2}
        if (l > 8) {l = 1}
        p1 = c(xm[k], ym[k], zm[k])
        p2 = c(xm[j], ym[j], zm[j])
        p3 = c(xm[l], ym[l], zm[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      # ２階上面２の法線ベクトルの算出
      for (j in 3:6) {
        k = j - 1
        l = j + 1
        if (k == 2) {k = 6}
        if (l > 6) {l = 3}
        p1 = c(xm[k], ym[k], zm[k])
        p2 = c(xm[j], ym[j], zm[j])
        p3 = c(xm[l], ym[l], zm[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      sink(file = "fileoutput.dae", append = TRUE, split = TRUE)
      cat ("\t\t\t\t\t\t", Nor, "\n")
      sink()
      
      # Nor_technique_commonの書き出し
      F_Nor_Tech_com (id)
      
      # vertices + polilystの書き出し
      poly_mate_cnt = 1 + 4 + 3 + 1 + 4 + 2
      vcnt = c(4, 4, 6, 4, 6, 4, 4, 4, 4, 4, 5, 4, 5, 4, 4)
      F_verti_polist (id)
    }
    
    else {
      # 片流れ屋根の上面頂点Z座標の算出
      s = array(dim = c(4))
      if (yanetype == 4) {
        for (j in 1:4) {
          s[j] = sqrt((xb[j] - xb[j+1])^2 + (yb[j] - yb[j+1])^2)
          if (j == 4) {
            s[j] = sqrt((xb[j] - xb[1])^2 + (yb[j] - yb[1])^2)
          }
        }
        if (yanemuki == 1) {
          zt[3] = zt[3] + (incline * s[2])
          zt[4] = zt[4] + (incline * s[4])
        }
        if (yanemuki == 2) {
          zt[4] = zt[4] + (incline * s[3])
          zt[1] = zt[1] + (incline * s[1])
        }
        if (yanemuki == 3) {
          zt[1] = zt[1] + (incline * s[4])
          zt[2] = zt[2] + (incline * s[2])
        }
        if (yanemuki == 4) {
          zt[2] = zt[2] + (incline * s[1])
          zt[3] = zt[3] + (incline * s[3])
        }
      }

      # ID番号の設定（iをidに変更 → counterまでが建物本体）
      id = i
      
      # 切妻屋根以外の建物モデリング
      F_lib_geo_Pos (id, count)
      
      # 底面頂点座標の書き出し
      F_bottom_plate (vertex, xb, yb, zb)
      
      # 側面頂点座標の書き出し
      F_side_plate (vertex, xb, yb, zb, xt, yt, zt)
      
      # 上面頂点座標の書き出し
      F_top_plate (vertex, xt, yt, zt)
      
      # Pos_technique_commonの書き出し
      F_Pos_Tech_com (id)
      
      # library_geometries(Normal)の書き出し
      F_lib_geo_Nor (id, count)
      
      ## 法線ベクトルの書き出し
      Nor = c()
      
      # 底面法線ベクトルの算出
      for (j in 1:vertex) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = vertex}
        if (l > vertex) {l = 1}
        p1 = c(xb[k], yb[k], zb[k])
        p2 = c(xb[j], yb[j], zb[j])
        p3 = c(xb[l], yb[l], zb[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      # 側面法線ベクトルの算出
      for (j in 1:vertex) {
        l = j + 1
        if (l > vertex) {l = 1}
        for (k in 1:4) {
          if (k == 1) {
            p1 = c(xt[j], yt[j], zt[j])
            p2 = c(xb[j], yb[j], zb[j])
            p3 = c(xb[l], yb[l], zb[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 2) {
            p1 = c(xb[j], yb[j], zb[j])
            p2 = c(xb[l], yb[l], zb[l])
            p3 = c(xt[l], yt[l], zt[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 3) {
            p1 = c(xb[l], yb[l], zb[l])
            p2 = c(xt[l], yt[l], zt[l])
            p3 = c(xt[j], yt[j], zt[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 4) {
            p1 = c(xt[l], yt[l], zt[l])
            p2 = c(xt[j], yt[j], zt[j])
            p3 = c(xb[j], yb[j], zb[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
        }
      }
      
      # 上面法線ベクトルの算出
      for (j in 1:vertex) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = vertex}
        if (l > vertex) {l = 1}
        p1 = c(xt[k], yt[k], zt[k])
        p2 = c(xt[j], yt[j], zt[j])
        p3 = c(xt[l], yt[l], zt[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      sink(file = "fileoutput.dae", append = TRUE, split = TRUE)
      cat ("\t\t\t\t\t\t", Nor, "\n")
      sink()
      
      # Nor_technique_commonの書き出し
      F_Nor_Tech_com (id)
      
      # vertices + polilystの書き出し
      vcnt = c(vertex)
      for (j in 1:vertex) {
        vcnt = c(vcnt, 4)
      }
      vcnt = c(vcnt, vertex)
      F_verti_polist (id)
    }
    
    ## 屋根タイプ別に屋根モデリング
    # 屋根モデルのid設定用カウンター
    # yane_count = yane_count + 1
    if (yanetype != 0) {
      yane_count = yane_count + 1
    }
    
    ## 屋根頂点座標の算出（共通事項）
    # 直線（辺）の傾きと切片
    a = array(dim = c(4))   # 直線（辺）の傾き 
    b = array(dim = c(4))   # 直線（辺）の切片
    
    # 直線S1（頂点1→2）～S4（頂点4→1）の式
    for (j in 1:4) {
      if (j == 4) {
        if ((xb[j] - xb[1]) == 0) {
          xb[j] = xb[1] + 0.00001
        }
        a[j] = (yb[j] - yb[1]) / (xb[j] - xb[1])
      } else {
        if ((xb[j] - xb[j+1]) == 0) {
          xb[j] = xb[j+1] + 0.00001
        }
        a[j] = (yb[j] - yb[j+1]) / (xb[j] - xb[j+1])
      }
      b[j] = yb[j] - xb[j] * a[j]
    }
    
    # 直線に平行な直線の式（傾きは同じ）
    # hisashi，kerabaに応じて切片が変化
    # 基本はS1，S3の面に対して平入り
    d = c(hisashi, keraba, hisashi, keraba)
    # hiratumaスイッチが１の場合（平入り⇔妻入り）
    if (hiratuma == 1) {
      d = c(keraba, hisashi, keraba, hisashi)
    }
    # 寄棟屋根の場合
    if ((yanetype == 2) || (yanetype == 3)) {
      d = c(hisashi, hisashi, hisashi, hisashi)
    }
    # 片流れ屋根の場合，流れ方向が2，4の場合
    if (yanetype == 4) {
      if ((yanemuki == 2) || (yanemuki == 4)) {
        d = c(keraba, hisashi, keraba, hisashi)
      }
    }
    
    # 仮の切片の値の算出（xMaxの時のｙの値）
    xMax = xt[1]
    for (j in 2:4) {
      if (xMax < xt[j]) {
        xMax = xt[j]
      }
    }
    br = array(dim = c(4))
    for (j in 1:4) {
      br[j] = xMax * a[j] + b[j]
    }
    
    # 向かい合う直線の切片間の距離
    D = array(dim = c(4))
    D[1] = abs(br[1] - br[3])
    D[2] = abs(br[2] - br[4])
    D[3] = abs(br[3] - br[1])
    D[4] = abs(br[4] - br[2])
    
    # 直線S1（頂点1→2）～S4（頂点4→1）に平行な直線の式の切片
    B = array(dim = c(4, 2))
    bo = array(dim = c(4))
    # Br = array(dim = c(2))
    for (j in 1:2) {
      B[j, 1] = br[j] + d[j] / kansan * sqrt(a[j]^2 + 1)
      B[j, 2] = br[j] - d[j] / kansan * sqrt(a[j]^2 + 1)
      if (D[j] < abs(B[j, 1] - br[j+2])) {
        bo[j] = b[j] + d[j] / kansan * sqrt(a[j]^2 + 1)
      } else {
        bo[j] = b[j] - d[j] / kansan * sqrt(a[j]^2 + 1)
      }
    }
    for (j in 3:4) {
      B[j, 1] = br[j] + d[j] / kansan * sqrt(a[j]^2 + 1)
      B[j, 2] = br[j] - d[j] / kansan * sqrt(a[j]^2 + 1)
      if (D[j] < abs(B[j, 1] - br[j-2])) {
        bo[j] = b[j] + d[j] / kansan * sqrt(a[j]^2 + 1)
      } else {
        bo[j] = b[j] - d[j] / kansan * sqrt(a[j]^2 + 1)
      }
    }
    
    # 屋根モデルのための４頂点の座標の設定
    xo = array(dim = c(4))
    yo = array(dim = c(4))
    zo = array(dim = c(4))
    xr = array(dim = c(4))
    yr = array(dim = c(4))
    zr = array(dim = c(4))
    
    # 屋根モデルのための４頂点のX･Y座標の算出
    for (j in 1:4) {
      if (j == 1) {
        xo[1] = (bo[1] - bo[4]) / (a[4] - a[1])
      } else {
        xo[j] = (bo[j] - bo[j-1]) / (a[j-1] - a[j])
      }
      xr[j] = xo[j]
      yo[j] = xo[j] * a[j] + bo[j]
      yr[j] = yo[j]
    }
    
    # ID番号の設定（iをidに変更→counter以降の増加分が屋根）
    id = counter + yane_count
    
    # 切妻屋根のモデリング
    if (yanetype == 1) {
      xo1 = array(dim = c(4))
      yo1 = array(dim = c(4))
      zo1 = array(dim = c(4))
      xo2 = array(dim = c(4))
      yo2 = array(dim = c(4))
      zo2 = array(dim = c(4))
      xr1 = array(dim = c(4))
      yr1 = array(dim = c(4))
      zr1 = array(dim = c(4))
      xr2 = array(dim = c(4))
      yr2 = array(dim = c(4))
      zr2 = array(dim = c(4))
      
      # 軒下端の高さ（Z座標）
      nh = hisashi * incline / kansan
      zo1[1] = zt[1] - nh
      zo1[2] = zt[2] - nh
      zo2[1] = zt[3] - nh
      zo2[2] = zt[4] - nh
      
      # yaneatuによる垂直方向の厚さ（軒側）
      kh = yaneatu / sqrt(1 + incline^2) / kansan
      # yaneatuによる垂直方向の厚さ（棟側）
      rh = yaneatu * sqrt(1 + incline^2) / kansan
      
      # 軒上端の高さ（Z座標）
      zr1[1] = zo1[1] + kh
      zr1[2] = zo1[2] + kh
      zr2[1] = zo2[1] + kh
      zr2[2] = zo2[2] + kh
      
      # 棟頂点高さの計算
      zo1[3] = zk[7]
      zo1[4] = zk[8]
      zo2[3] = zk[3]
      zo2[4] = zk[4]
      zr1[3] = zo1[3] + rh
      zr1[4] = zo1[4] + rh
      zr2[3] = zo2[3] + rh
      zr2[4] = zo2[4] + rh
        
      # 軒頂点座標の計算
      xo1[1] = xo[1]
      yo1[1] = yo[1]
      xo1[2] = xo[2]
      yo1[2] = yo[2]
      xo1[3] = (xo[2] + xo[3]) / 2
      yo1[3] = (yo[2] + yo[3]) / 2
      xo1[4] = (xo[1] + xo[4]) / 2
      yo1[4] = (yo[1] + yo[4]) / 2
      
      xo2[1] = xo[3]
      yo2[1] = yo[3]
      xo2[2] = xo[4]
      yo2[2] = yo[4]
      xo2[3] = (xo[1] + xo[4]) / 2
      yo2[3] = (yo[1] + yo[4]) / 2
      xo2[4] = (xo[2] + xo[3]) / 2
      yo2[4] = (yo[2] + yo[3]) / 2
        
      p1 = c(xo1[4], yo1[4], zo1[4])
      p2 = c(xo1[1], yo1[1], zo1[1])
      p3 = c(xo1[2], yo1[2], zo1[2])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr1[1] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 軒側のX座標
      yr1[1] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 軒側のY座標
      p1 = c(xo1[1], yo1[1], zo1[1])
      p2 = c(xo1[2], yo1[2], zo1[2])
      p3 = c(xo1[3], yo1[3], zo1[3])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr1[2] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 軒側のX座標
      yr1[2] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 軒側のY座標
      
      p1 = c(xo2[4], yo2[4], zo2[4])
      p2 = c(xo2[1], yo2[1], zo2[1])
      p3 = c(xo2[2], yo2[2], zo2[2])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr2[1] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 軒側のX座標
      yr2[1] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 軒側のY座標
      p1 = c(xo2[1], yo2[1], zo2[1])
      p2 = c(xo2[2], yo2[2], zo2[2])
      p3 = c(xo2[3], yo2[3], zo2[3])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr2[2] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 軒側のX座標
      yr2[2] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 軒側のY座標
      
      xr1[3] = (xr1[2] + xr2[1]) / 2
      yr1[3] = (yr1[2] + yr2[1]) / 2
      xr1[4] = (xr1[1] + xr2[2]) / 2
      yr1[4] = (yr1[1] + yr2[2]) / 2
      xr2[3] = (xr1[1] + xr2[2]) / 2
      yr2[3] = (yr1[1] + yr2[2]) / 2
      xr2[4] = (xr1[2] + xr2[1]) / 2
      yr2[4] = (yr1[2] + yr2[1]) / 2
      
      # 屋根モデルのモデリング(ID設定)
      ver_num = (1 + 4 + 1) * 2 * 4
      count = ver_num * 3
      F_lib_geo_Pos (id, count)
      
      # 屋根底面頂点座標の書き出し
      F_bottom_plate (4, xo1, yo1, zo1)
      F_bottom_plate (4, xo2, yo2, zo2)
      
      # 側面頂点座標の書き出し
      F_side_plate (4, xo1, yo1, zo1, xr1, yr1, zr1)
      F_side_plate (4, xo2, yo2, zo2, xr2, yr2, zr2)
      
      # 屋根上面頂点座標の書き出し
      F_top_plate (4, xr1, yr1, zr1)
      F_top_plate (4, xr2, yr2, zr2)
      
      # Pos_technique_commonの書き出し
      F_Pos_Tech_com (id)
      
      # library_geometries(Normal)の書き出し
      F_lib_geo_Nor (id, count)
      
      ## 法線ベクトルの書き出し
      Nor = c()
      
      # 底面法線ベクトルの算出
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xo1[k], yo1[k], zo1[k])
        p2 = c(xo1[j], yo1[j], zo1[j])
        p3 = c(xo1[l], yo1[l], zo1[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xo2[k], yo2[k], zo2[k])
        p2 = c(xo2[j], yo2[j], zo2[j])
        p3 = c(xo2[l], yo2[l], zo2[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      # 側面法線ベクトルの算出
      for (j in 1:4) {
        l = j + 1
        if (l > 4) {l = 1}
        for (k in 1:4) {
          if (k == 1) {
            p1 = c(xr1[j], yr1[j], zr1[j])
            p2 = c(xo1[j], yo1[j], zo1[j])
            p3 = c(xo1[l], yo1[l], zo1[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 2) {
            p1 = c(xo1[j], yo1[j], zo1[j])
            p2 = c(xo1[l], yo1[l], zo1[l])
            p3 = c(xr1[l], yr1[l], zr1[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 3) {
            p1 = c(xo1[l], yo1[l], zo1[l])
            p2 = c(xr1[l], yr1[l], zr1[l])
            p3 = c(xr1[j], yr1[j], zr1[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 4) {
            p1 = c(xr1[l], yr1[l], zr1[l])
            p2 = c(xr1[j], yr1[j], zr1[j])
            p3 = c(xo1[j], yo1[j], zo1[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
        }
      }
      for (j in 1:4) {
        l = j + 1
        if (l > 4) {l = 1}
        for (k in 1:4) {
          if (k == 1) {
            p1 = c(xr2[j], yr2[j], zr2[j])
            p2 = c(xo2[j], yo2[j], zo2[j])
            p3 = c(xo2[l], yo2[l], zo2[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 2) {
            p1 = c(xo2[j], yo2[j], zo2[j])
            p2 = c(xo2[l], yo2[l], zo2[l])
            p3 = c(xr2[l], yr2[l], zr2[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 3) {
            p1 = c(xo2[l], yo2[l], zo2[l])
            p2 = c(xr2[l], yr2[l], zr2[l])
            p3 = c(xr2[j], yr2[j], zr2[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 4) {
            p1 = c(xr2[l], yr2[l], zr2[l])
            p2 = c(xr2[j], yr2[j], zr2[j])
            p3 = c(xo2[j], yo2[j], zo2[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
        }
      }
      # 上面法線ベクトルの算出
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xr1[k], yr1[k], zr1[k])
        p2 = c(xr1[j], yr1[j], zr1[j])
        p3 = c(xr1[l], yr1[l], zr1[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xr2[k], yr2[k], zr2[k])
        p2 = c(xr2[j], yr2[j], zr2[j])
        p3 = c(xr2[l], yr2[l], zr2[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      sink(file = "fileoutput.dae", append = TRUE, split = TRUE)
      cat ("\t\t\t\t\t\t", Nor, "\n")
      sink()
      
      # Nor_technique_commonの書き出し
      F_Nor_Tech_com (id)
      
      # vertices + polilystの書き出し
      poly_mate_cnt = (1 + 4 + 1) * 2
      vcnt = ""
      for (j in 1:poly_mate_cnt) {
        vcnt = c(vcnt, 4)
      }
      F_verti_polist (id)
    }
    
    # 寄棟屋根のモデリング
    if (yanetype == 2) {
      # 軒下端の高さ（Z座標）
      zo[1] = zt[1]
      zo[2] = zt[2]
      zo[3] = zt[3]
      zo[4] = zt[4]
      
      # yaneatuによる垂直方向の厚さ（軒側）
      kh = yaneatu / sqrt(1 + incline^2) / kansan
      
      # 寄棟屋根の平側と妻側をチェックする
      l_s1 = sqrt((xo[1] - xo[2])^2 + (yo[1] - yo[2])^2)
      l_s2 = sqrt((xo[2] - xo[3])^2 + (yo[2] - yo[3])^2)
      l_s3 = sqrt((xo[3] - xo[4])^2 + (yo[3] - yo[4])^2)
      l_s4 = sqrt((xo[4] - xo[1])^2 + (yo[4] - yo[1])^2)
      # 直線S1･s3が直線S2･s4より小さい場合，平側と妻側を入れ替える
      if ((l_s1 + l_s3) < (l_s2 + l_s4)) {
        xo2 = array(dim = c(4))
        yo2 = array(dim = c(4))
        for (j in 1:4) {
          xo2[j] = xo[j]
          yo2[j] = yo[j]
        }
        for (j in 1:3) {
          xo[j] = xo2[j+1]
          yo[j] = yo2[j+1]
        }
        xo[4] = xo2[1]
        yo[4] = yo2[1]
      }
      
      # 軒上端の高さ（Z座標）
      zr[1] = zo[1] + kh
      zr[2] = zo[2] + kh
      zr[3] = zo[3] + kh
      zr[4] = zo[4] + kh
      
      ## 寄棟屋根の軒上端座標の計算
      xr2 = array(dim = c(4))
      yr2 = array(dim = c(4))
      
      # 軒上端の高さ計算用の仮の高さ（Z座標）
      zr2 = array(dim = c(4))
      zr2[1] = l_s4 * incline + zo[1]
      zr2[2] = l_s2 * incline + zo[2]
      zr2[3] = l_s2 * incline + zo[3]
      zr2[4] = l_s4 * incline + zo[4]
      
      # 平側の軒の出を計算する
      p1 = c(xo[4], yo[4], zr2[4])
      p2 = c(xo[1], yo[1], zo[1])
      p3 = c(xo[2], yo[2], zo[2])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr2[1] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 平側軒のX座標
      yr2[1] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 平側軒のY座標
      p1 = c(xo[1], yo[1], zo[1])
      p2 = c(xo[2], yo[2], zo[2])
      p3 = c(xo[3], yo[3], zr2[3])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr2[2] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 平側軒のX座標
      yr2[2] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 平側軒のY座標
      p1 = c(xo[2], yo[2], zr2[2])
      p2 = c(xo[3], yo[3], zo[3])
      p3 = c(xo[4], yo[4], zo[4])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr2[3] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xo[3]  # 平側軒のX座標
      yr2[3] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yo[3]  # 平側軒のY座標
      p1 = c(xo[3], yo[3], zo[3])
      p2 = c(xo[4], yo[4], zo[4])
      p3 = c(xo[1], yo[1], zr2[1])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr2[4] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xo[4]  # 平側軒のX座標
      yr2[4] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yo[4]  # 平側軒のY座標
      
      # 軒上端の高さ計算用の仮の高さ（Z座標）
      zr2[1] = l_s1 * incline + zo[1]
      zr2[2] = l_s3 * incline + zo[2]
      zr2[3] = l_s3 * incline + zo[3]
      zr2[4] = l_s1 * incline + zo[4]
      
      # 妻側の軒の出を計算する
      p1 = c(xr2[1], yr2[1], zr2[1])
      p2 = c(xr2[2], yr2[2], zo[2])
      p3 = c(xr2[3], yr2[3], zo[3])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr[2] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xr2[2]  # 妻側軒のX座標
      yr[2] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yr2[2]  # 妻側軒のY座標
      p1 = c(xr2[2], yr2[2], zo[2])
      p2 = c(xr2[3], yr2[3], zo[3])
      p3 = c(xr2[4], yr2[4], zr2[4])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr[3] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xr2[3]  # 妻側軒のX座標
      yr[3] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yr2[3]  # 妻側軒のY座標
      p1 = c(xr2[3], yr2[3], zr2[3])
      p2 = c(xr2[4], yr2[4], zo[4])
      p3 = c(xr2[1], yr2[1], zo[1])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr[4] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xr2[4]  # 妻側軒のX座標
      yr[4] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yr2[4]  # 妻側軒のY座標
      p1 = c(xr2[4], yr2[4], zo[4])
      p2 = c(xr2[1], yr2[1], zo[1])
      p3 = c(xr2[2], yr2[2], zr2[2])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr[1] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xr2[1]  # 妻側軒のX座標
      yr[1] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yr2[1]  # 妻側軒のY座標
      
      # 棟頂点座標の計算
      xt2 = array(dim = c(2))       # 妻側の中点座標用
      yt2 = array(dim = c(2))
      xm2 = array(dim = c(4))       # 棟頂点の座標用
      ym2 = array(dim = c(4))
      zm2 = array(dim = c(4))
      # 妻側の中点計算
      xt2[1] = (xr[1] + xr[4]) / 2
      yt2[1] = (yr[1] + yr[4]) / 2
      xt2[2] = (xr[2] + xr[3]) / 2
      yt2[2] = (yr[2] + yr[3]) / 2
      # 棟頂点の座標計算
      xm2[1] = (xt2[1] * 3 + xt2[2]) / 4
      ym2[1] = (yt2[1] * 3 + yt2[2]) / 4
      xm2[2] = (xt2[2] * 3 + xt2[1]) / 4
      ym2[2] = (yt2[2] * 3 + yt2[1]) / 4
      xm2[3] = (xt2[2] * 3 + xt2[1]) / 4
      ym2[3] = (yt2[2] * 3 + yt2[1]) / 4
      xm2[4] = (xt2[1] * 3 + xt2[2]) / 4
      ym2[4] = (yt2[1] * 3 + yt2[2]) / 4
      zm2[1] = sqrt((xt2[1] - xm2[1])^2 + (yt2[1] - ym2[1])^2) * incline + (zr[1] + zr[4]) / 2
      zm2[2] = sqrt((xt2[2] - xm2[2])^2 + (yt2[2] - ym2[2])^2) * incline + (zr[2] + zr[3]) / 2
      zm2[3] = sqrt((xt2[2] - xm2[2])^2 + (yt2[2] - ym2[2])^2) * incline + (zr[2] + zr[3]) / 2
      zm2[4] = sqrt((xt2[1] - xm2[1])^2 + (yt2[1] - ym2[1])^2) * incline + (zr[1] + zr[4]) / 2
      
      # 屋根モデルのモデリング(ID設定)
      ver_num = (1 + 4 + 4) * 4
      count = ver_num * 3
      F_lib_geo_Pos (id, count)
      
      # 屋根底面頂点座標の書き出し
      F_bottom_plate (vertex, xo, yo, zo)
      
      # 側面頂点座標の書き出し
      F_side_plate (vertex, xo, yo, zo, xr, yr, zr)
      
      # 屋根上面頂点座標の書き出し
      F_side_plate (vertex, xr, yr, zr, xm2, ym2, zm2)
      
      # Pos_technique_commonの書き出し
      F_Pos_Tech_com (id)
      
      # library_geometries(Normal)の書き出し
      F_lib_geo_Nor (id, count)
      
      ## 法線ベクトルの書き出し
      Nor = c()
      
      # 底面法線ベクトルの算出
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = vertex}
        if (l > vertex) {l = 1}
        p1 = c(xo[k], yo[k], zo[k])
        p2 = c(xo[j], yo[j], zo[j])
        p3 = c(xo[l], yo[l], zo[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      # 側面法線ベクトルの算出
      for (j in 1:4) {
        l = j + 1
        if (l > vertex) {l = 1}
        for (k in 1:4) {
          if (k == 1) {
            p1 = c(xr[j], yr[j], zr[j])
            p2 = c(xo[j], yo[j], zo[j])
            p3 = c(xo[l], yo[l], zo[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 2) {
            p1 = c(xo[j], yo[j], zo[j])
            p2 = c(xo[l], yo[l], zo[l])
            p3 = c(xr[l], yr[l], zr[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 3) {
            p1 = c(xo[l], yo[l], zo[l])
            p2 = c(xr[l], yr[l], zr[l])
            p3 = c(xr[j], yr[j], zr[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 4) {
            p1 = c(xr[l], yr[l], zr[l])
            p2 = c(xr[j], yr[j], zr[j])
            p3 = c(xo[j], yo[j], zo[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
        }
      }
      
      # 上面法線ベクトルの算出
      for (j in 1:4) {
        l = j + 1
        if (l > 4) {l = 1}
        for (k in 1:4) {
          if (k == 1) {
            p1 = c(xm2[j], ym2[j], zm2[j])
            p2 = c(xr[j], yr[j], zr[j])
            p3 = c(xr[l], yr[l], zr[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 2) {
            p1 = c(xr[j], yr[j], zr[j])
            p2 = c(xr[l], yr[l], zr[l])
            p3 = c(xm2[l], ym2[l], zm2[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 3) {
            if (j == 1 || j == 3) {
              p1 = c(xr[l], yr[l], zr[l])
              p2 = c(xm2[l], ym2[l], zm2[l])
              p3 = c(xm2[j], ym2[j], zm2[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (j == 2 || j == 4) {
              p1 = c(xr[l], yr[l], zr[l])
              p2 = c(xm2[l], ym2[l], zm2[l])
              p3 = c(xr[j], yr[j], zr[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
          }
          if (k == 4) {
            if (j == 1 || j == 3) {
              p1 = c(xm2[l], ym2[l], zm2[l])
              p2 = c(xm2[j], ym2[j], zm2[j])
              p3 = c(xr[j], yr[j], zr[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (j == 2 || j == 4) {
              p1 = c(xr[l], yr[l], zr[l])
              p2 = c(xm2[j], ym2[j], zm2[j])
              p3 = c(xr[j], yr[j], zr[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
          }
        }
      }
      
      sink(file = "fileoutput.dae", append = TRUE, split = TRUE)
      cat ("\t\t\t\t\t\t", Nor, "\n")
      sink()
      
      # Nor_technique_commonの書き出し
      F_Nor_Tech_com (id)
      
      # vertices + polilystの書き出し
      poly_mate_cnt = 1 + 4 + 4
      vcnt = ""
      for (j in 1:poly_mate_cnt) {
        vcnt = c(vcnt, 4)
      }
      F_verti_polist (id)
    }
    
    # 入母屋屋根のモデリング
    if (yanetype == 3) {
      # 軒下端の高さ（Z座標）
      zo[1] = zt[1]
      zo[2] = zt[2]
      zo[3] = zt[3]
      zo[4] = zt[4]
      
      # yaneatuによる垂直方向の厚さ（軒側）
      kh = yaneatu / sqrt(1 + incline^2) / kansan
      
      # 入母屋屋根の平側と妻側をチェックする
      l_s1 = sqrt((xo[1] - xo[2])^2 + (yo[1] - yo[2])^2)
      l_s2 = sqrt((xo[2] - xo[3])^2 + (yo[2] - yo[3])^2)
      l_s3 = sqrt((xo[3] - xo[4])^2 + (yo[3] - yo[4])^2)
      l_s4 = sqrt((xo[4] - xo[1])^2 + (yo[4] - yo[1])^2)
      # 直線S1･s3が直線S2･s4より小さい場合，平側と妻側を入れ替える
      if ((l_s1 + l_s3) < (l_s2 + l_s4)) {
        xo2 = array(dim = c(4))
        yo2 = array(dim = c(4))
        for (j in 1:4) {
          xo2[j] = xo[j]
          yo2[j] = yo[j]
        }
        for (j in 1:3) {
          xo[j] = xo2[j+1]
          yo[j] = yo2[j+1]
        }
        xo[4] = xo2[1]
        yo[4] = yo2[1]
      }
      
      # 軒上端の高さ（Z座標）
      zr[1] = zo[1] + kh
      zr[2] = zo[2] + kh
      zr[3] = zo[3] + kh
      zr[4] = zo[4] + kh
      
      ## 入母屋屋根の軒上端座標の計算
      xr2 = array(dim = c(4))
      yr2 = array(dim = c(4))
      
      # 軒上端の高さ計算用の仮の高さ（Z座標）
      zr2 = array(dim = c(4))
      zr2[1] = l_s4 * incline + zo[1]
      zr2[2] = l_s2 * incline + zo[2]
      zr2[3] = l_s2 * incline + zo[3]
      zr2[4] = l_s4 * incline + zo[4]
      
      # 平側の軒の出を計算する
      # p1 = array (dim = c(3))
      p1 = c(xo[4], yo[4], zr2[4])
      p2 = c(xo[1], yo[1], zo[1])
      p3 = c(xo[2], yo[2], zo[2])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr2[1] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 平側軒のX座標
      yr2[1] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 平側軒のY座標
      p1 = c(xo[1], yo[1], zo[1])
      p2 = c(xo[2], yo[2], zo[2])
      p3 = c(xo[3], yo[3], zr2[3])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr2[2] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 平側軒のX座標
      yr2[2] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 平側軒のY座標
      p1 = c(xo[2], yo[2], zr2[2])
      p2 = c(xo[3], yo[3], zo[3])
      p3 = c(xo[4], yo[4], zo[4])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr2[3] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xo[3]  # 平側軒のX座標
      yr2[3] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yo[3]  # 平側軒のY座標
      p1 = c(xo[3], yo[3], zo[3])
      p2 = c(xo[4], yo[4], zo[4])
      p3 = c(xo[1], yo[1], zr2[1])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr2[4] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xo[4]  # 平側軒のX座標
      yr2[4] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yo[4]  # 平側軒のY座標
      
      # 軒上端の高さ計算用の仮の高さ（Z座標）
      zr2[1] = l_s1 * incline + zo[1]
      zr2[2] = l_s3 * incline + zo[2]
      zr2[3] = l_s3 * incline + zo[3]
      zr2[4] = l_s1 * incline + zo[4]
      
      # 妻側の軒の出を計算する
      p1 = c(xr2[1], yr2[1], zr2[1])
      p2 = c(xr2[2], yr2[2], zo[2])
      p3 = c(xr2[3], yr2[3], zo[3])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr[2] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xr2[2]  # 妻側軒のX座標
      yr[2] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yr2[2]  # 妻側軒のY座標
      p1 = c(xr2[2], yr2[2], zo[2])
      p2 = c(xr2[3], yr2[3], zo[3])
      p3 = c(xr2[4], yr2[4], zr2[4])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr[3] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xr2[3]  # 妻側軒のX座標
      yr[3] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yr2[3]  # 妻側軒のY座標
      p1 = c(xr2[3], yr2[3], zr2[3])
      p2 = c(xr2[4], yr2[4], zo[4])
      p3 = c(xr2[1], yr2[1], zo[1])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr[4] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xr2[4]  # 妻側軒のX座標
      yr[4] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yr2[4]  # 妻側軒のY座標
      p1 = c(xr2[4], yr2[4], zo[4])
      p2 = c(xr2[1], yr2[1], zo[1])
      p3 = c(xr2[2], yr2[2], zr2[2])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr[1] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xr2[1]  # 妻側軒のX座標
      yr[1] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yr2[1]  # 妻側軒のY座標
      
      # 棟頂点座標の計算
      xt2 = array(dim = c(2))       # 妻側の中点座標用
      yt2 = array(dim = c(2))
      xm2 = array(dim = c(4))       # 仮の棟頂点の座標用
      ym2 = array(dim = c(4))
      zm2 = array(dim = c(4))
      xm3 = array(dim = c(4))       # 棟頂点（破風面下両端）の座標用
      ym3 = array(dim = c(4))
      zm3 = array(dim = c(4))
      xm4 = array(dim = c(4))        # 棟頂点（破風上端）の座標用
      ym4 = array(dim = c(4))
      zm4 = array(dim = c(4))
      # 妻側の中点計算
      xt2[1] = (xr[1] + xr[4]) / 2
      yt2[1] = (yr[1] + yr[4]) / 2
      xt2[2] = (xr[2] + xr[3]) / 2
      yt2[2] = (yr[2] + yr[3]) / 2
      # 仮の棟頂点の座標計算
      xm2[1] = (xt2[1] * 3 + xt2[2] * 2) / 5
      ym2[1] = (yt2[1] * 3 + yt2[2] * 2) / 5
      xm2[2] = (xt2[2] * 3 + xt2[1] * 2) / 5
      ym2[2] = (yt2[2] * 3 + yt2[1] * 2) / 5
      xm2[3] = (xt2[2] * 3 + xt2[1] * 2) / 5
      ym2[3] = (yt2[2] * 3 + yt2[1] * 2) / 5
      xm2[4] = (xt2[1] * 3 + xt2[2] * 2) / 5
      ym2[4] = (yt2[1] * 3 + yt2[2] * 2) / 5
      zm2[1] = sqrt((xt2[1] - xm2[1])^2 + (yt2[1] - ym2[1])^2) * incline + (zr[1] + zr[4]) / 2
      zm2[2] = sqrt((xt2[2] - xm2[2])^2 + (yt2[2] - ym2[2])^2) * incline + (zr[2] + zr[3]) / 2
      zm2[3] = sqrt((xt2[2] - xm2[2])^2 + (yt2[2] - ym2[2])^2) * incline + (zr[2] + zr[3]) / 2
      zm2[4] = sqrt((xt2[1] - xm2[1])^2 + (yt2[1] - ym2[1])^2) * incline + (zr[1] + zr[4]) / 2
      # 棟頂点（破風面下両端）の座標計算
      xm3[1] = (xm2[1] + xr[1]) / 2
      ym3[1] = (ym2[1] + yr[1]) / 2
      xm3[2] = (xm2[2] + xr[2]) / 2
      ym3[2] = (ym2[2] + yr[2]) / 2
      xm3[3] = (xm2[3] + xr[3]) / 2
      ym3[3] = (ym2[3] + yr[3]) / 2
      xm3[4] = (xm2[4] + xr[4]) / 2
      ym3[4] = (ym2[4] + yr[4]) / 2
      zm3[1] = (zr[1] + zm2[1]) / 2
      zm3[2] = (zr[2] + zm2[2]) / 2
      zm3[3] = (zr[3] + zm2[3]) / 2
      zm3[4] = (zr[4] + zm2[4]) / 2
      # 棟頂点（破風上端）の座標計算
      xm4[1] = (xm3[1] + xm3[4]) / 2
      xm4[2] = (xm3[2] + xm3[3]) / 2
      xm4[3] = (xm3[2] + xm3[3]) / 2
      xm4[4] = (xm3[1] + xm3[4]) / 2
      ym4[1] = (ym3[1] + ym3[4]) / 2
      ym4[2] = (ym3[2] + ym3[3]) / 2
      ym4[3] = (ym3[2] + ym3[3]) / 2
      ym4[4] = (ym3[1] + ym3[4]) / 2
      zm4[1] = (xm4[1] - xm2[2]) * (zm2[1] - zm2[2]) / (xm2[1] - xm2[2]) + zm2[2]
      zm4[2] = (xm4[2] - xm2[1]) * (zm2[2] - zm2[1]) / (xm2[2] - xm2[1]) + zm2[1]
      zm4[3] = (xm4[3] - xm2[4]) * (zm2[3] - zm2[4]) / (xm2[3] - xm2[4]) + zm2[4]
      zm4[4] = (xm4[4] - xm2[3]) * (zm2[4] - zm2[3]) / (xm2[4] - xm2[3]) + zm2[3]
      
      # 屋根モデルのモデリング(ID設定)
      ver_num = (1 + 4 + 4 + 4) * 4
      count = ver_num * 3
      F_lib_geo_Pos (id, count)
      
      # 屋根底面頂点座標の書き出し
      F_bottom_plate (vertex, xo, yo, zo)
      
      # 側面頂点座標の書き出し
      F_side_plate (vertex, xo, yo, zo, xr, yr, zr)
      
      # 屋根寄棟面頂点座標の書き出し
      F_side_plate (vertex, xr, yr, zr, xm3, ym3, zm3)
      
      # 屋根切妻面頂点座標の書き出し
      F_side_plate (vertex, xm3, ym3, zm3, xm4, ym4, zm4)
      
      # Pos_technique_commonの書き出し
      F_Pos_Tech_com (id)
      
      # library_geometries(Normal)の書き出し
      F_lib_geo_Nor (id, count)
      
      ## 法線ベクトルの書き出し
      Nor = c()
      
      # 底面法線ベクトルの算出
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xo[k], yo[k], zo[k])
        p2 = c(xo[j], yo[j], zo[j])
        p3 = c(xo[l], yo[l], zo[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      # 側面法線ベクトルの算出
      for (j in 1:4) {
        l = j + 1
        if (l > 4) {l = 1}
        for (k in 1:4) {
          if (k == 1) {
            p1 = c(xr[j], yr[j], zr[j])
            p2 = c(xo[j], yo[j], zo[j])
            p3 = c(xo[l], yo[l], zo[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 2) {
            p1 = c(xo[j], yo[j], zo[j])
            p2 = c(xo[l], yo[l], zo[l])
            p3 = c(xr[l], yr[l], zr[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 3) {
            p1 = c(xo[l], yo[l], zo[l])
            p2 = c(xr[l], yr[l], zr[l])
            p3 = c(xr[j], yr[j], zr[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 4) {
            p1 = c(xr[l], yr[l], zr[l])
            p2 = c(xr[j], yr[j], zr[j])
            p3 = c(xo[j], yo[j], zo[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
        }
      }
      
      # 寄棟面法線ベクトルの算出
      for (j in 1:4) {
        l = j + 1
        if (l > 4) {l = 1}
        for (k in 1:4) {
          if (k == 1) {
            p1 = c(xm3[j], ym3[j], zm3[j])
            p2 = c(xr[j], yr[j], zr[j])
            p3 = c(xr[l], yr[l], zr[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 2) {
            p1 = c(xr[j], yr[j], zr[j])
            p2 = c(xr[l], yr[l], zr[l])
            p3 = c(xm3[l], ym3[l], zm3[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 3) {
            p1 = c(xr[l], yr[l], zr[l])
            p2 = c(xm3[l], ym3[l], zm3[l])
            p3 = c(xm3[j], ym3[j], zm3[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 4) {
            p1 = c(xm3[l], ym3[l], zm3[l])
            p2 = c(xm3[j], ym3[j], zm3[j])
            p3 = c(xr[j], yr[j], zr[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
        }
      }
      
      # 切妻面法線ベクトルの算出
      for (j in 1:4) {
        l = j + 1
        if (l > 4) {l = 1}
        for (k in 1:4) {
          if (k == 1) {
            p1 = c(xm4[j], ym4[j], zm4[j])
            p2 = c(xm3[j], ym3[j], zm3[j])
            p3 = c(xm3[l], ym3[l], zm3[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 2) {
            p1 = c(xm3[j], ym3[j], zm3[j])
            p2 = c(xm3[l], ym3[l], zm3[l])
            p3 = c(xm4[l], ym4[l], zm4[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 3) {
            if (j == 1 || j == 3) {
              p1 = c(xr[l], yr[l], zr[l])
              p2 = c(xm2[l], ym2[l], zm2[l])
              p3 = c(xm2[j], ym2[j], zm2[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (j == 2 || j == 4) {
              p1 = c(xr[l], yr[l], zr[l])
              p2 = c(xm2[l], ym2[l], zm2[l])
              p3 = c(xr[j], yr[j], zr[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
          }
          if (k == 4) {
            if (j == 1 || j == 3) {
              p1 = c(xm2[l], ym2[l], zm2[l])
              p2 = c(xm2[j], ym2[j], zm2[j])
              p3 = c(xr[j], yr[j], zr[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
            if (j == 2 || j == 4) {
              p1 = c(xr[l], yr[l], zr[l])
              p2 = c(xm2[j], ym2[j], zm2[j])
              p3 = c(xr[j], yr[j], zr[j])
              Nor = c(Nor, F_normal_vector (p1, p2, p3))
            }
          }
        }
      }
      
      sink(file = "fileoutput.dae", append = TRUE, split = TRUE)
      cat ("\t\t\t\t\t\t", Nor, "\n")
      sink()
      
      # Nor_technique_commonの書き出し
      F_Nor_Tech_com (id)
      
      # vertices + polilystの書き出し
      poly_mate_cnt = 1 + 4 + 4 + 4
      vcnt = ""
      for (j in 1:poly_mate_cnt) {
        vcnt = c(vcnt, 4)
      }
      F_verti_polist (id)
    }
    
    # 片流れ屋根のモデリング
    if (yanetype == 4) {
      # 片流れ屋根底面座標の高さの計算
      mh = hisashi * incline / kansan
      nh = hisashi * incline / kansan
      if (yanemuki == 1) {
        zo[3] = zt[3] + mh
        zo[4] = zt[4] + mh
        zo[1] = zt[1] - nh
        zo[2] = zt[2] - nh
      }
      if (yanemuki == 2) {
        zo[4] = zt[4] + mh
        zo[1] = zt[1] + mh
        zo[2] = zt[2] - nh
        zo[3] = zt[3] - nh
      }
      if (yanemuki == 3) {
        zo[1] = zt[1] + mh
        zo[2] = zt[2] + mh
        zo[3] = zt[3] - nh
        zo[4] = zt[4] - nh
      }
      if (yanemuki == 4) {
        zo[2] = zt[2] + mh
        zo[3] = zt[3] + mh
        zo[4] = zt[4] - nh
        zo[1] = zt[1] - nh
      }
      
      # yaneatuによる垂直方向の厚さ（棟側）
      rh = yaneatu * sqrt(1 + incline^2) / kansan
      # yaneatuによる垂直方向の厚さ（軒側）
      kh = yaneatu / sqrt(1 + incline^2) / kansan
      
      # 片流れ屋根上面座標の計算
      if (yanemuki == 1) {
        zr[3] = zo[3] + rh
        zr[4] = zo[4] + rh
        zr[1] = zo[1] + kh
        zr[2] = zo[2] + kh
        p1 = c(xo[4], yo[4], zo[4])
        p2 = c(xo[1], yo[1], zo[1])
        p3 = c(xo[2], yo[2], zo[2])
        NorRoof = c(F_normal_vector (p1, p2, p3))
        xr[1] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 軒側のX座標
        yr[1] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 軒側のY座標
        p1 = c(xo[1], yo[1], zo[1])
        p2 = c(xo[2], yo[2], zo[2])
        p3 = c(xo[3], yo[3], zo[3])
        NorRoof = c(F_normal_vector (p1, p2, p3))
        xr[2] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 軒側のX座標
        yr[2] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 軒側のY座標
      }
      if (yanemuki == 2) {
        zr[4] = zo[4] + rh
        zr[1] = zo[1] + rh
        zr[2] = zo[2] + kh
        zr[3] = zo[3] + kh
        p1 = c(xo[1], yo[1], zo[1])
        p2 = c(xo[2], yo[2], zo[2])
        p3 = c(xo[3], yo[3], zo[3])
        NorRoof = c(F_normal_vector (p1, p2, p3))
        xr[2] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xo[2]  # 軒側のX座標
        yr[2] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yo[2]  # 軒側のY座標
        p1 = c(xo[2], yo[2], zo[2])
        p2 = c(xo[3], yo[3], zo[3])
        p3 = c(xo[4], yo[4], zo[4])
        NorRoof = c(F_normal_vector (p1, p2, p3))
        xr[3] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xo[3]  # 軒側のX座標
        yr[3] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yo[3]  # 軒側のY座標
      }
      if (yanemuki == 3) {
        zr[1] = zo[1] + rh
        zr[2] = zo[2] + rh
        zr[3] = zo[3] + kh
        zr[4] = zo[4] + kh
        p1 = c(xo[2], yo[2], zo[2])
        p2 = c(xo[3], yo[3], zo[3])
        p3 = c(xo[4], yo[4], zo[4])
        NorRoof = c(F_normal_vector (p1, p2, p3))
        xr[3] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xo[3]  # 軒側のX座標
        yr[3] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yo[3]  # 軒側のY座標
        p1 = c(xo[3], yo[3], zo[3])
        p2 = c(xo[4], yo[4], zo[4])
        p3 = c(xo[1], yo[1], zo[1])
        NorRoof = c(F_normal_vector (p1, p2, p3))
        xr[4] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xo[4]  # 軒側のX座標
        yr[4] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yo[4]  # 軒側のY座標
      }
      if (yanemuki == 4) {
        zr[2] = zo[2] + rh
        zr[3] = zo[3] + rh
        zr[4] = zo[4] + kh
        zr[1] = zo[1] + kh
        p1 = c(xo[3], yo[3], zo[3])
        p2 = c(xo[4], yo[4], zo[4])
        p3 = c(xo[1], yo[1], zo[1])
        NorRoof = c(F_normal_vector (p1, p2, p3))
        xr[4] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xo[4]  # 軒側のX座標
        yr[4] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yo[4]  # 軒側のY座標
        p1 = c(xo[4], yo[4], zo[4])
        p2 = c(xo[1], yo[1], zo[1])
        p3 = c(xo[2], yo[2], zo[2])
        NorRoof = c(F_normal_vector (p1, p2, p3))
        xr[1] = kh / NorRoof[c(3)] * NorRoof[c(1)] + xo[1]  # 軒側のX座標
        yr[1] = kh / NorRoof[c(3)] * NorRoof[c(2)] + yo[1]  # 軒側のY座標
      }
      
      # 屋根モデルのモデリング(ID設定)
      F_lib_geo_Pos (id, count)
      
      # 屋根底面頂点座標の書き出し
      F_bottom_plate (vertex, xo, yo, zo)
      
      # 側面頂点座標の書き出し
      F_side_plate (vertex, xo, yo, zo, xr, yr, zr)
      
      # 屋根上面頂点座標の書き出し
      F_top_plate (vertex, xr, yr, zr)
      
      # Pos_technique_commonの書き出し
      F_Pos_Tech_com (id)
      
      # library_geometries(Normal)の書き出し
      F_lib_geo_Nor (id, count)
      
      ## 法線ベクトルの書き出し
      Nor = c()
      
      # 底面法線ベクトルの算出
      for (j in 1:vertex) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = vertex}
        if (l > vertex) {l = 1}
        p1 = c(xo[k], yo[k], zo[k])
        p2 = c(xo[j], yo[j], zo[j])
        p3 = c(xo[l], yo[l], zo[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      # 側面法線ベクトルの算出
      for (j in 1:vertex) {
        l = j + 1
        if (l > vertex) {l = 1}
        for (k in 1:4) {
          if (k == 1) {
            p1 = c(xr[j], yr[j], zr[j])
            p2 = c(xo[j], yo[j], zo[j])
            p3 = c(xo[l], yo[l], zo[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 2) {
            p1 = c(xo[j], yo[j], zo[j])
            p2 = c(xo[l], yo[l], zo[l])
            p3 = c(xr[l], yr[l], zr[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 3) {
            p1 = c(xo[l], yo[l], zo[l])
            p2 = c(xr[l], yr[l], zr[l])
            p3 = c(xr[j], yr[j], zr[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 4) {
            p1 = c(xr[l], yr[l], zr[l])
            p2 = c(xr[j], yr[j], zr[j])
            p3 = c(xo[j], yo[j], zo[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
        }
      }
      
      # 上面法線ベクトルの算出
      for (j in 1:vertex) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = vertex}
        if (l > vertex) {l = 1}
        p1 = c(xr[k], yr[k], zr[k])
        p2 = c(xr[j], yr[j], zr[j])
        p3 = c(xr[l], yr[l], zr[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      sink(file = "fileoutput.dae", append = TRUE, split = TRUE)
      cat ("\t\t\t\t\t\t", Nor, "\n")
      sink()
      
      # Nor_technique_commonの書き出し
      F_Nor_Tech_com (id)
      
      # vertices + polilystの書き出し
      vcnt = c(vertex)
      for (j in 1:vertex) {
        vcnt = c(vcnt, 4)
      }
      vcnt = c(vcnt, vertex)
      F_verti_polist (id)
    }
    
    # 町家屋根のモデリング
    if (yanetype == 8) {
      # ２階屋根の頂点座標の設定
      xo1 = array(dim = c(4))
      yo1 = array(dim = c(4))
      zo1 = array(dim = c(4))
      xo2 = array(dim = c(4))
      yo2 = array(dim = c(4))
      zo2 = array(dim = c(4))
      xr1 = array(dim = c(4))
      yr1 = array(dim = c(4))
      zr1 = array(dim = c(4))
      xr2 = array(dim = c(4))
      yr2 = array(dim = c(4))
      zr2 = array(dim = c(4))
      
      # ２階屋根軒下端の高さ（Z座標）
      nh = hisashi * incline / kansan
      zo1[1] = zt[1] + matiya2 / kansan - nh
      zo1[2] = zt[2] + matiya2 / kansan - nh
      zo2[1] = zt[3] + matiya2 / kansan - nh
      zo2[2] = zt[4] + matiya2 / kansan - nh
      
      # yaneatuによる垂直方向の厚さ（軒側）
      kh = yaneatu / sqrt(1 + incline^2) / kansan
      # yaneatuによる垂直方向の厚さ（棟側）
      rh = yaneatu * sqrt(1 + incline^2) / kansan
      
      # ２階屋根軒上端の高さ（Z座標）
      zr1[1] = zo1[1] + kh
      zr1[2] = zo1[2] + kh
      zr2[1] = zo2[1] + kh
      zr2[2] = zo2[2] + kh
      
      # ２階屋根棟頂点高さの計算
      zo1[3] = zm[7]
      zo1[4] = zm[8]
      zo2[3] = zm[3]
      zo2[4] = zm[4]
      zr1[3] = zo1[3] + rh
      zr1[4] = zo1[4] + rh
      zr2[3] = zo2[3] + rh
      zr2[4] = zo2[4] + rh
      
      # ２階の壁面線に平行な直線の式の切片
      Bm = array(dim = c(4, 2))     # 平行な２直線の切片
      bom = array(dim = c(4))       # 新しい切片の値
      Bm2 = array(dim = c(4, 2))    # ２階壁面線位置（１階軒庇用）
      bom2 = array(dim = c(4))
      for (j in 1:4) {
        if ((j == 1) || (j == 3)) {
          # 仮の切片による切片の差の比較（２階屋根用）
          Bm[j, 1] = br[j] + (koutai2 - d[j]) / kansan * sqrt(a[j]^2 + 1)
          Bm[j, 2] = br[j] - (koutai2 - d[j]) / kansan * sqrt(a[j]^2 + 1)
          # 新しい切片の値の設定
          if (j == 1) {
            if (D[j] > abs(Bm[j, 1] - br[j+2])) {
              bom[j] = b[j] + (koutai2 - d[j]) / kansan * sqrt(a[j]^2 + 1)
            } else {
              bom[j] = b[j] - (koutai2 - d[j]) / kansan * sqrt(a[j]^2 + 1)
            }
          }
          if (j == 3) {
            if (D[j] > abs(Bm[j, 1] - br[j-2])) {
              bom[j] = b[j] + (koutai2 - d[j]) / kansan * sqrt(a[j]^2 + 1)
            } else {
              bom[j] = b[j] - (koutai2 - d[j]) / kansan * sqrt(a[j]^2 + 1)
            }
          }
          # 仮の切片による切片の差の比較（１階軒庇用）
          Bm2[j, 1] = br[j] + koutai2 / kansan * sqrt(a[j]^2 + 1)
          Bm2[j, 2] = br[j] - koutai2 / kansan * sqrt(a[j]^2 + 1)
          # 新しい切片の値の設定（１階軒庇用）
          if (j == 1) {
            if (D[j] > abs(Bm2[j, 1] - br[j+2])) {
              bom2[j] = b[j] + koutai2 / kansan * sqrt(a[j]^2 + 1)
            } else {
              bom2[j] = b[j] - koutai2 / kansan * sqrt(a[j]^2 + 1)
            }
          }
          if (j == 3) {
            if (D[j] > abs(Bm2[j, 1] - br[j-2])) {
              bom2[j] = b[j] + koutai2 / kansan * sqrt(a[j]^2 + 1)
            } else {
              bom2[j] = b[j] - koutai2 / kansan * sqrt(a[j]^2 + 1)
            }
          }
        }
        if ((j == 2) || (j == 4)) {
          # 仮の切片による切片の差の比較（２階屋根用）
          Bm[j, 1] = br[j] + d[j] / kansan * sqrt(a[j]^2 + 1)
          Bm[j, 2] = br[j] - d[j] / kansan * sqrt(a[j]^2 + 1)
          # 新しい切片の値の設定
          if (j == 2) {
            if (D[j] < abs(Bm[j, 1] - br[j+2])) {
              bom[j] = b[j] + d[j] / kansan * sqrt(a[j]^2 + 1)
            } else {
              bom[j] = b[j] - d[j] / kansan * sqrt(a[j]^2 + 1)
            }
          }
          if (j == 4) {
            if (D[j] < abs(Bm[j, 1] - br[j-2])) {
              bom[j] = b[j] + d[j] / kansan * sqrt(a[j]^2 + 1)
            } else {
              bom[j] = b[j] - d[j] / kansan * sqrt(a[j]^2 + 1)
            }
          }
        }
      }
      
      # ２階屋根の４頂点のX･Y座標の算出
      for (j in 1:4) {
        if (j == 1) {
          xo1[1] = (bom[1] - bom[4]) / (a[4] - a[1])
          yo1[1] = xo1[1] * a[1] + bom[1]
        }
        if (j == 2) {
          xo1[2] = (bom[2] - bom[1]) / (a[1] - a[2])
          yo1[2] = xo1[2] * a[2] + bom[2]
        }
        if (j == 3) {
          xo2[1] = (bom[3] - bom[2]) / (a[2] - a[3])
          yo2[1] = xo2[1] * a[3] + bom[3]
        }
        if (j == 4) {
          xo2[2] = (bom[4] - bom[3]) / (a[3] - a[4])
          yo2[2] = xo2[2] * a[4] + bom[4]
        }
      }
      
      # 軒頂点下端座標の計算
      xo1[3] = (xo1[2] + xo2[1]) / 2
      yo1[3] = (yo1[2] + yo2[1]) / 2
      xo1[4] = (xo1[1] + xo2[2]) / 2
      yo1[4] = (yo1[1] + yo2[2]) / 2
      xo2[3] = (xo1[1] + xo2[2]) / 2
      yo2[3] = (yo1[1] + yo2[2]) / 2
      xo2[4] = (xo1[2] + xo2[1]) / 2
      yo2[4] = (yo1[2] + yo2[1]) / 2
      
      # ２階屋根軒上端座標の計算
      p1 = c(xo1[4], yo1[4], zo1[4])
      p2 = c(xo1[1], yo1[1], zo1[1])
      p3 = c(xo1[2], yo1[2], zo1[2])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr1[1] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 軒側のX座標
      yr1[1] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 軒側のY座標
      p1 = c(xo1[1], yo1[1], zo1[1])
      p2 = c(xo1[2], yo1[2], zo1[2])
      p3 = c(xo1[3], yo1[3], zo1[3])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr1[2] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 軒側のX座標
      yr1[2] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 軒側のY座標
      
      p1 = c(xo2[4], yo2[4], zo2[4])
      p2 = c(xo2[1], yo2[1], zo2[1])
      p3 = c(xo2[2], yo2[2], zo2[2])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr2[1] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 軒側のX座標
      yr2[1] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 軒側のY座標
      p1 = c(xo2[1], yo2[1], zo2[1])
      p2 = c(xo2[2], yo2[2], zo2[2])
      p3 = c(xo2[3], yo2[3], zo2[3])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xr2[2] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 軒側のX座標
      yr2[2] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 軒側のY座標
      
      # 棟頂点上端座標の計算
      xr1[3] = (xr1[2] + xr2[1]) / 2
      yr1[3] = (yr1[2] + yr2[1]) / 2
      xr1[4] = (xr1[1] + xr2[2]) / 2
      yr1[4] = (yr1[1] + yr2[2]) / 2
      xr2[3] = (xr1[1] + xr2[2]) / 2
      yr2[3] = (yr1[1] + yr2[2]) / 2
      xr2[4] = (xr1[2] + xr2[1]) / 2
      yr2[4] = (yr1[2] + yr2[1]) / 2
      
      # １階軒庇の頂点座標の設定
      xu1 = array(dim = c(4))
      yu1 = array(dim = c(4))
      zu1 = array(dim = c(4))
      xu2 = array(dim = c(4))
      yu2 = array(dim = c(4))
      zu2 = array(dim = c(4))
      xn1 = array(dim = c(4))
      yn1 = array(dim = c(4))
      zn1 = array(dim = c(4))
      xn2 = array(dim = c(4))
      yn2 = array(dim = c(4))
      zn2 = array(dim = c(4))
      
      # １階軒庇下端の頂点座標
      xu1[1] = xo[1]
      yu1[1] = yo[1]
      xu1[2] = xo[2]
      yu1[2] = yo[2]
      xu2[1] = xo[3]
      yu2[1] = yo[3]
      xu2[2] = xo[4]
      yu2[2] = yo[4]
      zu1[1] = zt[1] - hisashi * incline / kansan
      zu1[2] = zt[2] - hisashi * incline / kansan
      zu2[1] = zt[3] - hisashi * incline / kansan
      zu2[2] = zt[4] - hisashi * incline / kansan
      
      # １階軒庇壁面側下端の頂点座標
      for (j in 1:4) {
        if (j == 1) {
          xu1[4] = (bom2[1] - bo[4]) / (a[4] - a[1])
          yu1[4] = xu1[4] * a[1] + bom2[1]
        }
        if (j == 2) {
          xu1[3] = (bo[2] - bom2[1]) / (a[1] - a[2])
          yu1[3] = xu1[3] * a[2] + bo[2]
        }
        if (j == 3) {
          xu2[4] = (bom2[3] - bo[2]) / (a[2] - a[3])
          yu2[4] = xu2[4] * a[3] + bom2[3]
        }
        if (j == 4) {
          xu2[3] = (bo[4] - bom2[3]) / (a[3] - a[4])
          yu2[3] = xu2[3] * a[4] + bo[4]
        }
      }
      
      zu1[4] = zt[1] + koutai2 * incline / kansan
      zu1[3] = zt[2] + koutai2 * incline / kansan
      zu2[4] = zt[3] + koutai2 * incline / kansan
      zu2[3] = zt[4] + koutai2 * incline / kansan
      
      # １階軒庇上端座標の計算
      p1 = c(xu1[4], yu1[4], zu1[4])
      p2 = c(xu1[1], yu1[1], zu1[1])
      p3 = c(xu1[2], yu1[2], zu1[2])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xn1[1] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 軒側のX座標
      yn1[1] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 軒側のY座標
      p1 = c(xu1[1], yu1[1], zu1[1])
      p2 = c(xu1[2], yu1[2], zu1[2])
      p3 = c(xu1[3], yu1[3], zu1[3])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xn1[2] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 軒側のX座標
      yn1[2] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 軒側のY座標
      
      p1 = c(xu2[4], yu2[4], zu2[4])
      p2 = c(xu2[1], yu2[1], zu2[1])
      p3 = c(xu2[2], yu2[2], zu2[2])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xn2[1] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 軒側のX座標
      yn2[1] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 軒側のY座標
      p1 = c(xu2[1], yu2[1], zu2[1])
      p2 = c(xu2[2], yu2[2], zu2[2])
      p3 = c(xu2[3], yu2[3], zu2[3])
      NorRoof = c(F_normal_vector (p1, p2, p3))
      xn2[2] = kh / NorRoof[c(3)] * NorRoof[c(1)] + p2[1]  # 軒側のX座標
      yn2[2] = kh / NorRoof[c(3)] * NorRoof[c(2)] + p2[2]  # 軒側のY座標
      
      zn1[1] = zu1[1] + kh
      zn1[2] = zu1[2] + kh
      zn2[1] = zu2[1] + kh
      zn2[2] = zu2[2] + kh
      
      # １階軒庇壁面側上端の頂点座標
      xn1[3] = xu1[3]
      yn1[3] = yu1[3]
      xn1[4] = xu1[4]
      yn1[4] = yu1[4]
      xn2[3] = xu2[3]
      yn2[3] = yu2[3]
      xn2[4] = xu2[4]
      yn2[4] = yu2[4]
      zn1[3] = zu1[3] + rh 
      zn1[4] = zu1[4] + rh 
      zn2[3] = zu2[3] + rh 
      zn2[4] = zu2[4] + rh 
      
      # 屋根モデルのモデリング(ID設定)
      ver_num = ((1 + 4 + 1) * 2 * 4) * 2
      count = ver_num * 3
      F_lib_geo_Pos (id, count)
      
      # ２階屋根底面頂点座標の書き出し
      F_bottom_plate (4, xo1, yo1, zo1)
      F_bottom_plate (4, xo2, yo2, zo2)
      
      # ２階屋根側面頂点座標の書き出し
      F_side_plate (4, xo1, yo1, zo1, xr1, yr1, zr1)
      F_side_plate (4, xo2, yo2, zo2, xr2, yr2, zr2)
      
      # ２階屋根上面頂点座標の書き出し
      F_top_plate (4, xr1, yr1, zr1)
      F_top_plate (4, xr2, yr2, zr2)
      
      # １階軒庇底面頂点座標の書き出し
      F_bottom_plate (4, xu1, yu1, zu1)
      F_bottom_plate (4, xu2, yu2, zu2)
      
      # １階軒庇側面頂点座標の書き出し
      F_side_plate (4, xu1, yu1, zu1, xn1, yn1, zn1)
      F_side_plate (4, xu2, yu2, zu2, xn2, yn2, zn2)
      
      # １階軒庇上面頂点座標の書き出し
      F_top_plate (4, xn1, yn1, zn1)
      F_top_plate (4, xn2, yn2, zn2)
      
      # Pos_technique_commonの書き出し
      F_Pos_Tech_com (id)
      
      # library_geometries(Normal)の書き出し
      F_lib_geo_Nor (id, count)
      
      ## 法線ベクトルの書き出し
      Nor = c()
      
      # ２階屋根底面法線ベクトルの算出
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xo1[k], yo1[k], zo1[k])
        p2 = c(xo1[j], yo1[j], zo1[j])
        p3 = c(xo1[l], yo1[l], zo1[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xo2[k], yo2[k], zo2[k])
        p2 = c(xo2[j], yo2[j], zo2[j])
        p3 = c(xo2[l], yo2[l], zo2[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      # ２階屋根側面法線ベクトルの算出
      for (j in 1:4) {
        l = j + 1
        if (l > 4) {l = 1}
        for (k in 1:4) {
          if (k == 1) {
            p1 = c(xr1[j], yr1[j], zr1[j])
            p2 = c(xo1[j], yo1[j], zo1[j])
            p3 = c(xo1[l], yo1[l], zo1[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 2) {
            p1 = c(xo1[j], yo1[j], zo1[j])
            p2 = c(xo1[l], yo1[l], zo1[l])
            p3 = c(xr1[l], yr1[l], zr1[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 3) {
            p1 = c(xo1[l], yo1[l], zo1[l])
            p2 = c(xr1[l], yr1[l], zr1[l])
            p3 = c(xr1[j], yr1[j], zr1[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 4) {
            p1 = c(xr1[l], yr1[l], zr1[l])
            p2 = c(xr1[j], yr1[j], zr1[j])
            p3 = c(xo1[j], yo1[j], zo1[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
        }
      }
      for (j in 1:4) {
        l = j + 1
        if (l > 4) {l = 1}
        for (k in 1:4) {
          if (k == 1) {
            p1 = c(xr2[j], yr2[j], zr2[j])
            p2 = c(xo2[j], yo2[j], zo2[j])
            p3 = c(xo2[l], yo2[l], zo2[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 2) {
            p1 = c(xo2[j], yo2[j], zo2[j])
            p2 = c(xo2[l], yo2[l], zo2[l])
            p3 = c(xr2[l], yr2[l], zr2[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 3) {
            p1 = c(xo2[l], yo2[l], zo2[l])
            p2 = c(xr2[l], yr2[l], zr2[l])
            p3 = c(xr2[j], yr2[j], zr2[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 4) {
            p1 = c(xr2[l], yr2[l], zr2[l])
            p2 = c(xr2[j], yr2[j], zr2[j])
            p3 = c(xo2[j], yo2[j], zo2[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
        }
      }
      # ２階屋根上面法線ベクトルの算出
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xr1[k], yr1[k], zr1[k])
        p2 = c(xr1[j], yr1[j], zr1[j])
        p3 = c(xr1[l], yr1[l], zr1[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xr2[k], yr2[k], zr2[k])
        p2 = c(xr2[j], yr2[j], zr2[j])
        p3 = c(xr2[l], yr2[l], zr2[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }

      # １階軒庇底面法線ベクトルの算出
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xu1[k], yu1[k], zu1[k])
        p2 = c(xu1[j], yu1[j], zu1[j])
        p3 = c(xu1[l], yu1[l], zu1[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xu2[k], yu2[k], zu2[k])
        p2 = c(xu2[j], yu2[j], zu2[j])
        p3 = c(xu2[l], yu2[l], zu2[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      # １階軒庇側面法線ベクトルの算出
      for (j in 1:4) {
        l = j + 1
        if (l > 4) {l = 1}
        for (k in 1:4) {
          if (k == 1) {
            p1 = c(xn1[j], yn1[j], zn1[j])
            p2 = c(xu1[j], yu1[j], zu1[j])
            p3 = c(xu1[l], yu1[l], zu1[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 2) {
            p1 = c(xu1[j], yu1[j], zu1[j])
            p2 = c(xu1[l], yu1[l], zu1[l])
            p3 = c(xn1[l], yn1[l], zn1[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 3) {
            p1 = c(xu1[l], yu1[l], zu1[l])
            p2 = c(xn1[l], yn1[l], zn1[l])
            p3 = c(xn1[j], yn1[j], zn1[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 4) {
            p1 = c(xn1[l], yn1[l], zn1[l])
            p2 = c(xn1[j], yn1[j], zn1[j])
            p3 = c(xu1[j], yu1[j], zu1[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
        }
      }
      for (j in 1:4) {
        l = j + 1
        if (l > 4) {l = 1}
        for (k in 1:4) {
          if (k == 1) {
            p1 = c(xn2[j], yn2[j], zn2[j])
            p2 = c(xu2[j], yu2[j], zu2[j])
            p3 = c(xu2[l], yu2[l], zu2[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 2) {
            p1 = c(xu2[j], yu2[j], zu2[j])
            p2 = c(xu2[l], yu2[l], zu2[l])
            p3 = c(xn2[l], yn2[l], zn2[l])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 3) {
            p1 = c(xu2[l], yu2[l], zu2[l])
            p2 = c(xn2[l], yn2[l], zn2[l])
            p3 = c(xn2[j], yn2[j], zn2[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
          if (k == 4) {
            p1 = c(xn2[l], yn2[l], zn2[l])
            p2 = c(xn2[j], yn2[j], zn2[j])
            p3 = c(xu2[j], yu2[j], zu2[j])
            Nor = c(Nor, F_normal_vector (p1, p2, p3))
          }
        }
      }
      # １階軒庇上面法線ベクトルの算出
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xn1[k], yn1[k], zn1[k])
        p2 = c(xn1[j], yn1[j], zn1[j])
        p3 = c(xn1[l], yn1[l], zn1[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      for (j in 1:4) {
        k = j - 1
        l = j + 1
        if (k == 0) {k = 4}
        if (l > 4) {l = 1}
        p1 = c(xn2[k], yn2[k], zn2[k])
        p2 = c(xn2[j], yn2[j], zn2[j])
        p3 = c(xn2[l], yn2[l], zn2[l])
        Nor = c(Nor, F_normal_vector (p1, p2, p3))
      }
      
      sink(file = "fileoutput.dae", append = TRUE, split = TRUE)
      cat ("\t\t\t\t\t\t", Nor, "\n")
      sink()
      
      # Nor_technique_commonの書き出し
      F_Nor_Tech_com (id)
      
      # vertices + polilystの書き出し
      poly_mate_cnt = ((1 + 4 + 1) * 2) * 2
      vcnt = ""
      for (j in 1:poly_mate_cnt) {
        vcnt = c(vcnt, 4)
      }
      F_verti_polist (id)
    }
  }     # ここまで４頂点建物モデルのモデリング

else {  # (vertex != 4)
    # 多頂点陸屋根の建物モデリング（完成版）
    
    # ID番号の設定（iをidに変更）
    id = i
    
    # library_geometries(Position)の書き出し
    F_lib_geo_Pos (id, count)
    
    # 底面頂点座標の書き出し
    F_bottom_plate (vertex, xb, yb, zb)
    
    # 側面頂点座標の書き出し
    F_side_plate (vertex, xb, yb, zb, xt, yt, zt)
    
    # 上面頂点座標の書き出し
    F_top_plate (vertex, xt, yt, zt)
    
    # Pos_technique_commonの書き出し
    F_Pos_Tech_com (id)
    
    # library_geometries(Normal)の書き出し
    F_lib_geo_Nor (id, count)
    
    ## 法線ベクトルの書き出し
    normal = array(dim = c(3))
    Nor = c()
    
    # 底面法線ベクトルの算出
    for (j in 1:vertex) {
      k = j - 1
      l = j + 1
      if (k == 0) {k = vertex}
      if (l > vertex) {l = 1}
      p1 = c(xb[k], yb[k], zb[k])
      p2 = c(xb[j], yb[j], zb[j])
      p3 = c(xb[l], yb[l], zb[l])
      Nor = c(Nor, F_normal_vector (p1, p2, p3))
    }
    
    # 側面法線ベクトルの算出
    for (j in 1:vertex) {
      l = j + 1
      if (l > vertex) {l = 1}
      for (k in 1:4) {
        if (k == 1) {
          p1 = c(xt[j], yt[j], zt[j])
          p2 = c(xb[j], yb[j], zb[j])
          p3 = c(xb[l], yb[l], zb[l])
          Nor = c(Nor, F_normal_vector (p1, p2, p3))
        }
        if (k == 2) {
          p1 = c(xb[j], yb[j], zb[j])
          p2 = c(xb[l], yb[l], zb[l])
          p3 = c(xt[l], yt[l], zt[l])
          Nor = c(Nor, F_normal_vector (p1, p2, p3))
        }
        if (k == 3) {
          p1 = c(xb[l], yb[l], zb[l])
          p2 = c(xt[l], yt[l], zt[l])
          p3 = c(xt[j], yt[j], zt[j])
          Nor = c(Nor, F_normal_vector (p1, p2, p3))
        }
        if (k == 4) {
          p1 = c(xt[l], yt[l], zt[l])
          p2 = c(xt[j], yt[j], zt[j])
          p3 = c(xb[j], yb[j], zb[j])
          Nor = c(Nor, F_normal_vector (p1, p2, p3))
        }
      }
    }
    
    # 上面法線ベクトルの算出
    for (j in 1:vertex) {
      k = j - 1
      l = j + 1
      if (k == 0) {k = vertex}
      if (l > vertex) {l = 1}
      p1 = c(xt[k], yt[k], zt[k])
      p2 = c(xt[j], yt[j], zt[j])
      p3 = c(xt[l], yt[l], zt[l])
      Nor = c(Nor, F_normal_vector (p1, p2, p3))
    }
    sink(file = "fileoutput.dae", append = TRUE, split = TRUE)
    cat ("\t\t\t\t\t\t", Nor, "\n")
    sink()
    
    # Nor_technique_commonの書き出し
    F_Nor_Tech_com (id)
    
    # vertices + polilystの書き出し
    vcnt = c(vertex)
    for (j in 1:vertex) {
      vcnt = c(vcnt, 4)
    }
    vcnt = c(vcnt, vertex)
    F_verti_polist (id)
  }
}   # ここまで逐次処理

# library_materialsの書き出し
# データ毎に必要な部分（idのみが変わる）
F_lib_mate (s_count)

# library_effectsの書き出し
# データ毎に必要な部分（idのみが変わる）
F_lib_effe (s_count, col_rgb)

# COLLADAファイルの出力終了
scene = array(dim = c(4))
scene[1] = "\t</library_effects>"
scene[2] = "\t<scene>"
scene[3] = "\t\t<instance_visual_scene url=\"#DefaultScene\" />"
scene[4] = "\t</scene>"
for (i in 1:4){
  write (scene[i], "fileoutput.dae", append = TRUE)
}
collada_2 = "</COLLADA>"
write (collada_2, "fileoutput.dae", append = TRUE)