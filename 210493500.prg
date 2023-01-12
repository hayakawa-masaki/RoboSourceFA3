1 ' ===================================
2 '
3 '  210493500 STEP5 Assy1プログラムCD用
4 '
5 ' 作成者：自動化T
6 ' 作成日：2022.07.16
7 ' Ver 01 2022.07.16 FA3 DVD用をベースにジャイロ削除、FA2との統一化等
8 ' ===================================
9 '===== <Insight定数> =====
10 '===== <Insight変数定義> =====
11 Dim PInspPosition(30)               '画像処理Function引渡し用位置変数
12 Dim MInspGroup%(30)                 '画像処理Function引渡し用変数
13 Def Inte MIN_IS_Ready               '【入力IO】Insight準備OK
14 Def Inte MIN_IS_JobLoadOK           '【入力IO】Insightジョブロード正常終了
15 Def Inte MIN_IS_JobLoadNG           '【入力IO】Insightジョブロード異常終了
16 Def Inte MIN_IS_InspGSetOK          '【入力IO】Insight検査グループ番号設定正常終了
17 Def Inte MIN_IS_InspGSetNG          '【入力IO】Insight検査グループ番号設定異常終了
18 Def Inte MIN_IS_InspOK              '【入力IO】Insight検査OK
19 Def Inte MIN_IS_InspNG              '【入力IO】Insight検査NG
20 Def Inte MIN_IS_InspErr             '【入力IO】Insight検査異常終了
21 Def Inte MIN_IS_InspCapDone         '【入力IO】Insight検査画像取込完了
22 '
23 Def Inte MIN_IS_ErrNum              '【入力IO】Insight処理エラー番号取得開始アドレス(16bit)
24 'Output Signal
25 Def Inte MOUT_IS_JobLoadReq         '【出力IO】Insight JOBロード要求
26 Def Inte MOUT_IS_InspGSetReq        '【出力IO】Insight 検査グループ番号設定要求
27 Def Inte MOUT_IS_Insp               '【出力IO】Insight 検査実行要求
28 '
29 Def Inte MOUT_IS_JobNum             '【出力IO】Insight JOB番号設定開始アドレス(16bit)
30 Def Inte MOUT_IS_InspGNum           '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
31 '
32 Def Inte MOUT_InspErrNum            '【出力IO】検査実行エラー番号開始アドレス(16bit)
33 Def Inte MOUT_InspNGStepNum         '【出力IO】検査実行NGStep番号開始アドレス(16bit)
34 '作業用変数
35 Def Inte MInspErrNum                '検査実行エラー番号
36 Def Inte MInspNGStepNum             '検査実行NGStep番号
37 Def Inte MRtn                       'Function戻り値取得用
38 Def Inte MRtn2                      'Function戻り値取得用
39 Def Inte MRet3                      'Function戻り値取得用
40 Def Inte MGRtn                      'Function戻り値取得用 ネジ供給機
41 Def Inte MInspErrNumSub             '検査実行エラー番号sub　20190820追加
42 Def Inte MovrdA                     'ネジ締めOvrd 可変用   20191127追加
43 Def Float MSpdA                     'ネジ締めSpd　可変用   20191127追加
44 Def Pos PTemp                       'ネジ締め上空位置計算用    20200312追加'
45 MovrdA% = 20                        'ネジ締めOvrd 可変用   20191127追加
46 MSpdA = 800                        'ネジ締めSpd　可変用   20191127追加
47 '===== <Insight変数設定> =====
48 MIN_IS_Ready%        =   11380      '【入力IO】Insight準備OK
49 MIN_IS_JobLoadOK%    =   11381      '【入力IO】Insightジョブロード正常終了
50 MIN_IS_JobLoadNG%    =   11382      '【入力IO】Insightジョブロード異常終了
51 MIN_IS_InspGSetOK%   =   11383      '【入力IO】Insight検査グループ番号設定正常終了
52 MIN_IS_InspGSetNG%   =   11384      '【入力IO】Insight検査グループ番号設定異常終了
53 MIN_IS_InspOK%       =   11385      '【入力IO】Insight検査OK
54 MIN_IS_InspNG%       =   11386      '【入力IO】Insight検査NG
55 MIN_IS_InspErr%      =   11387      '【入力IO】Insight検査異常終了
56 MIN_IS_InspCapDone%  =   11388      '【入力IO】Insight検査画像取込完了
57 MIN_IS_ErrNum%       =   11408      '【入力IO】Insight処理エラー番号開始アドレス(16bit)
58 'Output Signal
59 MOUT_IS_JobLoadReq%  =   12370      '【出力IO】Insight JOBロード要求
60 MOUT_IS_InspGSetReq% =   12371      '【出力IO】Insight 検査グループ番号設定要求
61 MOUT_IS_Insp%        =   12372      '【出力IO】Insight 検査実行要求
62 MOUT_IS_JobNum%      =   12384      '【出力IO】Insight JOB番号設定開始アドレス(16bit)
63 MOUT_IS_InspGNum%    =   12400      '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
64 MOUT_InspErrNum%     =   12416      '【出力IO】検査実行エラー番号開始アドレス(16bit)
65 MOUT_InspNGStepNum%  =   12432      '【出力IO】検査実行NGStep番号開始アドレス(16bit)
66 '===== <電ドラ定数> =====
67 '===== <電ドラ変数定義> =====
68 X20_Driver=11248                    '電ドラステイタス1　Driver Status 1
69 X21_Driver=11249 '電ドラステイタス2  Driver Status 2
70 X22_Driver=11250 '電ドラステイタス3  Driver Status 3
71 X23_Driver=11251 '電ドラステイタス4  Driver Status 4
72 X24_Driver=11252 '電ドラエラーメッセージ1 Driver Error E1
73 X25_Driver=11253 '電ドラエラーメッセージ2 Driver Error E2
74 X26_Driver=11254 '電ドラエラーメッセージ3 Driver Error E3
75 X27_Driver=11255 '電ドラエラーメッセージ4 Driver Error E4
76 X28_Driver=11256 '電ドラトータルエラーシグナル Total Error
77 X29_Driver=11257 '電ドラ終了シグナル Comlete signal
78 X2A_Driver=11258 '電ドラエラーメッセージ5 Driver Error E5
79 '11584   'toRBトルクドライバ-COMP_ERR送信
80 Y60_Driver=12240 '電ドラ半時計回り CCW
81 Y61_Driver=12241 '電ドラ時計回り CW
82 Y62_Driver=12242 'バンクセッティング BANK C1
83 Y63_Driver=12243 'バンクセッティング BANK C2
84 Y64_Driver=12244 'バンクセッティング BANK C3
85 Y65_Driver=12245 'プログラムセッティング PRG SET F1
86 Y66_Driver=12246 'プログラムセッティング PRG SET F2
87 Y67_Driver=12247 'プログラムセッティング PRG SET F3
88 '組立2
89 X34_NG1=11268 'ねじっこ1　Read
90 X35_NG2=11269 'ねじっこ2　Read
91 '組立3
92 X3F_NG1=11279 'ねじっこ1　Read
93 '
94 Dim PScrewPosTemp(10)                                               'ネジ締め用Function引数変数
95 Dim PGetScrewPosTemp(10)                                            'ねじ供給機からねじを得るFunction引数変数
96 Dim PEscapePosi(10)
97 MLoopCnt% = 0'
98 '===== <ロボット定数> =====
99 '===== <ロボット変数定義> =====
100 MRBTOpeGroupNo = 0                    'ロボット動作番号初期化
101 MCommentD1001 = 0
102 MCommentD1002 = 0
103 MCommentD1003 = 0
104 MScreenNo = 0
105 '
106 MCommentTSU = 0
107 MCommentTSD = 0
108 'ウィンド画面番号設定
109 MWindReSet = 0
110 MWindInfoScr = 5
111 MWindErrScr = 10
112 MWindErrScr2 = 11
113 MWindErrScr3 = 13
114 MWindErrScr17 = 17
115 MWindErrScr18 = 18
116 MWindCmmnScr = 20
117 MWindJigRelase19049 = 60
118 MWindJigRelase19050 = 61
119 MWindJigRelase19051 = 62
120 '
121 MClear% = 0        'KEY_のクリア
122 MAbout% = 1        'KEY_停止
123 MNext% = 2         'KEY_次のステップへ移行
124 MContinue% = 3     'KEY_継続 再度同じ動作を行う
125 '
126 Def Inte MNgProcess
127 MNgProcess% = 5      'KEY_NG
128 '
129 MAssyOK% = 6       '組立完了
130 MPass% = 7         '工程パス
131 MPiasNG% = 8       'Pias確認時履歴NG
132 '
133 '初期化用KEY番号   '
134 MRobotInit1% = 11  '初期位置用
135 MRobotInit2% = 12  '初期位置用
136 MRobotInit3% = 13  '初期位置用
137 MRobotInit4% = 14  '初期位置用
138 '
139 MIN_INIT1REQUEST% = 11568 'toRBT_ロボット初期位置1要求
140 MIN_INIT2REQUEST% = 11569 'toRBT_ロボット初期位置2要求
141 MIN_INIT3REQUEST% = 11570 'toRBT_ロボット初期位置3要求
142 MIN_INIT4REQUEST% = 11571 'toRBT_ロボット初期位置4要求
143 '
144 MOUT_INIT1RECIVE% = 12560 'toPLC_ロボット初期位置1受信
145 MOUT_INIT2RECIVE% = 12561 'toPLC_ロボット初期位置2受信
146 MOUT_INIT3RECIVE% = 12562 'toPLC_ロボット初期位置3受信
147 MOUT_INIT4RECIVE% = 12563 'toPLC_ロボット初期位置4受信
148 '
149 MopeNo = 0
150 '
151 MOK% = 1               '各判定用
152 MNG% = 0               '各判定用
153 MTIMEOUT% = -1         '各判定用
154 MJudge% = 0            '判定情報格納用
155 '
156 '
157 MRECIVETIME& = 0
158 MSETTIMEOUT10& = 10000&                '10秒設定
159 MSETTIMEOUT03& = 3000&                 '3秒設定
160 MSETTIMEOUT01& = 1000&                 '1秒設定
161 MSETTIMEOUT05& = 5000&                 '5秒設定
162 MSETTIMEOUT009& = 900&                 '0.9秒設定
163 MSETTIMEOUT008& = 800&                 '0.8秒設定
164 MSETTIMEOUT007& = 700&                 '0.7秒設定
165 MSETTIMEOUT006& = 600&                 '0.6秒設定
166 MSETTIMEOUT005& = 500&                 '0.5秒設定
167 MSETTIMEOUT004& = 400&                 '0.4秒設定
168 MSETTIMEOUT003& = 300&                 '0.3秒設定
169 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
170 MIN_PIAS_ComOK% = 11552                'PC通信OK
171 MIN_PIAS_ComTimeOut% = 11576           'PC通信確認タイムアウト
172 MIN_PIAS_ComNG% = 11553                'PC通信NG
173 MOUT_PIAS_ComCheck% = 12544            'PC通信確認要求
174 MOUT_PIAS_Missing_Process% = 12546     '工程抜け確認要求
175 MIN_PIAS_ModelTypeNG% = 11554          'モデル仕向NG
176 MIN_PIAS_ProcessHistryNG% = 11555      '前工程履歴NG
177 MIN_PIAS_ProcessHistryOK% = 11556      '前工程履歴OK
178 MIN_PIAS_ProcessHistryErr% = 11557     '工程履歴処理エラー
179 MIN_PIAS_MyProcessComp% = 11573        '自工程履歴あり
180 MIN_PIAS_ProcessHistryTimeOut% = 11578 '工程履歴タイムアウト
181 MOUT_OKNG% = 12549                     'PLC OUT でOK=1, NG=0 出力
182 '
183 MOUT_PiasPCBNumberCheck = 12557        '基板番号照合
184 MIN_PiasPCBNumberOK% = 11566          '基板番号OK
185 MIN_PiasPCBNumberNG% = 11565          '基板番号NG
186 MIN_PiasPCBNumberErr% = 11567         '基板番号処理エラー
187 '
188 MOUT_PiasAssyResultOK% = 12549    '組立OK
189 MOUT_PiasAssyResultNG% = 12550    '組立NG
190 MOUT_PiasAssyResultWr% = 12548    '工程履歴書き込み
191 '
192 MIN_PiasProcessNG% = 11559        '工程履歴処理NG
193 MIN_PiasProcessOtherErr% = 11560  '工程履歴処理エラー(なんかのトラブル)
194 MIN_PiasProcessOK% = 11558        '工程履歴処理OK
195 '
196 MIN_Insight_Use% = 11369               '画像確認ON
197 MIN_TorqueCheck% = 11348               'トルクチェック
198 '
199 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT操作権
200 MOUT_RED_LIGHT% = 12356            'PATLIGHT 赤 点灯
201 MOUT_RED_FLASH% = 12357            'PATLIGHT 赤 点滅
202 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT 黄 点灯
203 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT 黄 点滅
204 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT 青 点灯
205 MOUT_GREEN_FLASH% = 12361          'PATLIGHT 青 点滅
206 '
207 MOUT_ST_DATETIME% = 12551          '組立開始日付時刻
208 MOUT_ED_DATETIME% = 12552          '組立終了日付時刻
209 '
210 MOUT_TORQUE_CHECK% = 12367         'PLCへトルクチェック中を送信
211 '
212 MIN_ASSY_CANCEL% = 11366           '組立を行うかのフラグ
213 '
214 MLoopFlg% = 0                      'KEY入力後のOK or NG内容
215 MRtn% = 0
216 MopeNo = 0
217 MRet = 0
218 'MRtn = 0
219 MRet3% = 0
220 '
221 Def Inte MInputQty          '投入数 演算変数
222 Def Inte MAssyOkQty         '組立ＯＫ数 演算変数
223 Def Inte MAssyNgQty         '組立ＮＧ数 演算変数(未使用)
224 Def Inte MSuctionErrQty     '吸着エラー数 演算変数 2022/04/27 渡辺
225 Def Inte nAssyOkQty         '未使用
226 Def Inte MScrewNo
227 Def Inte MReTry
228 '===== <IO変数定義> =====
229 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
230 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
231 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
232 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
233 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
234 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
235 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
236 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
237 '
238 Def Inte Y68_VV1            ' アーム先端　ネジ吸着バルブ
239 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
240 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
241 '
242 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
243 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
244 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
245 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
246 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
247 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
248 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
249 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
250 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
251 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
252 '
253 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
254 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
255 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
256 '
257 Def Inte MOUT_LED1          ' 画像処理用LED照明
258 '
259 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
260 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
261 '
262 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
263 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
264 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
265 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
266 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
267 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
268 '
269 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
270 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
271 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
272 '
273 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
274 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
275 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
276 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
277 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
278 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
279 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
280 Y68_VV1%    =  12248    ' アーム先端　ネジ吸着バルブ 'Y68_VV1% = 12250をY68_VV1% = 12248に変更(8/27中村)
281 Y6B_VB1%    =  12250    'アーム先端　吸着破壊バルブ'Y6B_VB1% = 12251をY6B_VB1% = 12250に変更(8/27中村)
282 MOUT_VB1%   =  12251    ' アーム先端　ネジ吸着破壊バルブ
283 '
284 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
285 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
286 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
287 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
288 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
289 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
290 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
291 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
292 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
293 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
294 '
295 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
296 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
297 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
298 '
299 MOUT_LED1%  =  12239    ' 画像処理用LED照明
300 '
301 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
302 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
303 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
304 '
305 '
306 '共通
307 Def Inte MTEST_KEY                      'デバックテスト用
308 Def Inte MOn                            '出力=1
309 Def Inte MOff                           '出力=0
310 '
311 'ねじ締め装置_出力アドレス
312 Def Inte MOUT_ScwT_ComChk               '通信確認
313 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
314 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
315 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
316 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
317 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
318 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
319 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
320 'ねじ締め装置_入力アドレス
321 Def Inte MIN_ScwT_comOK                 '通信確認返信
322 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
323 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
324 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
325 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
326 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
327 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
328 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
329 '
330 Def Inte MRetryLimit                    ' リトライ回数
331 Def Inte MRetryCount                    ' リトライカウント
332 '
333 Dim MScwT_Case1%(2)               '条件1停止変数
334 Dim MScwT_Case2%(2)               '条件2停止変数
335 Dim MScwT_Case3%(2)               '条件3停止変数
336 Dim MScwT_Case4%(2)               '条件4停止変数
337 Dim MScwT_Case5%(2)               '条件5停止変数
338 '
339 Def Pos PActive                     '直交座標系 位置変数 現在位置
340 Def Pos Pmove                       '直交座標系 位置変数 移動先
341 Def Inte MRecoveryPass              '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行'
342 '共通
343 MTEST_KEY% = 11359                       'デバッグ用テストKEY
344 MOn% = 1                                 '出力 = 1
345 MOff% = 0                                '出力 = 0
346 '
347 'ねじ締め機_アドレス設定
348 MOUT_ScwT_ComChk% = 12816               '通信確認送信
349 MOUT_ScwT_ST% = 12849                   'ねじ締め開始を送信
350 MOUT_ScwT_ReSTOK% = 12850               '再開始受信を送信
351 MOUT_ScwT_FinOK% = 12852                'ねじ締め完了受信を送信
352 MOUT_ScwT_Case1OK% = 12858              '条件1停止受信を送信
353 MOUT_ScwT_Case2OK% = 12859              '条件2停止受信を送信
354 MOUT_ScwT_Case3OK% = 12860              '条件3停止受信を送信
355 MOUT_ScwT_Case4OK% = 12861              '条件4停止受信を送信
356 MOUT_ScwT_Case5OK% = 12862              '条件5停止受信を送信
357 '
358 MIN_ScwT_comOK% = 11824                 'ねじ締め装置から返信
359 MIN_ScwT_STRec% = 11857                 'ねじ締め開始を受信
360 MIN_ScwT_ReST% = 11858                  '再開始を受信
361 MIN_ScwT_Fin% = 11860                   'ねじ締め完了を受信
362 MIN_ScwT_Case1% = 11866                 '条件1停止待機を受信
363 MIN_ScwT_Case2% = 11867                 '条件2停止待機を受信
364 MIN_ScwT_Case3% = 11868                 '条件3停止待機を受信
365 MIN_ScwT_Case4% = 11869                 '条件4停止待機を受信
366 MIN_ScwT_Case5% = 11870                 '条件5停止待機を受信
367 '
368 MScwT_Case1%(1) = MIN_ScwT_Case1%
369 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
370 MScwT_Case2%(1) = MIN_ScwT_Case2%
371 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
372 MScwT_Case3%(1) = MIN_ScwT_Case3%
373 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
374 MScwT_Case4%(1) = MIN_ScwT_Case4%
375 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
376 MScwT_Case5%(1) = MIN_ScwT_Case5%
377 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
378 '
379 '
380 PCalcGetMainScrew = (+0.00,+0.00,-1.20,+0.00,+0.00,+0.00,+0.00)  'Mainねじ供給機の補正値
381 PCalcGetFanScrew = (+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)   'Fanねじ供給機の補正値
382 '
383 MRetryLimit% = 2
384 '
385 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
386 Function M% fnAssyStart
387     M_20# = MClear%                       '初期化
388     '組み立て開始
389     'プログラム原点
390     Ovrd 100
391     ' 初期位置をIDチケット上とするため削除 9/16 M.Hayakawa
392 '    Mov PInitialPosition        '原点回避
393     '初期位置を設定
394     PTemp = P_Curr
395     MRtn = 0
396     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
397         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
398             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
399                 MRtn = 1
400                 Break
401             EndIf
402             Break
403         EndIf
404         Break
405     EndIf
406     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
407     If MRtn = 1 Then
408         M_Out(12256) = 1 Dly 0.3            '位置決め出ON
409         Mov PTicketRead
410         Break
411     Else
412         Mov PInitialPosition
413         M_Out(12256) = 1 Dly 0.3           '位置決め出ON
414         Mov PTicketRead_1           'チケットID読み取り回避点
415         Mvs PTicketRead             'ID読み位置
416         Break
417     EndIf
418     *RE_PUSH
419 '    If M_20# = MContinue% Then M_Out(12257) = 0
420     If M_20# = MContinue% Then M_Out(12256) = 1 Dly 0.3
421     If M_20# = MContinue% Then M_20# = MClear%
422     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '位置決め出端検出(8/26中村)
423     If MRtn = 1 Then GoTo *CompPush
424         M_Out(12257) = 1 Dly 0.3    ' Y71 1:位置決めCY 解除
425         fErrorProcess(11,231,282,0)
426     If M_20# = MNext% Then M_Out(12256) = 1 Dly 0.3 'Y70 1:位置決めCY 固定
427     If M_20# = MNext% Then M_20# = MClear%
428     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
429     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
430     If M_20# = MContinue% Then GoTo *RE_PUSH
431     *CompPush
432 '
433     *RE_READ
434     If M_20# = MContinue% Then M_20# = MClear%
435 '
436     MRtn = 1                            'MRtn初期化
437     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
438         MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
439     EndIf
440         '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
441         '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
442 '
443     If MRtn = 1 Then GoTo *CompRead
444     Mvs PTicketRead_1                       ' 一旦上空に待避 追加 22/07/16 M,H
445     'fErrorProcess(11,97,25,0)
446     If M_20# = MPass% Then GoTo *AssyEnd    ' 次へを押された時のコメント解除＋ジャンプ先変更 2022/07/20 M.H
447     If M_20# = MNext% Then M_20# = MClear%
448     If M_20# = MAbout% Then GoTo *AssyEnd       ' コメント解除＋ジャンプ先変更 22/07/16 M,H
449     If M_20# = MNgProcess% Then GoTo *AssyEnd   ' コメント解除＋ジャンプ先変更 22/07/16 M,H
450     If M_20# = MContinue% Then GoTo *RE_READ
451 '    If M_20# = MNext% Then M_20# = MPass%
452     GoTo *ASSY_ERROR_END
453     *CompRead
454     '
455 '【MAIN基板ID読み込み】
456     *RE_MEIN_CHECK
457     PInspPosition(1) = PMainPcbRead 'MAIN基板読込位置
458     MInspGroup%(1) = 2              '検査G番号
459     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
460 '
461     If MRtn = 1 Then GoTo *CompMainCheck
462     fErrorProcess(11,38,25,0)
463     If M_20# = MNext% Then M_20# = MClear%
464     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
465     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
466     If M_20# = MContinue% Then GoTo *RE_MEIN_CHECK
467     *CompMainCheck
468 ''【GYRO基板ID読み込み】
469 '    *RE_GYRO_CHECK
470 '    PInspPosition(1) = PGyroPcbRead 'GYRO基板読込位置
471 '    MInspGroup%(1) = 3              '検査G番号
472 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
473 ''
474 '    If MRtn = 1 Then GoTo *CompGyroCheck
475 '    fErrorProcess(11,38,25,0)
476 '    If M_20# = MNext% Then M_20# = MClear%
477 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
478 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
479 '    If M_20# = MContinue% Then GoTo *RE_GYRO_CHECK
480     *CompGyroCheck
481 '【基板IDコピー】
482     *RE_PCB_RECORD
483     M_Out(12571) = 1    ' 領域1 基板番号コピー (D2600-) On
484     Dly 0.1
485 '    M_Out(12572) = 1    ' 領域2 基板番号コピー (D2612-) On
486     Dly 0.1
487     M_Out(12566) = 1    ' toPLC_基板番号コピー要求 On
488 '
489     MRtn = frInCheck(11581,1,MSETTIMEOUT05&)    ' toRBT_基板番号コピー完了 On
490     If MRtn = 1 Then
491         M_Out(12571) = 0  ' 領域1 基板番号コピー (D2600-) Off
492         Dly 0.1
493 '        M_Out(12572) = 0  ' 領域2 基板番号コピー (D2612-) Off
494         Dly 0.1
495         M_Out(12566) = 0  ' toPLC_基板番号コピー要求 Off
496 '        GoTo *RE_PCB_COMPAIRE   ' 基板番号照合にスキップ
497     Else
498         fErrorProcess(11,39,25,0)
499         If M_20# = MNext% Then M_20# = MClear%
500         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
501         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
502         If M_20# = MContinue% Then GoTo *RE_PCB_RECORD
503     EndIf
504 '【基板ID照合（紐付け）】
505     MRetryCount% = 0
506     While (MRetryCount% <= MRetryLimit%)
507         *RE_PCB_COMPAIRE
508         M_Out(12557)= 1 ' 基板番号照合ビットON
509         MRtn = frInCheck(11566,1,MSETTIMEOUT05&)    ' toRBT_基板番号照合OK(M420) On
510         If MRtn = 1 Then
511             M_Out(12557)= 0     ' 基板番号照合ビットOff
512             ' リトライ回数設定でループを抜ける
513             MRetryCount% = 99
514         Else
515             If MRetryCount% = MRetryLimit% Then
516                 If M_In(11565) = 1 Then
517                     fErrorProcess(11,37,25,0)
518                 Else
519                     fErrorProcess(11,38,25,0)
520                 EndIf
521                 If M_20# = MNext% Then
522                     M_20# = MClear%
523                     ' リトライ回数設定でループを抜ける
524                     MRetryCount% = 99
525                 EndIf
526                 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
527                 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
528                 If M_20# = MContinue% Then
529                     MRetryCount% = 0
530                 EndIf
531             Else
532                 ' リトライ回数インクリメント
533                 MRetryCount% = MRetryCount% + 1
534                 Dly 0.1  ' 他の工程とタイミングをずらす為のディレイ
535             EndIf
536         EndIf
537     WEnd
538 '
539     *RE_CHECK
540     PInspPosition(1) = PParts1Check '部品1画像チェック位置(MAIN基板周辺）
541     MInspGroup%(1) = 4              '検査G番号
542     PInspPosition(2) = PParts2Check '部品2画像チェック位置（背面板周辺）
543     MInspGroup%(2) = 5              '検査G番号
544 '    PInspPosition(3) = PParts3Check '部品3画像チェック位置（SOC基板周辺）
545 '    MInspGroup%(3) = 6              '検査G番号
546 '    PInspPosition(4) = PParts4Check '部品4画像チェック位置（側板周辺）
547 '    MInspGroup%(4) = 7              '検査G番号
548     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 2, -1, 1 )  '画像処理検査実行
549     If MRtn = 1 Then GoTo *CompCheck
550     fErrorProcess(11,43,23,0)
551     If M_20# = MNext% Then M_20# = MClear%
552     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
553     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
554     If M_20# = MContinue% Then GoTo *RE_CHECK
555     *CompCheck
556     '
557     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
558     '製品位置決め(ID読込後に変更 9/16 M.Hayakawa）
559     *RE_POS
560     If M_20# = MContinue% Then M_20# = MClear%
561     M_Out(12256)=1 Dly 0.3      '位置決めCY用SV出端パルス出力
562     MRtn = FnCtlValue2(99)       '読書開始信号OFF  2022/04/28 渡辺
563 '
564     'Wait M_In(11266)=1          '位置決め出端検出修正につきコメントアウト(8/26中村))
565     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '位置決め出端検出(8/26中村)
566     If MRtn = 1 Then GoTo *Comp_Pos_1
567     fErrorProcess(11,231,282,0)
568     If M_20# = MNext% Then M_20# = MClear%
569     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
570     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
571     If M_20# = MContinue% Then GoTo *RE_POS
572     *Comp_Pos_1
573     '
574     M_Out(12258)=1 Dly 0.3      'プッシュCY用SV出端パルス出力(タクト短縮のため位置移動(12/13中村))
575     M_Out(12260)=1 Dly 0.3      'FANクランプ戻端パルス出力(タクト短縮のため位置移動(12/13中村))
576     Mov PScrewSupplyMain_1
577 '
578 '    M_Out(12258)=1 Dly 0.3      'プッシュCY用SV出端パルス出力
579     'Wait M_In(11268)=1          'プッシュ出端検出(修正につきコメントアウト(8/26中村))
580     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'プッシュ出端検出
581     If MRtn = 1 Then GoTo *Comp_Pos_2
582     fErrorProcess(11,231,282,0)
583     If M_20# = MNext% Then M_20# = MClear%
584     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
585     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
586     If M_20# = MContinue% Then GoTo *RE_POS
587     *Comp_Pos_2
588     '
589 '    M_Out(12260)=1 Dly 0.3      'FANクランプ戻端パルス出力(メインねじ締め後に変更 M.Hayakawa)(タクト短縮のため位置移動(12/13中村))
590     'Wait M_In(11270)=1          'FANクランプ戻端検出(修正につきコメントアウト(8/26中村))
591     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    'FANクランプ戻端検出(8/26中村)
592     If MRtn = 1 Then GoTo *Comp_Pos_3
593     fErrorProcess(11,231,282,0)
594     If M_20# = MNext% Then M_20# = MClear%
595     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
596     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
597     If M_20# = MContinue% Then GoTo *RE_POS
598     *Comp_Pos_3
599     '
600     '
601     'Main基板のネジ締め
602     'Main基板用ネジ供給機へネジを取りに行く
603     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
604     '
605     '*ScrewSupplyMain           '一時コメントアウト(以下5行,8/5中村)
606 '    Mov PScrewSupplyMain_2      'ネジ供給機回避点
607 '    Mov PScrewSupplyMain_1      'ネジピックアップ上空
608 '    Mvs PScrewSupplyMain        'ネジピックアップ
609 '    Mvs PScrewSupplyMain_1      'ネジピックアップ上空
610 '    Mov PScrewSupplyMain_2      'ネジ供給機回避点
611     'Return                     '一時コメントアウト(8/4中村)
612     'ScrewPositionDebug_1()      'デバック用(別関数使用のためコメントアウト(8/26中村))
613     '
614     PGetScrewPosTemp(1) = PScrewSupplyMain_1   'ネジピックアップ上空を代入(8/26中村)
615     PGetScrewPosTemp(2) = PScrewSupplyMain_2   'ネジ供給回避点を代入(8/26中村)
616     PGetScrewPosTemp(9) = PScrewSupplyMain_9   'ネジ供給機上空ネジ捨て位置(10/6 M.H追加)
617     PGetScrewPosTemp(10) = PScrewSupplyMain    'ネジピックアップを代入(8/26中村)
618     '
619     *RE_SCREW_GET_1                                'リトライ用ラベル
620     If M_20# = MContinue% Then M_20# = MClear%
621     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
622     If M_20# = MClear% Then GoTo *Comp_Screw_1
623     If M_20# = MNext% Then M_20# = MClear%
624     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
625     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
626     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
627     *Comp_Screw_1
628     '
629     '①番ネジ締め
630 '    Mov PScrewMain1_1           '①上空(以下5行一時コメントアウト(8/26中村))
631 '    Ovrd 5
632 '    Mvs PScrewMain1             '①ネジ着座
633 '    Ovrd 10
634 '    Mvs PScrewMain1_1           '①上空
635     PScrewPosTemp(1) = PScrewMain1_1    'ネジ1締め開始位置上空を代入(8/26中村)
636     PScrewPosTemp(2) = PScrewMain1_0    'ネジ1締め開始位置を代入(8/26中村)
637     PScrewPosTemp(10) = PScrewMain1     'ネジ1締め終了位置を代入(8/26中村)
638     M_Out16(12672) = 1                  'ネジ締め位置番号送信
639     MRtn = ScrewTight(PScrewPosTemp,1,10.0)    'ネジ1締めの実行(8/26中村)
640     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
641     If MRtn = 1 Then GoTo *CompScrew1
642 '
643     Mov PInitialPosition
644     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
645     MScrewErrorCord% = MScrewErrorCord% + 1
646     If MRtn = 0 Then
647         fErrorProcess(11,MScrewErrorCord%,52,0)
648     ElseIf MRtn = -1 Then
649         fErrorProcess(11,94,95,0)
650     EndIf
651 '    fErrorProcess(11,53,52,0)
652     If M_20# = MNext% Then M_20# = MClear%
653     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
654     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
655     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
656     *CompScrew1
657     '
658     'Main基板用ネジ供給機へネジを取りに行く
659     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
660     'ScrewPositionDebug_1()      'デバック用(別関数使用のためコメントアウト(8/26中村))
661     *RE_SCREW_GET_2                                'リトライ用ラベル
662     If M_20# = MContinue% Then M_20# = MClear%
663     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
664     If M_20# = MClear% Then GoTo *Comp_Screw_2
665     If M_20# = MNext% Then M_20# = MClear%
666     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
667     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
668     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
669     *Comp_Screw_2
670     '②番ネジ締め
671 '    Mov PScrewMain2_1           '②上空(以下5行一時コメントアウト(8/26中村))
672 '    Ovrd 5
673 '    Mvs PScrewMain2             '②ネジ着座
674 '    Ovrd 10
675 '    Mvs PScrewMain2_1           '②上空
676     PScrewPosTemp(1) = PScrewMain2_1    'ネジ2締め開始位置上空を代入(8/26中村)
677     PScrewPosTemp(2) = PScrewMain2_0    'ネジ2締め開始位置を代入(8/26中村)
678     PScrewPosTemp(10) = PScrewMain2     'ネジ1締め終了位置を代入(8/26中村)
679     M_Out16(12672) = 2                  'ネジ締め位置番号送信
680     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        'ネジ締め2の実行(8/26中村)
681     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
682     If MRtn = 1 Then GoTo *CompScrew2
683     Mov PInitialPosition
684     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
685     MScrewErrorCord% = MScrewErrorCord% + 2
686     If MRtn = 0 Then
687         fErrorProcess(11,MScrewErrorCord%,52,0)
688     ElseIf MRtn = -1 Then
689         fErrorProcess(11,94,95,0)
690     EndIf
691 '    fErrorProcess(11,54,52,0)
692     If M_20# = MNext% Then M_20# = MClear%
693     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
694     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
695     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
696     *CompScrew2
697     '
698     'Main基板用ネジ供給機へネジを取りに行く
699     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
700     *RE_SCREW_GET_3                                'リトライ用ラベル
701     If M_20# = MContinue% Then M_20# = MClear%
702     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
703     If M_20# = MClear% Then GoTo *Comp_Screw_3
704     If M_20# = MNext% Then M_20# = MClear%
705     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
706     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
707     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
708     *Comp_Screw_3
709     '③番ネジ締め
710 '    Mov PScrewMain3_1           '③上空(以下5行一時コメントアウト(8/26中村))
711 '    Ovrd 5
712 '    Mvs PScrewMain3             '③ネジ着座
713 '    Ovrd 10
714 '    Mvs PScrewMain3_1           '③上空
715     PScrewPosTemp(1) = PScrewMain3_1    'ネジ3締め開始位置上空を代入(8/26中村)
716     PScrewPosTemp(2) = PScrewMain3_0    'ネジ3締め開始位置を代入(8/26中村)
717     PScrewPosTemp(10) = PScrewMain3     'ネジ3締め終了位置を代入(8/26中村)
718     M_Out16(12672) = 3                  'ネジ締め位置番号送信
719     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        'ネジ締め3の実行(8/26中村)
720     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
721     If MRtn = 1 Then GoTo *CompScrew3
722     Mov PInitialPosition
723     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
724     MScrewErrorCord% = MScrewErrorCord% + 3
725     If MRtn = 0 Then
726         fErrorProcess(11,MScrewErrorCord%,52,0)
727     ElseIf MRtn = -1 Then
728         fErrorProcess(11,94,95,0)
729     EndIf
730 '    fErrorProcess(11,55,52,0)
731     If M_20# = MNext% Then M_20# = MClear%
732     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
733     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
734     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
735     *CompScrew3
736     '
737     'Main基板用ネジ供給機へネジを取りに行く
738     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
739     *RE_SCREW_GET_4                                'リトライ用ラベル
740     If M_20# = MContinue% Then M_20# = MClear%
741     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
742     If M_20# = MClear% Then GoTo *Comp_Screw_4
743     If M_20# = MNext% Then M_20# = MClear%
744     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
745     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
746     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
747     *Comp_Screw_4
748     '④番ネジ締め
749 '    Mov PScrewMain4_1           '④上空(以下5行一時コメントアウト(8/26中村))
750 '    Ovrd 5
751 '    Mvs PScrewMain4             '④ネジ着座
752 '    Ovrd 10
753 '    Mvs PScrewMain4_1           '④上空
754     PScrewPosTemp(1) = PScrewMain4_1    'ネジ4締め開始位置上空を代入(8/26中村)
755     PScrewPosTemp(2) = PScrewMain4_0    'ネジ4締め開始位置を代入(8/26中村)
756     PScrewPosTemp(10) = PScrewMain4     'ネジ4締め終了位置を代入(8/26中村)
757     M_Out16(12672) = 4                  'ネジ締め位置番号送信
758     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        'ネジ締め4の実行(8/26中村)
759     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
760     If MRtn = 1 Then GoTo *CompScrew4
761     Mov PInitialPosition
762     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
763     MScrewErrorCord% = MScrewErrorCord% + 4
764     If MRtn = 0 Then
765         fErrorProcess(11,MScrewErrorCord%,52,0)
766     ElseIf MRtn = -1 Then
767         fErrorProcess(11,94,95,0)
768     EndIf
769 '    fErrorProcess(11,56,52,0)
770     If M_20# = MNext% Then M_20# = MClear%
771     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
772     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
773     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
774     *CompScrew4
775     '
776     'Main基板用ネジ供給機へネジを取りに行く
777     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
778     *RE_SCREW_GET_5                                'リトライ用ラベル
779     If M_20# = MContinue% Then M_20# = MClear%
780     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
781     If M_20# = MClear% Then GoTo *Comp_Screw_5
782     If M_20# = MNext% Then M_20# = MClear%
783     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
784     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
785     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
786     *Comp_Screw_5
787     '⑤番ネジ締め
788 '    Mov PScrewMain5_1           '⑤上空(以下5行一時コメントアウト(8/26中村))
789 '    Ovrd 5
790 '    Mvs PScrewMain5             '⑤ネジ着座
791 '    Ovrd 10
792 '    Mvs PScrewMain5_1           '⑤上空
793     PScrewPosTemp(1) = PScrewMain5_1    'ネジ5締め開始位置上空を代入(8/26中村)
794     PScrewPosTemp(2) = PScrewMain5_0    'ネジ5締め開始位置を代入(8/26中村)
795     PScrewPosTemp(10) = PScrewMain5     'ネジ5締め終了位置を代入(8/26中村)
796     M_Out16(12672) = 5                  'ネジ締め位置番号送信
797     MRtn = ScrewTight(PScrewPosTemp,6,10.0)        'ネジ締め5の実行(8/26中村)
798     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
799     If MRtn = 1 Then GoTo *CompScrew5
800     Mov PInitialPosition
801     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
802     MScrewErrorCord% = MScrewErrorCord% + 5
803     If MRtn = 0 Then
804         fErrorProcess(11,MScrewErrorCord%,52,0)
805     ElseIf MRtn = -1 Then
806         fErrorProcess(11,94,95,0)
807     EndIf
808 '    fErrorProcess(11,57,52,0)
809     If M_20# = MNext% Then M_20# = MClear%
810     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
811     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
812     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
813     *CompScrew5
814     '
815     'Main基板用ネジ供給機へネジを取りに行く
816     'GoSub *ScrewSupplyMain     '一時コメントアウト(8/4中村)
817 '以下3行PP品にネジ穴がないため一時削除 9/16 M.Hayakawa
818     *RE_SCREW_GET_6                                'リトライ用ラベル
819     If M_20# = MContinue% Then M_20# = MClear%
820     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          'ネジ受け取り開始
821     If M_20# = MClear% Then GoTo *Comp_Screw_6
822     If M_20# = MNext% Then M_20# = MClear%
823     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
824     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
825     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
826     *Comp_Screw_6
827     '⑥番ネジ締め
828 '    Mov PScrewMain6_1           '⑥上空(以下5行一時コメントアウト(8/26中村))
829 '    Ovrd 5
830 '    Mvs PScrewMain6             '⑥ネジ着座
831 '    Ovrd 10
832 '    Mvs PScrewMain6_1           '⑥上空
833 '以下3行PP品にネジ穴がないため一時削除 9/16 M.Hayakawa
834     PScrewPosTemp(1) = PScrewMain6_1    'ネジ6締め開始位置上空を代入(8/26中村)
835     PScrewPosTemp(2) = PScrewMain6_0    'ネジ6締め開始位置を代入(8/26中村)
836     PScrewPosTemp(10) = PScrewMain6     'ネジ6締め終了位置を代入(8/26中村)
837     M_Out16(12672) = 6                  'ネジ締め位置番号送信
838     MRtn = ScrewTight(PScrewPosTemp,6,10.0)        'ネジ締め6の実行(8/26中村)
839     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
840     If MRtn = 1 Then GoTo *CompScrew6
841     Mov PInitialPosition
842     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
843     MScrewErrorCord% = MScrewErrorCord% + 6
844     If MRtn = 0 Then
845         fErrorProcess(11,MScrewErrorCord%,52,0)
846     ElseIf MRtn = -1 Then
847         fErrorProcess(11,94,95,0)
848     EndIf
849 '    fErrorProcess(11,58,52,0)
850     If M_20# = MNext% Then M_20# = MClear%
851     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
852     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
853     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
854     *CompScrew6
855     '
856     'FAN用ネジ供給機へネジを取りに行く
857     'GoSub *ScrewSupplyFan      '一時コメントアウト(8/4中村)
858 ' ネジ位置指定前に取りにいっている？ 1行一時削除 9/16 M.Hayakawa
859 '    MRtn = ScrewGet(PGetScrewPosTemp)       'ネジを取りに行く(8/26中村)
860     '
861 '    *ScrewSupplyFan
862 '    Mov PScrewSupplyFan_2       'ネジ供給機回避点
863 '    Mov PScrewSupplyFan_1       'ネジピックアップ上空
864 '    Mvs PScrewSupplyFan         ''ネジピックアップ
865 '    Mvs PScrewSupplyFan_1       'ネジピックアップ上空
866 '    Mov PScrewSupplyFan_2       'ネジ供給機回避点
867    ' Return                     '一時コメントアウト(8/4中村)
868     'ScrewPositionDebug_2()       'デバック用(別関数使用のためコメントアウト(8/26中村))
869     '
870     PGetScrewPosTemp(1) = PScrewSupplyFan_1   'ネジピックアップ上空を代入(8/26中村)
871     PGetScrewPosTemp(2) = PScrewSupplyFan_2   'ネジ供給回避点を代入(8/26中村)
872     PGetScrewPosTemp(9) = PScrewSupplyFan_9   'ネジ供給機上空ネジ捨て位置(10/6 M.H追加)
873     PGetScrewPosTemp(10) = PScrewSupplyFan    'ネジピックアップを代入(8/26中村)
874 '
875     *RE_SCREW_GET_7                                'リトライ用ラベル
876 '
877     If M_20# = MContinue% Then M_20# = MClear%
878     ScrewGet(PGetScrewPosTemp , 11260 , 0)          'ネジ受け取り開始
879     If M_20# = MClear% Then GoTo *Comp_Screw_7
880     If M_20# = MNext% Then M_20# = MClear%
881     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
882     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
883     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_7
884     *Comp_Screw_7
885     '⑦番ネジ締め
886 '    Mov PScrewFan1_1            '⑦上空(以下5行一時コメントアウト(8/26中村))
887 '    Ovrd 5
888 '    Mvs PScrewFan1              '⑦ネジ着座
889 '    Ovrd 10
890 '    Mvs PScrewFan1_1            '⑦上空
891     PScrewPosTemp(1) = PScrewFan1_1    'Fan1ネジ締め開始位置上空を代入(8/26中村)
892     PScrewPosTemp(2) = PScrewFan1_0    'Fan1ネジ締め開始位置を代入(8/26中村)
893     PScrewPosTemp(10) = PScrewFan1     'Fan1ネジ締め終了位置を代入(8/26中村)
894     M_Out16(12672) = 7                  'ネジ締め位置番号送信
895     MRtn = ScrewTight(PScrewPosTemp,2,10.0)       'Fanネジ締め1の実行(8/26中村)送り速度6.7から10.0へ7/27中村
896     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
897     If MRtn = 1 Then GoTo *CompScrew7
898     Mov PInitialPosition
899     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
900     MScrewErrorCord% = MScrewErrorCord% + 7
901     If MRtn = 0 Then
902         fErrorProcess(11,MScrewErrorCord%,52,0)
903     ElseIf MRtn = -1 Then
904         fErrorProcess(11,94,95,0)
905     EndIf
906 '    fErrorProcess(11,59,52,0)
907     If M_20# = MNext% Then M_20# = MClear%
908     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
909     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
910     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_7
911     *CompScrew7
912 '
913     '
914     'FAN用ネジ供給機へネジを取りに行く
915     'GoSub *ScrewSupplyFan      '一時コメントアウト(8/4中村)
916     'ScrewPositionDebug_2()      'デバック用(別関数使用のためコメントアウト(8/26中村))
917     *RE_SCREW_GET_8                                'リトライ用ラベル
918     If M_20# = MContinue% Then M_20# = MClear%
919     ScrewGet(PGetScrewPosTemp , 11260 , 0)          'ネジ受け取り開始
920     If M_20# = MClear% Then GoTo *Comp_Screw_8
921     If M_20# = MNext% Then M_20# = MClear%
922     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
923     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
924     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_8
925     *Comp_Screw_8
926     '⑧番ネジ締め
927 '    Mov PScrewFan2_1            '⑧上空(以下5行一時コメントアウト(8/26中村))
928 '    Ovrd 5
929 '    Mvs PScrewFan2              '⑧ネジ着座
930 '    Ovrd 10
931 '    Mvs PScrewFan2_1            '⑧上空
932     PScrewPosTemp(1) = PScrewFan2_1    'Fan2ネジ締め開始位置上空を代入(8/26中村)
933     PScrewPosTemp(2) = PScrewFan2_0    'Fan2ネジ締め開始位置を代入(8/26中村)
934     PScrewPosTemp(10) = PScrewFan2     'Fan2ネジ締め終了位置を代入(8/26中村)
935     M_Out16(12672) = 8                  'ネジ締め位置番号送信
936     MRtn = ScrewTight(PScrewPosTemp,2,10.0)       'Fanネジ締め2の実行(8/26中村)送り速度6.7から10.0へ7/27中村
937     M_Out16(12672) = 0                  'ネジ締め位置番号クリア
938     If MRtn = 1 Then GoTo *CompScrew8
939     Mov PInitialPosition
940     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
941     MScrewErrorCord% = MScrewErrorCord% + 8
942     If MRtn = 0 Then
943         fErrorProcess(11,MScrewErrorCord%,52,0)
944     ElseIf MRtn = -1 Then
945         fErrorProcess(11,94,95,0)
946     EndIf
947     If M_20# = MNext% Then M_20# = MClear%
948     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
949     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
950     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_8
951     *CompScrew8
952 '
953     'プログラム原点
954     'Mov PInitialPosition        ' 原点回避
955     MRtn = FnCtlValue2(2)       '組立ＯＫ＋１  2022/04/28 渡辺
956     Mov PTicketRead_1           ' チケットリード位置
957     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
958     InitialState()              ' 初期状態にする
959     M_20# = MAssyOK%              ' 正常終了処理
960     GoTo *fnAssyStart_FEndPosi
961 '
962 *ASSY_ERROR_END
963     fnInitialZone() ' 初期位置に移動
964 *AssyEnd
965     InitialState()  ' 初期状態にする
966 *fnAssyStart_FEndPosi
967     Exit Function
968 FEnd
969 '
970 '■fnPiasCheck
971 ''' <summary>
972 ''' PIASチケット読込み
973 ''' </summary>
974 ''' <returns>   0 : NG
975 '''             1 : OK(読込み完了)
976 ''' </returns>
977 ''' <remarks>
978 ''' Date   : 2021/07/07 : M.Hayakawa
979 ''' </remarks>'
980 ''' <Update>
981 ''' Date   : 2022/01/11 : 中村
982 ''' </Update>
983 Function M% fnPiasCheck
984     fnPiasCheck = 0
985     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
986     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功
987 '
988 *RETRY_PIAS
989     M_20# = MClear%
990     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
991     '
992     '【IDチケット読み込み】
993     PInspPosition(1) = PTicketRead  'IDチケット読取位置
994     MInspGroup%(1) = 1              '検査G番号
995     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
996 '
997     'エラーの場合
998     If MRtn <> 1 Then
999         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1000         If MRtn <> 1 Then
1001             'D720 -> D1300 コピー要求
1002             M_Out(12565) = 1
1003             Dly 0.5
1004             M_Out(12565) = 0
1005             'エラー処理記述
1006             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1007             'GOT KEY入力待ち
1008             MKeyNumber = fnKEY_WAIT()
1009             '
1010             Select MKeyNumber
1011                 Case MNext%         '次へを選択した場合
1012                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1013                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1014                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1015                     Break
1016                 Case MAbout%        '停止を選択した場合
1017                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1018                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1019                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1020                     Break
1021                 Case MNgProcess%    'NGを選択した場合
1022                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1023                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1024                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1025                     Break
1026                 Case MContinue%     '継続を選択した場合
1027                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1028                     M_20# = MContinue%
1029                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1030                     Break
1031             End Select
1032         EndIf
1033     EndIf
1034     If M_20# = MPass% Then GoTo *fnPiasCheck_End            'PIASチェック終了
1035     If M_20# = MAbout% Then GoTo *fnPiasCheck_End           'PIASチェック終了
1036     If M_20# = MNgProcess% Then GoTo *fnPiasCheck_End       'PIASチェック終了
1037     If M_20# = MContinue% Then GoTo *RETRY_PIAS             'PIASチェックリトライ
1038 '----------D720 -> D1300 コピー要求----------
1039     M_Out(12565) = 1
1040     Dly 0.5
1041     M_Out(12565) = 0
1042 '----------通信確認をする----------
1043     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1044     MRtn = 0                ' 初期化
1045     M_20# = MClear%         ' 初期化
1046     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1047     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1048 '    If MRtn <> 1 Then
1049 '        If M_20# = MContinue% Then
1050 '            GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1051 '        Else
1052 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1053 '        EndIf
1054 '    EndIf
1055     If MRtn = 1 Then GoTo *PCComu_OK                '通信OK時ラベルへジャンプ
1056     If M_20# = MContinue% Then GoTo *RETRY_PIAS      'チケット読み直しからリトライ
1057     GoTo *fnPiasCheck_End                           'その他の場合PIASチェック終了
1058     *PCComu_OK
1059 '----------工程抜け確認----------
1060     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1061     MRtn = 0                ' 初期化
1062     M_20# = MClear%         ' 初期化
1063     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1064     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1065 '    If MRtn <> 1 Then
1066 '        If M_20# = MContinue% Then
1067 '            GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1068 '        Else
1069 '            GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1070 '        EndIf
1071 '    EndIf
1072     If MRtn = 1 Then GoTo *ProcessCheck_OK                '工程チェックOK時ラベルへジャンプ
1073     If M_20# = MContinue% Then GoTo *RETRY_PIAS      'チケット読み直しからリトライ
1074     GoTo *fnPiasCheck_End                           'その他の場合PIASチェック終了
1075     *ProcessCheck_OK
1076     '
1077     fnPiasCheck = 1
1078     *fnPiasCheck_End
1079     Exit Function
1080 FEnd
1081 '
1082 '■fnPCComuCheck
1083 ''' <summary>
1084 ''' PC-PLC通信チェック
1085 ''' </summary>
1086 ''' <returns>   0 : NG
1087 '''             1 : OK(読込み完了)
1088 ''' </returns>
1089 ''' <remarks>
1090 ''' Date   : 2021/07/07 : M.Hayakawa
1091 ''' </remarks>'
1092 Function M% fnPCComuCheck
1093     fnPCComuCheck = 0
1094     MJudge% = 0                                  '初期化
1095     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1096     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1097     '
1098     For MStaNo = 0 To 5
1099         '
1100         If M_In(MIN_PIAS_ComOK%) = 1 Then
1101             'PC通信OK(M400)
1102             MJudge% = MOK%
1103             MStaNo = 5
1104             Break
1105         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1106             'toRBT_通信確認time out
1107             MJudge% = MNG%
1108             MCommentD1001 = 15
1109             MCommentD1002 = 21
1110             MStaNo = 5
1111             Break
1112         Else
1113             'toRBT_通信確認time out
1114             MJudge% = MNG%
1115             MCommentD1001 = 14
1116             MCommentD1002 = 21
1117             Break
1118         EndIf
1119     Next MStaNo
1120     '
1121     '上記で返信フラグを受信してからPC通信確認OFF
1122     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1123     '
1124     'エラー画面
1125     If MJudge% <> MOK% Then
1126         M_20# = MClear%     '初期化
1127         'エラー処理記述
1128         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1129         'GOT KEY入力待ち
1130         MKeyNumber = fnKEY_WAIT()
1131         '
1132         If MKeyNumber = MAbout% Then            '停止を選択した場合
1133             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1134             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1135             Break
1136         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1137             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1138             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1139             Break
1140         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1141             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1142             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1143             Break
1144         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1145             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1146             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1147             Break
1148         EndIf
1149     Else
1150         'OKの場合
1151         fnPCComuCheck = 1
1152     EndIf
1153     Exit Function
1154 FEnd
1155 '
1156 '■fnProcessCheck
1157 ''' <summary>
1158 ''' 工程抜け確認
1159 ''' </summary>
1160 ''' <returns>    1：工程履歴OK     0：異常終了
1161 '''             -1：前工程履歴NG  -2：自工程履歴あり
1162 '''             -3：モデル仕向NG  -4：タイムアウト
1163 '''             -5：履歴処理エラー
1164 ''' </returns>
1165 ''' <remarks>
1166 ''' Date   : 2021/07/07 : M.Hayakawa
1167 ''' </remarks>'
1168 Function M% fnProcessCheck
1169     fnProcessCheck = 0
1170     MJudge% = MNG%      '一旦NGを初期化とする
1171 '----------工程抜け確認----------
1172     MCommentD1001 = 0   'コメント初期化
1173     For MStaNo = 0 To 5
1174         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1175         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1176         '
1177         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1178             MJudge% = MOK%
1179             fnAutoScreenComment(85)     ' AUTO画面
1180             MStaNo = 5
1181             Break
1182         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1183             MFlgLoop% = 0
1184             MJudge% = MNG%
1185             MCommentD1001 = 27
1186             MCommentD1002 = 22
1187             fnAutoScreenComment(94)     ' AUTO画面
1188             fnProcessCheck = -2         ' NGは-2を返す
1189             MStaNo = 5
1190             Break
1191         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1192            MJudge% = MNG%
1193             MCommentD1001 = 31
1194             MCommentD1002 = 22
1195             fnAutoScreenComment(83)     ' AUTO画面
1196             fnProcessCheck = -3         ' NGは-3を返す
1197             MStaNo = 5
1198             Break
1199         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1200             '履歴NGは直ぐに終了せず繰り返し確認を行う
1201             '前工程の書込みが終了していない可能性があるため
1202             MJudge% = MNG%
1203             MCommentD1001 = 32
1204             MCommentD1002 = 22
1205             fnAutoScreenComment(84)     ' AUTO画面
1206             fnProcessCheck = -1         ' NGは-1を返す
1207             Dly 1.0
1208             '工程抜け確認OFF
1209             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1210             Dly 1.0
1211            'MStaNo = 5
1212             Break
1213         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1214             MFlgLoop% = 0
1215             MJudge% = MNG%
1216             MCommentD1001 = 29
1217             MCommentD1002 = 22
1218             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1219             fnProcessCheck = -5         ' NGは-5を返す
1220             MStaNo = 5
1221             Break
1222         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1223             MJudge% = MNG%
1224             If MCommentD1001 = 32 Then
1225                 '何もしない
1226             Else
1227                 MCommentD1001 = 26
1228             EndIf
1229             MCommentD1002 = 22
1230             fnProcessCheck = -4         ' NGは-4を返す
1231             MStaNo = 5
1232             Break
1233         Else
1234             MJudge% = MNG%
1235             MCommentD1001 = 28
1236             MCommentD1002 = 22
1237         EndIf
1238     Next MStaNo
1239     '工程抜け確認OFF
1240     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1241     '通過履歴NG 工程抜けの場合
1242     If MJudge% = MPass% Then
1243         M_20# = MPass%
1244     EndIf
1245     '
1246     'エラー画面
1247     If MJudge% <> MOK% Then
1248         M_20# = MClear%     '初期化
1249         'エラー処理記述
1250         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1251         'GOT KEY入力待ち
1252         MKeyNumber = fnKEY_WAIT()
1253         '
1254         Select MKeyNumber
1255             Case MAbout%        '停止を選択した場合
1256                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1257                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1258                 Break
1259             Case MNext%         '次へを選択した場合
1260                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1261                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1262                 Break
1263             Case MContinue%     '継続を選択した場合
1264                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1265                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1266                 Break
1267             Case MNgProcess%    'NGを選択した場合
1268                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1269                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1270                 Break
1271         End Select
1272     Else
1273         fnProcessCheck = 1  ' OKは1を返す
1274     EndIf
1275     Exit Function
1276 FEnd
1277 '
1278 '■fnPiasWrite
1279 ''' <summary>
1280 ''' Pias 組立結果書込み要求
1281 ''' </summary>
1282 '''<param name="MFlg%">
1283 ''' MOK%(1) = 工程履歴にOKを書込む
1284 ''' MNG%(0) = 工程履歴にNGを書込む
1285 '''</param>
1286 '''<returns></returns>
1287 ''' <remarks>
1288 ''' Date   : 2021/07/07 : M.Hayakawa
1289 ''' </remarks>'
1290 Function M% fnPiasWrite(ByVal MFlg%)
1291       fnPiasWrite = 0
1292 *RETRY_PIASWRITE
1293     '
1294     '組立OK(MOK%)の場合　M306 ON
1295    '組立NG(MNG%)の場合　M307 ON
1296     If MFlg% = MOK% Then
1297         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1298     Else
1299         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1300     EndIf
1301     Dly 0.1                  '念のため
1302     '
1303     'Piasへ書込み開始 M305 -> ON
1304     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1305     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1306     '
1307     MJudge% = MNG%
1308     '
1309     For MStaNo = 0 To 5
1310         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1311             MJudge% = MOK%
1312             'MRet = fnAutoScreenComment(85)  'AUTO画面
1313             MStaNo = 5
1314             Break
1315         '
1316         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1317             MJudge% = MNG%
1318             'MRet = fnAutoScreenComment(85)  'AUTO画面
1319            MCommentD1001 = 34
1320            MCommentD1002 = 25
1321             MStaNo = 5
1322             Break
1323         '
1324         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1325             MJudge% = MNG%
1326             'MRet = fnAutoScreenComment(85)  'AUTO画面
1327            MCommentD1001 = 35
1328            MCommentD1002 = 25
1329             MStaNo = 5
1330             Break
1331         '
1332         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1333             MJudge% = MNG%
1334             'MRet = fnAutoScreenComment(85)  'AUTO画面
1335            MCommentD1001 = 36
1336            MCommentD1002 = 25
1337             MStaNo = 5
1338             Break
1339         '
1340         Else
1341             MJudge% = MNG%
1342            MCommentD1001 = 42
1343            MCommentD1002 = 25
1344         '
1345         EndIf
1346         '
1347     Next MStaNo
1348     '
1349     'Piasへ書込み開始 M305 -> OfF
1350     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1351     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1352     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1353     '
1354     '
1355     '通過履歴NG 工程抜けの場合
1356     If MJudge% = MPass% Then
1357         M_20# = MPass%
1358     EndIf
1359     '
1360    M_20# = MClear%     '初期化
1361     '
1362     'エラー画面
1363     If MJudge% < MOK% Then
1364     '
1365 '残しておくが現状では使用しないラベル
1366 *RETRY_ERR_WRITE
1367         M_20# = MClear%     '初期化
1368         'エラー処理記述
1369         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1370         'GOT KEY入力待ち
1371         MKeyNumber = fnKEY_WAIT()
1372         '
1373         If MKeyNumber = MAbout% Then   '停止を選択した場合
1374             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1375            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1376             Break
1377         '
1378         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1379             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1380             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1381             Break
1382         '
1383         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1384             M_20# = MPass%            'M_20# プログラム間共通外部変数
1385             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1386             Break
1387         '
1388         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1389             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1390            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1391             Break
1392         '
1393         EndIf
1394         '
1395 '        If M_20# = MClear% Then *RETRY_ERR_WRITE
1396         '
1397     EndIf
1398     '
1399     If M_20# = MContinue% Then *RETRY_PIASWRITE
1400     '
1401     fnPiasWrite = 1
1402     Exit Function
1403 FEnd
1404 '
1405 '■fnPCBNumberCheck
1406 ''' <summary>
1407 ''' Pias 基板番号照合要求
1408 ''' </summary>
1409 '''<returns>0（固定）</returns>
1410 ''' <remarks>
1411 ''' Date   : 2021/07/07 : M.Hayakawa
1412 ''' </remarks>'
1413 Function M% fnPCBNumberCheck
1414       fnPCBNumberCheck = 0
1415     '
1416 *RETRY_PCBCHECK
1417     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1418     'Piasへ基板照合開始 M310 -> ON
1419     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1420     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1421     '
1422     MJudge% = MNG%
1423     '
1424     For MStaNo = 0 To 5
1425         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1426             MJudge% = MOK%
1427             fnAutoScreenComment(96)  'AUTO画面
1428             MStaNo = 5
1429             Break
1430         '
1431         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1432             MJudge% = MNG%
1433             fnAutoScreenComment(97)  'AUTO画面
1434             MCommentD1001 = 37
1435             MCommentD1002 = 25
1436             MStaNo = 5
1437             Break
1438         '
1439         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1440             MJudge% = MNG%
1441             fnAutoScreenComment(98)  'AUTO画面
1442             MCommentD1001 = 38
1443             MCommentD1002 = 25
1444             MStaNo = 5
1445             Break
1446         '
1447         ElseIf M_In(11580) = 1 Then                         'time out
1448             MJudge% = MNG%
1449             fnAutoScreenComment(99)  'AUTO画面
1450             MCommentD1001 = 39
1451             MCommentD1002 = 25
1452             MStaNo = 5
1453             Break
1454         '
1455         Else
1456             MJudge% = MNG%
1457            MCommentD1001 = 41
1458            MCommentD1002 = 25
1459         '
1460         EndIf
1461         '
1462     Next MStaNo
1463     '
1464     'Piasへ基板照合開始 M310 -> OfF
1465     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1466     '
1467     '
1468     '通過履歴NG 工程抜けの場合
1469     If MJudge% = MPass% Then
1470         M_20# = MPass%
1471     EndIf
1472     '
1473    M_20# = MClear%     '初期化
1474     '
1475     'エラー画面
1476     If MJudge% < MOK% Then
1477     '
1478 '残しておくが現状では使用しないラベル
1479 *RETRY_ERR_PCBNUMBER
1480         M_20# = MClear%     '初期化
1481         'エラー処理記述
1482         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1483         'GOT KEY入力待ち
1484         MKeyNumber = fnKEY_WAIT()
1485         '
1486         If MKeyNumber = MAbout% Then   '停止を選択した場合
1487             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1488             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1489             Break
1490         '
1491         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1492             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1493             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1494         '
1495         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1496             M_20# = MPass%            'M_20# プログラム間共通外部変数
1497             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1498         '
1499         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1500             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1501             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1502             Break
1503         '
1504         EndIf
1505         '
1506 '        If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1507         '
1508     EndIf
1509     '
1510     If M_20# = MContinue% Then *RETRY_PCBCHECK
1511     Exit Function
1512 FEnd
1513 '
1514 '■ScrewTight
1515 ''' <summary>
1516 ''' ねじ締めを行う(Sタイト)
1517 ''' </summary>
1518 '''<param name="PScrewPos()">
1519 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1520 '''             PScrewPos(2)    ：ねじ締め回避点
1521 '''             PScrewPos(10)   ：ねじ締め終了高さ
1522 '''<param name="MScrewType">ネジタイプ(mm/sec)
1523 '''             1:6mm Sタイト銀ネジ
1524 '''             2:8mm Pタイト
1525 '''             3:6mm Sタイト黒ネジ
1526 '''             4:13mm Sタイト
1527 '''             5:6mm Mネジ
1528 '''</param>
1529 '''<param name="MFeedSpd">送り速度(mm/sec)</param>
1530 '''<returns>整数
1531 '''         0=異常終了、1=正常終了、ねじ無し=-1
1532 '''</returns>
1533 ''' <remarks>
1534 ''' Date   : 2021/07/07 : M.Hayakawa
1535 ''' Update : 2021/09/28 : M.Hayakawa ネジタイプ、送り速度を引数に追加
1536 ''' Update : 2022/07/08 : 中村 上空で吸着確認するように変更
1537 ''' </remarks>'
1538 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   'ネジ締め個別設定
1539     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
1540     ScrewTight = 0
1541     MOKNGFlg = 0
1542     Ovrd 100
1543     Mov PScrewPosition(1)       ' パレット上ねじ締めS①の安全回避位置
1544     MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1545     If MRtn = 0 Then
1546        ScrewTight = -1
1547     EndIf
1548     If MRtn = 0 Then GoTo *ScrewEnd
1549 '
1550     Fine 0.05 , P
1551     Ovrd MOvrdA%
1552     ' 減速設定
1553     Accel 100, 10
1554     ' パレット上ねじ締め開始位置へ移動
1555     Mvs PScrewPosition(2)
1556     ' 加減速を元に戻す
1557     Accel
1558     ' 内部Ovrd設定
1559 '    Ovrd MOvrdA%
1560     Ovrd 100
1561     ' Spd設定
1562 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
1563     Spd MFeedSpd
1564     ' 設定進み量5.0 × 操作パネルのオーバーライド係数 × プログラム内オーバーライド係数
1565     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
1566     Select MScrewType%
1567         Case 1
1568             ' Sタイト：プログラム1、バンク1に設定
1569             ProgramBankSet(1,1)
1570             Break
1571         Case 2
1572             ' Pタイト：プログラム1、バンク1に設定
1573             ProgramBankSet(3,1)
1574             Break
1575         Case 3
1576             ' Sタイト黒：プログラム1、バンク1に設定
1577             ProgramBankSet(1,1)
1578             Break
1579         Case 4
1580             ' Sタイト13mm：プログラム1、バンク1に設定
1581             ProgramBankSet(1,1)
1582             Break
1583         Case 5
1584             ' Mネジ：プログラム1、バンク1に設定
1585             ProgramBankSet(1,1)
1586             Break
1587         Case 6
1588             ' Sタイト：プログラム1、バンク4に設定
1589             ProgramBankSet(1,4)
1590             Break
1591         Default
1592             ' プログラム1、バンクなし設定
1593             ProgramBankSet(0,0)
1594             Break
1595     End Select
1596 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     'ドライバーON　CW
1597      'ドライバーON　CW
1598     M_Out(12241)=1
1599     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1600     Wait M_In(11584)=1          '完了/エラー検出 暫定コメント 10/6 M.H
1601     Fine 0 , P
1602     Dly 0.1
1603     Spd M_NSpd
1604     '
1605     If M_In(11256)=1 Then  'ねじトータルエラー検出時
1606         M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1607         Dly 0.1
1608        ' プログラム・バンク解除
1609         ProgramBankSet(0,0)
1610         'パレット上ねじ締め終了位置上空へ移動
1611         Mvs PScrewPosition(10),-80
1612         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1613         M_Out(12249)=1 Dly 0.3
1614         MOKNGFlg = -1
1615         ScrewTight = 0
1616     Else
1617          'ドライバーOFF　CW
1618         M_Out(12241)=0
1619          'ドライバーOFF　CW
1620         M_Out(12241)=0
1621        ' プログラム・バンク解除
1622         ProgramBankSet(0,0)
1623         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1624         M_Out(12249)=1 Dly 0.3
1625     '     ↓PScrewPos(2) → PScrewPosition(10)に変更 9/16 M.Hayakawa
1626         'パレット上ねじ締め終了位置上空へ移動
1627        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1628         'Mvs PScrewPosition(10),-80
1629         ScrewTight = 1
1630     EndIf
1631 ' 暫定（暫定マスク　9/16 M.Hayakawa)
1632 '    Ovrd 10
1633 '    Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1634     Ovrd 100
1635     Exit Function
1636 *ScrewEnd
1637 FEnd
1638 '
1639 '■ScrewGet
1640 ''' <summary>
1641 ''' ねじ供給機からねじを得る
1642 ''' </summary>
1643 '''<param name="%">
1644 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1645 '''         PScrewPos(2)    ：ねじ供給器回避点
1646 '''         PScrewPos(9)    ：ねじ供給器上空ネジ捨位置
1647 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1648 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1649 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1650 '''</param>
1651 '''<param name = FeederReadyNo%> 指定の供給機Ready </param>
1652 '''<param name = FeederScrewSensor%> 指定の誤供給防止センサー指定(0でセンサー無し)</param>
1653 '''<returns>整数
1654 '''         0=異常終了、1=正常終了、-1=ねじ供給NG、-2=ねじ誤供給NG、-3=吸着エラー
1655 '''</returns>
1656 ''' <remarks>
1657 ''' Date   : 2021/07/07 : M.Hayakawa
1658 ''' </remarks>
1659 '''<update>
1660 '''Date    : 2021/11/15 : 中村
1661 '''Date    : 2021/02/07 : 早川 念のため確認を削除
1662 '''</update>
1663 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1664     fnAutoScreenComment(522)    '状態表示[ネジ供給待ち] 2022/05/09 渡辺
1665     ScrewGet = 0
1666     MScrewJudge% = 0
1667     'ねじ供給器初期動作エラーチェック
1668 '
1669     For MCnt% = 0 To MFinCnt%
1670         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '指定ねじ供給機がReadyになっているか確認(5秒間)
1671         If MRtn = 0 Then
1672             M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1673             ScrewGet = -1
1674             MScrewJudge% = 2
1675         EndIf
1676         Ovrd 100
1677         If FeederScrewSensor% <> 0 Then
1678             If M_In(FeederScrewSensor%) = 1 Then  '誤供給が検出されたら
1679                 'Ovrd 30
1680                 M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1681                 'NGとしてここの関数から抜ける
1682                 ScrewGet = -2
1683                 MScrewJudge% = 3
1684             EndIf
1685         EndIf
1686         Ovrd 100
1687         Spd M_NSpd
1688         If MScrewJudge% = 0 Then
1689     '        ScrewGet = 0
1690             M_Out(Y63_Driver)=1         ' バンクセッティング　C2
1691 '            Dly 0.3                    ' CD用に伴いコメント化(FA2と共通化） M.H
1692             MScrewCnt% = 0
1693             MFinCnt% = 2
1694             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
1695             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1696             'ビット回転(シーケンス見直しにつき処理位置変更4/3中村)
1697             M_Out(Y60_Driver)=1
1698             'ビット回転安定状態監視開始
1699             '
1700             '
1701             'Ovrd 40 '2に変更 10/6 M.H '5に変更10/7中村
1702             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1703             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
1704             'Mvs PScrewPosition(10), 1.2
1705            Mvs PScrewPosition(10)       'Fan用ねじ吸着位置修正のため変更 2022-02-01AJI
1706 '            'ビット回転(シーケンス見直しにつき処理位置変更4/3中村)
1707 '            M_Out(Y60_Driver)=1
1708 '            'ビット回転安定状態監視開始
1709             M_Timer(4) = 0
1710             MloopFlg = 0
1711             MCrtTime& = 0
1712            'ビット回転安定まで待機
1713             While MloopFlg = 0
1714                 MCrtTime& = M_Timer(4)
1715                 If MCrtTime& >= 180 Then
1716                     MloopFlg = 1
1717                 EndIf
1718             WEnd
1719             '
1720            M_Out(Y68_VV1)=1 Dly 0.3     ' ねじ吸着　ON'Dly 0.3追加(8/27中村)
1721             'ビット回転(シーケンス見直しにつき処理位置変更3/30中村)
1722 '            M_Out(Y60_Driver)=1
1723 '            Dly 0.2
1724             '吸着位置にて吸着確認
1725             MRtn = 0
1726             MRtn = frInCheck(11264, 1, MSETTIMEOUT01&)      'チェックはするがエラー判定はしない
1727             '
1728             JOvrd M_NJovrd
1729             Spd M_NSpd
1730             'ネジ吸着確認位置移動
1731             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
1732             Mvs PScrewPosition(10), -30  ' ネジ吸着確認位置
1733            'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1734             'ビット回転停止
1735             M_Out(Y60_Driver)=0
1736             '
1737 '            If MRtn = 1 Then            'シーケンス変更につきコメントアウト(5/13中村)
1738                 '1秒間ネジ吸着確認 始めの閾値
1739                 MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1740 '            EndIf                       'シーケンス変更につきコメントアウト(5/13中村)
1741             'MRtn = 0'強制エラー
1742             '吸着エラーの場合
1743             'ネジをねじ太郎に戻す
1744             If MRtn = 0 Then
1745                 Ovrd 30      '2から5に変更'5から30に変更(3/30中村)
1746                 'ビット回転停止
1747                 M_Out(Y60_Driver)=0
1748                 'ネジ供給機上空
1749                 Mvs PScrewPosition(1)
1750                 '更に上空
1751                 Mov PScrewPosition(1), -140
1752                 'ネジ捨て位置
1753                 If FeederReadyNo% = 11260 Then     '供給機別に吸着エラー数をカウント　2022/05/19 渡辺
1754                     MRtn = FnCtlValue2(3)          '供給機２吸着エラー数＋１
1755                 Else
1756                     MRtn = FnCtlValue2(4)          '供給機１吸着エラー数＋１  2022/04/28 渡辺
1757                 EndIf
1758                 Mov PScrewPosition(9)
1759                 MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1760                 '吸着OFF
1761                 M_Out(12249)=1 Dly 0.3 'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1762                 Dly 0.2
1763                 '破壊ON
1764                 M_Out(Y6B_VB1)=1 '真空破壊ON
1765                 'ビット回転
1766                 M_Out(Y61_Driver)=1
1767                 Dly 0.5
1768                 '                '
1769                 Ovrd 100
1770                 JOvrd M_NJovrd
1771                 Spd M_NSpd
1772                 'ドライバーを上下させねじを振り落とす
1773                 Mov PScrewPosition(9), 10
1774                 Mov PScrewPosition(9)
1775                 Dly 0.1
1776                 Mov PScrewPosition(9), 10
1777                 Mov PScrewPosition(9)
1778                 '
1779                 'ネジ落ち待ち
1780                 Wait M_In(11265) = 0
1781                 'ビット回転停止
1782                 M_Out(Y61_Driver)=0
1783                 Dly 0.1
1784                 '破壊OFF
1785                 M_Out(Y6B_VB1)=0 '真空破壊OFF
1786                 'ねじ落ちたとして、移動更に上空
1787                 Mov PScrewPosition(1), -140
1788                 Ovrd 100
1789                 Spd M_NSpd
1790                 'ネジ供給機上空
1791                 Mvs PScrewPosition(1)
1792 '                '
1793                 ScrewGet = -3
1794                 If MCnt% = MFinCnt% Then
1795                     MScrewJudge% = 4
1796                     Mov PScrewPosition(2)
1797                     Break
1798                 EndIf
1799                 Break
1800 '                '
1801             Else
1802                 MCnt% = MFinCnt%
1803                 ScrewGet = 1
1804             EndIf
1805         Else
1806             MCnt% =MFinCnt%
1807         EndIf
1808     Next  MCnt%
1809         '
1810 '    If MScrewJudge% = 0 Then
1811 '        Ovrd 100
1812 '        Spd M_NSpd
1813 '        PScrewPosition(1)
1814 '        Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1815 '        'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1816 '        M_Out(Y60_Driver)=0     ' ビット回転停止
1817 '        M_Out(Y63_Driver)=0     ' バンクセッティング　C2
1818 '        'Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1819 '        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1820 '        'Mov PScrewPosition(2)
1821 '        'もう一度吸着確認　上空の最終閾値
1822 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1823 '        If MRtn = 0 Then      '吸着エラーの場合
1824 '            MScrewJudge% = 4
1825 '            ScrewGet = -3
1826 '        ElseIf MRtn = 1 Then      '吸着OKの場合
1827 '            MScrewJudge% = 1
1828 '            ScrewGet = 1
1829 '        EndIf
1830 '        Break
1831 '    EndIf
1832     '
1833 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1834     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1835     '
1836     Select MScrewJudge%
1837 '        Case 0
1838 ''            fErrorProcess(11,162,163,0) '異常終了
1839 '            MCommentD1001 = 162
1840 '            MCommentD1002 = 96
1841 '            Break
1842         Case 2
1843 '            fErrorProcess(11,63,161,0) '供給NG
1844             MCommentD1001 = 63
1845             MCommentD1002 = 96
1846             Break
1847         Case 3
1848 '            fErrorProcess(11,160,164,0) '誤供給
1849             MCommentD1001 = 237
1850             MCommentD1002 = 96
1851             Break
1852         Case 4
1853 '            fErrorProcess(11,94,95,0) '吸着NG
1854             MCommentD1001 = 94
1855             MCommentD1002 = 95
1856             Break
1857     End Select
1858     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1859     '
1860     Select M_20#
1861         Case MAbout%          '停止が押された場合
1862             Mov PScrewPosition(2)                  '初期位置に戻って停止処理
1863             Mov PInitialPosition
1864             Break
1865         Case MContinue%       'リトライが押されていた場合(関数を抜けた先で処理)
1866             Break
1867         Case MNext%           '継続が押された場合
1868             M_20# = MClear%     '初期化
1869             Break
1870         Case MNgProcess%      'NGが押された場合
1871             Mov PScrewPosition(2)   'PIASにNG書き込みを行い,初期位置に戻って行程終了
1872             Mov PInitialPosition
1873             Break
1874         End Select
1875 *End_ScrewGet
1876     Exit Function
1877 FEnd
1878 '
1879 '■ProgramBankSet
1880 ''' <summary>
1881 ''' ねじ締めを行う(Pタイト)
1882 ''' </summary>
1883 '''<param name="MProgramNo">プログラム番号</param>
1884 '''<param name="MBankNo">バンク番号</param>
1885 '''</returns>
1886 ''' <remarks>
1887 ''' Date   : 2021/10/05 : M.Hayakawa
1888 ''' </remarks>'
1889 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1890 '
1891     MLocalPrgNo% = (MProgramNo% - 1) * 32
1892     MLocalBankNo% = MBankNo% * 4
1893 '
1894     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1895         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1896     Else
1897         MLocalOutNo% = 0
1898     EndIf
1899 '
1900     M_Out8(12240) = MLocalOutNo%
1901     Dly 0.1
1902     Exit Function
1903 FEnd
1904 '
1905 '■fnKEY_WAIT()
1906 ''' <summary>
1907 ''' GOTからのキー入力待ち
1908 ''' </summary>
1909 '''<returns>1：停止    2：次へ
1910 '''         3：継続    4：トルクチェック開始
1911 '''         5：NG
1912 '''         11：ロボット初期位置1    12：ロボット初期位置2
1913 '''         13：ロボット初期位置3    14：ロボット初期位置4
1914 '''</returns>
1915 ''' <remarks>
1916 ''' Date   : 2021/07/07 : M.Hayakawa
1917 ''' </remarks>'
1918 Function M% fnKEY_WAIT()
1919     fnKEY_WAIT = 0
1920     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
1921     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
1922     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
1923     '下記キー待ちの継続に反応させないため
1924     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
1925     Dly 0.2
1926     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
1927     MLocalLoopFlg=1
1928     While MLocalLoopFlg=1
1929         If M_In(11345) = 1 Then         '停止   M5345
1930             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
1931             fnKEY_WAIT = 1
1932             MLocalLoopFlg=-1
1933             Break
1934         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
1935             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
1936             fnKEY_WAIT = 2
1937             MLocalLoopFlg=-1
1938             Break
1939         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
1940             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
1941             fnKEY_WAIT = 3
1942             MLocalLoopFlg=-1
1943             Break
1944         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
1945             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
1946             fnKEY_WAIT = 4
1947             MLocalLoopFlg=-1
1948             Break
1949         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
1950             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
1951             fnKEY_WAIT = 5
1952             MLocalLoopFlg=-1
1953             Break
1954         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
1955             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
1956             fnKEY_WAIT = MRobotInit1%
1957             MLocalLoopFlg=-1
1958             Break
1959         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
1960             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
1961             fnKEY_WAIT = MRobotInit2%
1962             MLocalLoopFlg=-1
1963             Break
1964         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
1965             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
1966             fnKEY_WAIT = MRobotInit3%
1967             MLocalLoopFlg=-1
1968             Break
1969         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
1970             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
1971             fnKEY_WAIT = MRobotInit4%
1972             MLocalLoopFlg=-1
1973             Break
1974         Else
1975         EndIf
1976     WEnd
1977     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
1978     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
1979     Exit Function
1980 FEnd
1981 '
1982 '■ fnAUTO_CTL
1983 ''' <summary>
1984 ''' AUTOモードOFF、PLCからの開始待ち
1985 ''' </summary>
1986 ''' <remarks>
1987 ''' Date   : 2021/07/07 : M.Hayakawa
1988 ''' </remarks>
1989 Function M% fnAUTO_CTL
1990     fnAUTO_CTL = 0
1991     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1992     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
1993     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1994     '
1995     If M_Svo=0 Then             'サーボON確認
1996         Servo On
1997     EndIf
1998     Wait M_Svo=1
1999     Exit Function
2000 FEnd
2001 '
2002 '■ fnWindScreenOpen
2003 ''' <summary>
2004 ''' ウィンド画面の表示、非表示設定
2005 ''' </summary>
2006 '''<param name="%"></param>
2007 '''<param name="%"></param>
2008 '''<param name="%"></param>
2009 '''<param name="%"></param>
2010 ''' <remarks>
2011 ''' コメントD1001, D1002, D1003の設定
2012 ''' MWindReSet = 0     画面非表示
2013 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2014 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2015 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2016 ''' Date   : 2021/07/07 : M.Hayakawa
2017 ''' </remarks>
2018 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2019     If MCommentD1001 <> 0 Then              'コメント 0 は設定がないので確認
2020         M_Out16(12480) = MCommentD1001      'D1001 コメント
2021     EndIf
2022     '
2023     If MCommentD1002 <> 0 Then              'コメント 0 は設定がないので確認
2024         M_Out16(12496) = MCommentD1002      'D1002 コメント
2025     EndIf
2026     '
2027     If MCommentD1003 <> 0 Then              'コメント 0 は設定がないので確認
2028        M_Out16(12512) = MCommentD1003       'D1003 コメント
2029     EndIf
2030     '
2031     M_Out16(12448) = MScreenNo              '画面番号  M6448   10=エラー画面
2032     M_Out(12363) = 1 Dly 0.5                'ウィンド画面設定  M6362
2033     Exit Function
2034 FEnd
2035 '
2036 '■FnCtlValue2
2037 ''' <summary>
2038 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2039 ''' </summary>
2040 ''' <param name="MCtlNo%"></param>
2041 ''' <remarks>
2042 ''' Date : 2022/04/28 渡辺
2043 ''' </remarks>
2044 '''
2045 '''  1：投入数       ＋１
2046 '''  2：組立ＯＫ数   ＋１
2047 '''  3：供給機２吸着エラー数 ＋１　　組立NGから変更 2022/05/19 渡辺
2048 '''  4：供給機１吸着エラー数 ＋１
2049 ''' 99：読書開始信号 OFF
2050 '''
2051 Function M% FnCtlValue2(ByVal MCtlNo%)
2052     FnCtlValue2 = 1
2053     Select MCtlNo%
2054         Case 1        '投入数＋１
2055             M_Out(12569) = 0             '書込み開始信号OFF
2056             M_Out(12568) = 1             '読込み開始信号ON
2057             MInputQty = M_In16(11600)    '投入数受信
2058             MInputQty = MInputQty + 1    '投入数＋１
2059             M_Out16(12592) = MInputQty   '投入数送信
2060             M_Out(12569) = 1             '書込み開始信号ON
2061             Break
2062             '
2063         Case 2        '組立ＯＫ数＋１
2064             M_Out(12569) = 0             '書込み開始信号OFF
2065             M_Out(12568) = 1             '読込み開始信号ON
2066             MAssyOkQty = M_In16(11616)   '組立OK数受信
2067             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2068             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2069             M_Out(12569) = 1             '書込み開始信号ON
2070             Break
2071             '
2072         Case 3        '供給機２吸着エラー数＋１
2073             M_Out(12569) = 0                       '書込み開始信号OFF
2074             M_Out(12568) = 1                       '読込み開始信号ON
2075             MSuctionErrQty = M_In16(11632)         '供給機２吸着エラー数受信
2076             MSuctionErrQty = MSuctionErrQty + 1    '供給機２吸着エラー数＋１
2077             M_Out16(12624) = MSuctionErrQty        '供給機２吸着エラー数送信
2078             M_Out(12569) = 1                       '書込み開始信号ON
2079             Break
2080             '
2081         Case 4        '供給機１吸着エラー数＋１
2082             M_Out(12569) = 0                       '書込み開始信号OFF
2083             M_Out(12568) = 1                       '読込み開始信号ON
2084             MSuctionErrQty = M_In16(11648)         '供給機１吸着エラー数受信
2085             MSuctionErrQty = MSuctionErrQty + 1    '供給機１吸着エラー数＋１
2086             M_Out16(12640) = MSuctionErrQty        '供給機１吸着エラー数送信
2087             M_Out(12569) = 1                       '書込み開始信号ON
2088             Break
2089             '
2090         Case 99        '読書開始信号OFF
2091             M_Out(12568) = 0        '読込み開始信号OFF
2092             M_Out(12569) = 0        '書込み開始信号OFF
2093             Break
2094             '
2095     End Select
2096     Exit Function
2097 FEnd
2098 '
2099 '
2100 '■FnScreEroorCord
2101 ''' 電動ドライバーのエラーコードを含めたコメントを出す為のコメント番号の作成
2102 ''' 新規作成：2022/05/23 : 渡辺
2103 '''
2104 Function M% FnScreEroorCord()
2105     MScrewErrorCord% = 0
2106     If M_In(11252) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 1    '11252:E_driver Error Massage1 E1
2107     If M_In(11253) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 2    '11253:E_driver Error Massage1 E2
2108     If M_In(11254) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 4    '11254:E_driver Error Massage1 E3
2109     If M_In(11255) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 8    '11255:E_driver Error Massage1 E4
2110     If M_In(11258) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 16   '11258:E_driver Error Massage1 E5
2111     MScrewErrorCord% = MScrewErrorCord% * 10
2112     MScrewErrorCord% = MScrewErrorCord% + 500
2113     FnScreEroorCord = MScrewErrorCord%
2114     Exit Function
2115 FEnd
2116 '
2117 '
2118 'Insightによる画像処理検査実行（並列処理なし）
2119 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2120 '-------------------------------------------------------------------------------
2121 'Insightによる画像処理検査実行（並列処理なし）
2122 '   引数
2123 '       PInspPos()      ：検査位置
2124 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2125 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2126 '       MInspCnt%       ：検査位置数
2127 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2128 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2129 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2130 '   戻り値：整数
2131 '       0=異常終了、1=正常終了
2132 '
2133 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2134 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2135 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2136 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2137 '   20200410    :   検査グループ設定Retry追加
2138 '-------------------------------------------------------------------------------
2139     '----- 初期設定 -----
2140     Cnt 0                                                           '移動効率化解除(初期値=0)
2141     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2142 '    Cnt 1,0.1,0.1
2143     '変数宣言・初期化
2144     Def Inte MNum                                                   '検査番号(検査順1～)
2145     MNum% = 1                                                       '検査番号初期値設定
2146     Def Inte MEndFlg                                                '検査終了フラグ
2147     MEndFlg% = 0
2148     '
2149     '検査G番号設定要求・検査実行要求off
2150     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2151     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2152     'エラー番号クリア
2153     MInspErrNum = 0                                                 '検査実行エラー番号
2154     M_Out16(MOUT_InspErrNum) = MInspErrNum
2155     MInspNGStepNum = 0                                              '検査実行NGStep番号
2156     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2157     '
2158     'Insight Ready check?
2159     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2160         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2161         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2162         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2163         ISInspectionSingle = 0                                      '異常終了戻り値設定
2164         'Exit Function
2165     EndIf
2166     If MInspErrNum = 20 Then GoTo *ISInspectionSingle_End
2167     '
2168     '検査位置数確認
2169     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2170         MInspErrNum = 21                                            '検査データなし 21　引数<1
2171         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2172         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2173         ISInspectionSingle = 0                                      '異常終了戻り値設定
2174         'Exit Function
2175     EndIf
2176    If MInspErrNum = 21 Then GoTo *ISInspectionSingle_End
2177     '
2178     '
2179     '
2180     '----- メイン処理 -----
2181     '設定された検査位置数分の検査実行
2182     While( MEndFlg% = 0 )
2183         '----- 検査グループ番号設定Retry追加 20200410
2184         MSetGrNumRetryExitFlg = 0
2185         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2186         While( MSetGrNumRetryExitFlg = 0 )
2187         '----- 検査グループ番号設定Retry追加ここまで 20200410
2188             '
2189             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2190             '
2191             '----- 検査グループ番号設定 -----
2192             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2193             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2194             '
2195             '検査位置へ移動・移動完了待ち
2196             fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2197             Mov PInspPos( MNum% )                                       '移動
2198             fnAutoScreenComment(523)                                    '状態表示[画像処理検査中] 2022/05/09 渡辺
2199             Dly 0.2                                                     '移動完了後Delay 0.05>>0.2
2200             '
2201             '検査グループ番号設定終了確認
2202             M_Timer(1) = 0
2203             MExitFlg = 0
2204             While( MExitFlg = 0 )
2205                 '検査G設定正常終了?
2206                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2207                     MExitFlg = 1
2208                 '
2209                 '検査G設定異常終了?
2210                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2211                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2212                     If MInspErrNum = 0 Then                             '1回目のエラー?
2213                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2214                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2215                     EndIf
2216                     MExitFlg = 1
2217                 '
2218                 'timeoutチェック
2219                 ElseIf 1000 < M_Timer(1) Then
2220                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2221                     If MInspErrNum = 0 Then                             '1回目のエラー?
2222                         MInspErrNum = 12                                'timeout エラー番号=12
2223                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2224                     EndIf
2225                     MExitFlg = 1
2226                 EndIf
2227             WEnd
2228             '
2229             '検査G番号設定要求off
2230             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2231             '
2232             '----- 検査グループ設定Retry追加 20200410
2233             'NGなければ抜ける
2234             If MCurrentStepErr = 0 Then
2235                 MSetGrNumRetryExitFlg = 1
2236             Else
2237                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2238                 If MSetGrNumRetryCnt = 0 Then
2239                     MSetGrNumRetryExitFlg = 1
2240                 Else
2241                     'Retryへ　その前にDelay
2242                     Dly 0.5
2243                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2244                 EndIf
2245             EndIf
2246             '----- 検査グループ設定Retry追加ここまで 20200410
2247             '
2248         WEnd
2249         '
2250         '
2251         '
2252         '----- 検査実行 -----
2253         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2254             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2255                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2256                 MInspRetryExitFlg = 0
2257                 MRetryCnt = 2                                        'Retry回数設定
2258                 While( MInspRetryExitFlg = 0 )
2259                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2260                     '
2261                     '検査完了確認
2262                     MRetryCnt = MRetryCnt - 1
2263                     M_Timer(1) = 0
2264                     MExitFlg = 0
2265                     While( MExitFlg = 0 )
2266                     '検査完了待ち
2267                         '検査OK終了?
2268                         If M_In( MIN_IS_InspOK% ) = 1  Then
2269                             MJudgeOKFlg = 1                         '検査OKフラグON
2270                             MExitFlg = 1
2271                         '
2272                         '検査NG終了?
2273                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2274                             If MInspErrNum = 0 Then                 '1回目のエラー?
2275                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2276                                     MInspErrNum = 32                    '検査NG エラー番号=32
2277                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2278                                 EndIf
2279                             EndIf
2280                             MExitFlg = 1
2281                         '
2282                         '検査異常終了(IS timeout)?
2283                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2284                             If MInspErrNum = 0 Then                 '1回目のエラー?
2285                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2286                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2287                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2288                                 EndIf
2289                             EndIf
2290                             MExitFlg = 1
2291                         '
2292                         'timeoutチェック
2293                         ElseIf 3000 < M_Timer(1) Then
2294                             If MInspErrNum = 0 Then                 '1回目のエラー?
2295                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2296                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2297                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2298                                 EndIf
2299                             EndIf
2300                             MExitFlg = 1
2301                         EndIf
2302                     WEnd
2303                     '
2304                     '検査開始要求off
2305                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2306                     '
2307                     'OKなら抜ける
2308                     If MJudgeOKFlg = 1 Then
2309                         MInspRetryExitFlg = 1
2310                     Else
2311                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2312                         If MRetryCnt = 0 Then
2313                             MInspRetryExitFlg = 1
2314                         Else
2315                             'Retryへ　その前にDelay
2316                             Dly 0.3
2317                         EndIf
2318                     EndIf
2319                     '
2320                 WEnd
2321             EndIf
2322         EndIf
2323         '
2324         '
2325         '
2326         MNum% = MNum% + 1                                           '検査Step+1
2327         '検査終了確認　検査終了フラグセット
2328         If (MInspCnt% < MNum% ) Then
2329             MEndFlg% = 1                                            '検査終了フラグセット
2330         EndIf
2331         'NG発生時続行時処理
2332         If MInspErrNum <> 0 Then                                    'NGあり?
2333             If MNgContinue% <> 1 Then                               'NG続行?
2334                 MEndFlg% = 1                                        '検査終了フラグセット
2335             EndIf
2336         EndIf
2337     WEnd
2338     '
2339     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2340     If 0 < MZAxis% Then
2341         PCurrentPos = P_Curr                                        '現在位置取得
2342         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2343         fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2344         Mvs PCurrentPos                                             '現在位置上空へ移動
2345     EndIf
2346     '
2347     '戻り値設定
2348     If MInspErrNum = 0 Then
2349         ISInspectionSingle = 1                                      '正常終了戻り値設定
2350     Else
2351         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2352         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2353         ISInspectionSingle = 0                                      '異常終了戻り値設定
2354     EndIf
2355 '
2356 *ISInspectionSingle_End
2357 Fine 0 , P
2358     Exit Function
2359 FEnd
2360 '
2361 '■fnAutoScreenComment
2362 ''' <summary>
2363 ''' メイン画面の動作状況表示
2364 ''' コメントD1005の設定
2365 ''' </summary>
2366 '''<param name="McommentD1005%">コメントID</param>
2367 ''' <remarks>
2368 ''' Date   : 2021/07/07 : M.Hayakawa
2369 ''' </remarks>
2370 Function fnAutoScreenComment(ByVal McommentD1005%)
2371     M_Out16(12576) = McommentD1005%
2372     Exit Function
2373 FEnd
2374 '
2375 '■fnRoboPosChk
2376 ''' <summary>
2377 ''' 最後に終了したロボットポジションの確認
2378 ''' </summary>
2379 '''<param name="MINNumber%">入力番号</param>
2380 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2381 '''<param name="MTimeCnt&">タイムアウト時間</param>
2382 ''' PLCに保続した番号を読込み、確認
2383 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2384 '''<returns>整数 0:タイムアウト 1:OK</returns>
2385 ''' <remarks>
2386 ''' Date   : 2021/07/07 : M.Hayakawa
2387 ''' </remarks>
2388 Function M% fnRoboPosChk
2389     fnRoboPosChk = 0
2390     MRet = fnStepRead()
2391     '初期位置でないと判断した場合
2392     'ウィンド画面切換え
2393     If MRBTOpeGroupNo > 5 Then
2394         '下記キー待ちの継続に反応させないため
2395         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2396         Dly 0.2
2397         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2398         Dly 1.5
2399         '
2400         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2401         '
2402         MLoopFlg% = 1
2403         While MLoopFlg% = 1
2404             '
2405             '
2406             MKeyNumber% = fnKEY_WAIT()
2407             Select MKeyNumber%
2408                 Case Is = MAbout%       '停止
2409                     M_20# = MAbout%
2410                     MLoopFlg% = -1
2411                     Break
2412                 Case Is = MNext%        '次へ
2413                     'MLoopFlg% = -1
2414                     Break
2415                 Case Is = MContinue%    '継続
2416                     M_20# = MContinue%
2417                     MLoopFlg% = -1
2418                     Break
2419                 Default
2420                     Break
2421             End Select
2422         WEnd
2423     EndIf
2424     '
2425     If M_20# = MContinue% Then                              '継続ボタンが押された場合
2426         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
2427         Ovrd 5                                   '低速オーバーライド値設定
2428         Select MRBTOpeGroupNo
2429             Case Is = 5                          '何もしない
2430                 Break
2431             Case Is = 10                         '初期位置へ戻す
2432                 'Mov PTEST001
2433                 Break
2434             Case Is = 15                         '初期位置へ戻す
2435                 'Mov PTEST002
2436                 Dly 0.5
2437                 'Mov PTEST001
2438                 Dly 0.5
2439                 Break
2440             Default
2441                 Break
2442         End Select
2443         '
2444         Ovrd M_NOvrd                            'システムの初期値を設定
2445         M_Out(12364) = 1                        'toPLC_データ保存ON
2446         MRBTOpeGroupNo = 5
2447         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
2448         Dly 1.0
2449         M_Out(12364) = 0                        'toPLC_データ保存OFF
2450         fnRoboPosChk = 1                        '初期位置動作実行
2451         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
2452     EndIf
2453     Exit Function
2454 FEnd
2455 '
2456 '■frInCheck
2457 ''' <summary>
2458 ''' センサーINチェック
2459 ''' </summary>
2460 '''<param name="MINNumber%">入力番号</param>
2461 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2462 '''<param name="MTimeCnt&">タイムアウト時間</param>
2463 '''<returns>整数 0:タイムアウト 1:OK</returns>
2464 ''' <remarks>
2465 ''' Date   : 2021/07/07 : M.Hayakawa
2466 ''' </remarks>
2467 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2468     M_Timer(4) = 0
2469     MloopFlg = 0
2470     While MloopFlg = 0
2471         MCrtTime& = M_Timer(4)
2472         If M_In(MINNumber%) = MCMPFLG% Then
2473             MloopFlg = 1
2474             frInCheck = 1
2475         ElseIf MCrtTime& > MTimeCnt& Then
2476             MloopFlg = 1
2477             frInCheck = 0
2478         EndIf
2479     WEnd
2480     Exit Function
2481 FEnd
2482 '-----------------------------------------------
2483 '
2484 'ねじ締め機通信確認
2485 '
2486 '-----------------------------------------------
2487 Function M% fScewTcomChk
2488     fScewTcomChk = 0
2489     '通信確認送信
2490     M_Out(MOUT_ScwT_ComChk%) = MOn%
2491     '通信確認受信待機
2492     Wait M_In(MIN_ScwT_comOK%) = MOn%
2493     '通信確認送信終了
2494     M_Out(MOUT_ScwT_ComChk%) = MOff%
2495     Exit Function
2496 FEnd
2497 '
2498 '
2499 '-----------------------------------------------
2500 '
2501 'ねじ締め開始送信
2502 '
2503 '-----------------------------------------------
2504 Function M% fScewTStart
2505     fScewTStart = 0
2506     'ねじ締め開始待機を受信
2507     Wait M_In(MIN_ScwT_STRec%) = MOn%
2508     Dly 0.1
2509     'ねじ締め開始受信を送信
2510     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msecパルス
2511     Exit Function
2512 FEnd
2513 '
2514 '
2515 '-----------------------------------------------
2516 '
2517 'ねじ締め完了受信
2518 '
2519 '-----------------------------------------------
2520 Function M% fScewTFinish
2521     fScewTFinish = 0
2522     'ねじ締め完了待機を受信
2523     Wait M_In(MIN_ScwT_Fin%) = MOn%
2524     Dly 0.1
2525     'ねじ締め完了受信を送信
2526     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msecパルス
2527     Exit Function
2528 FEnd
2529 '
2530 '
2531 '-----------------------------------------------
2532 '
2533 '条件xx停止受信
2534 '
2535 '-----------------------------------------------
2536 Function M% fScewTCaseStop(ByVal MCase%())
2537     fScewTCaseStop = 0
2538     '条件xx停止を受信
2539     Wait M_In(MCase%(1)) = MOn%
2540     Dly 0.1
2541     '条件xx停止受信を送信
2542     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msecパルス
2543     Exit Function
2544 FEnd
2545 '
2546 '-----------------------------------------------
2547 '
2548 '再開始受信
2549 '
2550 '-----------------------------------------------
2551 Function M% fScewTReStart()
2552     fScewTReStart = 0
2553     '再開始を受信
2554     Wait M_In(MIN_ScwT_ReST%) = MOn%
2555     Dly 0.1
2556     '再開始受信を送信
2557     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msecパルス
2558     Exit Function
2559 FEnd
2560 '
2561 '■fErrorProcess
2562 '<summary>
2563 'エラー処理
2564 '</summary>
2565 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
2566 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
2567 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
2568 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
2569 '<make>
2570 '2021/11/5 中村天哉
2571 '</make>
2572 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2573     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
2574     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
2575     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
2576     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
2577 *RETRY_ERR_PROCESS
2578      M_20# = MClear%     '初期化
2579 '        'エラー処理記述
2580         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2581 '        'GOT KEY入力待ち
2582         MKeyNumber = fnKEY_WAIT()
2583 '        '
2584         If MKeyNumber = MAbout% Then   '停止を選択した場合
2585             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2586  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2587             Break
2588          '
2589         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
2590             M_20# = MContinue%            'M_20# プログラム間共通外部変数
2591  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2592         '
2593         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
2594             M_20# = MNext%            'M_20# プログラム間共通外部変数
2595  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2596          '
2597         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
2598             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
2599  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2600             Break
2601         '
2602         EndIf
2603         '
2604         '
2605         '
2606         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2607         fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2608     Exit Function
2609 FEnd
2610 '
2611 '■fnInitialZone
2612 ''' <summary>
2613 ''' 現在位置から上空に待避し、初期位置に戻る
2614 ''' </summary>
2615 ''' <remarks>
2616 ''' Date : 2021/12/2 : M.Hayakawa
2617 ''' Update:2022/06/2 : M.Hayakawa 他工程の非常停止復帰に合わせて変更
2618 ''' </remarks>
2619 Function fnInitialZone()
2620     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中]
2621 '
2622     Ovrd 5
2623 ' 上空退避
2624     PActive = P_Curr
2625     Pmove = PActive
2626 '
2627     If PActive.X > 580 Then
2628         Pmove.Z =380        'パレット上に腕を伸ばしているときは500まで上げられない為、例外処置
2629     Else
2630         Pmove.Z =500        '上記以外はZ:500まで持ち上げ
2631     EndIf
2632 '
2633     Mvs Pmove
2634     Mov PInitialPosition
2635 ' ロックを開放
2636     InitialState()
2637 ' 一旦停止
2638     fErrorProcess(20,70,256,0)
2639     Exit Function
2640 FEnd
2641 '
2642 '■InitialState
2643 ''' <summary>
2644 ''' ハンド、治具を初期位置にする
2645 ''' </summary>
2646 ''' <returns>   0 : OK
2647 '''             1 : NG
2648 ''' </returns>
2649 ''' <remarks>
2650 ''' Date : 2021/12/2 : M.Hayakawa
2651 ''' </remarks>
2652 Function M% InitialState()
2653     InitialState = 0
2654     '製品位置決め解除
2655     M_Out(12261)=1 Dly 0.3      'FANクランプ解除ON
2656     'Wait M_In(11271)=1          'FANクランプ出端検出(修正につきコメントアウト(8/26中村))
2657     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'FANクランプ出端検出(8/26中村)
2658     If MRtn = 0 Then
2659         fErrorProcess(11,234,284,0)
2660         Select M_20#
2661             Case MAbout%            '停止か
2662                 InitialState = 1
2663                 Break
2664             Case MNgProcess%        'NGが押された場合
2665                 InitialState = 0
2666                 Break
2667             Case MContinue%
2668                 M_20# = MClear%
2669                 InitialState = 0
2670                 Break
2671             Case MNext%
2672                 M_20# = MClear%
2673                 InitialState = 0
2674                 Break
2675         End Select
2676     EndIf
2677     '
2678     M_Out(12259)=1 Dly 0.3      'プッシュCY用SV戻端パルス出力
2679     'Wait M_In(11269)=1          'プッシュ戻端検出(修正につきコメントアウト(8/26中村))
2680     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)    'プッシュ戻端検出
2681     If MRtn = 0 Then
2682         fErrorProcess(11,234,284,0)
2683         Select M_20#
2684             Case MAbout%            '停止か
2685                 InitialState = 1
2686                 Break
2687             Case MNgProcess%        'NGが押された場合
2688                 InitialState = 1
2689                 Break
2690             Case MContinue%
2691                 M_20# = MClear%
2692                 InitialState = 0
2693                 Break
2694             Case MNext%
2695                 M_20# = MClear%
2696                 InitialState = 0
2697                 Break
2698         End Select
2699     EndIf
2700     '
2701     M_Out(12257)=1 Dly 0.3      '位置決めCY用SV戻端パルス出力
2702     'Wait M_In(11267)=1          '位置決め戻端検出(修正につきコメントアウト(8/26中村))
2703     MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '位置決め戻端検出(8/26中村)
2704     If MRtn = 0 Then
2705         fErrorProcess(11,234,284,0)
2706         Select M_20#
2707             Case MAbout%            '停止か
2708                 InitialState = 1
2709                 Break
2710             Case MNgProcess%        'NGが押された場合
2711                 InitialState = 1
2712                 Break
2713             Case MContinue%
2714                 M_20# = MClear%
2715                 InitialState = 0
2716                 Break
2717             Case MNext%
2718                 M_20# = MClear%
2719                 InitialState = 0
2720                 Break
2721         End Select
2722     EndIf
2723     Exit Function
2724 FEnd
2725 '
2726 '■fnTorqueCheck
2727 ''' <summary>
2728 ''' トルクチェック動作用のメイン
2729 ''' </summary>
2730 ''' <remarks>
2731 ''' Date   : 2021/12/21 : H.AJI
2732 ''' </remarks>'
2733 Function M% fnTorqueCheck
2734     'トルクチェック中送信  搬送系停止
2735     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
2736     '
2737     fnTorqueCheck = 0
2738     Ovrd 20
2739     Mov PInitialPosition              '初期位置移動
2740     Ovrd 100
2741     '下記キー待ちの継続に反応させないため
2742     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2743     Dly 0.2
2744     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2745     '
2746     'M6340  トルクチェック受信
2747     M_Out(12340) = 1 Dly 1.0                'トルクチェック受信 M6340
2748     Dly 1.0
2749     M_Out(12340) = 0
2750     '
2751     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
2752     '
2753     MLoopFlg = 1
2754     While MLoopFlg = 1
2755         '
2756         Mov PInitialPosition              '初期位置移動
2757         '
2758         MKeyNumber = fnKEY_WAIT()
2759         Select MKeyNumber
2760             Case Is = 1           '停止
2761                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
2762                 Dly 1.0
2763                 M_Out(12343) = 0
2764                 Ovrd 20
2765                 Mov PTicketRead_1
2766                 Ovrd 100
2767                 M_20# = 1
2768                 MLoopFlg = -1
2769                 Break
2770             Case Is = 2           '次へ
2771                 Break
2772             Case Is = 3           '継続
2773                 Break
2774             Case Is = 4           'トルクチェック開始
2775                 M_Out(12545) = 1    ' toPLC_PCトルクチェック1要求受信(M315)
2776                 M_Out(12342) = 1 Dly 1.0    'トルクチェック開始要求受信 M6342
2777                 fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
2778                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
2779                 MRet = fnMoveTorquePosi()
2780                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
2781                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2782                 Break
2783             Default
2784                 Break
2785         End Select
2786     WEnd
2787     '
2788     'トルクチェック中停止送信
2789     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
2790     '
2791     'ロボットの位置を元に戻す
2792     '
2793     Exit Function
2794  FEnd
2795  '
2796 '
2797 '
2798 '---------------------------
2799 '
2800 '    メイン画面の表示、非表示設定
2801 '         コメントD1001, D1002, D1003の設定
2802 '           MWindReSet = 0     画面非表示
2803 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
2804 '           MWindErrScr = 10    エラー画面 D1001, D1002
2805 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2806 '
2807 '---------------------------
2808 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2809     fnMainScreenOpen = 0
2810     '
2811    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2812         M_Out16(12480) = MCommentD1001            'D1001 コメント
2813     EndIf
2814     '
2815     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2816         M_Out16(12496) = MCommentD1002            'D1002 コメント
2817     EndIf
2818     '
2819     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2820         M_Out16(12512) = MCommentD1003            'D1003 コメント
2821     EndIf
2822     '
2823     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2824     M_Out(12362) = 1                         'ウィンド画面設定  M6362
2825     Dly 0.5
2826     M_Out(12362) = 0                         'ウィンド画面設定
2827     Exit Function
2828 FEnd
2829 '
2830 '■Main
2831 ''' <summary>
2832 ''' トルクチェック実動作
2833 ''' </summary>
2834 ''' <remarks>
2835 ''' Date   : 2021/12/21 : H.AJI
2836 ''' </remarks>'
2837 Function M% fnMoveTorquePosi
2838      fnMoveTorquePosi = 0
2839      Ovrd 50
2840      Mov PTorqueCheck_1 'トルクチェックメーター上空へ移動
2841     '
2842     Spd M_NSpd
2843 '-------------      ドライバーRST
2844     M_Out(12240)=0     'ドライバーOFF CCW
2845     M_Out(12241)=0     'ドライバーOFF CW
2846     M_Out(12242)=1     'ドライバー解除 C1
2847     M_Out(12243)=1     'ドライバー解除 C2
2848     M_Out(12245)=0     'プログラム解除 F1/プログラム2
2849 '---------------------------------------
2850 '[P-11]
2851 '--------------------------------------------------------------   【トルクチェック 0.4N - P11】
2852     Mov PTorqueCheck, -50                     ' トルク-1　置き位置上空 50mm へ移動
2853     Dly 0.1
2854 '-----------------------
2855    'Cnt 0                           'Cnt動作-2　終了
2856 '-----------------------
2857     Mov PTorqueCheck , -5                      'トルク-1　置き位置上空 5mm へ移動
2858     Dly 0.2
2859 '-----------------------
2860     ProgramBankSet(1,3)
2861     M_Out(12241)=0                   'ドライバーOFF  CW
2862     'Dly 0.1
2863 '--------------------------------
2864     Ovrd 40
2865    'Dly 0.1
2866 '--------------------------------  ネジ締め速度設定
2867     Spd 14                            'ライド 100-40 100% :Spd 12
2868     Dly 0.1
2869 '--------------------------------
2870 '--------------------------------
2871 '---------------------------------【ねじ締め動作】
2872 '
2873     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '移動中エラー検出
2874    Mvs PTorqueCheck               'トルクチェック位置へ移動
2875     Dly 0.3                          '動作安定待ち
2876    M_Out(12241)=1                   'ドライバーON  CW
2877 '
2878     Wait M_In(11584)=1                '完了/エラー検出
2879     Dly 0.1
2880     Spd M_NSpd
2881    'Ovrd 20
2882     If M_In(11256)=1 Then *LBL1       'ネジトータルエラー検出
2883     Wait M_In(11257)=1                'ネジ完了SC
2884 '---------------------------------
2885     Dly 0.1
2886     M_Out(12241)=0                    'ドライバーOFF CW
2887     Dly 0.1
2888     M_Out(12242)=0                    'ドライバー解除 C1
2889     Dly 0.1
2890     M_Out(12243)=0                    'ドライバー解除 C2 (バンク3)
2891     Dly 0.1
2892     M_Out(12245)=0                    'プログラム2解除 F1
2893 '--------------------------------------------------------------   【トルクチェック 0.4N - P11ここまで】
2894 '
2895     Mvs PTorqueCheck,-60                       'あえてmov から変更
2896     Dly 0.1
2897 '--------------------------------------------------------------
2898    'Ovrd 80
2899 '--------------------------------------------------------------
2900 '---------------------------------------
2901 '---------------------------------------
2902 '---------------------------------------エラー離脱処理
2903    *LBL1
2904    Fsc Off            '力覚センサ　Off   *STEP1は不要
2905    Mvs ,-100
2906    M_Out(12241)=0     'ドライバーOFF CW
2907    Dly 0.1
2908    M_Out(12242)=0     'ドライバー解除 C1
2909    Dly 0.1
2910    M_Out(12243)=0     'ドライバー解除 C2 (バンク3)
2911    Dly 0.1
2912    M_Out(12245)=0     'プログラム解除 F1
2913 '---------------------------------------
2914 '---------------------------------------
2915 '-------------
2916    'Mov PInitPos19049
2917    Dly 0.1
2918 '
2919 '
2920     Exit Function
2921 FEnd
2922 '
2923 '■Main
2924 ''' <summary>
2925 ''' 組立動作用のメイン
2926 ''' </summary>
2927 ''' <remarks>
2928 ''' Date   : 2021/07/07 : M.Hayakawa
2929 ''' </remarks>'
2930 Function Main
2931     MopeNo = M_21#         '外部変数にて動作番号代入
2932     '
2933     If M_Svo=0 Then
2934         Servo On
2935     EndIf
2936     Wait M_Svo=1
2937 '組立スタート日付時刻要求パルスON
2938     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2939 'パトライト操作
2940     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
2941     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
2942     '
2943     M_20# = 0                                   'KEY入力初期化
2944     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
2945     MRet% = 0
2946 '復帰動作　実行・未実行判別      2022/03/22 渡辺 作成
2947     PActive = P_Curr                    '現在位置を取得
2948     MRecoveryPass% = 0
2949     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2950         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2951             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2952             MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
2953         EndIf
2954     EndIf
2955     EndIf
2956     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2957         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2958             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2959                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
2960             EndIf
2961         EndIf
2962     EndIf
2963     If MRecoveryPass% = 0 Then
2964         fnInitialZone()        '復帰動作パスフラグが立っていない時は復帰動作を実行
2965     EndIf
2966 '
2967     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
2968         M_Out(12364) = 1            'toPLC_データ保存ON
2969 'トルクチェック
2970         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2971             MRet% = fnTorqueCheck()
2972             Break
2973         Else
2974 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
2975 '                MRtn = InspInit()               '画像処理初期化処理
2976 '            EndIf
2977             '
2978            M_20# = MClear%                    '初期化
2979 '組立開始
2980             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2981                 fnAssyStart()
2982             Else
2983                 M_20# = MPass%
2984             EndIf
2985 '組立終了日付時刻
2986             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
2987             Wait M_In(11572) = 1            '日付取得完了
2988             Dly 0.1
2989             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
2990             '  KEY入力が何もない場合 OKと判断
2991             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
2992 ' 後工程へフラグ出力
2993             If M_20# <> MAbout% Then
2994                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
2995             ElseIf M_20# = MPass% Then
2996                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
2997             EndIf
2998 'About(停止)以外はOKを出力（パレット降下）
2999             If M_20# <> MAbout% Then
3000                 M_Out(12339) = 1 Dly 0.5    'M6339  toPLC_RBT完了パルス出力
3001             EndIf
3002             M_Out(12346) = 0                ' M6346 toPLC_組立開始受信 OFF
3003 'PIASに組立完了書込み
3004             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3005                 If M_20# = MPass% Then
3006                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3007                 Else
3008                     'KEY入力がNGの場合
3009                     If M_20# = MNgProcess% Then
3010 '                        M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3011                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3012                         MRet% = fnPiasWrite(MNG%)
3013                        nAssyNgQty = nAssyNgQty + 1
3014                     EndIf
3015                     '
3016                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3017                     If M_20# = MAssyOK% Then
3018                             '-----------------------
3019                             'D732 -> D2600 コピー要求
3020                             M_Out(12566) = 1
3021 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3022                             M_Out(12566) = 0
3023                             '
3024                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3025                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3026                             '基板番号照合(PPは未使用）
3027 '                            MRet% = fnPCBNumberCheck()
3028                         Else
3029                             MRet% = 1
3030                         EndIf
3031                         '
3032                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3033                             If M_20# <> MAbout% Then
3034                                 '工程履歴OK書き込み
3035                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3036                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3037                                 MRet% = fnPiasWrite(MOK%)
3038                                 nAssyOkQty = 0
3039                                 nAssyOkQty = nAssyOkQty + 1
3040                             Else
3041                                 nAssyOkQty = nAssyOkQty + 1
3042                             EndIf
3043                         EndIf
3044                     EndIf
3045 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3046 '                    MRet% = fnPiasWrite(MOK%)
3047                 EndIf
3048             Else
3049                 nAssyOkQty = nAssyOkQty + 1
3050             EndIf
3051             '
3052             '組立終了日付時刻解除
3053             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3054             '投入数、組立OK数、組立NG数書込み
3055 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3056             '
3057 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3058 '                '画像処理終了処理
3059 '                MRtn = InspQuit()
3060 '            EndIf
3061         EndIf
3062         M_Out(12364) = 0                          'toPLC_データ保存OFF
3063     EndIf
3064 'パトライト操作
3065     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3066     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3067 'GOT表示
3068     fnAutoScreenComment(93)  'AUTO画面 工程完了
3069 '    M_Out(12339) = 1 Dly 0.5        ' M6339 toPLC_RBT完了パルスON
3070 '    M_Out(12346) = 0        'M6346  toPLC_AssY開始受信 OFF
3071 '
3072 FEnd
3073 End
3074 '
3075 '
3076 'おまじないコメント
3077 '絶対削除するな
3078 '
3079 '
3080 '
3081 '
PInspPosition(1)=(+313.20,-30.00,+430.00,+180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(+348.35,+127.39,+410.00,+180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PInspPosition(3)=(+538.88,+54.12,+420.00,+180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PInspPosition(4)=(+520.00,-46.00,+397.00,-180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PInspPosition(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(11)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(12)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(13)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(14)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(15)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(16)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(17)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(18)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(19)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(20)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(21)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(22)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(23)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(24)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(25)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(26)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(27)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(28)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(29)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(30)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PTemp=(+602.00,-150.75,+450.00,-180.00,-0.02,+90.00,+0.00,+0.00)(7,0)
PScrewPosTemp(1)=(+349.00,+91.40,+370.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPosTemp(2)=(+349.00,+91.40,+313.84,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPosTemp(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(10)=(+349.00,+91.40,+303.84,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(1)=(+233.52,+389.28,+380.00,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(2)=(+166.05,+146.93,+400.00,-180.00,+0.00,+128.05,+0.00,+0.00)(7,0)
PGetScrewPosTemp(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(9)=(+127.52,+411.68,+432.42,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(10)=(+233.52,+389.28,+338.64,+180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PEscapePosi(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PActive=(+602.00,-150.75,+450.00,-180.00,-0.02,+90.00,+0.00,+0.00)(7,0)
Pmove=(+602.00,-150.75,+380.00,+180.00,-0.02,+90.00,+0.00,+0.00)(7,0)
PCalcGetMainScrew=(+0.00,+0.00,-1.20,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PCalcGetFanScrew=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGyroPcbRead=(+315.06,-65.41,+419.00,+160.00,+0.00,+90.00)(7,0)
PGyroPcbRead_1=(+329.62,-57.71,+450.00,-180.00,+0.00,+90.00)(7,0)
PInitialPosition=(+300.00,+0.00,+450.00,-180.00,+0.00,-180.00)(7,0)
PMainPcbRead=(+309.58,-174.17,+408.00,-180.00,+0.00,+180.00)(7,0)
PMainPcbRead_1=(+309.58,-174.17,+450.00,-180.00,+0.00,+180.00)(7,0)
PParts1Check=(+313.20,-30.00,+430.00,+180.00,+0.00,-180.00)(7,0)
PParts1Check_1=(+313.20,-30.00,+480.00,-180.00,+0.00,+180.00)(7,0)
PParts2Check=(+348.35,+127.39,+410.00,+180.00,+0.00,-180.00)(7,0)
PParts2Check_1=(+348.35,+127.39,+450.00,+180.00,+0.00,+180.00)(7,0)
PParts3Check=(+538.88,+54.12,+420.00,+180.00,+0.00,-180.00)(7,0)
PParts3Check_1=(+538.88,+54.12,+460.00,-180.00,+0.00,+180.00)(7,0)
PParts4Check=(+520.00,-46.00,+397.00,-180.00,+0.00,+180.00)(7,0)
PParts4Check_1=(+520.00,-46.00,+460.00,-180.00,+0.00,-180.00)(7,0)
PScrewFan1=(+317.47,+123.55,+303.88,+180.00,+0.00,+90.00)(7,0)
PScrewFan1_0=(+317.47,+123.55,+313.88,+180.00,+0.00,+90.00)(7,0)
PScrewFan1_1=(+317.47,+123.55,+370.00,-180.00,+0.00,+90.00)(7,0)
PScrewFan2=(+349.00,+91.40,+303.84,+180.00,+0.00,+90.00)(7,0)
PScrewFan2_0=(+349.00,+91.40,+313.84,+180.00,+0.00,+90.00)(7,0)
PScrewFan2_1=(+349.00,+91.40,+370.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain1=(+305.10,-26.35,+304.11,-180.00,+0.00,+90.00)(7,0)
PScrewMain1_0=(+305.10,-26.35,+310.11,-180.00,+0.00,+90.00)(7,0)
PScrewMain1_1=(+305.10,-26.35,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain2=(+305.00,-175.00,+304.33,-180.00,+0.00,+90.00)(7,0)
PScrewMain2_0=(+305.00,-175.00,+310.33,+180.00,+0.00,+90.00)(7,0)
PScrewMain2_1=(+305.00,-175.00,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain3=(+366.84,-180.48,+304.36,-180.00,+0.00,+90.00)(7,0)
PScrewMain3_0=(+366.84,-180.48,+310.36,-180.00,+0.00,+90.00)(7,0)
PScrewMain3_1=(+366.84,-180.48,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain4=(+379.32,+24.31,+305.67,-180.00,+0.00,+60.00)(7,0)
PScrewMain4_0=(+379.32,+24.31,+311.67,-180.00,+0.00,+60.00)(7,0)
PScrewMain4_1=(+379.32,+24.31,+380.00,-180.00,+0.00,+60.00)(7,0)
PScrewMain5=(+385.18,-105.04,+323.66,-180.00,+0.00,+90.00)(7,0)
PScrewMain5_0=(+385.18,-105.04,+335.06,-180.00,+0.00,+90.00)(7,0)
PScrewMain5_1=(+385.18,-105.04,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain6=(+325.37,-78.92,+323.77,+180.00,+0.00,+90.00)(7,0)
PScrewMain6_0=(+325.37,-78.92,+334.17,-180.00,+0.00,+90.00)(7,0)
PScrewMain6_1=(+325.37,-78.92,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewSupplyFan=(+233.52,+389.28,+338.64,+180.00,+0.00,+180.00)(7,0)
PScrewSupplyFan_1=(+233.52,+389.28,+380.00,-180.00,+0.00,-180.00)(7,0)
PScrewSupplyFan_2=(+166.05,+146.93,+400.00,-180.00,+0.00,+128.05)(7,0)
PScrewSupplyFan_9=(+127.52,+411.68,+432.42,-180.00,+0.00,-180.00)(7,0)
PScrewSupplyMain=(+103.30,+195.32,+338.34,-180.00,+0.00,+180.00)(7,0)
PScrewSupplyMain_1=(+103.30,+195.32,+380.00,-180.00,+0.00,+180.00)(7,0)
PScrewSupplyMain_2=(+166.05,+146.93,+447.34,-180.00,+0.00,+128.05)(7,0)
PScrewSupplyMain_9=(-3.19,+216.64,+432.44,+180.00,+0.00,-180.00)(7,0)
PTicketRead=(+602.00,-150.75,+373.00,+180.00,-0.02,+90.00)(7,0)
PTicketRead_1=(+602.00,-150.75,+450.00,+180.00,-0.02,+90.00)(7,0)
PTorqueCheck=(+143.66,-242.00,+340.00,-180.00,-0.01,+90.00)(7,0)
PTorqueCheck_1=(+143.66,-242.00,+360.00,-180.00,-0.01,+90.00)(7,0)
