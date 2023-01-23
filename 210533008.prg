1 ' ===================================
2 '
3 '  21053001 STEP5 Assy5プログラム
4 '
5 ' 作成者：M.Hayakawa
6 ' 作成日：2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1から流用
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
42 Def Inte MovrdA                     'ネジ締めOvrd 可変用
43 Def Float MSpdA                     'ネジ締めSpd　可変用
44 Def Pos PTemp                       'ネジ締め上空位置計算用
45 '===== <Insight変数設定> =====
46 MIN_IS_Ready%        =   11380      '【入力IO】Insight準備OK
47 MIN_IS_JobLoadOK%    =   11381      '【入力IO】Insightジョブロード正常終了
48 MIN_IS_JobLoadNG%    =   11382      '【入力IO】Insightジョブロード異常終了
49 MIN_IS_InspGSetOK%   =   11383      '【入力IO】Insight検査グループ番号設定正常終了
50 MIN_IS_InspGSetNG%   =   11384      '【入力IO】Insight検査グループ番号設定異常終了
51 MIN_IS_InspOK%       =   11385      '【入力IO】Insight検査OK
52 MIN_IS_InspNG%       =   11386      '【入力IO】Insight検査NG
53 MIN_IS_InspErr%      =   11387      '【入力IO】Insight検査異常終了
54 MIN_IS_InspCapDone%  =   11388      '【入力IO】Insight検査画像取込完了
55 MIN_IS_ErrNum%       =   11408      '【入力IO】Insight処理エラー番号開始アドレス(16bit)
56 'Output Signal
57 MOUT_IS_JobLoadReq%  =   12370      '【出力IO】Insight JOBロード要求
58 MOUT_IS_InspGSetReq% =   12371      '【出力IO】Insight 検査グループ番号設定要求
59 MOUT_IS_Insp%        =   12372      '【出力IO】Insight 検査実行要求
60 MOUT_IS_JobNum%      =   12384      '【出力IO】Insight JOB番号設定開始アドレス(16bit)
61 MOUT_IS_InspGNum%    =   12400      '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
62 MOUT_InspErrNum%     =   12416      '【出力IO】検査実行エラー番号開始アドレス(16bit)
63 MOUT_InspNGStepNum%  =   12432      '【出力IO】検査実行NGStep番号開始アドレス(16bit)
64 '===== <電ドラ変数定義> =====
65 X20_Driver=11248                    '電ドラステイタス1　Driver Status 1
66 X21_Driver=11249 '電ドラステイタス2  Driver Status 2
67 X22_Driver=11250 '電ドラステイタス3  Driver Status 3
68 X23_Driver=11251 '電ドラステイタス4  Driver Status 4
69 X24_Driver=11252 '電ドラエラーメッセージ1 Driver Error E1
70 X25_Driver=11253 '電ドラエラーメッセージ2 Driver Error E2
71 X26_Driver=11254 '電ドラエラーメッセージ3 Driver Error E3
72 X27_Driver=11255 '電ドラエラーメッセージ4 Driver Error E4
73 X28_Driver=11256 '電ドラトータルエラーシグナル Total Error
74 X29_Driver=11257 '電ドラ終了シグナル Comlete signal
75 X2A_Driver=11258 '電ドラエラーメッセージ5 Driver Error E5
76 '11584   'toRBトルクドライバ-COMP_ERR送信
77 Y60_Driver=12240 '電ドラ半時計回り CCW
78 Y61_Driver=12241 '電ドラ時計回り CW
79 Y62_Driver=12242 'バンクセッティング BANK C1
80 Y63_Driver=12243 'バンクセッティング BANK C2
81 Y64_Driver=12244 'バンクセッティング BANK C3
82 Y65_Driver=12245 'プログラムセッティング PRG SET F1
83 Y66_Driver=12246 'プログラムセッティング PRG SET F2
84 Y67_Driver=12247 'プログラムセッティング PRG SET F3
85 X34_ScrewReady1=11259 'ねじっこ1　Read
86 '===== <電ドラ定数> =====
87 Dim PScrewPos(10)       'ネジ締め用Function引数変数
88 Dim PGetScrewPos(10)    'ねじ供給機からねじを得るFunction引数変数
89 Dim PEscapePosi(10)
90 MLoopCnt% = 0'
91 '===== <ロボット定数> =====
92 '===== <ロボット変数定義> =====
93 MRBTOpeGroupNo = 0      'ロボット動作番号初期化
94 MCommentD1001 = 0
95 MCommentD1002 = 0
96 MCommentD1003 = 0
97 MScreenNo = 0
98 '
99 MCommentTSU = 0
100 MCommentTSD = 0
101 'ウィンド画面番号設定
102 MWindReSet = 0
103 MWindInfoScr = 5
104 MWindErrScr = 10
105 MWindErrScr2 = 11
106 MWindErrScr3 = 13
107 MWindErrScr17 = 17
108 MWindErrScr18 = 18
109 MWindCmmnScr = 20
110 MWindJigRelase19049 = 60
111 MWindJigRelase19050 = 61
112 MWindJigRelase19051 = 62
113 '
114 MClear% = 0        'KEY_のクリア
115 MAbout% = 1        'KEY_停止
116 MNext% = 2         'KEY_次のステップへ移行
117 MContinue% = 3     'KEY_継続 再度同じ動作を行う
118 '
119 Def Inte MNgProcess
120 MNgProcess% = 5      'KEY_NG
121 '
122 MAssyOK% = 6       '組立完了
123 MPass% = 7         '工程パス
124 MPiasNG% = 8       'Pias確認時履歴NG
125 '
126 '初期化用KEY番号   '
127 MRobotInit1% = 11  '初期位置用
128 MRobotInit2% = 12  '初期位置用
129 MRobotInit3% = 13  '初期位置用
130 MRobotInit4% = 14  '初期位置用
131 '
132 MIN_INIT1REQUEST% = 11568 'toRBT_ロボット初期位置1要求
133 MIN_INIT2REQUEST% = 11569 'toRBT_ロボット初期位置2要求
134 MIN_INIT3REQUEST% = 11570 'toRBT_ロボット初期位置3要求
135 MIN_INIT4REQUEST% = 11571 'toRBT_ロボット初期位置4要求
136 '
137 MOUT_INIT1RECIVE% = 12560 'toPLC_ロボット初期位置1受信
138 MOUT_INIT2RECIVE% = 12561 'toPLC_ロボット初期位置2受信
139 MOUT_INIT3RECIVE% = 12562 'toPLC_ロボット初期位置3受信
140 MOUT_INIT4RECIVE% = 12563 'toPLC_ロボット初期位置4受信
141 '
142 MOK% = 1               '各判定用
143 MNG% = 0               '各判定用
144 MTIMEOUT% = -1         '各判定用
145 MJudge% = 0            '判定情報格納用
146 '
147 MRECIVETIME& = 0
148 MSETTIMEOUT10& = 10000&                '10秒設定
149 MSETTIMEOUT03& = 3000&                 '3秒設定
150 MSETTIMEOUT01& = 1000&                 '1秒設定
151 MSETTIMEOUT05& = 5000&                 '5秒設定
152 MSETTIMEOUT009& = 900&                 '0.9秒設定
153 MSETTIMEOUT008& = 800&                 '0.8秒設定
154 MSETTIMEOUT007& = 700&                 '0.7秒設定
155 MSETTIMEOUT006& = 600&                 '0.6秒設定
156 MSETTIMEOUT005& = 500&                 '0.5秒設定
157 MSETTIMEOUT004& = 400&                 '0.4秒設定
158 MSETTIMEOUT003& = 300&                 '0.3秒設定
159 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
160 MIN_PIAS_ComOK% = 11552                'PC通信OK
161 MIN_PIAS_ComTimeOut% = 11576           'PC通信確認タイムアウト
162 MIN_PIAS_ComNG% = 11553                'PC通信NG
163 MOUT_PIAS_ComCheck% = 12544            'PC通信確認要求
164 MOUT_PIAS_Missing_Process% = 12546     '工程抜け確認要求
165 MIN_PIAS_ModelTypeNG% = 11554          'モデル仕向NG
166 MIN_PIAS_ProcessHistryNG% = 11555      '前工程履歴NG
167 MIN_PIAS_ProcessHistryOK% = 11556      '前工程履歴OK
168 MIN_PIAS_ProcessHistryErr% = 11557     '工程履歴処理エラー
169 MIN_PIAS_MyProcessComp% = 11573        '自工程履歴あり
170 MIN_PIAS_ProcessHistryTimeOut% = 11578 '工程履歴タイムアウト
171 MOUT_OKNG% = 12549                     'PLC OUT でOK=1, NG=0 出力
172 '
173 MOUT_PiasPCBNumberCheck = 12557        '基板番号照合
174 MIN_PiasPCBNumberOK% = 11566          '基板番号OK
175 MIN_PiasPCBNumberNG% = 11565          '基板番号NG
176 MIN_PiasPCBNumberErr% = 11567         '基板番号処理エラー
177 '
178 MOUT_PiasAssyResultOK% = 12549    '組立OK
179 MOUT_PiasAssyResultNG% = 12550    '組立NG
180 MOUT_PiasAssyResultWr% = 12548    '工程履歴書き込み
181 '
182 MIN_PiasProcessNG% = 11559        '工程履歴処理NG
183 MIN_PiasProcessOtherErr% = 11560  '工程履歴処理エラー(なんかのトラブル)
184 MIN_PiasProcessOK% = 11558        '工程履歴処理OK
185 '
186 MIN_Insight_Use% = 11369               '画像確認ON
187 MIN_TorqueCheck% = 11348               'トルクチェック
188 '
189 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT操作権
190 MOUT_RED_LIGHT% = 12356            'PATLIGHT 赤 点灯
191 MOUT_RED_FLASH% = 12357            'PATLIGHT 赤 点滅
192 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT 黄 点灯
193 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT 黄 点滅
194 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT 青 点灯
195 MOUT_GREEN_FLASH% = 12361          'PATLIGHT 青 点滅
196 '
197 MOUT_ST_DATETIME% = 12551          '組立開始日付時刻
198 MOUT_ED_DATETIME% = 12552          '組立終了日付時刻
199 '
200 MOUT_TORQUE_CHECK% = 12367         'PLCへトルクチェック中を送信
201 '
202 MIN_ASSY_CANCEL% = 11366           '組立を行うかのフラグ
203 '
204 MLoopFlg% = 0                      'KEY入力後のOK or NG内容
205 MopeNo% = 0
206 MRtn% = 0
207 MRet = 0
208 MRet3% = 0
209 '
210 Def Inte MInputQty          '投入数 演算変数
211 Def Inte MAssyOkQty         '組立ＯＫ数 演算変数
212 Def Inte MAssyNgQty         '組立ＮＧ数 演算変数(未使用)
213 Def Inte MSuctionErrQty     '吸着エラー数 2022/04/27 渡辺
214 Def Inte MScrewNo
215 Def Inte MReTry
216 '===== <IO変数定義> =====
217 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
218 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
219 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
220 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
221 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
222 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
223 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
224 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
225 '
226 Def Inte Y6A_VV1            ' アーム先端　ネジ吸着バルブ
227 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
228 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
229 '
230 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
231 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
232 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
233 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
234 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
235 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
236 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
237 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
238 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
239 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
240 '
241 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
242 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
243 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
244 '
245 Def Inte MOUT_LED1          ' 画像処理用LED照明
246 '
247 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
248 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
249 '
250 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
251 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
252 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
253 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
254 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
255 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
256 '
257 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
258 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
259 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
260 '
261 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
262 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
263 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
264 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
265 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
266 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
267 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
268 Y6A_VV1%    =  12250    ' アーム先端　ネジ吸着バルブ
269 Y6B_VB1%    =  12251    'アーム先端　吸着破壊バルブ
270 MOUT_VB1%   =  12251    ' アーム先端　ネジ吸着破壊バルブ
271 '
272 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
273 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
274 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
275 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
276 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
277 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
278 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
279 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
280 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
281 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
282 '
283 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
284 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
285 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
286 '
287 MOUT_LED1%  =  12239    ' 画像処理用LED照明
288 '
289 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
290 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
291 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
292 '
293 '共通
294 Def Inte MTEST_KEY                      'デバックテスト用
295 Def Inte MOn                            '出力=1
296 Def Inte MOff                           '出力=0
297 '
298 'ねじ締め装置_出力アドレス
299 Def Inte MOUT_ScwT_ComChk               '通信確認
300 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
301 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
302 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
303 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
304 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
305 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
306 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
307 'ねじ締め装置_入力アドレス
308 Def Inte MIN_ScwT_comOK                 '通信確認返信
309 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
310 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
311 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
312 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
313 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
314 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
315 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
316 '
317 Dim MScwT_Case1%(2)               '条件1停止変数
318 Dim MScwT_Case2%(2)               '条件2停止変数
319 Dim MScwT_Case3%(2)               '条件3停止変数
320 Dim MScwT_Case4%(2)               '条件4停止変数
321 Dim MScwT_Case5%(2)               '条件5停止変数
322 '
323 '共通
324 MTEST_KEY% = 11359                       'デバッグ用テストKEY
325 MOn% = 1                                 '出力 = 1
326 MOff% = 0                                '出力 = 0
327 '
328 'ねじ締め機_アドレス設定
329 MOUT_ScwT_ComChk% = 12832               '通信確認送信
330 MOUT_ScwT_ST% = 12865                   'ねじ締め開始を送信
331 MOUT_ScwT_ReSTOK% = 12866               '再開始受信を送信
332 MOUT_ScwT_FinOK% = 12868                'ねじ締め完了受信を送信
333 MOUT_ScwT_Case1OK% = 12874              '条件1停止受信を送信
334 MOUT_ScwT_Case2OK% = 12875              '条件2停止受信を送信
335 MOUT_ScwT_Case3OK% = 12876              '条件3停止受信を送信
336 MOUT_ScwT_Case4OK% = 12877              '条件4停止受信を送信
337 MOUT_ScwT_Case5OK% = 12878              '条件5停止受信を送信
338 '
339 MIN_ScwT_comOK% = 11840                 'ねじ締め装置から返信
340 MIN_ScwT_STRec% = 11873                 'ねじ締め開始を受信
341 MIN_ScwT_ReST% = 11874                  '再開始を受信
342 MIN_ScwT_Fin% = 11876                   'ねじ締め完了を受信
343 MIN_ScwT_Case1% = 11882                 '条件1停止待機を受信
344 MIN_ScwT_Case2% = 11883                 '条件2停止待機を受信
345 MIN_ScwT_Case3% = 11884                 '条件3停止待機を受信
346 MIN_ScwT_Case4% = 11885                 '条件4停止待機を受信
347 MIN_ScwT_Case5% = 11886                 '条件5停止待機を受信
348 '
349 MScwT_Case1%(1) = MIN_ScwT_Case1%
350 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
351 MScwT_Case2%(1) = MIN_ScwT_Case2%
352 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
353 MScwT_Case3%(1) = MIN_ScwT_Case3%
354 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
355 MScwT_Case4%(1) = MIN_ScwT_Case4%
356 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
357 MScwT_Case5%(1) = MIN_ScwT_Case5%
358 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
359 '
360 '設定 InitialZoneBで使用する変数
361 Def Pos PActive       '直交座標系 位置変数 現在位置
362 Def Pos Pmove         '直交座標系 位置変数 移動先
363 Def Jnt JActive       '関節座標系 位置変数 現在位置
364 Def Jnt Jmove         '関節座標系 位置変数 移動先
365 Def Jnt JTaihi        '関節座標系 位置変数 退避ポジション ティーチングで設定
366 Def Inte MRecoveryPass      '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行
367 Def Inte MJ6          'J6軸の値を比較する為の変数
368 Def Inte MStandby              '待機位置確認フラグ
369 Def Inte MRecoveryChuckOpen    'チャック解放フラグ（復帰動作前）両掴み対策
370 '★注意★初期位置を変更した時には、変更が必要！
371 '
372 '
373 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
374 Function M% fnAssyStart
375     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
376 '
377 '   BaseUnit6通信確認
378     *RE_COM_CHECK
379     M_20# = MClear%
380     MRtn = 1    '初期化
381     If M_In(11920) = 0 Then     'BaseUnit6が拾得中のフラグを立てていない
382         If M_In(11930) = 0 And M_In(11931) = 0 Then   '通信にて回転角不明
383             Dly 2.3                                   '回転待ち
384             If M_In(11930) = 0 And M_In(11931) = 0 Then  'もう一度確認
385                 MRtn = 0                              '通信にて異常
386                 Break
387             EndIf
388             Break
389         EndIf
390         Break
391     EndIf
392     If MRtn = 1 Then GoTo *BU6Com_OK    '通信OKならラベルジャンプ
393     fErrorProcess(11,298,284,0)           '0,284→298,287に変更6/2中村
394     If M_20# = MNext% Then GoTo *RE_COM_CHECK
395     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
396     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
397     If M_20# = MContinue% Then GoTo *RE_COM_CHECK
398     *BU6Com_OK
399 '
400 '
401 ' PIASチケット読込み工程抜け確認
402     M_20# = MClear%                       '初期化
403 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
404 '        MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
405 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
406 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
407 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' メニューへ戻る
408 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NGを工程履歴に書込み次の工程へ
409 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NGを工程履歴に書込み次の工程へ
410 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' 履歴NG, 工程抜け
411 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK以外は組立終了
412 '    EndIf
413 '    '
414 '    '座標移動
415 '    '
416 '    '条件xx停止
417 '    fScewTCaseStop(MScwT_Case5%)
418 '    '
419 '    'ベースユニットKEY
420 '    Wait M_In(MTEST_KEY%) = MOn%
421 '    '
422 '    '再開始
423 '    fScewTReStart()
424 '    '
425 '    '座標移動
426 '    '
427 '    'ねじ締め完了
428 '    Mret% = fScewTFinish()
429 ' ネジ締めテスト終了
430 ' PIASテスト -----------
431 '    MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
432 '    MRet% = fnPiasWrite(MNG%)
433  '   MRet% = fnPCBNumberCheck()
434 ' PIASテスト終了 -------
435 '
436     '組立開始(9/6追加(中村))
437     'プログラム原点
438     Ovrd 100
439     ' ハンド状態初期化(10/29追加M.H)(2/11修正(中村))
440     Cmp Off                     'コンプライアンスモード終了
441     ColChk On                   '衝突検知ON
442     If M_In(11266) Then
443         M_Out(12256) = 0
444         M_Out(12257) = 1
445     EndIf
446     If M_In(11269) Then
447         M_Out(12258) = 0
448         M_Out(12259) = 1
449     EndIf
450     If M_In(11271) Then
451         M_Out(12260) = 0
452         M_Out(12261) = 1
453     EndIf
454     *WAIT_HAND_INI
455     If M_In(11265) = 1 And M_In(11268) = 1 And M_In(11270) = 1 Then GoTo *CompHandIni Else GoTo *WAIT_HAND_INI
456     *CompHandIni
457     M_Out(12257) = 0
458     M_Out(12259) = 0
459     M_Out(12261) = 0
460 '
461 '
462 'Dly 5                                  'デバッグ用(22/09/30中村)
463     ' ねじ締め機テスト用 ----------
464      Mret% = fScrewTcomChk()
465     If Mret% = -1 Then GoTo *ASSY_ERROR_END
466     ' ねじ締め機通信開始
467 '    fScrewTStart()           '処理位置変更2/27中村
468     'チケットIDを読む
469     M_Out(12262) = 1 Dly 0.5 '本体位置決め出端ON
470     PTemp = P_Curr
471     MRtn = 0
472 '    If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
473 '        If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
474 '            If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
475 '                MRtn = 1
476 '            EndIf
477 '        EndIf
478 '    EndIf
479 '    If MRtn = 1 Then
480 '        Mov PTicketRead
481 '    Else
482 '        Cnt 1 , 10 , 10
483 '        Mov PInitialPosition
484 '        Mov PTicketRead_1           'チケットID読み取り回避点
485 '        Cnt 0
486 '        Mvs PTicketRead             'ID読み位置
487 '    EndIf
488 '
489 ' 2022/04/12 安全方向へ条件変更 渡辺
490 ' PInitialPosition 在席 MStandby=2
491 ' PTicketRead_1 在席 MStandby=1
492 '
493     MStandby = 0    '待機位置フラグを初期化
494     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
495         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
496             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
497                 MStandby = 2
498             EndIf
499         EndIf
500     EndIf
501     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
502         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
503             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
504                 MStandby = 1
505             EndIf
506         EndIf
507     EndIf
508     If MStandby = 2 Then
509         Mov PTicketRead_1           'チケットID読み取り回避点
510         Cnt 0
511     EndIf
512     If MStandby <> 0 Then GoTo *PositionOK
513     fErrorProcess(11,230,281,0)            '初期位置にいない時はエラーにする
514     If M_20# = MNext% Then GoTo *ASSY_ERROR_END
515     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
516     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
517     If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
518     *PositionOK
519 '
520     Mvs PTicketRead             'ID読み位置
521 '
522     M_Out(12259) = 0            'DVDメカチャック開OfF
523     M_Out(12258) = 1            'DVDメカチャック閉ON
524 '
525     '
526     MRtn = 1        'MRtn初期化
527 *RE_TICKET_READ
528 '    MRtn = fnPiasCheck()               'ID読み取り
529 'PInspPosition(1) = PTicketRead  'IDチケット読取位置
530 'MInspGroup%(1) = 1              '検査G番号
531 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
532 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
533     MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
534     '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
535     '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
536 EndIf
537 If MRtn = 1 Then GoTo *CompRead
538 '
539     'エラー時製品位置決めを解除
540 *RE_ERR_REL_1
541 If M_20# = MContinue% Then M_20# = MRtn
542 M_Out(12263) = 1 Dly 0.5    '製品位置決めを解除
543 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)
544 '
545 If MRtn = 1 Then GoTo *CompErrorRelease
546 MRtn = M_20#        'M_20#一時避難
547 M_20# = MClear%
548 fErrorProcess(11,234,284,0)     '位置決め戻端エラー
549 If M_20# = MContinue% Then GoTo *RE_ERR_REL_1
550 If M_20# = MNext% Then M_20# = MRtn
551 If M_20# = MNgProcess% Then M_20# = MAbout%
552 *CompErrorRelease
553 '
554 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
555 If M_20# = MNext% Then M_20# = MPass%
556 Mvs PTicketRead_1                         '22/04/07 追加 渡辺
557 GoTo *ASSY_ERROR_END
558 *CompRead
559     fScrewTStart()           '処理位置変更2/27中村)
560 '
561 '
562 ''    Wait M_In(11888) = 1        'ねじロボ2停止1受信(処理位置変更2/7中村)
563 '    MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
564 '    *RE_ERR_REL_2
565 '    If M_20# = MContinue% Then M_20# = MRtn2
566 '    If MRtn = 0 Then
567 '        MRtn2 = 1       'MRtn2初期化
568 '        M_Out(12263) = 1 Dly 0.5    '製品位置決めを解除
569 '        Mov PInitialPosition  '"イニシャルに戻る動き"
570 '        MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
571 '        If MRtn2 = 0 Then
572 '            MRtn2 = M_20#                   '位置決め戻端エラーならM_20#内部を一時避難
573 '            M_20# = MClear%                 'M_20#初期化
574 '            fErrorProcess(11,234,284,0)     '位置決め戻端エラー
575 '            If M_20# = MNext% Then M_20# = MRtn2        'M_20#に避難した値を代入
576 '                '位置決めエラーが発生して工程を抜ける場合停止処理を行う
577 '            If M_20# = MNgProcess% Then M_20# = MAbout%
578 '            Break
579 '        EndIf
580 '        Break
581 '            EndIf
582 '    If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
583 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
584     '
585     'パレットから製品を取る
586     '
587     *RE_POSITIONING
588     '
589     M_Out(12262) = 1 Dly 0.5 '本体位置決め出端ON
590 '    Wait M_In(11273) = 1     '本体位置決め出端検出
591     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '本体位置決め出端検出
592     If MRtn = 1 Then GoTo *CompPositioning
593     fErrorProcess(11,231,282,0)
594     If M_20# = MNext% Then M_20# = MClear%
595     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
596     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
597     If M_20# = MContinue% Then GoTo *RE_POSITIONING
598     *CompPositioning
599 '
600     Mov PProductOnPltGet_2      '本体受け取り上空回避点
601 '
602 ''    Wait M_In(11888) = 1        'ねじロボ2停止1受信(処理位置変更3/14中村)
603 '    MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
604 '    *RE_ERR_REL_2
605 '    If M_20# = MContinue% Then M_20# = MRtn2
606 '    If MRtn = 0 Then
607 '        MRtn2 = 1       'MRtn2初期化
608 '        M_Out(12263) = 1 Dly 0.5    '製品位置決めを解除
609 '        Mov PInitialPosition  '"イニシャルに戻る動き"
610 '        MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
611 '        If MRtn2 = 0 Then
612 '            MRtn2 = M_20#                   '位置決め戻端エラーならM_20#内部を一時避難
613 '            M_20# = MClear%                 'M_20#初期化
614 '            fErrorProcess(11,234,284,0)     '位置決め戻端エラー
615 '            If M_20# = MNext% Then M_20# = MRtn2        'M_20#に避難した値を代入
616 '                '位置決めエラーが発生して工程を抜ける場合停止処理を行う
617 '            If M_20# = MNgProcess% Then M_20# = MAbout%
618 '            Break
619 '        EndIf
620 '        Break
621 '            EndIf
622 '    If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
623 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
624 '
625 '    Mov PProductOnPltGet_1      '本体受け取り上空
626     '
627     *RE_PLT_GET_1
628     '
629     M_Out(12256) = 0            '本体チャック閉OFF
630     M_Out(12257) = 1            '本体チャック開ON
631     '
632 '    Wait M_In(11265) = 1        '本体チャック開検出
633     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
634     If MRtn = 1 Then GoTo *CompPltGet1
635     fErrorProcess(11,244,284,0)
636     If M_20# = MNext% Then M_20# = MClear%
637     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
638     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
639     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
640     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
641     *CompPltGet1
642     '
643     Mov PProductOnPltGet_1      '本体受け取り上空
644     '
645     Ovrd 25
646 '    Fine 0.05 , P
647     Mvs PProductOnPltGet        '本体受け取り位置
648     Dly 0.1
649     M_Out(12257) = 0            '本体チャック開OFF
650     M_Out(12256) = 1            '本体チャック閉ON
651 '    Fine 0 , P
652     '
653     M_Out(12263) = 1 Dly 0.5                    '本体位置決め戻端ON
654 '    Wait M_In(11274) = 1     '本体位置決め戻端検出
655     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)    '本体位置決め戻端検出
656     If MRtn = 1 Then GoTo *CompPltGet2
657     M_Out(12256) = 0                            '本体チャック閉OFF
658     M_Out(12257) = 1                            '本体チャック開ON
659     Dly 2.0
660     Mvs PProductOnPltGet_1
661     Mov PProductOnPltGet_2
662     fErrorProcess(11,234,284,0)
663     If M_20# = MNext% Then M_20# = MClear%
664     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
665     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
666     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
667     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
668     Mov PProductOnPltGet_1
669     Mvs PProductOnPltGet
670     M_Out(12257) = 0                            '本体チャック開OFF
671     M_Out(12256) = 1                            '本体チャック閉ON
672     Dly 2.0
673     *CompPltGet2
674     '
675 '    Wait M_In(11264) = 1        '本体検出
676     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '本体検出
677     If MRtn = 1 Then GoTo *CompPltGet3
678     M_Out(12256) = 0            '本体チャック閉OFF
679     M_Out(12257) = 1            '本体チャック開ON
680     Dly 2.0
681     Mvs PProductOnPltGet_1
682     Mov PProductOnPltGet_2
683     fErrorProcess(11,252,284,0)
684     If M_20# = MNext% Then M_20# = MClear%
685     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
686     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
687     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
688     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
689     Mov PProductOnPltGet_1
690     Mvs PProductOnPltGet
691     M_Out(12257) = 0            '本体チャック開OFF
692     M_Out(12256) = 1            '本体チャック閉ON
693     Dly 2.0
694     *CompPltGet3
695     '
696 '    Wait M_In(11266) = 1        '本体チャック閉検出
697     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
698     If MRtn = 1 Then GoTo *CompPltGet4
699     M_Out(12256) = 0            '本体チャック閉OFF
700     M_Out(12257) = 1            '本体チャック開ON
701     Dly 2.0
702     Mvs PProductOnPltGet_1
703     Mov PProductOnPltGet_2
704     Dly 0.1
705     M_Out(12257) = 0            '本体チャック開OFF
706     M_Out(12256) = 1            '本体チャック閉ON
707     Dly 3.0
708     fErrorProcess(11,245,284,0)
709     If M_20# = MNext% Then M_20# = MClear%
710     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
711     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
712     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
713     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
714     M_Out(12256) = 0            '本体チャック閉OFF
715     M_Out(12257) = 1            '本体チャック開ON
716     Dly 2.0
717     Mov PProductOnPltGet_1
718     Mvs PProductOnPltGet
719     M_Out(12257) = 0            '本体チャック開OFF
720     M_Out(12256) = 1            '本体チャック閉ON
721     Dly 2.0
722     *CompPltGet4
723     '
724     Dly 0.2                     '念のためディレイ
725     Cnt 1 , 10 , 10
726     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
727     Mvs PProductOnPltGet_1      '本体受け取り上空
728     MRtn = FnCtlValue2(99)       '読書開始信号OFF  2022/04/28 渡辺
729     Ovrd 100
730     Mov PProductOnPltGet_2      '本体受け取り上空回避点
731     '
732     '製品をねじロボ2に置く
733     Mov PProductOnRoboSet_3     '経路
734     Cnt 0
735 '    Wait M_In(11888) = 1        'ねじロボ2停止1受信(処理位置,処理変更3/1中村)
736     MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
737     *RE_ERR_REL_2
738     If MRtn = 0 Then
739         Cnt 0
740         Mov PProductOnPltSet_2
741         Mov PProductOnPltSet_1
742         Mvs PProductOnPltSet
743         M_Out(12256) = 0        '本体チャック閉OFF
744         M_Out(12257) = 1        '本体チャック開ON
745         Dly 2.0
746         Mvs PProductOnPltSet_1
747         Mvs PProductOnPltSet_2
748         Mov PInitialPosition
749     EndIf
750     If MRtn = 0 Then GoTo *ASSY_ERROR_END
751     '
752     *RE_ROBO_SET_1
753     '
754     M_Out(12259) = 0            'DVDメカチャック開OfF
755     M_Out(12258) = 1            'DVDメカチャック閉ON
756 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
757     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
758     If MRtn = 1 Then GoTo *CompRoboSet1
759     fErrorProcess(11,269,284,0)
760     If M_20# = MNext% Then M_20# = MClear%
761     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
762     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
763     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
764     *CompRoboSet1
765 '
766     Mvs PProductOnRoboSet_2     'ねじロボ製品置き上空回避点
767 '    Mvs PProductOnRoboSet_4     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)
768     Ovrd 25
769     Mvs PProductOnRoboSet_1     'ねじロボ製品置き上空
770     Mvs PProductOnRoboSet       'ねじロボ製品置き位置
771     M_Out(12866) = 1 Dly 0.3    'ねじロボ2動作再開(停止1～停止2)
772 '    Wait M_In(11889) = 1        'ねじロボ2停止2受信
773     MScrewRoboNgFlg% = 0
774     MRtn = fScrewTighenRoboCheck(11889)    '停止状態を受信する
775     If MRtn = 0 Then
776         MScrewRoboNgFlg% = 1
777     EndIf
778 '
779     *RE_ROBO_SET_2
780 '
781     M_Out(12256) = 0            '本体チャック閉OFF
782     M_Out(12257) = 1            '本体チャック開ON
783 '    Wait M_In(11265)            '本体チャック開検出
784     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
785     If MRtn = 1 Then GoTo *CompRoboSet2
786     fErrorProcess(11,244,284,0)
787     If M_20# = MNext% Then M_20# = MClear%
788     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
789     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
790     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_2
791     *CompRoboSet2
792     '
793     Mvs PProductOnRoboSet_1     'ねじロボ製品置き上空
794     Mvs PProductOnRoboSet_2     'ねじロボ製品置き上空回避点
795 '    Mvs PProductOnRoboSet_4     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)
796     Ovrd 100
797     Cnt 1 , 10 , 10
798     Mov PProductOnRoboSet_3     '経路
799     '
800     If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition
801     If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
802 '
803 '
804 '
805     '
806     'チルトスライダーを押す
807     Cnt 1 , 10
808     Mov PPushTilt_3             'チルトスライダー押し向き変え回避点
809     Cnt 0
810     Mov PPushTilt_2             'チルトスライダー押し回避点
811     Ovrd 30
812     Mvs PPushTilt_1             'チルトスライダー押し上空
813     Spd 1000
814     Ovrd 5
815     Mvs PPushTilt               'チルトスライダー押し
816     Spd M_NSpd
817     Ovrd 30
818     Cnt 1 , 1 , 1
819     Mvs PPushTilt_1             'チルトスライダー押し上空
820     Cnt 1 , 1 , 10
821     Ovrd 100
822     Mov PPushTilt_2             'チルトスライダー押し回避点
823     '
824     '背面板を取る(コンプライアンスモード実装11/8中村)
825     Mov PPlateBackGet_2         '背面板受け取り上空回避点
826 '    Cnt 1 , 10'暫定
827     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止2～停止3)
828 '    MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する'12/20位置の変更(中村)
829 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
830 '    Mov PPlateBackGet_1         '背面板受け取り上空'暫定
831     Cnt 0
832     '
833     *RE_PLATE_GET
834     '
835     Fine 0.05 , P               'ファイン命令ON
836     Ovrd 25
837     Mvs PPlateBackGet           '背面板受け取り位置
838 '    Dly 0.2                     '一時コメントアウト
839     M_Out(12257) = 0            '本体チャック開OFF
840     M_Out(12256) = 1            '本体チャック閉ON
841     '
842 '    Wait M_In(11266) = 1        '本体チャック閉検出
843     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
844     If MRtn = 1 Then GoTo *CompPlateGet_1
845     M_Out(12256) = 0            '本体チャック閉OFF
846     M_Out(12257) = 1            '本体チャック開ON
847     Mvs PPlateBackGet_1
848     fErrorProcess(11,245,293,0) '284→293に変更6/2中村
849     If M_20# = MNext% Then M_20# = MClear%
850     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
851     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
852     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
853     Mvs PPlateBackGet           '背面板受け取り位置
854     M_Out(12257) = 0            '本体チャック開OFF
855     M_Out(12256) = 1            '本体チャック閉ON
856     *CompPlateGet_1
857     Fine 0 , P                  'ファイン命令OFF
858     '
859     Ovrd 5
860     Accel 25 , 100
861     Dly 0.7                     'ディレイ時間調節中(把持力確保)
862 '    CmpG 0.7,0.7,,,,,,       'X,Y軸ゲインを0.7に変更
863 '    ColChk Off                  '衝突検知OFF
864 '    Cmp Pos , &B11          'X,Y軸コンプライアンスモード開始
865     Mov PPlateBackGet_1         '背面板受け取り上空
866     Cnt 1 , 10 , 10
867 '    Cmp Off                     'コンプライアンスモード終了
868 '    ColChk On                   '衝突検知ON
869     Ovrd 50
870     Mov PPlateBackGet_2         '背面板受け取り上空回避点
871     Ovrd 100
872     Accel 100 , 100
873     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '背面パネルチェック
874     If MRtn = 1 Then GoTo *CompPlateGet_2
875     Cnt 0
876     fErrorProcess(11,267,293,0)                     '284→293に変更6/2中村
877     If M_20# = MNext% Then M_20# = MClear%
878     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
879     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
880     If M_20# = MContinue% Then
881         Mov PPlateBackGet_1
882         Dly 0.3
883         M_Out(12256) = 0            '本体チャック閉OFF
884         M_Out(12257) = 1            '本体チャック開ON
885         Dly 2.0
886     EndIf
887     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
888     *CompPlateGet_2    '
889     '背面板を置く
890 '    Wait M_In(11889) = 1        'ねじロボ2停止2受信
891     ColChk Off
892     Mov PPlateBackSet_12        '背面板置き上空
893     Cnt 1 , 10
894 '
895     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        'もう一度背面パネルチェック
896     If MRtn = 1 Then GoTo *CompPlateGet_3
897     fErrorProcess(11,267,293,0)                     '284→293に変更6/2中村
898     If M_20# = MNext% Then M_20# = MClear%
899     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
900     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
901     If M_20# = MContinue% Then
902         Mov PPlateBackGet_2
903         Mov PPlateBackGet_1
904         M_Out(12256) = 0            '本体チャック閉OFF
905         M_Out(12257) = 1            '本体チャック開ON
906         Dly 2.0
907     EndIf
908     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
909     *CompPlateGet_3
910 '
911 '    ' 部品供給要求送信
912     M_Out(12787) = 1
913 '
914     MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する'12/20位置の変更(中村)
915     If MRtn = 0 Then Mov PInitialPosition    '"イニシャルに戻る動き"
916     If MRtn = 0 Then GoTo *ASSY_ERROR_END
917 '
918 '    Mov PPlateBackSet_11        '経路1
919     Ovrd 50
920 '    Mov PPlateBackSet_10        '経路2
921 '    Mov PPlateBackSet_9         '経路3
922 '    Mov PPlateBackSet_8         '経路4
923 '    Mov PPlateBackSet_7         '経路5
924     Mov PPlateBackSet_6         '経路6
925     Cnt 0
926     Ovrd 25
927     Mvs PPlateBackSet_5         '経路7
928     Mvs PPlateBackSet_4         '経路8
929     Mov PPlateBackSet_3         '経路9
930     Mov PPlateBackSet_2         '経路10
931     Mov PPlateBackSet_1         '経路11
932     Mov PPlateBackSet           '背面板置き位置
933     *RE_PLATE_SET
934     M_Out(12256) = 0            '本体チャック閉OFF
935     M_Out(12257) = 1            '本体チャック開ON
936     '
937 '    Wait M_In(11265)            '本体チャック開検出
938     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
939     If MRtn = 1 Then GoTo *CompPlateSet
940     fErrorProcess(11,244,284,0)
941     If M_20# = MNext% Then M_20# = MClear%
942     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
943     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
944     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
945     *CompPlateSet
946     '
947     ColChk On
948     Mov PPlateBackSet_12        '背面板置き上空
949     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～停止4)
950     Ovrd 100
951     '
952 ''    ' 部品供給要求送信(処理位置変更2/27中村)
953 '    M_Out(12787) = 1
954     'ねじロボ製品クランプ固定待ち
955 '    Wait M_In(11891) = 1        'ねじロボ2停止4受信
956 MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
957 If MRtn = 0 Then Mov PInitialPosition    '"イニシャルに戻る動き"
958 If MRtn = 0 Then GoTo *ASSY_ERROR_END
959     '置き位置画像検査
960 '    Mov PPlateBackCheck_3       '画像検査位置上空
961 '    Mov PPlateBackCheck_2       '通過点
962 '    Mvs PPlateBackCheck         '確認位置
963     '
964     'PInspPosition(2) = PPlateBackCheck
965     'MInspGroup(2) = 2
966     'MRtn = ISInspection(PInspPosition,MInspGroup,2,-1,1)
967     'If MRtn <> 1 Then
968     '   'エラー処理
969     'EndIf
970     '
971 ''    ' 部品供給要求送信
972 '    M_Out(12787) = 1    '処理位置変更
973 '    Mov PPlateBackCheck_3       '画像検査位置上空
974     '
975     'ねじロボ引き込み待ち
976     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～完了)
977     '
978     'DVDメカを取る
979     M_Out(12258) = 0            'DVDメカチャック閉OFF
980     M_Out(12259) = 1            'DVDメカチャック開ON
981     '
982     Mov PMechaGet_3             '経路1
983     Mov PMechaGet_2             'DVDメカ受け取り上空回避点
984 '    Wait M_In(11272)            '部品供給機Ready
985 '    MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '部品供給機Ready
986 '    If MRtn = 0 Then
987 '        fErrorProcess()         'エラー処理
988 '    EndIf
989 '
990 '    ' 部品供給要求送信(処理位置変更)
991     fnAutoScreenComment(513)    '状態表示[部品供給待ち] 2022/04/26 渡辺
992 '    M_Out(12787) = 1
993     '    ' 部品供給完了待ち(処理変更2/27中村)
994 *RE_FEEDER_READY
995 '    Wait M_In(11810) = 1
996 MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '供給待ち
997 If MRtn = 1 Then GoTo *CompFeederReady
998 '   ' 部品供給要求終了
999 M_Out(12787) = 0
1000 fErrorProcess(11,289,290,0)                '284→290に変更6/2中村
1001 If M_20# = MNext% Then M_20# = MClear%
1002 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1003     Mov PMechaGet_2
1004     Mov PMechaGet_3
1005     Mov PMechaGet_4
1006     Mov PInitialPosition
1007 EndIf
1008 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1009 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1010     ' 部品供給要求
1011 M_Out(12787) = 1
1012 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
1013 *CompFeederReady
1014 '    ' 部品供給要求終了
1015     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1016     M_Out(12787) = 0
1017 '
1018     Mov PMechaGet_1             'DVDメカ受け取り上空
1019     '
1020     *RE_MECHA_GET_1
1021     '
1022     M_Out(12258) = 0            'DVDメカチャック閉OFF
1023     M_Out(12259) = 1            'DVDメカチャック開ON
1024     '
1025 '    Wait M_In(11268) = 1        'DVDメカチャック開検出
1026     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1027     If MRtn = 1 Then GoTo *CompMechaGet1
1028     Mov PMechaGet_2
1029     Mov PMechaGet_3
1030     Mov PMechaGet_4
1031     fErrorProcess(11,270,284,0)
1032     If M_20# = MNext% Then M_20# = MClear%
1033     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1034     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1035     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1036     Mov PMechaGet_3
1037     Mov PMechaGet_2
1038     Mov PMechaGet_1
1039     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1040     *CompMechaGet1
1041     '
1042     M_Out(12261) = 0            'DVDメカシリンダー戻OFF
1043     M_Out(12260) = 1            'DVDメカシリンダー出ON
1044 '    Wait M_In(11271) = 1        'DVDメカシリンダー出検出
1045     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVDメカシリンダー出検出
1046     If MRtn = 1 Then GoTo *CompMechaGet2
1047     Mov PMechaGet_2
1048     Mov PMechaGet_3
1049     Mov PMechaGet_4
1050     fErrorProcess(11,271,284,0)
1051     If M_20# = MNext% Then M_20# = MClear%
1052     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1053     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1054     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1055     Mov PMechaGet_3
1056     Mov PMechaGet_2
1057     Mov PMechaGet_1
1058     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1059     *CompMechaGet2
1060     '
1061     Ovrd 25
1062     Mvs PMechaGet               'DVDメカ受け取り位置
1063     Dly 0.1
1064 '
1065     MRtn = 0
1066     MRtn2 = 0
1067     *RE_MECHA_GET_2
1068     If M_20# = MContinue% Then M_20# = MClear%
1069     M_Out(12259) = 0            'DVDメカチャック開OfF
1070     M_Out(12258) = 1            'DVDメカチャック閉ON
1071     '
1072 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
1073     If MRtn = 1 Then Dly 1.0
1074     If MRtn = 1 Then GoTo *CompMechaGet3
1075     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
1076     If M_20# = MNext% Then GoTo *CompMechaGet3
1077     If MRtn = 1 Then GoTo *CompMechaGet3
1078     M_Out(12258) = 0            'DVDメカチャック閉OfF
1079     M_Out(12259) = 1            'DVDメカチャック開ON
1080     Dly 2.0
1081     Mvs PMechaGet_1
1082     Mov PMechaGet_2
1083     M_Out(12259) = 0            'DVDメカチャック開OfF
1084     M_Out(12258) = 1            'DVDメカチャック閉ON
1085     Mov PMechaGet_3
1086     Mov PMechaGet_4
1087     fErrorProcess(11,269,284,0)
1088     If M_20# = MNext% Then
1089         M_20# = MClear%
1090         MRtn = 1
1091     EndIf
1092     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1093     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1094     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1095     M_Out(12258) = 0            'DVDメカチャック閉OfF
1096     M_Out(12259) = 1            'DVDメカチャック開ON
1097     Mov PMechaGet_3
1098     Mov PMechaGet_2
1099     Mov PMechaGet_1
1100     Mvs PMechaGet
1101     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1102     *CompMechaGet3
1103     M_20# = MClear%
1104     '
1105 '    Wait M_In(11267) = 1        'DVDメカ検出
1106     If MRtn2 = 1 Then GoTo *CompMechaGet4
1107     MRtn2 = frInCheck(11267,1,MSETTIMEOUT05&)   'DVDメカ検出
1108     If MRtn2 = 1 Then GoTo *CompMechaGet4
1109     M_Out(12258) = 0            'DVDメカチャック閉OfF
1110     M_Out(12259) = 1            'DVDメカチャック開ON
1111     Dly 2.0
1112     Mvs PMechaGet_1
1113     Mov PMechaGet_2
1114     M_Out(12259) = 0            'DVDメカチャック開OfF
1115     M_Out(12258) = 1            'DVDメカチャック閉ON
1116     Mov PMechaGet_3
1117     Mov PMechaGet_4
1118     fErrorProcess(11,273,284,0)
1119     If M_20# = MNext% Then
1120         M_20# = MClear%
1121         MRtn2 = 1
1122     EndIf
1123     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1124     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1125     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1126     M_Out(12258) = 0            'DVDメカチャック閉OfF
1127     M_Out(12259) = 1            'DVDメカチャック開ON
1128     Mov PMechaGet_3
1129     Mov PMechaGet_2
1130     Mov PMechaGet_1
1131     Mvs PMechaGet
1132     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1133     *CompMechaGet4
1134     M_20# = MClear%
1135     Dly 0.5
1136     '
1137     Mvs PMechaGet_1             'DVDメカ受け取り上空
1138 '    *RE_MECHA_GET_3
1139     M_Out(12260) = 0            'DVDメカシリンダー出OFF
1140     M_Out(12261) = 1            'DVDメカシリンダー戻ON
1141 '    Wait M_In(11270) = 1        'DVDメカシリンダー戻検出
1142     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVDメカシリンダー戻検出
1143 '    If MRtn = 1 Then GoTo *CompMechaGet5       '処理位置変更2/11中村
1144 '    fErrorProcess(11,272,284,0)
1145 '    If M_20# = MNext% Then M_20# = MClear%
1146 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1147 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1148 '    If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1149 '    *CompMechaGet5
1150     '
1151     If MRtn = 1 Then Ovrd 100
1152     Mov PMechaGet_2             'DVDメカ受け取り上空回避点
1153 '    ' 部品供給要求終了
1154     M_Out(12787) = 0
1155 '    ' 部品取得完了送信(パルス)
1156     M_Out(12800) = 1 Dly 0.5
1157     Mov PMechaGet_3             '経路1
1158     Mov PMechaGet_4             '経路2
1159 '
1160     *RE_MECHA_GET_3
1161     M_Out(12260) = 0            'DVDメカシリンダー出OFF
1162     M_Out(12261) = 1            'DVDメカシリンダー戻ON
1163     If MRtn = 1 Then GoTo *CompMechaGet5
1164     fErrorProcess(11,272,284,0)
1165     If M_20# = MNext% Then M_20# = MClear%
1166     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1167     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1168     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1169     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1170     *CompMechaGet5
1171     '
1172     'DVDメカを仮置き台へ置く
1173 '    Wait M_In(11920) = 0             'BaseUnit6側仮置き台フラグ確認(処理位置変更2/11中村)
1174 '
1175 '   仮置き台が回転中か確認
1176     *Loop_CW_CCW_1
1177     If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_1 Else GoTo *Loop_CW_CCW_1
1178     *Next_CW_CCW_1
1179     If M_In(11920) = 0 Then GoTo *OK_FLG_1 Else GoTo *Loop_CW_CCW_1          'BaseUnit6側仮置き台フラグ確認
1180     *OK_FLG_1
1181 '
1182     M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1183     '
1184     'DVDメカが仮置き台に置かれていないかの確認(追加ここから10/1中村)
1185     MRtn = 1
1186     If M_In(11931) = 1 Then          '回転方向の確認(CW方向)
1187         If M_In(11928) = 0 Then      'BaseUnit5側にDVDメカが置かれていた場合
1188             M_Out(12912) = 0         '仮置き台フラグ回収(回転可能状態にするため)
1189             Wait M_In(11930) = 1     '仮置き台回転待ち
1190             MRtn = 0
1191         EndIf
1192     ElseIf M_In(11930) = 1 Then      '回転方向の確認(CCW方向)
1193         If M_In(11929) = 0 Then      'BaseUnit5側にDVDメカが置かれていた場合
1194             M_Out(12912) = 0         '仮置き台フラグ回収(回転可能状態にするため)
1195             Wait M_In(11931) = 1     '仮置き台回転待ち
1196             MRtn = 0
1197         EndIf
1198     Else
1199         MRtn = 0
1200     EndIf
1201     If MRtn = 0 Then GoTo *Loop_CW_CCW_1
1202     '
1203 *Loop_CW_CCW_S
1204     fnAutoScreenComment(530)    '状態表示[工程６の動作終了待ち] 2022/04/26 渡辺
1205 'Ver 0.4 追加 -----------------------
1206 '自工程が12912 = 1 を出力した後、再度 工程6側の動作中監視を行う
1207     MRtn = 0
1208     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec 工程6のフラグON監視
1209     If MRtn = 1 Then M_Out(12912) = 0                  '仮置き台フラグ回収 工程6優先のため12912=0を出力
1210     If MRtn = 1 Then Dly 0.7
1211     If MRtn = 1 Then GoTo *Loop_CW_CCW_S        '工程6の動作中フラグがONしていたら再度ループ
1212     M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1213 'Ver 0.4 ここまで -------------------
1214 '
1215     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1216     Mov PMechaSet_3             'DVDメカ仮置き回避点1
1217 'Dly 5.0   'デバッグ用
1218 '
1219 *Loop_CW_CCW_2
1220 'Ver 0.4 追加 -----------------------
1221     '自工程が12912 = 1 を出力した後、再度 工程6側の動作中監視を行う
1222     MRtn = 0
1223     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec 工程6のフラグON監視
1224     If MRtn = 1 Then M_Out(12912) = 0                  '仮置き台フラグ回収 工程6優先のため12912=0を出力
1225     If MRtn = 1 Then Dly 0.7
1226     If MRtn = 1 Then GoTo *Loop_CW_CCW_2        '工程6の動作中フラグがONしていたら再度ループ
1227     M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1228 'Ver 0.4 ここまで -------------------
1229 '
1230     Mov PMechaSet_2             'DVDメカ仮置き回避点2
1231 '
1232 '    *Loop_CW_CCW_2  '仮置き台の状態をもう一度確認する(コメントアウト2/27中村)
1233 '    If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_2 Else GoTo *Loop_CW_CCW_2
1234 '    *Next_CW_CCW_2
1235 '    If M_In(11920) = 0 Then GoTo *OK_FLG_2 Else GoTo *Loop_CW_CCW_2         'BaseUnit6側仮置き台フラグ確認
1236 '    *OK_FLG_2
1237 ''
1238 '    M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1239 '
1240     '
1241     *RE_MECHA_SET_1
1242     If M_20# = MContinue% Then M_20# = MClear%
1243     Ovrd 25
1244     M_Out(12261) = 0            'DVDメカシリンダー戻OFF
1245     M_Out(12260) = 1            'DVDメカシリンダー出ON
1246 '    Wait M_In(11271) = 1        'DVDメカシリンダー出検出
1247     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVDメカシリンダー出検出
1248     If MRtn = 1 Then GoTo *CompMechaSet1
1249     Mov PMechaSet_3
1250     Mov PMechaGet_4
1251     fErrorProcess(11,271,284,0)
1252     If M_20# = MNext% Then M_20# = MClear%
1253     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1254     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1255     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1256     Mov PMechaSet_3
1257     Mov PMechaSet_2
1258     Ovrd 100
1259     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_1
1260     *CompMechaSet1
1261     '
1262     *RE_MECHA_SET_12
1263     Fine 0.05 , P
1264 '    Wait M_In(11920) = 0        'BaseUnit6側仮置き台フラグ確認(コメントアウト2/27中村)
1265 '    M_Out(12912) = 1            '仮置き台フラグ立て
1266     If M_In(11931) = 1 Then     '回転方向の確認(CW方向)(追加ここまで10/1中村)
1267         Mov PMechaSet1_1        'DVDメカ仮置き上空1
1268         Ovrd 10
1269         Mvs PMechaSet1          'DVDメカ仮置き位置1
1270         Dly 0.1
1271         M_Out(12258) = 0        'DVDメカチャック閉OFF
1272         M_Out(12259) = 1        'DVDメカチャック開ON
1273 '        Wait M_In(11268) = 1    'DVDメカチャック開検出
1274         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1275         Mvs PMechaSet1_1        'DVDメカ仮置き上空1
1276     ElseIf M_In(11930) = 1 Then '回転方向の確認(CCW方向)(追加ここから10/1中村)
1277         Mov PMechaSet2_1        'DVDメカ仮置き上空
1278         Ovrd 10
1279         Mvs PMechaSet2          'DVDメカ仮置き位置2
1280         Dly 0.1
1281         M_Out(12258) = 0        'DVDメカチャック閉OFF
1282         M_Out(12259) = 1        'DVDメカチャック開ON
1283 '        Wait M_In(11268) = 1    'DVDメカチャック開検出
1284         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1285         Mvs PMechaSet2_1        'DVDメカ仮置き上空2
1286     'Else
1287         'エラー文(仮置き台が正常な位置に無い)
1288     EndIf                       '追加ここまで10/1中村
1289     Fine 0 , P
1290     '
1291     If MRtn = 1 Then GoTo *CompMechaSet2
1292     fErrorProcess(11,270,284,0)
1293     If M_20# = MNext% Then M_20# = MClear%
1294     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1295     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1296     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_12
1297     *CompMechaSet2
1298     '
1299     Ovrd 100
1300     *RE_MECHA_SET_2
1301     M_Out(12260) = 0            'DVDメカシリンダー出OFF
1302     M_Out(12261) = 1            'DVDメカシリンダー戻ON
1303 '    Wait M_In(11270) = 1        'DVDメカシリンダー戻検出
1304     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVDメカシリンダー戻検出
1305     If MRtn = 1 Then GoTo *CompMechaSet3
1306     fErrorProcess(11,272,284,0)
1307     If M_20# = MNext% Then M_20# = MClear%
1308     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1309     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1310     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_2
1311     *CompMechaSet3
1312     '
1313     Mov PMechaSet_2             'DVDメカ仮置き回避点2
1314     M_Out(12912) = 0                  '仮置き台フラグ回収(追加10/1中村)
1315     '
1316     'ねじロボ2の製品を取る
1317     Mov PProductOnRoboGet_4     '経路3から4へ
1318     M_Out(12259) = 0            'DVDメカチャック開OfF
1319     M_Out(12258) = 1            'DVDメカチャック閉ON
1320 '    Wait M_In(11876) = 1        'ねじロボ2完了待機を受信
1321 MRtn = fScrewTighenRoboCheck(11876)    '停止状態を受信する
1322 If MRtn = 0 Then Mov PInitialPosition   '"イニシャルに戻る動き"
1323 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1324 '
1325     *RE_ROBO_GET_1
1326 '
1327     M_Out(12259) = 0            'DVDメカチャック開OFF
1328     M_Out(12258) = 1            'DVDメカチャック閉ON
1329     If M_20# = MContinue% Then Dly 0.5
1330 '
1331 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
1332     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
1333     If MRtn = 1 Then GoTo *CompRoboGet1
1334     fErrorProcess(11,269,284,0)
1335     If M_20# = MNext% Then M_20# = MClear%
1336     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1337     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1338     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1339     If M_20# = MContinue% Then GoTo *RE_ROBO_GET_1
1340     *CompRoboGet1
1341     '
1342 '    Ovrd 50
1343     Mov PProductOnRoboGet_3     'ねじロボ製品取り上空回避点2から3へ
1344 '    Ovrd 20
1345     Mvs PProductOnRoboGet_2     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から2へ
1346     Ovrd 20
1347     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
1348     Ovrd 10
1349     Mvs PProductOnRoboGet       'ねじロボ製品取り位置
1350     Dly 0.2
1351 '
1352     *RE_ROBO_GET_2
1353 '
1354     M_Out(12257) = 0            '本体チャック開OFF
1355     M_Out(12256) = 1            '本体チャック閉ON
1356 '
1357 '    Wait M_In(11266) = 1        '本体チャック閉検出
1358     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
1359     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
1360     M_Out(12256) = 0            '本体チャック閉OFF
1361     M_Out(12257) = 1            '本体チャック開ON
1362     Dly 2.0
1363     Mvs PProductOnRoboGet_1
1364     Mvs PProductOnRoboGet_2
1365     Mov PProductOnRoboGet_3
1366     Mov PProductOnRoboGet_4
1367     Mov PInitialPosition
1368     M_Out(12257) = 0            '本体チャック開OFF
1369     M_Out(12256) = 1            '本体チャック閉ON
1370     Dly 1.0
1371     fErrorProcess(11,245,284,0)
1372     If M_20# = MNext% Then MRtn = 1
1373     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1374     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1375     M_Out(12256) = 0            '本体チャック閉OFF
1376     M_Out(12257) = 1            '本体チャック開ON
1377     Dly 2.0
1378     Mov PProductOnRoboGet_4
1379     Mov PProductOnRoboGet_3
1380     Mov PProductOnRoboGet_2
1381     Mvs PProductOnRoboGet_1
1382     Mvs PProductOnRoboGet
1383     If M_20# = MContinue% Or M_20# = MNext% Then GoTo *RE_ROBO_GET_2
1384     *CompRoboGet2
1385     M_20# = MClear%
1386     '
1387     Dly 0.2
1388     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
1389     Ovrd 50
1390     Mvs PProductOnRoboGet_2     'ねじロボ製品取り上空回避点(9/27暫定コメントアウト)12/15コメント解除
1391     Mvs PProductOnRoboGet_3     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から3へ
1392     Ovrd 100
1393     Mov PProductOnRoboGet_4     '経路3から4へ
1394     Cnt 1 , 10 , 10
1395 '
1396     M_Out(12868) = 1 Dly 0.3    'ねじロボ2動作完了を送信
1397     *RE_ROBO_GET_3
1398     M_Out(12258) = 0            'DVDメカチャック閉OFF
1399     M_Out(12259) = 1            'DVDメカチャック開ON
1400 '    Wait M_In(11268) = 1        'DVDメカチャック開検出
1401     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1402     If MRtn = 1 Then GoTo *CompRoboGet3
1403     fErrorProcess(11,270,284,0)
1404     If M_20# = MNext% Then M_20# = MClear%
1405     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1406     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1407     If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1408     *CompRoboGet3
1409     '
1410     'パレットへ製品を置く
1411     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1412     Cnt 1 , 10
1413     Mov PProductOnPltSet_1      '本体置き位置上空
1414     Cnt 0
1415     Ovrd 10
1416     Mvs PProductOnPltSet        '本体置き位置
1417     Dly 0.5
1418 '
1419     *RE_PLT_SET
1420 '
1421     M_Out(12256) = 0            '本体チャック閉OFF
1422     M_Out(12257) = 1            '本体チャック開ON
1423 '
1424     Wait M_In(11265) = 1        '本体チャック開検出
1425 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
1426     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
1427     If MRtn = 1 Then GoTo *CompPltSet
1428     fErrorProcess(11,244,284,0)
1429     If M_20# = MNext% Then M_20# = MClear%
1430     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1431         Mvs PProductOnPltSet_1
1432         Mov PProductOnPltSet_2
1433         Mov PInitialPosition
1434     EndIf
1435     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1436     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1437     If M_20# = MContinue% Then GoTo *RE_PLT_SET
1438     *CompPltSet
1439 '
1440     Mvs PProductOnPltSet_1      '本体置き位置上空
1441     Ovrd 100
1442     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1443 '    Mov PInitialPosition        'イニシャルポジション
1444     MRtn = FnCtlValue2(2)          '組立ＯＫ＋１  2022/04/28 渡辺
1445     Mov PTicketRead_1           'チケットID読み取り回避点
1446     MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1447     '
1448     'チケットID書き込み
1449     M_20# = MAssyOK%
1450     *ASSY_ERROR_END
1451     *AssyEnd
1452     *fnAssyStart_FEndPosi
1453 FEnd
1454 '
1455 '■fnPiasCheck
1456 ''' <summary>
1457 ''' PIASチケット読込み
1458 ''' </summary>
1459 ''' <returns>   0 : NG
1460 '''             1 : OK(読込み完了)
1461 ''' </returns>
1462 ''' <remarks>
1463 ''' Date   : 2021/07/07 : M.Hayakawa
1464 ''' </remarks>'
1465 Function M% fnPiasCheck
1466     fnPiasCheck = 0
1467     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1468     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1469 '
1470 *RETRY_PIAS
1471     M_20# = MClear%
1472     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1473     '
1474     '【IDチケット読み込み】
1475     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1476     MInspGroup%(1) = 1              '検査G番号
1477     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1478 '
1479     'エラーの場合
1480     If MRtn <> 1 Then
1481         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1482         If MRtn <> 1 Then
1483             'D720 -> D1300 コピー要求
1484             M_Out(12565) = 1
1485             Dly 0.5
1486             M_Out(12565) = 0
1487             'エラー処理記述
1488             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1489             'GOT KEY入力待ち
1490             MKeyNumber = fnKEY_WAIT()
1491             '
1492             Select MKeyNumber
1493                 Case MNext%         '次へを選択した場合
1494                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1495                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1496                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1497                     Break
1498                 Case MAbout%        '停止を選択した場合
1499                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1500                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1501                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1502                     Break
1503                 Case MNgProcess%    'NGを選択した場合
1504                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1505                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1506                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1507                     Break
1508                 Case MContinue%     '継続を選択した場合
1509                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1510                     M_20# = MContinue%
1511                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1512                     Break
1513             End Select
1514         EndIf
1515     EndIf
1516 '----------D720 -> D1300 コピー要求----------
1517     M_Out(12565) = 1
1518     Dly 0.5
1519     M_Out(12565) = 0
1520 '----------通信確認をする----------
1521     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1522     MRtn = 0                ' 初期化
1523     M_20# = MClear%         ' 初期化
1524     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1525     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1526     If MRtn <> 1 Then
1527         If M_20# = MContinue% Then
1528             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1529         Else
1530             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1531         EndIf
1532     EndIf
1533 '----------工程抜け確認----------
1534     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1535     MRtn = 0                ' 初期化
1536     M_20# = MClear%         ' 初期化
1537     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1538     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1539     If MRtn <> 1 Then
1540         If M_20# = MContinue% Then
1541             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1542         Else
1543             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1544         EndIf
1545     EndIf
1546     '
1547     fnPiasCheck = 1
1548     *fnPiasCheck_End
1549 FEnd
1550 '
1551 '■fnPCComuCheck
1552 ''' <summary>
1553 ''' PC-PLC通信チェック
1554 ''' </summary>
1555 ''' <returns>   0 : NG
1556 '''             1 : OK(読込み完了)
1557 ''' </returns>
1558 ''' <remarks>
1559 ''' Date   : 2021/07/07 : M.Hayakawa
1560 ''' </remarks>'
1561 Function M% fnPCComuCheck
1562     fnPCComuCheck = 0
1563     MJudge% = 0                                  '初期化
1564     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1565     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1566     '
1567     For MStaNo = 0 To 5
1568         '
1569         If M_In(MIN_PIAS_ComOK%) = 1 Then
1570             'PC通信OK(M400)
1571             MJudge% = MOK%
1572             MStaNo = 5
1573             Break
1574         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1575             'toRBT_通信確認time out
1576             MJudge% = MNG%
1577             MCommentD1001 = 15
1578             MCommentD1002 = 21
1579             MStaNo = 5
1580             Break
1581         Else
1582             'toRBT_通信確認time out
1583             MJudge% = MNG%
1584             MCommentD1001 = 14
1585             MCommentD1002 = 21
1586             Break
1587         EndIf
1588     Next MStaNo
1589     '
1590     '上記で返信フラグを受信してからPC通信確認OFF
1591     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1592     '
1593     'エラー画面
1594     If MJudge% <> MOK% Then
1595         M_20# = MClear%     '初期化
1596         'エラー処理記述
1597         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1598         'GOT KEY入力待ち
1599         MKeyNumber = fnKEY_WAIT()
1600         '
1601         If MKeyNumber = MAbout% Then            '停止を選択した場合
1602             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1603             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1604             Break
1605         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1606             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1607             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1608             Break
1609         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1610             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1611             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1612             Break
1613         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1614             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1615             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1616             Break
1617         EndIf
1618     Else
1619         'OKの場合
1620         fnPCComuCheck = 1
1621     EndIf
1622 FEnd
1623 '
1624 '■fnProcessCheck
1625 ''' <summary>
1626 ''' 工程抜け確認
1627 ''' </summary>
1628 ''' <returns>    1：工程履歴OK     0：異常終了
1629 '''             -1：前工程履歴NG  -2：自工程履歴あり
1630 '''             -3：モデル仕向NG  -4：タイムアウト
1631 '''             -5：履歴処理エラー
1632 ''' </returns>
1633 ''' <remarks>
1634 ''' Date   : 2021/07/07 : M.Hayakawa
1635 ''' </remarks>'
1636 Function M% fnProcessCheck
1637     fnProcessCheck = 0
1638     MJudge% = MNG%      '一旦NGを初期化とする
1639 '----------工程抜け確認----------
1640     MCommentD1001 = 0   'コメント初期化
1641     For MStaNo = 0 To 5
1642         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1643         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1644         '
1645         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1646             MJudge% = MOK%
1647             fnAutoScreenComment(85)     ' AUTO画面
1648             MStaNo = 5
1649             Break
1650         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1651             MFlgLoop% = 0
1652             MJudge% = MNG%
1653             MCommentD1001 = 27
1654             MCommentD1002 = 22
1655             fnAutoScreenComment(94)     ' AUTO画面
1656             fnProcessCheck = -2         ' NGは-2を返す
1657             MStaNo = 5
1658             Break
1659         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1660            MJudge% = MNG%
1661             MCommentD1001 = 31
1662             MCommentD1002 = 22
1663             fnAutoScreenComment(83)     ' AUTO画面
1664             fnProcessCheck = -3         ' NGは-3を返す
1665             MStaNo = 5
1666             Break
1667         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1668             '履歴NGは直ぐに終了せず繰り返し確認を行う
1669             '前工程の書込みが終了していない可能性があるため
1670             MJudge% = MNG%
1671             MCommentD1001 = 32
1672             MCommentD1002 = 22
1673             fnAutoScreenComment(84)     ' AUTO画面
1674             fnProcessCheck = -1         ' NGは-1を返す
1675             Dly 1.0
1676             '工程抜け確認OFF
1677             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1678             Dly 1.0
1679            'MStaNo = 5
1680             Break
1681         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1682             MFlgLoop% = 0
1683             MJudge% = MNG%
1684             MCommentD1001 = 29
1685             MCommentD1002 = 22
1686             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1687             fnProcessCheck = -5         ' NGは-5を返す
1688             MStaNo = 5
1689             Break
1690         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1691             MJudge% = MNG%
1692             If MCommentD1001 = 32 Then
1693                 '何もしない
1694             Else
1695                 MCommentD1001 = 26
1696             EndIf
1697             MCommentD1002 = 22
1698             fnProcessCheck = -4         ' NGは-4を返す
1699             MStaNo = 5
1700             Break
1701         Else
1702             MJudge% = MNG%
1703             MCommentD1001 = 28
1704             MCommentD1002 = 22
1705         EndIf
1706     Next MStaNo
1707     '工程抜け確認OFF
1708     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1709     '通過履歴NG 工程抜けの場合
1710     If MJudge% = MPass% Then
1711         M_20# = MPass%
1712     EndIf
1713     '
1714     'エラー画面
1715     If MJudge% <> MOK% Then
1716         M_20# = MClear%     '初期化
1717         'エラー処理記述
1718         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1719         'GOT KEY入力待ち
1720         MKeyNumber = fnKEY_WAIT()
1721         '
1722         Select MKeyNumber
1723             Case MAbout%        '停止を選択した場合
1724                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1725                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1726                 Break
1727             Case MNext%         '次へを選択した場合
1728                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1729                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1730                 Break
1731             Case MContinue%     '継続を選択した場合
1732                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1733                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1734                 Break
1735             Case MNgProcess%    'NGを選択した場合
1736                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1737                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1738                 Break
1739         End Select
1740     Else
1741         fnProcessCheck = 1  ' OKは1を返す
1742     EndIf
1743 FEnd
1744 '
1745 '■fnPiasWrite
1746 ''' <summary>
1747 ''' Pias 組立結果書込み要求
1748 ''' </summary>
1749 '''<param name="MFlg%">
1750 '''                 MOK%(1) = 工程履歴にOKを書込む
1751 '''                 MNG%(0) = 工程履歴にNGを書込む
1752 '''</param>
1753 '''<returns></returns>
1754 ''' <remarks>
1755 ''' Date   : 2021/07/07 : M.Hayakawa
1756 ''' </remarks>'
1757 Function M% fnPiasWrite(ByVal MFlg%)
1758       fnPiasWrite = 0
1759 *RETRY_PIASWRITE
1760     '
1761     '組立OK(MOK%)の場合　M306 ON
1762    '組立NG(MNG%)の場合　M307 ON
1763     If MFlg% = MOK% Then
1764         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1765     Else
1766         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1767     EndIf
1768     Dly 0.1                  '念のため
1769     '
1770     'Piasへ書込み開始 M305 -> ON
1771     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1772     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1773     '
1774     MJudge% = MNG%
1775     '
1776     For MStaNo = 0 To 5
1777         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1778             MJudge% = MOK%
1779             'MRet = fnAutoScreenComment(85)  'AUTO画面
1780             MStaNo = 5
1781             Break
1782         '
1783         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1784             MJudge% = MNG%
1785             'MRet = fnAutoScreenComment(85)  'AUTO画面
1786            MCommentD1001 = 34
1787            MCommentD1002 = 25
1788             MStaNo = 5
1789             Break
1790         '
1791         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1792             MJudge% = MNG%
1793             'MRet = fnAutoScreenComment(85)  'AUTO画面
1794            MCommentD1001 = 35
1795            MCommentD1002 = 25
1796             MStaNo = 5
1797             Break
1798         '
1799         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1800             MJudge% = MNG%
1801             'MRet = fnAutoScreenComment(85)  'AUTO画面
1802            MCommentD1001 = 36
1803            MCommentD1002 = 25
1804             MStaNo = 5
1805             Break
1806         '
1807         Else
1808             MJudge% = MNG%
1809            MCommentD1001 = 42
1810            MCommentD1002 = 25
1811         '
1812         EndIf
1813         '
1814     Next MStaNo
1815     '
1816     'Piasへ書込み開始 M305 -> OfF
1817     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1818     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1819     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1820     '
1821     '
1822     '通過履歴NG 工程抜けの場合
1823     If MJudge% = MPass% Then
1824         M_20# = MPass%
1825     EndIf
1826     '
1827    M_20# = MClear%     '初期化
1828     '
1829     'エラー画面
1830     If MJudge% < MOK% Then
1831     '
1832 '残しておくが現状では使用しないラベル
1833 *RETRY_ERR_WRITE
1834         M_20# = MClear%     '初期化
1835         'エラー処理記述
1836         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1837         'GOT KEY入力待ち
1838         MKeyNumber = fnKEY_WAIT()
1839         '
1840         If MKeyNumber = MAbout% Then   '停止を選択した場合
1841             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1842            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1843             Break
1844         '
1845         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1846             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1847             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1848         '
1849         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1850             M_20# = MPass%            'M_20# プログラム間共通外部変数
1851             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1852         '
1853         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1854             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1855            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1856             Break
1857         '
1858         EndIf
1859         '
1860         If M_20# = MClear% Then *RETRY_ERR_WRITE
1861         '
1862     EndIf
1863     '
1864     If M_20# = MContinue% Then *RETRY_PIASWRITE
1865     '
1866     fnPiasWrite = 1
1867     '
1868 FEnd
1869 '
1870 '■fnPCBNumberCheck
1871 ''' <summary>
1872 ''' Pias 基板番号照合要求
1873 ''' </summary>
1874 '''<param name="%"></param>
1875 '''<param name="%"></param>
1876 '''<returns></returns>
1877 ''' <remarks>
1878 ''' Date   : 2021/07/07 : M.Hayakawa
1879 ''' </remarks>'
1880 Function M% fnPCBNumberCheck
1881       fnPCBNumberCheck = 0
1882     '
1883 *RETRY_PCBCHECK
1884     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1885     'Piasへ基板照合開始 M310 -> ON
1886     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1887     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1888     '
1889     MJudge% = MNG%
1890     '
1891     For MStaNo = 0 To 5
1892         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1893             MJudge% = MOK%
1894             fnAutoScreenComment(96)  'AUTO画面
1895             MStaNo = 5
1896             Break
1897         '
1898         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1899             MJudge% = MNG%
1900             fnAutoScreenComment(97)  'AUTO画面
1901             MCommentD1001 = 37
1902             MCommentD1002 = 25
1903             MStaNo = 5
1904             Break
1905         '
1906         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1907             MJudge% = MNG%
1908             fnAutoScreenComment(98)  'AUTO画面
1909             MCommentD1001 = 38
1910             MCommentD1002 = 25
1911             MStaNo = 5
1912             Break
1913         '
1914         ElseIf M_In(11580) = 1 Then                         'time out
1915             MJudge% = MNG%
1916             fnAutoScreenComment(99)  'AUTO画面
1917             MCommentD1001 = 39
1918             MCommentD1002 = 25
1919             MStaNo = 5
1920             Break
1921         '
1922         Else
1923             MJudge% = MNG%
1924            MCommentD1001 = 41
1925            MCommentD1002 = 25
1926         '
1927         EndIf
1928         '
1929     Next MStaNo
1930     '
1931     'Piasへ基板照合開始 M310 -> OfF
1932     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1933     '
1934     '
1935     '通過履歴NG 工程抜けの場合
1936     If MJudge% = MPass% Then
1937         M_20# = MPass%
1938     EndIf
1939     '
1940    M_20# = MClear%     '初期化
1941     '
1942     'エラー画面
1943     If MJudge% < MOK% Then
1944     '
1945 '残しておくが現状では使用しないラベル
1946 *RETRY_ERR_PCBNUMBER
1947         M_20# = MClear%     '初期化
1948         'エラー処理記述
1949         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1950         'GOT KEY入力待ち
1951         MKeyNumber = fnKEY_WAIT()
1952         '
1953         If MKeyNumber = MAbout% Then   '停止を選択した場合
1954             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1955             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1956             Break
1957         '
1958         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1959             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1960             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1961         '
1962         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1963             M_20# = MPass%            'M_20# プログラム間共通外部変数
1964             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1965         '
1966         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1967             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1968             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1969             Break
1970         '
1971         EndIf
1972         '
1973         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1974         '
1975     EndIf
1976     '
1977     If M_20# = MContinue% Then *RETRY_PCBCHECK
1978 FEnd
1979 '
1980 '■ScrewTight_S2
1981 ''' <summary>
1982 ''' ねじ締めを行う
1983 ''' </summary>
1984 '''<param name="PScrewPos()">
1985 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1986 '''             PScrewPos(2)    ：ねじ締め回避点
1987 '''             PScrewPos(10)   ：ねじ締め終了高さ
1988 '''</param>
1989 '''<returns>整数
1990 '''         0=異常終了、1=正常終了
1991 '''</returns>
1992 ''' <remarks>
1993 ''' Date   : 2021/07/07 : M.Hayakawa
1994 ''' </remarks>'
1995 Function M% ScrewTight_S2(ByVal PScrewPosition())   'ネジ締め個別設定
1996     ScrewTight_S2 = 0
1997     MOKNGFlg = 0
1998     Ovrd 100
1999     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
2000     ' 暫定
2001     Ovrd 5
2002     Mvs PScrewPosition(10),-10    ' パレット上ねじ締めS①の上空へ移動
2003 '    Ovrd MOvrdA
2004     '暫定マスク
2005 '    M_Out(Y62_Driver)=1     ' バンクセッティング　C1
2006 '    Dly 0.1
2007 '    M_Out(Y61_Driver)=1     'ドライバーON　CW
2008 '    'Spd 8.3 '外部ライド100  内部ライド60   'ライド100-40　100%：Spd　15　'ねじ締め速度設定
2009 '    Spd MSpdA               'ネジ締め時Spd個別設定
2010     ' 暫定移動のみ
2011     Mvs PScrewPosition(10)
2012 '    '
2013 '    Dly 0.1
2014 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
2015 '    Wait M_In(11584)=1          '完了/エラー検出
2016 '    Dly 0.1
2017 '    Spd M_NSpd
2018 '    '
2019 '    If M_In(X28_Driver)=1 Then  'ねじトータルエラー検出時
2020 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
2021 '        Dly 0.1
2022 '        M_Out(Y62_Driver)=0     'バンクセッティング解除　C1
2023 '        Dly 0.1
2024 '        M_Out(Y63_Driver)=0     'バンクセッティング解除　C1
2025 '        Dly 0.1
2026 '        M_Out(Y65_Driver)=0     'プログラム解除　F1
2027 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
2028 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
2029 '        MOKNGFlg = -1
2030 '        ScrewTight_S2 = 0
2031 '    Else
2032 '        Wait M_In(X29_Driver)=1 ' 正常完了時
2033 '        Dly 0.1
2034 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
2035 '        Dly 0.1
2036 '        M_Out(Y62_Driver)=0     'バンクセッティング解除
2037 '        Dly 0.1
2038 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
2039 '        Dly 0.1
2040 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
2041 '        ScrewTight_S2 = 1
2042 '    EndIf
2043 ' 暫定
2044     Ovrd 10
2045     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
2046     Ovrd 100
2047 FEnd
2048 '
2049 '■ScrewGet_S3
2050 ''' <summary>
2051 ''' ねじ供給機からねじを得る
2052 ''' </summary>
2053 '''<param name="%"></param>
2054 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
2055 '''         PScrewPos(2)    ：ねじ供給器回避点
2056 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2057 '''         PScrewPos(3)    ：Mねじポカヨケ位置
2058 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
2059 '''<returns>整数
2060 '''         0=異常終了、1=正常終了、-1=MネジセンサーNG、-2=MネジセンサーON、-3=吸着エラー
2061 '''</returns>
2062 ''' <remarks>
2063 ''' Date   : 2021/07/07 : M.Hayakawa
2064 ''' </remarks>'
2065 Function M% ScrewGet_S3(ByVal PScrewPosition())
2066     ScrewGet_S3 = 0
2067     MMScrewJudge% = 0
2068     'ねじ供給器初期動作エラーチェック
2069 ' ↓暫定削除
2070 '    Wait M_In(X34_ScrewReady1)=1 'ねじ供給器SがReadyになるまで待つ　←　何秒か待ってReadyにならなければ抜けるプログラムが必要？
2071 '    Ovrd 100
2072 '    If M_In(X33_SS2)=0 Then  'Mねじ検出センサがOFF（故障）していた場合
2073 '        Ovrd 30
2074 '        Mvs,-80             'その場所から80mm上空へ移動
2075 '        Mov PInitPos19049   '19049初期位置へ移動
2076 '        M_Out(Y6A_VV1)=0    'ねじ吸着 Off
2077 '        'NGとしてここの関数から抜ける
2078 '        ScrewGet_S3 = -1
2079 '        MMScrewJudge% = 1
2080 '        MCommentD1001 = 61
2081 '    EndIf
2082 '    If ScrewGet_S3 = 0 Then
2083 '        'Sタイト用ねじ供給機にMねじが混入していないか監視
2084 '        MMScrewJudge% = 0 'MMScrewJudgeを初期化する
2085 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
2086 '        If MRtn = 0 Then
2087 '            Ovrd 30
2088 '            Mvs,-80            'その場所から50mm上空へ移動
2089 '            Mov PInitPos19049  '19049初期位置へ移動
2090 '            MMScrewJudge% = 2
2091 '            MRtn = All_CLamp_Release()'全てのクランプ解除へ分岐
2092 '            MCnt% = 2   '2を設定
2093 '            MCommentD1001 = 62
2094 '        EndIf
2095 '        If MMScrewJudge% = 2 Then
2096 '            ScrewGet_S3 = -2
2097 '        EndIf
2098 '    EndIf
2099 '    'Mネジ判定がONの場合 NGとして関数を抜ける
2100 '    If MMScrewJudge% = 2 Then
2101 '        ScrewGet_S3 = -2
2102 '    EndIf
2103     'Sネジ用ねじ太郎のMネジ混入確認用ここまで
2104     Ovrd 100
2105     Spd M_NSpd
2106     If MMScrewJudge% = 0 Then
2107         ScrewGet_S3 = 0
2108         M_Out(Y63_Driver)=1         ' バンクセッティング　C2
2109         MScrewCnt% = 0
2110         MFinCnt% = 2
2111 '        For MCnt% = 0 To MFinCnt%
2112             Mov PScrewPosition(2)        ' ねじ供給機回避点
2113             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
2114             Ovrd 80
2115             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2116             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
2117             Mvs PScrewPosition(10), 1.2
2118             M_Out(Y6A_VV1)=1        ' ねじ吸着　ON
2119             'ビット回転
2120             M_Out(Y60_Driver)=1
2121             Dly 0.2
2122             '
2123             Ovrd 100
2124             JOvrd M_NJovrd
2125             Spd M_NSpd
2126             'ネジ吸着確認位置移動
2127             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
2128             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
2129             'ビット回転停止
2130             'M_Out(Y60_Driver)=0
2131             '
2132             '1秒間ネジ吸着確認
2133 ' 以下暫定削除
2134 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2135 '            'MRtn = 0'強制エラー
2136 '            '吸着エラーの場合
2137 '            'ネジをねじ太郎に戻す
2138 '            If MRtn = 0 Then
2139 '                Ovrd 30
2140 '                'ビット回転停止
2141 '                M_Out(Y60_Driver)=0
2142 '                'ネジ供給機上空
2143 '                Mvs PScrewPos(1)
2144 '                '更に上空
2145 '                Mov PScrewPos(1), -75
2146 '                'ネジ捨て位置
2147 '                Mov PScrewFeedS021
2148 '                '吸着OFF
2149 '                M_Out(Y6A_VV1)=0 'ねじ吸着　OFF
2150 '                Dly 0.2
2151 '                '破壊ON
2152 '                M_Out(Y6B_VB1)=1 '真空破壊ON
2153 '                'ビット回転
2154 '                M_Out(Y61_Driver)=1
2155 '                Dly 0.5
2156 '                '
2157 '                Ovrd 100
2158 '                JOvrd M_NJovrd
2159 '                Spd M_NSpd
2160 '                'ドライバーを上下させねじを振り落とす
2161 '                Mov PScrewFeedS021, 10
2162 '                Mov PScrewFeedS021
2163 '                Dly 0.1
2164 '                Mov PScrewFeedS021, 10
2165 '                Mov PScrewFeedS021
2166 '                '
2167 '                'ネジ落ち待ち
2168 '                'ビット回転停止
2169 '                M_Out(Y61_Driver)=0
2170 '                Dly 0.1
2171 '                '破壊OFF
2172 '                M_Out(Y6B_VB1)=0 '真空破壊OFF
2173 '                '
2174 '                '
2175 '                'ねじ落ちたとして、移動更に上空
2176 '                Mov PScrewPos(1), -75
2177 '                Ovrd 100
2178 '                Spd M_NSpd
2179 '                'ネジ供給機上空
2180 '                Mvs PScrewPos(1)
2181 '                '
2182 '                ScrewGet_S3 = -3
2183 '                Break
2184 '                '
2185 '            Else
2186 '                MCnt% = MFinCnt%
2187 '                ScrewGet_S3 = 0
2188 '            EndIf
2189 '        Next  MCnt%
2190         '
2191         Ovrd 100
2192         Spd M_NSpd
2193         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2194         M_Out(Y60_Driver)=0     ' ビット回転停止
2195         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
2196         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2197         'もう一度吸着確認
2198 ' 以下暫定削除
2199 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2200 '        If MRtn = 0 Then      '吸着エラーの場合
2201 '            MCommentD1001 = 94
2202 '            MCommentD1002 = 95
2203 '            ScrewGet_S3 = -3
2204 '        EndIf
2205 '        If MRtn = 1 Then      '吸着OKの場合
2206 '            ScrewGet_S3 = 1
2207 '        EndIf
2208 '        Break
2209     Else
2210         'Mネジ
2211         If MMScrewJudge% = 2 Then
2212             ScrewGet_S3 = -2
2213         EndIf
2214     EndIf
2215 FEnd
2216 '
2217 '■fnKEY_WAIT()
2218 ''' <summary>
2219 ''' GOTからのキー入力待ち
2220 ''' </summary>
2221 '''<returns>1：停止    2：次へ
2222 '''         3：継続    4：トルクチェック開始
2223 '''         5：NG
2224 '''         11：ロボット初期位置1    12：ロボット初期位置2
2225 '''         13：ロボット初期位置3    14：ロボット初期位置4
2226 '''</returns>
2227 ''' <remarks>
2228 ''' Date   : 2021/07/07 : M.Hayakawa
2229 ''' </remarks>'
2230 Function M% fnKEY_WAIT()
2231     fnKEY_WAIT = 0
2232     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
2233     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
2234     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
2235     '下記キー待ちの継続に反応させないため
2236     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
2237     Dly 0.2
2238     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
2239     MLocalLoopFlg=1
2240     While MLocalLoopFlg=1
2241         If M_In(11345) = 1 Then         '停止   M5345
2242             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
2243             fnKEY_WAIT = 1
2244             MLocalLoopFlg=-1
2245             Break
2246         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
2247             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
2248             fnKEY_WAIT = 2
2249             MLocalLoopFlg=-1
2250             Break
2251         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
2252             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
2253             fnKEY_WAIT = 3
2254             MLocalLoopFlg=-1
2255             Break
2256         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
2257             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
2258             fnKEY_WAIT = 4
2259             MLocalLoopFlg=-1
2260             Break
2261         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
2262             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
2263             fnKEY_WAIT = 5
2264             MLocalLoopFlg=-1
2265             Break
2266             '
2267         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
2268             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
2269             fnKEY_WAIT = MRobotInit1%
2270             MLocalLoopFlg=-1
2271             Break
2272             '
2273         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
2274             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
2275             fnKEY_WAIT = MRobotInit2%
2276             MLocalLoopFlg=-1
2277             Break
2278             '
2279         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
2280             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
2281             fnKEY_WAIT = MRobotInit3%
2282             MLocalLoopFlg=-1
2283             Break
2284             '
2285         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2286             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2287             fnKEY_WAIT = MRobotInit4%
2288             MLocalLoopFlg=-1
2289             Break
2290             '
2291         Else
2292         EndIf
2293     WEnd
2294     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2295     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2296 FEnd
2297 '
2298 '■ fnAUTO_CTL
2299 ''' <summary>
2300 ''' AUTOモードOFF、PLCからの開始待ち
2301 ''' </summary>
2302 ''' <remarks>
2303 ''' Date   : 2021/07/07 : M.Hayakawa
2304 ''' </remarks>
2305 Function M% fnAUTO_CTL
2306     fnAUTO_CTL = 0
2307     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2308     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2309     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2310     '
2311     If M_Svo=0 Then             'サーボON確認
2312         Servo On
2313     EndIf
2314     Wait M_Svo=1
2315 FEnd
2316 '
2317 '■ fnWindScreenOpen
2318 ''' <summary>
2319 ''' ウィンド画面の表示、非表示設定
2320 ''' </summary>
2321 '''<param name="%"></param>
2322 '''<param name="%"></param>
2323 '''<param name="%"></param>
2324 '''<param name="%"></param>
2325 ''' <remarks>
2326 ''' コメントD1001, D1002, D1003の設定
2327 ''' MWindReSet = 0     画面非表示
2328 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2329 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2330 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2331 ''' Date   : 2021/07/07 : M.Hayakawa
2332 ''' </remarks>
2333 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2334     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2335         M_Out16(12480) = MCommentD1001            'D1001 コメント
2336     EndIf
2337     '
2338     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2339         M_Out16(12496) = MCommentD1002            'D1002 コメント
2340     EndIf
2341     '
2342     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2343        M_Out16(12512) = MCommentD1003            'D1003 コメント
2344     EndIf
2345     '
2346     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2347     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2348     Dly 0.5
2349     M_Out(12363) = 0                         'ウィンド画面設定
2350 FEnd
2351 '
2352 '■FnCtlValue2
2353 ''' <summary>
2354 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2355 ''' </summary>
2356 ''' <param name="MCtlNo%"></param>
2357 ''' <remarks>
2358 ''' Date : 2022/04/28 渡辺
2359 ''' </remarks>
2360 '''
2361 '''  1：投入数       ＋１
2362 '''  2：組立ＯＫ数   ＋１
2363 '''  3：組立ＮＧ数   ＋１ (未使用)
2364 '''  4：吸着エラー数 ＋１
2365 ''' 99：読書開始信号 OFF
2366 '''
2367 Function M% FnCtlValue2(ByVal MCtlNo%)
2368     FnCtlValue2 = 1
2369     Select MCtlNo%
2370         Case 1        '投入数＋１
2371             M_Out(12569) = 0             '書込み開始信号OFF
2372             M_Out(12568) = 1             '読込み開始信号ON
2373             MInputQty = M_In16(11600)    '投入数受信
2374             MInputQty = MInputQty + 1    '投入数＋１
2375             M_Out16(12592) = MInputQty   '投入数送信
2376             M_Out(12569) = 1             '書込み開始信号ON
2377             Break
2378             '
2379         Case 2        '組立ＯＫ数＋１
2380             M_Out(12569) = 0             '書込み開始信号OFF
2381             M_Out(12568) = 1             '読込み開始信号ON
2382             MAssyOkQty = M_In16(11616)   '組立OK数受信
2383             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2384             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2385             M_Out(12569) = 1             '書込み開始信号ON
2386             Break
2387             '
2388         Case 4        '吸着エラー数＋１
2389             M_Out(12569) = 0                       '書込み開始信号OFF
2390             M_Out(12568) = 1                       '読込み開始信号ON
2391             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2392             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2393             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2394             M_Out(12569) = 1                       '書込み開始信号ON
2395             Break
2396             '
2397         Case 99        '読書開始信号OFF
2398             M_Out(12568) = 0        '読込み開始信号OFF
2399             M_Out(12569) = 0        '書込み開始信号OFF
2400             Break
2401             '
2402     End Select
2403     Exit Function
2404 FEnd
2405 'Insightによる画像処理検査実行（並列処理なし）
2406 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2407 '-------------------------------------------------------------------------------
2408 'Insightによる画像処理検査実行（並列処理なし）
2409 '   引数
2410 '       PInspPos()      ：検査位置
2411 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2412 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2413 '       MInspCnt%       ：検査位置数
2414 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2415 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2416 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2417 '   戻り値：整数
2418 '       0=異常終了、1=正常終了
2419 '
2420 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2421 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2422 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2423 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2424 '   20200410    :   検査グループ設定Retry追加
2425 '-------------------------------------------------------------------------------
2426     '----- 初期設定 -----
2427     Cnt 0                                                           '移動効率化解除(初期値=0)
2428     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2429 '    Cnt 1,0.1,0.1
2430     '変数宣言・初期化
2431     Def Inte MNum                                                   '検査番号(検査順1～)
2432     MNum% = 1                                                       '検査番号初期値設定
2433     Def Inte MEndFlg                                                '検査終了フラグ
2434     MEndFlg% = 0
2435     '
2436     '検査G番号設定要求・検査実行要求off
2437     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2438     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2439     'エラー番号クリア
2440     MInspErrNum = 0                                                 '検査実行エラー番号
2441     M_Out16(MOUT_InspErrNum) = MInspErrNum
2442     MInspNGStepNum = 0                                              '検査実行NGStep番号
2443     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2444     '
2445     'Insight Ready check?
2446     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2447         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2448         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2449         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2450         ISInspectionSingle = 0                                      '異常終了戻り値設定
2451         Exit Function
2452     EndIf
2453     '
2454     '検査位置数確認
2455     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2456         MInspErrNum = 21                                            '検査データなし 21　引数<1
2457         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2458         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2459         ISInspectionSingle = 0                                      '異常終了戻り値設定
2460         Exit Function
2461     EndIf
2462     '
2463     '
2464     '
2465     '----- メイン処理 -----
2466     '設定された検査位置数分の検査実行
2467     While( MEndFlg% = 0 )
2468         '----- 検査グループ番号設定Retry追加 20200410
2469         MSetGrNumRetryExitFlg = 0
2470         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2471         While( MSetGrNumRetryExitFlg = 0 )
2472         '----- 検査グループ番号設定Retry追加ここまで 20200410
2473             '
2474             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2475             '
2476             '----- 検査グループ番号設定 -----
2477             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2478             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2479             '
2480             '検査位置へ移動・移動完了待ち
2481             Mvs PInspPos( MNum% )                                       '移動
2482             Dly 0.05                                                    '移動完了後Delay
2483             '
2484             '検査グループ番号設定終了確認
2485             M_Timer(1) = 0
2486             MExitFlg = 0
2487             While( MExitFlg = 0 )
2488                 '検査G設定正常終了?
2489                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2490                     MExitFlg = 1
2491                 '
2492                 '検査G設定異常終了?
2493                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2494                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2495                     If MInspErrNum = 0 Then                             '1回目のエラー?
2496                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2497                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2498                     EndIf
2499                     MExitFlg = 1
2500                 '
2501                 'timeoutチェック
2502                 ElseIf 1000 < M_Timer(1) Then
2503                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2504                     If MInspErrNum = 0 Then                             '1回目のエラー?
2505                         MInspErrNum = 12                                'timeout エラー番号=12
2506                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2507                     EndIf
2508                     MExitFlg = 1
2509                 EndIf
2510             WEnd
2511             '
2512             '検査G番号設定要求off
2513             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2514             '
2515             '----- 検査グループ設定Retry追加 20200410
2516             'NGなければ抜ける
2517             If MCurrentStepErr = 0 Then
2518                 MSetGrNumRetryExitFlg = 1
2519             Else
2520                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2521                 If MSetGrNumRetryCnt = 0 Then
2522                     MSetGrNumRetryExitFlg = 1
2523                 Else
2524                     'Retryへ　その前にDelay
2525                     Dly 0.5
2526                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2527                 EndIf
2528             EndIf
2529             '----- 検査グループ設定Retry追加ここまで 20200410
2530             '
2531         WEnd
2532         '
2533         '
2534         '
2535         '----- 検査実行 -----
2536         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2537             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2538                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2539                 MInspRetryExitFlg = 0
2540                 MRetryCnt = 2                                        'Retry回数設定
2541                 While( MInspRetryExitFlg = 0 )
2542                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2543                     '
2544                     '検査完了確認
2545                     MRetryCnt = MRetryCnt - 1
2546                     M_Timer(1) = 0
2547                     MExitFlg = 0
2548                     While( MExitFlg = 0 )
2549                     '検査完了待ち
2550                         '検査OK終了?
2551                         If M_In( MIN_IS_InspOK% ) = 1  Then
2552                             MJudgeOKFlg = 1                         '検査OKフラグON
2553                             MExitFlg = 1
2554                         '
2555                         '検査NG終了?
2556                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2557                             If MInspErrNum = 0 Then                 '1回目のエラー?
2558                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2559                                     MInspErrNum = 32                    '検査NG エラー番号=32
2560                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2561                                 EndIf
2562                             EndIf
2563                             MExitFlg = 1
2564                         '
2565                         '検査異常終了(IS timeout)?
2566                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2567                             If MInspErrNum = 0 Then                 '1回目のエラー?
2568                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2569                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2570                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2571                                 EndIf
2572                             EndIf
2573                             MExitFlg = 1
2574                         '
2575                         'timeoutチェック
2576                         ElseIf 3000 < M_Timer(1) Then
2577                             If MInspErrNum = 0 Then                 '1回目のエラー?
2578                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2579                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2580                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2581                                 EndIf
2582                             EndIf
2583                             MExitFlg = 1
2584                         EndIf
2585                     WEnd
2586                     '
2587                     '検査開始要求off
2588                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2589                     '
2590                     'OKなら抜ける
2591                     If MJudgeOKFlg = 1 Then
2592                         MInspRetryExitFlg = 1
2593                     Else
2594                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2595                         If MRetryCnt = 0 Then
2596                             MInspRetryExitFlg = 1
2597                         Else
2598                             'Retryへ　その前にDelay
2599                             Dly 0.3
2600                         EndIf
2601                     EndIf
2602                     '
2603                 WEnd
2604             EndIf
2605         EndIf
2606         '
2607         '
2608         '
2609         MNum% = MNum% + 1                                           '検査Step+1
2610         '検査終了確認　検査終了フラグセット
2611         If (MInspCnt% < MNum% ) Then
2612             MEndFlg% = 1                                            '検査終了フラグセット
2613         EndIf
2614         'NG発生時続行時処理
2615         If MInspErrNum <> 0 Then                                    'NGあり?
2616             If MNgContinue% <> 1 Then                               'NG続行?
2617                 MEndFlg% = 1                                        '検査終了フラグセット
2618             EndIf
2619         EndIf
2620     WEnd
2621     '
2622     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2623     If 0 < MZAxis% Then
2624         PCurrentPos = P_Curr                                        '現在位置取得
2625         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2626         Mvs PCurrentPos                                             '現在位置上空へ移動
2627     EndIf
2628     Fine 0 , P
2629     '
2630     '戻り値設定
2631     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
2632         ISInspectionSingle = 1                                      '正常終了戻り値設定
2633     Else
2634         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2635         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2636         ISInspectionSingle = 0                                      '異常終了戻り値設定
2637     EndIf
2638     '
2639 FEnd
2640 '
2641 ' ■ISInspection
2642 ''' <summary>
2643 ''' Insightによる画像処理検査実行
2644 ''' </summary>
2645 '''<param name="PInspPos()">検査位置</param>
2646 '''<param name="MInspGrNum%()">検査位置での検査グループ番号（=0：画像検査未実施）</param>
2647 '''             PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2648 '''<param name="MInspCnt%">検査位置数</param>
2649 '''<param name="MZAxis%">終了時のZ軸退避座標（-1:無効）</param>
2650 '''             終了時にZ軸をMZAxisで設定された位置まで上昇させる
2651 '''<param name="MNgContinue%">=1で検査エラー・NG発生時に全Stepの検査を行う</param>
2652 '''<returns>    整数 0=異常終了、1=正常終了</returns>
2653 '''         MInspErrNum     ：異常終了時にエラー番号が設定される
2654 '''         MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される"
2655 ''' <remarks>
2656 ''' Date   : 2021/07/07 : M.Hayakawa
2657 ''' </remarks>
2658 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2659 '    '画像使用確認 0<- 画像確認無しの場合
2660 '    If M_In(11369) = 0 Then            'toRBT_使用確認
2661 '        ISInspection = 1                                        '正常終了戻り値設定
2662 '    EndIf
2663 ''
2664 '    Cnt 0                                                       '移動効率化解除(初期値=0)
2665 '    Fine 0.05,P                                                 '位置決め完了条件設置　0.05mm
2666 '    MNum% = 1                                                   '検査番号初期値設定
2667 '    Def Inte MEndFlg                                            '検査終了フラグ
2668 '    MEndFlg% = 0
2669 '    '
2670 '    'エラー番号クリア
2671 '    MInspErrNumSub = 0                                          '検査実行エラー番号sub
2672 '    MInspErrNum = 0                                             '検査実行エラー番号
2673 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2674 '    MInspNGStepNum = 0                                          '検査実行NGStep番号
2675 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2676 '    '
2677 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready offなら終了
2678 '        MInspErrNum = 20                                        '検査実行エラー番号 20 Insight offline
2679 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2680 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2681 '        ISInspection = 0                                        '異常終了戻り値設定
2682 ''
2683 '    EndIf
2684 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2685 '    '
2686 '    '検査位置数確認
2687 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2688 '        MInspErrNum = 21                                        '検査データなし 21　引数<1
2689 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2690 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2691 '        ISInspection = 0                                        '異常終了戻り値設定
2692 ''
2693 '    EndIf
2694 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2695 '    '
2696 '    '設定された検査位置数分の検査実行
2697 '    While( MEndFlg% = 0 )
2698 '        '検査終了確認　検査終了フラグセット
2699 '        If (MInspCnt% < MNum% ) Then
2700 '            MEndFlg% = 1                                        '検査終了フラグセット
2701 '        EndIf
2702 '        '
2703 '        'タスク　検査G番号設定・検査完了確認処理開始　INSPTAST1
2704 '        If MEndFlg% = 0 Then
2705 '            M_01# = MInspGrNum%(MNum%)                          '検査G番号引渡し
2706 '        EndIf
2707 '        M_02# = MEndFlg%                                        '検査終了フラグ引渡し
2708 '        M_05# = MNum%                                           '検査番号(検査順1～)
2709 '        'タスク　検査G設定フラグ引渡し
2710 '        If MEndFlg% = 0 Then
2711 '            If 0 < MInspGrNum%(MNum%) Then
2712 '                M_03# = 1
2713 '            Else
2714 '                M_03# = 0
2715 '            EndIf
2716 '        Else
2717 '            M_03# = 0
2718 '        EndIf
2719 '        'タスク　検査結果確認フラグ引渡し
2720 '        If 1 < MNum% Then
2721 '            If 0 < MInspGrNum%(MNum%-1) Then
2722 '                M_04# = 1
2723 '            Else
2724 '                M_04# = 0
2725 '            EndIf
2726 '        Else
2727 '            M_04# = 0
2728 '        EndIf
2729 '        '
2730 '        'タスク処理開始
2731 '        M_00# = 1                                               'TASK処理開始
2732 '        'タスク処理開始確認
2733 '        M_Timer(1) = 0
2734 '        MExitFlg = 0
2735 '        While( MExitFlg = 0 )
2736 '            '処理開始完了確認
2737 '            If M_00# = 0 And M_10# = 8 Then
2738 '                MExitFlg = 1
2739 '            EndIf
2740 '            'timeoutチェック
2741 '            If 2000 < M_Timer(1) Then
2742 '                If MNgContinue% = 1 Then                        'NG続行?
2743 '                    MInspErrNumSub = 36                         'エラー番号設定36
2744 '                Else
2745 '                    MInspErrNum = 36                            'エラー番号設定36
2746 '                EndIf
2747 '                MExitFlg = 1
2748 '            EndIf
2749 '        WEnd
2750 '        '
2751 '        '検査位置へ移動・移動完了待ち
2752 '        If 0 = MInspErrNum Then
2753 '            If MEndFlg% = 0 Then
2754 '                Mvs PInspPos( MNum% )                           '移動
2755 '            EndIf
2756 '        EndIf
2757 '        '
2758 '        'タスク　検査G番号設定・検査完了確認処理終了待ち　INSPTAST1
2759 '        If 0 = MInspErrNum Then
2760 '            M_Timer(1) = 0
2761 '            MExitFlg = 0
2762 '            While( MExitFlg = 0 )
2763 '                '処理完了待ち（正常終了）
2764 '                If M_10# = 1 Then
2765 '                    MExitFlg = 1
2766 '                EndIf
2767 '                '処理完了待ち（異常終了）
2768 '                If M_10# = 0 Then
2769 '                    If MNgContinue% = 1 Then                    'NG続行?
2770 '                        MInspErrNumSub = M_12#                  'エラー番号設定　M12
2771 '                    Else
2772 '                        MInspErrNum = M_12#                     'エラー番号設定　M12
2773 '                    EndIf
2774 '                    MExitFlg = 1
2775 '                EndIf
2776 '                'timeoutチェック
2777 '                If 5000 < M_Timer(1) Then
2778 '                    If MNgContinue% = 1 Then                    'NG続行?
2779 '                        MInspErrNumSub = 31                     'エラー番号設定31
2780 '                    Else
2781 '                        MInspErrNum = 31                        'エラー番号設定31
2782 '                    EndIf
2783 '                    MExitFlg = 1
2784 '                EndIf
2785 '            WEnd
2786 '        EndIf
2787 '        '
2788 '        '検査結果確認
2789 '        If 0 = MInspErrNum Then
2790 '            If 1 < MNum% Then
2791 '                If 0 < MInspGrNum%(MNum%-1) Then                '検査あり?
2792 '                    If M_11# = 2 Then                           '検査NG?
2793 '                        If MNgContinue% = 1 Then                'NG続行?
2794 '                            If MInspNGStepNum = 0 Then          'NG未発生?
2795 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2796 '                            EndIf
2797 '                            MInspErrNumSub = 32                 'エラー番号設定 32:検査NG
2798 '                        Else
2799 ''                            MInspNGStepNum = MNum% - 1          '検査実行NGStep番号設定
2800 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2801 '                            MInspErrNum = 32                    'エラー番号設定 32:検査NG
2802 '                        EndIf
2803 '                   EndIf
2804 '                EndIf
2805 '            EndIf
2806 '        EndIf
2807 '        '
2808 '        'エラーなら検査中断終了するのでLoopから抜けるため終了フラグセット
2809 '        If 0 <> MInspErrNum Then
2810 '            MEndFlg% = 1
2811 '        EndIf
2812 '        '
2813 '        '検査実行、取込完了待ち
2814 '        If 0 = MInspErrNum Then
2815 '            If MEndFlg% = 0 Then
2816 '                If 0 < MInspGrNum%(MNum%) Then                  '検査あり?
2817 '                    M_Out(MOUT_IS_Insp%) = 1                    '検査実行要求on
2818 '                    '取込完了確認
2819 '                    M_Timer(1) = 0
2820 '                    MExitFlg = 0
2821 '                    While( MExitFlg = 0 )
2822 '                        '処理完了待ち
2823 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2824 '                            MExitFlg = 1
2825 '                        EndIf
2826 '                        'timeoutチェック
2827 '                        If 2000 < M_Timer(1) Then
2828 '                            If MNgContinue% = 1 Then            'NG続行?
2829 '                                MInspErrNumSub = 33             'エラー番号設定33
2830 '                            Else
2831 '                                MInspErrNum = 33                'エラー番号設定33
2832 '                            EndIf
2833 '                            MExitFlg = 1
2834 '                        EndIf
2835 '                    WEnd
2836 '                EndIf
2837 '                '
2838 '            EndIf
2839 '        EndIf
2840 '        MNum% = MNum% + 1
2841 '    WEnd
2842 '    '
2843 '    '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2844 '    If 0 < MZAxis% Then
2845 '        PCurrentPos = P_Curr                                    '現在位置取得
2846 '        PCurrentPos.Z = MZAxis%                                 'Z軸を設定
2847 '        Mvs PCurrentPos                                         '現在位置上空へ移動
2848 '    EndIf
2849 '    '
2850 '    'NG続行時処理
2851 '    If MNgContinue% = 1 Then                                    'NG続行?
2852 '        MInspErrNum = MInspErrNumSub                            'エラー番号設定
2853 '    EndIf
2854 '    '
2855 '    '戻り値設定
2856 '    If MInspErrNum = 0 Then
2857 '        ISInspection = 1                                        '正常終了戻り値設定
2858 '    Else
2859 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2860 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2861 '        ISInspection = 0                                        '異常終了戻り値設定
2862 '    EndIf
2863 '    '
2864 '*ISInspection_End
2865 'FEnd
2866 '
2867 '■InitialZoneB
2868 ''' <summary>
2869 ''' 非常停止後の復帰動作
2870 ''' 1)上空退避　Z方向上に移動
2871 ''' 2)J1軸以外を退避ポジションへ移動
2872 ''' 3)J1軸のみを退避ポジションへ移動
2873 ''' 4)イニシャルポジションへ移動
2874 ''' </summary>
2875 ''' <remarks>
2876 ''' Date : 2022/04/08 : N.Watanabe
2877 ''' </remarks>
2878 Function V fnInitialZoneB()
2879     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/04/26 渡辺
2880 '
2881 'パラメータ
2882     Ovrd 5
2883 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2884 '    Cmp Pos, &B100011
2885 '
2886 '復帰動作開始
2887 '
2888 '置き台と両掴みの場所は、チャックを解放する
2889 *RecoveryChuckOpen
2890     PActive = P_Curr          '現在位置を取得
2891     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
2892 'PProductOnRoboSet(ねじロボ製品置き位置)は、チャック解放
2893     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2894         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2895             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2896                 MRecoveryChuckOpen = 1
2897             EndIf
2898         EndIf
2899     EndIf
2900 'PProductOnRoboGet(ねじロボ製品取り位置)は、チャック解放
2901     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2902         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2903             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2904                 MRecoveryChuckOpen = 1
2905             EndIf
2906         EndIf
2907     EndIf
2908 '
2909     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2910     M_Out(12256) = 0                           '本体チャック閉OFF
2911     M_Out(12257) = 1                           '本体チャック開ON
2912 '
2913     M_20# = 0                                  'KEY入力初期化
2914     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
2915     If MRtn = 1 Then M_Out(12257) = 0          '本体チャック開OFF
2916     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2917 '
2918     fErrorProcess(11,244,284,0)
2919     If M_20# = MNext% Then M_20# = MClear%
2920     If M_20# = MAbout% Then GoTo *RecoveryEnd
2921     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2922     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2923 '
2924     *RecoveryChuckOpenEnd
2925 '
2926 '背面板回避
2927 'PPlateBackSet～PPlateBackSet_6のエリアにいるときは、本体チャック開く
2928 '・PPlateBackSet_6         '経路6
2929 '・PPlateBackSet_5         '経路7
2930 '・PPlateBackSet_4         '経路8
2931 '・PPlateBackSet_3         '経路9
2932 '・PPlateBackSet_2         '経路10
2933 '・PPlateBackSet_1         '経路11
2934 '・PPlateBackSet           '背面板置き位置
2935 '上記７点のＸ座標・Ｙ座標・Ｚ座標とJ6軸が下記If文の範囲に入っている事を確認する事
2936     PActive = P_Curr                    '現在位置を取得
2937     JActive = J_Curr                    '現在位置を取得
2938     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2939     If (PActive.X >= -35) And (PActive.X <= -5) Then
2940         If (PActive.Y >= 340) And (PActive.Y <= 510) Then
2941             If (PActive.Z >= 470) And (PActive.Z <= 560) Then
2942                 If (MJ6 >= 160) And (MJ6 <= 200) Then
2943                     M_Out(12256) = 0            '本体チャック閉OFF
2944                     M_Out(12257) = 1            '本体チャック開ON
2945                 Dly 1.0
2946                 EndIf
2947             EndIf
2948         EndIf
2949     EndIf
2950 '
2951 '
2952 '特殊回避　直接、上空退避が出来ない所の対処
2953 '
2954     Ovrd 1
2955 'PProductOnRoboSet(Get)～PProductOnRoboSet(Get)_2のエリアにいるときは、PProductOnRoboSet_2へ
2956 '・PProductOnRoboSet
2957 '・PProductOnRoboSet_1
2958 '・PProductOnRoboSet_2
2959 '・PProductOnRoboGet
2960 '・PProductOnRoboGet_1
2961 '・PProductOnRoboGet_2
2962 '上記６点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
2963     PActive = P_Curr                    '現在位置を取得
2964     JActive = J_Curr                    '現在位置を取得
2965     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2966     If (PActive.X >= -35) And (PActive.X <= 0) Then
2967         If (PActive.Y >= 350) And (PActive.Y <= 420) Then
2968             If (PActive.Z >= 300) And (PActive.Z <= 450) Then
2969                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2970                     Mvs PProductOnRoboSet_1
2971                     Dly 1.0
2972                     Mvs PProductOnRoboSet_2
2973                     Dly 1.0
2974                     Mov PProductOnRoboSet_3
2975                     Dly 1.0
2976                 EndIf
2977             EndIf
2978         EndIf
2979     EndIf
2980 '
2981 'PProductOnRoboSet(Get)_2～PProductOnRoboSet(Get)_3のエリアにいるときは、PProductOnRoboSet_3へ
2982 '・PProductOnRoboSet_2
2983 '・PProductOnRoboSet_3
2984 '・PProductOnRoboGet_2
2985 '・PProductOnRoboGet_3
2986 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
2987     PActive = P_Curr                    '現在位置を取得
2988     JActive = J_Curr                    '現在位置を取得
2989     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2990     If (PActive.X >= -35) And (PActive.X <= 0) Then
2991         If (PActive.Y >= 280) And (PActive.Y <= 390) Then
2992             If (PActive.Z >= 410) And (PActive.Z <= 570) Then
2993                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2994                     Mvs PProductOnRoboSet_3
2995                     Dly 1.0
2996                 EndIf
2997             EndIf
2998         EndIf
2999     EndIf
3000 '
3001     Ovrd 5
3002 '
3003 '上空退避
3004     PActive = P_Curr
3005     Pmove = PActive
3006     Pmove.Z = 640           '上空退避する一律の高さ
3007     If PActive.X > 550 Then
3008         Pmove.Z =550        'パレット上に腕を伸ばしているときは640まで上げられない為、例外処置
3009     EndIf
3010     If PActive.Z < Pmove.Z Then
3011         Mvs Pmove
3012     EndIf
3013     Dly 1.0
3014 'J1軸以外を退避ポジションへ移動
3015     JActive = J_Curr
3016     Jmove = JTaihi
3017     Jmove.J1 = JActive.J1        'J1軸は現在値を使用し、JTaihiのポーズを取る
3018     Jmove.J6 = JActive.J6        'J6軸は現在値を使用し、JTaihiのポーズを取る
3019     Mov Jmove
3020     Dly 1.0
3021 'J1軸のみを退避ポジションへ移動
3022     Mov JTaihi
3023     Dly 1.0
3024 'イニシャルポジションへ移動
3025     Mov PInitialPosition
3026     Cmp Off
3027     Ovrd 100
3028 ' ねじロボを初期位置に戻すために強制的に自動運転開始
3029     If M_In(11856) = 0 Then                 ' 停止中のみ
3030         fnAutoScreenComment(501)            ' 状態表示[ネジ締め機自動運転開始中] 2022/04/25 渡辺
3031         M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
3032         MRet = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
3033         If MRet = 0 Then
3034         Else
3035             M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
3036         EndIf
3037     EndIf
3038     M_Out(12262) = 0            '位置決め出OFF
3039     M_Out(12263) = 1            '位置決め戻ON
3040     fErrorProcess(11,253,281,0)
3041 *RecoveryEnd
3042     Exit Function
3043 FEnd
3044 '
3045 '
3046 '■fnAutoScreenComment
3047 ''' <summary>
3048 ''' メイン画面の動作状況表示
3049 ''' コメントD1005の設定
3050 ''' </summary>
3051 '''<param name="McommentD1005%">コメントID</param>
3052 ''' <remarks>
3053 ''' Date   : 2021/07/07 : M.Hayakawa
3054 ''' </remarks>
3055 Function fnAutoScreenComment(ByVal McommentD1005%)
3056     M_Out16(12576) = McommentD1005%
3057 FEnd
3058 '
3059 '■fnRoboPosChk
3060 ''' <summary>
3061 ''' 最後に終了したロボットポジションの確認
3062 ''' </summary>
3063 '''<param name="MINNumber%">入力番号</param>
3064 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3065 '''<param name="MTimeCnt&">タイムアウト時間</param>
3066 ''' PLCに保続した番号を読込み、確認
3067 ''' MRBTOpeGroupNo = 5 が初期位置に設定
3068 '''<returns>整数 0:タイムアウト 1:OK</returns>
3069 ''' <remarks>
3070 ''' Date   : 2021/07/07 : M.Hayakawa
3071 ''' </remarks>
3072 Function M% fnRoboPosChk
3073     fnRoboPosChk = 0
3074     MRet = fnStepRead()
3075     '初期位置でないと判断した場合
3076     'ウィンド画面切換え
3077     If MRBTOpeGroupNo > 5 Then
3078         '下記キー待ちの継続に反応させないため
3079         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3080         Dly 0.2
3081         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3082         Dly 1.5
3083         '
3084         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
3085         '
3086         MLoopFlg% = 1
3087         While MLoopFlg% = 1
3088             '
3089             '
3090             MKeyNumber% = fnKEY_WAIT()
3091             Select MKeyNumber%
3092                 Case Is = MAbout%       '停止
3093                     M_20# = MAbout%
3094                     MLoopFlg% = -1
3095                     Break
3096                 Case Is = MNext%        '次へ
3097                     'MLoopFlg% = -1
3098                     Break
3099                 Case Is = MContinue%    '継続
3100                     M_20# = MContinue%
3101                     MLoopFlg% = -1
3102                     Break
3103                 Default
3104                     Break
3105             End Select
3106         WEnd
3107     EndIf
3108     '
3109     If M_20# = MContinue% Then                              '継続ボタンが押された場合
3110         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
3111         Ovrd 5                                   '低速オーバーライド値設定
3112         Select MRBTOpeGroupNo
3113             Case Is = 5                          '何もしない
3114                 Break
3115             Case Is = 10                         '初期位置へ戻す
3116                 'Mov PTEST001
3117                 Break
3118             Case Is = 15                         '初期位置へ戻す
3119                 'Mov PTEST002
3120                 Dly 0.5
3121                 'Mov PTEST001
3122                 Dly 0.5
3123                 Break
3124             Default
3125                 Break
3126         End Select
3127         '
3128         Ovrd M_NOvrd                            'システムの初期値を設定
3129         M_Out(12364) = 1                        'toPLC_データ保存ON
3130         MRBTOpeGroupNo = 5
3131         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
3132         Dly 1.0
3133         M_Out(12364) = 0                        'toPLC_データ保存OFF
3134         fnRoboPosChk = 1                        '初期位置動作実行
3135         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
3136     EndIf
3137     Exit Function
3138 FEnd
3139 '
3140 '■frInCheck
3141 ''' <summary>
3142 ''' センサーINチェック
3143 ''' </summary>
3144 '''<param name="MINNumber%">入力番号</param>
3145 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3146 '''<param name="MTimeCnt&">タイムアウト時間</param>
3147 '''<returns>整数 0:タイムアウト 1:OK</returns>
3148 ''' <remarks>
3149 ''' Date   : 2021/07/07 : M.Hayakawa
3150 ''' </remarks>
3151 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3152     M_Timer(4) = 0
3153     MloopFlg = 0
3154     While MloopFlg = 0
3155         MCrtTime& = M_Timer(4)
3156         If M_In(MINNumber%) = MCMPFLG% Then
3157             MloopFlg = 1
3158             frInCheck = 1
3159         ElseIf MCrtTime& > MTimeCnt& Then
3160             MloopFlg = 1
3161             frInCheck = 0
3162         EndIf
3163     WEnd
3164 FEnd
3165 '-----------------------------------------------
3166 '
3167 'ねじ締め機通信確認
3168 '
3169 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3170 'fScrewTcomChk = 0　：正常終了
3171 '          　 　 -1 ：異常終了
3172 '-----------------------------------------------
3173 Function M% fScrewTcomChk
3174 *ReCheckScewTcomChk
3175     fScrewTcomChk = 0
3176     '通信確認送信
3177     M_Out(MOUT_ScwT_ComChk%) = MOn%
3178     '通信確認受信待機
3179 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3180     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3181     '通信確認送信終了
3182     M_Out(MOUT_ScwT_ComChk%) = MOff%
3183     If MRtn = 0 Then
3184         fScrewTcomChk = -1
3185     EndIf
3186     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
3187  '
3188 FEnd
3189 '
3190 '
3191 '-----------------------------------------------
3192 '
3193 'ねじ締め開始送信
3194 '
3195 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3196 'fScrewTStart = 0　：正常終了
3197 '           　　-1 ：異常終了
3198 '-----------------------------------------------
3199 Function M% fScrewTStart
3200     fScrewTStart = 0
3201     nRet% = 0
3202     'ねじ締め開始待機を受信
3203 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3204     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3205     If MRtn = 0 Then nRet% = -1
3206     If MRtn = 0 Then GoTo *ScrewStartERROR      '開始できなかった場合ジャンプ
3207     Dly 0.1
3208     'ねじ締め開始受信を送信
3209     M_Out(MOUT_ScwT_ST%) = MOn%
3210     Dly 0.5
3211     'Wait M_In(MTEST_KEY%) = MOn%
3212     'ねじ締め開始送信終了
3213     M_Out(MOUT_ScwT_ST%) = MOff%
3214     '
3215 *ScrewStartERROR
3216     fScrewTStart = nRet%
3217 FEnd
3218 '
3219 '
3220 '
3221 '-----------------------------------------------
3222 '
3223 'ねじ締め完了受信
3224 '
3225 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3226 'fScewTFinish = 0　：正常終了
3227 '          　 　-1 ：異常終了
3228 '-----------------------------------------------
3229 Function M% fScewTFinish
3230 *ReCheckScewTFinish
3231     fScewTFinish = 0
3232     'ねじ締め完了待機を受信
3233 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3234     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3235     If MRtn = 0 Then
3236         fScewTFinish = -1
3237     EndIf
3238     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3239     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3240     Dly 0.1
3241     'ねじ締め完了受信を送信
3242     M_Out(MOUT_ScwT_FinOK%) = MOn%
3243     Dly 0.5                          'とりあえず保持時間0.5msec
3244     'ねじ締め開始送信終了
3245     M_Out(MOUT_ScwT_FinOK%) = MOff%
3246     'Wait M_In(MTEST_KEY%) = MOn%
3247     '
3248 *ScewTFinish_ErrEnd
3249 FEnd
3250 '
3251 '
3252 '-----------------------------------------------
3253 '
3254 '条件xx停止受信
3255 '
3256 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3257 'fScewTCaseStop = 0　：正常終了
3258 '          　   　-1 ：異常終了
3259 '-----------------------------------------------
3260 Function M% fScewTCaseStop(ByVal MCase%())
3261 *ReCheckScewTCaseStop
3262     fScewTCaseStop = 0
3263     '条件xx停止を受信
3264     Wait M_In(MCase%(1)) = MOn%
3265     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3266     If MRtn = 0 Then
3267         fScewTCaseStop = -1
3268     EndIf
3269     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3270     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3271     Dly 0.1
3272     '条件xx停止受信を送信
3273     M_Out(MCase%(2)) = MOn%
3274     Dly 0.5                          'とりあえず保持時間0.5msec
3275     'ねじ締め開始送信終了
3276     M_Out(MCase%(2)) = MOff%
3277 *ScewTCaseStop_ErrEnd
3278     '
3279 FEnd
3280 '
3281 '■fScrewTighenRoboCheck
3282 '<summary>
3283 'ねじロボ監視
3284 '</summary>
3285 '<param name = "MStopNum%"> 停止番号</param>
3286 '<returns>整数 0:ねじロボ異常終了 1:OK </returns>
3287 '<make>
3288 '2021/12/2 中村天哉
3289 '</make>
3290 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3291     fnAutoScreenComment(503)    '状態表示[ねじロボ動作終了待ち] 2022/04/26 渡辺
3292     fScrewTighenRoboCheck = 1
3293     MScrewTighenRoboFlg% = 1    'フラグの初期化
3294     MCheck% = 0
3295     While MScrewTighenRoboFlg% = 1
3296         MCheck% = M_In16(11904)
3297         If M_In(MStopNum%) = 1 Then '停止位置まで来たら
3298             MScrewTighenRoboFlg% = 0 '関数を抜ける
3299             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
3300         EndIf
3301         If MCheck% <> 0 Then
3302             fScrewTighenRoboError(MCheck%)
3303             Select M_20#
3304                 Case MAbout%            '停止が押された場合
3305                     M_Out(12869) = 1 Dly 1.0
3306                     MScrewTighenRoboFlg% = 0
3307                     fScrewTighenRoboCheck = 0   '異常終了
3308                     Break
3309                 Case MNgProcess%        'NGが押された場合
3310                     M_Out(12873) = 1 Dly 1.0
3311                     MScrewTighenRoboFlg% = 0
3312                     fScrewTighenRoboCheck = 0   '異常終了
3313                     Break
3314                 Case MContinue%             'リトライが押された場合
3315                     M_20# = MClear%         'M_20#初期化
3316                     M_Out(12871) = 1 Dly 1.0
3317                     Break
3318                 Case MNext%                 '次へが押された場合
3319                     M_20# = MClear%         'M_20#初期化
3320                     M_Out(12874) = 1 Dly 1.0
3321                     Break
3322             End Select
3323             Dly 0.5
3324         EndIf
3325     WEnd
3326 FEnd
3327 '
3328 '■fScrewTighenRoboError
3329 '<summary>
3330 'ねじロボエラー処理
3331 '</summary>
3332 '<param name = "ErrorCode%"> エラー番号</param>
3333 '<make>
3334 '2021/12/2 中村天哉
3335 '</make>
3336 Function fScrewTighenRoboError(ByVal MErrorCode%)
3337     MErrorScreenCode% = 0
3338     MErrorScreenCode% = MErrorCode% + 300
3339     fErrorProcess(11,MErrorScreenCode%,0,0)
3340 FEnd
3341 '
3342 '■fErrorProcess
3343 '<summary>
3344 'エラー処理
3345 '</summary>
3346 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
3347 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
3348 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
3349 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
3350 '<make>
3351 '2021/11/5 中村天哉
3352 '</make>
3353 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3354     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
3355     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
3356     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
3357     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
3358 *RETRY_ERR_PROCESS
3359      M_20# = MClear%     '初期化
3360 '        'エラー処理記述
3361         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3362 '        'GOT KEY入力待ち
3363         MKeyNumber = fnKEY_WAIT()
3364 '        '
3365         If MKeyNumber = MAbout% Then   '停止を選択した場合
3366             M_20# = MAbout%            'M_20# プログラム間共通外部変数
3367             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3368             Break
3369          '
3370         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
3371             M_20# = MContinue%            'M_20# プログラム間共通外部変数
3372             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3373         '
3374         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
3375             M_20# = MNext%            'M_20# プログラム間共通外部変数
3376             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3377          '
3378         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
3379             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
3380             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3381             Break
3382         '
3383         EndIf
3384         '
3385         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3386 FEnd
3387 '
3388 '■fnTorqueCheck
3389 ''' <summary>
3390 ''' トルクチェック動作用のメイン
3391 ''' </summary>
3392 ''' <remarks>
3393 ''' Date   : 2021/12/21 : H.AJI
3394 ''' </remarks>'
3395 Function M% fnTorqueCheck
3396     'トルクチェック中送信  搬送系停止
3397     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
3398     '
3399     fnTorqueCheck = 0
3400     Ovrd 20
3401     Mov PInitialPosition              '初期位置移動
3402     Ovrd 100
3403     '下記キー待ちの継続に反応させないため
3404     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3405     Dly 0.2
3406     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3407     '
3408     'M6340  トルクチェック受信
3409     'Dly 5.0
3410     M_Out(12340) = 1          'トルクチェック受信 M6340
3411     Dly 1.0
3412     M_Out(12340) = 0
3413     '
3414     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3415     M_Out(12835) = 1                         'ねじロボトルクチェック画面切替
3416    Wait M_In(11843) = 1                         'ねじロボトルクチェック画面切替
3417     M_Out(12835) = 0                         'ねじロボトルクチェック画面切替完了
3418     '
3419     '
3420     MLoopFlg = 1
3421     While MLoopFlg = 1
3422         '
3423         Mov PInitialPosition              '初期位置移動
3424         '
3425         MKeyNumber = fnKEY_WAIT()
3426         Select MKeyNumber
3427             Case Is = 1           '停止
3428                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3429                 Dly 1.0
3430                 M_Out(12343) = 0
3431                 Ovrd 20
3432                 'Mov PTicketRead_1
3433                 M_Out(12840) = 1          'トルクチェック終了
3434                 Wait M_In(11859) = 1      'ねじロボからの終了
3435                 M_Out(12840) = 0          'トルクチェック終了
3436                 Ovrd 100
3437                 M_20# = 1
3438                 MLoopFlg = -1
3439                 Break
3440             Case Is = 2           '次へ
3441                 Break
3442             Case Is = 3           '継続
3443                 Break
3444             Case Is = 4           'トルクチェック開始
3445                 M_Out(12342) = 1          'トルクチェック開始要求受信 M6342
3446                 Dly 1.0
3447                 M_Out(12342) = 0
3448                 If M_In(11862) = 1 Then             'トルクチェッカー確認
3449                     fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3450                     MRet = fnScrewMTorque()           'ねじロボ用トルクチェック
3451                 EndIf
3452                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3453                 'MRet = fnMoveTorquePosi()
3454                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3455                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3456                 Break
3457             Default
3458                 Break
3459         End Select
3460     WEnd
3461     '
3462     'トルクチェック中停止送信
3463     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3464     '
3465     'ロボットの位置を元に戻す
3466     '
3467     '
3468  FEnd
3469  '
3470 '
3471 '
3472 '---------------------------
3473 '
3474 '    メイン画面の表示、非表示設定
3475 '         コメントD1001, D1002, D1003の設定
3476 '           MWindReSet = 0     画面非表示
3477 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3478 '           MWindErrScr = 10    エラー画面 D1001, D1002
3479 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3480 '
3481 '---------------------------
3482 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3483     fnMainScreenOpen = 0
3484     '
3485    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3486         M_Out16(12480) = MCommentD1001            'D1001 コメント
3487     EndIf
3488     '
3489     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3490         M_Out16(12496) = MCommentD1002            'D1002 コメント
3491     EndIf
3492     '
3493     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3494         M_Out16(12512) = MCommentD1003            'D1003 コメント
3495     EndIf
3496     '
3497     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3498     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3499     Dly 0.5
3500     M_Out(12362) = 0                         'ウィンド画面設定
3501 FEnd
3502 '
3503 '■Main
3504 ''' <summary>
3505 ''' トルクチェック実動作
3506 ''' </summary>
3507 ''' <remarks>
3508 ''' Date   : 2021/12/21 : H.AJI
3509 ''' </remarks>'
3510 Function M% fnScrewMTorque
3511     fnScrewMTorque = 0
3512     M_Out(12838) = 1                         'トルクチェック開始1
3513     Wait M_In(11857) = 1                     '受信完了
3514     M_Out(12838) = 0                         'トルクチェック開始1
3515     Dly 2.0
3516 FEnd
3517 '
3518 '----------------------------------------------------------------
3519 'fTimeOutJudge
3520 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3521 '引数
3522 'Address% = 監視アドレス番号
3523 'JudgeFlg% = 対象アドレスの正常終了時の値
3524 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3525 '戻り値 = 0 エラー
3526 '         1 正常終了
3527 '         2 リトライ
3528 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3529 '作成日
3530 '2022/9/20 中村
3531 '----------------------------------------------------------------
3532 '
3533 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3534     fTimeOutJudge = 0
3535     MJudge% = 1
3536     MRtn = 0
3537     M_20# = MClear%
3538     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3539 *TimeOutLoop
3540     If MRtn = 1 Then GoTo *TimeOut
3541         fErrorProcess(11,202,203,0)
3542         If M_20# = MNext% Then GoTo *TimeOutLoop
3543         If M_20# = MContinue% Then MJudge% = 2
3544         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3545 *TimeOut
3546     fTimeOutJudge = MJudge%
3547 '
3548 *JUDGE_ERROR_END
3549 FEnd
3550 '■Main
3551 ''' <summary>
3552 ''' 組立動作用のメイン
3553 ''' </summary>
3554 ''' <remarks>
3555 ''' Date   : 2021/07/07 : M.Hayakawa
3556 ''' </remarks>'
3557 Function Main
3558     MopeNo = M_21#         '外部変数にて動作番号代入
3559     '
3560     If M_Svo=0 Then
3561         Servo On
3562     EndIf
3563     Wait M_Svo=1
3564 '組立スタート日付時刻要求パルスON
3565     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3566 'パトライト操作
3567     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3568     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3569     '
3570     M_20# = 0                                   'KEY入力初期化
3571     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
3572     MRet% = 0
3573 '初期位置の確認と移動
3574 '
3575 '復帰動作　実行・未実行判別      2022/04/08 渡辺 作成
3576     PActive = P_Curr                    '現在位置を取得
3577     MRecoveryPass% = 0
3578     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3579         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3580             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3581                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3582             EndIf
3583         EndIf
3584     EndIf
3585     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3586         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3587             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3588                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3589             EndIf
3590         EndIf
3591     EndIf
3592     If MRecoveryPass% = 0 Then
3593        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3594     EndIf
3595 '
3596 '
3597 '    MRet% = fnRoboPosChk()
3598     If MRet% = 1 Then                           '初期位置の動作を行った場合
3599         fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  'ウィンド画面
3600         MKeyNumber% = fnKEY_WAIT()
3601         Select MKeyNumber%
3602             Case Is = MAbout%       '停止
3603                 M_20# = MAbout%
3604                 MLoopFlg% = -1
3605                 Break
3606             Case Is = MNext%        '次へ
3607                 'MLoopFlg = -1
3608                 Break
3609             Case Is = MContinue%    '継続
3610                 M_20# = MContinue%
3611                 MLoopFlg% = -1
3612                 Break
3613             Default
3614                 Break
3615         End Select
3616     EndIf
3617     '
3618     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3619         M_Out(12364) = 1            'toPLC_データ保存ON
3620 'トルクチェック
3621         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3622             MRet% = fnTorqueCheck()
3623             Break
3624         Else
3625 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
3626 '                MRtn = InspInit()               '画像処理初期化処理
3627 '            EndIf
3628 '
3629             M_20# = MClear%             '初期化
3630 '組立開始
3631             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3632                 fnAssyStart()
3633             Else
3634                 M_20# = MPass%
3635             EndIf
3636 '組立終了日付時刻
3637             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3638             Wait M_In(11572) = 1            '日付取得完了
3639             Dly 0.1
3640             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3641 'リフターユニットへのOUT
3642             '  KEY入力が何もない場合 OKと判断
3643             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3644             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3645 'OK/NGフラグ出力
3646             If M_20# <= 0 Then
3647                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3648             ElseIf M_20# = MPass% Then
3649                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3650             EndIf
3651 'PIASに組立完了書込み
3652             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3653                 If M_20# = MPass% Then
3654                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3655                 Else
3656                     'KEY入力がNGの場合
3657                     If M_20# = MNgProcess% Then
3658                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3659                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3660                         MRet% = fnPiasWrite(MNG%)
3661                        nAssyNgQty = nAssyNgQty + 1
3662                     EndIf
3663                     '
3664                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3665                     If M_20# = MAssyOK% Then
3666                             '-----------------------
3667                             'D732 -> D2600 コピー要求
3668                             M_Out(12566) = 1
3669 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3670                             M_Out(12566) = 0
3671                             '
3672                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3673                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3674                             '基板番号照合(PPは未使用）
3675 '                            MRet% = fnPCBNumberCheck()
3676                         Else
3677                             MRet% = 1
3678                         EndIf
3679                         '
3680                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3681                             If M_20# <> MAbout% Then
3682                                 '工程履歴OK書き込み
3683                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3684                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3685                                 MRet% = fnPiasWrite(MOK%)
3686                                 nAssyOkQty = 0
3687                                 nAssyOkQty = nAssyOkQty + 1
3688                             Else
3689                                 nAssyOkQty = nAssyOkQty + 1
3690                             EndIf
3691                         EndIf
3692                     EndIf
3693 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3694 '                    MRet% = fnPiasWrite(MOK%)
3695                 EndIf
3696             Else
3697                 nAssyOkQty = nAssyOkQty + 1
3698             EndIf
3699             '
3700             '組立終了日付時刻解除
3701             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3702             '投入数、組立OK数、組立NG数書込み
3703 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3704             '
3705 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3706 '                '画像処理終了処理
3707 '                MRtn = InspQuit()
3708 '            EndIf
3709         EndIf
3710         M_Out(12364) = 0                          'toPLC_データ保存OFF
3711     EndIf
3712 'パトライト操作
3713     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3714     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3715 'GOT表示
3716     fnAutoScreenComment(93)  'AUTO画面 工程完了
3717 FEnd
3718 End
3719 '
3720 'おまじないコメント
3721 '絶対削除するな
3722 '
3723 '
3724 '
3725 '
3726 '
3727 '
3728 '
PInspPosition(1)=(+602.00,-150.00,+500.00,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
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
PTemp=(+340.00,+0.03,+579.98,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PScrewPos(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
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
PActive=(+340.00,+0.03,+579.98,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
Pmove=(-18.02,+402.01,+640.00,-179.10,-27.37,+88.86,+0.00,+0.00)(7,1048576)
PInitialPosition=(+340.00,+0.00,+580.00,-180.00,+0.00,+180.00)(7,0)
PMechaGet=(-415.66,-8.74,+299.32,+179.45,-2.22,+176.86)(7,1048577)
PMechaGet_1=(-415.66,-8.74,+409.96,+179.45,-2.22,+176.86)(7,1048577)
PMechaGet_2=(-189.84,-0.01,+629.06,-180.00,+0.00,-179.99)(7,1)
PMechaGet_3=(+0.01,+189.84,+629.07,-180.00,+0.00,+90.00)(7,0)
PMechaGet_4=(+327.50,+0.02,+596.24,-179.99,+0.00,+103.50)(7,0)
PMechaGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaSet1=(+159.80,-334.91,+320.00,+122.02,+86.64,+31.39)(6,0)
PMechaSet1_1=(+159.80,-334.91,+340.00,+122.02,+86.64,+31.39)(6,0)
PMechaSet2=(+160.19,-334.97,+320.18,+122.02,+86.64,+31.36)(6,0)
PMechaSet2_1=(+160.19,-334.97,+339.97,+122.02,+86.64,+31.36)(6,0)
PMechaSet_2=(+162.58,-305.37,+557.38,+179.47,+90.00,+89.47)(6,0)
PMechaSet_3=(+114.45,-288.22,+565.58,+180.00,+0.00,+112.11)(7,0)
PMechaSet_4=(+310.11,-0.04,+565.56,+180.00,+0.00,-179.55)(7,0)
PMechaSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck=(-90.21,+513.03,+577.72,-180.00,+0.00,-90.00)(7,0)
PPlateBackCheck_2=(+66.39,+429.86,+577.75,+180.00,+0.00,-90.00)(7,0)
PPlateBackCheck_3=(-18.78,+286.22,+630.88,+180.00,+0.00,-90.00)(7,0)
PPlateBackCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet=(+477.48,+103.78,+401.13,+179.78,+0.01,+178.57)(7,0)
PPlateBackGet_1=(+477.48,+103.78,+430.00,+179.78,+0.01,+178.57)(7,0)
PPlateBackGet_2=(+477.48,+103.78,+560.00,+179.78,+0.01,+178.57)(7,0)
PPlateBackGet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackSet=(-18.00,+492.33,+543.82,-179.86,-0.33,+89.00)(7,1048576)
PPlateBackSet_1=(-18.00,+446.52,+535.30,-179.37,-13.97,+88.96)(7,1048576)
PPlateBackSet_10=(-18.77,+363.66,+487.69,-179.91,-41.20,+90.53)(7,1048576)
PPlateBackSet_11=(-18.77,+362.28,+493.38,+179.84,-39.34,+90.58)(7,1048576)
PPlateBackSet_12=(-17.62,+286.22,+630.90,-179.82,-0.29,+90.49)(7,1048576)
PPlateBackSet_2=(-18.00,+402.05,+517.82,-179.10,-27.37,+88.87)(7,1048576)
PPlateBackSet_3=(-18.00,+376.88,+500.39,-178.89,-36.13,+88.75)(7,1048576)
PPlateBackSet_4=(-18.83,+358.54,+485.67,-179.10,-42.67,+88.56)(7,1048576)
PPlateBackSet_5=(-18.90,+355.89,+483.22,-179.10,-42.67,+88.56)(7,1048576)
PPlateBackSet_6=(-18.90,+352.51,+487.90,-179.10,-42.67,+88.56)(7,1048576)
PPlateBackSet_7=(-18.83,+378.77,+503.51,+179.34,-35.63,+90.89)(7,1048576)
PPlateBackSet_8=(-18.81,+374.94,+499.62,+179.31,-36.77,+90.90)(7,1048576)
PPlateBackSet_9=(-18.81,+372.36,+496.87,+179.85,-37.92,+90.58)(7,1048576)
PProductOnPltGet=(+479.30,-99.28,+372.11,+179.79,-0.35,+178.62)(7,0)
PProductOnPltGet_1=(+479.30,-99.28,+410.00,+179.79,-0.35,+178.62)(7,0)
PProductOnPltGet_2=(+479.30,-99.28,+500.00,+179.79,-0.35,+178.62)(7,0)
PProductOnPltGet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet=(+478.90,-98.88,+372.11,+179.79,-0.35,+178.62)(7,0)
PProductOnPltSet_1=(+478.90,-98.88,+410.00,+179.79,-0.35,+178.62)(7,0)
PProductOnPltSet_2=(+478.90,-98.88,+500.00,+179.79,-0.35,+178.62)(7,0)
PProductOnPltSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboGet=(-18.40,+403.83,+318.61,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboGet_1=(-18.40,+403.83,+425.20,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboGet_2=(-18.40,+370.97,+425.20,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboGet_3=(-18.40,+300.00,+550.00,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboGet_4=(-18.40,+300.00,+550.00,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboSet=(-18.40,+403.83,+318.61,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboSet_1=(-18.40,+403.83,+425.20,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboSet_2=(-18.40,+370.97,+425.20,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboSet_3=(-18.40,+300.00,+550.00,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboSet_4=(-18.86,+404.69,+360.00,-102.74,+89.08,-12.36)(6,0)
PProductOnRoboSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPushTilt=(-213.31,+564.75,+464.13,+179.96,-0.02,+1.51)(7,0)
PPushTilt_1=(-213.31,+564.75,+480.78,+179.96,-0.02,+1.51)(7,0)
PPushTilt_2=(-213.31,+564.75,+620.00,+179.96,-0.02,+1.51)(7,0)
PPushTilt_3=(+0.02,+340.00,+610.00,-180.00,-0.01,-91.91)(7,0)
PTicketRead=(+602.00,-150.00,+500.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_1=(+602.00,-150.00,+550.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
JActive=(+92.75,-1.56,+103.00,-1.14,+51.19,+184.17,+0.00,+0.00)
Jmove=(+92.75,-46.87,+111.64,+0.00,+80.58,+184.17,+0.00,+0.00)
JTaihi=(+0.00,-46.87,+111.64,+0.00,+80.58,+0.00,+0.00,+0.00)
