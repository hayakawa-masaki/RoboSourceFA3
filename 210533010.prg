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
892     Mov PPlateBackSet_13        '背面板置き上空
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
918     Mov PPlateBackSet_12        '爪入れ前回避点
919     Cnt 0
920     Ovrd 25
921     Accel 25 , 25
922     Mvs PPlateBackSet_11        '爪入れ込み前
923     Mvs PPlateBackSet_10        '爪入れ込み1
924     Mvs PPlateBackSet_9         '爪入れ込み2
925 '    CmpG 1.0,1.0,1.0,1.0,1.0,1.0,,
926 '    Cmp Pos, &B001000
927     Cnt 1           'コメントアウト
928     Mov PPlateBackSet_8         '経路1
929     Mov PPlateBackSet_7         '経路2
930     Mov PPlateBackSet_6         '経路3
931     Mov PPlateBackSet_5         '経路4
932     Mov PPlateBackSet_4         '経路5
933     Mov PPlateBackSet_3         '経路6
934     Mov PPlateBackSet_2         '経路7
935     Mov PPlateBackSet_1         '経路8
936     Mov PPlateBackSet           '背面板離し位置
937 '    Cmp Off
938     Accel 100 , 100
939     Cnt 0
940     Dly 0.1
941 *RE_PLATE_SET
942     M_Out(12256) = 0            '本体チャック閉OFF
943     M_Out(12257) = 1            '本体チャック開ON
944     '
945 '    Wait M_In(11265)            '本体チャック開検出
946     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
947     If MRtn = 1 Then GoTo *CompPlateSet
948     fErrorProcess(11,244,284,0)
949     If M_20# = MNext% Then M_20# = MClear%
950     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
951     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
952     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
953     *CompPlateSet
954 '-----暫定押し-------------------------------------(22/12/14中村)ここから
955 *RE_BUCK_PUSH
956     M_20# = MClear%
957     Mov PPlateBackPush_2
958 '
959     M_Out(12257) = 0            '本体チャック開OFF
960     M_Out(12256) = 1            '本体チャック閉ON
961 '
962     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
963 '
964     If MRtn = 1 Then GoTo *CompBuckPushSetting  '正常なら押し動作へ
965 '
966     fErrorProcess(11,245,287,0) '閉端センサーNG時エラー表示
967         If M_20# = MNext% Then M_20# = MClear%              '次へが押されたら値を初期化
968         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '停止が押されたらエラーエンドへ
969         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END    'NGが押されたらエラーエンドへ
970         If M_20# = MContinue% Then GoTo *RE_BUCK_PUSH       'リトライが押されたらもう一度閉じる
971 '
972 *CompBuckPushSetting
973 '
974     Mvs PPlateBackPush_1
975     Ovrd 10
976     Mvs PPlateBackPush
977 '    Dly 0.1     'クランプをするので削除(221219中村)
978 '背面クランプここから(12/15)
979     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～停止4)
980     MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
981         If MRtn = 0 Then
982             Mvs PPlateBackPush_1
983             Mov PPlateBackSet_13
984             Mov PInitialPosition    '"イニシャルに戻る動き"
985         EndIf
986         If MRtn = 0 Then GoTo *ASSY_ERROR_END
987 '背面クランプここまで(12/15)
988     Ovrd 50                     '20→50に変更(221219中村)
989     Mvs PPlateBackPush_1
990 *RE_CHUCK_OPEN
991     M_20# = MClear%
992     M_Out(12256) = 0            '本体チャック閉OFF
993     M_Out(12257) = 1            '本体チャック開ON
994     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
995     If MRtn = 1 Then GoTo *CompChuckOpenForBackPush
996     fErrorProcess(11,244,284,0)
997         If M_20# = MNext% Then M_20# = MClear%              '次へが押されたら値を初期化
998         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '停止が押されたらエラーエンドへ
999         If M_20# = MNgProcess% Then GoTo *RE_CHUCK_OPEN    'NGが押されたらエラーエンドへ
1000         If M_20# = MContinue% Then GoTo *RE_CHUCK_OPEN       'リトライが押されたらもう一度閉じる
1001 *CompChuckOpenForBackPush
1002 '-----暫定押し-------------------------------------(22/12/14中村)ここまで
1003 '
1004     ColChk On
1005     Mov PPlateBackSet_13        '背面板置き上空
1006     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～停止4)
1007     Ovrd 100
1008     '
1009 ''    ' 部品供給要求送信(処理位置変更2/27中村)
1010 '    M_Out(12787) = 1
1011     'ねじロボ製品クランプ固定待ち
1012 '    Wait M_In(11891) = 1        'ねじロボ2停止4受信
1013 MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
1014 If MRtn = 0 Then Mov PInitialPosition    '"イニシャルに戻る動き"
1015 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1016     '置き位置画像検査
1017 '    Mov PPlateBackCheck_3       '画像検査位置上空
1018 '    Mov PPlateBackCheck_2       '通過点
1019 '    Mvs PPlateBackCheck         '確認位置
1020     '
1021     'PInspPosition(2) = PPlateBackCheck
1022     'MInspGroup(2) = 2
1023     'MRtn = ISInspection(PInspPosition,MInspGroup,2,-1,1)
1024     'If MRtn <> 1 Then
1025     '   'エラー処理
1026     'EndIf
1027     '
1028 ''    ' 部品供給要求送信
1029 '    M_Out(12787) = 1    '処理位置変更
1030 '    Mov PPlateBackCheck_3       '画像検査位置上空
1031     '
1032     'ねじロボ引き込み待ち
1033     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～完了)
1034     '
1035     'DVDメカを取る
1036     M_Out(12258) = 0            'DVDメカチャック閉OFF
1037     M_Out(12259) = 1            'DVDメカチャック開ON
1038     '
1039     Mov PMechaGet_3             '経路1
1040     Mov PMechaGet_2             'DVDメカ受け取り上空回避点
1041 '    Wait M_In(11272)            '部品供給機Ready
1042 '    MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '部品供給機Ready
1043 '    If MRtn = 0 Then
1044 '        fErrorProcess()         'エラー処理
1045 '    EndIf
1046 '
1047 '    ' 部品供給要求送信(処理位置変更)
1048     fnAutoScreenComment(513)    '状態表示[部品供給待ち] 2022/04/26 渡辺
1049 '    M_Out(12787) = 1
1050     '    ' 部品供給完了待ち(処理変更2/27中村)
1051 *RE_FEEDER_READY
1052 '    Wait M_In(11810) = 1
1053 MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '供給待ち
1054 If MRtn = 1 Then GoTo *CompFeederReady
1055 '   ' 部品供給要求終了
1056 M_Out(12787) = 0
1057 fErrorProcess(11,289,290,0)                '284→290に変更6/2中村
1058 If M_20# = MNext% Then M_20# = MClear%
1059 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1060     Mov PMechaGet_2
1061     Mov PMechaGet_3
1062     Mov PMechaGet_4
1063     Mov PInitialPosition
1064 EndIf
1065 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1066 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1067     ' 部品供給要求
1068 M_Out(12787) = 1
1069 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
1070 *CompFeederReady
1071 '    ' 部品供給要求終了
1072     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1073     M_Out(12787) = 0
1074 '
1075     Mov PMechaGet_1             'DVDメカ受け取り上空
1076     '
1077     *RE_MECHA_GET_1
1078     '
1079     M_Out(12258) = 0            'DVDメカチャック閉OFF
1080     M_Out(12259) = 1            'DVDメカチャック開ON
1081     '
1082 '    Wait M_In(11268) = 1        'DVDメカチャック開検出
1083     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1084     If MRtn = 1 Then GoTo *CompMechaGet1
1085     Mov PMechaGet_2
1086     Mov PMechaGet_3
1087     Mov PMechaGet_4
1088     fErrorProcess(11,270,284,0)
1089     If M_20# = MNext% Then M_20# = MClear%
1090     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1091     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1092     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1093     Mov PMechaGet_3
1094     Mov PMechaGet_2
1095     Mov PMechaGet_1
1096     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1097     *CompMechaGet1
1098     '
1099     M_Out(12261) = 0            'DVDメカシリンダー戻OFF
1100     M_Out(12260) = 1            'DVDメカシリンダー出ON
1101 '    Wait M_In(11271) = 1        'DVDメカシリンダー出検出
1102     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVDメカシリンダー出検出
1103     If MRtn = 1 Then GoTo *CompMechaGet2
1104     Mov PMechaGet_2
1105     Mov PMechaGet_3
1106     Mov PMechaGet_4
1107     fErrorProcess(11,271,284,0)
1108     If M_20# = MNext% Then M_20# = MClear%
1109     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1110     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1111     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1112     Mov PMechaGet_3
1113     Mov PMechaGet_2
1114     Mov PMechaGet_1
1115     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1116     *CompMechaGet2
1117     '
1118     Ovrd 25
1119     Mvs PMechaGet               'DVDメカ受け取り位置
1120     Dly 0.1
1121 '
1122     MRtn = 0
1123     MRtn2 = 0
1124     *RE_MECHA_GET_2
1125     If M_20# = MContinue% Then M_20# = MClear%
1126     M_Out(12259) = 0            'DVDメカチャック開OfF
1127     M_Out(12258) = 1            'DVDメカチャック閉ON
1128     '
1129 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
1130     If MRtn = 1 Then Dly 1.0
1131     If MRtn = 1 Then GoTo *CompMechaGet3
1132     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
1133     If M_20# = MNext% Then GoTo *CompMechaGet3
1134     If MRtn = 1 Then GoTo *CompMechaGet3
1135     M_Out(12258) = 0            'DVDメカチャック閉OfF
1136     M_Out(12259) = 1            'DVDメカチャック開ON
1137     Dly 2.0
1138     Mvs PMechaGet_1
1139     Mov PMechaGet_2
1140     M_Out(12259) = 0            'DVDメカチャック開OfF
1141     M_Out(12258) = 1            'DVDメカチャック閉ON
1142     Mov PMechaGet_3
1143     Mov PMechaGet_4
1144     fErrorProcess(11,269,284,0)
1145     If M_20# = MNext% Then
1146         M_20# = MClear%
1147         MRtn = 1
1148     EndIf
1149     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1150     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1151     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1152     M_Out(12258) = 0            'DVDメカチャック閉OfF
1153     M_Out(12259) = 1            'DVDメカチャック開ON
1154     Mov PMechaGet_3
1155     Mov PMechaGet_2
1156     Mov PMechaGet_1
1157     Mvs PMechaGet
1158     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1159     *CompMechaGet3
1160     M_20# = MClear%
1161     '
1162 '    Wait M_In(11267) = 1        'DVDメカ検出
1163     If MRtn2 = 1 Then GoTo *CompMechaGet4
1164     MRtn2 = frInCheck(11267,1,MSETTIMEOUT05&)   'DVDメカ検出
1165     If MRtn2 = 1 Then GoTo *CompMechaGet4
1166     M_Out(12258) = 0            'DVDメカチャック閉OfF
1167     M_Out(12259) = 1            'DVDメカチャック開ON
1168     Dly 2.0
1169     Mvs PMechaGet_1
1170     Mov PMechaGet_2
1171     M_Out(12259) = 0            'DVDメカチャック開OfF
1172     M_Out(12258) = 1            'DVDメカチャック閉ON
1173     Mov PMechaGet_3
1174     Mov PMechaGet_4
1175     fErrorProcess(11,273,284,0)
1176     If M_20# = MNext% Then
1177         M_20# = MClear%
1178         MRtn2 = 1
1179     EndIf
1180     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1181     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1182     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1183     M_Out(12258) = 0            'DVDメカチャック閉OfF
1184     M_Out(12259) = 1            'DVDメカチャック開ON
1185     Mov PMechaGet_3
1186     Mov PMechaGet_2
1187     Mov PMechaGet_1
1188     Mvs PMechaGet
1189     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1190     *CompMechaGet4
1191     M_20# = MClear%
1192     Dly 0.5
1193     '
1194     Mvs PMechaGet_1             'DVDメカ受け取り上空
1195 '    *RE_MECHA_GET_3
1196     M_Out(12260) = 0            'DVDメカシリンダー出OFF
1197     M_Out(12261) = 1            'DVDメカシリンダー戻ON
1198 '    Wait M_In(11270) = 1        'DVDメカシリンダー戻検出
1199     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVDメカシリンダー戻検出
1200 '    If MRtn = 1 Then GoTo *CompMechaGet5       '処理位置変更2/11中村
1201 '    fErrorProcess(11,272,284,0)
1202 '    If M_20# = MNext% Then M_20# = MClear%
1203 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1204 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1205 '    If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1206 '    *CompMechaGet5
1207     '
1208     If MRtn = 1 Then Ovrd 100
1209     Mov PMechaGet_2             'DVDメカ受け取り上空回避点
1210 '    ' 部品供給要求終了
1211     M_Out(12787) = 0
1212 '    ' 部品取得完了送信(パルス)
1213     M_Out(12800) = 1 Dly 0.5
1214     Mov PMechaGet_3             '経路1
1215     Mov PMechaGet_4             '経路2
1216 '
1217     *RE_MECHA_GET_3
1218     M_Out(12260) = 0            'DVDメカシリンダー出OFF
1219     M_Out(12261) = 1            'DVDメカシリンダー戻ON
1220     If MRtn = 1 Then GoTo *CompMechaGet5
1221     fErrorProcess(11,272,284,0)
1222     If M_20# = MNext% Then M_20# = MClear%
1223     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1224     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1225     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1226     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1227     *CompMechaGet5
1228     '
1229     'DVDメカを仮置き台へ置く
1230 '    Wait M_In(11920) = 0             'BaseUnit6側仮置き台フラグ確認(処理位置変更2/11中村)
1231 '
1232 '   仮置き台が回転中か確認
1233     *Loop_CW_CCW_1
1234     If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_1 Else GoTo *Loop_CW_CCW_1
1235     *Next_CW_CCW_1
1236     If M_In(11920) = 0 Then GoTo *OK_FLG_1 Else GoTo *Loop_CW_CCW_1          'BaseUnit6側仮置き台フラグ確認
1237     *OK_FLG_1
1238 '
1239     M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1240     '
1241     'DVDメカが仮置き台に置かれていないかの確認(追加ここから10/1中村)
1242     MRtn = 1
1243     If M_In(11931) = 1 Then          '回転方向の確認(CW方向)
1244         If M_In(11928) = 0 Then      'BaseUnit5側にDVDメカが置かれていた場合
1245             M_Out(12912) = 0         '仮置き台フラグ回収(回転可能状態にするため)
1246             Wait M_In(11930) = 1     '仮置き台回転待ち
1247             MRtn = 0
1248         EndIf
1249     ElseIf M_In(11930) = 1 Then      '回転方向の確認(CCW方向)
1250         If M_In(11929) = 0 Then      'BaseUnit5側にDVDメカが置かれていた場合
1251             M_Out(12912) = 0         '仮置き台フラグ回収(回転可能状態にするため)
1252             Wait M_In(11931) = 1     '仮置き台回転待ち
1253             MRtn = 0
1254         EndIf
1255     Else
1256         MRtn = 0
1257     EndIf
1258     If MRtn = 0 Then GoTo *Loop_CW_CCW_1
1259     '
1260 *Loop_CW_CCW_S
1261     fnAutoScreenComment(530)    '状態表示[工程６の動作終了待ち] 2022/04/26 渡辺
1262 'Ver 0.4 追加 -----------------------
1263 '自工程が12912 = 1 を出力した後、再度 工程6側の動作中監視を行う
1264     MRtn = 0
1265     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec 工程6のフラグON監視
1266     If MRtn = 1 Then M_Out(12912) = 0                  '仮置き台フラグ回収 工程6優先のため12912=0を出力
1267     If MRtn = 1 Then Dly 0.7
1268     If MRtn = 1 Then GoTo *Loop_CW_CCW_S        '工程6の動作中フラグがONしていたら再度ループ
1269     M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1270 'Ver 0.4 ここまで -------------------
1271 '
1272     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1273     Mov PMechaSet_3             'DVDメカ仮置き回避点1
1274 'Dly 5.0   'デバッグ用
1275 '
1276 *Loop_CW_CCW_2
1277 'Ver 0.4 追加 -----------------------
1278     '自工程が12912 = 1 を出力した後、再度 工程6側の動作中監視を行う
1279     MRtn = 0
1280     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec 工程6のフラグON監視
1281     If MRtn = 1 Then M_Out(12912) = 0                  '仮置き台フラグ回収 工程6優先のため12912=0を出力
1282     If MRtn = 1 Then Dly 0.7
1283     If MRtn = 1 Then GoTo *Loop_CW_CCW_2        '工程6の動作中フラグがONしていたら再度ループ
1284     M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1285 'Ver 0.4 ここまで -------------------
1286 '
1287     Mov PMechaSet_2             'DVDメカ仮置き回避点2
1288 '
1289 '    *Loop_CW_CCW_2  '仮置き台の状態をもう一度確認する(コメントアウト2/27中村)
1290 '    If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_2 Else GoTo *Loop_CW_CCW_2
1291 '    *Next_CW_CCW_2
1292 '    If M_In(11920) = 0 Then GoTo *OK_FLG_2 Else GoTo *Loop_CW_CCW_2         'BaseUnit6側仮置き台フラグ確認
1293 '    *OK_FLG_2
1294 ''
1295 '    M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1296 '
1297     '
1298     *RE_MECHA_SET_1
1299     If M_20# = MContinue% Then M_20# = MClear%
1300     Ovrd 25
1301     M_Out(12261) = 0            'DVDメカシリンダー戻OFF
1302     M_Out(12260) = 1            'DVDメカシリンダー出ON
1303 '    Wait M_In(11271) = 1        'DVDメカシリンダー出検出
1304     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVDメカシリンダー出検出
1305     If MRtn = 1 Then GoTo *CompMechaSet1
1306     Mov PMechaSet_3
1307     Mov PMechaGet_4
1308     fErrorProcess(11,271,284,0)
1309     If M_20# = MNext% Then M_20# = MClear%
1310     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1311     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1312     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1313     Mov PMechaSet_3
1314     Mov PMechaSet_2
1315     Ovrd 100
1316     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_1
1317     *CompMechaSet1
1318     '
1319     *RE_MECHA_SET_12
1320     Fine 0.05 , P
1321 '    Wait M_In(11920) = 0        'BaseUnit6側仮置き台フラグ確認(コメントアウト2/27中村)
1322 '    M_Out(12912) = 1            '仮置き台フラグ立て
1323     If M_In(11931) = 1 Then     '回転方向の確認(CW方向)(追加ここまで10/1中村)
1324         Mov PMechaSet1_1        'DVDメカ仮置き上空1
1325         Ovrd 10
1326         Mvs PMechaSet1          'DVDメカ仮置き位置1
1327         Dly 0.1
1328         M_Out(12258) = 0        'DVDメカチャック閉OFF
1329         M_Out(12259) = 1        'DVDメカチャック開ON
1330 '        Wait M_In(11268) = 1    'DVDメカチャック開検出
1331         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1332         Mvs PMechaSet1_1        'DVDメカ仮置き上空1
1333     ElseIf M_In(11930) = 1 Then '回転方向の確認(CCW方向)(追加ここから10/1中村)
1334         Mov PMechaSet2_1        'DVDメカ仮置き上空
1335         Ovrd 10
1336         Mvs PMechaSet2          'DVDメカ仮置き位置2
1337         Dly 0.1
1338         M_Out(12258) = 0        'DVDメカチャック閉OFF
1339         M_Out(12259) = 1        'DVDメカチャック開ON
1340 '        Wait M_In(11268) = 1    'DVDメカチャック開検出
1341         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1342         Mvs PMechaSet2_1        'DVDメカ仮置き上空2
1343     'Else
1344         'エラー文(仮置き台が正常な位置に無い)
1345     EndIf                       '追加ここまで10/1中村
1346     Fine 0 , P
1347     '
1348     If MRtn = 1 Then GoTo *CompMechaSet2
1349     fErrorProcess(11,270,284,0)
1350     If M_20# = MNext% Then M_20# = MClear%
1351     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1352     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1353     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_12
1354     *CompMechaSet2
1355     '
1356     Ovrd 100
1357     *RE_MECHA_SET_2
1358     M_Out(12260) = 0            'DVDメカシリンダー出OFF
1359     M_Out(12261) = 1            'DVDメカシリンダー戻ON
1360 '    Wait M_In(11270) = 1        'DVDメカシリンダー戻検出
1361     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVDメカシリンダー戻検出
1362     If MRtn = 1 Then GoTo *CompMechaSet3
1363     fErrorProcess(11,272,284,0)
1364     If M_20# = MNext% Then M_20# = MClear%
1365     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1366     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1367     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_2
1368     *CompMechaSet3
1369     '
1370     Mov PMechaSet_2             'DVDメカ仮置き回避点2
1371     M_Out(12912) = 0                  '仮置き台フラグ回収(追加10/1中村)
1372     '
1373     'ねじロボ2の製品を取る
1374     Mov PProductOnRoboGet_4     '経路3から4へ
1375     M_Out(12259) = 0            'DVDメカチャック開OfF
1376     M_Out(12258) = 1            'DVDメカチャック閉ON
1377 '    Wait M_In(11876) = 1        'ねじロボ2完了待機を受信
1378 MRtn = fScrewTighenRoboCheck(11876)    '停止状態を受信する
1379 If MRtn = 0 Then Mov PInitialPosition   '"イニシャルに戻る動き"
1380 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1381 '
1382     *RE_ROBO_GET_1
1383 '
1384     M_Out(12259) = 0            'DVDメカチャック開OFF
1385     M_Out(12258) = 1            'DVDメカチャック閉ON
1386     If M_20# = MContinue% Then Dly 0.5
1387 '
1388 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
1389     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
1390     If MRtn = 1 Then GoTo *CompRoboGet1
1391     fErrorProcess(11,269,284,0)
1392     If M_20# = MNext% Then M_20# = MClear%
1393     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1394     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1395     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1396     If M_20# = MContinue% Then GoTo *RE_ROBO_GET_1
1397     *CompRoboGet1
1398     '
1399 '    Ovrd 50
1400     Mov PProductOnRoboGet_3     'ねじロボ製品取り上空回避点2から3へ
1401 '    Ovrd 20
1402     Mvs PProductOnRoboGet_2     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から2へ
1403     Ovrd 20
1404     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
1405     Ovrd 10
1406     Mvs PProductOnRoboGet       'ねじロボ製品取り位置
1407     Dly 0.2
1408 '
1409     *RE_ROBO_GET_2
1410 '
1411     M_Out(12257) = 0            '本体チャック開OFF
1412     M_Out(12256) = 1            '本体チャック閉ON
1413 '
1414 '    Wait M_In(11266) = 1        '本体チャック閉検出
1415     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
1416     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
1417     M_Out(12256) = 0            '本体チャック閉OFF
1418     M_Out(12257) = 1            '本体チャック開ON
1419     Dly 2.0
1420     Mvs PProductOnRoboGet_1
1421     Mvs PProductOnRoboGet_2
1422     Mov PProductOnRoboGet_3
1423     Mov PProductOnRoboGet_4
1424     Mov PInitialPosition
1425     M_Out(12257) = 0            '本体チャック開OFF
1426     M_Out(12256) = 1            '本体チャック閉ON
1427     Dly 1.0
1428     fErrorProcess(11,245,284,0)
1429     If M_20# = MNext% Then MRtn = 1
1430     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1431     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1432     M_Out(12256) = 0            '本体チャック閉OFF
1433     M_Out(12257) = 1            '本体チャック開ON
1434     Dly 2.0
1435     Mov PProductOnRoboGet_4
1436     Mov PProductOnRoboGet_3
1437     Mov PProductOnRoboGet_2
1438     Mvs PProductOnRoboGet_1
1439     Mvs PProductOnRoboGet
1440     If M_20# = MContinue% Or M_20# = MNext% Then GoTo *RE_ROBO_GET_2
1441     *CompRoboGet2
1442     M_20# = MClear%
1443     '
1444     Dly 0.2
1445     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
1446     Ovrd 50
1447     Mvs PProductOnRoboGet_2     'ねじロボ製品取り上空回避点(9/27暫定コメントアウト)12/15コメント解除
1448     Mvs PProductOnRoboGet_3     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から3へ
1449     Ovrd 100
1450     Mov PProductOnRoboGet_4     '経路3から4へ
1451     Cnt 1 , 10 , 10
1452 '
1453     M_Out(12868) = 1 Dly 0.3    'ねじロボ2動作完了を送信
1454     *RE_ROBO_GET_3
1455     M_Out(12258) = 0            'DVDメカチャック閉OFF
1456     M_Out(12259) = 1            'DVDメカチャック開ON
1457 '    Wait M_In(11268) = 1        'DVDメカチャック開検出
1458     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1459     If MRtn = 1 Then GoTo *CompRoboGet3
1460     fErrorProcess(11,270,284,0)
1461     If M_20# = MNext% Then M_20# = MClear%
1462     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1463     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1464     If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1465     *CompRoboGet3
1466     '
1467     'パレットへ製品を置く
1468     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1469     Cnt 1 , 10
1470     Mov PProductOnPltSet_1      '本体置き位置上空
1471     Cnt 0
1472     Ovrd 10
1473     Mvs PProductOnPltSet        '本体置き位置
1474     Dly 0.5
1475 '
1476     *RE_PLT_SET
1477 '
1478     M_Out(12256) = 0            '本体チャック閉OFF
1479     M_Out(12257) = 1            '本体チャック開ON
1480 '
1481     Wait M_In(11265) = 1        '本体チャック開検出
1482 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
1483     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
1484     If MRtn = 1 Then GoTo *CompPltSet
1485     fErrorProcess(11,244,284,0)
1486     If M_20# = MNext% Then M_20# = MClear%
1487     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1488         Mvs PProductOnPltSet_1
1489         Mov PProductOnPltSet_2
1490         Mov PInitialPosition
1491     EndIf
1492     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1493     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1494     If M_20# = MContinue% Then GoTo *RE_PLT_SET
1495     *CompPltSet
1496 '
1497     Mvs PProductOnPltSet_1      '本体置き位置上空
1498     Ovrd 100
1499     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1500 '    Mov PInitialPosition        'イニシャルポジション
1501     MRtn = FnCtlValue2(2)          '組立ＯＫ＋１  2022/04/28 渡辺
1502     Mov PTicketRead_1           'チケットID読み取り回避点
1503     MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1504     '
1505     'チケットID書き込み
1506     M_20# = MAssyOK%
1507     *ASSY_ERROR_END
1508     *AssyEnd
1509     *fnAssyStart_FEndPosi
1510 FEnd
1511 '
1512 '■fnPiasCheck
1513 ''' <summary>
1514 ''' PIASチケット読込み
1515 ''' </summary>
1516 ''' <returns>   0 : NG
1517 '''             1 : OK(読込み完了)
1518 ''' </returns>
1519 ''' <remarks>
1520 ''' Date   : 2021/07/07 : M.Hayakawa
1521 ''' </remarks>'
1522 Function M% fnPiasCheck
1523     fnPiasCheck = 0
1524     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1525     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1526 '
1527 *RETRY_PIAS
1528     M_20# = MClear%
1529     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1530     '
1531     '【IDチケット読み込み】
1532     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1533     MInspGroup%(1) = 1              '検査G番号
1534     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1535 '
1536     'エラーの場合
1537     If MRtn <> 1 Then
1538         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1539         If MRtn <> 1 Then
1540             'D720 -> D1300 コピー要求
1541             M_Out(12565) = 1
1542             Dly 0.5
1543             M_Out(12565) = 0
1544             'エラー処理記述
1545             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1546             'GOT KEY入力待ち
1547             MKeyNumber = fnKEY_WAIT()
1548             '
1549             Select MKeyNumber
1550                 Case MNext%         '次へを選択した場合
1551                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1552                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1553                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1554                     Break
1555                 Case MAbout%        '停止を選択した場合
1556                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1557                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1558                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1559                     Break
1560                 Case MNgProcess%    'NGを選択した場合
1561                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1562                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1563                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1564                     Break
1565                 Case MContinue%     '継続を選択した場合
1566                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1567                     M_20# = MContinue%
1568                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1569                     Break
1570             End Select
1571         EndIf
1572     EndIf
1573 '----------D720 -> D1300 コピー要求----------
1574     M_Out(12565) = 1
1575     Dly 0.5
1576     M_Out(12565) = 0
1577 '----------通信確認をする----------
1578     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1579     MRtn = 0                ' 初期化
1580     M_20# = MClear%         ' 初期化
1581     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1582     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1583     If MRtn <> 1 Then
1584         If M_20# = MContinue% Then
1585             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1586         Else
1587             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1588         EndIf
1589     EndIf
1590 '----------工程抜け確認----------
1591     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1592     MRtn = 0                ' 初期化
1593     M_20# = MClear%         ' 初期化
1594     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1595     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1596     If MRtn <> 1 Then
1597         If M_20# = MContinue% Then
1598             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1599         Else
1600             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1601         EndIf
1602     EndIf
1603     '
1604     fnPiasCheck = 1
1605     *fnPiasCheck_End
1606 FEnd
1607 '
1608 '■fnPCComuCheck
1609 ''' <summary>
1610 ''' PC-PLC通信チェック
1611 ''' </summary>
1612 ''' <returns>   0 : NG
1613 '''             1 : OK(読込み完了)
1614 ''' </returns>
1615 ''' <remarks>
1616 ''' Date   : 2021/07/07 : M.Hayakawa
1617 ''' </remarks>'
1618 Function M% fnPCComuCheck
1619     fnPCComuCheck = 0
1620     MJudge% = 0                                  '初期化
1621     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1622     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1623     '
1624     For MStaNo = 0 To 5
1625         '
1626         If M_In(MIN_PIAS_ComOK%) = 1 Then
1627             'PC通信OK(M400)
1628             MJudge% = MOK%
1629             MStaNo = 5
1630             Break
1631         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1632             'toRBT_通信確認time out
1633             MJudge% = MNG%
1634             MCommentD1001 = 15
1635             MCommentD1002 = 21
1636             MStaNo = 5
1637             Break
1638         Else
1639             'toRBT_通信確認time out
1640             MJudge% = MNG%
1641             MCommentD1001 = 14
1642             MCommentD1002 = 21
1643             Break
1644         EndIf
1645     Next MStaNo
1646     '
1647     '上記で返信フラグを受信してからPC通信確認OFF
1648     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1649     '
1650     'エラー画面
1651     If MJudge% <> MOK% Then
1652         M_20# = MClear%     '初期化
1653         'エラー処理記述
1654         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1655         'GOT KEY入力待ち
1656         MKeyNumber = fnKEY_WAIT()
1657         '
1658         If MKeyNumber = MAbout% Then            '停止を選択した場合
1659             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1660             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1661             Break
1662         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1663             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1664             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1665             Break
1666         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1667             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1668             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1669             Break
1670         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1671             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1672             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1673             Break
1674         EndIf
1675     Else
1676         'OKの場合
1677         fnPCComuCheck = 1
1678     EndIf
1679 FEnd
1680 '
1681 '■fnProcessCheck
1682 ''' <summary>
1683 ''' 工程抜け確認
1684 ''' </summary>
1685 ''' <returns>    1：工程履歴OK     0：異常終了
1686 '''             -1：前工程履歴NG  -2：自工程履歴あり
1687 '''             -3：モデル仕向NG  -4：タイムアウト
1688 '''             -5：履歴処理エラー
1689 ''' </returns>
1690 ''' <remarks>
1691 ''' Date   : 2021/07/07 : M.Hayakawa
1692 ''' </remarks>'
1693 Function M% fnProcessCheck
1694     fnProcessCheck = 0
1695     MJudge% = MNG%      '一旦NGを初期化とする
1696 '----------工程抜け確認----------
1697     MCommentD1001 = 0   'コメント初期化
1698     For MStaNo = 0 To 5
1699         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1700         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1701         '
1702         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1703             MJudge% = MOK%
1704             fnAutoScreenComment(85)     ' AUTO画面
1705             MStaNo = 5
1706             Break
1707         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1708             MFlgLoop% = 0
1709             MJudge% = MNG%
1710             MCommentD1001 = 27
1711             MCommentD1002 = 22
1712             fnAutoScreenComment(94)     ' AUTO画面
1713             fnProcessCheck = -2         ' NGは-2を返す
1714             MStaNo = 5
1715             Break
1716         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1717            MJudge% = MNG%
1718             MCommentD1001 = 31
1719             MCommentD1002 = 22
1720             fnAutoScreenComment(83)     ' AUTO画面
1721             fnProcessCheck = -3         ' NGは-3を返す
1722             MStaNo = 5
1723             Break
1724         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1725             '履歴NGは直ぐに終了せず繰り返し確認を行う
1726             '前工程の書込みが終了していない可能性があるため
1727             MJudge% = MNG%
1728             MCommentD1001 = 32
1729             MCommentD1002 = 22
1730             fnAutoScreenComment(84)     ' AUTO画面
1731             fnProcessCheck = -1         ' NGは-1を返す
1732             Dly 1.0
1733             '工程抜け確認OFF
1734             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1735             Dly 1.0
1736            'MStaNo = 5
1737             Break
1738         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1739             MFlgLoop% = 0
1740             MJudge% = MNG%
1741             MCommentD1001 = 29
1742             MCommentD1002 = 22
1743             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1744             fnProcessCheck = -5         ' NGは-5を返す
1745             MStaNo = 5
1746             Break
1747         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1748             MJudge% = MNG%
1749             If MCommentD1001 = 32 Then
1750                 '何もしない
1751             Else
1752                 MCommentD1001 = 26
1753             EndIf
1754             MCommentD1002 = 22
1755             fnProcessCheck = -4         ' NGは-4を返す
1756             MStaNo = 5
1757             Break
1758         Else
1759             MJudge% = MNG%
1760             MCommentD1001 = 28
1761             MCommentD1002 = 22
1762         EndIf
1763     Next MStaNo
1764     '工程抜け確認OFF
1765     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1766     '通過履歴NG 工程抜けの場合
1767     If MJudge% = MPass% Then
1768         M_20# = MPass%
1769     EndIf
1770     '
1771     'エラー画面
1772     If MJudge% <> MOK% Then
1773         M_20# = MClear%     '初期化
1774         'エラー処理記述
1775         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1776         'GOT KEY入力待ち
1777         MKeyNumber = fnKEY_WAIT()
1778         '
1779         Select MKeyNumber
1780             Case MAbout%        '停止を選択した場合
1781                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1782                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1783                 Break
1784             Case MNext%         '次へを選択した場合
1785                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1786                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1787                 Break
1788             Case MContinue%     '継続を選択した場合
1789                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1790                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1791                 Break
1792             Case MNgProcess%    'NGを選択した場合
1793                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1794                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1795                 Break
1796         End Select
1797     Else
1798         fnProcessCheck = 1  ' OKは1を返す
1799     EndIf
1800 FEnd
1801 '
1802 '■fnPiasWrite
1803 ''' <summary>
1804 ''' Pias 組立結果書込み要求
1805 ''' </summary>
1806 '''<param name="MFlg%">
1807 '''                 MOK%(1) = 工程履歴にOKを書込む
1808 '''                 MNG%(0) = 工程履歴にNGを書込む
1809 '''</param>
1810 '''<returns></returns>
1811 ''' <remarks>
1812 ''' Date   : 2021/07/07 : M.Hayakawa
1813 ''' </remarks>'
1814 Function M% fnPiasWrite(ByVal MFlg%)
1815       fnPiasWrite = 0
1816 *RETRY_PIASWRITE
1817     '
1818     '組立OK(MOK%)の場合　M306 ON
1819    '組立NG(MNG%)の場合　M307 ON
1820     If MFlg% = MOK% Then
1821         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1822     Else
1823         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1824     EndIf
1825     Dly 0.1                  '念のため
1826     '
1827     'Piasへ書込み開始 M305 -> ON
1828     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1829     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1830     '
1831     MJudge% = MNG%
1832     '
1833     For MStaNo = 0 To 5
1834         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1835             MJudge% = MOK%
1836             'MRet = fnAutoScreenComment(85)  'AUTO画面
1837             MStaNo = 5
1838             Break
1839         '
1840         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1841             MJudge% = MNG%
1842             'MRet = fnAutoScreenComment(85)  'AUTO画面
1843            MCommentD1001 = 34
1844            MCommentD1002 = 25
1845             MStaNo = 5
1846             Break
1847         '
1848         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1849             MJudge% = MNG%
1850             'MRet = fnAutoScreenComment(85)  'AUTO画面
1851            MCommentD1001 = 35
1852            MCommentD1002 = 25
1853             MStaNo = 5
1854             Break
1855         '
1856         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1857             MJudge% = MNG%
1858             'MRet = fnAutoScreenComment(85)  'AUTO画面
1859            MCommentD1001 = 36
1860            MCommentD1002 = 25
1861             MStaNo = 5
1862             Break
1863         '
1864         Else
1865             MJudge% = MNG%
1866            MCommentD1001 = 42
1867            MCommentD1002 = 25
1868         '
1869         EndIf
1870         '
1871     Next MStaNo
1872     '
1873     'Piasへ書込み開始 M305 -> OfF
1874     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1875     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1876     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1877     '
1878     '
1879     '通過履歴NG 工程抜けの場合
1880     If MJudge% = MPass% Then
1881         M_20# = MPass%
1882     EndIf
1883     '
1884    M_20# = MClear%     '初期化
1885     '
1886     'エラー画面
1887     If MJudge% < MOK% Then
1888     '
1889 '残しておくが現状では使用しないラベル
1890 *RETRY_ERR_WRITE
1891         M_20# = MClear%     '初期化
1892         'エラー処理記述
1893         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1894         'GOT KEY入力待ち
1895         MKeyNumber = fnKEY_WAIT()
1896         '
1897         If MKeyNumber = MAbout% Then   '停止を選択した場合
1898             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1899            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1900             Break
1901         '
1902         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1903             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1904             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1905         '
1906         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1907             M_20# = MPass%            'M_20# プログラム間共通外部変数
1908             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1909         '
1910         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1911             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1912            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1913             Break
1914         '
1915         EndIf
1916         '
1917         If M_20# = MClear% Then *RETRY_ERR_WRITE
1918         '
1919     EndIf
1920     '
1921     If M_20# = MContinue% Then *RETRY_PIASWRITE
1922     '
1923     fnPiasWrite = 1
1924     '
1925 FEnd
1926 '
1927 '■fnPCBNumberCheck
1928 ''' <summary>
1929 ''' Pias 基板番号照合要求
1930 ''' </summary>
1931 '''<param name="%"></param>
1932 '''<param name="%"></param>
1933 '''<returns></returns>
1934 ''' <remarks>
1935 ''' Date   : 2021/07/07 : M.Hayakawa
1936 ''' </remarks>'
1937 Function M% fnPCBNumberCheck
1938       fnPCBNumberCheck = 0
1939     '
1940 *RETRY_PCBCHECK
1941     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1942     'Piasへ基板照合開始 M310 -> ON
1943     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1944     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1945     '
1946     MJudge% = MNG%
1947     '
1948     For MStaNo = 0 To 5
1949         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1950             MJudge% = MOK%
1951             fnAutoScreenComment(96)  'AUTO画面
1952             MStaNo = 5
1953             Break
1954         '
1955         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1956             MJudge% = MNG%
1957             fnAutoScreenComment(97)  'AUTO画面
1958             MCommentD1001 = 37
1959             MCommentD1002 = 25
1960             MStaNo = 5
1961             Break
1962         '
1963         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1964             MJudge% = MNG%
1965             fnAutoScreenComment(98)  'AUTO画面
1966             MCommentD1001 = 38
1967             MCommentD1002 = 25
1968             MStaNo = 5
1969             Break
1970         '
1971         ElseIf M_In(11580) = 1 Then                         'time out
1972             MJudge% = MNG%
1973             fnAutoScreenComment(99)  'AUTO画面
1974             MCommentD1001 = 39
1975             MCommentD1002 = 25
1976             MStaNo = 5
1977             Break
1978         '
1979         Else
1980             MJudge% = MNG%
1981            MCommentD1001 = 41
1982            MCommentD1002 = 25
1983         '
1984         EndIf
1985         '
1986     Next MStaNo
1987     '
1988     'Piasへ基板照合開始 M310 -> OfF
1989     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1990     '
1991     '
1992     '通過履歴NG 工程抜けの場合
1993     If MJudge% = MPass% Then
1994         M_20# = MPass%
1995     EndIf
1996     '
1997    M_20# = MClear%     '初期化
1998     '
1999     'エラー画面
2000     If MJudge% < MOK% Then
2001     '
2002 '残しておくが現状では使用しないラベル
2003 *RETRY_ERR_PCBNUMBER
2004         M_20# = MClear%     '初期化
2005         'エラー処理記述
2006         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
2007         'GOT KEY入力待ち
2008         MKeyNumber = fnKEY_WAIT()
2009         '
2010         If MKeyNumber = MAbout% Then   '停止を選択した場合
2011             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2012             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2013             Break
2014         '
2015         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
2016             M_20# = MContinue%            'M_20# プログラム間共通外部変数
2017             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2018         '
2019         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
2020             M_20# = MPass%            'M_20# プログラム間共通外部変数
2021             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2022         '
2023         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
2024             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
2025             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2026             Break
2027         '
2028         EndIf
2029         '
2030         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
2031         '
2032     EndIf
2033     '
2034     If M_20# = MContinue% Then *RETRY_PCBCHECK
2035 FEnd
2036 '
2037 '■ScrewTight_S2
2038 ''' <summary>
2039 ''' ねじ締めを行う
2040 ''' </summary>
2041 '''<param name="PScrewPos()">
2042 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
2043 '''             PScrewPos(2)    ：ねじ締め回避点
2044 '''             PScrewPos(10)   ：ねじ締め終了高さ
2045 '''</param>
2046 '''<returns>整数
2047 '''         0=異常終了、1=正常終了
2048 '''</returns>
2049 ''' <remarks>
2050 ''' Date   : 2021/07/07 : M.Hayakawa
2051 ''' </remarks>'
2052 Function M% ScrewTight_S2(ByVal PScrewPosition())   'ネジ締め個別設定
2053     ScrewTight_S2 = 0
2054     MOKNGFlg = 0
2055     Ovrd 100
2056     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
2057     ' 暫定
2058     Ovrd 5
2059     Mvs PScrewPosition(10),-10    ' パレット上ねじ締めS①の上空へ移動
2060 '    Ovrd MOvrdA
2061     '暫定マスク
2062 '    M_Out(Y62_Driver)=1     ' バンクセッティング　C1
2063 '    Dly 0.1
2064 '    M_Out(Y61_Driver)=1     'ドライバーON　CW
2065 '    'Spd 8.3 '外部ライド100  内部ライド60   'ライド100-40　100%：Spd　15　'ねじ締め速度設定
2066 '    Spd MSpdA               'ネジ締め時Spd個別設定
2067     ' 暫定移動のみ
2068     Mvs PScrewPosition(10)
2069 '    '
2070 '    Dly 0.1
2071 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
2072 '    Wait M_In(11584)=1          '完了/エラー検出
2073 '    Dly 0.1
2074 '    Spd M_NSpd
2075 '    '
2076 '    If M_In(X28_Driver)=1 Then  'ねじトータルエラー検出時
2077 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
2078 '        Dly 0.1
2079 '        M_Out(Y62_Driver)=0     'バンクセッティング解除　C1
2080 '        Dly 0.1
2081 '        M_Out(Y63_Driver)=0     'バンクセッティング解除　C1
2082 '        Dly 0.1
2083 '        M_Out(Y65_Driver)=0     'プログラム解除　F1
2084 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
2085 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
2086 '        MOKNGFlg = -1
2087 '        ScrewTight_S2 = 0
2088 '    Else
2089 '        Wait M_In(X29_Driver)=1 ' 正常完了時
2090 '        Dly 0.1
2091 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
2092 '        Dly 0.1
2093 '        M_Out(Y62_Driver)=0     'バンクセッティング解除
2094 '        Dly 0.1
2095 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
2096 '        Dly 0.1
2097 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
2098 '        ScrewTight_S2 = 1
2099 '    EndIf
2100 ' 暫定
2101     Ovrd 10
2102     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
2103     Ovrd 100
2104 FEnd
2105 '
2106 '■ScrewGet_S3
2107 ''' <summary>
2108 ''' ねじ供給機からねじを得る
2109 ''' </summary>
2110 '''<param name="%"></param>
2111 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
2112 '''         PScrewPos(2)    ：ねじ供給器回避点
2113 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2114 '''         PScrewPos(3)    ：Mねじポカヨケ位置
2115 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
2116 '''<returns>整数
2117 '''         0=異常終了、1=正常終了、-1=MネジセンサーNG、-2=MネジセンサーON、-3=吸着エラー
2118 '''</returns>
2119 ''' <remarks>
2120 ''' Date   : 2021/07/07 : M.Hayakawa
2121 ''' </remarks>'
2122 Function M% ScrewGet_S3(ByVal PScrewPosition())
2123     ScrewGet_S3 = 0
2124     MMScrewJudge% = 0
2125     'ねじ供給器初期動作エラーチェック
2126 ' ↓暫定削除
2127 '    Wait M_In(X34_ScrewReady1)=1 'ねじ供給器SがReadyになるまで待つ　←　何秒か待ってReadyにならなければ抜けるプログラムが必要？
2128 '    Ovrd 100
2129 '    If M_In(X33_SS2)=0 Then  'Mねじ検出センサがOFF（故障）していた場合
2130 '        Ovrd 30
2131 '        Mvs,-80             'その場所から80mm上空へ移動
2132 '        Mov PInitPos19049   '19049初期位置へ移動
2133 '        M_Out(Y6A_VV1)=0    'ねじ吸着 Off
2134 '        'NGとしてここの関数から抜ける
2135 '        ScrewGet_S3 = -1
2136 '        MMScrewJudge% = 1
2137 '        MCommentD1001 = 61
2138 '    EndIf
2139 '    If ScrewGet_S3 = 0 Then
2140 '        'Sタイト用ねじ供給機にMねじが混入していないか監視
2141 '        MMScrewJudge% = 0 'MMScrewJudgeを初期化する
2142 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
2143 '        If MRtn = 0 Then
2144 '            Ovrd 30
2145 '            Mvs,-80            'その場所から50mm上空へ移動
2146 '            Mov PInitPos19049  '19049初期位置へ移動
2147 '            MMScrewJudge% = 2
2148 '            MRtn = All_CLamp_Release()'全てのクランプ解除へ分岐
2149 '            MCnt% = 2   '2を設定
2150 '            MCommentD1001 = 62
2151 '        EndIf
2152 '        If MMScrewJudge% = 2 Then
2153 '            ScrewGet_S3 = -2
2154 '        EndIf
2155 '    EndIf
2156 '    'Mネジ判定がONの場合 NGとして関数を抜ける
2157 '    If MMScrewJudge% = 2 Then
2158 '        ScrewGet_S3 = -2
2159 '    EndIf
2160     'Sネジ用ねじ太郎のMネジ混入確認用ここまで
2161     Ovrd 100
2162     Spd M_NSpd
2163     If MMScrewJudge% = 0 Then
2164         ScrewGet_S3 = 0
2165         M_Out(Y63_Driver)=1         ' バンクセッティング　C2
2166         MScrewCnt% = 0
2167         MFinCnt% = 2
2168 '        For MCnt% = 0 To MFinCnt%
2169             Mov PScrewPosition(2)        ' ねじ供給機回避点
2170             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
2171             Ovrd 80
2172             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2173             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
2174             Mvs PScrewPosition(10), 1.2
2175             M_Out(Y6A_VV1)=1        ' ねじ吸着　ON
2176             'ビット回転
2177             M_Out(Y60_Driver)=1
2178             Dly 0.2
2179             '
2180             Ovrd 100
2181             JOvrd M_NJovrd
2182             Spd M_NSpd
2183             'ネジ吸着確認位置移動
2184             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
2185             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
2186             'ビット回転停止
2187             'M_Out(Y60_Driver)=0
2188             '
2189             '1秒間ネジ吸着確認
2190 ' 以下暫定削除
2191 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2192 '            'MRtn = 0'強制エラー
2193 '            '吸着エラーの場合
2194 '            'ネジをねじ太郎に戻す
2195 '            If MRtn = 0 Then
2196 '                Ovrd 30
2197 '                'ビット回転停止
2198 '                M_Out(Y60_Driver)=0
2199 '                'ネジ供給機上空
2200 '                Mvs PScrewPos(1)
2201 '                '更に上空
2202 '                Mov PScrewPos(1), -75
2203 '                'ネジ捨て位置
2204 '                Mov PScrewFeedS021
2205 '                '吸着OFF
2206 '                M_Out(Y6A_VV1)=0 'ねじ吸着　OFF
2207 '                Dly 0.2
2208 '                '破壊ON
2209 '                M_Out(Y6B_VB1)=1 '真空破壊ON
2210 '                'ビット回転
2211 '                M_Out(Y61_Driver)=1
2212 '                Dly 0.5
2213 '                '
2214 '                Ovrd 100
2215 '                JOvrd M_NJovrd
2216 '                Spd M_NSpd
2217 '                'ドライバーを上下させねじを振り落とす
2218 '                Mov PScrewFeedS021, 10
2219 '                Mov PScrewFeedS021
2220 '                Dly 0.1
2221 '                Mov PScrewFeedS021, 10
2222 '                Mov PScrewFeedS021
2223 '                '
2224 '                'ネジ落ち待ち
2225 '                'ビット回転停止
2226 '                M_Out(Y61_Driver)=0
2227 '                Dly 0.1
2228 '                '破壊OFF
2229 '                M_Out(Y6B_VB1)=0 '真空破壊OFF
2230 '                '
2231 '                '
2232 '                'ねじ落ちたとして、移動更に上空
2233 '                Mov PScrewPos(1), -75
2234 '                Ovrd 100
2235 '                Spd M_NSpd
2236 '                'ネジ供給機上空
2237 '                Mvs PScrewPos(1)
2238 '                '
2239 '                ScrewGet_S3 = -3
2240 '                Break
2241 '                '
2242 '            Else
2243 '                MCnt% = MFinCnt%
2244 '                ScrewGet_S3 = 0
2245 '            EndIf
2246 '        Next  MCnt%
2247         '
2248         Ovrd 100
2249         Spd M_NSpd
2250         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2251         M_Out(Y60_Driver)=0     ' ビット回転停止
2252         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
2253         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2254         'もう一度吸着確認
2255 ' 以下暫定削除
2256 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2257 '        If MRtn = 0 Then      '吸着エラーの場合
2258 '            MCommentD1001 = 94
2259 '            MCommentD1002 = 95
2260 '            ScrewGet_S3 = -3
2261 '        EndIf
2262 '        If MRtn = 1 Then      '吸着OKの場合
2263 '            ScrewGet_S3 = 1
2264 '        EndIf
2265 '        Break
2266     Else
2267         'Mネジ
2268         If MMScrewJudge% = 2 Then
2269             ScrewGet_S3 = -2
2270         EndIf
2271     EndIf
2272 FEnd
2273 '
2274 '■fnKEY_WAIT()
2275 ''' <summary>
2276 ''' GOTからのキー入力待ち
2277 ''' </summary>
2278 '''<returns>1：停止    2：次へ
2279 '''         3：継続    4：トルクチェック開始
2280 '''         5：NG
2281 '''         11：ロボット初期位置1    12：ロボット初期位置2
2282 '''         13：ロボット初期位置3    14：ロボット初期位置4
2283 '''</returns>
2284 ''' <remarks>
2285 ''' Date   : 2021/07/07 : M.Hayakawa
2286 ''' </remarks>'
2287 Function M% fnKEY_WAIT()
2288     fnKEY_WAIT = 0
2289     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
2290     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
2291     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
2292     '下記キー待ちの継続に反応させないため
2293     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
2294     Dly 0.2
2295     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
2296     MLocalLoopFlg=1
2297     While MLocalLoopFlg=1
2298         If M_In(11345) = 1 Then         '停止   M5345
2299             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
2300             fnKEY_WAIT = 1
2301             MLocalLoopFlg=-1
2302             Break
2303         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
2304             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
2305             fnKEY_WAIT = 2
2306             MLocalLoopFlg=-1
2307             Break
2308         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
2309             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
2310             fnKEY_WAIT = 3
2311             MLocalLoopFlg=-1
2312             Break
2313         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
2314             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
2315             fnKEY_WAIT = 4
2316             MLocalLoopFlg=-1
2317             Break
2318         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
2319             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
2320             fnKEY_WAIT = 5
2321             MLocalLoopFlg=-1
2322             Break
2323             '
2324         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
2325             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
2326             fnKEY_WAIT = MRobotInit1%
2327             MLocalLoopFlg=-1
2328             Break
2329             '
2330         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
2331             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
2332             fnKEY_WAIT = MRobotInit2%
2333             MLocalLoopFlg=-1
2334             Break
2335             '
2336         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
2337             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
2338             fnKEY_WAIT = MRobotInit3%
2339             MLocalLoopFlg=-1
2340             Break
2341             '
2342         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2343             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2344             fnKEY_WAIT = MRobotInit4%
2345             MLocalLoopFlg=-1
2346             Break
2347             '
2348         Else
2349         EndIf
2350     WEnd
2351     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2352     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2353 FEnd
2354 '
2355 '■ fnAUTO_CTL
2356 ''' <summary>
2357 ''' AUTOモードOFF、PLCからの開始待ち
2358 ''' </summary>
2359 ''' <remarks>
2360 ''' Date   : 2021/07/07 : M.Hayakawa
2361 ''' </remarks>
2362 Function M% fnAUTO_CTL
2363     fnAUTO_CTL = 0
2364     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2365     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2366     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2367     '
2368     If M_Svo=0 Then             'サーボON確認
2369         Servo On
2370     EndIf
2371     Wait M_Svo=1
2372 FEnd
2373 '
2374 '■ fnWindScreenOpen
2375 ''' <summary>
2376 ''' ウィンド画面の表示、非表示設定
2377 ''' </summary>
2378 '''<param name="%"></param>
2379 '''<param name="%"></param>
2380 '''<param name="%"></param>
2381 '''<param name="%"></param>
2382 ''' <remarks>
2383 ''' コメントD1001, D1002, D1003の設定
2384 ''' MWindReSet = 0     画面非表示
2385 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2386 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2387 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2388 ''' Date   : 2021/07/07 : M.Hayakawa
2389 ''' </remarks>
2390 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2391     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2392         M_Out16(12480) = MCommentD1001            'D1001 コメント
2393     EndIf
2394     '
2395     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2396         M_Out16(12496) = MCommentD1002            'D1002 コメント
2397     EndIf
2398     '
2399     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2400        M_Out16(12512) = MCommentD1003            'D1003 コメント
2401     EndIf
2402     '
2403     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2404     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2405     Dly 0.5
2406     M_Out(12363) = 0                         'ウィンド画面設定
2407 FEnd
2408 '
2409 '■FnCtlValue2
2410 ''' <summary>
2411 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2412 ''' </summary>
2413 ''' <param name="MCtlNo%"></param>
2414 ''' <remarks>
2415 ''' Date : 2022/04/28 渡辺
2416 ''' </remarks>
2417 '''
2418 '''  1：投入数       ＋１
2419 '''  2：組立ＯＫ数   ＋１
2420 '''  3：組立ＮＧ数   ＋１ (未使用)
2421 '''  4：吸着エラー数 ＋１
2422 ''' 99：読書開始信号 OFF
2423 '''
2424 Function M% FnCtlValue2(ByVal MCtlNo%)
2425     FnCtlValue2 = 1
2426     Select MCtlNo%
2427         Case 1        '投入数＋１
2428             M_Out(12569) = 0             '書込み開始信号OFF
2429             M_Out(12568) = 1             '読込み開始信号ON
2430             MInputQty = M_In16(11600)    '投入数受信
2431             MInputQty = MInputQty + 1    '投入数＋１
2432             M_Out16(12592) = MInputQty   '投入数送信
2433             M_Out(12569) = 1             '書込み開始信号ON
2434             Break
2435             '
2436         Case 2        '組立ＯＫ数＋１
2437             M_Out(12569) = 0             '書込み開始信号OFF
2438             M_Out(12568) = 1             '読込み開始信号ON
2439             MAssyOkQty = M_In16(11616)   '組立OK数受信
2440             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2441             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2442             M_Out(12569) = 1             '書込み開始信号ON
2443             Break
2444             '
2445         Case 4        '吸着エラー数＋１
2446             M_Out(12569) = 0                       '書込み開始信号OFF
2447             M_Out(12568) = 1                       '読込み開始信号ON
2448             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2449             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2450             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2451             M_Out(12569) = 1                       '書込み開始信号ON
2452             Break
2453             '
2454         Case 99        '読書開始信号OFF
2455             M_Out(12568) = 0        '読込み開始信号OFF
2456             M_Out(12569) = 0        '書込み開始信号OFF
2457             Break
2458             '
2459     End Select
2460     Exit Function
2461 FEnd
2462 'Insightによる画像処理検査実行（並列処理なし）
2463 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2464 '-------------------------------------------------------------------------------
2465 'Insightによる画像処理検査実行（並列処理なし）
2466 '   引数
2467 '       PInspPos()      ：検査位置
2468 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2469 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2470 '       MInspCnt%       ：検査位置数
2471 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2472 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2473 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2474 '   戻り値：整数
2475 '       0=異常終了、1=正常終了
2476 '
2477 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2478 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2479 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2480 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2481 '   20200410    :   検査グループ設定Retry追加
2482 '-------------------------------------------------------------------------------
2483     '----- 初期設定 -----
2484     Cnt 0                                                           '移動効率化解除(初期値=0)
2485     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2486 '    Cnt 1,0.1,0.1
2487     '変数宣言・初期化
2488     Def Inte MNum                                                   '検査番号(検査順1～)
2489     MNum% = 1                                                       '検査番号初期値設定
2490     Def Inte MEndFlg                                                '検査終了フラグ
2491     MEndFlg% = 0
2492     '
2493     '検査G番号設定要求・検査実行要求off
2494     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2495     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2496     'エラー番号クリア
2497     MInspErrNum = 0                                                 '検査実行エラー番号
2498     M_Out16(MOUT_InspErrNum) = MInspErrNum
2499     MInspNGStepNum = 0                                              '検査実行NGStep番号
2500     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2501     '
2502     'Insight Ready check?
2503     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2504         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2505         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2506         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2507         ISInspectionSingle = 0                                      '異常終了戻り値設定
2508         Exit Function
2509     EndIf
2510     '
2511     '検査位置数確認
2512     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2513         MInspErrNum = 21                                            '検査データなし 21　引数<1
2514         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2515         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2516         ISInspectionSingle = 0                                      '異常終了戻り値設定
2517         Exit Function
2518     EndIf
2519     '
2520     '
2521     '
2522     '----- メイン処理 -----
2523     '設定された検査位置数分の検査実行
2524     While( MEndFlg% = 0 )
2525         '----- 検査グループ番号設定Retry追加 20200410
2526         MSetGrNumRetryExitFlg = 0
2527         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2528         While( MSetGrNumRetryExitFlg = 0 )
2529         '----- 検査グループ番号設定Retry追加ここまで 20200410
2530             '
2531             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2532             '
2533             '----- 検査グループ番号設定 -----
2534             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2535             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2536             '
2537             '検査位置へ移動・移動完了待ち
2538             Mvs PInspPos( MNum% )                                       '移動
2539             Dly 0.05                                                    '移動完了後Delay
2540             '
2541             '検査グループ番号設定終了確認
2542             M_Timer(1) = 0
2543             MExitFlg = 0
2544             While( MExitFlg = 0 )
2545                 '検査G設定正常終了?
2546                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2547                     MExitFlg = 1
2548                 '
2549                 '検査G設定異常終了?
2550                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2551                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2552                     If MInspErrNum = 0 Then                             '1回目のエラー?
2553                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2554                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2555                     EndIf
2556                     MExitFlg = 1
2557                 '
2558                 'timeoutチェック
2559                 ElseIf 1000 < M_Timer(1) Then
2560                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2561                     If MInspErrNum = 0 Then                             '1回目のエラー?
2562                         MInspErrNum = 12                                'timeout エラー番号=12
2563                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2564                     EndIf
2565                     MExitFlg = 1
2566                 EndIf
2567             WEnd
2568             '
2569             '検査G番号設定要求off
2570             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2571             '
2572             '----- 検査グループ設定Retry追加 20200410
2573             'NGなければ抜ける
2574             If MCurrentStepErr = 0 Then
2575                 MSetGrNumRetryExitFlg = 1
2576             Else
2577                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2578                 If MSetGrNumRetryCnt = 0 Then
2579                     MSetGrNumRetryExitFlg = 1
2580                 Else
2581                     'Retryへ　その前にDelay
2582                     Dly 0.5
2583                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2584                 EndIf
2585             EndIf
2586             '----- 検査グループ設定Retry追加ここまで 20200410
2587             '
2588         WEnd
2589         '
2590         '
2591         '
2592         '----- 検査実行 -----
2593         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2594             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2595                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2596                 MInspRetryExitFlg = 0
2597                 MRetryCnt = 2                                        'Retry回数設定
2598                 While( MInspRetryExitFlg = 0 )
2599                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2600                     '
2601                     '検査完了確認
2602                     MRetryCnt = MRetryCnt - 1
2603                     M_Timer(1) = 0
2604                     MExitFlg = 0
2605                     While( MExitFlg = 0 )
2606                     '検査完了待ち
2607                         '検査OK終了?
2608                         If M_In( MIN_IS_InspOK% ) = 1  Then
2609                             MJudgeOKFlg = 1                         '検査OKフラグON
2610                             MExitFlg = 1
2611                         '
2612                         '検査NG終了?
2613                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2614                             If MInspErrNum = 0 Then                 '1回目のエラー?
2615                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2616                                     MInspErrNum = 32                    '検査NG エラー番号=32
2617                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2618                                 EndIf
2619                             EndIf
2620                             MExitFlg = 1
2621                         '
2622                         '検査異常終了(IS timeout)?
2623                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2624                             If MInspErrNum = 0 Then                 '1回目のエラー?
2625                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2626                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2627                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2628                                 EndIf
2629                             EndIf
2630                             MExitFlg = 1
2631                         '
2632                         'timeoutチェック
2633                         ElseIf 3000 < M_Timer(1) Then
2634                             If MInspErrNum = 0 Then                 '1回目のエラー?
2635                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2636                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2637                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2638                                 EndIf
2639                             EndIf
2640                             MExitFlg = 1
2641                         EndIf
2642                     WEnd
2643                     '
2644                     '検査開始要求off
2645                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2646                     '
2647                     'OKなら抜ける
2648                     If MJudgeOKFlg = 1 Then
2649                         MInspRetryExitFlg = 1
2650                     Else
2651                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2652                         If MRetryCnt = 0 Then
2653                             MInspRetryExitFlg = 1
2654                         Else
2655                             'Retryへ　その前にDelay
2656                             Dly 0.3
2657                         EndIf
2658                     EndIf
2659                     '
2660                 WEnd
2661             EndIf
2662         EndIf
2663         '
2664         '
2665         '
2666         MNum% = MNum% + 1                                           '検査Step+1
2667         '検査終了確認　検査終了フラグセット
2668         If (MInspCnt% < MNum% ) Then
2669             MEndFlg% = 1                                            '検査終了フラグセット
2670         EndIf
2671         'NG発生時続行時処理
2672         If MInspErrNum <> 0 Then                                    'NGあり?
2673             If MNgContinue% <> 1 Then                               'NG続行?
2674                 MEndFlg% = 1                                        '検査終了フラグセット
2675             EndIf
2676         EndIf
2677     WEnd
2678     '
2679     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2680     If 0 < MZAxis% Then
2681         PCurrentPos = P_Curr                                        '現在位置取得
2682         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2683         Mvs PCurrentPos                                             '現在位置上空へ移動
2684     EndIf
2685     Fine 0 , P
2686     '
2687     '戻り値設定
2688     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
2689         ISInspectionSingle = 1                                      '正常終了戻り値設定
2690     Else
2691         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2692         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2693         ISInspectionSingle = 0                                      '異常終了戻り値設定
2694     EndIf
2695     '
2696 FEnd
2697 '
2698 ' ■ISInspection
2699 ''' <summary>
2700 ''' Insightによる画像処理検査実行
2701 ''' </summary>
2702 '''<param name="PInspPos()">検査位置</param>
2703 '''<param name="MInspGrNum%()">検査位置での検査グループ番号（=0：画像検査未実施）</param>
2704 '''             PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2705 '''<param name="MInspCnt%">検査位置数</param>
2706 '''<param name="MZAxis%">終了時のZ軸退避座標（-1:無効）</param>
2707 '''             終了時にZ軸をMZAxisで設定された位置まで上昇させる
2708 '''<param name="MNgContinue%">=1で検査エラー・NG発生時に全Stepの検査を行う</param>
2709 '''<returns>    整数 0=異常終了、1=正常終了</returns>
2710 '''         MInspErrNum     ：異常終了時にエラー番号が設定される
2711 '''         MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される"
2712 ''' <remarks>
2713 ''' Date   : 2021/07/07 : M.Hayakawa
2714 ''' </remarks>
2715 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2716 '    '画像使用確認 0<- 画像確認無しの場合
2717 '    If M_In(11369) = 0 Then            'toRBT_使用確認
2718 '        ISInspection = 1                                        '正常終了戻り値設定
2719 '    EndIf
2720 ''
2721 '    Cnt 0                                                       '移動効率化解除(初期値=0)
2722 '    Fine 0.05,P                                                 '位置決め完了条件設置　0.05mm
2723 '    MNum% = 1                                                   '検査番号初期値設定
2724 '    Def Inte MEndFlg                                            '検査終了フラグ
2725 '    MEndFlg% = 0
2726 '    '
2727 '    'エラー番号クリア
2728 '    MInspErrNumSub = 0                                          '検査実行エラー番号sub
2729 '    MInspErrNum = 0                                             '検査実行エラー番号
2730 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2731 '    MInspNGStepNum = 0                                          '検査実行NGStep番号
2732 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2733 '    '
2734 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready offなら終了
2735 '        MInspErrNum = 20                                        '検査実行エラー番号 20 Insight offline
2736 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2737 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2738 '        ISInspection = 0                                        '異常終了戻り値設定
2739 ''
2740 '    EndIf
2741 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2742 '    '
2743 '    '検査位置数確認
2744 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2745 '        MInspErrNum = 21                                        '検査データなし 21　引数<1
2746 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2747 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2748 '        ISInspection = 0                                        '異常終了戻り値設定
2749 ''
2750 '    EndIf
2751 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2752 '    '
2753 '    '設定された検査位置数分の検査実行
2754 '    While( MEndFlg% = 0 )
2755 '        '検査終了確認　検査終了フラグセット
2756 '        If (MInspCnt% < MNum% ) Then
2757 '            MEndFlg% = 1                                        '検査終了フラグセット
2758 '        EndIf
2759 '        '
2760 '        'タスク　検査G番号設定・検査完了確認処理開始　INSPTAST1
2761 '        If MEndFlg% = 0 Then
2762 '            M_01# = MInspGrNum%(MNum%)                          '検査G番号引渡し
2763 '        EndIf
2764 '        M_02# = MEndFlg%                                        '検査終了フラグ引渡し
2765 '        M_05# = MNum%                                           '検査番号(検査順1～)
2766 '        'タスク　検査G設定フラグ引渡し
2767 '        If MEndFlg% = 0 Then
2768 '            If 0 < MInspGrNum%(MNum%) Then
2769 '                M_03# = 1
2770 '            Else
2771 '                M_03# = 0
2772 '            EndIf
2773 '        Else
2774 '            M_03# = 0
2775 '        EndIf
2776 '        'タスク　検査結果確認フラグ引渡し
2777 '        If 1 < MNum% Then
2778 '            If 0 < MInspGrNum%(MNum%-1) Then
2779 '                M_04# = 1
2780 '            Else
2781 '                M_04# = 0
2782 '            EndIf
2783 '        Else
2784 '            M_04# = 0
2785 '        EndIf
2786 '        '
2787 '        'タスク処理開始
2788 '        M_00# = 1                                               'TASK処理開始
2789 '        'タスク処理開始確認
2790 '        M_Timer(1) = 0
2791 '        MExitFlg = 0
2792 '        While( MExitFlg = 0 )
2793 '            '処理開始完了確認
2794 '            If M_00# = 0 And M_10# = 8 Then
2795 '                MExitFlg = 1
2796 '            EndIf
2797 '            'timeoutチェック
2798 '            If 2000 < M_Timer(1) Then
2799 '                If MNgContinue% = 1 Then                        'NG続行?
2800 '                    MInspErrNumSub = 36                         'エラー番号設定36
2801 '                Else
2802 '                    MInspErrNum = 36                            'エラー番号設定36
2803 '                EndIf
2804 '                MExitFlg = 1
2805 '            EndIf
2806 '        WEnd
2807 '        '
2808 '        '検査位置へ移動・移動完了待ち
2809 '        If 0 = MInspErrNum Then
2810 '            If MEndFlg% = 0 Then
2811 '                Mvs PInspPos( MNum% )                           '移動
2812 '            EndIf
2813 '        EndIf
2814 '        '
2815 '        'タスク　検査G番号設定・検査完了確認処理終了待ち　INSPTAST1
2816 '        If 0 = MInspErrNum Then
2817 '            M_Timer(1) = 0
2818 '            MExitFlg = 0
2819 '            While( MExitFlg = 0 )
2820 '                '処理完了待ち（正常終了）
2821 '                If M_10# = 1 Then
2822 '                    MExitFlg = 1
2823 '                EndIf
2824 '                '処理完了待ち（異常終了）
2825 '                If M_10# = 0 Then
2826 '                    If MNgContinue% = 1 Then                    'NG続行?
2827 '                        MInspErrNumSub = M_12#                  'エラー番号設定　M12
2828 '                    Else
2829 '                        MInspErrNum = M_12#                     'エラー番号設定　M12
2830 '                    EndIf
2831 '                    MExitFlg = 1
2832 '                EndIf
2833 '                'timeoutチェック
2834 '                If 5000 < M_Timer(1) Then
2835 '                    If MNgContinue% = 1 Then                    'NG続行?
2836 '                        MInspErrNumSub = 31                     'エラー番号設定31
2837 '                    Else
2838 '                        MInspErrNum = 31                        'エラー番号設定31
2839 '                    EndIf
2840 '                    MExitFlg = 1
2841 '                EndIf
2842 '            WEnd
2843 '        EndIf
2844 '        '
2845 '        '検査結果確認
2846 '        If 0 = MInspErrNum Then
2847 '            If 1 < MNum% Then
2848 '                If 0 < MInspGrNum%(MNum%-1) Then                '検査あり?
2849 '                    If M_11# = 2 Then                           '検査NG?
2850 '                        If MNgContinue% = 1 Then                'NG続行?
2851 '                            If MInspNGStepNum = 0 Then          'NG未発生?
2852 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2853 '                            EndIf
2854 '                            MInspErrNumSub = 32                 'エラー番号設定 32:検査NG
2855 '                        Else
2856 ''                            MInspNGStepNum = MNum% - 1          '検査実行NGStep番号設定
2857 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2858 '                            MInspErrNum = 32                    'エラー番号設定 32:検査NG
2859 '                        EndIf
2860 '                   EndIf
2861 '                EndIf
2862 '            EndIf
2863 '        EndIf
2864 '        '
2865 '        'エラーなら検査中断終了するのでLoopから抜けるため終了フラグセット
2866 '        If 0 <> MInspErrNum Then
2867 '            MEndFlg% = 1
2868 '        EndIf
2869 '        '
2870 '        '検査実行、取込完了待ち
2871 '        If 0 = MInspErrNum Then
2872 '            If MEndFlg% = 0 Then
2873 '                If 0 < MInspGrNum%(MNum%) Then                  '検査あり?
2874 '                    M_Out(MOUT_IS_Insp%) = 1                    '検査実行要求on
2875 '                    '取込完了確認
2876 '                    M_Timer(1) = 0
2877 '                    MExitFlg = 0
2878 '                    While( MExitFlg = 0 )
2879 '                        '処理完了待ち
2880 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2881 '                            MExitFlg = 1
2882 '                        EndIf
2883 '                        'timeoutチェック
2884 '                        If 2000 < M_Timer(1) Then
2885 '                            If MNgContinue% = 1 Then            'NG続行?
2886 '                                MInspErrNumSub = 33             'エラー番号設定33
2887 '                            Else
2888 '                                MInspErrNum = 33                'エラー番号設定33
2889 '                            EndIf
2890 '                            MExitFlg = 1
2891 '                        EndIf
2892 '                    WEnd
2893 '                EndIf
2894 '                '
2895 '            EndIf
2896 '        EndIf
2897 '        MNum% = MNum% + 1
2898 '    WEnd
2899 '    '
2900 '    '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2901 '    If 0 < MZAxis% Then
2902 '        PCurrentPos = P_Curr                                    '現在位置取得
2903 '        PCurrentPos.Z = MZAxis%                                 'Z軸を設定
2904 '        Mvs PCurrentPos                                         '現在位置上空へ移動
2905 '    EndIf
2906 '    '
2907 '    'NG続行時処理
2908 '    If MNgContinue% = 1 Then                                    'NG続行?
2909 '        MInspErrNum = MInspErrNumSub                            'エラー番号設定
2910 '    EndIf
2911 '    '
2912 '    '戻り値設定
2913 '    If MInspErrNum = 0 Then
2914 '        ISInspection = 1                                        '正常終了戻り値設定
2915 '    Else
2916 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2917 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2918 '        ISInspection = 0                                        '異常終了戻り値設定
2919 '    EndIf
2920 '    '
2921 '*ISInspection_End
2922 'FEnd
2923 '
2924 '■InitialZoneB
2925 ''' <summary>
2926 ''' 非常停止後の復帰動作
2927 ''' 1)上空退避　Z方向上に移動
2928 ''' 2)J1軸以外を退避ポジションへ移動
2929 ''' 3)J1軸のみを退避ポジションへ移動
2930 ''' 4)イニシャルポジションへ移動
2931 ''' </summary>
2932 ''' <remarks>
2933 ''' Date : 2022/04/08 : N.Watanabe
2934 ''' </remarks>
2935 Function V fnInitialZoneB()
2936     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/04/26 渡辺
2937 '
2938 'パラメータ
2939     Ovrd 5
2940 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2941 '    Cmp Pos, &B100011
2942 '
2943 '復帰動作開始
2944 '
2945 '置き台と両掴みの場所は、チャックを解放する
2946 *RecoveryChuckOpen
2947     PActive = P_Curr          '現在位置を取得
2948     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
2949 'PProductOnRoboSet(ねじロボ製品置き位置)は、チャック解放
2950     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2951         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2952             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2953                 MRecoveryChuckOpen = 1
2954             EndIf
2955         EndIf
2956     EndIf
2957 'PProductOnRoboGet(ねじロボ製品取り位置)は、チャック解放
2958     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2959         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2960             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2961                 MRecoveryChuckOpen = 1
2962             EndIf
2963         EndIf
2964     EndIf
2965 '
2966     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2967     M_Out(12256) = 0                           '本体チャック閉OFF
2968     M_Out(12257) = 1                           '本体チャック開ON
2969 '
2970     M_20# = 0                                  'KEY入力初期化
2971     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
2972     If MRtn = 1 Then M_Out(12257) = 0          '本体チャック開OFF
2973     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2974 '
2975     fErrorProcess(11,244,284,0)
2976     If M_20# = MNext% Then M_20# = MClear%
2977     If M_20# = MAbout% Then GoTo *RecoveryEnd
2978     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2979     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2980 '
2981     *RecoveryChuckOpenEnd
2982 '
2983 '背面板回避
2984 'PPlateBackSet～PPlateBackSet_6のエリアにいるときは、本体チャック開く
2985 '・PPlateBackSet_6         '経路6
2986 '・PPlateBackSet_5         '経路7
2987 '・PPlateBackSet_4         '経路8
2988 '・PPlateBackSet_3         '経路9
2989 '・PPlateBackSet_2         '経路10
2990 '・PPlateBackSet_1         '経路11
2991 '・PPlateBackSet           '背面板置き位置
2992 '上記７点のＸ座標・Ｙ座標・Ｚ座標とJ6軸が下記If文の範囲に入っている事を確認する事
2993     PActive = P_Curr                    '現在位置を取得
2994     JActive = J_Curr                    '現在位置を取得
2995     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2996     If (PActive.X >= -35) And (PActive.X <= -5) Then
2997         If (PActive.Y >= 340) And (PActive.Y <= 510) Then
2998             If (PActive.Z >= 470) And (PActive.Z <= 560) Then
2999                 If (MJ6 >= 160) And (MJ6 <= 200) Then
3000                     M_Out(12256) = 0            '本体チャック閉OFF
3001                     M_Out(12257) = 1            '本体チャック開ON
3002                 Dly 1.0
3003                 EndIf
3004             EndIf
3005         EndIf
3006     EndIf
3007 '
3008 '
3009 '特殊回避　直接、上空退避が出来ない所の対処
3010 '
3011     Ovrd 1
3012 'PProductOnRoboSet(Get)～PProductOnRoboSet(Get)_2のエリアにいるときは、PProductOnRoboSet_2へ
3013 '・PProductOnRoboSet
3014 '・PProductOnRoboSet_1
3015 '・PProductOnRoboSet_2
3016 '・PProductOnRoboGet
3017 '・PProductOnRoboGet_1
3018 '・PProductOnRoboGet_2
3019 '上記６点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
3020     PActive = P_Curr                    '現在位置を取得
3021     JActive = J_Curr                    '現在位置を取得
3022     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
3023     If (PActive.X >= -35) And (PActive.X <= 0) Then
3024         If (PActive.Y >= 350) And (PActive.Y <= 420) Then
3025             If (PActive.Z >= 300) And (PActive.Z <= 450) Then
3026                 If (MJ6 >= -20) And (MJ6 <= 20) Then
3027                     Mvs PProductOnRoboSet_1
3028                     Dly 1.0
3029                     Mvs PProductOnRoboSet_2
3030                     Dly 1.0
3031                     Mov PProductOnRoboSet_3
3032                     Dly 1.0
3033                 EndIf
3034             EndIf
3035         EndIf
3036     EndIf
3037 '
3038 'PProductOnRoboSet(Get)_2～PProductOnRoboSet(Get)_3のエリアにいるときは、PProductOnRoboSet_3へ
3039 '・PProductOnRoboSet_2
3040 '・PProductOnRoboSet_3
3041 '・PProductOnRoboGet_2
3042 '・PProductOnRoboGet_3
3043 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
3044     PActive = P_Curr                    '現在位置を取得
3045     JActive = J_Curr                    '現在位置を取得
3046     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
3047     If (PActive.X >= -35) And (PActive.X <= 0) Then
3048         If (PActive.Y >= 280) And (PActive.Y <= 390) Then
3049             If (PActive.Z >= 410) And (PActive.Z <= 570) Then
3050                 If (MJ6 >= -20) And (MJ6 <= 20) Then
3051                     Mvs PProductOnRoboSet_3
3052                     Dly 1.0
3053                 EndIf
3054             EndIf
3055         EndIf
3056     EndIf
3057 '
3058     Ovrd 5
3059 '
3060 '上空退避
3061     PActive = P_Curr
3062     Pmove = PActive
3063     Pmove.Z = 640           '上空退避する一律の高さ
3064     If PActive.X > 550 Then
3065         Pmove.Z =550        'パレット上に腕を伸ばしているときは640まで上げられない為、例外処置
3066     EndIf
3067     If PActive.Z < Pmove.Z Then
3068         Mvs Pmove
3069     EndIf
3070     Dly 1.0
3071 'J1軸以外を退避ポジションへ移動
3072     JActive = J_Curr
3073     Jmove = JTaihi
3074     Jmove.J1 = JActive.J1        'J1軸は現在値を使用し、JTaihiのポーズを取る
3075     Jmove.J6 = JActive.J6        'J6軸は現在値を使用し、JTaihiのポーズを取る
3076     Mov Jmove
3077     Dly 1.0
3078 'J1軸のみを退避ポジションへ移動
3079     Mov JTaihi
3080     Dly 1.0
3081 'イニシャルポジションへ移動
3082     Mov PInitialPosition
3083     Cmp Off
3084     Ovrd 100
3085 ' ねじロボを初期位置に戻すために強制的に自動運転開始
3086     If M_In(11856) = 0 Then                 ' 停止中のみ
3087         fnAutoScreenComment(501)            ' 状態表示[ネジ締め機自動運転開始中] 2022/04/25 渡辺
3088         M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
3089         MRet = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
3090         If MRet = 0 Then
3091         Else
3092             M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
3093         EndIf
3094     EndIf
3095     M_Out(12262) = 0            '位置決め出OFF
3096     M_Out(12263) = 1            '位置決め戻ON
3097     fErrorProcess(11,253,281,0)
3098 *RecoveryEnd
3099     Exit Function
3100 FEnd
3101 '
3102 '
3103 '■fnAutoScreenComment
3104 ''' <summary>
3105 ''' メイン画面の動作状況表示
3106 ''' コメントD1005の設定
3107 ''' </summary>
3108 '''<param name="McommentD1005%">コメントID</param>
3109 ''' <remarks>
3110 ''' Date   : 2021/07/07 : M.Hayakawa
3111 ''' </remarks>
3112 Function fnAutoScreenComment(ByVal McommentD1005%)
3113     M_Out16(12576) = McommentD1005%
3114 FEnd
3115 '
3116 '■fnRoboPosChk
3117 ''' <summary>
3118 ''' 最後に終了したロボットポジションの確認
3119 ''' </summary>
3120 '''<param name="MINNumber%">入力番号</param>
3121 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3122 '''<param name="MTimeCnt&">タイムアウト時間</param>
3123 ''' PLCに保続した番号を読込み、確認
3124 ''' MRBTOpeGroupNo = 5 が初期位置に設定
3125 '''<returns>整数 0:タイムアウト 1:OK</returns>
3126 ''' <remarks>
3127 ''' Date   : 2021/07/07 : M.Hayakawa
3128 ''' </remarks>
3129 Function M% fnRoboPosChk
3130     fnRoboPosChk = 0
3131     MRet = fnStepRead()
3132     '初期位置でないと判断した場合
3133     'ウィンド画面切換え
3134     If MRBTOpeGroupNo > 5 Then
3135         '下記キー待ちの継続に反応させないため
3136         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3137         Dly 0.2
3138         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3139         Dly 1.5
3140         '
3141         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
3142         '
3143         MLoopFlg% = 1
3144         While MLoopFlg% = 1
3145             '
3146             '
3147             MKeyNumber% = fnKEY_WAIT()
3148             Select MKeyNumber%
3149                 Case Is = MAbout%       '停止
3150                     M_20# = MAbout%
3151                     MLoopFlg% = -1
3152                     Break
3153                 Case Is = MNext%        '次へ
3154                     'MLoopFlg% = -1
3155                     Break
3156                 Case Is = MContinue%    '継続
3157                     M_20# = MContinue%
3158                     MLoopFlg% = -1
3159                     Break
3160                 Default
3161                     Break
3162             End Select
3163         WEnd
3164     EndIf
3165     '
3166     If M_20# = MContinue% Then                              '継続ボタンが押された場合
3167         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
3168         Ovrd 5                                   '低速オーバーライド値設定
3169         Select MRBTOpeGroupNo
3170             Case Is = 5                          '何もしない
3171                 Break
3172             Case Is = 10                         '初期位置へ戻す
3173                 'Mov PTEST001
3174                 Break
3175             Case Is = 15                         '初期位置へ戻す
3176                 'Mov PTEST002
3177                 Dly 0.5
3178                 'Mov PTEST001
3179                 Dly 0.5
3180                 Break
3181             Default
3182                 Break
3183         End Select
3184         '
3185         Ovrd M_NOvrd                            'システムの初期値を設定
3186         M_Out(12364) = 1                        'toPLC_データ保存ON
3187         MRBTOpeGroupNo = 5
3188         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
3189         Dly 1.0
3190         M_Out(12364) = 0                        'toPLC_データ保存OFF
3191         fnRoboPosChk = 1                        '初期位置動作実行
3192         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
3193     EndIf
3194     Exit Function
3195 FEnd
3196 '
3197 '■frInCheck
3198 ''' <summary>
3199 ''' センサーINチェック
3200 ''' </summary>
3201 '''<param name="MINNumber%">入力番号</param>
3202 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3203 '''<param name="MTimeCnt&">タイムアウト時間</param>
3204 '''<returns>整数 0:タイムアウト 1:OK</returns>
3205 ''' <remarks>
3206 ''' Date   : 2021/07/07 : M.Hayakawa
3207 ''' </remarks>
3208 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3209     M_Timer(4) = 0
3210     MloopFlg = 0
3211     While MloopFlg = 0
3212         MCrtTime& = M_Timer(4)
3213         If M_In(MINNumber%) = MCMPFLG% Then
3214             MloopFlg = 1
3215             frInCheck = 1
3216         ElseIf MCrtTime& > MTimeCnt& Then
3217             MloopFlg = 1
3218             frInCheck = 0
3219         EndIf
3220     WEnd
3221 FEnd
3222 '-----------------------------------------------
3223 '
3224 'ねじ締め機通信確認
3225 '
3226 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3227 'fScrewTcomChk = 0　：正常終了
3228 '          　 　 -1 ：異常終了
3229 '-----------------------------------------------
3230 Function M% fScrewTcomChk
3231 *ReCheckScewTcomChk
3232     fScrewTcomChk = 0
3233     '通信確認送信
3234     M_Out(MOUT_ScwT_ComChk%) = MOn%
3235     '通信確認受信待機
3236 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3237     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3238     '通信確認送信終了
3239     M_Out(MOUT_ScwT_ComChk%) = MOff%
3240     If MRtn = 0 Then
3241         fScrewTcomChk = -1
3242     EndIf
3243     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
3244  '
3245 FEnd
3246 '
3247 '
3248 '-----------------------------------------------
3249 '
3250 'ねじ締め開始送信
3251 '
3252 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3253 'fScrewTStart = 0　：正常終了
3254 '           　　-1 ：異常終了
3255 '-----------------------------------------------
3256 Function M% fScrewTStart
3257     fScrewTStart = 0
3258     nRet% = 0
3259     'ねじ締め開始待機を受信
3260 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3261     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3262     If MRtn = 0 Then nRet% = -1
3263     If MRtn = 0 Then GoTo *ScrewStartERROR      '開始できなかった場合ジャンプ
3264     Dly 0.1
3265     'ねじ締め開始受信を送信
3266     M_Out(MOUT_ScwT_ST%) = MOn%
3267     Dly 0.5
3268     'Wait M_In(MTEST_KEY%) = MOn%
3269     'ねじ締め開始送信終了
3270     M_Out(MOUT_ScwT_ST%) = MOff%
3271     '
3272 *ScrewStartERROR
3273     fScrewTStart = nRet%
3274 FEnd
3275 '
3276 '
3277 '
3278 '-----------------------------------------------
3279 '
3280 'ねじ締め完了受信
3281 '
3282 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3283 'fScewTFinish = 0　：正常終了
3284 '          　 　-1 ：異常終了
3285 '-----------------------------------------------
3286 Function M% fScewTFinish
3287 *ReCheckScewTFinish
3288     fScewTFinish = 0
3289     'ねじ締め完了待機を受信
3290 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3291     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3292     If MRtn = 0 Then
3293         fScewTFinish = -1
3294     EndIf
3295     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3296     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3297     Dly 0.1
3298     'ねじ締め完了受信を送信
3299     M_Out(MOUT_ScwT_FinOK%) = MOn%
3300     Dly 0.5                          'とりあえず保持時間0.5msec
3301     'ねじ締め開始送信終了
3302     M_Out(MOUT_ScwT_FinOK%) = MOff%
3303     'Wait M_In(MTEST_KEY%) = MOn%
3304     '
3305 *ScewTFinish_ErrEnd
3306 FEnd
3307 '
3308 '
3309 '-----------------------------------------------
3310 '
3311 '条件xx停止受信
3312 '
3313 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3314 'fScewTCaseStop = 0　：正常終了
3315 '          　   　-1 ：異常終了
3316 '-----------------------------------------------
3317 Function M% fScewTCaseStop(ByVal MCase%())
3318 *ReCheckScewTCaseStop
3319     fScewTCaseStop = 0
3320     '条件xx停止を受信
3321     Wait M_In(MCase%(1)) = MOn%
3322     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3323     If MRtn = 0 Then
3324         fScewTCaseStop = -1
3325     EndIf
3326     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3327     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3328     Dly 0.1
3329     '条件xx停止受信を送信
3330     M_Out(MCase%(2)) = MOn%
3331     Dly 0.5                          'とりあえず保持時間0.5msec
3332     'ねじ締め開始送信終了
3333     M_Out(MCase%(2)) = MOff%
3334 *ScewTCaseStop_ErrEnd
3335     '
3336 FEnd
3337 '
3338 '■fScrewTighenRoboCheck
3339 '<summary>
3340 'ねじロボ監視
3341 '</summary>
3342 '<param name = "MStopNum%"> 停止番号</param>
3343 '<returns>整数 0:ねじロボ異常終了 1:OK </returns>
3344 '<make>
3345 '2021/12/2 中村天哉
3346 '</make>
3347 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3348     fnAutoScreenComment(503)    '状態表示[ねじロボ動作終了待ち] 2022/04/26 渡辺
3349     fScrewTighenRoboCheck = 1
3350     MScrewTighenRoboFlg% = 1    'フラグの初期化
3351     MCheck% = 0
3352     While MScrewTighenRoboFlg% = 1
3353         MCheck% = M_In16(11904)
3354         If M_In(MStopNum%) = 1 Then '停止位置まで来たら
3355             MScrewTighenRoboFlg% = 0 '関数を抜ける
3356             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
3357         EndIf
3358         If MCheck% <> 0 Then
3359             fScrewTighenRoboError(MCheck%)
3360             Select M_20#
3361                 Case MAbout%            '停止が押された場合
3362                     M_Out(12869) = 1 Dly 1.0
3363                     MScrewTighenRoboFlg% = 0
3364                     fScrewTighenRoboCheck = 0   '異常終了
3365                     Break
3366                 Case MNgProcess%        'NGが押された場合
3367                     M_Out(12873) = 1 Dly 1.0
3368                     MScrewTighenRoboFlg% = 0
3369                     fScrewTighenRoboCheck = 0   '異常終了
3370                     Break
3371                 Case MContinue%             'リトライが押された場合
3372                     M_20# = MClear%         'M_20#初期化
3373                     M_Out(12871) = 1 Dly 1.0
3374                     Break
3375                 Case MNext%                 '次へが押された場合
3376                     M_20# = MClear%         'M_20#初期化
3377                     M_Out(12874) = 1 Dly 1.0
3378                     Break
3379             End Select
3380             Dly 0.5
3381         EndIf
3382     WEnd
3383 FEnd
3384 '
3385 '■fScrewTighenRoboError
3386 '<summary>
3387 'ねじロボエラー処理
3388 '</summary>
3389 '<param name = "ErrorCode%"> エラー番号</param>
3390 '<make>
3391 '2021/12/2 中村天哉
3392 '</make>
3393 Function fScrewTighenRoboError(ByVal MErrorCode%)
3394     MErrorScreenCode% = 0
3395     MErrorScreenCode% = MErrorCode% + 300
3396     fErrorProcess(11,MErrorScreenCode%,0,0)
3397 FEnd
3398 '
3399 '■fErrorProcess
3400 '<summary>
3401 'エラー処理
3402 '</summary>
3403 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
3404 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
3405 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
3406 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
3407 '<make>
3408 '2021/11/5 中村天哉
3409 '</make>
3410 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3411     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
3412     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
3413     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
3414     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
3415 *RETRY_ERR_PROCESS
3416      M_20# = MClear%     '初期化
3417 '        'エラー処理記述
3418         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3419 '        'GOT KEY入力待ち
3420         MKeyNumber = fnKEY_WAIT()
3421 '        '
3422         If MKeyNumber = MAbout% Then   '停止を選択した場合
3423             M_20# = MAbout%            'M_20# プログラム間共通外部変数
3424             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3425             Break
3426          '
3427         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
3428             M_20# = MContinue%            'M_20# プログラム間共通外部変数
3429             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3430         '
3431         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
3432             M_20# = MNext%            'M_20# プログラム間共通外部変数
3433             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3434          '
3435         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
3436             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
3437             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3438             Break
3439         '
3440         EndIf
3441         '
3442         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3443 FEnd
3444 '
3445 '■fnTorqueCheck
3446 ''' <summary>
3447 ''' トルクチェック動作用のメイン
3448 ''' </summary>
3449 ''' <remarks>
3450 ''' Date   : 2021/12/21 : H.AJI
3451 ''' </remarks>'
3452 Function M% fnTorqueCheck
3453     'トルクチェック中送信  搬送系停止
3454     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
3455     '
3456     fnTorqueCheck = 0
3457     Ovrd 20
3458     Mov PInitialPosition              '初期位置移動
3459     Ovrd 100
3460     '下記キー待ちの継続に反応させないため
3461     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3462     Dly 0.2
3463     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3464     '
3465     'M6340  トルクチェック受信
3466     'Dly 5.0
3467     M_Out(12340) = 1          'トルクチェック受信 M6340
3468     Dly 1.0
3469     M_Out(12340) = 0
3470     '
3471     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3472     M_Out(12835) = 1                         'ねじロボトルクチェック画面切替
3473    Wait M_In(11843) = 1                         'ねじロボトルクチェック画面切替
3474     M_Out(12835) = 0                         'ねじロボトルクチェック画面切替完了
3475     '
3476     '
3477     MLoopFlg = 1
3478     While MLoopFlg = 1
3479         '
3480         Mov PInitialPosition              '初期位置移動
3481         '
3482         MKeyNumber = fnKEY_WAIT()
3483         Select MKeyNumber
3484             Case Is = 1           '停止
3485                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3486                 Dly 1.0
3487                 M_Out(12343) = 0
3488                 Ovrd 20
3489                 'Mov PTicketRead_1
3490                 M_Out(12840) = 1          'トルクチェック終了
3491                 Wait M_In(11859) = 1      'ねじロボからの終了
3492                 M_Out(12840) = 0          'トルクチェック終了
3493                 Ovrd 100
3494                 M_20# = 1
3495                 MLoopFlg = -1
3496                 Break
3497             Case Is = 2           '次へ
3498                 Break
3499             Case Is = 3           '継続
3500                 Break
3501             Case Is = 4           'トルクチェック開始
3502                 M_Out(12342) = 1          'トルクチェック開始要求受信 M6342
3503                 Dly 1.0
3504                 M_Out(12342) = 0
3505                 If M_In(11862) = 1 Then             'トルクチェッカー確認
3506                     fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3507                     MRet = fnScrewMTorque()           'ねじロボ用トルクチェック
3508                 EndIf
3509                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3510                 'MRet = fnMoveTorquePosi()
3511                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3512                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3513                 Break
3514             Default
3515                 Break
3516         End Select
3517     WEnd
3518     '
3519     'トルクチェック中停止送信
3520     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3521     '
3522     'ロボットの位置を元に戻す
3523     '
3524     '
3525  FEnd
3526  '
3527 '
3528 '
3529 '---------------------------
3530 '
3531 '    メイン画面の表示、非表示設定
3532 '         コメントD1001, D1002, D1003の設定
3533 '           MWindReSet = 0     画面非表示
3534 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3535 '           MWindErrScr = 10    エラー画面 D1001, D1002
3536 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3537 '
3538 '---------------------------
3539 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3540     fnMainScreenOpen = 0
3541     '
3542    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3543         M_Out16(12480) = MCommentD1001            'D1001 コメント
3544     EndIf
3545     '
3546     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3547         M_Out16(12496) = MCommentD1002            'D1002 コメント
3548     EndIf
3549     '
3550     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3551         M_Out16(12512) = MCommentD1003            'D1003 コメント
3552     EndIf
3553     '
3554     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3555     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3556     Dly 0.5
3557     M_Out(12362) = 0                         'ウィンド画面設定
3558 FEnd
3559 '
3560 '■Main
3561 ''' <summary>
3562 ''' トルクチェック実動作
3563 ''' </summary>
3564 ''' <remarks>
3565 ''' Date   : 2021/12/21 : H.AJI
3566 ''' </remarks>'
3567 Function M% fnScrewMTorque
3568     fnScrewMTorque = 0
3569     M_Out(12838) = 1                         'トルクチェック開始1
3570     Wait M_In(11857) = 1                     '受信完了
3571     M_Out(12838) = 0                         'トルクチェック開始1
3572     Dly 2.0
3573 FEnd
3574 '
3575 '----------------------------------------------------------------
3576 'fTimeOutJudge
3577 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3578 '引数
3579 'Address% = 監視アドレス番号
3580 'JudgeFlg% = 対象アドレスの正常終了時の値
3581 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3582 '戻り値 = 0 エラー
3583 '         1 正常終了
3584 '         2 リトライ
3585 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3586 '作成日
3587 '2022/9/20 中村
3588 '----------------------------------------------------------------
3589 '
3590 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3591     fTimeOutJudge = 0
3592     MJudge% = 1
3593     MRtn = 0
3594     M_20# = MClear%
3595     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3596 *TimeOutLoop
3597     If MRtn = 1 Then GoTo *TimeOut
3598         fErrorProcess(11,202,203,0)
3599         If M_20# = MNext% Then GoTo *TimeOutLoop
3600         If M_20# = MContinue% Then MJudge% = 2
3601         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3602 *TimeOut
3603     fTimeOutJudge = MJudge%
3604 '
3605 *JUDGE_ERROR_END
3606 FEnd
3607 '■Main
3608 ''' <summary>
3609 ''' 組立動作用のメイン
3610 ''' </summary>
3611 ''' <remarks>
3612 ''' Date   : 2021/07/07 : M.Hayakawa
3613 ''' </remarks>'
3614 Function Main
3615     MopeNo = M_21#         '外部変数にて動作番号代入
3616     '
3617     If M_Svo=0 Then
3618         Servo On
3619     EndIf
3620     Wait M_Svo=1
3621 '組立スタート日付時刻要求パルスON
3622     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3623 'パトライト操作
3624     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3625     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3626     '
3627     M_20# = 0                                   'KEY入力初期化
3628     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
3629     MRet% = 0
3630 '初期位置の確認と移動
3631 '
3632 '復帰動作　実行・未実行判別      2022/04/08 渡辺 作成
3633     PActive = P_Curr                    '現在位置を取得
3634     MRecoveryPass% = 0
3635     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3636         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3637             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3638                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3639             EndIf
3640         EndIf
3641     EndIf
3642     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3643         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3644             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3645                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3646             EndIf
3647         EndIf
3648     EndIf
3649     If MRecoveryPass% = 0 Then
3650        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3651     EndIf
3652 '
3653 '
3654 '    MRet% = fnRoboPosChk()
3655     If MRet% = 1 Then                           '初期位置の動作を行った場合
3656         fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  'ウィンド画面
3657         MKeyNumber% = fnKEY_WAIT()
3658         Select MKeyNumber%
3659             Case Is = MAbout%       '停止
3660                 M_20# = MAbout%
3661                 MLoopFlg% = -1
3662                 Break
3663             Case Is = MNext%        '次へ
3664                 'MLoopFlg = -1
3665                 Break
3666             Case Is = MContinue%    '継続
3667                 M_20# = MContinue%
3668                 MLoopFlg% = -1
3669                 Break
3670             Default
3671                 Break
3672         End Select
3673     EndIf
3674     '
3675     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3676         M_Out(12364) = 1            'toPLC_データ保存ON
3677 'トルクチェック
3678         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3679             MRet% = fnTorqueCheck()
3680             Break
3681         Else
3682 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
3683 '                MRtn = InspInit()               '画像処理初期化処理
3684 '            EndIf
3685 '
3686             M_20# = MClear%             '初期化
3687 '組立開始
3688             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3689                 fnAssyStart()
3690             Else
3691                 M_20# = MPass%
3692             EndIf
3693 '組立終了日付時刻
3694             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3695             Wait M_In(11572) = 1            '日付取得完了
3696             Dly 0.1
3697             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3698 'リフターユニットへのOUT
3699             '  KEY入力が何もない場合 OKと判断
3700             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3701             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3702 'OK/NGフラグ出力
3703             If M_20# <= 0 Then
3704                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3705             ElseIf M_20# = MPass% Then
3706                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3707             EndIf
3708 'PIASに組立完了書込み
3709             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3710                 If M_20# = MPass% Then
3711                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3712                 Else
3713                     'KEY入力がNGの場合
3714                     If M_20# = MNgProcess% Then
3715                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3716                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3717                         MRet% = fnPiasWrite(MNG%)
3718                        nAssyNgQty = nAssyNgQty + 1
3719                     EndIf
3720                     '
3721                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3722                     If M_20# = MAssyOK% Then
3723                             '-----------------------
3724                             'D732 -> D2600 コピー要求
3725                             M_Out(12566) = 1
3726 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3727                             M_Out(12566) = 0
3728                             '
3729                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3730                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3731                             '基板番号照合(PPは未使用）
3732 '                            MRet% = fnPCBNumberCheck()
3733                         Else
3734                             MRet% = 1
3735                         EndIf
3736                         '
3737                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3738                             If M_20# <> MAbout% Then
3739                                 '工程履歴OK書き込み
3740                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3741                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3742                                 MRet% = fnPiasWrite(MOK%)
3743                                 nAssyOkQty = 0
3744                                 nAssyOkQty = nAssyOkQty + 1
3745                             Else
3746                                 nAssyOkQty = nAssyOkQty + 1
3747                             EndIf
3748                         EndIf
3749                     EndIf
3750 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3751 '                    MRet% = fnPiasWrite(MOK%)
3752                 EndIf
3753             Else
3754                 nAssyOkQty = nAssyOkQty + 1
3755             EndIf
3756             '
3757             '組立終了日付時刻解除
3758             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3759             '投入数、組立OK数、組立NG数書込み
3760 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3761             '
3762 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3763 '                '画像処理終了処理
3764 '                MRtn = InspQuit()
3765 '            EndIf
3766         EndIf
3767         M_Out(12364) = 0                          'toPLC_データ保存OFF
3768     EndIf
3769 'パトライト操作
3770     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3771     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3772 'GOT表示
3773     fnAutoScreenComment(93)  'AUTO画面 工程完了
3774 FEnd
3775 End
3776 '
3777 'おまじないコメント
3778 '絶対削除するな
3779 '
3780 '
3781 '
3782 '
3783 '
3784 '
3785 '
JActive=(96.330,-32.290,124.210,0.000,88.080,180.000,0.000,0.000)
Jmove=(96.330,-46.870,111.640,0.000,80.580,180.000,0.000,0.000)
JTaihi=(0.000,-46.870,111.640,0.000,80.580,0.000,0.000,0.000)
PActive=(602.000,-150.000,550.000,-180.000,0.000,90.000,0.000,0.000)(7,0)
PInitialPosition=(340.000,0.000,580.000,-180.000,0.000,180.000)(7,0)
PMechaGet=(-415.660,-8.740,299.320,179.450,-2.220,176.860)(7,1048577)
PMechaGet_1=(-415.660,-8.740,409.960,179.450,-2.220,176.860)(7,1048577)
PMechaGet_2=(-189.840,-0.010,629.060,-180.000,0.000,-179.990)(7,1)
PMechaGet_3=(0.010,189.840,629.070,-180.000,0.000,90.000)(7,0)
PMechaGet_4=(327.500,0.020,596.240,-179.990,0.000,103.500)(7,0)
PMechaGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaSet1=(159.800,-334.910,320.000,122.020,86.640,31.390)(6,0)
PMechaSet1_1=(159.800,-334.910,340.000,122.020,86.640,31.390)(6,0)
PMechaSet2=(160.190,-334.970,320.180,122.020,86.640,31.360)(6,0)
PMechaSet2_1=(160.190,-334.970,339.970,122.020,86.640,31.360)(6,0)
PMechaSet_2=(162.580,-305.370,557.380,179.470,90.000,89.470)(6,0)
PMechaSet_3=(114.450,-288.220,565.580,180.000,0.000,112.110)(7,0)
PMechaSet_4=(310.110,-0.040,565.560,180.000,0.000,-179.550)(7,0)
PMechaSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
Pmove=(-20.940,188.680,640.000,-180.000,0.000,96.340,0.000,0.000)(7,0)
PPlateBackCheck=(-90.210,513.030,577.720,-180.000,0.000,-90.000)(7,0)
PPlateBackCheck_2=(66.390,429.860,577.750,180.000,0.000,-90.000)(7,0)
PPlateBackCheck_3=(-18.780,286.220,630.880,180.000,0.000,-90.000)(7,0)
PPlateBackCheck_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackCheck_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackCheck_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet=(477.780,104.880,400.930,179.920,-0.070,179.070)(7,0)
PPlateBackGet_1=(477.780,104.880,430.000,179.920,-0.070,179.070)(7,0)
PPlateBackGet_2=(477.780,104.880,560.000,179.920,-0.070,179.070)(7,0)
PPlateBackGet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackPush=(-18.720,415.610,534.990,179.960,0.000,88.990)(7,1048576)
PPlateBackPush_1=(-18.720,400.000,534.990,179.960,0.000,88.990)(7,1048576)
PPlateBackPush_2=(-17.580,398.810,550.570,179.960,0.000,88.990)(7,1048576)
PPlateBackSet=(-17.580,456.120,539.500,-179.730,-11.000,88.560)(7,1048576)
PPlateBackSet_1=(-17.580,449.110,537.790,-179.680,-13.000,88.560)(7,1048576)
PPlateBackSet_10=(-17.620,351.380,478.080,-178.780,-45.000,88.590)(7,1048576)
PPlateBackSet_11=(-17.620,346.000,478.080,-178.780,-45.000,88.590)(7,1048576)
PPlateBackSet_12=(-17.620,345.820,487.160,179.690,-45.000,90.690)(7,1048576)
PPlateBackSet_13=(-17.620,286.220,630.900,-179.820,-0.290,90.490)(7,1048576)
PPlateBackSet_2=(-17.580,435.640,533.420,-179.580,-17.000,88.560)(7,1048576)
PPlateBackSet_3=(-17.580,422.800,528.430,-179.470,-21.000,88.560)(7,1048576)
PPlateBackSet_4=(-17.580,409.820,522.120,-179.370,-25.000,88.560)(7,1048576)
PPlateBackSet_5=(-17.580,397.610,515.400,-179.260,-29.000,88.560)(7,1048576)
PPlateBackSet_6=(-17.580,385.280,508.060,-179.140,-33.000,88.560)(7,1048576)
PPlateBackSet_7=(-17.580,373.780,499.740,-179.030,-37.000,88.590)(7,1048576)
PPlateBackSet_8=(-17.580,363.120,490.440,-178.910,-41.000,88.680)(7,1048576)
PPlateBackSet_9=(-17.580,353.110,479.810,-178.780,-45.000,88.590)(7,1048576)
PProductOnPltGet=(477.970,-98.800,372.680,179.760,-0.070,178.770)(7,0)
PProductOnPltGet_1=(477.970,-98.800,410.000,179.760,-0.070,178.770)(7,0)
PProductOnPltGet_2=(477.970,-98.800,500.000,179.760,-0.070,178.770)(7,0)
PProductOnPltGet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltGet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet=(476.450,-98.290,372.680,179.790,-0.350,178.820)(7,0)
PProductOnPltSet_1=(476.450,-98.290,410.000,179.790,-0.350,178.820)(7,0)
PProductOnPltSet_2=(476.450,-98.290,500.000,179.790,-0.350,178.820)(7,0)
PProductOnPltSet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboGet=(-18.410,403.360,318.120,76.690,88.680,166.880)(6,0)
PProductOnRoboGet_1=(-18.410,403.360,425.200,76.690,88.680,166.880)(6,0)
PProductOnRoboGet_2=(-18.410,307.970,425.200,76.690,88.680,166.880)(6,0)
PProductOnRoboGet_3=(-18.400,300.000,550.000,75.410,88.780,165.600)(6,0)
PProductOnRoboGet_4=(-18.400,300.000,550.000,75.410,88.780,165.600)(6,0)
PProductOnRoboGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboSet=(-18.410,403.360,318.120,76.690,88.680,166.880)(6,0)
PProductOnRoboSet_1=(-18.410,403.360,425.200,76.690,88.680,166.880)(6,0)
PProductOnRoboSet_2=(-18.410,307.970,425.200,76.690,88.680,166.880)(6,0)
PProductOnRoboSet_3=(-18.400,300.000,550.000,75.410,88.780,165.600)(6,0)
PProductOnRoboSet_4=(-18.860,404.690,360.000,-102.740,89.080,-12.360)(6,0)
PProductOnRoboSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPushTilt=(-213.310,564.750,464.130,179.960,-0.020,1.510)(7,0)
PPushTilt_1=(-213.310,564.750,480.780,179.960,-0.020,1.510)(7,0)
PPushTilt_2=(-213.310,564.750,620.000,179.960,-0.020,1.510)(7,0)
PPushTilt_3=(0.020,340.000,610.000,-180.000,-0.010,-91.910)(7,0)
PTemp=(602.000,-150.000,550.000,-180.000,0.000,90.000,0.000,0.000)(7,0)
PTicketRead=(602.000,-150.000,500.000,180.000,0.000,90.000)(7,0)
PTicketRead_1=(602.000,-150.000,550.000,180.000,0.000,90.000)(7,0)
PTicketRead_2=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(1)=(602.000,-150.000,500.000,180.000,0.000,90.000,0.000,0.000)(7,0)
PInspPosition(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(11)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(12)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(13)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(14)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(15)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(16)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(17)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(18)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(19)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(20)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(21)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(22)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(23)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(24)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(25)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(26)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(27)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(28)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(29)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(30)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
