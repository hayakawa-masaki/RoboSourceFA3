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
376 '   BaseUnit6通信確認 CDに伴い削除 2022/07/27 M.H
377 '
378 '
379 ' PIASチケット読込み工程抜け確認
380     M_20# = MClear%                       '初期化
381 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
382 '        MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
383 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
384 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
385 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' メニューへ戻る
386 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NGを工程履歴に書込み次の工程へ
387 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NGを工程履歴に書込み次の工程へ
388 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' 履歴NG, 工程抜け
389 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK以外は組立終了
390 '    EndIf
391 '    '
392 '    '座標移動
393 '    '
394 '    '条件xx停止
395 '    fScewTCaseStop(MScwT_Case5%)
396 '    '
397 '    'ベースユニットKEY
398 '    Wait M_In(MTEST_KEY%) = MOn%
399 '    '
400 '    '再開始
401 '    fScewTReStart()
402 '    '
403 '    '座標移動
404 '    '
405 '    'ねじ締め完了
406 '    Mret% = fScewTFinish()
407 ' ネジ締めテスト終了
408 ' PIASテスト -----------
409 '    MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
410 '    MRet% = fnPiasWrite(MNG%)
411  '   MRet% = fnPCBNumberCheck()
412 ' PIASテスト終了 -------
413 '
414     '組立開始(9/6追加(中村))
415     'プログラム原点
416     Ovrd 100
417     ' ハンド状態初期化(10/29追加M.H)(2/11修正(中村))
418     Cmp Off                     'コンプライアンスモード終了
419     ColChk On                   '衝突検知ON
420     If M_In(11266) Then
421         M_Out(12256) = 0
422         M_Out(12257) = 1
423     EndIf
424     If M_In(11269) Then
425         M_Out(12258) = 0
426         M_Out(12259) = 1
427     EndIf
428     If M_In(11271) Then
429         M_Out(12260) = 0
430         M_Out(12261) = 1
431     EndIf
432     *WAIT_HAND_INI
433     If M_In(11265) = 1 And M_In(11268) = 1 And M_In(11270) = 1 Then GoTo *CompHandIni Else GoTo *WAIT_HAND_INI
434     *CompHandIni
435     M_Out(12257) = 0
436     M_Out(12259) = 0
437     M_Out(12261) = 0
438 '
439 '
440 'Dly 5                           'デバッグ用(22/09/30中村)
441     ' ねじ締め機テスト用 ----------
442     Mret% = fScrewTcomChk()
443     If Mret% = -1 Then GoTo *ASSY_ERROR_END
444     'チケットIDを読む
445     M_Out(12262) = 1 Dly 0.5 '本体位置決め出端ON
446     PTemp = P_Curr
447     MRtn = 0
448 '    If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
449 '        If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
450 '            If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
451 '                MRtn = 1
452 '            EndIf
453 '        EndIf
454 '    EndIf
455 '    If MRtn = 1 Then
456 '        Mov PTicketRead
457 '    Else
458 '        Cnt 1 , 10 , 10
459 '        Mov PInitialPosition
460 '        Mov PTicketRead_1           'チケットID読み取り回避点
461 '        Cnt 0
462 '        Mvs PTicketRead             'ID読み位置
463 '    EndIf
464 '
465 ' 2022/04/12 安全方向へ条件変更 渡辺
466 ' PInitialPosition 在席 MStandby=2
467 ' PTicketRead_1 在席 MStandby=1
468 '
469     MStandby = 0    '待機位置フラグを初期化
470     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
471         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
472             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
473                 MStandby = 2
474             EndIf
475         EndIf
476     EndIf
477     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
478         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
479             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
480                 MStandby = 1
481             EndIf
482         EndIf
483     EndIf
484     If MStandby = 2 Then
485         Mov PTicketRead_1           'チケットID読み取り回避点
486         Cnt 0
487     EndIf
488     If MStandby <> 0 Then GoTo *PositionOK
489     fErrorProcess(11,230,281,0)            '初期位置にいない時はエラーにする
490     If M_20# = MNext% Then GoTo *ASSY_ERROR_END
491     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
492     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
493     If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
494     *PositionOK
495 '
496     Mvs PTicketRead             'ID読み位置
497 ' CDに伴い削除 2022/07/27 M.H
498 '    M_Out(12259) = 0            'DVDメカチャック開OfF
499 '    M_Out(12258) = 1            'DVDメカチャック閉ON
500 '
501     '
502     MRtn = 1        'MRtn初期化
503 *RE_TICKET_READ
504 '    MRtn = fnPiasCheck()               'ID読み取り
505 'PInspPosition(1) = PTicketRead  'IDチケット読取位置
506 'MInspGroup%(1) = 1              '検査G番号
507 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
508 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
509     MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
510     '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
511     '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
512 EndIf
513 If MRtn = 1 Then GoTo *CompRead
514 '
515     'エラー時製品位置決めを解除
516 *RE_ERR_REL_1
517 If M_20# = MContinue% Then M_20# = MRtn
518 M_Out(12263) = 1 Dly 0.5    '製品位置決めを解除
519 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)
520 '
521 If MRtn = 1 Then GoTo *CompErrorRelease
522 MRtn = M_20#        'M_20#一時避難
523 M_20# = MClear%
524 fErrorProcess(11,234,284,0)     '位置決め戻端エラー
525 If M_20# = MContinue% Then GoTo *RE_ERR_REL_1
526 If M_20# = MNext% Then M_20# = MRtn
527 If M_20# = MNgProcess% Then M_20# = MAbout%
528 *CompErrorRelease
529 '
530 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
531 If M_20# = MNext% Then M_20# = MPass%
532 Mvs PTicketRead_1                         '22/04/07 追加 渡辺
533 GoTo *ASSY_ERROR_END
534 *CompRead
535     fScrewTStart()           '処理位置変更2/27中村)
536 '
537     'パレットから製品を取る
538     '
539     *RE_POSITIONING
540     '
541     M_Out(12262) = 1 Dly 0.5 '本体位置決め出端ON
542 '    Wait M_In(11273) = 1     '本体位置決め出端検出
543     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '本体位置決め出端検出
544     If MRtn = 1 Then GoTo *CompPositioning
545     fErrorProcess(11,231,282,0)
546     If M_20# = MNext% Then M_20# = MClear%
547     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
548     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
549     If M_20# = MContinue% Then GoTo *RE_POSITIONING
550     *CompPositioning
551 '
552     Mov PProductOnPltGet_2      '本体受け取り上空回避点
553 '
554 ''    Wait M_In(11888) = 1        'ねじロボ2停止1受信(処理位置変更3/14中村)
555 '    MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
556 '    *RE_ERR_REL_2
557 '    If M_20# = MContinue% Then M_20# = MRtn2
558 '    If MRtn = 0 Then
559 '        MRtn2 = 1       'MRtn2初期化
560 '        M_Out(12263) = 1 Dly 0.5    '製品位置決めを解除
561 '        Mov PInitialPosition  '"イニシャルに戻る動き"
562 '        MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
563 '        If MRtn2 = 0 Then
564 '            MRtn2 = M_20#                   '位置決め戻端エラーならM_20#内部を一時避難
565 '            M_20# = MClear%                 'M_20#初期化
566 '            fErrorProcess(11,234,284,0)     '位置決め戻端エラー
567 '            If M_20# = MNext% Then M_20# = MRtn2        'M_20#に避難した値を代入
568 '                '位置決めエラーが発生して工程を抜ける場合停止処理を行う
569 '            If M_20# = MNgProcess% Then M_20# = MAbout%
570 '            Break
571 '        EndIf
572 '        Break
573 '            EndIf
574 '    If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
575 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
576 '
577 '    Mov PProductOnPltGet_1      '本体受け取り上空
578     '
579     *RE_PLT_GET_1
580     '
581     M_Out(12256) = 0            '本体チャック閉OFF
582     M_Out(12257) = 1            '本体チャック開ON
583     '
584 '    Wait M_In(11265) = 1        '本体チャック開検出
585     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
586     If MRtn = 1 Then GoTo *CompPltGet1
587     fErrorProcess(11,244,284,0)
588     If M_20# = MNext% Then M_20# = MClear%
589     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
590     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
591     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
592     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
593     *CompPltGet1
594     '
595     Mov PProductOnPltGet_1      '本体受け取り上空
596     '
597     Ovrd 25
598 '    Fine 0.05 , P
599     Mvs PProductOnPltGet        '本体受け取り位置
600     Dly 0.1
601     M_Out(12257) = 0            '本体チャック開OFF
602     M_Out(12256) = 1            '本体チャック閉ON
603 '    Fine 0 , P
604     '
605     M_Out(12263) = 1 Dly 0.5                    '本体位置決め戻端ON
606 '    Wait M_In(11274) = 1     '本体位置決め戻端検出
607     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)    '本体位置決め戻端検出
608     If MRtn = 1 Then GoTo *CompPltGet2
609     M_Out(12256) = 0                            '本体チャック閉OFF
610     M_Out(12257) = 1                            '本体チャック開ON
611     Dly 2.0
612     Mvs PProductOnPltGet_1
613     Mov PProductOnPltGet_2
614     fErrorProcess(11,234,284,0)
615     If M_20# = MNext% Then M_20# = MClear%
616     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
617     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
618     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
619     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
620     Mov PProductOnPltGet_1
621     Mvs PProductOnPltGet
622     M_Out(12257) = 0                            '本体チャック開OFF
623     M_Out(12256) = 1                            '本体チャック閉ON
624     Dly 2.0
625     *CompPltGet2
626     '
627 '    Wait M_In(11264) = 1        '本体検出
628     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '本体検出
629     If MRtn = 1 Then GoTo *CompPltGet3
630     M_Out(12256) = 0            '本体チャック閉OFF
631     M_Out(12257) = 1            '本体チャック開ON
632     Dly 2.0
633     Mvs PProductOnPltGet_1
634     Mov PProductOnPltGet_2
635     fErrorProcess(11,252,284,0)
636     If M_20# = MNext% Then M_20# = MClear%
637     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
638     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
639     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
640     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
641     Mov PProductOnPltGet_1
642     Mvs PProductOnPltGet
643     M_Out(12257) = 0            '本体チャック開OFF
644     M_Out(12256) = 1            '本体チャック閉ON
645     Dly 2.0
646     *CompPltGet3
647     '
648 '    Wait M_In(11266) = 1        '本体チャック閉検出
649     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
650     If MRtn = 1 Then GoTo *CompPltGet4
651     M_Out(12256) = 0            '本体チャック閉OFF
652     M_Out(12257) = 1            '本体チャック開ON
653     Dly 2.0
654     Mvs PProductOnPltGet_1
655     Mov PProductOnPltGet_2
656     Dly 0.1
657     M_Out(12257) = 0            '本体チャック開OFF
658     M_Out(12256) = 1            '本体チャック閉ON
659     Dly 3.0
660     fErrorProcess(11,245,284,0)
661     If M_20# = MNext% Then M_20# = MClear%
662     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
663     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
664     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
665     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
666     M_Out(12256) = 0            '本体チャック閉OFF
667     M_Out(12257) = 1            '本体チャック開ON
668     Dly 2.0
669     Mov PProductOnPltGet_1
670     Mvs PProductOnPltGet
671     M_Out(12257) = 0            '本体チャック開OFF
672     M_Out(12256) = 1            '本体チャック閉ON
673     Dly 2.0
674     *CompPltGet4
675     '
676     Dly 0.2                     '念のためディレイ
677     Cnt 1 , 10 , 10
678     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
679     Mvs PProductOnPltGet_1      '本体受け取り上空
680     MRtn = FnCtlValue2(99)       '読書開始信号OFF  2022/04/28 渡辺
681     Ovrd 100
682     Mov PProductOnPltGet_2      '本体受け取り上空回避点
683     '
684     '製品をねじロボ2に置く
685     Mov PProductOnRoboSet_3     '経路
686     Cnt 0
687 '    Wait M_In(11888) = 1        'ねじロボ2停止1受信(処理位置,処理変更3/1中村)
688     MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
689     *RE_ERR_REL_2
690     If MRtn = 0 Then
691         Cnt 0
692         Mov PProductOnPltSet_2
693         Mov PProductOnPltSet_1
694         Mvs PProductOnPltSet
695         M_Out(12256) = 0        '本体チャック閉OFF
696         M_Out(12257) = 1        '本体チャック開ON
697         Dly 2.0
698         Mvs PProductOnPltSet_1
699         Mvs PProductOnPltSet_2
700         Mov PInitialPosition
701     EndIf
702     If MRtn = 0 Then GoTo *ASSY_ERROR_END
703     '
704     *RE_ROBO_SET_1
705     '
706     M_Out(12259) = 0            'DVDメカチャック開OfF
707     M_Out(12258) = 1            'DVDメカチャック閉ON
708 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
709     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
710     If MRtn = 1 Then GoTo *CompRoboSet1
711     fErrorProcess(11,269,284,0)
712     If M_20# = MNext% Then M_20# = MClear%
713     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
714     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
715     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
716     *CompRoboSet1
717 '
718     Mvs PProductOnRoboSet_2     'ねじロボ製品置き上空回避点
719 '    Mvs PProductOnRoboSet_4     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)
720     Ovrd 25
721     Mvs PProductOnRoboSet_1     'ねじロボ製品置き上空
722     Mvs PProductOnRoboSet       'ねじロボ製品置き位置
723     M_Out(12866) = 1 Dly 0.3    'ねじロボ2動作再開(停止1～停止2)
724 '    Wait M_In(11889) = 1        'ねじロボ2停止2受信
725     MScrewRoboNgFlg% = 0
726     MRtn = fScrewTighenRoboCheck(11889)    '停止状態を受信する
727     If MRtn = 0 Then
728         MScrewRoboNgFlg% = 1
729     EndIf
730 '
731     *RE_ROBO_SET_2
732 '
733     M_Out(12256) = 0            '本体チャック閉OFF
734     M_Out(12257) = 1            '本体チャック開ON
735 '    Wait M_In(11265)            '本体チャック開検出
736     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
737     If MRtn = 1 Then GoTo *CompRoboSet2
738     fErrorProcess(11,244,284,0)
739     If M_20# = MNext% Then M_20# = MClear%
740     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
741     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
742     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_2
743     *CompRoboSet2
744     '
745     Mvs PProductOnRoboSet_1     'ねじロボ製品置き上空
746     Mvs PProductOnRoboSet_2     'ねじロボ製品置き上空回避点
747 '    Mvs PProductOnRoboSet_4     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)
748     Ovrd 100
749     Cnt 1 , 10 , 10
750     Mov PProductOnRoboSet_3     '経路
751     '
752     If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition
753     If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
754 '
755 '
756 '
757     '
758     'チルトスライダーを押す
759     Cnt 1 , 10
760     Mov PPushTilt_3             'チルトスライダー押し向き変え回避点
761     Cnt 0
762     Mov PPushTilt_2             'チルトスライダー押し回避点
763     Ovrd 30
764     Mvs PPushTilt_1             'チルトスライダー押し上空
765     Spd 1000
766     Ovrd 5
767     Mvs PPushTilt               'チルトスライダー押し
768     Spd M_NSpd
769     Ovrd 30
770     Cnt 1 , 1 , 1
771     Mvs PPushTilt_1             'チルトスライダー押し上空
772     Cnt 1 , 1 , 10
773     Ovrd 100
774     Mov PPushTilt_2             'チルトスライダー押し回避点
775     '
776     '背面板を取る(コンプライアンスモード実装11/8中村)
777     Mov PPlateBackGet_2         '背面板受け取り上空回避点
778 '    Cnt 1 , 10'暫定
779     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止2～停止3)
780 '    MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する'12/20位置の変更(中村)
781 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
782 '    Mov PPlateBackGet_1         '背面板受け取り上空'暫定
783     Cnt 0
784     '
785     *RE_PLATE_GET
786     '
787     Fine 0.05 , P               'ファイン命令ON
788     Ovrd 25
789     Mvs PPlateBackGet           '背面板受け取り位置
790 '    Dly 0.2                     '一時コメントアウト
791     M_Out(12257) = 0            '本体チャック開OFF
792     M_Out(12256) = 1            '本体チャック閉ON
793     '
794 '    Wait M_In(11266) = 1        '本体チャック閉検出
795     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
796     If MRtn = 1 Then GoTo *CompPlateGet_1
797     M_Out(12256) = 0            '本体チャック閉OFF
798     M_Out(12257) = 1            '本体チャック開ON
799     Mvs PPlateBackGet_1
800     fErrorProcess(11,245,293,0) '284→293に変更6/2中村
801     If M_20# = MNext% Then M_20# = MClear%
802     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
803     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
804     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
805     Mvs PPlateBackGet           '背面板受け取り位置
806     M_Out(12257) = 0            '本体チャック開OFF
807     M_Out(12256) = 1            '本体チャック閉ON
808     *CompPlateGet_1
809     Fine 0 , P                  'ファイン命令OFF
810     '
811     Ovrd 5
812     Accel 25 , 100
813     Dly 0.7                     'ディレイ時間調節中(把持力確保)
814 '    CmpG 0.7,0.7,,,,,,       'X,Y軸ゲインを0.7に変更
815 '    ColChk Off                  '衝突検知OFF
816 '    Cmp Pos , &B11          'X,Y軸コンプライアンスモード開始
817     Mov PPlateBackGet_1         '背面板受け取り上空
818     Cnt 1 , 10 , 10
819 '    Cmp Off                     'コンプライアンスモード終了
820 '    ColChk On                   '衝突検知ON
821     Ovrd 50
822     Mov PPlateBackGet_2         '背面板受け取り上空回避点
823     Ovrd 100
824     Accel 100 , 100
825     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '背面パネルチェック
826     If MRtn = 1 Then GoTo *CompPlateGet_2
827     Cnt 0
828     fErrorProcess(11,267,293,0)                     '284→293に変更6/2中村
829     If M_20# = MNext% Then M_20# = MClear%
830     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
831     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
832     If M_20# = MContinue% Then
833         Mov PPlateBackGet_1
834         Dly 0.3
835         M_Out(12256) = 0            '本体チャック閉OFF
836         M_Out(12257) = 1            '本体チャック開ON
837         Dly 2.0
838     EndIf
839     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
840     *CompPlateGet_2    '
841     '背面板を置く
842 '    Wait M_In(11889) = 1        'ねじロボ2停止2受信
843     ColChk Off
844     Mov PPlateBackSet_13        '背面板置き上空
845     Cnt 1 , 10
846 '
847     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        'もう一度背面パネルチェック
848     If MRtn = 1 Then GoTo *CompPlateGet_3
849     fErrorProcess(11,267,293,0)                     '284→293に変更6/2中村
850     If M_20# = MNext% Then M_20# = MClear%
851     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
852     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
853     If M_20# = MContinue% Then
854         Mov PPlateBackGet_2
855         Mov PPlateBackGet_1
856         M_Out(12256) = 0            '本体チャック閉OFF
857         M_Out(12257) = 1            '本体チャック開ON
858         Dly 2.0
859     EndIf
860     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
861     *CompPlateGet_3
862 '
863 ' CDに伴い削除 2022/07/27 M.H
864 '    ' 部品供給要求送信
865 '    M_Out(12787) = 1
866 '
867     MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する'12/20位置の変更(中村)
868 '    If MRtn = 0 Then Mov PInitialPosition    '"イニシャルに戻る動き"
869     If MRtn = 0 Then GoTo *ASSY_ERROR_END
870 '
871     Mov PPlateBackSet_12        '爪入れ前回避点
872     Cnt 0
873     Ovrd 25
874     Accel 25 , 25
875     Mvs PPlateBackSet_11        '爪入れ込み前
876     Mvs PPlateBackSet_10        '爪入れ込み1
877     Mvs PPlateBackSet_9         '爪入れ込み2
878 '    CmpG 1.0,1.0,1.0,1.0,1.0,1.0,,
879 '    Cmp Pos, &B001000
880     Cnt 1           'コメントアウト
881     Mov PPlateBackSet_8         '経路1
882     Mov PPlateBackSet_7         '経路2
883     Mov PPlateBackSet_6         '経路3
884     Mov PPlateBackSet_5         '経路4
885     Mov PPlateBackSet_4         '経路5
886     Mov PPlateBackSet_3         '経路6
887     Mov PPlateBackSet_2         '経路7
888     Mov PPlateBackSet_1         '経路8
889     Mov PPlateBackSet           '背面板離し位置
890 '    Cmp Off
891     Accel 100 , 100
892     Cnt 0
893     Dly 0.1
894 *RE_PLATE_SET
895     M_Out(12256) = 0            '本体チャック閉OFF
896     M_Out(12257) = 1            '本体チャック開ON
897     '
898 '    Wait M_In(11265)            '本体チャック開検出
899     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
900     If MRtn = 1 Then GoTo *CompPlateSet
901     fErrorProcess(11,244,284,0)
902     If M_20# = MNext% Then M_20# = MClear%
903     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
904     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
905     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
906     *CompPlateSet
907 '-----暫定押し-------------------------------------(22/12/14中村)ここから
908 *RE_BUCK_PUSH
909     M_20# = MClear%
910     Mov PPlateBackPush_2
911 '
912     M_Out(12257) = 0            '本体チャック開OFF
913     M_Out(12256) = 1            '本体チャック閉ON
914 '
915     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
916 '
917     If MRtn = 1 Then GoTo *CompBuckPushSetting  '正常なら押し動作へ
918 '
919     fErrorProcess(11,245,287,0) '閉端センサーNG時エラー表示
920         If M_20# = MNext% Then M_20# = MClear%              '次へが押されたら値を初期化
921         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '停止が押されたらエラーエンドへ
922         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END    'NGが押されたらエラーエンドへ
923         If M_20# = MContinue% Then GoTo *RE_BUCK_PUSH       'リトライが押されたらもう一度閉じる
924 '
925 *CompBuckPushSetting
926 '
927     Mvs PPlateBackPush_1
928     Ovrd 10
929     Mvs PPlateBackPush
930 '    Dly 0.1     'クランプをするので削除(221219中村)
931 '背面クランプここから(12/15)
932     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～停止4)
933     MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
934         If MRtn = 0 Then
935             Mvs PPlateBackPush_1
936             Mov PPlateBackSet_13
937             Mov PInitialPosition    '"イニシャルに戻る動き"
938         EndIf
939         If MRtn = 0 Then GoTo *ASSY_ERROR_END
940 '背面クランプここまで(12/15)
941     Ovrd 50                     '20→50に変更(221219中村)
942     Mvs PPlateBackPush_1
943 *RE_CHUCK_OPEN
944     M_20# = MClear%
945     M_Out(12256) = 0            '本体チャック閉OFF
946     M_Out(12257) = 1            '本体チャック開ON
947     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
948     If MRtn = 1 Then GoTo *CompChuckOpenForBackPush
949     fErrorProcess(11,244,284,0)
950         If M_20# = MNext% Then M_20# = MClear%              '次へが押されたら値を初期化
951         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '停止が押されたらエラーエンドへ
952         If M_20# = MNgProcess% Then GoTo *RE_CHUCK_OPEN    'NGが押されたらエラーエンドへ
953         If M_20# = MContinue% Then GoTo *RE_CHUCK_OPEN       'リトライが押されたらもう一度閉じる
954 *CompChuckOpenForBackPush
955 '-----暫定押し-------------------------------------(22/12/14中村)ここまで
956 '
957     ColChk On
958     Mov PPlateBackSet_13        '背面板置き上空
959     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～停止4)
960     Ovrd 100
961 '
962     '
963 ''    ' 部品供給要求送信(処理位置変更2/27中村)
964 '    M_Out(12787) = 1
965     'ねじロボ製品クランプ固定待ち
966 '    Wait M_In(11891) = 1        'ねじロボ2停止4受信
967 MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
968 'If MRtn = 0 Then Mov PInitialPosition    '"イニシャルに戻る動き"
969 If MRtn = 0 Then GoTo *ASSY_ERROR_END
970     '置き位置画像検査
971 '    Mov PPlateBackCheck_3       '画像検査位置上空
972 '    Mov PPlateBackCheck_2       '通過点
973 '    Mvs PPlateBackCheck         '確認位置
974     '
975     'PInspPosition(2) = PPlateBackCheck
976     'MInspGroup(2) = 2
977     'MRtn = ISInspection(PInspPosition,MInspGroup,2,-1,1)
978     'If MRtn <> 1 Then
979     '   'エラー処理
980     'EndIf
981     '
982 ''    ' 部品供給要求送信
983 '    M_Out(12787) = 1    '処理位置変更
984 '    Mov PPlateBackCheck_3       '画像検査位置上空
985     '
986     'ねじロボ引き込み待ち
987     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～完了)
988     '
989     'DVDメカを取る CDに伴い削除 2022/07/27 M.H
990 *Loop_CW_CCW_S
991     *RE_MECHA_SET_1
992 MRtn = fScrewTighenRoboCheck(11876)    '停止状態を受信する
993 'If MRtn = 0 Then Mov PInitialPosition   '"イニシャルに戻る動き"
994 If MRtn = 0 Then GoTo *ASSY_ERROR_END
995 '
996     *CompRoboGet1
997     '
998 '    Ovrd 50
999     Mov PProductOnRoboGet_3     'ねじロボ製品取り上空回避点2から3へ
1000 '    Ovrd 20
1001     Mvs PProductOnRoboGet_2     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から2へ
1002     Ovrd 20
1003     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
1004     Ovrd 10
1005     Mvs PProductOnRoboGet       'ねじロボ製品取り位置
1006     Dly 0.2
1007 '
1008     *RE_ROBO_GET_2
1009 '
1010     M_Out(12257) = 0            '本体チャック開OFF
1011     M_Out(12256) = 1            '本体チャック閉ON
1012 '
1013 '    Wait M_In(11266) = 1        '本体チャック閉検出
1014     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
1015     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
1016     M_Out(12256) = 0            '本体チャック閉OFF
1017     M_Out(12257) = 1            '本体チャック開ON
1018     Dly 2.0
1019     Mvs PProductOnRoboGet_1
1020     Mvs PProductOnRoboGet_2
1021     Mov PProductOnRoboGet_3
1022     Mov PProductOnRoboGet_4
1023     Mov PInitialPosition
1024     M_Out(12257) = 0            '本体チャック開OFF
1025     M_Out(12256) = 1            '本体チャック閉ON
1026     Dly 1.0
1027     fErrorProcess(11,245,284,0)
1028     If M_20# = MNext% Then MRtn = 1
1029     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1030     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1031     M_Out(12256) = 0            '本体チャック閉OFF
1032     M_Out(12257) = 1            '本体チャック開ON
1033     Dly 2.0
1034     Mov PProductOnRoboGet_4
1035     Mov PProductOnRoboGet_3
1036     Mov PProductOnRoboGet_2
1037     Mvs PProductOnRoboGet_1
1038     Mvs PProductOnRoboGet
1039     If M_20# = MContinue% Or M_20# = MNext% Then GoTo *RE_ROBO_GET_2
1040     *CompRoboGet2
1041     M_20# = MClear%
1042     '
1043     Dly 0.2
1044     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
1045     Ovrd 50
1046     Mvs PProductOnRoboGet_2     'ねじロボ製品取り上空回避点(9/27暫定コメントアウト)12/15コメント解除
1047     Mvs PProductOnRoboGet_3     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から3へ
1048     Ovrd 100
1049     Mov PProductOnRoboGet_4     '経路3から4へ
1050     Cnt 1 , 10 , 10
1051 '
1052     M_Out(12868) = 1 Dly 0.3    'ねじロボ2動作完了を送信
1053     *RE_ROBO_GET_3
1054 ' CDに伴い削除 2022/07/27 M.H
1055 '    M_Out(12258) = 0            'DVDメカチャック閉OFF
1056 '    M_Out(12259) = 1            'DVDメカチャック開ON
1057 ''    Wait M_In(11268) = 1        'DVDメカチャック開検出
1058 '    MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1059 '    If MRtn = 1 Then GoTo *CompRoboGet3
1060 '    fErrorProcess(11,270,284,0)
1061 '    If M_20# = MNext% Then M_20# = MClear%
1062 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1063 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1064 '    If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1065     *CompRoboGet3
1066     '
1067     'パレットへ製品を置く
1068     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1069     Cnt 1 , 10
1070     Mov PProductOnPltSet_1      '本体置き位置上空
1071     Cnt 0
1072     Ovrd 10
1073     Mvs PProductOnPltSet        '本体置き位置
1074     Dly 0.5
1075 '
1076     *RE_PLT_SET
1077 '
1078     M_Out(12256) = 0            '本体チャック閉OFF
1079     M_Out(12257) = 1            '本体チャック開ON
1080 '
1081     Wait M_In(11265) = 1        '本体チャック開検出
1082 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
1083     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
1084     If MRtn = 1 Then GoTo *CompPltSet
1085     fErrorProcess(11,244,284,0)
1086     If M_20# = MNext% Then M_20# = MClear%
1087     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1088         Mvs PProductOnPltSet_1
1089         Mov PProductOnPltSet_2
1090         Mov PInitialPosition
1091     EndIf
1092     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1093     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1094     If M_20# = MContinue% Then GoTo *RE_PLT_SET
1095     *CompPltSet
1096 '
1097     Mvs PProductOnPltSet_1      '本体置き位置上空
1098     Ovrd 100
1099     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1100 '    Mov PInitialPosition        'イニシャルポジション
1101     MRtn = FnCtlValue2(2)          '組立ＯＫ＋１  2022/04/28 渡辺
1102     Mov PTicketRead_1           'チケットID読み取り回避点
1103     MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1104     '
1105     'チケットID書き込み
1106     M_20# = MAssyOK%
1107     *ASSY_ERROR_END
1108     *AssyEnd
1109     *fnAssyStart_FEndPosi
1110 FEnd
1111 '
1112 '■fnPiasCheck
1113 ''' <summary>
1114 ''' PIASチケット読込み
1115 ''' </summary>
1116 ''' <returns>   0 : NG
1117 '''             1 : OK(読込み完了)
1118 ''' </returns>
1119 ''' <remarks>
1120 ''' Date   : 2021/07/07 : M.Hayakawa
1121 ''' </remarks>'
1122 Function M% fnPiasCheck
1123     fnPiasCheck = 0
1124     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1125     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1126 '
1127 *RETRY_PIAS
1128     M_20# = MClear%
1129     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1130     '
1131     '【IDチケット読み込み】
1132     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1133     MInspGroup%(1) = 1              '検査G番号
1134     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1135 '
1136     'エラーの場合
1137     If MRtn <> 1 Then
1138         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1139         If MRtn <> 1 Then
1140             'D720 -> D1300 コピー要求
1141             M_Out(12565) = 1
1142             Dly 0.5
1143             M_Out(12565) = 0
1144             'エラー処理記述
1145             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1146             'GOT KEY入力待ち
1147             MKeyNumber = fnKEY_WAIT()
1148             '
1149             Select MKeyNumber
1150                 Case MNext%         '次へを選択した場合
1151                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1152                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1153                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1154                     Break
1155                 Case MAbout%        '停止を選択した場合
1156                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1157                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1158                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1159                     Break
1160                 Case MNgProcess%    'NGを選択した場合
1161                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1162                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1163                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1164                     Break
1165                 Case MContinue%     '継続を選択した場合
1166                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1167                     M_20# = MContinue%
1168                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1169                     Break
1170             End Select
1171         EndIf
1172     EndIf
1173 '----------D720 -> D1300 コピー要求----------
1174     M_Out(12565) = 1
1175     Dly 0.5
1176     M_Out(12565) = 0
1177 '----------通信確認をする----------
1178     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1179     MRtn = 0                ' 初期化
1180     M_20# = MClear%         ' 初期化
1181     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1182     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1183     If MRtn <> 1 Then
1184         If M_20# = MContinue% Then
1185             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1186         Else
1187             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1188         EndIf
1189     EndIf
1190 '----------工程抜け確認----------
1191     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1192     MRtn = 0                ' 初期化
1193     M_20# = MClear%         ' 初期化
1194     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1195     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1196     If MRtn <> 1 Then
1197         If M_20# = MContinue% Then
1198             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1199         Else
1200             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1201         EndIf
1202     EndIf
1203     '
1204     fnPiasCheck = 1
1205     *fnPiasCheck_End
1206 FEnd
1207 '
1208 '■fnPCComuCheck
1209 ''' <summary>
1210 ''' PC-PLC通信チェック
1211 ''' </summary>
1212 ''' <returns>   0 : NG
1213 '''             1 : OK(読込み完了)
1214 ''' </returns>
1215 ''' <remarks>
1216 ''' Date   : 2021/07/07 : M.Hayakawa
1217 ''' </remarks>'
1218 Function M% fnPCComuCheck
1219     fnPCComuCheck = 0
1220     MJudge% = 0                                  '初期化
1221     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1222     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1223     '
1224     For MStaNo = 0 To 5
1225         '
1226         If M_In(MIN_PIAS_ComOK%) = 1 Then
1227             'PC通信OK(M400)
1228             MJudge% = MOK%
1229             MStaNo = 5
1230             Break
1231         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1232             'toRBT_通信確認time out
1233             MJudge% = MNG%
1234             MCommentD1001 = 15
1235             MCommentD1002 = 21
1236             MStaNo = 5
1237             Break
1238         Else
1239             'toRBT_通信確認time out
1240             MJudge% = MNG%
1241             MCommentD1001 = 14
1242             MCommentD1002 = 21
1243             Break
1244         EndIf
1245     Next MStaNo
1246     '
1247     '上記で返信フラグを受信してからPC通信確認OFF
1248     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1249     '
1250     'エラー画面
1251     If MJudge% <> MOK% Then
1252         M_20# = MClear%     '初期化
1253         'エラー処理記述
1254         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1255         'GOT KEY入力待ち
1256         MKeyNumber = fnKEY_WAIT()
1257         '
1258         If MKeyNumber = MAbout% Then            '停止を選択した場合
1259             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1260             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1261             Break
1262         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1263             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1264             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1265             Break
1266         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1267             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1268             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1269             Break
1270         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1271             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1272             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1273             Break
1274         EndIf
1275     Else
1276         'OKの場合
1277         fnPCComuCheck = 1
1278     EndIf
1279 FEnd
1280 '
1281 '■fnProcessCheck
1282 ''' <summary>
1283 ''' 工程抜け確認
1284 ''' </summary>
1285 ''' <returns>    1：工程履歴OK     0：異常終了
1286 '''             -1：前工程履歴NG  -2：自工程履歴あり
1287 '''             -3：モデル仕向NG  -4：タイムアウト
1288 '''             -5：履歴処理エラー
1289 ''' </returns>
1290 ''' <remarks>
1291 ''' Date   : 2021/07/07 : M.Hayakawa
1292 ''' </remarks>'
1293 Function M% fnProcessCheck
1294     fnProcessCheck = 0
1295     MJudge% = MNG%      '一旦NGを初期化とする
1296 '----------工程抜け確認----------
1297     MCommentD1001 = 0   'コメント初期化
1298     For MStaNo = 0 To 5
1299         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1300         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1301         '
1302         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1303             MJudge% = MOK%
1304             fnAutoScreenComment(85)     ' AUTO画面
1305             MStaNo = 5
1306             Break
1307         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1308             MFlgLoop% = 0
1309             MJudge% = MNG%
1310             MCommentD1001 = 27
1311             MCommentD1002 = 22
1312             fnAutoScreenComment(94)     ' AUTO画面
1313             fnProcessCheck = -2         ' NGは-2を返す
1314             MStaNo = 5
1315             Break
1316         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1317            MJudge% = MNG%
1318             MCommentD1001 = 31
1319             MCommentD1002 = 22
1320             fnAutoScreenComment(83)     ' AUTO画面
1321             fnProcessCheck = -3         ' NGは-3を返す
1322             MStaNo = 5
1323             Break
1324         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1325             '履歴NGは直ぐに終了せず繰り返し確認を行う
1326             '前工程の書込みが終了していない可能性があるため
1327             MJudge% = MNG%
1328             MCommentD1001 = 32
1329             MCommentD1002 = 22
1330             fnAutoScreenComment(84)     ' AUTO画面
1331             fnProcessCheck = -1         ' NGは-1を返す
1332             Dly 1.0
1333             '工程抜け確認OFF
1334             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1335             Dly 1.0
1336            'MStaNo = 5
1337             Break
1338         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1339             MFlgLoop% = 0
1340             MJudge% = MNG%
1341             MCommentD1001 = 29
1342             MCommentD1002 = 22
1343             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1344             fnProcessCheck = -5         ' NGは-5を返す
1345             MStaNo = 5
1346             Break
1347         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1348             MJudge% = MNG%
1349             If MCommentD1001 = 32 Then
1350                 '何もしない
1351             Else
1352                 MCommentD1001 = 26
1353             EndIf
1354             MCommentD1002 = 22
1355             fnProcessCheck = -4         ' NGは-4を返す
1356             MStaNo = 5
1357             Break
1358         Else
1359             MJudge% = MNG%
1360             MCommentD1001 = 28
1361             MCommentD1002 = 22
1362         EndIf
1363     Next MStaNo
1364     '工程抜け確認OFF
1365     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1366     '通過履歴NG 工程抜けの場合
1367     If MJudge% = MPass% Then
1368         M_20# = MPass%
1369     EndIf
1370     '
1371     'エラー画面
1372     If MJudge% <> MOK% Then
1373         M_20# = MClear%     '初期化
1374         'エラー処理記述
1375         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1376         'GOT KEY入力待ち
1377         MKeyNumber = fnKEY_WAIT()
1378         '
1379         Select MKeyNumber
1380             Case MAbout%        '停止を選択した場合
1381                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1382                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1383                 Break
1384             Case MNext%         '次へを選択した場合
1385                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1386                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1387                 Break
1388             Case MContinue%     '継続を選択した場合
1389                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1390                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1391                 Break
1392             Case MNgProcess%    'NGを選択した場合
1393                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1394                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1395                 Break
1396         End Select
1397     Else
1398         fnProcessCheck = 1  ' OKは1を返す
1399     EndIf
1400 FEnd
1401 '
1402 '■fnPiasWrite
1403 ''' <summary>
1404 ''' Pias 組立結果書込み要求
1405 ''' </summary>
1406 '''<param name="MFlg%">
1407 '''                 MOK%(1) = 工程履歴にOKを書込む
1408 '''                 MNG%(0) = 工程履歴にNGを書込む
1409 '''</param>
1410 '''<returns></returns>
1411 ''' <remarks>
1412 ''' Date   : 2021/07/07 : M.Hayakawa
1413 ''' </remarks>'
1414 Function M% fnPiasWrite(ByVal MFlg%)
1415       fnPiasWrite = 0
1416 *RETRY_PIASWRITE
1417     '
1418     '組立OK(MOK%)の場合　M306 ON
1419    '組立NG(MNG%)の場合　M307 ON
1420     If MFlg% = MOK% Then
1421         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1422     Else
1423         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1424     EndIf
1425     Dly 0.1                  '念のため
1426     '
1427     'Piasへ書込み開始 M305 -> ON
1428     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1429     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1430     '
1431     MJudge% = MNG%
1432     '
1433     For MStaNo = 0 To 5
1434         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1435             MJudge% = MOK%
1436             'MRet = fnAutoScreenComment(85)  'AUTO画面
1437             MStaNo = 5
1438             Break
1439         '
1440         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1441             MJudge% = MNG%
1442             'MRet = fnAutoScreenComment(85)  'AUTO画面
1443            MCommentD1001 = 34
1444            MCommentD1002 = 25
1445             MStaNo = 5
1446             Break
1447         '
1448         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1449             MJudge% = MNG%
1450             'MRet = fnAutoScreenComment(85)  'AUTO画面
1451            MCommentD1001 = 35
1452            MCommentD1002 = 25
1453             MStaNo = 5
1454             Break
1455         '
1456         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1457             MJudge% = MNG%
1458             'MRet = fnAutoScreenComment(85)  'AUTO画面
1459            MCommentD1001 = 36
1460            MCommentD1002 = 25
1461             MStaNo = 5
1462             Break
1463         '
1464         Else
1465             MJudge% = MNG%
1466            MCommentD1001 = 42
1467            MCommentD1002 = 25
1468         '
1469         EndIf
1470         '
1471     Next MStaNo
1472     '
1473     'Piasへ書込み開始 M305 -> OfF
1474     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1475     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1476     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1477     '
1478     '
1479     '通過履歴NG 工程抜けの場合
1480     If MJudge% = MPass% Then
1481         M_20# = MPass%
1482     EndIf
1483     '
1484    M_20# = MClear%     '初期化
1485     '
1486     'エラー画面
1487     If MJudge% < MOK% Then
1488     '
1489 '残しておくが現状では使用しないラベル
1490 *RETRY_ERR_WRITE
1491         M_20# = MClear%     '初期化
1492         'エラー処理記述
1493         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1494         'GOT KEY入力待ち
1495         MKeyNumber = fnKEY_WAIT()
1496         '
1497         If MKeyNumber = MAbout% Then   '停止を選択した場合
1498             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1499            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1500             Break
1501         '
1502         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1503             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1504             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1505         '
1506         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1507             M_20# = MPass%            'M_20# プログラム間共通外部変数
1508             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1509         '
1510         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1511             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1512            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1513             Break
1514         '
1515         EndIf
1516         '
1517         If M_20# = MClear% Then *RETRY_ERR_WRITE
1518         '
1519     EndIf
1520     '
1521     If M_20# = MContinue% Then *RETRY_PIASWRITE
1522     '
1523     fnPiasWrite = 1
1524     '
1525 FEnd
1526 '
1527 '■fnPCBNumberCheck
1528 ''' <summary>
1529 ''' Pias 基板番号照合要求
1530 ''' </summary>
1531 '''<param name="%"></param>
1532 '''<param name="%"></param>
1533 '''<returns></returns>
1534 ''' <remarks>
1535 ''' Date   : 2021/07/07 : M.Hayakawa
1536 ''' </remarks>'
1537 Function M% fnPCBNumberCheck
1538       fnPCBNumberCheck = 0
1539     '
1540 *RETRY_PCBCHECK
1541     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1542     'Piasへ基板照合開始 M310 -> ON
1543     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1544     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1545     '
1546     MJudge% = MNG%
1547     '
1548     For MStaNo = 0 To 5
1549         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1550             MJudge% = MOK%
1551             fnAutoScreenComment(96)  'AUTO画面
1552             MStaNo = 5
1553             Break
1554         '
1555         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1556             MJudge% = MNG%
1557             fnAutoScreenComment(97)  'AUTO画面
1558             MCommentD1001 = 37
1559             MCommentD1002 = 25
1560             MStaNo = 5
1561             Break
1562         '
1563         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1564             MJudge% = MNG%
1565             fnAutoScreenComment(98)  'AUTO画面
1566             MCommentD1001 = 38
1567             MCommentD1002 = 25
1568             MStaNo = 5
1569             Break
1570         '
1571         ElseIf M_In(11580) = 1 Then                         'time out
1572             MJudge% = MNG%
1573             fnAutoScreenComment(99)  'AUTO画面
1574             MCommentD1001 = 39
1575             MCommentD1002 = 25
1576             MStaNo = 5
1577             Break
1578         '
1579         Else
1580             MJudge% = MNG%
1581            MCommentD1001 = 41
1582            MCommentD1002 = 25
1583         '
1584         EndIf
1585         '
1586     Next MStaNo
1587     '
1588     'Piasへ基板照合開始 M310 -> OfF
1589     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1590     '
1591     '
1592     '通過履歴NG 工程抜けの場合
1593     If MJudge% = MPass% Then
1594         M_20# = MPass%
1595     EndIf
1596     '
1597    M_20# = MClear%     '初期化
1598     '
1599     'エラー画面
1600     If MJudge% < MOK% Then
1601     '
1602 '残しておくが現状では使用しないラベル
1603 *RETRY_ERR_PCBNUMBER
1604         M_20# = MClear%     '初期化
1605         'エラー処理記述
1606         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1607         'GOT KEY入力待ち
1608         MKeyNumber = fnKEY_WAIT()
1609         '
1610         If MKeyNumber = MAbout% Then   '停止を選択した場合
1611             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1612             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1613             Break
1614         '
1615         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1616             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1617             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1618         '
1619         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1620             M_20# = MPass%            'M_20# プログラム間共通外部変数
1621             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1622         '
1623         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1624             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1625             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1626             Break
1627         '
1628         EndIf
1629         '
1630         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1631         '
1632     EndIf
1633     '
1634     If M_20# = MContinue% Then *RETRY_PCBCHECK
1635 FEnd
1636 '
1637 '■ScrewTight_S2
1638 ''' <summary>
1639 ''' ねじ締めを行う
1640 ''' </summary>
1641 '''<param name="PScrewPos()">
1642 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1643 '''             PScrewPos(2)    ：ねじ締め回避点
1644 '''             PScrewPos(10)   ：ねじ締め終了高さ
1645 '''</param>
1646 '''<returns>整数
1647 '''         0=異常終了、1=正常終了
1648 '''</returns>
1649 ''' <remarks>
1650 ''' Date   : 2021/07/07 : M.Hayakawa
1651 ''' </remarks>'
1652 Function M% ScrewTight_S2(ByVal PScrewPosition())   'ネジ締め個別設定
1653     ScrewTight_S2 = 0
1654     MOKNGFlg = 0
1655     Ovrd 100
1656     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1657     ' 暫定
1658     Ovrd 5
1659     Mvs PScrewPosition(10),-10    ' パレット上ねじ締めS①の上空へ移動
1660 '    Ovrd MOvrdA
1661     '暫定マスク
1662 '    M_Out(Y62_Driver)=1     ' バンクセッティング　C1
1663 '    Dly 0.1
1664 '    M_Out(Y61_Driver)=1     'ドライバーON　CW
1665 '    'Spd 8.3 '外部ライド100  内部ライド60   'ライド100-40　100%：Spd　15　'ねじ締め速度設定
1666 '    Spd MSpdA               'ネジ締め時Spd個別設定
1667     ' 暫定移動のみ
1668     Mvs PScrewPosition(10)
1669 '    '
1670 '    Dly 0.1
1671 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1672 '    Wait M_In(11584)=1          '完了/エラー検出
1673 '    Dly 0.1
1674 '    Spd M_NSpd
1675 '    '
1676 '    If M_In(X28_Driver)=1 Then  'ねじトータルエラー検出時
1677 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1678 '        Dly 0.1
1679 '        M_Out(Y62_Driver)=0     'バンクセッティング解除　C1
1680 '        Dly 0.1
1681 '        M_Out(Y63_Driver)=0     'バンクセッティング解除　C1
1682 '        Dly 0.1
1683 '        M_Out(Y65_Driver)=0     'プログラム解除　F1
1684 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1685 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
1686 '        MOKNGFlg = -1
1687 '        ScrewTight_S2 = 0
1688 '    Else
1689 '        Wait M_In(X29_Driver)=1 ' 正常完了時
1690 '        Dly 0.1
1691 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1692 '        Dly 0.1
1693 '        M_Out(Y62_Driver)=0     'バンクセッティング解除
1694 '        Dly 0.1
1695 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
1696 '        Dly 0.1
1697 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1698 '        ScrewTight_S2 = 1
1699 '    EndIf
1700 ' 暫定
1701     Ovrd 10
1702     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1703     Ovrd 100
1704 FEnd
1705 '
1706 '■ScrewGet_S3
1707 ''' <summary>
1708 ''' ねじ供給機からねじを得る
1709 ''' </summary>
1710 '''<param name="%"></param>
1711 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1712 '''         PScrewPos(2)    ：ねじ供給器回避点
1713 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1714 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1715 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1716 '''<returns>整数
1717 '''         0=異常終了、1=正常終了、-1=MネジセンサーNG、-2=MネジセンサーON、-3=吸着エラー
1718 '''</returns>
1719 ''' <remarks>
1720 ''' Date   : 2021/07/07 : M.Hayakawa
1721 ''' </remarks>'
1722 Function M% ScrewGet_S3(ByVal PScrewPosition())
1723     ScrewGet_S3 = 0
1724     MMScrewJudge% = 0
1725     'ねじ供給器初期動作エラーチェック
1726 ' ↓暫定削除
1727 '    Wait M_In(X34_ScrewReady1)=1 'ねじ供給器SがReadyになるまで待つ　←　何秒か待ってReadyにならなければ抜けるプログラムが必要？
1728 '    Ovrd 100
1729 '    If M_In(X33_SS2)=0 Then  'Mねじ検出センサがOFF（故障）していた場合
1730 '        Ovrd 30
1731 '        Mvs,-80             'その場所から80mm上空へ移動
1732 '        Mov PInitPos19049   '19049初期位置へ移動
1733 '        M_Out(Y6A_VV1)=0    'ねじ吸着 Off
1734 '        'NGとしてここの関数から抜ける
1735 '        ScrewGet_S3 = -1
1736 '        MMScrewJudge% = 1
1737 '        MCommentD1001 = 61
1738 '    EndIf
1739 '    If ScrewGet_S3 = 0 Then
1740 '        'Sタイト用ねじ供給機にMねじが混入していないか監視
1741 '        MMScrewJudge% = 0 'MMScrewJudgeを初期化する
1742 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
1743 '        If MRtn = 0 Then
1744 '            Ovrd 30
1745 '            Mvs,-80            'その場所から50mm上空へ移動
1746 '            Mov PInitPos19049  '19049初期位置へ移動
1747 '            MMScrewJudge% = 2
1748 '            MRtn = All_CLamp_Release()'全てのクランプ解除へ分岐
1749 '            MCnt% = 2   '2を設定
1750 '            MCommentD1001 = 62
1751 '        EndIf
1752 '        If MMScrewJudge% = 2 Then
1753 '            ScrewGet_S3 = -2
1754 '        EndIf
1755 '    EndIf
1756 '    'Mネジ判定がONの場合 NGとして関数を抜ける
1757 '    If MMScrewJudge% = 2 Then
1758 '        ScrewGet_S3 = -2
1759 '    EndIf
1760     'Sネジ用ねじ太郎のMネジ混入確認用ここまで
1761     Ovrd 100
1762     Spd M_NSpd
1763     If MMScrewJudge% = 0 Then
1764         ScrewGet_S3 = 0
1765         M_Out(Y63_Driver)=1         ' バンクセッティング　C2
1766         MScrewCnt% = 0
1767         MFinCnt% = 2
1768 '        For MCnt% = 0 To MFinCnt%
1769             Mov PScrewPosition(2)        ' ねじ供給機回避点
1770             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1771             Ovrd 80
1772             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1773             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
1774             Mvs PScrewPosition(10), 1.2
1775             M_Out(Y6A_VV1)=1        ' ねじ吸着　ON
1776             'ビット回転
1777             M_Out(Y60_Driver)=1
1778             Dly 0.2
1779             '
1780             Ovrd 100
1781             JOvrd M_NJovrd
1782             Spd M_NSpd
1783             'ネジ吸着確認位置移動
1784             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
1785             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
1786             'ビット回転停止
1787             'M_Out(Y60_Driver)=0
1788             '
1789             '1秒間ネジ吸着確認
1790 ' 以下暫定削除
1791 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
1792 '            'MRtn = 0'強制エラー
1793 '            '吸着エラーの場合
1794 '            'ネジをねじ太郎に戻す
1795 '            If MRtn = 0 Then
1796 '                Ovrd 30
1797 '                'ビット回転停止
1798 '                M_Out(Y60_Driver)=0
1799 '                'ネジ供給機上空
1800 '                Mvs PScrewPos(1)
1801 '                '更に上空
1802 '                Mov PScrewPos(1), -75
1803 '                'ネジ捨て位置
1804 '                Mov PScrewFeedS021
1805 '                '吸着OFF
1806 '                M_Out(Y6A_VV1)=0 'ねじ吸着　OFF
1807 '                Dly 0.2
1808 '                '破壊ON
1809 '                M_Out(Y6B_VB1)=1 '真空破壊ON
1810 '                'ビット回転
1811 '                M_Out(Y61_Driver)=1
1812 '                Dly 0.5
1813 '                '
1814 '                Ovrd 100
1815 '                JOvrd M_NJovrd
1816 '                Spd M_NSpd
1817 '                'ドライバーを上下させねじを振り落とす
1818 '                Mov PScrewFeedS021, 10
1819 '                Mov PScrewFeedS021
1820 '                Dly 0.1
1821 '                Mov PScrewFeedS021, 10
1822 '                Mov PScrewFeedS021
1823 '                '
1824 '                'ネジ落ち待ち
1825 '                'ビット回転停止
1826 '                M_Out(Y61_Driver)=0
1827 '                Dly 0.1
1828 '                '破壊OFF
1829 '                M_Out(Y6B_VB1)=0 '真空破壊OFF
1830 '                '
1831 '                '
1832 '                'ねじ落ちたとして、移動更に上空
1833 '                Mov PScrewPos(1), -75
1834 '                Ovrd 100
1835 '                Spd M_NSpd
1836 '                'ネジ供給機上空
1837 '                Mvs PScrewPos(1)
1838 '                '
1839 '                ScrewGet_S3 = -3
1840 '                Break
1841 '                '
1842 '            Else
1843 '                MCnt% = MFinCnt%
1844 '                ScrewGet_S3 = 0
1845 '            EndIf
1846 '        Next  MCnt%
1847         '
1848         Ovrd 100
1849         Spd M_NSpd
1850         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
1851         M_Out(Y60_Driver)=0     ' ビット回転停止
1852         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
1853         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
1854         'もう一度吸着確認
1855 ' 以下暫定削除
1856 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
1857 '        If MRtn = 0 Then      '吸着エラーの場合
1858 '            MCommentD1001 = 94
1859 '            MCommentD1002 = 95
1860 '            ScrewGet_S3 = -3
1861 '        EndIf
1862 '        If MRtn = 1 Then      '吸着OKの場合
1863 '            ScrewGet_S3 = 1
1864 '        EndIf
1865 '        Break
1866     Else
1867         'Mネジ
1868         If MMScrewJudge% = 2 Then
1869             ScrewGet_S3 = -2
1870         EndIf
1871     EndIf
1872 FEnd
1873 '
1874 '■fnKEY_WAIT()
1875 ''' <summary>
1876 ''' GOTからのキー入力待ち
1877 ''' </summary>
1878 '''<returns>1：停止    2：次へ
1879 '''         3：継続    4：トルクチェック開始
1880 '''         5：NG
1881 '''         11：ロボット初期位置1    12：ロボット初期位置2
1882 '''         13：ロボット初期位置3    14：ロボット初期位置4
1883 '''</returns>
1884 ''' <remarks>
1885 ''' Date   : 2021/07/07 : M.Hayakawa
1886 ''' </remarks>'
1887 Function M% fnKEY_WAIT()
1888     fnKEY_WAIT = 0
1889     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
1890     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
1891     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
1892     '下記キー待ちの継続に反応させないため
1893     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
1894     Dly 0.2
1895     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
1896     MLocalLoopFlg=1
1897     While MLocalLoopFlg=1
1898         If M_In(11345) = 1 Then         '停止   M5345
1899             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
1900             fnKEY_WAIT = 1
1901             MLocalLoopFlg=-1
1902             Break
1903         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
1904             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
1905             fnKEY_WAIT = 2
1906             MLocalLoopFlg=-1
1907             Break
1908         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
1909             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
1910             fnKEY_WAIT = 3
1911             MLocalLoopFlg=-1
1912             Break
1913         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
1914             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
1915             fnKEY_WAIT = 4
1916             MLocalLoopFlg=-1
1917             Break
1918         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
1919             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
1920             fnKEY_WAIT = 5
1921             MLocalLoopFlg=-1
1922             Break
1923             '
1924         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
1925             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
1926             fnKEY_WAIT = MRobotInit1%
1927             MLocalLoopFlg=-1
1928             Break
1929             '
1930         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
1931             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
1932             fnKEY_WAIT = MRobotInit2%
1933             MLocalLoopFlg=-1
1934             Break
1935             '
1936         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
1937             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
1938             fnKEY_WAIT = MRobotInit3%
1939             MLocalLoopFlg=-1
1940             Break
1941             '
1942         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
1943             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
1944             fnKEY_WAIT = MRobotInit4%
1945             MLocalLoopFlg=-1
1946             Break
1947             '
1948         Else
1949         EndIf
1950     WEnd
1951     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
1952     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
1953 FEnd
1954 '
1955 '■ fnAUTO_CTL
1956 ''' <summary>
1957 ''' AUTOモードOFF、PLCからの開始待ち
1958 ''' </summary>
1959 ''' <remarks>
1960 ''' Date   : 2021/07/07 : M.Hayakawa
1961 ''' </remarks>
1962 Function M% fnAUTO_CTL
1963     fnAUTO_CTL = 0
1964     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1965     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
1966     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1967     '
1968     If M_Svo=0 Then             'サーボON確認
1969         Servo On
1970     EndIf
1971     Wait M_Svo=1
1972 FEnd
1973 '
1974 '■ fnWindScreenOpen
1975 ''' <summary>
1976 ''' ウィンド画面の表示、非表示設定
1977 ''' </summary>
1978 '''<param name="%"></param>
1979 '''<param name="%"></param>
1980 '''<param name="%"></param>
1981 '''<param name="%"></param>
1982 ''' <remarks>
1983 ''' コメントD1001, D1002, D1003の設定
1984 ''' MWindReSet = 0     画面非表示
1985 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
1986 ''' MWindErrScr = 10    エラー画面 D1001, D1002
1987 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
1988 ''' Date   : 2021/07/07 : M.Hayakawa
1989 ''' </remarks>
1990 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
1991     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
1992         M_Out16(12480) = MCommentD1001            'D1001 コメント
1993     EndIf
1994     '
1995     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
1996         M_Out16(12496) = MCommentD1002            'D1002 コメント
1997     EndIf
1998     '
1999     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2000        M_Out16(12512) = MCommentD1003            'D1003 コメント
2001     EndIf
2002     '
2003     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2004     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2005     Dly 0.5
2006     M_Out(12363) = 0                         'ウィンド画面設定
2007 FEnd
2008 '
2009 '■FnCtlValue2
2010 ''' <summary>
2011 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2012 ''' </summary>
2013 ''' <param name="MCtlNo%"></param>
2014 ''' <remarks>
2015 ''' Date : 2022/04/28 渡辺
2016 ''' </remarks>
2017 '''
2018 '''  1：投入数       ＋１
2019 '''  2：組立ＯＫ数   ＋１
2020 '''  3：組立ＮＧ数   ＋１ (未使用)
2021 '''  4：吸着エラー数 ＋１
2022 ''' 99：読書開始信号 OFF
2023 '''
2024 Function M% FnCtlValue2(ByVal MCtlNo%)
2025     FnCtlValue2 = 1
2026     Select MCtlNo%
2027         Case 1        '投入数＋１
2028             M_Out(12569) = 0             '書込み開始信号OFF
2029             M_Out(12568) = 1             '読込み開始信号ON
2030             MInputQty = M_In16(11600)    '投入数受信
2031             MInputQty = MInputQty + 1    '投入数＋１
2032             M_Out16(12592) = MInputQty   '投入数送信
2033             M_Out(12569) = 1             '書込み開始信号ON
2034             Break
2035             '
2036         Case 2        '組立ＯＫ数＋１
2037             M_Out(12569) = 0             '書込み開始信号OFF
2038             M_Out(12568) = 1             '読込み開始信号ON
2039             MAssyOkQty = M_In16(11616)   '組立OK数受信
2040             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2041             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2042             M_Out(12569) = 1             '書込み開始信号ON
2043             Break
2044             '
2045         Case 4        '吸着エラー数＋１
2046             M_Out(12569) = 0                       '書込み開始信号OFF
2047             M_Out(12568) = 1                       '読込み開始信号ON
2048             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2049             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2050             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2051             M_Out(12569) = 1                       '書込み開始信号ON
2052             Break
2053             '
2054         Case 99        '読書開始信号OFF
2055             M_Out(12568) = 0        '読込み開始信号OFF
2056             M_Out(12569) = 0        '書込み開始信号OFF
2057             Break
2058             '
2059     End Select
2060     Exit Function
2061 FEnd
2062 'Insightによる画像処理検査実行（並列処理なし）
2063 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2064 '-------------------------------------------------------------------------------
2065 'Insightによる画像処理検査実行（並列処理なし）
2066 '   引数
2067 '       PInspPos()      ：検査位置
2068 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2069 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2070 '       MInspCnt%       ：検査位置数
2071 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2072 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2073 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2074 '   戻り値：整数
2075 '       0=異常終了、1=正常終了
2076 '
2077 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2078 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2079 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2080 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2081 '   20200410    :   検査グループ設定Retry追加
2082 '-------------------------------------------------------------------------------
2083     '----- 初期設定 -----
2084     Cnt 0                                                           '移動効率化解除(初期値=0)
2085     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2086 '    Cnt 1,0.1,0.1
2087     '変数宣言・初期化
2088     Def Inte MNum                                                   '検査番号(検査順1～)
2089     MNum% = 1                                                       '検査番号初期値設定
2090     Def Inte MEndFlg                                                '検査終了フラグ
2091     MEndFlg% = 0
2092     '
2093     '検査G番号設定要求・検査実行要求off
2094     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2095     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2096     'エラー番号クリア
2097     MInspErrNum = 0                                                 '検査実行エラー番号
2098     M_Out16(MOUT_InspErrNum) = MInspErrNum
2099     MInspNGStepNum = 0                                              '検査実行NGStep番号
2100     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2101     '
2102     'Insight Ready check?
2103     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2104         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2105         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2106         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2107         ISInspectionSingle = 0                                      '異常終了戻り値設定
2108         Exit Function
2109     EndIf
2110     '
2111     '検査位置数確認
2112     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2113         MInspErrNum = 21                                            '検査データなし 21　引数<1
2114         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2115         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2116         ISInspectionSingle = 0                                      '異常終了戻り値設定
2117         Exit Function
2118     EndIf
2119     '
2120     '
2121     '
2122     '----- メイン処理 -----
2123     '設定された検査位置数分の検査実行
2124     While( MEndFlg% = 0 )
2125         '----- 検査グループ番号設定Retry追加 20200410
2126         MSetGrNumRetryExitFlg = 0
2127         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2128         While( MSetGrNumRetryExitFlg = 0 )
2129         '----- 検査グループ番号設定Retry追加ここまで 20200410
2130             '
2131             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2132             '
2133             '----- 検査グループ番号設定 -----
2134             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2135             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2136             '
2137             '検査位置へ移動・移動完了待ち
2138             Mvs PInspPos( MNum% )                                       '移動
2139             Dly 0.05                                                    '移動完了後Delay
2140             '
2141             '検査グループ番号設定終了確認
2142             M_Timer(1) = 0
2143             MExitFlg = 0
2144             While( MExitFlg = 0 )
2145                 '検査G設定正常終了?
2146                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2147                     MExitFlg = 1
2148                 '
2149                 '検査G設定異常終了?
2150                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2151                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2152                     If MInspErrNum = 0 Then                             '1回目のエラー?
2153                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2154                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2155                     EndIf
2156                     MExitFlg = 1
2157                 '
2158                 'timeoutチェック
2159                 ElseIf 1000 < M_Timer(1) Then
2160                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2161                     If MInspErrNum = 0 Then                             '1回目のエラー?
2162                         MInspErrNum = 12                                'timeout エラー番号=12
2163                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2164                     EndIf
2165                     MExitFlg = 1
2166                 EndIf
2167             WEnd
2168             '
2169             '検査G番号設定要求off
2170             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2171             '
2172             '----- 検査グループ設定Retry追加 20200410
2173             'NGなければ抜ける
2174             If MCurrentStepErr = 0 Then
2175                 MSetGrNumRetryExitFlg = 1
2176             Else
2177                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2178                 If MSetGrNumRetryCnt = 0 Then
2179                     MSetGrNumRetryExitFlg = 1
2180                 Else
2181                     'Retryへ　その前にDelay
2182                     Dly 0.5
2183                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2184                 EndIf
2185             EndIf
2186             '----- 検査グループ設定Retry追加ここまで 20200410
2187             '
2188         WEnd
2189         '
2190         '
2191         '
2192         '----- 検査実行 -----
2193         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2194             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2195                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2196                 MInspRetryExitFlg = 0
2197                 MRetryCnt = 2                                        'Retry回数設定
2198                 While( MInspRetryExitFlg = 0 )
2199                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2200                     '
2201                     '検査完了確認
2202                     MRetryCnt = MRetryCnt - 1
2203                     M_Timer(1) = 0
2204                     MExitFlg = 0
2205                     While( MExitFlg = 0 )
2206                     '検査完了待ち
2207                         '検査OK終了?
2208                         If M_In( MIN_IS_InspOK% ) = 1  Then
2209                             MJudgeOKFlg = 1                         '検査OKフラグON
2210                             MExitFlg = 1
2211                         '
2212                         '検査NG終了?
2213                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2214                             If MInspErrNum = 0 Then                 '1回目のエラー?
2215                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2216                                     MInspErrNum = 32                    '検査NG エラー番号=32
2217                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2218                                 EndIf
2219                             EndIf
2220                             MExitFlg = 1
2221                         '
2222                         '検査異常終了(IS timeout)?
2223                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2224                             If MInspErrNum = 0 Then                 '1回目のエラー?
2225                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2226                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2227                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2228                                 EndIf
2229                             EndIf
2230                             MExitFlg = 1
2231                         '
2232                         'timeoutチェック
2233                         ElseIf 3000 < M_Timer(1) Then
2234                             If MInspErrNum = 0 Then                 '1回目のエラー?
2235                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2236                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2237                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2238                                 EndIf
2239                             EndIf
2240                             MExitFlg = 1
2241                         EndIf
2242                     WEnd
2243                     '
2244                     '検査開始要求off
2245                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2246                     '
2247                     'OKなら抜ける
2248                     If MJudgeOKFlg = 1 Then
2249                         MInspRetryExitFlg = 1
2250                     Else
2251                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2252                         If MRetryCnt = 0 Then
2253                             MInspRetryExitFlg = 1
2254                         Else
2255                             'Retryへ　その前にDelay
2256                             Dly 0.3
2257                         EndIf
2258                     EndIf
2259                     '
2260                 WEnd
2261             EndIf
2262         EndIf
2263         '
2264         '
2265         '
2266         MNum% = MNum% + 1                                           '検査Step+1
2267         '検査終了確認　検査終了フラグセット
2268         If (MInspCnt% < MNum% ) Then
2269             MEndFlg% = 1                                            '検査終了フラグセット
2270         EndIf
2271         'NG発生時続行時処理
2272         If MInspErrNum <> 0 Then                                    'NGあり?
2273             If MNgContinue% <> 1 Then                               'NG続行?
2274                 MEndFlg% = 1                                        '検査終了フラグセット
2275             EndIf
2276         EndIf
2277     WEnd
2278     '
2279     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2280     If 0 < MZAxis% Then
2281         PCurrentPos = P_Curr                                        '現在位置取得
2282         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2283         Mvs PCurrentPos                                             '現在位置上空へ移動
2284     EndIf
2285     Fine 0 , P
2286     '
2287     '戻り値設定
2288     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
2289         ISInspectionSingle = 1                                      '正常終了戻り値設定
2290     Else
2291         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2292         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2293         ISInspectionSingle = 0                                      '異常終了戻り値設定
2294     EndIf
2295     '
2296 FEnd
2297 '
2298 ' ■ISInspection
2299 ''' <summary>
2300 ''' Insightによる画像処理検査実行
2301 ''' </summary>
2302 '''<param name="PInspPos()">検査位置</param>
2303 '''<param name="MInspGrNum%()">検査位置での検査グループ番号（=0：画像検査未実施）</param>
2304 '''             PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2305 '''<param name="MInspCnt%">検査位置数</param>
2306 '''<param name="MZAxis%">終了時のZ軸退避座標（-1:無効）</param>
2307 '''             終了時にZ軸をMZAxisで設定された位置まで上昇させる
2308 '''<param name="MNgContinue%">=1で検査エラー・NG発生時に全Stepの検査を行う</param>
2309 '''<returns>    整数 0=異常終了、1=正常終了</returns>
2310 '''         MInspErrNum     ：異常終了時にエラー番号が設定される
2311 '''         MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される"
2312 ''' <remarks>
2313 ''' Date   : 2021/07/07 : M.Hayakawa
2314 ''' </remarks>
2315 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2316 '    '画像使用確認 0<- 画像確認無しの場合
2317 '    If M_In(11369) = 0 Then            'toRBT_使用確認
2318 '        ISInspection = 1                                        '正常終了戻り値設定
2319 '    EndIf
2320 ''
2321 '    Cnt 0                                                       '移動効率化解除(初期値=0)
2322 '    Fine 0.05,P                                                 '位置決め完了条件設置　0.05mm
2323 '    MNum% = 1                                                   '検査番号初期値設定
2324 '    Def Inte MEndFlg                                            '検査終了フラグ
2325 '    MEndFlg% = 0
2326 '    '
2327 '    'エラー番号クリア
2328 '    MInspErrNumSub = 0                                          '検査実行エラー番号sub
2329 '    MInspErrNum = 0                                             '検査実行エラー番号
2330 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2331 '    MInspNGStepNum = 0                                          '検査実行NGStep番号
2332 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2333 '    '
2334 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready offなら終了
2335 '        MInspErrNum = 20                                        '検査実行エラー番号 20 Insight offline
2336 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2337 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2338 '        ISInspection = 0                                        '異常終了戻り値設定
2339 ''
2340 '    EndIf
2341 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2342 '    '
2343 '    '検査位置数確認
2344 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2345 '        MInspErrNum = 21                                        '検査データなし 21　引数<1
2346 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2347 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2348 '        ISInspection = 0                                        '異常終了戻り値設定
2349 ''
2350 '    EndIf
2351 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2352 '    '
2353 '    '設定された検査位置数分の検査実行
2354 '    While( MEndFlg% = 0 )
2355 '        '検査終了確認　検査終了フラグセット
2356 '        If (MInspCnt% < MNum% ) Then
2357 '            MEndFlg% = 1                                        '検査終了フラグセット
2358 '        EndIf
2359 '        '
2360 '        'タスク　検査G番号設定・検査完了確認処理開始　INSPTAST1
2361 '        If MEndFlg% = 0 Then
2362 '            M_01# = MInspGrNum%(MNum%)                          '検査G番号引渡し
2363 '        EndIf
2364 '        M_02# = MEndFlg%                                        '検査終了フラグ引渡し
2365 '        M_05# = MNum%                                           '検査番号(検査順1～)
2366 '        'タスク　検査G設定フラグ引渡し
2367 '        If MEndFlg% = 0 Then
2368 '            If 0 < MInspGrNum%(MNum%) Then
2369 '                M_03# = 1
2370 '            Else
2371 '                M_03# = 0
2372 '            EndIf
2373 '        Else
2374 '            M_03# = 0
2375 '        EndIf
2376 '        'タスク　検査結果確認フラグ引渡し
2377 '        If 1 < MNum% Then
2378 '            If 0 < MInspGrNum%(MNum%-1) Then
2379 '                M_04# = 1
2380 '            Else
2381 '                M_04# = 0
2382 '            EndIf
2383 '        Else
2384 '            M_04# = 0
2385 '        EndIf
2386 '        '
2387 '        'タスク処理開始
2388 '        M_00# = 1                                               'TASK処理開始
2389 '        'タスク処理開始確認
2390 '        M_Timer(1) = 0
2391 '        MExitFlg = 0
2392 '        While( MExitFlg = 0 )
2393 '            '処理開始完了確認
2394 '            If M_00# = 0 And M_10# = 8 Then
2395 '                MExitFlg = 1
2396 '            EndIf
2397 '            'timeoutチェック
2398 '            If 2000 < M_Timer(1) Then
2399 '                If MNgContinue% = 1 Then                        'NG続行?
2400 '                    MInspErrNumSub = 36                         'エラー番号設定36
2401 '                Else
2402 '                    MInspErrNum = 36                            'エラー番号設定36
2403 '                EndIf
2404 '                MExitFlg = 1
2405 '            EndIf
2406 '        WEnd
2407 '        '
2408 '        '検査位置へ移動・移動完了待ち
2409 '        If 0 = MInspErrNum Then
2410 '            If MEndFlg% = 0 Then
2411 '                Mvs PInspPos( MNum% )                           '移動
2412 '            EndIf
2413 '        EndIf
2414 '        '
2415 '        'タスク　検査G番号設定・検査完了確認処理終了待ち　INSPTAST1
2416 '        If 0 = MInspErrNum Then
2417 '            M_Timer(1) = 0
2418 '            MExitFlg = 0
2419 '            While( MExitFlg = 0 )
2420 '                '処理完了待ち（正常終了）
2421 '                If M_10# = 1 Then
2422 '                    MExitFlg = 1
2423 '                EndIf
2424 '                '処理完了待ち（異常終了）
2425 '                If M_10# = 0 Then
2426 '                    If MNgContinue% = 1 Then                    'NG続行?
2427 '                        MInspErrNumSub = M_12#                  'エラー番号設定　M12
2428 '                    Else
2429 '                        MInspErrNum = M_12#                     'エラー番号設定　M12
2430 '                    EndIf
2431 '                    MExitFlg = 1
2432 '                EndIf
2433 '                'timeoutチェック
2434 '                If 5000 < M_Timer(1) Then
2435 '                    If MNgContinue% = 1 Then                    'NG続行?
2436 '                        MInspErrNumSub = 31                     'エラー番号設定31
2437 '                    Else
2438 '                        MInspErrNum = 31                        'エラー番号設定31
2439 '                    EndIf
2440 '                    MExitFlg = 1
2441 '                EndIf
2442 '            WEnd
2443 '        EndIf
2444 '        '
2445 '        '検査結果確認
2446 '        If 0 = MInspErrNum Then
2447 '            If 1 < MNum% Then
2448 '                If 0 < MInspGrNum%(MNum%-1) Then                '検査あり?
2449 '                    If M_11# = 2 Then                           '検査NG?
2450 '                        If MNgContinue% = 1 Then                'NG続行?
2451 '                            If MInspNGStepNum = 0 Then          'NG未発生?
2452 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2453 '                            EndIf
2454 '                            MInspErrNumSub = 32                 'エラー番号設定 32:検査NG
2455 '                        Else
2456 ''                            MInspNGStepNum = MNum% - 1          '検査実行NGStep番号設定
2457 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2458 '                            MInspErrNum = 32                    'エラー番号設定 32:検査NG
2459 '                        EndIf
2460 '                   EndIf
2461 '                EndIf
2462 '            EndIf
2463 '        EndIf
2464 '        '
2465 '        'エラーなら検査中断終了するのでLoopから抜けるため終了フラグセット
2466 '        If 0 <> MInspErrNum Then
2467 '            MEndFlg% = 1
2468 '        EndIf
2469 '        '
2470 '        '検査実行、取込完了待ち
2471 '        If 0 = MInspErrNum Then
2472 '            If MEndFlg% = 0 Then
2473 '                If 0 < MInspGrNum%(MNum%) Then                  '検査あり?
2474 '                    M_Out(MOUT_IS_Insp%) = 1                    '検査実行要求on
2475 '                    '取込完了確認
2476 '                    M_Timer(1) = 0
2477 '                    MExitFlg = 0
2478 '                    While( MExitFlg = 0 )
2479 '                        '処理完了待ち
2480 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2481 '                            MExitFlg = 1
2482 '                        EndIf
2483 '                        'timeoutチェック
2484 '                        If 2000 < M_Timer(1) Then
2485 '                            If MNgContinue% = 1 Then            'NG続行?
2486 '                                MInspErrNumSub = 33             'エラー番号設定33
2487 '                            Else
2488 '                                MInspErrNum = 33                'エラー番号設定33
2489 '                            EndIf
2490 '                            MExitFlg = 1
2491 '                        EndIf
2492 '                    WEnd
2493 '                EndIf
2494 '                '
2495 '            EndIf
2496 '        EndIf
2497 '        MNum% = MNum% + 1
2498 '    WEnd
2499 '    '
2500 '    '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2501 '    If 0 < MZAxis% Then
2502 '        PCurrentPos = P_Curr                                    '現在位置取得
2503 '        PCurrentPos.Z = MZAxis%                                 'Z軸を設定
2504 '        Mvs PCurrentPos                                         '現在位置上空へ移動
2505 '    EndIf
2506 '    '
2507 '    'NG続行時処理
2508 '    If MNgContinue% = 1 Then                                    'NG続行?
2509 '        MInspErrNum = MInspErrNumSub                            'エラー番号設定
2510 '    EndIf
2511 '    '
2512 '    '戻り値設定
2513 '    If MInspErrNum = 0 Then
2514 '        ISInspection = 1                                        '正常終了戻り値設定
2515 '    Else
2516 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2517 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2518 '        ISInspection = 0                                        '異常終了戻り値設定
2519 '    EndIf
2520 '    '
2521 '*ISInspection_End
2522 'FEnd
2523 '
2524 '■InitialZoneB
2525 ''' <summary>
2526 ''' 非常停止後の復帰動作
2527 ''' 1)上空退避　Z方向上に移動
2528 ''' 2)J1軸以外を退避ポジションへ移動
2529 ''' 3)J1軸のみを退避ポジションへ移動
2530 ''' 4)イニシャルポジションへ移動
2531 ''' </summary>
2532 ''' <remarks>
2533 ''' Date : 2022/04/08 : N.Watanabe
2534 ''' </remarks>
2535 Function V fnInitialZoneB()
2536     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/04/26 渡辺
2537 '
2538 'パラメータ
2539     Ovrd 5
2540 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2541 '    Cmp Pos, &B100011
2542 '
2543 '復帰動作開始
2544 '
2545 '置き台と両掴みの場所は、チャックを解放する
2546 *RecoveryChuckOpen
2547     PActive = P_Curr          '現在位置を取得
2548     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
2549 'PProductOnRoboSet(ねじロボ製品置き位置)は、チャック解放
2550     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2551         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2552             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2553                 MRecoveryChuckOpen = 1
2554             EndIf
2555         EndIf
2556     EndIf
2557 'PProductOnRoboGet(ねじロボ製品取り位置)は、チャック解放
2558     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2559         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2560             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2561                 MRecoveryChuckOpen = 1
2562             EndIf
2563         EndIf
2564     EndIf
2565 '
2566     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2567     M_Out(12256) = 0                           '本体チャック閉OFF
2568     M_Out(12257) = 1                           '本体チャック開ON
2569 '
2570     M_20# = 0                                  'KEY入力初期化
2571     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
2572     If MRtn = 1 Then M_Out(12257) = 0          '本体チャック開OFF
2573     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2574 '
2575     fErrorProcess(11,244,284,0)
2576     If M_20# = MNext% Then M_20# = MClear%
2577     If M_20# = MAbout% Then GoTo *RecoveryEnd
2578     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2579     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2580 '
2581     *RecoveryChuckOpenEnd
2582 '
2583 '背面板回避
2584 'PPlateBackSet～PPlateBackSet_6のエリアにいるときは、本体チャック開く
2585 '・PPlateBackSet_6         '経路6
2586 '・PPlateBackSet_5         '経路7
2587 '・PPlateBackSet_4         '経路8
2588 '・PPlateBackSet_3         '経路9
2589 '・PPlateBackSet_2         '経路10
2590 '・PPlateBackSet_1         '経路11
2591 '・PPlateBackSet           '背面板置き位置
2592 '上記７点のＸ座標・Ｙ座標・Ｚ座標とJ6軸が下記If文の範囲に入っている事を確認する事
2593     PActive = P_Curr                    '現在位置を取得
2594     JActive = J_Curr                    '現在位置を取得
2595     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2596     If (PActive.X >= -35) And (PActive.X <= -5) Then
2597         If (PActive.Y >= 340) And (PActive.Y <= 510) Then
2598             If (PActive.Z >= 470) And (PActive.Z <= 560) Then
2599                 If (MJ6 >= 160) And (MJ6 <= 200) Then
2600                     M_Out(12256) = 0            '本体チャック閉OFF
2601                     M_Out(12257) = 1            '本体チャック開ON
2602                 Dly 1.0
2603                 EndIf
2604             EndIf
2605         EndIf
2606     EndIf
2607 '
2608 '
2609 '特殊回避　直接、上空退避が出来ない所の対処
2610 '
2611     Ovrd 1
2612 'PProductOnRoboSet(Get)～PProductOnRoboSet(Get)_2のエリアにいるときは、PProductOnRoboSet_2へ
2613 '・PProductOnRoboSet
2614 '・PProductOnRoboSet_1
2615 '・PProductOnRoboSet_2
2616 '・PProductOnRoboGet
2617 '・PProductOnRoboGet_1
2618 '・PProductOnRoboGet_2
2619 '上記６点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
2620     PActive = P_Curr                    '現在位置を取得
2621     JActive = J_Curr                    '現在位置を取得
2622     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2623     If (PActive.X >= -35) And (PActive.X <= 0) Then
2624         If (PActive.Y >= 350) And (PActive.Y <= 420) Then
2625             If (PActive.Z >= 300) And (PActive.Z <= 450) Then
2626                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2627                     Mvs PProductOnRoboSet_1
2628                     Dly 1.0
2629                     Mvs PProductOnRoboSet_2
2630                     Dly 1.0
2631                     Mov PProductOnRoboSet_3
2632                     Dly 1.0
2633                 EndIf
2634             EndIf
2635         EndIf
2636     EndIf
2637 '
2638 'PProductOnRoboSet(Get)_2～PProductOnRoboSet(Get)_3のエリアにいるときは、PProductOnRoboSet_3へ
2639 '・PProductOnRoboSet_2
2640 '・PProductOnRoboSet_3
2641 '・PProductOnRoboGet_2
2642 '・PProductOnRoboGet_3
2643 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
2644     PActive = P_Curr                    '現在位置を取得
2645     JActive = J_Curr                    '現在位置を取得
2646     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2647     If (PActive.X >= -35) And (PActive.X <= 0) Then
2648         If (PActive.Y >= 280) And (PActive.Y <= 390) Then
2649             If (PActive.Z >= 410) And (PActive.Z <= 570) Then
2650                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2651                     Mvs PProductOnRoboSet_3
2652                     Dly 1.0
2653                 EndIf
2654             EndIf
2655         EndIf
2656     EndIf
2657 '
2658     Ovrd 5
2659 '
2660 '上空退避
2661     PActive = P_Curr
2662     Pmove = PActive
2663     Pmove.Z = 640           '上空退避する一律の高さ
2664     If PActive.X > 550 Then
2665         Pmove.Z =550        'パレット上に腕を伸ばしているときは640まで上げられない為、例外処置
2666     EndIf
2667     If PActive.Z < Pmove.Z Then
2668         Mvs Pmove
2669     EndIf
2670     Dly 1.0
2671 'J1軸以外を退避ポジションへ移動
2672     JActive = J_Curr
2673     Jmove = JTaihi
2674     Jmove.J1 = JActive.J1        'J1軸は現在値を使用し、JTaihiのポーズを取る
2675     Jmove.J6 = JActive.J6        'J6軸は現在値を使用し、JTaihiのポーズを取る
2676     Mov Jmove
2677     Dly 1.0
2678 'J1軸のみを退避ポジションへ移動
2679     Mov JTaihi
2680     Dly 1.0
2681 'イニシャルポジションへ移動
2682     Mov PInitialPosition
2683     Cmp Off
2684     Ovrd 100
2685 ' ねじロボを初期位置に戻すために強制的に自動運転開始
2686     If M_In(11856) = 0 Then                 ' 停止中のみ
2687         fnAutoScreenComment(501)            ' 状態表示[ネジ締め機自動運転開始中] 2022/04/25 渡辺
2688         M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
2689         MRet = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
2690         If MRet = 0 Then
2691         Else
2692             M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
2693         EndIf
2694     EndIf
2695     M_Out(12262) = 0            '位置決め出OFF
2696     M_Out(12263) = 1            '位置決め戻ON
2697     fErrorProcess(11,253,281,0)
2698 *RecoveryEnd
2699     Exit Function
2700 FEnd
2701 '
2702 '
2703 '■fnAutoScreenComment
2704 ''' <summary>
2705 ''' メイン画面の動作状況表示
2706 ''' コメントD1005の設定
2707 ''' </summary>
2708 '''<param name="McommentD1005%">コメントID</param>
2709 ''' <remarks>
2710 ''' Date   : 2021/07/07 : M.Hayakawa
2711 ''' </remarks>
2712 Function fnAutoScreenComment(ByVal McommentD1005%)
2713     M_Out16(12576) = McommentD1005%
2714 FEnd
2715 '
2716 '■fnRoboPosChk
2717 ''' <summary>
2718 ''' 最後に終了したロボットポジションの確認
2719 ''' </summary>
2720 '''<param name="MINNumber%">入力番号</param>
2721 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2722 '''<param name="MTimeCnt&">タイムアウト時間</param>
2723 ''' PLCに保続した番号を読込み、確認
2724 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2725 '''<returns>整数 0:タイムアウト 1:OK</returns>
2726 ''' <remarks>
2727 ''' Date   : 2021/07/07 : M.Hayakawa
2728 ''' </remarks>
2729 Function M% fnRoboPosChk
2730     fnRoboPosChk = 0
2731     MRet = fnStepRead()
2732     '初期位置でないと判断した場合
2733     'ウィンド画面切換え
2734     If MRBTOpeGroupNo > 5 Then
2735         '下記キー待ちの継続に反応させないため
2736         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2737         Dly 0.2
2738         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2739         Dly 1.5
2740         '
2741         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2742         '
2743         MLoopFlg% = 1
2744         While MLoopFlg% = 1
2745             '
2746             '
2747             MKeyNumber% = fnKEY_WAIT()
2748             Select MKeyNumber%
2749                 Case Is = MAbout%       '停止
2750                     M_20# = MAbout%
2751                     MLoopFlg% = -1
2752                     Break
2753                 Case Is = MNext%        '次へ
2754                     'MLoopFlg% = -1
2755                     Break
2756                 Case Is = MContinue%    '継続
2757                     M_20# = MContinue%
2758                     MLoopFlg% = -1
2759                     Break
2760                 Default
2761                     Break
2762             End Select
2763         WEnd
2764     EndIf
2765     '
2766     If M_20# = MContinue% Then                              '継続ボタンが押された場合
2767         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
2768         Ovrd 5                                   '低速オーバーライド値設定
2769         Select MRBTOpeGroupNo
2770             Case Is = 5                          '何もしない
2771                 Break
2772             Case Is = 10                         '初期位置へ戻す
2773                 'Mov PTEST001
2774                 Break
2775             Case Is = 15                         '初期位置へ戻す
2776                 'Mov PTEST002
2777                 Dly 0.5
2778                 'Mov PTEST001
2779                 Dly 0.5
2780                 Break
2781             Default
2782                 Break
2783         End Select
2784         '
2785         Ovrd M_NOvrd                            'システムの初期値を設定
2786         M_Out(12364) = 1                        'toPLC_データ保存ON
2787         MRBTOpeGroupNo = 5
2788         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
2789         Dly 1.0
2790         M_Out(12364) = 0                        'toPLC_データ保存OFF
2791         fnRoboPosChk = 1                        '初期位置動作実行
2792         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
2793     EndIf
2794     Exit Function
2795 FEnd
2796 '
2797 '■frInCheck
2798 ''' <summary>
2799 ''' センサーINチェック
2800 ''' </summary>
2801 '''<param name="MINNumber%">入力番号</param>
2802 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2803 '''<param name="MTimeCnt&">タイムアウト時間</param>
2804 '''<returns>整数 0:タイムアウト 1:OK</returns>
2805 ''' <remarks>
2806 ''' Date   : 2021/07/07 : M.Hayakawa
2807 ''' </remarks>
2808 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2809     M_Timer(4) = 0
2810     MloopFlg = 0
2811     While MloopFlg = 0
2812         MCrtTime& = M_Timer(4)
2813         If M_In(MINNumber%) = MCMPFLG% Then
2814             MloopFlg = 1
2815             frInCheck = 1
2816         ElseIf MCrtTime& > MTimeCnt& Then
2817             MloopFlg = 1
2818             frInCheck = 0
2819         EndIf
2820     WEnd
2821 FEnd
2822 '-----------------------------------------------
2823 '
2824 'ねじ締め機通信確認
2825 '
2826 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2827 'fScrewTcomChk = 0　：正常終了
2828 '          　 　 -1 ：異常終了
2829 '-----------------------------------------------
2830 Function M% fScrewTcomChk
2831 *ReCheckScewTcomChk
2832     fScrewTcomChk = 0
2833     '通信確認送信
2834     M_Out(MOUT_ScwT_ComChk%) = MOn%
2835     '通信確認受信待機
2836 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
2837     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
2838     '通信確認送信終了
2839     M_Out(MOUT_ScwT_ComChk%) = MOff%
2840     If MRtn = 0 Then
2841         fScrewTcomChk = -1
2842     EndIf
2843     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
2844  '
2845 FEnd
2846 '
2847 '
2848 '-----------------------------------------------
2849 '
2850 'ねじ締め開始送信
2851 '
2852 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2853 'fScrewTStart = 0　：正常終了
2854 '           　　-1 ：異常終了
2855 '-----------------------------------------------
2856 Function M% fScrewTStart
2857     fScrewTStart = 0
2858     nRet% = 0
2859     'ねじ締め開始待機を受信
2860 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
2861     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
2862     If MRtn = 0 Then nRet% = -1
2863     If MRtn = 0 Then GoTo *ScrewStartERROR      '開始できなかった場合ジャンプ
2864     Dly 0.1
2865     'ねじ締め開始受信を送信
2866     M_Out(MOUT_ScwT_ST%) = MOn%
2867     Dly 0.5
2868     'Wait M_In(MTEST_KEY%) = MOn%
2869     'ねじ締め開始送信終了
2870     M_Out(MOUT_ScwT_ST%) = MOff%
2871     '
2872 *ScrewStartERROR
2873     fScrewTStart = nRet%
2874 FEnd
2875 '
2876 '
2877 '
2878 '-----------------------------------------------
2879 '
2880 'ねじ締め完了受信
2881 '
2882 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2883 'fScewTFinish = 0　：正常終了
2884 '          　 　-1 ：異常終了
2885 '-----------------------------------------------
2886 Function M% fScewTFinish
2887 *ReCheckScewTFinish
2888     fScewTFinish = 0
2889     'ねじ締め完了待機を受信
2890 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
2891     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
2892     If MRtn = 0 Then
2893         fScewTFinish = -1
2894     EndIf
2895     If MRtn = 2 Then GoTo *ReCheckScewTFinish
2896     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
2897     Dly 0.1
2898     'ねじ締め完了受信を送信
2899     M_Out(MOUT_ScwT_FinOK%) = MOn%
2900     Dly 0.5                          'とりあえず保持時間0.5msec
2901     'ねじ締め開始送信終了
2902     M_Out(MOUT_ScwT_FinOK%) = MOff%
2903     'Wait M_In(MTEST_KEY%) = MOn%
2904     '
2905 *ScewTFinish_ErrEnd
2906 FEnd
2907 '
2908 '
2909 '-----------------------------------------------
2910 '
2911 '条件xx停止受信
2912 '
2913 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2914 'fScewTCaseStop = 0　：正常終了
2915 '          　   　-1 ：異常終了
2916 '-----------------------------------------------
2917 Function M% fScewTCaseStop(ByVal MCase%())
2918 *ReCheckScewTCaseStop
2919     fScewTCaseStop = 0
2920     '条件xx停止を受信
2921     Wait M_In(MCase%(1)) = MOn%
2922     MRtn = fTimeOutJudge(MCase%(1),MOn%)
2923     If MRtn = 0 Then
2924         fScewTCaseStop = -1
2925     EndIf
2926     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
2927     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
2928     Dly 0.1
2929     '条件xx停止受信を送信
2930     M_Out(MCase%(2)) = MOn%
2931     Dly 0.5                          'とりあえず保持時間0.5msec
2932     'ねじ締め開始送信終了
2933     M_Out(MCase%(2)) = MOff%
2934 *ScewTCaseStop_ErrEnd
2935     '
2936 FEnd
2937 '
2938 '■fScrewTighenRoboCheck
2939 '<summary>
2940 'ねじロボ監視
2941 '</summary>
2942 '<param name = "MStopNum%"> 停止番号</param>
2943 '<returns>整数 0:ねじロボ異常終了 1:OK </returns>
2944 '<make>
2945 '2021/12/2 中村天哉
2946 '</make>
2947 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
2948     fnAutoScreenComment(503)    '状態表示[ねじロボ動作終了待ち] 2022/04/26 渡辺
2949     fScrewTighenRoboCheck = 1
2950     MScrewTighenRoboFlg% = 1    'フラグの初期化
2951     MCheck% = 0
2952     While MScrewTighenRoboFlg% = 1
2953         MCheck% = M_In16(11904)
2954         If M_In(MStopNum%) = 1 Then '停止位置まで来たら
2955             MScrewTighenRoboFlg% = 0 '関数を抜ける
2956             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
2957         EndIf
2958         If MCheck% <> 0 Then
2959             fScrewTighenRoboError(MCheck%)
2960             Select M_20#
2961                 Case MAbout%            '停止が押された場合
2962                     M_Out(12869) = 1 Dly 1.0
2963                     MScrewTighenRoboFlg% = 0
2964                     fScrewTighenRoboCheck = 0   '異常終了
2965                     Break
2966                 Case MNgProcess%        'NGが押された場合
2967                     M_Out(12873) = 1 Dly 1.0
2968                     MScrewTighenRoboFlg% = 0
2969                     fScrewTighenRoboCheck = 0   '異常終了
2970                     Break
2971                 Case MContinue%             'リトライが押された場合
2972                     M_20# = MClear%         'M_20#初期化
2973                     M_Out(12871) = 1 Dly 1.0
2974                     Break
2975                 Case MNext%                 '次へが押された場合
2976                     M_20# = MClear%         'M_20#初期化
2977                     M_Out(12874) = 1 Dly 1.0
2978                     Break
2979             End Select
2980             Dly 0.5
2981         EndIf
2982     WEnd
2983 FEnd
2984 '
2985 '■fScrewTighenRoboError
2986 '<summary>
2987 'ねじロボエラー処理
2988 '</summary>
2989 '<param name = "ErrorCode%"> エラー番号</param>
2990 '<make>
2991 '2021/12/2 中村天哉
2992 '</make>
2993 Function fScrewTighenRoboError(ByVal MErrorCode%)
2994     MErrorScreenCode% = 0
2995     MErrorScreenCode% = MErrorCode% + 300
2996     fErrorProcess(11,MErrorScreenCode%,0,0)
2997 FEnd
2998 '
2999 '■fErrorProcess
3000 '<summary>
3001 'エラー処理
3002 '</summary>
3003 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
3004 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
3005 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
3006 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
3007 '<make>
3008 '2021/11/5 中村天哉
3009 '</make>
3010 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3011     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
3012     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
3013     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
3014     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
3015 *RETRY_ERR_PROCESS
3016      M_20# = MClear%     '初期化
3017 '        'エラー処理記述
3018         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3019 '        'GOT KEY入力待ち
3020         MKeyNumber = fnKEY_WAIT()
3021 '        '
3022         If MKeyNumber = MAbout% Then   '停止を選択した場合
3023             M_20# = MAbout%            'M_20# プログラム間共通外部変数
3024             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3025             Break
3026          '
3027         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
3028             M_20# = MContinue%            'M_20# プログラム間共通外部変数
3029             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3030         '
3031         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
3032             M_20# = MNext%            'M_20# プログラム間共通外部変数
3033             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3034          '
3035         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
3036             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
3037             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3038             Break
3039         '
3040         EndIf
3041         '
3042         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3043 FEnd
3044 '
3045 '■fnTorqueCheck
3046 ''' <summary>
3047 ''' トルクチェック動作用のメイン
3048 ''' </summary>
3049 ''' <remarks>
3050 ''' Date   : 2021/12/21 : H.AJI
3051 ''' </remarks>'
3052 Function M% fnTorqueCheck
3053     'トルクチェック中送信  搬送系停止
3054     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
3055     '
3056     fnTorqueCheck = 0
3057     Ovrd 20
3058     Mov PInitialPosition              '初期位置移動
3059     Ovrd 100
3060     '下記キー待ちの継続に反応させないため
3061     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3062     Dly 0.2
3063     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3064     '
3065     'M6340  トルクチェック受信
3066     'Dly 5.0
3067     M_Out(12340) = 1          'トルクチェック受信 M6340
3068     Dly 1.0
3069     M_Out(12340) = 0
3070     '
3071     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3072     M_Out(12835) = 1                         'ねじロボトルクチェック画面切替
3073    Wait M_In(11843) = 1                         'ねじロボトルクチェック画面切替
3074     M_Out(12835) = 0                         'ねじロボトルクチェック画面切替完了
3075     '
3076     '
3077     MLoopFlg = 1
3078     While MLoopFlg = 1
3079         '
3080         Mov PInitialPosition              '初期位置移動
3081         '
3082         MKeyNumber = fnKEY_WAIT()
3083         Select MKeyNumber
3084             Case Is = 1           '停止
3085                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3086                 Dly 1.0
3087                 M_Out(12343) = 0
3088                 Ovrd 20
3089                 'Mov PTicketRead_1
3090                 M_Out(12840) = 1          'トルクチェック終了
3091                 Wait M_In(11859) = 1      'ねじロボからの終了
3092                 M_Out(12840) = 0          'トルクチェック終了
3093                 Ovrd 100
3094                 M_20# = 1
3095                 MLoopFlg = -1
3096                 Break
3097             Case Is = 2           '次へ
3098                 Break
3099             Case Is = 3           '継続
3100                 Break
3101             Case Is = 4           'トルクチェック開始
3102                 M_Out(12342) = 1          'トルクチェック開始要求受信 M6342
3103                 Dly 1.0
3104                 M_Out(12342) = 0
3105                 If M_In(11862) = 1 Then             'トルクチェッカー確認
3106                     fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3107                     MRet = fnScrewMTorque()           'ねじロボ用トルクチェック
3108                 EndIf
3109                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3110                 'MRet = fnMoveTorquePosi()
3111                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3112                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3113                 Break
3114             Default
3115                 Break
3116         End Select
3117     WEnd
3118     '
3119     'トルクチェック中停止送信
3120     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3121     '
3122     'ロボットの位置を元に戻す
3123     '
3124     '
3125  FEnd
3126  '
3127 '
3128 '
3129 '---------------------------
3130 '
3131 '    メイン画面の表示、非表示設定
3132 '         コメントD1001, D1002, D1003の設定
3133 '           MWindReSet = 0     画面非表示
3134 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3135 '           MWindErrScr = 10    エラー画面 D1001, D1002
3136 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3137 '
3138 '---------------------------
3139 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3140     fnMainScreenOpen = 0
3141     '
3142    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3143         M_Out16(12480) = MCommentD1001            'D1001 コメント
3144     EndIf
3145     '
3146     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3147         M_Out16(12496) = MCommentD1002            'D1002 コメント
3148     EndIf
3149     '
3150     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3151         M_Out16(12512) = MCommentD1003            'D1003 コメント
3152     EndIf
3153     '
3154     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3155     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3156     Dly 0.5
3157     M_Out(12362) = 0                         'ウィンド画面設定
3158 FEnd
3159 '
3160 '■Main
3161 ''' <summary>
3162 ''' トルクチェック実動作
3163 ''' </summary>
3164 ''' <remarks>
3165 ''' Date   : 2021/12/21 : H.AJI
3166 ''' </remarks>'
3167 Function M% fnScrewMTorque
3168     fnScrewMTorque = 0
3169     M_Out(12838) = 1                         'トルクチェック開始1
3170     Wait M_In(11857) = 1                     '受信完了
3171     M_Out(12838) = 0                         'トルクチェック開始1
3172     Dly 2.0
3173 FEnd
3174 '
3175 '
3176 '----------------------------------------------------------------
3177 'fTimeOutJudge
3178 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3179 '引数
3180 'Address% = 監視アドレス番号
3181 'JudgeFlg% = 対象アドレスの正常終了時の値
3182 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3183 '戻り値 = 0 エラー
3184 '         1 正常終了
3185 '         2 リトライ
3186 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3187 '作成日
3188 '2022/9/20 中村
3189 '----------------------------------------------------------------
3190 '
3191 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3192     fTimeOutJudge = 0
3193     MJudge% = 1
3194     MRtn = 0
3195     M_20# = MClear%
3196     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3197 *TimeOutLoop
3198     If MRtn = 1 Then GoTo *TimeOut
3199         fErrorProcess(11,202,203,0)
3200         If M_20# = MNext% Then GoTo *TimeOutLoop
3201         If M_20# = MContinue% Then MJudge% = 2
3202         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3203 *TimeOut
3204     fTimeOutJudge = MJudge%
3205 '
3206 *JUDGE_ERROR_END
3207 FEnd
3208 '■Main
3209 ''' <summary>
3210 ''' 組立動作用のメイン
3211 ''' </summary>
3212 ''' <remarks>
3213 ''' Date   : 2021/07/07 : M.Hayakawa
3214 ''' </remarks>'
3215 Function Main
3216     MopeNo = M_21#         '外部変数にて動作番号代入
3217     '
3218     If M_Svo=0 Then
3219         Servo On
3220     EndIf
3221     Wait M_Svo=1
3222 '組立スタート日付時刻要求パルスON
3223     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3224 'パトライト操作
3225     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3226     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3227     '
3228     M_20# = 0                                   'KEY入力初期化
3229     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
3230     MRet% = 0
3231 '初期位置の確認と移動
3232 '
3233 '復帰動作　実行・未実行判別      2022/04/08 渡辺 作成
3234     PActive = P_Curr                    '現在位置を取得
3235     MRecoveryPass% = 0
3236     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3237         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3238             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3239                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3240             EndIf
3241         EndIf
3242     EndIf
3243     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3244         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3245             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3246                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3247             EndIf
3248         EndIf
3249     EndIf
3250     If MRecoveryPass% = 0 Then
3251        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3252     EndIf
3253 '
3254     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3255         M_Out(12364) = 1            'toPLC_データ保存ON
3256 'トルクチェック
3257         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3258             MRet% = fnTorqueCheck()
3259             Break
3260         Else
3261 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
3262 '                MRtn = InspInit()               '画像処理初期化処理
3263 '            EndIf
3264 '
3265             M_20# = MClear%             '初期化
3266 '組立開始
3267             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3268                 fnAssyStart()
3269             Else
3270                 M_20# = MPass%
3271             EndIf
3272 '組立終了日付時刻
3273             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3274             Wait M_In(11572) = 1            '日付取得完了
3275             Dly 0.1
3276             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3277 'リフターユニットへのOUT
3278             '  KEY入力が何もない場合 OKと判断
3279             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3280             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3281 'OK/NGフラグ出力
3282             If M_20# <= 0 Then
3283                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3284             ElseIf M_20# = MPass% Then
3285                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3286             EndIf
3287 'PIASに組立完了書込み
3288             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3289                 If M_20# = MPass% Then
3290                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3291                 Else
3292                     'KEY入力がNGの場合
3293                     If M_20# = MNgProcess% Then
3294                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3295                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3296                         MRet% = fnPiasWrite(MNG%)
3297                        nAssyNgQty = nAssyNgQty + 1
3298                     EndIf
3299                     '
3300                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3301                     If M_20# = MAssyOK% Then
3302                             '-----------------------
3303                             'D732 -> D2600 コピー要求
3304                             M_Out(12566) = 1
3305 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3306                             M_Out(12566) = 0
3307                             '
3308                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3309                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3310                             '基板番号照合(PPは未使用）
3311 '                            MRet% = fnPCBNumberCheck()
3312                         Else
3313                             MRet% = 1
3314                         EndIf
3315                         '
3316                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3317                             If M_20# <> MAbout% Then
3318                                 '工程履歴OK書き込み
3319                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3320                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3321                                 MRet% = fnPiasWrite(MOK%)
3322                                 nAssyOkQty = 0
3323                                 nAssyOkQty = nAssyOkQty + 1
3324                             Else
3325                                 nAssyOkQty = nAssyOkQty + 1
3326                             EndIf
3327                         EndIf
3328                     EndIf
3329 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3330 '                    MRet% = fnPiasWrite(MOK%)
3331                 EndIf
3332             Else
3333                 nAssyOkQty = nAssyOkQty + 1
3334             EndIf
3335             '
3336             '組立終了日付時刻解除
3337             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3338             '投入数、組立OK数、組立NG数書込み
3339 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3340             '
3341 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3342 '                '画像処理終了処理
3343 '                MRtn = InspQuit()
3344 '            EndIf
3345         EndIf
3346         M_Out(12364) = 0                          'toPLC_データ保存OFF
3347     EndIf
3348 'パトライト操作
3349     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3350     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3351 'GOT表示
3352     fnAutoScreenComment(93)  'AUTO画面 工程完了
3353 FEnd
3354 End
3355 '
3356 'おまじないコメント
3357 '絶対削除するな
3358 '
3359 '
3360 '
3361 '
3362 '
3363 '
3364 '
JActive=(93.470,-14.830,113.090,0.170,81.440,182.960,0.000,0.000)
Jmove=(93.470,-46.870,111.640,0.000,80.580,182.960,0.000,0.000)
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
