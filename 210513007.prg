1 ' ===================================
2 '
3 '  210512003 STEP5 Assy3プログラム
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
213 Def Inte MSuctionErrQty     '吸着エラー数 演算変数 2022/04/27 渡辺
214 Def Inte nAssyOkQty         '未使用
215 Def Inte MScrewNo
216 Def Inte MReTry
217 '===== <IO変数定義> =====
218 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
219 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
220 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
221 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
222 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
223 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
224 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
225 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
226 '
227 Def Inte Y68_VV1            ' アーム先端　ネジ吸着バルブ
228 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
229 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
230 '
231 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
232 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
233 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
234 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
235 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
236 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
237 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
238 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
239 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
240 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
241 '
242 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
243 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
244 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
245 '
246 Def Inte MOUT_LED1          ' 画像処理用LED照明
247 '
248 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
249 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
250 '
251 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
252 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
253 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
254 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
255 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
256 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
257 '
258 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
259 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
260 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
261 '
262 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
263 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
264 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
265 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
266 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
267 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
268 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
269 Y68_VV1%    =  12248    ' アーム先端　ネジ吸着バルブ '数値12250から12248へ変更(8/5中村)
270 Y6B_VB1%    =  12250    'アーム先端　吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
271 MOUT_VB1%   =  12250    ' アーム先端　ネジ吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
272 '
273 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
274 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
275 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
276 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
277 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
278 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
279 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
280 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
281 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
282 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
283 '
284 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
285 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
286 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
287 '
288 MOUT_LED1%  =  12239    ' 画像処理用LED照明
289 '
290 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
291 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
292 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
293 '
294 '共通
295 Def Inte MTEST_KEY                      'デバックテスト用
296 Def Inte MOn                            '出力=1
297 Def Inte MOff                           '出力=0
298 '
299 'ねじ締め装置_出力アドレス
300 Def Inte MOUT_ScwT_ComChk               '通信確認
301 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
302 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
303 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
304 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
305 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
306 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
307 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
308 'ねじ締め装置_入力アドレス
309 Def Inte MIN_ScwT_comOK                 '通信確認返信
310 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
311 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
312 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
313 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
314 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
315 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
316 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
317 '
318 Dim MScwT_Case1%(2)               '条件1停止変数
319 Dim MScwT_Case2%(2)               '条件2停止変数
320 Dim MScwT_Case3%(2)               '条件3停止変数
321 Dim MScwT_Case4%(2)               '条件4停止変数
322 Dim MScwT_Case5%(2)               '条件5停止変数
323 '
324 '共通
325 MTEST_KEY% = 11359                       'デバッグ用テストKEY
326 MOn% = 1                                 '出力 = 1
327 MOff% = 0                                '出力 = 0
328 '
329 'ねじ締め機_アドレス設定
330 MOUT_ScwT_ComChk% = 12832               '通信確認送信
331 MOUT_ScwT_ST% = 12865                   'ねじ締め開始を送信
332 MOUT_ScwT_ReSTOK% = 12866               '再開始受信を送信
333 MOUT_ScwT_FinOK% = 12868                'ねじ締め完了受信を送信
334 MOUT_ScwT_Case1OK% = 12874              '条件1停止受信を送信
335 MOUT_ScwT_Case2OK% = 12875              '条件2停止受信を送信
336 MOUT_ScwT_Case3OK% = 12876              '条件3停止受信を送信
337 MOUT_ScwT_Case4OK% = 12877              '条件4停止受信を送信
338 MOUT_ScwT_Case5OK% = 12878              '条件5停止受信を送信
339 '
340 MIN_ScwT_comOK% = 11840                 'ねじ締め装置から返信
341 MIN_ScwT_STRec% = 11873                 'ねじ締め開始を受信
342 MIN_ScwT_ReST% = 11874                  '再開始を受信
343 MIN_ScwT_Fin% = 11876                   'ねじ締め完了を受信
344 MIN_ScwT_Case1% = 11882                 '条件1停止待機を受信
345 MIN_ScwT_Case2% = 11883                 '条件2停止待機を受信
346 MIN_ScwT_Case3% = 11884                 '条件3停止待機を受信
347 MIN_ScwT_Case4% = 11885                 '条件4停止待機を受信
348 MIN_ScwT_Case5% = 11886                 '条件5停止待機を受信
349 '
350 MScwT_Case1%(1) = MIN_ScwT_Case1%
351 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
352 MScwT_Case2%(1) = MIN_ScwT_Case2%
353 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
354 MScwT_Case3%(1) = MIN_ScwT_Case3%
355 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
356 MScwT_Case4%(1) = MIN_ScwT_Case4%
357 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
358 MScwT_Case5%(1) = MIN_ScwT_Case5%
359 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
360 '
361 '設定 InitialZoneBで使用する変数
362 Def Pos PActive       '直交座標系 位置変数 現在位置
363 Def Pos Pmove         '直交座標系 位置変数 移動先
364 Def Jnt JActive       '関節座標系 位置変数 現在位置
365 Def Jnt Jmove         '関節座標系 位置変数 移動先
366 Def Jnt JTaihi        '関節座標系 位置変数 退避ポジション ティーチングで設定
367 Def Inte MRecoveryPass      '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行
368 Def Inte MStandby              '待機位置確認フラグ
369 Def Inte MRecoveryChuckOpen    'チャック解放フラグ（復帰動作前）両掴み対策
370 '★注意★初期位置を変更した時には、変更が必要！
371 '
372 '
373 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
374 Function M% fnAssyStart
375     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/27 渡辺
376     M_20# = MClear%                       '初期化
377 ''    'ねじ締め開始(処理位置変更2/16中村)
378 '    fScewTStart()
379 '
380 ' 組立開始
381 'プログラム原点            '(追加ここから(8/30中村))
382 Ovrd 100
383 '
384 'ハンドに本体,側板がないか確認
385 *RE_INITIAL_CHECK
386 If M_20# = MContinue% Then M_20# = MClear%
387 '
388 If M_In(11264) = 0 Then GoTo *CompInitial_1
389 fErrorProcess(11,253,281,0)
390 If M_20# = MNext% Then M_20# = MClear%
391 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
392 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
393 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
394 *CompInitial_1
395 '
396 If M_In(11267) =0 And M_In(11272)= 0 Then GoTo *CompInitial_2
397 fErrorProcess(11,255,281,0)
398 If M_20# = MNext% Then M_20# = MClear%
399 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
400 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
401 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
402 *CompInitial_2
403 '
404 'ハンドをイニシャルに戻す
405 If M_In(11266) = 1 Then             '本体チャック閉検出
406     M_Out(12256) = 0                '本体チャック閉OFF
407     M_Out(12257) = 1                '本体チャック開ON
408     Break
409 EndIf
410 If M_In(11269) = 1 Then             '側板Lシリンダー出検出
411     M_Out(12258) = 0                '側板Lシリンダー出OFF
412     M_Out(12259) = 1                '側板Lシリンダー戻ON
413     Break
414 EndIf
415 If M_In(11274) = 1 Then             '側板Rシリンダー出検出
416     M_Out(12260) = 0                '側板Rシリンダー出OFF
417     M_Out(12261) = 1                '側板Rシリンダー戻ON
418     Break
419 EndIf
420 M_Out(12262) = 0                    '側板チャック閉OFF
421 M_Out(12263) = 1                    '側板チャック開ON
422 '
423 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)'本体チャック開検出
424 If MRtn = 1 Then GoTo *CompInitial_3
425 fErrorProcess(11,244,281,0)
426 If M_20# = MNext% Then M_20# = MClear%
427 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
428 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
429 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
430 *CompInitial_3
431 '
432 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)'側板L戻検出
433 If MRtn = 1 Then GoTo *CompInitial_4
434 fErrorProcess(11,247,281,0)
435 If M_20# = MNext% Then M_20# = MClear%
436 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
437 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
438 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
439 *CompInitial_4
440 '
441 MRtn = frInCheck(11273,1,MSETTIMEOUT05&)'側板R戻検出
442 If MRtn = 1 Then GoTo *CompInitial_5
443 fErrorProcess(11,249,281,0)
444 If M_20# = MNext% Then M_20# = MClear%
445 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
446 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
447 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
448 *CompInitial_5
449 '
450 '初期位置を設定
451 PTemp = P_Curr
452 MRtn = 0
453 'If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
454 '    If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
455 '        If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
456 '            MRtn = 1
457 '            Break
458 '        EndIf
459 '        Break
460 '    EndIf
461 '    Break
462 'EndIf
463 'If MRtn = 1 Then
464 '    M_Out(12265) = 0            '位置決め戻OFF
465 '    M_Out(12264) = 1            '位置決め出ON
466 '    Mov PTicketRead
467 '    Break
468 'Else
469 '    Mov PInitialPosition
470 '    M_Out(12265) = 0            '位置決め戻OFF
471 '    M_Out(12264) = 1            '位置決め出ON
472 '    Mov PTicketRead_1           'チケットID読み取り回避点
473 '    Mvs PTicketRead             'ID読み位置
474 '    Break
475 'EndIf
476 '
477 ' 2022/04/12 安全方向へ条件変更 渡辺
478 ' PInitialPosition 在席 MStandby=2
479 ' PTicketRead_1 在席 MStandby=1
480 '
481 MStandby = 0    '待機位置フラグを初期化
482 If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
483     If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
484         If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
485             MStandby = 2
486         EndIf
487     EndIf
488 EndIf
489 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
490     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
491         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
492             MStandby = 1
493         EndIf
494     EndIf
495 EndIf
496 If MStandby = 2 Then
497     M_Out(12265) = 0            '位置決め戻OFF
498     M_Out(12264) = 1            '位置決め出ON
499     Mov PTicketRead_1           'チケットID読み取り回避点
500     Mvs PTicketRead             'ID読み位置
501     Break
502 EndIf
503 If MStandby = 1 Then
504     M_Out(12265) = 0            '位置決め戻OFF
505     M_Out(12264) = 1            '位置決め出ON
506     Mvs PTicketRead             'ID読み位置
507     Break
508 EndIf
509 If MStandby <> 0 Then GoTo *PositionOK
510 fErrorProcess(11,230,281,0)           '初期位置にいない時はエラーにする
511 If M_20# = MNext% Then GoTo *ASSY_ERROR_END
512 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
513 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
514 If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
515 *PositionOK
516 '
517 MRtn = 1        'MRtn初期化
518 'チケットIDを読む
519 *RE_TICKET_READ
520 M_20# = MClear%                       '初期化
521 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
522     MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
523     '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
524     '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
525 EndIf
526 If MRtn = 1 Then GoTo *CompRead
527 'fErrorProcess(11,244,284,0)
528 If M_20# = MNext% Then M_20# = MClear%
529 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
530 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
531 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
532 'If M_20# = MNext% Then M_20# = MPass%
533 Mov PTicketRead_1
534 Mov PInitialPosition
535 GoTo *ASSY_ERROR_END
536 *CompRead
537 Mov PTicketRead_1               'チケットID読み取り回避点
538 'Dly 5                   'デバッグ用(22/09/30中村)
539 '    'ねじ締め開始(処理位置変更2/16中村)
540     MRtn2 = fScewTStart()
541     If MRtn2 = 0 Then GoTo *INITIAL_CHECK
542         fErrorProcess(11,329,201,0)
543         If M_20# = MNext% Then GoTo *CompRead
544         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
545         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
546         If M_20# = MContinue% Then GoTo *CompRead
547 '
548 '
549 *INITIAL_CHECK
550 '
551 'パレットから製品を取る
552 Mov PProductOnPltGet_2      '本体回避点
553 '
554 *RE_PLT_GET_1
555 '
556 If M_20# = MContinue% Then M_20# = MClear%
557 '
558 M_Out(12256) = 0            '本体チャック閉OFF
559 M_Out(12257) = 1            '本体チャック開ON
560 '
561 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)        '本体チャック開センサーON
562 If MRtn = 1 Then GoTo *CompPltGet_1
563 fErrorProcess(11,244,284,0)
564 If M_20# = MNext% Then M_20# = MClear%
565 If M_20# = MAbout% Or M_20# = MNgProcess% Then
566     Mov PInitialPosition
567     M_Out(12264) = 0            '位置決め出OFF
568     M_Out(12265) = 1            '位置決め戻ON
569     Break
570 EndIf
571 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
572 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
573 If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
574 *CompPltGet_1
575 '
576 'Mov PProductOnPltGet_1      '本体上空(処理位置変更1/21中村)
577 'Wait M_In(11278) = 1        '位置決め出端検出(コメントアウト11/4中村)
578 MRtn = frInCheck(11278,1,MSETTIMEOUT05&)        '位置決め出端検出
579 If MRtn = 1 Then GoTo *CompPltGet_2
580 fErrorProcess(11,231,282,0)
581 If M_20# = MNext% Then M_20# = MClear%
582 If M_20# = MAbout% Or M_20# = MNgProcess% Then
583     Mov PProductOnPltGet_2
584     Mov PInitialPosition
585     M_Out(12264) = 0            '位置決め出OFF
586     M_Out(12265) = 1            '位置決め戻ON
587     Break
588 EndIf
589 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
590 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
591 If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
592 *CompPltGet_2
593 '
594 Mov PProductOnPltGet_1      '本体上空(処理位置変更1/21中村)
595 '
596 M_Out(12264) = 0            '位置決め出OFF
597 M_Out(12265) = 1            '位置決め戻ON
598 Ovrd 25
599 Mvs PProductOnPltGet        '本体を取る位置
600 '
601 *RE_PLT_GET_2
602 '
603 If M_20# = MContinue% Then M_20# = MClear%
604 '位置決め戻のエラー処理
605 MRtn = frInCheck(11279,1,MSETTIMEOUT05&)    '位置決め戻端検出
606 If MRtn = 1 Then GoTo *CompPushIni
607 fErrorProcess(11,234,284,0)
608 If M_20# = MNext% Then M_20# = MClear%
609 If M_20# = MAbout% Or M_20# = MNgProcess% Then
610     Mvs PProductOnPltGet_1
611     Mov PProductOnPltGet_2
612     Mov PInitialPosition
613 EndIf
614 If M_20# = MAbout% Or M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
615 If M_20# = MContinue% Then
616     M_Out(12265) = 0            '位置決め戻OFF
617     M_Out(12264) = 1            '位置決め出ON
618     Dly 1.0
619     M_Out(12264) = 0                            '位置決め出OFF
620     M_Out(12265) = 1                            '位置決め戻ON
621 EndIf
622 If M_20# = MContinue% Then GoTo *RE_PLT_GET_2
623 *CompPushIni
624 '
625 M_Out(12257) = 0            '本体チャック開OFF
626 M_Out(12256) = 1            '本体チャック閉ON
627 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '本体チャック閉センサーON
628 If MRtn = 1 Then GoTo *CompPltGet_3
629 M_Out(12256) = 0            '本体チャック閉OFF
630 M_Out(12257) = 1            '本体チャック開ON
631 Dly 2.0
632 Mvs PProductOnPltGet_1
633 Mov PProductOnPltGet_2
634 M_Out(12257) = 0            '本体チャック開OFF
635 M_Out(12256) = 1            '本体チャック閉ON
636 fErrorProcess(11,244,284,0)
637 If M_20# = MNext% Then M_20# = MClear%
638 If M_20# = MAbout% Or M_20# = MNgProcess% Then
639     Mov PInitialPosition
640     Break
641 EndIf
642 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
643 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
644 M_Out(12256) = 0            '本体チャック閉OFF
645 M_Out(12257) = 1            '本体チャック開ON
646 Dly 2.0
647 Mov PProductOnPltGet_1
648 Mvs PProductOnPltGet
649 If M_20# = MContinue% Then GoTo *RE_PLT_GET_2
650 M_Out(12257) = 0            '本体チャック開OFF
651 M_Out(12256) = 1            '本体チャック閉ON
652 Dly 2.0
653 *CompPltGet_3
654 '
655 MRth = frInCheck(11264,1,MSETTIMEOUT05&)        '本体検出センサーON
656 If MRtn = 1 Then GoTo *CompPltGet_4
657 M_Out(12256) = 0            '本体チャック閉OFF
658 M_Out(12257) = 1            '本体チャック開ON
659 Dly 2.0
660 Mvs PProductOnPltGet_1
661 Mov PProductOnPltGet_2
662 fErrorProcess(11,252,284,0)
663 If M_20# = MNext% Then M_20# = MClear%
664 If M_20# = MAbout% Or M_20# = MNgProcess% Then
665     Mov PInitialPosition
666     Break
667 EndIf
668 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
669 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
670 Mov PProductOnPltGet_1
671 Mvs PProductOnPltGet
672 If M_20# = MContinue% Then GoTo *RE_PLT_GET_2
673 M_Out(12257) = 0            '本体チャック開OFF
674 M_Out(12256) = 1            '本体チャック閉ON
675 Dly 2.0
676 *CompPltGet_4
677     '
678     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
679 '
680 Mvs PProductOnPltGet_1      '本体上空
681     MRtn = FnCtlValue2(99)       '読書開始信号OFF  2022/04/28 渡辺
682 '
683 'Ovrd 100
684 Moverride = 2000 / M_OPovrd
685 If Moverride > 100 Then Moverride = 100
686 Ovrd Moverride
687 '
688 Mov PProductOnPltGet_2      '本体回避点
689 '
690 '製品をねじロボ1に置く
691 Mov PProductOnRoboSet_2     'ねじロボ1回避点
692 'Wait M_In(11888) = 1        'ねじロボ1停止1受信
693 MScrewRoboNgFlg% = 0
694 '
695 MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
696 '
697 If MRtn = 0 Then MScrewRoboNgFlg% = 1
698 If MScrewRoboNgFlg% = 1 Then GoTo *ProductOnPltSet
699 Mov PProductOnRoboSet_1     'ねじロボ1上空
700 Ovrd 25
701 Mvs PProductOnRoboSet       '本体置き位置
702 *RE_ROBO_SET
703 If M_20# = MContinue% Then M_20# = MClear%
704 '
705 Dly 0.3
706 M_Out(12256) = 0            '本体チャック閉OFF
707 M_Out(12257) = 1            '本体チャック開ON
708 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)             '本体チャック開センサーON
709 If MRtn = 1 Then GoTo *CompRoboSet
710 fErrorProcess(11,244,284,0)
711 If M_20# = MNext% Then M_20# = MClear%
712 If M_20# = MAbout% Or M_20# = MNgProcess% Then
713     MScrewRoboNgFlg% = 1
714     Mvs PProductOnRoboSet_1
715     Mov PProductOnRoboSet_2
716     Break
717 EndIf
718 If M_20# = MAbout% Then GoTo *ProductOnPltSet    '念のため製品を置きに行く動作を行う
719 If M_20# = MNgProcess% Then GoTo *ProductOnPltSet    '念のため製品を置きに行く動作を行う
720 If M_20# = MContinue% Then GoTo *RE_ROBO_SET
721 *CompRoboSet
722 '
723 Mvs PProductOnRoboSet_1     'ねじロボ1上空
724 Ovrd 100
725 Mvs PProductOnRoboSet_2     'ねじロボ1回避点
726 M_Out(12866) = 1 Dly 0.5    'ねじロボ1動作再開(停止1～停止2)(処理位置変更1/21中村)
727 '
728 '
729 '側板Lをパレットから取る
730 Mov PPlateLGet_2            '側板L取り回避点
731 'M_Out(12866) = 1 Dly 0.5    'ねじロボ1動作再開(停止1～停止2)(処理位置変更1/21中村)
732 *RE_PLATE_L_GET_1
733 If M_20# = MContinue% Then M_20# = MClear%
734 '
735 M_Out(12257) = 0            '本体チャック開OFF(以下3行,側板受け取り時邪魔になるため)
736 M_Out(12256) = 1            '本体チャック閉ON
737 '
738 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '本体チャック閉センサーON
739 If MRtn = 1 Then GoTo *CompPlateLGet_1
740 fErrorProcess(11,245,284,0)
741 If M_20# = MNext% Then M_20# = MClear%
742 If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
743 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
744 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
745 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_1
746 *CompPlateLGet_1
747 '
748 Mov PPlateLGet_1            '側板L取り上空
749 Ovrd 10
750 M_Out(12259) = 0            '側板Lシリンダー戻OFF
751 M_Out(12258) = 1            '側板Lシリンダー出ON
752 MRtn = frInCheck(11269,1,MSETTIMEOUT05&)        '側板Lシリンダー出端検出センサーON
753 If MRtn = 1 Then GoTo *CompPlateLGet_2
754 fErrorProcess(11,246,284,0)
755 If M_20# = MNext% Then M_20# = MClear%
756 If M_20# = MAbout% Or M_20# = MNgProcess% Then
757     Mov PPlateLGet_2
758     Mov PInitialPosition
759 EndIf
760 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
761 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
762 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_1
763 *CompPlateLGet_2
764 '
765 *RE_PLATE_L_GET_2
766 If M_20# = MContinue% Then M_20# = MClear%
767 '
768 M_Out(12262) = 0            '側板チャック閉OFF
769 M_Out(12263) = 1            '側板チャック開ON
770 Fine 0.05 , P               '公差0.05[mm]以内
771 Mvs PPlateLGet              '側板Lを取る位置
772 Fine 0 , P                  'Fine解除
773 M_Out(12263) = 0            '側板チャック開OFF
774 M_Out(12262) = 1            '側板チャック閉ON
775 'Wait M_In(11271) = 1        '側板チャック閉検出センサーON
776 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)        '側板Lチャック閉検出センサーON
777 Dly 0.5
778 Mvs PPlateLGet_1            '側板L取り上空
779 If MRtn = 1 Then GoTo *CompPlateLGet_3
780 fErrorProcess(11,250,292,0) '284→292に変更(6/7中村)
781 If M_20# = MNext% Then M_20# = MClear%
782 If M_20# = MAbout% Or M_20# = MNgProcess% Then
783     Mov PPlateLGet_2
784     Mov PInitialPosition
785 EndIf
786 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
787 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
788 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_2
789 *CompPlateLGet_3
790 '
791 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)         '側板L検出
792 If MRtn = 1 Then GoTo *CompPlateLGet_4
793 fErrorProcess(11,254,292,0) '284→292に変更(6/7中村)
794 If M_20# = MNext% Then M_20# = MClear%
795 If M_20# = MAbout% Or M_20# = MNgProcess% Then
796     Mov PPlateLGet_2
797     Mov PInitialPosition
798 EndIf
799 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
800 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
801 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_2
802 *CompPlateLGet_4
803 'Dly 5                      'test(暫定コメントアウト)
804 'M_Out(12256) = 0            '本体チャック閉OFF(側板受け取り時邪魔になるため)以下3行暫定削除(11/17中村)
805 'M_Out(12257) = 1            '本体チャック開ON(側板受け取り時邪魔になるため)
806 'MRtn = frInCheck(11265,1,MSETTIMEOUT05&)         '本体チャック開センサーON
807 'If MRtn = 0 Then
808 '    fErrorProcess()         'エラー処理
809 'EndIf
810 Ovrd 100
811 Mov PPlateLGet_2            '側板L取り回避点
812 '
813 '側板Lを置く
814 Mov PPlateLSet_2            '側板L置き回避点
815 '
816 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)         'もう一度側板L検出
817 If MRtn = 1 Then GoTo *CompPlateLGet_5
818 fErrorProcess(11,254,292,0) '284→292に変更(6/7中村)
819 If M_20# = MNext% Then M_20# = MClear%
820 If M_20# = MAbout% Or M_20# = MNgProcess% Then
821     Mov PPlateLGet_2
822     Mov PInitialPosition
823 EndIf
824 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
825 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
826 If M_20# = MContinue% Then
827     Mov PPlateLGet_2
828     Mov PPlateLGet_1
829 EndIf
830 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_2
831 *CompPlateLGet_5
832 'Wait M_In(11889) = 1        'ねじロボ1停止2受信
833 MRtn = fScrewTighenRoboCheck(11889)    '停止状態を受信する
834 If MRtn = 0 Then Mov PInitialPosition
835 If MRtn = 0 Then GoTo *ASSY_ERROR_END
836 '
837 'Wait M_In(11889) = 1        'ねじロボ1停止2受信
838 MRtn = fScrewTighenRoboCheck(11889)    '停止状態を受信する
839 If MRtn = 0 Then Mov PInitialPosition
840 If MRtn = 0 Then GoTo *ASSY_ERROR_END
841 '
842 Mov PPlateLSet_1            '側板L置き上空
843 Ovrd 10
844 Mvs PPlateLSet              '側板Lを置く位置
845 Dly 0.2
846 M_Out(12866) = 1 Dly 0.5    'ねじロボ1動作再開(停止2～停止3)
847 'Wait M_In(11890) = 1        'ねじロボ1停止3受信
848 MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する
849 'If MRtn = 0 Then
850 '    Mvs PPlateLSet_1
851 '    Mov PPlateLSet_2
852 '    Mov PInitialPosition
853 'EndIf
854 If MRtn = 0 Then GoTo *ASSY_ERROR_END
855 M_Out(12262) = 0            '側板チャック閉OFF
856 M_Out(12263) = 1            '側板チャック開ON
857 Dly 0.5
858 Mvs PPlateLSet_1            '側板L置き上空
859 Ovrd 100
860 M_Out(12258) = 0            '側板Lシリンダー出OFF
861 M_Out(12259) = 1            '側板Lシリンダー戻ON
862 Mov PPlateLSet_2            '側板L置き回避点
863 '
864 '    ' 部品供給要求送信'12/20位置変更(中村)
865     M_Out(12787) = 1
866 '
867 '側板L置き位置画像検査
868 Mov PPlateLCheck_2          '側板L検査通過点
869 Mvs PPlateLCheck            '側板L検査位置
870 *RE_L_CHECK
871 PInspPosition(1) = PPlateLCheck
872 MInspGroup%(1) = 2
873 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,1,-1,1)
874 If MRtn = 1 Or M_In(11374) = 0 Then GoTo *CompLCheck
875 fErrorProcess(11,43,3,0)
876 If M_20# = MNext% Then M_20# = MClear%
877 If M_20# = MAbout% Or M_20# = MNgProcess% Then
878     Mov PInitialPosition
879     Break
880 EndIf
881 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
882 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
883 If M_20# = MContinue% Then GoTo *RE_L_CHECK
884 *CompLCheck
885 M_Out(12866) = 1 Dly 0.5    'ねじロボ動作再開(停止3～停止4)
886 '
887 '側板Rを供給機から取る
888 '
889 '    ' 部品供給要求送信'12/20位置変更(中村)
890 '    M_Out(12787) = 1
891 '    '    ' 部品供給完了待ち
892 '    Wait M_In(11810) = 1
893 '
894 Mov PPlateRGet_4            '経路1
895 Mov PPlateRGet_3            '経路2
896 Mov PPlateRGet_2            '側板R取り回避点
897     '    ' 部品供給完了待ち(処理変更2/27中村)
898 *RE_FEEDER_READY
899     fnAutoScreenComment(513)    '状態表示[部品供給待ち] 2022/04/27 渡辺
900 '    Wait M_In(11810) = 1
901 MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '供給待ち
902 If MRtn = 1 Then GoTo *CompFeederReady
903 '   ' 部品供給要求終了
904 M_Out(12787) = 0
905 fErrorProcess(11,289,290,0) '284→290に変更6/7中村
906 If M_20# = MNext% Then M_20# = MClear%
907 If M_20# = MAbout% Or M_20# = MNgProcess% Then
908     Mov PBracketRGet_2
909     Mov PBracketRGet_3
910     Mov PBracketRSet_3
911     Mov PInitialPosition1
912 EndIf
913 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
914 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
915     ' 部品供給要求
916 M_Out(12787) = 1
917 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
918 *CompFeederReady
919 '    ' 部品供給要求終了
920     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/27 渡辺
921     M_Out(12787) = 0
922 '
923 *RE_PLATE_R_GET_1
924 '
925 If M_20# = MContinue% Then M_20# = MClear%
926 '
927 M_Out(12257) = 0            '本体チャック開OFF(以下3行,側板受け取り時邪魔になるため)
928 M_Out(12256) = 1            '本体チャック閉ON
929 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '本体チャック閉センサーON
930 Dly 0.1
931 If MRtn = 1 Then GoTo *CompPlateRGet_1
932 fErrorProcess(11,245,284,0)
933 If M_20# = MNext% Then M_20# = MClear%
934 If M_20# = MAbout% Or M_20# = MNgProcess% Then
935     Mov PPlateRGet_2
936     Mov PPlateRGet_3
937     Mov PPlateRGet_4
938     Mov PInitialPosition
939     Break
940 EndIf
941 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
942 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
943 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_1
944 *CompPlateRGet_1
945 '
946 *RE_PLATE_R_GET_2
947 If M_20# = MContinue% Then M_20# = MClear%
948 '
949 Mov PPlateRGet_1            '側板R取り上空
950 Ovrd 10
951 M_Out(12261) = 0            '側板Rシリンダー戻OFF
952 M_Out(12260) = 1            '側板Rシリンダー出ON
953 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)        '側板Rシリンダー出端検出センサーON
954 If MRtn = 1 Then GoTo *CompPlateRGet_2
955 fErrorProcess(11,248,284,0)
956 If M_20# = MNext% Then M_20# = MClear%
957 If M_20# = MAbout% Or M_20# = MNgProcess% Then
958     Mov PPlateRGet_2
959     Mov PPlateRGet_3
960     Mov PPlateRGet_4
961     Mov PInitialPosition
962     Break
963 EndIf
964 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
965 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
966 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_2
967 *CompPlateRGet_2
968 '
969 M_Out(12262) = 0            '側板チャック閉OFF
970 M_Out(12263) = 1            '側板チャック開ON
971 Fine 0.05 , P               '公差0.05[mm]以内
972 Mvs PPlateRGet              '側板Rを取る位置
973 Ovrd 5
974 Fine 0 , P                  'Fine解除
975 M_Out(12263) = 0            '側板チャック開OFF
976 M_Out(12262) = 1            '側板チャック閉ON
977 Dly 0.5
978 Mvs PPlateRGet_1            '側板R取り上空
979 Ovrd 10
980 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '側板R検出
981 If MRtn = 1 Then GoTo *CompPlateRGet_3
982 fErrorProcess(11,254,292,0) '284→292に変更(6/7中村)
983 If M_20# = MNext% Then M_20# = MClear%
984 If M_20# = MAbout% Or M_20# = MNgProcess% Then
985     Mov PPlateRGet_2
986     Mov PPlateRGet_3
987     Mov PPlateRGet_4
988     Mov PInitialPosition
989     Break
990 EndIf
991 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
992 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
993 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_2
994 *CompPlateRGet_3
995 '
996 M_Out(12256) = 0            '本体チャック閉OFF(側板受け取り時邪魔になるため)
997 M_Out(12257) = 1            '本体チャック開ON(側板受け取り時邪魔になるため)
998 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)        '本体チャック開センサーON
999 If MRtn = 1 Then GoTo *CompPlateRGet_4
1000 fErrorProcess(11,244,284,0)
1001 If M_20# = MNext% Then M_20# = MClear%
1002 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1003     Mov PPlateRGet_2
1004     Mov PPlateRGet_3
1005     Mov PPlateRGet_4
1006     Mov PInitialPosition
1007     Break
1008 EndIf
1009 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1010 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1011 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_1
1012 *CompPlateRGet_4
1013 '
1014 Mov PPlateRGet_2            '側板R取り回避点
1015 Ovrd 50
1016 Mov PPlateRGet_3            '経路2
1017 ''    ' 部品供給要求終了
1018 '    M_Out(12787) = 0
1019 ''    ' 部品取得完了送信(パルス)
1020 '    M_Out(12800) = 1 Dly 0.5
1021     '
1022 Mov PPlateRGet_4            '経路1
1023 '
1024 '側板Rを置く
1025 Mov PPlateRSet_3            '経路
1026 Mov PPlateRSet_2            '側板R置き回避点
1027 Ovrd 100
1028 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        'もう一度側板R検出
1029 If MRtn = 1 Then GoTo *CompRGet
1030 fErrorProcess(11,254,292,0) '284→292に変更(6/7中村)
1031 If M_20# = MNext% Then M_20# = MClear%
1032 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1033     Mov PPlateRSet_3
1034     Mov PPlateRGet_3
1035 '    Mov PPlateRGet_4
1036     Mov PInitialPosition
1037     Break
1038 EndIf
1039 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1040 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1041 If M_20# = MContinue% Then
1042     Mov PPlateRSet_3
1043     Mov PPlateRGet_4
1044     Mov PPlateRGet_3
1045     Mov PPlateRGet_2
1046     Mov PPlateRGet_1
1047 EndIf
1048 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_1
1049 *CompRGet
1050 '
1051 '    ' 部品供給要求終了
1052     M_Out(12787) = 0
1053 '    ' 部品取得完了送信(パルス)
1054     M_Out(12800) = 1 Dly 0.5
1055 'Wait M_In(11891) = 1        'ねじロボ1停止4受信
1056 MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
1057 If MRtn = 0 Then Mov PInitialPosition
1058 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1059 Mov PPlateRSet_1            '側板R置き上空
1060 Ovrd 10
1061 Mvs PPlateRSet              '側板Rを置く位置
1062 Dly 0.2
1063 M_Out(12866) = 1 Dly 0.5    'ねじロボ動作再開(停止4～停止5)
1064 'Wait M_In(11892) = 1        'ねじロボ1停止5受信
1065 MRtn = fScrewTighenRoboCheck(11892)    '停止状態を受信する
1066 'If MRtn = 0 Then
1067 '    Mvs PPlateRSet_1
1068 '    Mov PPlateRSet_2
1069 '    Mov PInitialPosition
1070 'EndIf
1071 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1072 M_Out(12262) = 0            '側板チャック閉OFF
1073 M_Out(12263) = 1            '側板チャック開ON
1074 Dly 0.5
1075 Mvs PPlateRSet_1            '側板L置き上空
1076 Ovrd 100
1077 M_Out(12260) = 0            '側板Rシリンダー出OFF
1078 M_Out(12261) = 1            '側板Rシリンダー戻ON
1079 Mov PPlateRSet_2            '側板R置き回避点
1080 '
1081 '側板R置き位置画像検査
1082 Mov PPlateRCheck_2          '側板R検査通過点
1083 Mvs PPlateRCheck            '側板R検査位置
1084 *RE_R_CHECK
1085 If M_20# = MContinue% Then M_20# = MClear%
1086 PInspPosition(1) = PPlateRCheck
1087 MInspGroup%(1) = 3
1088 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,1,-1,1)
1089 If MRtn = 1 Or M_In(11374) = 0 Then GoTo *CompCheckR
1090 fErrorProcess(11,43,3,0)
1091 If M_20# = MNext% Then M_20# = MClear%
1092 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1093     Mvs PPlateRCheck_2
1094     Mov PInitialPosition
1095 EndIf
1096 If M_20# = MAbout% Or M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1097 If M_20# = MContinue% Then GoTo *RE_R_CHECK
1098 *CompCheckR
1099 M_Out(12866) = 1 Dly 0.5    'ねじロボ動作再開(停止5～停止6)
1100 '
1101 'ねじロボ1の製品を取る
1102 Mov PProductOnRoboGet_2     'ねじロボ1回避点
1103 'Wait M_In(11893) = 1        'ねじロボ1停止6受信
1104 'MRtn = frInCheck(11876,1,MSETTIMEOUT05&)   'ねじロボ1完了を受信
1105 '
1106 *RE_CYLINDER_R_INI
1107 MRtn = frInCheck(11273,1,MSETTIMEOUT05&)        'R側シリンダー戻検出
1108 If MRtn = 1 Then GoTo *CompCylinderRIni
1109 fErrorProcess(11,249,284,0)
1110 If M_20# = MNext% Then M_20# = MClear%
1111 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1112     Mov PPlateRSet_3
1113     Mov PPlateRGet_3
1114 '    Mov PPlateRGet_4
1115     Mov PInitialPosition
1116     Break
1117 EndIf
1118 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1119 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1120 If M_20# = MContinue% Then
1121     M_Out(12260) = 0            '側板Rシリンダー出OFF
1122     M_Out(12261) = 1            '側板Rシリンダー戻ON
1123 EndIf
1124 If M_20# = MContinue% Then GoTo *RE_CYLINDER_R_INI
1125 *CompCylinderRIni
1126 '
1127 MRtn = fScrewTighenRoboCheck(11893)    '停止状態を受信する
1128 If MRtn = 0 Then Mov PInitialPosition
1129 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1130 'Mvs PProductOnRoboGet_1     'ねじロボ1上空(2022/1/11移動タイミング変更(中村))
1131 'Ovrd 25
1132 '
1133 *RE_ROBO_GET
1134 '
1135 If M_20# = MContinue% Then M_20# = MClear%
1136 '
1137 M_Out(12256) = 0            '本体チャック閉OFF
1138 M_Out(12257) = 1            '本体チャック開ON
1139 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)        '本体チャック開センサーON
1140 If MRtn = 1 Then GoTo *CompRoboGet_1
1141 fErrorProcess(11,244,284,0)
1142 If M_20# = MNext% Then M_20# = MClear%
1143 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1144     Mvs PProductOnRoboGet_2
1145     Mov PInitialPosition
1146     Break
1147 EndIf
1148 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1149 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1150 If M_20# = MContinue% Then GoTo *RE_ROBO_GET
1151 *CompRoboGet_1
1152 '
1153 Mvs PProductOnRoboGet_1     'ねじロボ1上空(2022/1/11移動タイミング変更(中村))
1154 Ovrd 25
1155 '
1156 Mvs PProductOnRoboGet       '本体を取る位置
1157 M_Out(12866) = 1 Dly 0.5    'ねじロボ動作再開(停止6～完了)
1158 'Wait M_In(11876) = 1        'ねじロボ1完了を受信
1159 MRtn = fScrewTighenRoboCheck(11876)    '停止状態を受信する
1160 If MRtn = 0 Then
1161     Mvs PProductOnRoboGet_1
1162     Mvs PProductOnRoboGet_2
1163     Mov PInitialPosition
1164 EndIf
1165 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1166 '
1167 M_Out(12257) = 0            '本体チャック開OFF
1168 M_Out(12256) = 1            '本体チャック閉ON
1169 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '本体チャック閉センサーON
1170 If MRtn = 1 Then GoTo *CompRoboGet_2
1171 M_Out(12256) = 0            '本体チャック閉OFF
1172 M_Out(12257) = 1            '本体チャック開ON
1173 Dly 2.0
1174 Mvs PProductOnRoboGet_1
1175 Mvs PProductOnRoboGet_2
1176 Mov PInitialPosition
1177 M_Out(12257) = 0            '本体チャック開OFF
1178 M_Out(12256) = 1            '本体チャック閉ON
1179 fErrorProcess(11,245,284,0)
1180 If M_20# = MNext% Then M_20# = MClear%
1181 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1182 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1183 M_Out(12256) = 0            '本体チャック閉OFF
1184 M_Out(12257) = 1            '本体チャック開ON
1185 Dly 2.0
1186 Mov PProductOnRoboGet_2
1187 If M_20# = MContinue% Then GoTo *RE_ROBO_GET
1188 Mvs PProductOnRoboGet_1
1189 Mvs PProductOnRoboGet
1190 M_Out(12257) = 0            '本体チャック開OFF
1191 M_Out(12256) = 1            '本体チャック閉ON
1192 Dly 2.0
1193 *CompRoboGet_2
1194 '
1195 MRtn = frInCheck(11264,1,MSETTIMEOUT05&)        '本体検出センサーON
1196 'Mvs PProductOnRoboGet_1     'ねじロボ1上空
1197 If MRtn = 1 Then GoTo *CompRoboGet_3
1198 M_Out(12256) = 0            '本体チャック閉OFF
1199 M_Out(12257) = 1            '本体チャック開ON
1200 Dly 2.0
1201 Mvs PProductOnRoboGet_1
1202 Mvs PProductOnRoboGet_2
1203 Mov PInitialPosition
1204 fErrorProcess(11,252,284,0)
1205 If M_20# = MNext% Then M_20# = MClear%
1206 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1207 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1208 Mov PProductOnRoboGet_2
1209 If M_20# = MContinue% Then GoTo *RE_ROBO_GET
1210 Mvs PProductOnRoboGet_1
1211 Mvs PProductOnRoboGet
1212 M_Out(12257) = 0            '本体チャック開OFF
1213 M_Out(12256) = 1            '本体チャック閉ON
1214 Dly 2.0
1215 *CompRoboGet_3
1216 '
1217 Moverride = 2000 / M_OPovrd
1218 If Moverride >100 Then Moverride = 100
1219 Ovrd Moverride
1220 Mov PProductOnRoboGet_2     'ねじロボ1回避点
1221 '
1222 *ProductOnPltSet
1223 'パレットに製品を置く
1224 Mov PProductOnPltSet_2     'パレット回避点
1225 Mov PProductOnPltSet_1     'パレット上空
1226 Ovrd 10
1227 Mvs PProductOnPltSet       '本体置き位置
1228 Dly 0.2
1229 '
1230 *RE_PLT_SET
1231 '
1232 If MScrewRoboNgFlg% = 1 And M_20# = MContinue% Then M_20# = MRtn
1233 If M_20# = MContinue% Then M_20# = MClear%
1234 '
1235 M_Out(12256) = 0            '本体チャック閉OFF
1236 M_Out(12257) = 1            '本体チャック開ON
1237 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    '本体チャック開センサーON
1238 If MRtn = 1 Then GoTo *CompPltSet_1
1239 If MScrewRoboNgFlg% = 1 Then
1240     MRtn = M_20#
1241     M_20# = MClear%
1242 EndIf
1243 fErrorProcess(11,244,284,0)
1244 If M_20# = MNext% Then M_20# = MClear%
1245 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1246 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1247 If M_20# = MContinue% Then GoTo *RE_PLT_SET
1248 *CompPltSet_1
1249 If MScrewRoboNgFlg% = 1 Then M_20# = MRtn
1250 '
1251 Mvs PProductOnPltSet_1     'パレット上空
1252 Ovrd 100
1253 Mvs PProductOnPltSet_2     'パレット回避点
1254 '
1255 'Mov PInitialPosition        '暫定削除(11/17中村)
1256 '
1257     MRtn = FnCtlValue2(2)          '組立ＯＫ＋１  2022/04/28 渡辺
1258 '
1259 Mov PTicketRead_1
1260     MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1261 If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
1262 M_Out(12868) = 1 Dly 0.5    'ねじロボ1ねじ締め完了を送信
1263 '
1264 M_20# = MAssyOK%            'Assy正常終了
1265 '
1266 GoTo *AssyEnd
1267 '
1268 *ASSY_ERROR_END
1269     M_Out(12264) = 0            '位置決め出OFF
1270     M_Out(12265) = 1            '位置決め戻ON
1271 *AssyEnd
1272 *fnAssyStart_FEndPosi
1273     Exit Function
1274 FEnd
1275 '
1276 '■fnPiasCheck
1277 ''' <summary>
1278 ''' PIASチケット読込み
1279 ''' </summary>
1280 ''' <returns>   0 : NG
1281 '''             1 : OK(読込み完了)
1282 ''' </returns>
1283 ''' <remarks>
1284 ''' Date   : 2021/07/07 : M.Hayakawa
1285 ''' </remarks>'
1286 Function M% fnPiasCheck
1287     fnPiasCheck = 0
1288     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1289     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1290 '
1291 *RETRY_PIAS
1292     M_20# = MClear%
1293     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1294     '
1295     '【IDチケット読み込み】
1296     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1297     MInspGroup%(1) = 1              '検査G番号
1298     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1299 '
1300     'エラーの場合
1301     If MRtn <> 1 Then
1302         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1303         If MRtn <> 1 Then
1304             'D720 -> D1300 コピー要求
1305             M_Out(12565) = 1
1306             Dly 0.5
1307             M_Out(12565) = 0
1308             'エラー処理記述
1309             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1310             'GOT KEY入力待ち
1311             MKeyNumber = fnKEY_WAIT()
1312             '
1313             Select MKeyNumber
1314                 Case MNext%         '次へを選択した場合
1315                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1316                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1317                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1318                     Break
1319                 Case MAbout%        '停止を選択した場合
1320                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1321                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1322                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1323                     Break
1324                 Case MNgProcess%    'NGを選択した場合
1325                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1326                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1327                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1328                     Break
1329                 Case MContinue%     '継続を選択した場合
1330                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1331                     M_20# = MContinue%
1332                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1333                     Break
1334             End Select
1335         EndIf
1336     EndIf
1337 '----------D720 -> D1300 コピー要求----------
1338     M_Out(12565) = 1
1339     Dly 0.5
1340     M_Out(12565) = 0
1341 '----------通信確認をする----------
1342     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1343     MRtn = 0                ' 初期化
1344     M_20# = MClear%         ' 初期化
1345     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1346     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1347     If MRtn <> 1 Then
1348         If M_20# = MContinue% Then
1349             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1350         Else
1351             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1352         EndIf
1353     EndIf
1354 '----------工程抜け確認----------
1355     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1356     MRtn = 0                ' 初期化
1357     M_20# = MClear%         ' 初期化
1358     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1359     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1360     If MRtn <> 1 Then
1361         If M_20# = MContinue% Then
1362             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1363         Else
1364             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1365         EndIf
1366     EndIf
1367     '
1368     fnPiasCheck = 1
1369     *fnPiasCheck_End
1370     Exit Function
1371 FEnd
1372 '
1373 '■fnPCComuCheck
1374 ''' <summary>
1375 ''' PC-PLC通信チェック
1376 ''' </summary>
1377 ''' <returns>   0 : NG
1378 '''             1 : OK(読込み完了)
1379 ''' </returns>
1380 ''' <remarks>
1381 ''' Date   : 2021/07/07 : M.Hayakawa
1382 ''' </remarks>'
1383 Function M% fnPCComuCheck
1384     fnPCComuCheck = 0
1385     MJudge% = 0                                  '初期化
1386     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1387     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1388     '
1389     For MStaNo = 0 To 5
1390         '
1391         If M_In(MIN_PIAS_ComOK%) = 1 Then
1392             'PC通信OK(M400)
1393             MJudge% = MOK%
1394             MStaNo = 5
1395             Break
1396         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1397             'toRBT_通信確認time out
1398             MJudge% = MNG%
1399             MCommentD1001 = 15
1400             MCommentD1002 = 21
1401             MStaNo = 5
1402             Break
1403         Else
1404             'toRBT_通信確認time out
1405             MJudge% = MNG%
1406             MCommentD1001 = 14
1407             MCommentD1002 = 21
1408             Break
1409         EndIf
1410     Next MStaNo
1411     '
1412     '上記で返信フラグを受信してからPC通信確認OFF
1413     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1414     '
1415     'エラー画面
1416     If MJudge% <> MOK% Then
1417         M_20# = MClear%     '初期化
1418         'エラー処理記述
1419         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1420         'GOT KEY入力待ち
1421         MKeyNumber = fnKEY_WAIT()
1422         '
1423         If MKeyNumber = MAbout% Then            '停止を選択した場合
1424             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1425             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1426             Break
1427         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1428             M_20# = MPass%                      'M_20# プログラム間共通外部変数
1429             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1430             Break
1431         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1432             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1433             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1434             Break
1435         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1436             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1437             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1438             Break
1439         EndIf
1440     Else
1441         'OKの場合
1442         fnPCComuCheck = 1
1443     EndIf
1444     Exit Function
1445 FEnd
1446 '
1447 '■fnProcessCheck
1448 ''' <summary>
1449 ''' 工程抜け確認
1450 ''' </summary>
1451 ''' <returns>    1：工程履歴OK     0：異常終了
1452 '''             -1：前工程履歴NG  -2：自工程履歴あり
1453 '''             -3：モデル仕向NG  -4：タイムアウト
1454 '''             -5：履歴処理エラー
1455 ''' </returns>
1456 ''' <remarks>
1457 ''' Date   : 2021/07/07 : M.Hayakawa
1458 ''' </remarks>'
1459 Function M% fnProcessCheck
1460     fnProcessCheck = 0
1461     MJudge% = MNG%      '一旦NGを初期化とする
1462 '----------工程抜け確認----------
1463     MCommentD1001 = 0   'コメント初期化
1464     For MStaNo = 0 To 5
1465         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1466         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1467         '
1468         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1469             MJudge% = MOK%
1470             fnAutoScreenComment(85)     ' AUTO画面
1471             MStaNo = 5
1472             Break
1473         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1474             MFlgLoop% = 0
1475             MJudge% = MNG%
1476             MCommentD1001 = 27
1477             MCommentD1002 = 22
1478             fnAutoScreenComment(94)     ' AUTO画面
1479             fnProcessCheck = -2         ' NGは-2を返す
1480             MStaNo = 5
1481             Break
1482         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1483            MJudge% = MNG%
1484             MCommentD1001 = 31
1485             MCommentD1002 = 22
1486             fnAutoScreenComment(83)     ' AUTO画面
1487             fnProcessCheck = -3         ' NGは-3を返す
1488             MStaNo = 5
1489             Break
1490         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1491             '履歴NGは直ぐに終了せず繰り返し確認を行う
1492             '前工程の書込みが終了していない可能性があるため
1493             MJudge% = MNG%
1494             MCommentD1001 = 32
1495             MCommentD1002 = 22
1496             fnAutoScreenComment(84)     ' AUTO画面
1497             fnProcessCheck = -1         ' NGは-1を返す
1498             Dly 1.0
1499             '工程抜け確認OFF
1500             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1501             Dly 1.0
1502            'MStaNo = 5
1503             Break
1504         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1505             MFlgLoop% = 0
1506             MJudge% = MNG%
1507             MCommentD1001 = 29
1508             MCommentD1002 = 22
1509             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1510             fnProcessCheck = -5         ' NGは-5を返す
1511             MStaNo = 5
1512             Break
1513         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1514             MJudge% = MNG%
1515             If MCommentD1001 = 32 Then
1516                 '何もしない
1517             Else
1518                 MCommentD1001 = 26
1519             EndIf
1520             MCommentD1002 = 22
1521             fnProcessCheck = -4         ' NGは-4を返す
1522             MStaNo = 5
1523             Break
1524         Else
1525             MJudge% = MNG%
1526             MCommentD1001 = 28
1527             MCommentD1002 = 22
1528         EndIf
1529     Next MStaNo
1530     '工程抜け確認OFF
1531     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1532     '通過履歴NG 工程抜けの場合
1533     If MJudge% = MPass% Then
1534         M_20# = MPass%
1535     EndIf
1536     '
1537     'エラー画面
1538     If MJudge% <> MOK% Then
1539         M_20# = MClear%     '初期化
1540         'エラー処理記述
1541         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1542         'GOT KEY入力待ち
1543         MKeyNumber = fnKEY_WAIT()
1544         '
1545         Select MKeyNumber
1546             Case MAbout%        '停止を選択した場合
1547                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1548                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1549                 Break
1550             Case MNext%         '次へを選択した場合
1551                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1552                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1553                 Break
1554             Case MContinue%     '継続を選択した場合
1555                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1556                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1557                 Break
1558             Case MNgProcess%    'NGを選択した場合
1559                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1560                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1561                 Break
1562         End Select
1563     Else
1564         fnProcessCheck = 1  ' OKは1を返す
1565     EndIf
1566     Exit Function
1567 FEnd
1568 '
1569 '■fnPiasWrite
1570 ''' <summary>
1571 ''' Pias 組立結果書込み要求
1572 ''' </summary>
1573 '''<param name="MFlg%">
1574 '''                 MOK%(1) = 工程履歴にOKを書込む
1575 '''                 MNG%(0) = 工程履歴にNGを書込む
1576 '''</param>
1577 '''<returns></returns>
1578 ''' <remarks>
1579 ''' Date   : 2021/07/07 : M.Hayakawa
1580 ''' </remarks>'
1581 Function M% fnPiasWrite(ByVal MFlg%)
1582       fnPiasWrite = 0
1583 *RETRY_PIASWRITE
1584     '
1585     '組立OK(MOK%)の場合　M306 ON
1586    '組立NG(MNG%)の場合　M307 ON
1587     If MFlg% = MOK% Then
1588         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1589     Else
1590         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1591     EndIf
1592     Dly 0.1                  '念のため
1593     '
1594     'Piasへ書込み開始 M305 -> ON
1595     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1596     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1597     '
1598     MJudge% = MNG%
1599     '
1600     For MStaNo = 0 To 5
1601         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1602             MJudge% = MOK%
1603             'MRet = fnAutoScreenComment(85)  'AUTO画面
1604             MStaNo = 5
1605             Break
1606         '
1607         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1608             MJudge% = MNG%
1609             'MRet = fnAutoScreenComment(85)  'AUTO画面
1610            MCommentD1001 = 34
1611            MCommentD1002 = 25
1612             MStaNo = 5
1613             Break
1614         '
1615         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1616             MJudge% = MNG%
1617             'MRet = fnAutoScreenComment(85)  'AUTO画面
1618            MCommentD1001 = 35
1619            MCommentD1002 = 25
1620             MStaNo = 5
1621             Break
1622         '
1623         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1624             MJudge% = MNG%
1625             'MRet = fnAutoScreenComment(85)  'AUTO画面
1626            MCommentD1001 = 36
1627            MCommentD1002 = 25
1628             MStaNo = 5
1629             Break
1630         '
1631         Else
1632             MJudge% = MNG%
1633            MCommentD1001 = 42
1634            MCommentD1002 = 25
1635         '
1636         EndIf
1637         '
1638     Next MStaNo
1639     '
1640     'Piasへ書込み開始 M305 -> OfF
1641     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1642     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1643     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1644     '
1645     '
1646     '通過履歴NG 工程抜けの場合
1647     If MJudge% = MPass% Then
1648         M_20# = MPass%
1649     EndIf
1650     '
1651    M_20# = MClear%     '初期化
1652     '
1653     'エラー画面
1654     If MJudge% < MOK% Then
1655     '
1656 '残しておくが現状では使用しないラベル
1657 *RETRY_ERR_WRITE
1658         M_20# = MClear%     '初期化
1659         'エラー処理記述
1660         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1661         'GOT KEY入力待ち
1662         MKeyNumber = fnKEY_WAIT()
1663         '
1664         If MKeyNumber = MAbout% Then   '停止を選択した場合
1665             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1666            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1667             Break
1668         '
1669         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1670             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1671             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1672         '
1673         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1674             M_20# = MPass%            'M_20# プログラム間共通外部変数
1675             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1676         '
1677         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1678             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1679            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1680             Break
1681         '
1682         EndIf
1683         '
1684         If M_20# = MClear% Then *RETRY_ERR_WRITE
1685         '
1686     EndIf
1687     '
1688     If M_20# = MContinue% Then *RETRY_PIASWRITE
1689     '
1690     fnPiasWrite = 1
1691     Exit Function
1692 FEnd
1693 '
1694 '■fnPCBNumberCheck
1695 ''' <summary>
1696 ''' Pias 基板番号照合要求
1697 ''' </summary>
1698 '''<param name="%"></param>
1699 '''<param name="%"></param>
1700 '''<returns></returns>
1701 ''' <remarks>
1702 ''' Date   : 2021/07/07 : M.Hayakawa
1703 ''' </remarks>'
1704 Function M% fnPCBNumberCheck
1705       fnPCBNumberCheck = 0
1706     '
1707 *RETRY_PCBCHECK
1708     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1709     'Piasへ基板照合開始 M310 -> ON
1710     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1711     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1712     '
1713     MJudge% = MNG%
1714     '
1715     For MStaNo = 0 To 5
1716         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1717             MJudge% = MOK%
1718             fnAutoScreenComment(96)  'AUTO画面
1719             MStaNo = 5
1720             Break
1721         '
1722         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1723             MJudge% = MNG%
1724             fnAutoScreenComment(97)  'AUTO画面
1725             MCommentD1001 = 37
1726             MCommentD1002 = 25
1727             MStaNo = 5
1728             Break
1729         '
1730         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1731             MJudge% = MNG%
1732             fnAutoScreenComment(98)  'AUTO画面
1733             MCommentD1001 = 38
1734             MCommentD1002 = 25
1735             MStaNo = 5
1736             Break
1737         '
1738         ElseIf M_In(11580) = 1 Then                         'time out
1739             MJudge% = MNG%
1740             fnAutoScreenComment(99)  'AUTO画面
1741             MCommentD1001 = 39
1742             MCommentD1002 = 25
1743             MStaNo = 5
1744             Break
1745         '
1746         Else
1747             MJudge% = MNG%
1748            MCommentD1001 = 41
1749            MCommentD1002 = 25
1750         '
1751         EndIf
1752         '
1753     Next MStaNo
1754     '
1755     'Piasへ基板照合開始 M310 -> OfF
1756     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1757     '
1758     '
1759     '通過履歴NG 工程抜けの場合
1760     If MJudge% = MPass% Then
1761         M_20# = MPass%
1762     EndIf
1763     '
1764    M_20# = MClear%     '初期化
1765     '
1766     'エラー画面
1767     If MJudge% < MOK% Then
1768     '
1769 '残しておくが現状では使用しないラベル
1770 *RETRY_ERR_PCBNUMBER
1771         M_20# = MClear%     '初期化
1772         'エラー処理記述
1773         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1774         'GOT KEY入力待ち
1775         MKeyNumber = fnKEY_WAIT()
1776         '
1777         If MKeyNumber = MAbout% Then   '停止を選択した場合
1778             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1779             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1780             Break
1781         '
1782         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1783             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1784             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1785         '
1786         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1787             M_20# = MNext%            'M_20# プログラム間共通外部変数'MPass%→MNext%へ変更(12/7中村)
1788             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1789         '
1790         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1791             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1792             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1793             Break
1794         '
1795         EndIf
1796         '
1797         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1798         '
1799     EndIf
1800     '
1801     If M_20# = MContinue% Then *RETRY_PCBCHECK
1802     Exit Function
1803 FEnd
1804 '
1805 '■ScrewTight_S2
1806 ''' <summary>
1807 ''' ねじ締めを行う
1808 ''' </summary>
1809 '''<param name="PScrewPos()">
1810 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1811 '''             PScrewPos(2)    ：ねじ締め回避点
1812 '''             PScrewPos(10)   ：ねじ締め終了高さ
1813 '''</param>
1814 '''<returns>整数
1815 '''         0=異常終了、1=正常終了
1816 '''</returns>
1817 ''' <remarks>
1818 ''' Date   : 2021/07/07 : M.Hayakawa
1819 ''' </remarks>'
1820 Function M% ScrewTight_S2(ByVal PScrewPosition())   'ネジ締め個別設定
1821     ScrewTight_S2 = 0
1822     MOKNGFlg = 0
1823     Ovrd 100
1824     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1825     ' 暫定
1826     Ovrd 5
1827     Mvs PScrewPosition(10),-10    ' パレット上ねじ締めS①の上空へ移動
1828 '    Ovrd MOvrdA
1829     '暫定マスク
1830 '    M_Out(Y62_Driver)=1     ' バンクセッティング　C1
1831 '    Dly 0.1
1832 '    M_Out(Y61_Driver)=1     'ドライバーON　CW
1833 '    'Spd 8.3 '外部ライド100  内部ライド60   'ライド100-40　100%：Spd　15　'ねじ締め速度設定
1834 '    Spd MSpdA               'ネジ締め時Spd個別設定
1835     ' 暫定移動のみ
1836     Mvs PScrewPosition(10)
1837 '    '
1838 '    Dly 0.1
1839 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1840 '    Wait M_In(11584)=1          '完了/エラー検出
1841 '    Dly 0.1
1842 '    Spd M_NSpd
1843 '    '
1844 '    If M_In(X28_Driver)=1 Then  'ねじトータルエラー検出時
1845 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1846 '        Dly 0.1
1847 '        M_Out(Y62_Driver)=0     'バンクセッティング解除　C1
1848 '        Dly 0.1
1849 '        M_Out(Y63_Driver)=0     'バンクセッティング解除　C1
1850 '        Dly 0.1
1851 '        M_Out(Y65_Driver)=0     'プログラム解除　F1
1852 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1853 '        M_Out(Y68_VV1)=0        'ねじ吸着　OFF
1854 '        MOKNGFlg = -1
1855 '        ScrewTight_S2 = 0
1856 '    Else
1857 '        Wait M_In(X29_Driver)=1 ' 正常完了時
1858 '        Dly 0.1
1859 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1860 '        Dly 0.1
1861 '        M_Out(Y62_Driver)=0     'バンクセッティング解除
1862 '        Dly 0.1
1863         M_Out(12249)=1 Dly 0.3         'ねじ吸着　OFF (一時コメントアウト解除,(Y68_VV1)=0を(12249)=1 Dly 0.3に変更(8/5中村))
1864         M_Out(12250)=1 Dly 0.1         '暫定真空破壊
1865 '        Dly 0.1
1866 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1867 '        ScrewTight_S2 = 1
1868 '    EndIf
1869 ' 暫定
1870     Ovrd 10
1871     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1872     Ovrd 100
1873     Exit Function
1874 FEnd
1875 '
1876 '■ScrewGet_S3
1877 ''' <summary>
1878 ''' ねじ供給機からねじを得る
1879 ''' </summary>
1880 '''<param name="%"></param>
1881 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1882 '''         PScrewPos(2)    ：ねじ供給器回避点
1883 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1884 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1885 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1886 '''<returns>整数
1887 '''         0=異常終了、1=正常終了、-1=MネジセンサーNG、-2=MネジセンサーON、-3=吸着エラー
1888 '''</returns>
1889 ''' <remarks>
1890 ''' Date   : 2021/07/07 : M.Hayakawa
1891 ''' </remarks>'
1892 Function M% ScrewGet_S3(ByVal PScrewPosition())
1893     ScrewGet_S3 = 0
1894     MMScrewJudge% = 0
1895     'ねじ供給器初期動作エラーチェック
1896 ' ↓暫定削除
1897 '    Wait M_In(X34_ScrewReady1)=1 'ねじ供給器SがReadyになるまで待つ　←　何秒か待ってReadyにならなければ抜けるプログラムが必要？
1898 '    Ovrd 100
1899 '    If M_In(X33_SS2)=0 Then  'Mねじ検出センサがOFF（故障）していた場合
1900 '        Ovrd 30
1901 '        Mvs,-80             'その場所から80mm上空へ移動
1902 '        Mov PInitPos19049   '19049初期位置へ移動
1903 '        M_Out(Y68_VV1)=0    'ねじ吸着 Off
1904 '        'NGとしてここの関数から抜ける
1905 '        ScrewGet_S3 = -1
1906 '        MMScrewJudge% = 1
1907 '        MCommentD1001 = 61
1908 '    EndIf
1909 '    If ScrewGet_S3 = 0 Then
1910 '        'Sタイト用ねじ供給機にMねじが混入していないか監視
1911 '        MMScrewJudge% = 0 'MMScrewJudgeを初期化する
1912 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
1913 '        If MRtn = 0 Then
1914 '            Ovrd 30
1915 '            Mvs,-80            'その場所から50mm上空へ移動
1916 '            Mov PInitPos19049  '19049初期位置へ移動
1917 '            MMScrewJudge% = 2
1918 '            MRtn = All_CLamp_Release()'全てのクランプ解除へ分岐
1919 '            MCnt% = 2   '2を設定
1920 '            MCommentD1001 = 62
1921 '        EndIf
1922 '        If MMScrewJudge% = 2 Then
1923 '            ScrewGet_S3 = -2
1924 '        EndIf
1925 '    EndIf
1926 '    'Mネジ判定がONの場合 NGとして関数を抜ける
1927 '    If MMScrewJudge% = 2 Then
1928 '        ScrewGet_S3 = -2
1929 '    EndIf
1930     'Sネジ用ねじ太郎のMネジ混入確認用ここまで
1931     Ovrd 100
1932     Spd M_NSpd
1933     If MMScrewJudge% = 0 Then
1934         ScrewGet_S3 = 0
1935         M_Out(Y63_Driver)=1         ' バンクセッティング　C2
1936         MScrewCnt% = 0
1937         MFinCnt% = 2
1938 '        For MCnt% = 0 To MFinCnt%
1939             Mov PScrewPosition(2)        ' ねじ供給機回避点
1940             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1941             Ovrd 80
1942             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1943             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
1944             Mvs PScrewPosition(10), 1.2
1945             M_Out(Y68_VV1)=1 Dly 0.3        ' ねじ吸着　ON
1946             'ビット回転
1947             M_Out(Y60_Driver)=1
1948             Dly 0.2
1949             '
1950             Ovrd 100
1951             JOvrd M_NJovrd
1952             Spd M_NSpd
1953             'ネジ吸着確認位置移動
1954             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
1955             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
1956             'ビット回転停止
1957             'M_Out(Y60_Driver)=0
1958             '
1959             '1秒間ネジ吸着確認
1960 ' 以下暫定削除
1961 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
1962 '            'MRtn = 0'強制エラー
1963 '            '吸着エラーの場合
1964 '            'ネジをねじ太郎に戻す
1965 '            If MRtn = 0 Then
1966 '                Ovrd 30
1967 '                'ビット回転停止
1968 '                M_Out(Y60_Driver)=0
1969 '                'ネジ供給機上空
1970 '                Mvs PScrewPos(1)
1971 '                '更に上空
1972 '                Mov PScrewPos(1), -75
1973 '                'ネジ捨て位置
1974 '                Mov PScrewFeedS021
1975 '                '吸着OFF
1976 '                M_Out(Y68_VV1)=0 'ねじ吸着　OFF
1977 '                Dly 0.2
1978 '                '破壊ON
1979 '                M_Out(Y6B_VB1)=1 '真空破壊ON
1980 '                'ビット回転
1981 '                M_Out(Y61_Driver)=1
1982 '                Dly 0.5
1983 '                '
1984 '                Ovrd 100
1985 '                JOvrd M_NJovrd
1986 '                Spd M_NSpd
1987 '                'ドライバーを上下させねじを振り落とす
1988 '                Mov PScrewFeedS021, 10
1989 '                Mov PScrewFeedS021
1990 '                Dly 0.1
1991 '                Mov PScrewFeedS021, 10
1992 '                Mov PScrewFeedS021
1993 '                '
1994 '                'ネジ落ち待ち
1995 '                'ビット回転停止
1996 '                M_Out(Y61_Driver)=0
1997 '                Dly 0.1
1998 '                '破壊OFF
1999 '                M_Out(Y6B_VB1)=0 '真空破壊OFF
2000 '                '
2001 '                '
2002 '                'ねじ落ちたとして、移動更に上空
2003 '                Mov PScrewPos(1), -75
2004 '                Ovrd 100
2005 '                Spd M_NSpd
2006 '                'ネジ供給機上空
2007 '                Mvs PScrewPos(1)
2008 '                '
2009 '                ScrewGet_S3 = -3
2010 '                Break
2011 '                '
2012 '            Else
2013 '                MCnt% = MFinCnt%
2014 '                ScrewGet_S3 = 0
2015 '            EndIf
2016 '        Next  MCnt%
2017         '
2018         Ovrd 100
2019         Spd M_NSpd
2020         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2021         M_Out(Y60_Driver)=0     ' ビット回転停止
2022         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
2023         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2024         'もう一度吸着確認
2025 ' 以下暫定削除
2026 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2027 '        If MRtn = 0 Then      '吸着エラーの場合
2028 '            MCommentD1001 = 94
2029 '            MCommentD1002 = 95
2030 '            ScrewGet_S3 = -3
2031 '        EndIf
2032 '        If MRtn = 1 Then      '吸着OKの場合
2033 '            ScrewGet_S3 = 1
2034 '        EndIf
2035 '        Break
2036     Else
2037         'Mネジ
2038         If MMScrewJudge% = 2 Then
2039             ScrewGet_S3 = -2
2040         EndIf
2041     EndIf
2042     Exit Function
2043 FEnd
2044 '
2045 '■fnKEY_WAIT()
2046 ''' <summary>
2047 ''' GOTからのキー入力待ち
2048 ''' </summary>
2049 '''<returns>1：停止    2：次へ
2050 '''         3：継続    4：トルクチェック開始
2051 '''         5：NG
2052 '''         11：ロボット初期位置1    12：ロボット初期位置2
2053 '''         13：ロボット初期位置3    14：ロボット初期位置4
2054 '''</returns>
2055 ''' <remarks>
2056 ''' Date   : 2021/07/07 : M.Hayakawa
2057 ''' </remarks>'
2058 Function M% fnKEY_WAIT()
2059     fnKEY_WAIT = 0
2060     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
2061     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
2062     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
2063     '下記キー待ちの継続に反応させないため
2064     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
2065     Dly 0.2
2066     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
2067     MLocalLoopFlg=1
2068     While MLocalLoopFlg=1
2069         If M_In(11345) = 1 Then         '停止   M5345
2070             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
2071             fnKEY_WAIT = 1
2072             MLocalLoopFlg=-1
2073             Break
2074         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
2075             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
2076             fnKEY_WAIT = 2
2077             MLocalLoopFlg=-1
2078             Break
2079         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
2080             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
2081             fnKEY_WAIT = 3
2082             MLocalLoopFlg=-1
2083             Break
2084         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
2085             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
2086             fnKEY_WAIT = 4
2087             MLocalLoopFlg=-1
2088             Break
2089         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
2090             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
2091             fnKEY_WAIT = 5
2092             MLocalLoopFlg=-1
2093             Break
2094             '
2095         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
2096             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
2097             fnKEY_WAIT = MRobotInit1%
2098             MLocalLoopFlg=-1
2099             Break
2100             '
2101         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
2102             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
2103             fnKEY_WAIT = MRobotInit2%
2104             MLocalLoopFlg=-1
2105             Break
2106             '
2107         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
2108             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
2109             fnKEY_WAIT = MRobotInit3%
2110             MLocalLoopFlg=-1
2111             Break
2112             '
2113         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2114             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2115             fnKEY_WAIT = MRobotInit4%
2116             MLocalLoopFlg=-1
2117             Break
2118             '
2119         Else
2120         EndIf
2121     WEnd
2122     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2123     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2124     Exit Function
2125 FEnd
2126 '
2127 '■ fnAUTO_CTL
2128 ''' <summary>
2129 ''' AUTOモードOFF、PLCからの開始待ち
2130 ''' </summary>
2131 ''' <remarks>
2132 ''' Date   : 2021/07/07 : M.Hayakawa
2133 ''' </remarks>
2134 Function M% fnAUTO_CTL
2135     fnAUTO_CTL = 0
2136     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2137     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2138     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2139     '
2140     If M_Svo=0 Then             'サーボON確認
2141         Servo On
2142     EndIf
2143     Wait M_Svo=1
2144     Exit Function
2145 FEnd
2146 '
2147 '■ fnWindScreenOpen
2148 ''' <summary>
2149 ''' ウィンド画面の表示、非表示設定
2150 ''' </summary>
2151 '''<param name="%"></param>
2152 '''<param name="%"></param>
2153 '''<param name="%"></param>
2154 '''<param name="%"></param>
2155 ''' <remarks>
2156 ''' コメントD1001, D1002, D1003の設定
2157 ''' MWindReSet = 0     画面非表示
2158 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2159 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2160 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2161 ''' Date   : 2021/07/07 : M.Hayakawa
2162 ''' </remarks>
2163 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2164     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2165         M_Out16(12480) = MCommentD1001            'D1001 コメント
2166     EndIf
2167     '
2168     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2169         M_Out16(12496) = MCommentD1002            'D1002 コメント
2170     EndIf
2171     '
2172     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2173        M_Out16(12512) = MCommentD1003            'D1003 コメント
2174     EndIf
2175     '
2176     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2177     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2178     Dly 0.5
2179     M_Out(12363) = 0                         'ウィンド画面設定
2180     Exit Function
2181 FEnd
2182 '
2183 '■FnCtlValue2
2184 ''' <summary>
2185 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2186 ''' </summary>
2187 ''' <param name="MCtlNo%"></param>
2188 ''' <remarks>
2189 ''' Date : 2022/04/28 渡辺
2190 ''' </remarks>
2191 '''
2192 '''  1：投入数       ＋１
2193 '''  2：組立ＯＫ数   ＋１
2194 '''  3：組立ＮＧ数   ＋１ (未使用)
2195 '''  4：吸着エラー数 ＋１
2196 ''' 99：読書開始信号 OFF
2197 '''
2198 Function M% FnCtlValue2(ByVal MCtlNo%)
2199     FnCtlValue2 = 1
2200     Select MCtlNo%
2201         Case 1        '投入数＋１
2202             M_Out(12569) = 0             '書込み開始信号OFF
2203             M_Out(12568) = 1             '読込み開始信号ON
2204             MInputQty = M_In16(11600)    '投入数受信
2205             MInputQty = MInputQty + 1    '投入数＋１
2206             M_Out16(12592) = MInputQty   '投入数送信
2207             M_Out(12569) = 1             '書込み開始信号ON
2208             Break
2209             '
2210         Case 2        '組立ＯＫ数＋１
2211             M_Out(12569) = 0             '書込み開始信号OFF
2212             M_Out(12568) = 1             '読込み開始信号ON
2213             MAssyOkQty = M_In16(11616)   '組立OK数受信
2214             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2215             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2216             M_Out(12569) = 1             '書込み開始信号ON
2217             Break
2218             '
2219         Case 4        '吸着エラー数＋１
2220             M_Out(12569) = 0                       '書込み開始信号OFF
2221             M_Out(12568) = 1                       '読込み開始信号ON
2222             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2223             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2224             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2225             M_Out(12569) = 1                       '書込み開始信号ON
2226             Break
2227             '
2228         Case 99        '読書開始信号OFF
2229             M_Out(12568) = 0        '読込み開始信号OFF
2230             M_Out(12569) = 0        '書込み開始信号OFF
2231             Break
2232             '
2233     End Select
2234     Exit Function
2235 FEnd
2236 'Insightによる画像処理検査実行（並列処理なし）
2237 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2238 '-------------------------------------------------------------------------------
2239 'Insightによる画像処理検査実行（並列処理なし）
2240 '   引数
2241 '       PInspPos()      ：検査位置
2242 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2243 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2244 '       MInspCnt%       ：検査位置数
2245 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2246 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2247 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2248 '   戻り値：整数
2249 '       0=異常終了、1=正常終了
2250 '
2251 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2252 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2253 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2254 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2255 '   20200410    :   検査グループ設定Retry追加
2256 '-------------------------------------------------------------------------------
2257     '----- 初期設定 -----
2258     Cnt 0                                                           '移動効率化解除(初期値=0)
2259     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2260 '    Cnt 1,0.1,0.1
2261     '変数宣言・初期化
2262     Def Inte MNum                                                   '検査番号(検査順1～)
2263     MNum% = 1                                                       '検査番号初期値設定
2264     Def Inte MEndFlg                                                '検査終了フラグ
2265     MEndFlg% = 0
2266     '
2267     '検査G番号設定要求・検査実行要求off
2268     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2269     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2270     'エラー番号クリア
2271     MInspErrNum = 0                                                 '検査実行エラー番号
2272     M_Out16(MOUT_InspErrNum) = MInspErrNum
2273     MInspNGStepNum = 0                                              '検査実行NGStep番号
2274     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2275     '
2276     'Insight Ready check?
2277     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2278         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2279         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2280         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2281         ISInspectionSingle = 0                                      '異常終了戻り値設定
2282         Exit Function
2283     EndIf
2284     '
2285     '検査位置数確認
2286     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2287         MInspErrNum = 21                                            '検査データなし 21　引数<1
2288         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2289         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2290         ISInspectionSingle = 0                                      '異常終了戻り値設定
2291         Exit Function
2292     EndIf
2293     '
2294     '
2295     '
2296     '----- メイン処理 -----
2297     '設定された検査位置数分の検査実行
2298     While( MEndFlg% = 0 )
2299         '----- 検査グループ番号設定Retry追加 20200410
2300         MSetGrNumRetryExitFlg = 0
2301         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2302         While( MSetGrNumRetryExitFlg = 0 )
2303         '----- 検査グループ番号設定Retry追加ここまで 20200410
2304             '
2305             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2306             '
2307             '----- 検査グループ番号設定 -----
2308             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2309             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2310             '
2311             '検査位置へ移動・移動完了待ち
2312             Mvs PInspPos( MNum% )                                       '移動
2313             Dly 0.05                                                    '移動完了後Delay
2314             '
2315             '検査グループ番号設定終了確認
2316             M_Timer(1) = 0
2317             MExitFlg = 0
2318             While( MExitFlg = 0 )
2319                 '検査G設定正常終了?
2320                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2321                     MExitFlg = 1
2322                 '
2323                 '検査G設定異常終了?
2324                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2325                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2326                     If MInspErrNum = 0 Then                             '1回目のエラー?
2327                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2328                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2329                     EndIf
2330                     MExitFlg = 1
2331                 '
2332                 'timeoutチェック
2333                 ElseIf 1000 < M_Timer(1) Then
2334                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2335                     If MInspErrNum = 0 Then                             '1回目のエラー?
2336                         MInspErrNum = 12                                'timeout エラー番号=12
2337                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2338                     EndIf
2339                     MExitFlg = 1
2340                 EndIf
2341             WEnd
2342             '
2343             '検査G番号設定要求off
2344             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2345             '
2346             '----- 検査グループ設定Retry追加 20200410
2347             'NGなければ抜ける
2348             If MCurrentStepErr = 0 Then
2349                 MSetGrNumRetryExitFlg = 1
2350             Else
2351                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2352                 If MSetGrNumRetryCnt = 0 Then
2353                     MSetGrNumRetryExitFlg = 1
2354                 Else
2355                     'Retryへ　その前にDelay
2356                     Dly 0.5
2357                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2358                 EndIf
2359             EndIf
2360             '----- 検査グループ設定Retry追加ここまで 20200410
2361             '
2362         WEnd
2363         '
2364         '
2365         '
2366         '----- 検査実行 -----
2367         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2368             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2369                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2370                 MInspRetryExitFlg = 0
2371                 MRetryCnt = 2                                        'Retry回数設定
2372                 While( MInspRetryExitFlg = 0 )
2373                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2374                     '
2375                     '検査完了確認
2376                     MRetryCnt = MRetryCnt - 1
2377                     M_Timer(1) = 0
2378                     MExitFlg = 0
2379                     While( MExitFlg = 0 )
2380                     '検査完了待ち
2381                         '検査OK終了?
2382                         If M_In( MIN_IS_InspOK% ) = 1  Then
2383                             MJudgeOKFlg = 1                         '検査OKフラグON
2384                             MExitFlg = 1
2385                         '
2386                         '検査NG終了?
2387                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2388                             If MInspErrNum = 0 Then                 '1回目のエラー?
2389                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2390                                     MInspErrNum = 32                    '検査NG エラー番号=32
2391                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2392                                 EndIf
2393                             EndIf
2394                             MExitFlg = 1
2395                         '
2396                         '検査異常終了(IS timeout)?
2397                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2398                             If MInspErrNum = 0 Then                 '1回目のエラー?
2399                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2400                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2401                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2402                                 EndIf
2403                             EndIf
2404                             MExitFlg = 1
2405                         '
2406                         'timeoutチェック
2407                         ElseIf 3000 < M_Timer(1) Then
2408                             If MInspErrNum = 0 Then                 '1回目のエラー?
2409                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2410                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2411                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2412                                 EndIf
2413                             EndIf
2414                             MExitFlg = 1
2415                         EndIf
2416                     WEnd
2417                     '
2418                     '検査開始要求off
2419                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2420                     '
2421                     'OKなら抜ける
2422                     If MJudgeOKFlg = 1 Then
2423                         MInspRetryExitFlg = 1
2424                     Else
2425                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2426                         If MRetryCnt = 0 Then
2427                             MInspRetryExitFlg = 1
2428                         Else
2429                             'Retryへ　その前にDelay
2430                             Dly 0.3
2431                         EndIf
2432                     EndIf
2433                     '
2434                 WEnd
2435             EndIf
2436         EndIf
2437         '
2438         '
2439         '
2440         MNum% = MNum% + 1                                           '検査Step+1
2441         '検査終了確認　検査終了フラグセット
2442         If (MInspCnt% < MNum% ) Then
2443             MEndFlg% = 1                                            '検査終了フラグセット
2444         EndIf
2445         'NG発生時続行時処理
2446         If MInspErrNum <> 0 Then                                    'NGあり?
2447             If MNgContinue% <> 1 Then                               'NG続行?
2448                 MEndFlg% = 1                                        '検査終了フラグセット
2449             EndIf
2450         EndIf
2451     WEnd
2452     '
2453     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2454     If 0 < MZAxis% Then
2455         PCurrentPos = P_Curr                                        '現在位置取得
2456         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2457         Mvs PCurrentPos                                             '現在位置上空へ移動
2458     EndIf
2459     '
2460     '戻り値設定
2461     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
2462         ISInspectionSingle = 1                                      '正常終了戻り値設定
2463     Else
2464         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2465         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2466         ISInspectionSingle = 0                                      '異常終了戻り値設定
2467     EndIf
2468     '
2469     Fine 0 , P
2470     Exit Function
2471 FEnd
2472 '
2473 '■InitialZone
2474 ''' <summary>
2475 ''' 初期動作で現在位置を読み込んで最寄りの待避場所に移動
2476 ''' 設定された待避位置付近の場合はなにもしない。
2477 ''' それ以外の場合はオーバーライドを下げて移動。
2478 ''' </summary>
2479 ''' <remarks>
2480 ''' Date : 2022/01/24 : M.Hayakawa
2481 ''' </remarks>
2482 Function V fnInitialZone()
2483     PC = P_Curr
2484     Ovrd 5
2485 '    ColChk Off
2486 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2487 '    Cmp Pos, &B100011
2488     If (PC.X <= PPlateLCheck.X + 1.0) And (PC.X >= PPlateLCheck.X -1.0) And (PC.Y <= PPlateLCheck.Y + 1.0) And (PC.Y >= PPlateLCheck.Y -1.0) Then
2489         If (PC.Z <= PPlateLCheck.Z + 1.0) And (PC.Z >= PPlateLCheck.Z -1.0) Then
2490             Mov PPlateLCheck_2
2491         EndIf
2492     ElseIf (PC.X <= PPlateLCheck_2.X + 1.0) And (PC.X >= PPlateLCheck_2.X -1.0) And (PC.Y <= PPlateLCheck_2.Y + 1.0) And (PC.Y >= PPlateLCheck_2.Y -1.0) Then
2493         If (PC.Z <= PPlateLCheck_2.Z + 1.0) And (PC.Z >= PPlateLCheck_2.Z -1.0) Then
2494         EndIf
2495     ElseIf (PC.X <= PPlateLGet.X + 1.0) And (PC.X >= PPlateLGet.X -1.0) And (PC.Y <= PPlateLGet.Y + 1.0) And (PC.Y >= PPlateLGet.Y -1.0) Then
2496         If (PC.Z <= PPlateLGet.Z + 1.0) And (PC.Z >= PPlateLGet.Z -1.0) Then
2497             Mov PPlateLGet_1
2498         EndIf
2499     ElseIf (PC.X <= PPlateLGet_1.X + 1.0) And (PC.X >= PPlateLGet_1.X -1.0) And (PC.Y <= PPlateLGet_1.Y + 1.0) And (PC.Y >= PPlateLGet_1.Y -1.0) Then
2500         If (PC.Z <= PPlateLGet_1.Z + 1.0) And (PC.Z >= PPlateLGet_1.Z -1.0) Then
2501         EndIf
2502     ElseIf (PC.X <= PPlateLGet_2.X + 1.0) And (PC.X >= PPlateLGet_2.X -1.0) And (PC.Y <= PPlateLGet_2.Y + 1.0) And (PC.Y >= PPlateLGet_2.Y -1.0) Then
2503         If(PC.Z <= PPlateLGet_2.Z + 1.0) And (PC.Z >= PPlateLGet_2.Z -1.0) Then
2504             Break
2505         EndIf
2506     ElseIf (PC.X <= PPlateLSet.X + 1.0) And (PC.X >= PPlateLSet.X -1.0) And (PC.Y <= PPlateLSet.Y + 1.0) And (PC.Y >= PPlateLSet.Y -1.0) Then
2507         If (PC.Z <= PPlateLSet.Z + 1.0) And (PC.Z >= PPlateLSet.Z -1.0) Then
2508             Mov PPlateLSet_1
2509             Mov PPlateLSet_2
2510         EndIf
2511     ElseIf (PC.X <= PPlateLSet_1.X + 1.0) And (PC.X >= PPlateLSet_1.X -1.0) And (PC.Y <= PPlateLSet_1.Y + 1.0) And (PC.Y >= PPlateLSet_1.Y -1.0) Then
2512       If (PC.Z <= PPlateLSet_1.Z + 1.0) And (PC.Z >= PPlateLSet_1.Z -1.0) Then
2513         Mov PPlateLSet_2
2514         EndIf
2515     ElseIf (PC.X <= PPlateLSet_2.X + 1.0) And (PC.X >= PPlateLSet_2.X -1.0) And (PC.Y <= PPlateLSet_2.Y + 1.0) And (PC.Y >= PPlateLSet_2.Y -1.0) Then
2516         If  (PC.Z <= PPlateLSet_2.Z + 1.0) And (PC.Z >= PPlateLSet_2.Z -1.0) Then
2517         EndIf
2518     ElseIf (PC.X <= PPlateRCheck.X + 1.0) And (PC.X >= PPlateRCheck.X -1.0) And (PC.Y <= PPlateRCheck.Y + 1.0) And (PC.Y >= PPlateRCheck.Y -1.0) Then
2519         If (PC.Z <= PPlateRCheck.Z + 1.0) And (PC.Z >= PPlateRCheck.Z -1.0) Then
2520         Mov PPlateLCheck_2
2521         EndIf
2522     ElseIf (PC.X <= PPlateRCheck_2.X + 1.0) And (PC.X >= PPlateRCheck_2.X -1.0) And (PC.Y <= PPlateRCheck_2.Y + 1.0) And (PC.Y >= PPlateRCheck_2.Y -1.0) Then
2523         If (PC.Z <= PPlateRCheck_2.Z + 1.0) And (PC.Z >= PPlateRCheck_2.Z -1.0) Then
2524         EndIf
2525     ElseIf (PC.X <= PPlateRGet.X + 1.0) And (PC.X >= PPlateRGet.X -1.0) And (PC.Y <= PPlateRGet.Y + 1.0) And (PC.Y >= PPlateRGet.Y -1.0) Then
2526         If (PC.Z <= PPlateRGet.Z + 1.0) And (PC.Z >= PPlateRGet.Z -1.0) Then
2527             Mov PPlateRGet_1
2528             Mov PPlateRGet_2
2529             Mov PPlateRGet_3
2530             Mov PPlateRGet_4
2531         EndIf
2532     ElseIf (PC.X <= PPlateRGet_1.X + 1.0) And (PC.X >= PPlateRGet_1.X -1.0) And (PC.Y <= PPlateRGet_1.Y + 1.0) And (PC.Y >= PPlateRGet_1.Y -1.0) Then
2533         If (PC.Z <= PPlateRGet_1.Z + 1.0) And (PC.Z >= PPlateRGet_1.Z -1.0) Then
2534             Mov PPlateRGet_2
2535             Mov PPlateRGet_3
2536             Mov PPlateRGet_4
2537         EndIf
2538     ElseIf (PC.X <= PPlateRGet_2.X + 1.0) And (PC.X >= PPlateRGet_2.X -1.0) And (PC.Y <= PPlateRGet_2.Y + 1.0) And (PC.Y >= PPlateRGet_2.Y -1.0) Then
2539         If (PC.Z <= PPlateRGet_2.Z + 1.0) And (PC.Z >= PPlateRGet_2.Z -1.0) Then
2540             Mov PPlateRGet_3
2541             Mov PPlateRGet_4
2542         EndIf
2543     ElseIf (PC.X <= PPlateRGet_3.X + 1.0) And (PC.X >= PPlateRGet_3.X -1.0) And (PC.Y <= PPlateRGet_3.Y + 1.0) And (PC.Y >= PPlateRGet_3.Y -1.0) Then
2544         If (PC.Z <= PPlateRGet_3.Z + 1.0) And (PC.Z >= PPlateRGet_3.Z -1.0) Then
2545            Mov PPlateRGet_4
2546         EndIf
2547     ElseIf (PC.X <= PPlateRGet_4.X + 1.0) And (PC.X >= PPlateRGet_4.X -1.0) And (PC.Y <= PPlateRGet_4.Y + 1.0) And (PC.Y >= PPlateRGet_4.Y -1.0) Then
2548         If (PC.Z <= PPlateRGet_4.Z + 1.0) And (PC.Z >= PPlateRGet_4.Z -1.0) Then
2549         EndIf
2550     ElseIf (PC.X <= PPlateRSet.X + 1.0) And (PC.X >= PPlateRSet.X -1.0) And (PC.Y <= PPlateRSet.Y + 1.0) And (PC.Y >= PPlateRSet.Y -1.0) Then
2551         If (PC.Z <= PPlateRSet.Z + 1.0) And (PC.Z >= PPlateRSet.Z -1.0) Then
2552             Mov PPlateRSet_1
2553             Mov PPlateRSet_2
2554         EndIf
2555     ElseIf (PC.X <= PPlateRSet_1.X + 1.0) And (PC.X >= PPlateRSet_1.X -1.0) And (PC.Y <= PPlateRSet_1.Y + 1.0) And (PC.Y >= PPlateRSet_1.Y -1.0) Then
2556         If (PC.Z <= PPlateRSet_1.Z + 1.0) And (PC.Z >= PPlateRSet_1.Z -1.0) Then
2557             Mov PPlateRSet_2
2558         EndIf
2559     ElseIf (PC.X <= PPlateRSet_2.X + 1.0) And (PC.X >= PPlateRSet_2.X -1.0) And (PC.Y <= PPlateRSet_2.Y + 1.0) And (PC.Y >= PPlateRSet_2.Y -1.0) Then
2560         If (PC.Z <= PPlateRSet_2.Z + 1.0) And (PC.Z >= PPlateRSet_2.Z -1.0) Then
2561         EndIf
2562     ElseIf (PC.X <= PPlateRSet_3.X + 1.0) And (PC.X >= PPlateRSet_3.X -1.0) And (PC.Y <= PPlateRSet_3.Y + 1.0) And (PC.Y >= PPlateRSet_3.Y -1.0) Then
2563         If (PC.Z <= PPlateRSet_3.Z + 1.0) And (PC.Z >= PPlateRSet_3.Z -1.0) Then
2564         EndIf
2565     ElseIf (PC.X <= PProductOnPltGet.X + 1.0) And (PC.X >= PProductOnPltGet.X -1.0) And (PC.Y <= PProductOnPltGet.Y + 1.0) And (PC.Y >= PProductOnPltGet.Y -1.0) Then
2566         If (PC.Z <= PProductOnPltGet.Z + 1.0) And (PC.Z >= PProductOnPltGet.Z -1.0) Then
2567             'ハンドをイニシャルに戻す
2568             M_Out(12256) = 0    '本体チャック閉OFF
2569             M_Out(12257) = 1    '本体チャック開ON
2570             Mov PProductOnPltGet_2
2571         EndIf
2572     ElseIf (PC.X <= PProductOnPltGet_1.X + 1.0) And (PC.X >= PProductOnPltGet_1.X -1.0) And (PC.Y <= PProductOnPltGet_1.Y + 1.0) And (PC.Y >= PProductOnPltGet_1.Y -1.0) Then
2573         If (PC.Z <= PProductOnPltGet_1.Z + 1.0) And (PC.Z >= PProductOnPltGet_1.Z -1.0) Then
2574             Mov PProductOnPltGet_2
2575         EndIf
2576     ElseIf (PC.X <= PProductOnPltGet_2.X + 1.0) And (PC.X >= PProductOnPltGet_2.X -1.0) And (PC.Y <= PProductOnPltGet_2.Y + 1.0) And (PC.Y >= PProductOnPltGet_2.Y -1.0) Then
2577         If (PC.Z <= PProductOnPltGet_2.Z + 1.0) And (PC.Z >= PProductOnPltGet_2.Z -1.0) Then
2578         EndIf
2579     ElseIf (PC.X <= PProductOnPltSet.X + 1.0) And (PC.X >= PProductOnPltSet.X -1.0) And (PC.Y <= PProductOnPltSet.Y + 1.0) And (PC.Y >= PProductOnPltSet.Y -1.0) Then
2580         If (PC.Z <= PProductOnPltSet.Z + 1.0) And (PC.Z >= PProductOnPltSet.Z -1.0) Then
2581             'ハンドをイニシャルに戻す
2582             M_Out(12256) = 0    '本体チャック閉OFF
2583             M_Out(12257) = 1    '本体チャック開ON
2584             Mov PProductOnPltGet_2
2585         EndIf
2586     ElseIf (PC.X <= PProductOnPltSet_1.X + 1.0) And (PC.X >= PProductOnPltSet_1.X -1.0) And (PC.Y <= PProductOnPltSet_1.Y + 1.0) And (PC.Y >= PProductOnPltSet_1.Y -1.0) Then
2587         If (PC.Z <= PProductOnPltSet_1.Z + 1.0) And (PC.Z >= PProductOnPltSet_1.Z -1.0) Then
2588             Mov PProductOnPltGet_2
2589         EndIf
2590     ElseIf (PC.X <= PProductOnPltSet_2.X + 1.0) And (PC.X >= PProductOnPltSet_2.X -1.0) And (PC.Y <= PProductOnPltSet_2.Y + 1.0) And (PC.Y >= PProductOnPltSet_2.Y -1.0) Then
2591         If (PC.Z <= PProductOnPltSet_2.Z + 1.0) And (PC.Z >= PProductOnPltSet_2.Z -1.0) Then
2592         EndIf
2593     ElseIf (PC.X <= PProductOnRoboGet.X + 1.0) And (PC.X >= PProductOnRoboGet.X -1.0) And (PC.Y <= PProductOnRoboGet.Y + 1.0) And (PC.Y >= PProductOnRoboGet.Y -1.0) Then
2594         If (PC.Z <= PProductOnRoboGet.Z + 1.0) And (PC.Z >= PProductOnRoboGet.Z -1.0) Then
2595             Mov PProductOnRoboGet_2
2596         EndIf
2597     ElseIf (PC.X <= PProductOnRoboGet_1.X + 1.0) And (PC.X >= PProductOnRoboGet_1.X -1.0) And (PC.Y <= PProductOnRoboGet_1.Y + 1.0) And (PC.Y >= PProductOnRoboGet_1.Y -1.0) Then
2598         If (PC.Z <= PProductOnRoboGet_1.Z + 1.0) And (PC.Z >= PProductOnRoboGet_1.Z -1.0) Then
2599             Mov PProductOnRoboGet_2
2600         EndIf
2601     ElseIf (PC.X <= PProductOnRoboGet_2.X + 1.0) And (PC.X >= PProductOnRoboGet_2.X -1.0) And (PC.Y <= PProductOnRoboGet_2.Y + 1.0) And (PC.Y >= PProductOnRoboGet_2.Y -1.0) Then
2602         If (PC.Z <= PProductOnRoboGet_2.Z + 1.0) And (PC.Z >= PProductOnRoboGet_2.Z -1.0) Then
2603         EndIf
2604     ElseIf (PC.X <= PProductOnRoboSet.X + 1.0) And (PC.X >= PProductOnRoboSet.X -1.0) And (PC.Y <= PProductOnRoboSet.Y + 1.0) And (PC.Y >= PProductOnRoboSet.Y -1.0) Then
2605         If (PC.Z <= PProductOnRoboSet.Z + 1.0) And (PC.Z >= PProductOnRoboSet.Z -1.0) Then
2606             Mov PProductOnRoboGet_2
2607         EndIf
2608     ElseIf (PC.X <= PProductOnRoboSet_1.X + 1.0) And (PC.X >= PProductOnRoboSet_1.X -1.0) And (PC.Y <= PProductOnRoboSet_1.Y + 1.0) And (PC.Y >= PProductOnRoboSet_1.Y -1.0) Then
2609         If (PC.Z <= PProductOnRoboSet_1.Z + 1.0) And (PC.Z >= PProductOnRoboSet_1.Z -1.0) Then
2610             Mov PProductOnRoboGet_2
2611         EndIf
2612     ElseIf (PC.X <= PProductOnRoboSet_2.X + 1.0) And (PC.X >= PProductOnRoboSet_2.X -1.0) And (PC.Y <= PProductOnRoboSet_2.Y + 1.0) And (PC.Y >= PProductOnRoboSet_2.Y -1.0) Then
2613         If (PC.Z <= PProductOnRoboSet_2.Z + 1.0) And (PC.Z >= PProductOnRoboSet_2.Z -1.0) Then
2614         EndIf
2615     Else
2616         fnMoveToEscapePosition()
2617         Break
2618     EndIf
2619     Mov PInitialPosition
2620     Cmp Off
2621     ColChk On
2622     Exit Function
2623 FEnd
2624 '
2625 '■MoveToShuntPosition
2626 ''' <summary>
2627 ''' 現在位置を読み込んで最寄りの待避場所に移動
2628 ''' </summary>
2629 ''' <remarks>
2630 ''' Date    : 2022/01/24 : M.Hayakawa
2631 ''' </remarks>
2632 Function V fnMoveToEscapePosition()
2633     PC = P_Curr
2634     Ovrd 5
2635     If Zone2(PC,PTicketRead, PTicketRead_1,10) = 1 Then
2636         Mov PTicketRead_1
2637         Break
2638     ElseIf Zone2(PC,PTicketRead_1, PProductOnPltGet_2, 20) = 1 Then
2639         Break
2640     ElseIf Zone2(PC,PProductOnPltGet_2, PInitialPosition, 20) = 1 Then
2641         Break
2642     ElseIf Zone2(PC,PProductOnPltGet_1, PProductOnPltGet, 5) = 1 Then
2643         Break
2644     ElseIf Zone2(PC,PProductOnPltGet_1, PProductOnPltGet_2, 10) = 1 Then
2645         Break
2646     ElseIf Zone2(PC,PProductOnPltGet_2, PProductOnRoboSet_2, 30) = 1 Then
2647         Break
2648     ElseIf Zone2(PC,PProductOnRoboSet_2, PProductOnRoboSet_1, 10) = 1 Then
2649         Break
2650     ElseIf Zone2(PC,PProductOnRoboSet_1, PProductOnRoboSet, 5) = 1 Then
2651         Break
2652     ElseIf Zone2(PC,PProductOnRoboSet_1, PProductOnRoboSet_2, 10) = 1 Then
2653         Break
2654     ElseIf Zone2(PC,PProductOnRoboSet_2, PPlateLGet_2, 30) = 1 Then
2655         Break
2656     ElseIf Zone2(PC,PPlateLGet_2, PInitialPosition, 10) = 1 Then
2657         Break
2658     ElseIf Zone2(PC,PPlateLGet_2, PPlateLGet_1, 10) = 1 Then
2659         Break
2660     ElseIf Zone2(PC,PPlateLGet_1, PPlateLGet, 5) = 1 Then
2661         Break
2662     ElseIf Zone2(PC,PPlateLSet_2, PPlateLSet_1, 10) = 1 Then
2663         Break
2664     ElseIf Zone2(PC,PPlateLSet_1, PPlateLSet, 5) = 1 Then
2665         Break
2666     ElseIf Zone2(PC,PPlateLSet_2, PInitialPosition, 10) = 1 Then
2667         Break
2668     ElseIf Zone2(PC,PPlateLSet_2, PPlateLCheck_2,10) = 1 Then
2669         Break
2670     ElseIf Zone2(PC,PPlateLCheck_2, PPlateLCheck, 10) = 1 Then
2671         Break
2672     ElseIf Zone2(PC,PPlateLCheck, PPlateRGet_4, 10) = 1 Then
2673         Break
2674     ElseIf Zone2(PC,PPlateRGet_4, PPlateRGet_3, 10) = 1 Then
2675         Break
2676     ElseIf Zone2(PC,PPlateRGet_3, PPlateRGet_2,30) = 1 Then
2677         Break
2678     ElseIf Zone2(PC,PPlateRGet_2, PPlateRGet_1, 10) = 1 Then
2679         Break
2680     ElseIf Zone2(PC,PPlateRGet_4, PInitialPosition, 30) = 1 Then
2681         Break
2682     ElseIf Zone2(PC,PPlateRGet_1, PPlateRGet, 5) = 1 Then
2683         Break
2684     ElseIf Zone2(PC,PPlateRGet_4, PPlateRSet_3, 10) = 1 Then
2685         Break
2686     ElseIf Zone2(PC,PPlateRSet_3, PPlateRSet_2, 10) = 1 Then
2687         Break
2688     ElseIf Zone2(PC,PPlateRSet_2, PPlateRSet_1, 10) = 1 Then
2689         Break
2690     ElseIf Zone2(PC,PPlateRSet_2, PInitialPosition, 30) = 1 Then
2691         Break
2692     ElseIf Zone2(PC,PPlateRSet_1, PPlateRSet,5) = 1 Then
2693         Break
2694     ElseIf Zone2(PC,PPlateRSet_2, PPlateRCheck_2, 10) = 1 Then
2695         Break
2696     ElseIf Zone2(PC,PPlateRCheck_2, PPlateRCheck, 5) = 1 Then
2697         Break
2698     ElseIf Zone2(PC,PPlateRCheck, PProductOnRoboGet_2, 10) = 1 Then
2699         Break
2700     ElseIf Zone2(PC,PProductOnRoboGet_2, PInitialPosition, 30) = 1 Then
2701         Break
2702     ElseIf Zone2(PC,PProductOnRoboGet_2, PProductOnRoboGet_1, 10) = 1 Then
2703         Break
2704     ElseIf Zone2(PC,PProductOnRoboGet_1, PProductOnRoboGet, 5) = 1 Then
2705         Break
2706     ElseIf Zone2(PC,PProductOnRoboGet_2, PProductOnPltSet_2, 30) = 1 Then
2707         Break
2708     ElseIf Zone2(PC,PProductOnPltSet_2, PProductOnPltSet_1, 10) = 1 Then
2709         Break
2710     ElseIf Zone2(PC,PProductOnPltSet_1, PProductOnPltSet, 5) = 1 Then
2711         Break
2712     ElseIf Zone2(PC,PProductOnPltSet_2, PTicketRead_1, 10) = 1 Then
2713         Break
2714     ElseIf Zone2(PC,PTicketRead_1, PInitialPosition, 10) = 1 Then
2715         Mov PInitialPosition
2716         Break
2717     Else
2718         fErrorProcess(11,247,281,0)
2719         Break
2720     EndIf
2721     Mov PInitialPosition
2722     Exit Function
2723 FEnd
2724 '
2725 '■fnAutoScreenComment
2726 ''' <summary>
2727 ''' メイン画面の動作状況表示
2728 ''' コメントD1005の設定
2729 ''' </summary>
2730 '''<param name="McommentD1005%">コメントID</param>
2731 ''' <remarks>
2732 ''' Date   : 2021/07/07 : M.Hayakawa
2733 ''' </remarks>
2734 Function fnAutoScreenComment(ByVal McommentD1005%)
2735     M_Out16(12576) = McommentD1005%
2736     Exit Function
2737 FEnd
2738 '
2739 '■InitialZoneB
2740 ''' <summary>
2741 ''' 非常停止後の復帰動作
2742 ''' 1)上空退避　Z方向上に移動
2743 ''' 2)J1軸以外を退避ポジションへ移動
2744 ''' 3)J1軸のみを退避ポジションへ移動
2745 ''' 4)イニシャルポジションへ移動
2746 ''' </summary>
2747 ''' <remarks>
2748 ''' Date : 2022/04/08 : N.Watanabe
2749 ''' </remarks>
2750 Function V fnInitialZoneB()
2751     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/04/27 渡辺
2752 '
2753 'パラメータ
2754     Ovrd 5
2755 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2756 '    Cmp Pos, &B100011
2757 '
2758 '復帰動作開始
2759 '
2760 '置き台と両掴みの場所は、チャックを解放する
2761 *RecoveryChuckOpen
2762     PActive = P_Curr          '現在位置を取得
2763     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
2764 'PProductOnRoboSet(ねじロボ1本体置き位置)は、チャック解放
2765     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2766         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2767             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2768                 MRecoveryChuckOpen = 1
2769             EndIf
2770         EndIf
2771     EndIf
2772 'PProductOnRoboGet(ねじロボ1本体取り位置)は、チャック解放
2773     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2774         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2775             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2776                 MRecoveryChuckOpen = 1
2777             EndIf
2778         EndIf
2779     EndIf
2780     If MRecoveryChuckOpen = 1 Then
2781         M_Out(12256) = 0        '本体チャック閉OFF
2782         M_Out(12257) = 1        '本体チャック開ON
2783         M_20# = 0               'KEY入力初期化
2784         MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
2785         If MRtn = 0 Then
2786             fErrorProcess(11,244,284,0)
2787             If M_20# = MNext% Then M_20# = MClear%
2788             If M_20# = MAbout% Then GoTo *RecoveryEnd
2789             If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2790             If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2791         Else
2792             M_Out(12257) = 0        '本体チャック開OFF
2793         EndIf
2794     EndIf
2795 '
2796 '上空退避
2797     PActive = P_Curr
2798     Pmove = PActive
2799     Pmove.Z = 640           '上空退避する一律の高さ
2800     If PActive.X > 550 Then
2801         Pmove.Z =480        'パレット上に腕を伸ばしているときは640まで上げられない為、例外処置
2802     EndIf
2803     If PActive.Z < Pmove.Z Then   '現在の高さがPmoveより低いとのみ実行
2804         Mvs Pmove
2805     EndIf
2806 '
2807     Dly 1.0
2808 'J1軸以外を退避ポジションへ移動
2809     JActive = J_Curr
2810     Jmove = JTaihi
2811     Jmove.J1 = JActive.J1        'J1軸のみ現在値を使用し、他の軸はJTaihiのポーズを取る
2812     Mov Jmove
2813     Dly 1.0
2814 'J1軸のみを退避ポジションへ移動
2815     Mov JTaihi
2816     Dly 1.0
2817 'イニシャルポジションへ移動
2818     Mov PInitialPosition
2819     Cmp Off
2820 ' ねじロボを初期位置に戻すために強制的に自動運転開始
2821     If M_In(11856) = 0 Then                 ' 停止中のみ
2822         fnAutoScreenComment(501)            ' 状態表示[ネジ締め機自動運転開始中] 2022/04/27 渡辺
2823         M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
2824         MRet = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
2825         If MRet = 0 Then
2826         Else
2827             M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
2828         EndIf
2829     EndIf
2830     M_Out(12264) = 0            '位置決め出OFF
2831     M_Out(12265) = 1            '位置決め戻ON
2832    fErrorProcess(11,253,281,0)
2833     Exit Function
2834 *RecoveryEnd
2835 FEnd
2836 '
2837 '
2838 '■fnRoboPosChk
2839 ''' <summary>
2840 ''' 最後に終了したロボットポジションの確認
2841 ''' </summary>
2842 '''<param name="MINNumber%">入力番号</param>
2843 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2844 '''<param name="MTimeCnt&">タイムアウト時間</param>
2845 ''' PLCに保続した番号を読込み、確認
2846 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2847 '''<returns>整数 0:タイムアウト 1:OK</returns>
2848 ''' <remarks>
2849 ''' Date   : 2021/07/07 : M.Hayakawa
2850 ''' </remarks>
2851 Function M% fnRoboPosChk
2852     fnRoboPosChk = 0
2853     MRet = fnStepRead()
2854     '初期位置でないと判断した場合
2855     'ウィンド画面切換え
2856     If MRBTOpeGroupNo > 5 Then
2857         '下記キー待ちの継続に反応させないため
2858         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2859         Dly 0.2
2860         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2861         Dly 1.5
2862         '
2863         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2864         '
2865         MLoopFlg% = 1
2866         While MLoopFlg% = 1
2867             '
2868             '
2869             MKeyNumber% = fnKEY_WAIT()
2870             Select MKeyNumber%
2871                 Case Is = MAbout%       '停止
2872                     M_20# = MAbout%
2873                     MLoopFlg% = -1
2874                     Break
2875                 Case Is = MNext%        '次へ
2876                     'MLoopFlg% = -1
2877                     Break
2878                 Case Is = MContinue%    '継続
2879                     M_20# = MContinue%
2880                     MLoopFlg% = -1
2881                     Break
2882                 Default
2883                     Break
2884             End Select
2885         WEnd
2886     EndIf
2887     '
2888     If M_20# = MContinue% Then                              '継続ボタンが押された場合
2889         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
2890         Ovrd 5                                   '低速オーバーライド値設定
2891         Select MRBTOpeGroupNo
2892             Case Is = 5                          '何もしない
2893                 Break
2894             Case Is = 10                         '初期位置へ戻す
2895                 'Mov PTEST001
2896                 Break
2897             Case Is = 15                         '初期位置へ戻す
2898                 'Mov PTEST002
2899                 Dly 0.5
2900                 'Mov PTEST001
2901                 Dly 0.5
2902                 Break
2903             Default
2904                 Break
2905         End Select
2906         '
2907         Ovrd M_NOvrd                            'システムの初期値を設定
2908         M_Out(12364) = 1                        'toPLC_データ保存ON
2909         MRBTOpeGroupNo = 5
2910         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
2911         Dly 1.0
2912         M_Out(12364) = 0                        'toPLC_データ保存OFF
2913         fnRoboPosChk = 1                        '初期位置動作実行
2914         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
2915     EndIf
2916     Exit Function
2917 FEnd
2918 '
2919 '■frInCheck
2920 ''' <summary>
2921 ''' センサーINチェック
2922 ''' </summary>
2923 '''<param name="MINNumber%">入力番号</param>
2924 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2925 '''<param name="MTimeCnt&">タイムアウト時間</param>
2926 '''<returns>整数 0:タイムアウト 1:OK</returns>
2927 ''' <remarks>
2928 ''' Date   : 2021/07/07 : M.Hayakawa
2929 ''' </remarks>
2930 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2931     M_Timer(4) = 0
2932     MloopFlg = 0
2933     While MloopFlg = 0
2934         MCrtTime& = M_Timer(4)
2935         If M_In(MINNumber%) = MCMPFLG% Then
2936             MloopFlg = 1
2937             frInCheck = 1
2938         ElseIf MCrtTime& > MTimeCnt& Then
2939             MloopFlg = 1
2940             frInCheck = 0
2941         EndIf
2942     WEnd
2943     Exit Function
2944 FEnd
2945 '-----------------------------------------------
2946 '
2947 'ねじ締め機通信確認
2948 '
2949 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2950 'fScewTcomChk = 0　：正常終了
2951 '          　　 -1 ：異常終了
2952 '-----------------------------------------------
2953 Function M% fScewTcomChk
2954 *ReCheckScewTcomChk
2955     fScewTcomChk = 0
2956     '通信確認送信
2957     M_Out(MOUT_ScwT_ComChk%) = MOn%
2958     '通信確認受信待機
2959 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
2960     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
2961     '通信確認送信終了
2962     M_Out(MOUT_ScwT_ComChk%) = MOff%
2963     If MRtn = 0 Then
2964         fScewTcomChk = -1
2965     EndIf
2966     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
2967  '
2968 FEnd
2969 '
2970 '
2971 '-----------------------------------------------
2972 '
2973 'ねじ締め開始送信
2974 '
2975 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2976 'fScewTStart = 0　：正常終了
2977 '          　　-1 ：異常終了
2978 '-----------------------------------------------
2979 Function M% fScewTStart
2980     fScewTStart = 0
2981     nRet% = 0
2982     'ねじ締め開始待機を受信
2983 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
2984     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
2985     If MRtn = 0 Then nRet% = -1
2986     If MRtn = 0 Then GoTo *ScrewStartERROR      '開始できなかった場合ジャンプ
2987     Dly 0.1
2988     'ねじ締め開始受信を送信
2989     M_Out(MOUT_ScwT_ST%) = MOn%
2990     Dly 0.5
2991     'Wait M_In(MTEST_KEY%) = MOn%
2992     'ねじ締め開始送信終了
2993     M_Out(MOUT_ScwT_ST%) = MOff%
2994     '
2995 *ScrewStartERROR
2996     fScewTStart = nRet%
2997 FEnd
2998 '
2999 '
3000 '
3001 '-----------------------------------------------
3002 '
3003 'ねじ締め完了受信
3004 '
3005 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3006 'fScewTcomChk = 0　：正常終了
3007 '          　 　-1 ：異常終了
3008 '-----------------------------------------------
3009 Function M% fScewTFinish
3010 *ReCheckScewTFinish
3011     fScewTFinish = 0
3012     'ねじ締め完了待機を受信
3013 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3014     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3015     If MRtn = 0 Then
3016         fScewTFinish = -1
3017     EndIf
3018     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3019     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3020     Dly 0.1
3021     'ねじ締め完了受信を送信
3022     M_Out(MOUT_ScwT_FinOK%) = MOn%
3023     Dly 0.5                          'とりあえず保持時間0.5msec
3024     'ねじ締め開始送信終了
3025     M_Out(MOUT_ScwT_FinOK%) = MOff%
3026     'Wait M_In(MTEST_KEY%) = MOn%
3027     '
3028 *ScewTFinish_ErrEnd
3029 FEnd
3030 '
3031 '
3032 '-----------------------------------------------
3033 '
3034 '条件xx停止受信
3035 '
3036 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3037 'fScewTCaseStop = 0　：正常終了
3038 '          　   　-1 ：異常終了
3039 '-----------------------------------------------
3040 Function M% fScewTCaseStop(ByVal MCase%())
3041 *ReCheckScewTCaseStop
3042     fScewTCaseStop = 0
3043     '条件xx停止を受信
3044     Wait M_In(MCase%(1)) = MOn%
3045     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3046     If MRtn = 0 Then
3047         fScewTCaseStop = -1
3048     EndIf
3049     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3050     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3051     Dly 0.1
3052     '条件xx停止受信を送信
3053     M_Out(MCase%(2)) = MOn%
3054     Dly 0.5                          'とりあえず保持時間0.5msec
3055     'ねじ締め開始送信終了
3056     M_Out(MCase%(2)) = MOff%
3057 *ScewTCaseStop_ErrEnd
3058     '
3059 FEnd
3060 '
3061 '-----------------------------------------------
3062 '
3063 '再開始受信
3064 '
3065 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3066 'fScewTReStart = 0　：正常終了
3067 '              　-1 ：異常終了
3068 '-----------------------------------------------
3069 Function M% fScewTReStart()
3070 *ReCheckScewTReStart
3071     fScewTReStart = 0
3072     '再開始を受信
3073     Wait M_In(MIN_ScwT_ReST%) = MOn%
3074     MRtn = fTimeOutJudge(MIN_ScwT_ReST%,MOn%)
3075     If MRtn = 2 Then GoTo *ReCheckScewTReStart
3076     If MRtn = 0 Then GoTo *ScewTReStart_ErrEnd
3077     Dly 0.1
3078     '再開始受信を送信
3079     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msecパルス
3080 *ScewTReStart_ErrEnd
3081     Exit Function
3082 FEnd
3083 '
3084 '■fScrewTighenRoboCheck
3085 '<summary>
3086 'ねじロボ監視
3087 '</summary>
3088 '<param name = "MStopNum%"> 停止番号</param>
3089 '<returns>整数 0:ねじロボ異常終了 1:OK </returns>
3090 '<make>
3091 '2021/12/2 中村天哉
3092 '</make>
3093 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3094     fnAutoScreenComment(503)    '状態表示[ねじロボ動作終了待ち] 2022/04/27 渡辺
3095     fScrewTighenRoboCheck = 1
3096     MScrewTighenRoboFlg% = 1    'フラグの初期化
3097     MCheck% = 0
3098     While MScrewTighenRoboFlg% = 1
3099         MCheck% = M_In16(11904)
3100         If M_In(MStopNum%) = 1 Then '停止位置まで来たら
3101             MScrewTighenRoboFlg% = 0 '関数を抜ける
3102             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/27 渡辺
3103         EndIf
3104         If MCheck% <> 0 Then
3105             fScrewTighenRoboError(MCheck%)
3106             Select M_20#
3107                 Case MAbout%            '停止が押された場合
3108                     M_Out(12869) = 1 Dly 1.0
3109                     MScrewTighenRoboFlg% = 0
3110                     fScrewTighenRoboCheck = 0   '異常終了
3111                     Break
3112                 Case MNgProcess%        'NGが押された場合
3113                     M_Out(12873) = 1 Dly 1.0
3114                     MScrewTighenRoboFlg% = 0
3115                     fScrewTighenRoboCheck = 0   '異常終了
3116                     Break
3117                 Case MContinue%             'リトライが押された場合
3118                     M_20# = MClear%         'M_20#初期化
3119                     M_Out(12871) = 1 Dly 1.0
3120                     Break
3121                 Case MNext%                 '次へが押された場合
3122                     M_20# = MClear%         'M_20#初期化
3123                     M_Out(12874) = 1 Dly 1.0
3124                     Break
3125             End Select
3126             Dly 0.5
3127         EndIf
3128     WEnd
3129     Exit Function
3130 FEnd
3131 '■fScrewTighenRoboError
3132 '<summary>
3133 'ねじロボエラー処理
3134 '</summary>
3135 '<param name = "ErrorCode%"> エラー番号</param>
3136 '<make>
3137 '2021/12/2 中村天哉
3138 '</make>
3139 Function fScrewTighenRoboError(ErrorCode%)
3140     MCommentD1001 = ErrorCode% + 300
3141     fErrorProcess(11,MCommentD1001,0,0)
3142     Exit Function
3143 FEnd
3144 '■fErrorProcess
3145 '<summary>
3146 'エラー処理
3147 '</summary>
3148 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
3149 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
3150 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
3151 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
3152 '<make>
3153 '2021/11/5 中村天哉
3154 '</make>
3155 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3156     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
3157     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
3158     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
3159     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
3160 *RETRY_ERR_PROCESS
3161      M_20# = MClear%     '初期化
3162 '        'エラー処理記述
3163         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3164 '        'GOT KEY入力待ち
3165         MKeyNumber = fnKEY_WAIT()
3166 '        '
3167         If MKeyNumber = MAbout% Then   '停止を選択した場合
3168             M_20# = MAbout%            'M_20# プログラム間共通外部変数
3169  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3170             Break
3171          '
3172         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
3173             M_20# = MContinue%            'M_20# プログラム間共通外部変数
3174  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3175             Break
3176         '
3177         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
3178             M_20# = MNext%            'M_20# プログラム間共通外部変数
3179  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3180             Break
3181          '
3182         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
3183             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
3184  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3185             Break
3186         '
3187         EndIf
3188         '
3189         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3190         fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3191     Exit Function
3192 FEnd
3193 '
3194 '■fnTorqueCheck
3195 ''' <summary>
3196 ''' トルクチェック動作用のメイン
3197 ''' </summary>
3198 ''' <remarks>
3199 ''' Date   : 2021/12/21 : H.AJI
3200 ''' </remarks>'
3201 Function M% fnTorqueCheck
3202     'トルクチェック中送信  搬送系停止
3203     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
3204     '
3205     fnTorqueCheck = 0
3206     Ovrd 20
3207     'Mov PInitialPosition              '初期位置移動
3208     Ovrd 100
3209     '下記キー待ちの継続に反応させないため
3210     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3211     Dly 0.2
3212     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3213     '
3214     'M6340  トルクチェック受信
3215     'Dly 5.0
3216     M_Out(12340) = 1          'トルクチェック受信 M6340
3217     Dly 1.0
3218     M_Out(12340) = 0
3219     '
3220     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3221     M_Out(12835) = 1                         'ねじロボトルクチェック画面切替
3222    Wait M_In(11843) = 1                         'ねじロボトルクチェック画面切替
3223     M_Out(12835) = 0                         'ねじロボトルクチェック画面切替完了
3224     '
3225     '
3226     MLoopFlg = 1
3227     While MLoopFlg = 1
3228         '
3229         'Mov PInitialPosition              '初期位置移動
3230         '
3231         MKeyNumber = fnKEY_WAIT()
3232         Select MKeyNumber
3233             Case Is = 1           '停止
3234                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3235                 Dly 1.0
3236                 M_Out(12343) = 0
3237                 Ovrd 20
3238                 'Mov PTicketRead_1
3239                 M_Out(12840) = 1          'トルクチェック終了
3240                 Wait M_In(11859) = 1      'ねじロボからの終了
3241                 M_Out(12840) = 0          'トルクチェック終了
3242                 Ovrd 100
3243                 M_20# = 1
3244                 MLoopFlg = -1
3245                 Break
3246             Case Is = 2           '次へ
3247                 Break
3248             Case Is = 3           '継続
3249                 Break
3250             Case Is = 4           'トルクチェック開始
3251                 M_Out(12342) = 1          'トルクチェック開始要求受信 M6342
3252                 Dly 1.0
3253                 M_Out(12342) = 0
3254                 If M_In(11862) = 1 Then             'トルクチェッカー確認
3255                     fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3256                     MRet = fnScrewMTorque()           'ねじロボ用トルクチェック
3257                 EndIf
3258                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3259                 'MRet = fnMoveTorquePosi()
3260                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3261                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3262                 Break
3263             Default
3264                 Break
3265         End Select
3266     WEnd
3267     '
3268     'トルクチェック中停止送信
3269     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3270     '
3271     'ロボットの位置を元に戻す
3272     '
3273     Exit Function
3274  FEnd
3275  '
3276 '
3277 '
3278 '---------------------------
3279 '
3280 '    メイン画面の表示、非表示設定
3281 '         コメントD1001, D1002, D1003の設定
3282 '           MWindReSet = 0     画面非表示
3283 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3284 '           MWindErrScr = 10    エラー画面 D1001, D1002
3285 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3286 '
3287 '---------------------------
3288 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3289     fnMainScreenOpen = 0
3290     '
3291    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3292         M_Out16(12480) = MCommentD1001            'D1001 コメント
3293     EndIf
3294     '
3295     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3296         M_Out16(12496) = MCommentD1002            'D1002 コメント
3297     EndIf
3298     '
3299     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3300         M_Out16(12512) = MCommentD1003            'D1003 コメント
3301     EndIf
3302     '
3303     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3304     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3305     Dly 0.5
3306     M_Out(12362) = 0                         'ウィンド画面設定
3307     Exit Function
3308 FEnd
3309 '
3310 '■Main
3311 ''' <summary>
3312 ''' トルクチェック実動作
3313 ''' </summary>
3314 ''' <remarks>
3315 ''' Date   : 2021/12/21 : H.AJI
3316 ''' </remarks>'
3317 Function M% fnScrewMTorque
3318     fnScrewMTorque = 0
3319     M_Out(12838) = 1                         'トルクチェック開始1
3320     Wait M_In(11857) = 1                     '受信完了
3321     M_Out(12838) = 0                         'トルクチェック開始1
3322     Dly 2.0
3323     Exit Function
3324 FEnd
3325 '
3326 '----------------------------------------------------------------
3327 'fTimeOutJudge
3328 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3329 '引数
3330 'Address% = 監視アドレス番号
3331 'JudgeFlg% = 対象アドレスの正常終了時の値
3332 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3333 '戻り値 = 0 エラー
3334 '         1 正常終了
3335 '         2 リトライ
3336 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3337 '作成日
3338 '2022/9/20 中村
3339 '----------------------------------------------------------------
3340 '
3341 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3342     fTimeOutJudge = 0
3343     MJudge% = 1
3344     MRtn = 0
3345     M_20# = MClear%
3346     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3347 *TimeOutLoop
3348     If MRtn = 1 Then GoTo *TimeOut
3349         fErrorProcess(11,202,203,0)
3350         If M_20# = MNext% Then GoTo *TimeOutLoop
3351         If M_20# = MContinue% Then MJudge% = 2
3352         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3353 *TimeOut
3354     fTimeOutJudge = MJudge%
3355 '
3356 *JUDGE_ERROR_END
3357 FEnd
3358 '
3359 '■Main
3360 ''' <summary>
3361 ''' 組立動作用のメイン
3362 ''' </summary>
3363 ''' <remarks>
3364 ''' Date   : 2021/07/07 : M.Hayakawa
3365 ''' </remarks>'
3366 Function Main
3367     MopeNo = M_21#         '外部変数にて動作番号代入
3368     '
3369     If M_Svo=0 Then
3370         Servo On
3371     EndIf
3372     Wait M_Svo=1
3373 '組立スタート日付時刻要求パルスON
3374     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3375 'パトライト操作
3376     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3377     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3378     '
3379     M_20# = 0                                   'KEY入力初期化
3380     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
3381     MRet% = 0
3382 '初期位置の確認と移動
3383 '
3384 '復帰動作　実行・未実行判別      2022/04/08 渡辺 作成
3385     PActive = P_Curr                    '現在位置を取得
3386     MRecoveryPass% = 0
3387     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3388         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3389             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3390                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3391             EndIf
3392         EndIf
3393     EndIf
3394     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3395         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3396             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3397                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3398             EndIf
3399         EndIf
3400     EndIf
3401     If MRecoveryPass% = 0 Then
3402        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3403     EndIf
3404 '
3405 '
3406     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3407         M_Out(12364) = 1            'toPLC_データ保存ON
3408 'トルクチェック
3409         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3410             MRet% = fnTorqueCheck()
3411             Break
3412         Else
3413 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
3414 '                MRtn = InspInit()               '画像処理初期化処理
3415 '            EndIf
3416             '
3417            M_20# = MClear%                    '初期化
3418 '組立開始
3419             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3420                 MRet% = fnAssyStart()
3421             Else
3422                 M_20# = MPass%
3423             EndIf
3424 '組立終了日付時刻
3425             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3426             Wait M_In(11572) = 1            '日付取得完了
3427             Dly 0.1
3428             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3429 'リフターユニットへのOUT
3430             '  KEY入力が何もない場合 OKと判断
3431             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3432             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3433 'OK/NGフラグ出力
3434             If M_20# <= 0 Then
3435                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3436             ElseIf M_20# = MPass% Then
3437                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3438             EndIf
3439 'PIASに組立完了書込み
3440             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3441                 If M_20# = MPass% Then
3442                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3443                 Else
3444                     'KEY入力がNGの場合
3445                     If M_20# = MNgProcess% Then
3446                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3447                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3448                         MRet% = fnPiasWrite(MNG%)
3449                        nAssyNgQty = nAssyNgQty + 1
3450                     EndIf
3451                     '
3452                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3453                     If M_20# = MAssyOK% Then
3454                             '-----------------------
3455                             'D732 -> D2600 コピー要求
3456                             M_Out(12566) = 1
3457 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3458                             M_Out(12566) = 0
3459                             '
3460                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3461                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3462                             '基板番号照合(PPは未使用）
3463 '                            MRet% = fnPCBNumberCheck()
3464                         Else
3465                             MRet% = 1
3466                         EndIf
3467                         '
3468                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3469                             If M_20# <> MAbout% Then
3470                                 '工程履歴OK書き込み
3471                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3472                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3473                                 MRet% = fnPiasWrite(MOK%)
3474                                 nAssyOkQty = 0
3475                                 nAssyOkQty = nAssyOkQty + 1
3476                             Else
3477                                 nAssyOkQty = nAssyOkQty + 1
3478                             EndIf
3479                         EndIf
3480                     EndIf
3481 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3482 '                    MRet% = fnPiasWrite(MOK%)
3483                 EndIf
3484             Else
3485                 nAssyOkQty = nAssyOkQty + 1
3486             EndIf
3487             '
3488             '組立終了日付時刻解除
3489             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3490             '投入数、組立OK数、組立NG数書込み
3491 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3492             '
3493 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3494 '                '画像処理終了処理
3495 '                MRtn = InspQuit()
3496 '            EndIf
3497         EndIf
3498         M_Out(12364) = 0                          'toPLC_データ保存OFF
3499     EndIf
3500 'パトライト操作
3501     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3502     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3503 'GOT表示
3504     fnAutoScreenComment(93)  'AUTO画面 工程完了
3505 FEnd
3506 End
3507 '
3508 'おまじないコメント
3509 '絶対削除するな
3510 '
3511 ''
3512 '
3513 '
PInspPosition(1)=(-55.67,-374.21,+601.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
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
PTemp=(+599.21,-290.97,+540.00,+180.00,+0.00,-90.00,+0.00,+0.00)(7,0)
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
PActive=(+599.21,-290.97,+540.00,+180.00,+0.00,-90.00,+0.00,+0.00)(7,0)
Pmove=(+547.57,-98.78,+640.00,-180.00,+0.00,-179.50,+0.00,+0.00)(7,0)
PInitialPosition=(+350.00,+0.00,+540.00,+180.00,+0.00,+180.00)(7,0)
PPlateLCheck=(-53.94,-594.69,+601.00,-180.00,+0.00,+90.00)(7,0)
PPlateLCheck_2=(-53.94,-594.69,+631.00,+180.00,+0.00,+90.00)(7,0)
PPlateLGet=(+640.51,-162.13,+441.78,+179.52,-0.30,+91.57)(7,0)
PPlateLGet_1=(+640.51,-162.13,+460.00,+179.52,-0.30,+91.57)(7,0)
PPlateLGet_2=(+396.00,-160.55,+600.00,+179.49,+0.10,+91.01)(7,0)
PPlateLSet=(+48.29,-583.12,+545.10,+179.96,-0.32,-178.48)(7,0)
PPlateLSet_1=(+48.29,-583.12,+590.00,+179.96,-0.32,-178.48)(7,0)
PPlateLSet_2=(+44.56,-280.00,+640.60,+179.98,-0.12,-179.36)(7,0)
PPlateRCheck=(-55.67,-374.21,+601.00,-180.00,+0.00,+90.00)(7,0)
PPlateRCheck_2=(-55.67,-374.21,+631.00,-180.00,+0.00,+90.00)(7,0)
PPlateRGet=(-511.99,+86.93,+361.89,-179.41,+0.79,+2.57)(7,15)
PPlateRGet_1=(-511.99,+86.93,+400.00,-179.41,+0.79,+2.57)(7,15)
PPlateRGet_2=(-323.09,-0.06,+640.58,+179.98,-0.13,+1.25)(7,0)
PPlateRGet_3=(-271.20,+14.83,+640.56,+180.00,+0.00,-3.13)(7,15)
PPlateRGet_4=(+0.00,-271.61,+640.53,+180.00,+0.00,+90.00)(7,0)
PPlateRSet=(+42.80,-525.93,+544.80,-179.99,+0.91,-178.90)(7,0)
PPlateRSet_1=(+42.80,-525.93,+570.00,-179.99,+0.91,-178.90)(7,0)
PPlateRSet_2=(+45.33,-320.00,+640.00,+180.00,+0.00,-178.68)(7,0)
PPlateRSet_3=(+0.01,-336.64,+640.48,+180.00,-0.01,+90.00)(7,0)
PProductOnPltGet=(+547.57,-100.01,+413.85,+180.00,+0.00,-179.49)(7,0)
PProductOnPltGet_1=(+547.57,-100.01,+460.00,+180.00,+0.00,-179.49)(7,0)
PProductOnPltGet_2=(+547.57,-100.01,+530.00,+180.00,+0.00,-179.49)(7,0)
PProductOnPltSet=(+547.57,-98.71,+413.85,-180.00,+0.00,-179.49)(7,0)
PProductOnPltSet_1=(+547.57,-98.71,+460.00,-180.00,+0.00,-179.49)(7,0)
PProductOnPltSet_2=(+547.57,-98.71,+530.00,-180.00,+0.00,-179.49)(7,0)
PProductOnRoboGet=(+104.36,-555.82,+468.02,+179.84,+0.00,-179.68)(7,0)
PProductOnRoboGet_1=(+104.36,-555.82,+500.00,+179.84,+0.00,-179.68)(7,0)
PProductOnRoboGet_2=(+104.36,-555.82,+640.00,+179.84,+0.00,-179.68)(7,0)
PProductOnRoboSet=(+104.38,-556.26,+468.02,+179.84,+0.03,-179.68)(7,0)
PProductOnRoboSet_1=(+104.38,-556.26,+510.00,+179.84,+0.03,-179.68)(7,0)
PProductOnRoboSet_2=(+104.38,-556.26,+640.00,+179.84,+0.03,-179.68)(7,0)
PTicketRead=(+599.21,-290.97,+473.00,-180.00,+0.00,-90.00)(7,0)
PTicketRead_1=(+599.21,-290.97,+540.00,-180.00,+0.00,-90.00)(7,0)
JActive=(-10.23,+33.00,+58.57,+0.00,+88.43,-10.73,+0.00,+0.00)
Jmove=(-10.23,-9.85,+108.99,+0.00,+80.50,+0.00,+0.00,+0.00)
JTaihi=(+0.00,-9.85,+108.99,+0.00,+80.50,+0.00,+0.00,+0.00)
