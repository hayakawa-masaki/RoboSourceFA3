1 ' ===================================
2 '
3 '  2100301001 STEP5 Assy1プログラム
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
23 '
24 Def Inte MIN_IS_ErrNum              '【入力IO】Insight処理エラー番号取得開始アドレス(16bit)
25 'Output Signal
26 Def Inte MOUT_IS_JobLoadReq         '【出力IO】Insight JOBロード要求
27 Def Inte MOUT_IS_InspGSetReq        '【出力IO】Insight 検査グループ番号設定要求
28 Def Inte MOUT_IS_Insp               '【出力IO】Insight 検査実行要求
29 '
30 Def Inte MOUT_IS_JobNum             '【出力IO】Insight JOB番号設定開始アドレス(16bit)
31 Def Inte MOUT_IS_InspGNum           '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
32 '
33 Def Inte MOUT_InspErrNum            '【出力IO】検査実行エラー番号開始アドレス(16bit)
34 Def Inte MOUT_InspNGStepNum         '【出力IO】検査実行NGStep番号開始アドレス(16bit)
35 '作業用変数
36 Def Inte MInspErrNum                '検査実行エラー番号
37 Def Inte MInspNGStepNum             '検査実行NGStep番号
38 Def Inte MRtn                       'Function戻り値取得用
39 Def Inte MRtn2                      'Function戻り値取得用
40 Def Inte MRet3                      'Function戻り値取得用
41 Def Inte MGRtn                      'Function戻り値取得用 ネジ供給機
42 Def Inte MInspErrNumSub             '検査実行エラー番号sub　20190820追加
43 Def Inte MovrdA                     'ネジ締めOvrd 可変用
44 Def Float MSpdA                     'ネジ締めSpd　可変用
45 Def Pos PTemp                       'ネジ締め上空位置計算用
46 '===== <Insight変数設定> =====
47 MIN_IS_Ready%        =   11380      '【入力IO】Insight準備OK
48 MIN_IS_JobLoadOK%    =   11381      '【入力IO】Insightジョブロード正常終了
49 MIN_IS_JobLoadNG%    =   11382      '【入力IO】Insightジョブロード異常終了
50 MIN_IS_InspGSetOK%   =   11383      '【入力IO】Insight検査グループ番号設定正常終了
51 MIN_IS_InspGSetNG%   =   11384      '【入力IO】Insight検査グループ番号設定異常終了
52 MIN_IS_InspOK%       =   11385      '【入力IO】Insight検査OK
53 MIN_IS_InspNG%       =   11386      '【入力IO】Insight検査NG
54 MIN_IS_InspErr%      =   11387      '【入力IO】Insight検査異常終了
55 MIN_IS_InspCapDone%  =   11388      '【入力IO】Insight検査画像取込完了
56 MIN_IS_ErrNum%       =   11408      '【入力IO】Insight処理エラー番号開始アドレス(16bit)
57 'Output Signal
58 MOUT_IS_JobLoadReq%  =   12370      '【出力IO】Insight JOBロード要求
59 MOUT_IS_InspGSetReq% =   12371      '【出力IO】Insight 検査グループ番号設定要求
60 MOUT_IS_Insp%        =   12372      '【出力IO】Insight 検査実行要求
61 MOUT_IS_JobNum%      =   12384      '【出力IO】Insight JOB番号設定開始アドレス(16bit)
62 MOUT_IS_InspGNum%    =   12400      '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
63 MOUT_InspErrNum%     =   12416      '【出力IO】検査実行エラー番号開始アドレス(16bit)
64 MOUT_InspNGStepNum%  =   12432      '【出力IO】検査実行NGStep番号開始アドレス(16bit)
65 '===== <電ドラ変数定義> =====
66 X20_Driver=11248                    '電ドラステイタス1　Driver Status 1
67 X21_Driver=11249 '電ドラステイタス2  Driver Status 2
68 X22_Driver=11250 '電ドラステイタス3  Driver Status 3
69 X23_Driver=11251 '電ドラステイタス4  Driver Status 4
70 X24_Driver=11252 '電ドラエラーメッセージ1 Driver Error E1
71 X25_Driver=11253 '電ドラエラーメッセージ2 Driver Error E2
72 X26_Driver=11254 '電ドラエラーメッセージ3 Driver Error E3
73 X27_Driver=11255 '電ドラエラーメッセージ4 Driver Error E4
74 X28_Driver=11256 '電ドラトータルエラーシグナル Total Error
75 X29_Driver=11257 '電ドラ終了シグナル Comlete signal
76 X2A_Driver=11258 '電ドラエラーメッセージ5 Driver Error E5
77 '11584   'toRBトルクドライバ-COMP_ERR送信
78 Y60_Driver=12240 '電ドラ半時計回り CCW
79 Y61_Driver=12241 '電ドラ時計回り CW
80 Y62_Driver=12242 'バンクセッティング BANK C1
81 Y63_Driver=12243 'バンクセッティング BANK C2
82 Y64_Driver=12244 'バンクセッティング BANK C3
83 Y65_Driver=12245 'プログラムセッティング PRG SET F1
84 Y66_Driver=12246 'プログラムセッティング PRG SET F2
85 Y67_Driver=12247 'プログラムセッティング PRG SET F3
86 X34_ScrewReady1=11259 'ねじっこ1　Read
87 '===== <電ドラ定数> =====
88 Dim PScrewPos(10)       'ネジ締め用Function引数変数
89 Dim PGetScrewPos(10)    'ねじ供給機からねじを得るFunction引数変数
90 Dim PEscapePosi(10)
91 MLoopCnt% = 0'
92 '===== <ロボット定数> =====
93 '===== <ロボット変数定義> =====
94 MRBTOpeGroupNo = 0      'ロボット動作番号初期化
95 MCommentD1001 = 0
96 MCommentD1002 = 0
97 MCommentD1003 = 0
98 MScreenNo = 0
99 '
100 MCommentTSU = 0
101 MCommentTSD = 0
102 'ウィンド画面番号設定
103 MWindReSet = 0
104 MWindInfoScr = 5
105 MWindErrScr = 10
106 MWindErrScr2 = 11
107 MWindErrScr3 = 13
108 MWindErrScr17 = 17
109 MWindErrScr18 = 18
110 MWindCmmnScr = 20
111 MWindJigRelase19049 = 60
112 MWindJigRelase19050 = 61
113 MWindJigRelase19051 = 62
114 '
115 MClear% = 0        'KEY_のクリア
116 MAbout% = 1        'KEY_停止
117 MNext% = 2         'KEY_次のステップへ移行
118 MContinue% = 3     'KEY_継続 再度同じ動作を行う
119 '
120 Def Inte MNgProcess
121 MNgProcess% = 5      'KEY_NG
122 '
123 MAssyOK% = 6       '組立完了
124 MPass% = 7         '工程パス
125 MPiasNG% = 8       'Pias確認時履歴NG
126 '
127 '初期化用KEY番号   '
128 MRobotInit1% = 11  '初期位置用
129 MRobotInit2% = 12  '初期位置用
130 MRobotInit3% = 13  '初期位置用
131 MRobotInit4% = 14  '初期位置用
132 '
133 MIN_INIT1REQUEST% = 11568 'toRBT_ロボット初期位置1要求
134 MIN_INIT2REQUEST% = 11569 'toRBT_ロボット初期位置2要求
135 MIN_INIT3REQUEST% = 11570 'toRBT_ロボット初期位置3要求
136 MIN_INIT4REQUEST% = 11571 'toRBT_ロボット初期位置4要求
137 '
138 MOUT_INIT1RECIVE% = 12560 'toPLC_ロボット初期位置1受信
139 MOUT_INIT2RECIVE% = 12561 'toPLC_ロボット初期位置2受信
140 MOUT_INIT3RECIVE% = 12562 'toPLC_ロボット初期位置3受信
141 MOUT_INIT4RECIVE% = 12563 'toPLC_ロボット初期位置4受信
142 '
143 MOK% = 1               '各判定用
144 MNG% = 0               '各判定用
145 MTIMEOUT% = -1         '各判定用
146 MJudge% = 0            '判定情報格納用
147 '
148 MRECIVETIME& = 0
149 MSETTIMEOUT10& = 10000&                '10秒設定
150 MSETTIMEOUT03& = 3000&                 '3秒設定
151 MSETTIMEOUT01& = 1000&                 '1秒設定
152 MSETTIMEOUT05& = 5000&                 '5秒設定
153 MSETTIMEOUT009& = 900&                 '0.9秒設定
154 MSETTIMEOUT008& = 800&                 '0.8秒設定
155 MSETTIMEOUT007& = 700&                 '0.7秒設定
156 MSETTIMEOUT006& = 600&                 '0.6秒設定
157 MSETTIMEOUT005& = 500&                 '0.5秒設定
158 MSETTIMEOUT004& = 400&                 '0.4秒設定
159 MSETTIMEOUT003& = 300&                 '0.3秒設定
160 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
161 MIN_PIAS_ComOK% = 11552                'PC通信OK
162 MIN_PIAS_ComTimeOut% = 11576           'PC通信確認タイムアウト
163 MIN_PIAS_ComNG% = 11553                'PC通信NG
164 MOUT_PIAS_ComCheck% = 12544            'PC通信確認要求
165 MOUT_PIAS_Missing_Process% = 12546     '工程抜け確認要求
166 MIN_PIAS_ModelTypeNG% = 11554          'モデル仕向NG
167 MIN_PIAS_ProcessHistryNG% = 11555      '前工程履歴NG
168 MIN_PIAS_ProcessHistryOK% = 11556      '前工程履歴OK
169 MIN_PIAS_ProcessHistryErr% = 11557     '工程履歴処理エラー
170 MIN_PIAS_MyProcessComp% = 11573        '自工程履歴あり
171 MIN_PIAS_ProcessHistryTimeOut% = 11578 '工程履歴タイムアウト
172 MOUT_OKNG% = 12549                     'PLC OUT でOK=1, NG=0 出力
173 '
174 MOUT_PiasPCBNumberCheck = 12557        '基板番号照合
175 MIN_PiasPCBNumberOK% = 11566          '基板番号OK
176 MIN_PiasPCBNumberNG% = 11565          '基板番号NG
177 MIN_PiasPCBNumberErr% = 11567         '基板番号処理エラー
178 '
179 MOUT_PiasAssyResultOK% = 12549    '組立OK
180 MOUT_PiasAssyResultNG% = 12550    '組立NG
181 MOUT_PiasAssyResultWr% = 12548    '工程履歴書き込み
182 '
183 MIN_PiasProcessNG% = 11559        '工程履歴処理NG
184 MIN_PiasProcessOtherErr% = 11560  '工程履歴処理エラー(なんかのトラブル)
185 MIN_PiasProcessOK% = 11558        '工程履歴処理OK
186 '
187 MIN_Insight_Use% = 11369               '画像確認ON
188 MIN_TorqueCheck% = 11348               'トルクチェック
189 '
190 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT操作権
191 MOUT_RED_LIGHT% = 12356            'PATLIGHT 赤 点灯
192 MOUT_RED_FLASH% = 12357            'PATLIGHT 赤 点滅
193 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT 黄 点灯
194 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT 黄 点滅
195 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT 青 点灯
196 MOUT_GREEN_FLASH% = 12361          'PATLIGHT 青 点滅
197 '
198 MOUT_ST_DATETIME% = 12551          '組立開始日付時刻
199 MOUT_ED_DATETIME% = 12552          '組立終了日付時刻
200 '
201 MOUT_TORQUE_CHECK% = 12367         'PLCへトルクチェック中を送信
202 '
203 MIN_ASSY_CANCEL% = 11366           '組立を行うかのフラグ
204 '
205 MLoopFlg% = 0                      'KEY入力後のOK or NG内容
206 MopeNo% = 0
207 MRtn% = 0
208 MRet = 0
209 MRet3% = 0
210 '
211 Def Inte MInputQty          '投入数 演算変数
212 Def Inte MAssyOkQty         '組立ＯＫ数 演算変数
213 Def Inte MAssyNgQty         '組立ＮＧ数 演算変数(未使用)
214 Def Inte MSuctionErrQty     '吸着エラー数 演算変数 2022/04/27 渡辺
215 Def Inte nAssyOkQty         '未使用
216 Def Inte MScrewNo
217 Def Inte MReTry
218 '===== <IO変数定義> =====
219 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
220 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
221 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
222 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
223 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
224 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
225 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
226 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
227 '
228 Def Inte Y68_VV1            ' アーム先端　ネジ吸着バルブ
229 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
230 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
231 '
232 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
233 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
234 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
235 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
236 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
237 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
238 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
239 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
240 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
241 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
242 '
243 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
244 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
245 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
246 '
247 Def Inte MOUT_LED1          ' 画像処理用LED照明
248 '
249 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
250 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
251 '
252 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
253 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
254 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
255 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
256 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
257 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
258 '
259 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
260 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
261 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
262 '
263 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
264 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
265 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
266 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
267 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
268 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
269 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
270 Y68_VV1%    =  12248    ' アーム先端　ネジ吸着バルブ '数値12250から12248へ変更(8/5中村)
271 Y6B_VB1%    =  12250    'アーム先端　吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
272 MOUT_VB1%   =  12250    ' アーム先端　ネジ吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
273 '
274 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
275 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
276 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
277 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
278 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
279 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
280 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
281 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
282 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
283 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
284 '
285 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
286 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
287 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
288 '
289 MOUT_LED1%  =  12239    ' 画像処理用LED照明
290 '
291 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
292 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
293 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
294 '
295 '共通
296 Def Inte MTEST_KEY                      'デバックテスト用
297 Def Inte MOn                            '出力=1
298 Def Inte MOff                           '出力=0
299 '
300 'ねじ締め装置_出力アドレス
301 Def Inte MOUT_ScwT_ComChk               '通信確認
302 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
303 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
304 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
305 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
306 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
307 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
308 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
309 'ねじ締め装置_入力アドレス
310 Def Inte MIN_ScwT_comOK                 '通信確認返信
311 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
312 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
313 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
314 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
315 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
316 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
317 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
318 '
319 Dim MScwT_Case1%(2)               '条件1停止変数
320 Dim MScwT_Case2%(2)               '条件2停止変数
321 Dim MScwT_Case3%(2)               '条件3停止変数
322 Dim MScwT_Case4%(2)               '条件4停止変数
323 Dim MScwT_Case5%(2)               '条件5停止変数
324 '
325 '共通
326 MTEST_KEY% = 11359                       'デバッグ用テストKEY
327 MOn% = 1                                 '出力 = 1
328 MOff% = 0                                '出力 = 0
329 '
330 'ねじ締め機_アドレス設定
331 MOUT_ScwT_ComChk% = 12832               '通信確認送信
332 MOUT_ScwT_ST% = 12865                   'ねじ締め開始を送信
333 MOUT_ScwT_ReSTOK% = 12866               '再開始受信を送信
334 MOUT_ScwT_FinOK% = 12868                'ねじ締め完了受信を送信
335 MOUT_ScwT_Case1OK% = 12874              '条件1停止受信を送信
336 MOUT_ScwT_Case2OK% = 12875              '条件2停止受信を送信
337 MOUT_ScwT_Case3OK% = 12876              '条件3停止受信を送信
338 MOUT_ScwT_Case4OK% = 12877              '条件4停止受信を送信
339 MOUT_ScwT_Case5OK% = 12878              '条件5停止受信を送信
340 '
341 MIN_ScwT_comOK% = 11840                 'ねじ締め装置から返信
342 MIN_ScwT_STRec% = 11873                 'ねじ締め開始を受信
343 MIN_ScwT_ReST% = 11874                  '再開始を受信
344 MIN_ScwT_Fin% = 11876                   'ねじ締め完了を受信
345 MIN_ScwT_Case1% = 11882                 '条件1停止待機を受信
346 MIN_ScwT_Case2% = 11883                 '条件2停止待機を受信
347 MIN_ScwT_Case3% = 11884                 '条件3停止待機を受信
348 MIN_ScwT_Case4% = 11885                 '条件4停止待機を受信
349 MIN_ScwT_Case5% = 11886                 '条件5停止待機を受信
350 '
351 MScwT_Case1%(1) = MIN_ScwT_Case1%
352 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
353 MScwT_Case2%(1) = MIN_ScwT_Case2%
354 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
355 MScwT_Case3%(1) = MIN_ScwT_Case3%
356 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
357 MScwT_Case4%(1) = MIN_ScwT_Case4%
358 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
359 MScwT_Case5%(1) = MIN_ScwT_Case5%
360 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
361 '
362 '設定 InitialZoneBで使用する変数
363 Def Pos PActive       '直交座標系 位置変数 現在位置
364 Def Pos Pmove         '直交座標系 位置変数 移動先
365 Def Jnt JActive       '関節座標系 位置変数 現在位置
366 Def Jnt Jmove         '関節座標系 位置変数 移動先
367 Def Jnt JTaihi        '関節座標系 位置変数 退避ポジション ティーチングで設定
368 Def Inte MRecoveryPass      '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行
369 Def Inte MStandby              '待機位置確認フラグ
370 Def Inte MRecoveryChuckOpen    'チャック解放フラグ（復帰動作前）両掴み対策
371 '★注意★初期位置を変更した時には、変更が必要！
372 '
373 '
374 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
375 Function M% fnAssyStart
376     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/27 渡辺
377     M_20# = MClear%                       '初期化
378 '
379 '    fScewTStart()           '処理位置変更
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
520 If M_20# = MContinue% Then M_20# = MClear%
521 'PInspPosition(1) = PTicketRead  'IDチケット読取位置
522 'MInspGroup%(1) = 1              '検査G番号
523 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
524 'M_20# = MClear%                       '初期化
525 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
526     MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
527     '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
528     '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
529 EndIf
530 If MRtn = 1 Then GoTo *CompRead
531 'fErrorProcess(11,244,284,0)
532 If M_20# = MNext% Then M_20# = MClear%
533 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
534 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
535 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
536 'If M_20# = MNext% Then M_20# = MPass%
537 Mov PTicketRead_1                        'エラー終了時イニシャルに戻る
538 Mov PInitialPosition
539 GoTo *ASSY_ERROR_END
540 *CompRead
541 '
542 Mov PTicketRead_1               'チケットID読み取り回避点
543 'Dly 5                   'デバッグ用(22/09/30中村)
544 '    'ねじ締め開始(処理位置変更2/16中村)
545     MRtn2 = fScewTStart()
546     If MRtn2 = 0 Then GoTo *INITIAL_CHECK
547         fErrorProcess(11,329,201,0)
548         If M_20# = MNext% Then GoTo *CompRead
549         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
550         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
551         If M_20# = MContinue% Then GoTo *CompRead
552 '
553 '
554 *INITIAL_CHECK
555 '
556 '
557 'パレットから製品を取る
558 Mov PProductOnPltGet_2      '本体回避点
559 *RE_PLT_GET_1
560 If M_20# = MContinue% Then M_20# = MClear%
561 '
562 M_Out(12256) = 0            '本体チャック閉OFF
563 M_Out(12257) = 1            '本体チャック開ON
564 '
565 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)        '本体チャック開センサーON
566 If MRtn = 1 Then GoTo *CompPltGet_1
567 fErrorProcess(11,244,284,0)
568 If M_20# = MNext% Then M_20# = MClear%
569 If M_20# = MAbout% Or M_20# = MNgProcess% Then
570     Mov PInitialPosition
571     M_Out(12264) = 0            '位置決め出OFF
572     M_Out(12265) = 1            '位置決め戻ON
573     Break
574 EndIf
575 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
576 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
577 If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
578 *CompPltGet_1
579 '
580 'Mov PProductOnPltGet_1      '本体上空
581 'Wait M_In(11278) = 1        '位置決め出端検出(コメントアウト11/4中村)
582 MRtn = frInCheck(11278,1,MSETTIMEOUT05&)        '位置決め出端検出
583 If MRtn = 1 Then GoTo *CompPltGet_2
584 fErrorProcess(11,231,282,0)
585 If M_20# = MNext% Then M_20# = MClear%
586 If M_20# = MAbout% Or M_20# = MNgProcess% Then
587     Mov PProductOnPltGet_2
588     Mov PInitialPosition
589     M_Out(12264) = 0            '位置決め出OFF
590     M_Out(12265) = 1            '位置決め戻ON
591     Break
592 EndIf
593 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
594 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
595 If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
596 *CompPltGet_2
597 Mov PProductOnPltGet_1      '本体上空
598 '
599 M_Out(12264) = 0            '位置決め出OFF
600 M_Out(12265) = 1            '位置決め戻ON
601 Ovrd 25
602 Mvs PProductOnPltGet        '本体を取る位置
603 '
604 *RE_PLT_GET_2
605 '
606 If M_20# = MContinue% Then M_20# = MClear%
607 '
608 MRtn = frInCheck(11279,1,MSETTIMEOUT05&)    '位置決め戻端検出
609 If MRtn = 1 Then GoTo *CompPushIni
610 fErrorProcess(11,234,284,0)
611 If M_20# = MNext% Then M_20# = MClear%
612 If M_20# = MAbout% Or M_20# = MNgProcess% Then
613     Mvs PProductOnPltGet_1
614     Mov PProductOnPltGet_2
615     Mov PInitialPosition
616 EndIf
617 If M_20# = MAbout% Or M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
618 If M_20# = MContinue% Then
619     M_Out(12265) = 0            '位置決め戻OFF
620     M_Out(12264) = 1            '位置決め出ON
621     Dly 1.0
622     M_Out(12264) = 0                            '位置決め出OFF
623     M_Out(12265) = 1                            '位置決め戻ON
624 EndIf
625 If M_20# = MContinue% Then GoTo *RE_PLT_GET_2
626 *CompPushIni
627 '
628 M_Out(12257) = 0            '本体チャック開OFF
629 M_Out(12256) = 1            '本体チャック閉ON
630 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '本体チャック閉センサーON
631 If MRtn = 1 Then GoTo *CompPltGet_3
632 M_Out(12256) = 0            '本体チャック閉OFF
633 M_Out(12257) = 1            '本体チャック開ON
634 Dly 2.0
635 Mvs PProductOnPltGet_1
636 Mov PProductOnPltGet_2
637 M_Out(12257) = 0            '本体チャック開OFF
638 M_Out(12256) = 1            '本体チャック閉ON
639 fErrorProcess(11,244,284,0)
640 If M_20# = MNext% Then M_20# = MClear%
641 If M_20# = MAbout% Or M_20# = MNgProcess% Then
642     Mov PInitialPosition
643     Break
644 EndIf
645 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
646 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
647 M_Out(12256) = 0            '本体チャック閉OFF
648 M_Out(12257) = 1            '本体チャック開ON
649 Dly 2.0
650 Mov PProductOnPltGet_1
651 Mvs PProductOnPltGet
652 If M_20# = MContinue% Then GoTo *RE_PLT_GET_2
653 M_Out(12257) = 0            '本体チャック開OFF
654 M_Out(12256) = 1            '本体チャック閉ON
655 Dly 2.0
656 *CompPltGet_3
657 '
658 MRth = frInCheck(11264,1,MSETTIMEOUT05&)        '本体検出センサーON
659 If MRtn = 1 Then GoTo *CompPltGet_4
660 M_Out(12256) = 0            '本体チャック閉OFF
661 M_Out(12257) = 1            '本体チャック開ON
662 Dly 2.0
663 Mvs PProductOnPltGet_1
664 Mov PProductOnPltGet_2
665 fErrorProcess(11,252,284,0)
666 If M_20# = MNext% Then M_20# = MClear%
667 If M_20# = MAbout% Or M_20# = MNgProcess% Then
668     Mov PInitialPosition
669     Break
670 EndIf
671 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
672 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
673 Mov PProductOnPltGet_1
674 Mvs PProductOnPltGet
675 If M_20# = MContinue% Then GoTo *RE_PLT_GET_2
676 M_Out(12257) = 0            '本体チャック開OFF
677 M_Out(12256) = 1            '本体チャック閉ON
678 Dly 2.0
679 *CompPltGet_4
680     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
681 Mvs PProductOnPltGet_1      '本体上空
682     MRtn = FnCtlValue2(99)       '読書開始信号OFF  2022/04/28 渡辺
683 '
684 'Ovrd 100
685 Accel 50 , 50
686 MOverride = 2000 / M_OPovrd
687 If MOverride >100 Then MOverride = 100
688 Ovrd MOverride
689 '
690 Mov PProductOnPltGet_2      '本体回避点
691 '
692 '製品をねじロボ1に置く
693 Mov PProductOnRoboSet_2     'ねじロボ1回避点
694 'Wait M_In(11888) = 1        'ねじロボ1停止1受信
695 MScrewRoboNgFlg% = 0
696 Accel 100 , 100
697 MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
698 If MRtn = 0 Then MScrewRoboNgFlg% = 1
699 If MScrewRoboNgFlg% = 1 Then GoTo *ProductOnPltSet
700 Mov PProductOnRoboSet_1     'ねじロボ1上空
701 Ovrd 10
702 Mvs PProductOnRoboSet       '本体置き位置
703 Dly 0.2
704 *RE_ROBO_SET
705 If M_20# = MContinue% Then M_20# = MClear%
706 '
707 Dly 0.3
708 M_Out(12256) = 0            '本体チャック閉OFF
709 M_Out(12257) = 1            '本体チャック開ON
710 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)             '本体チャック開センサーON
711 If MRtn = 1 Then GoTo *CompRoboSet
712 fErrorProcess(11,244,284,0)
713 If M_20# = MNext% Then M_20# = MClear%
714 If M_20# = MAbout% Or M_20# = MNgProcess% Then
715     MScrewRoboNgFlg% = 1
716     Mvs PProductOnRoboSet_1
717     Mov PProductOnRoboSet_2
718     Break
719 EndIf
720 If M_20# = MAbout% Then GoTo *ProductOnPltSet    '念のため製品を置きに行く動作を行う
721 If M_20# = MNgProcess% Then GoTo *ProductOnPltSet    '念のため製品を置きに行く動作を行う
722 If M_20# = MContinue% Then GoTo *RE_ROBO_SET
723 *CompRoboSet
724 '
725 Mvs PProductOnRoboSet_1     'ねじロボ1上空
726 Ovrd 100
727 Mvs PProductOnRoboSet_2     'ねじロボ1回避点
728 '
729 '側板Lをパレットから取る
730 Mov PPlateLGet_2            '側板L取り回避点
731 M_Out(12866) = 1 Dly 0.5    'ねじロボ1動作再開(停止1～停止2)
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
749 Ovrd 25
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
778 Ovrd 10                     'オーバーライド変更(22/12/09中村)
779 Accel 10,10                 '加速度変更(22/12/09中村)
780 Mvs PPlateLGet_1            '側板L取り上空
781 If MRtn = 1 Then GoTo *CompPlateLGet_3
782 fErrorProcess(11,250,292,0) '284→292に変更(6/7中村)
783 If M_20# = MNext% Then M_20# = MClear%
784 If M_20# = MAbout% Or M_20# = MNgProcess% Then
785     Mov PPlateLGet_2
786     Mov PInitialPosition
787 EndIf
788 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
789 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
790 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_2
791 *CompPlateLGet_3
792 '
793 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)         '側板L検出
794 If MRtn = 1 Then GoTo *CompPlateLGet_4
795 fErrorProcess(11,254,292,0) '284→292に変更(6/7中村)
796 If M_20# = MNext% Then M_20# = MClear%
797 If M_20# = MAbout% Or M_20# = MNgProcess% Then
798     Mov PPlateLGet_2
799     Mov PInitialPosition
800 EndIf
801 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
802 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
803 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_2
804 *CompPlateLGet_4
805 'Dly 5                      'test(暫定コメントアウト)
806 'M_Out(12256) = 0            '本体チャック閉OFF(側板受け取り時邪魔になるため)以下3行暫定削除(11/17中村)
807 'M_Out(12257) = 1            '本体チャック開ON(側板受け取り時邪魔になるため)
808 'MRtn = frInCheck(11265,1,MSETTIMEOUT05&)         '本体チャック開センサーON
809 'If MRtn = 0 Then
810 '    fErrorProcess()         'エラー処理
811 'EndIf
812 Ovrd 50             'オーバーライド変更(22/12/09中村)
813 Mov PPlateLGet_2            '側板L取り回避点
814 '
815 '側板Lを置く
816 Mov PPlateLSet_2            '側板L置き回避点
817 Accel 100,100       '加速度変更(22/12/09中村)
818 Ovrd 100            'オーバーライド変更(22/12/09中村)
819 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)         'もう一度側板L検出
820 If MRtn = 1 Then GoTo *CompPlateLGet_5
821 fErrorProcess(11,254,292,0) '284→292に変更(6/7中村)
822 If M_20# = MNext% Then M_20# = MClear%
823 If M_20# = MAbout% Or M_20# = MNgProcess% Then
824     Mov PPlateLGet_2
825     Mov PInitialPosition
826 EndIf
827 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
828 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
829 If M_20# = MContinue% Then
830     Mov PPlateLGet_2
831     Mov PPlateLGet_1
832 EndIf
833 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_2
834 *CompPlateLGet_5
835 'Wait M_In(11889) = 1        'ねじロボ1停止2受信
836 MRtn = fScrewTighenRoboCheck(11889)    '停止状態を受信する
837 If MRtn = 0 Then Mov PInitialPosition
838 If MRtn = 0 Then GoTo *ASSY_ERROR_END
839 '
840 Fine 0.05 , P
841 '
842 Mov PPlateLSet_1            '側板L置き上空
843 Ovrd 10
844 Mvs PPlateLSet              '側板Lを置く位置
845 '
846 Fine 0 , P
847 '
848 M_Out(12866) = 1 Dly 0.5    'ねじロボ1動作再開(停止2～停止3)
849 'Wait M_In(11890) = 1        'ねじロボ1停止3受信
850 MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する
851 'If MRtn = 0 Then
852 '    Mvs PPlateLSet_1
853 '    Mov PPlateLSet_2
854 '    Mov PInitialPosition
855 'EndIf
856 If MRtn = 0 Then GoTo *ASSY_ERROR_END
857 M_Out(12262) = 0            '側板チャック閉OFF
858 M_Out(12263) = 1            '側板チャック開ON
859 Dly 0.5
860 Mvs PPlateLSet_1            '側板L置き上空
861 Ovrd 100
862 M_Out(12258) = 0            '側板Lシリンダー出OFF
863 M_Out(12259) = 1            '側板Lシリンダー戻ON
864 Mov PPlateLSet_2            '側板L置き回避点
865 '
866     ' 部品供給要求送信'12/20位置変更(中村)
867     M_Out(12787) = 1
868 '
869 *RE_CYLINDER_L_INI
870 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)        'L側シリンダー戻検出
871 If MRtn = 1 Then GoTo *CompCylinderLIni
872 fErrorProcess(11,247,284,0)
873 If M_20# = MNext% Then M_20# = MClear%
874 If M_20# = MAbout% Or M_20# = MNgProcess% Then
875     Mov PPlateLSet_3
876     Mov PInitialPosition
877     Break
878 EndIf
879 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
880 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
881 If M_20# = MContinue% Then
882     M_Out(12258) = 0            '側板Lシリンダー出OFF
883     M_Out(12259) = 1            '側板Lシリンダー戻ON
884 EndIf
885 If M_20# = MContinue% Then GoTo *RE_CYLINDER_L_INI
886 *CompCylinderLIni
887 '側板L置き位置画像検査
888 Mov PPlateLCheck_2          '側板L検査通過点
889 Mvs PPlateLCheck            '側板L検査位置
890 *RE_L_CHECK
891 PInspPosition(1) = PPlateLCheck
892 MInspGroup%(1) = 2
893 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,1,-1,1)
894 If MRtn = 1 Then GoTo *CompLCheck
895 fErrorProcess(11,43,3,0)
896 If M_20# = MNext% Then M_20# = MClear%
897 If M_20# = MAbout% Or M_20# = MNgProcess% Then
898     Mov PInitialPosition
899     Break
900 EndIf
901 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
902 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
903 If M_20# = MContinue% Then GoTo *RE_L_CHECK
904 *CompLCheck
905 '
906 M_Out(12866) = 1 Dly 0.5    'ねじロボ動作再開(停止3～停止4)
907 '
908 '側板Rを供給機から取る
909 '
910 ''    ' 部品供給要求送信'12/20位置変更(中村)
911 '    M_Out(12787) = 1
912     '    ' 部品供給完了待ち'12/21位置変更(中村)
913 '    Wait M_In(11810) = 1
914 '
915 'Mov PPlateRGet_4            '経路1
916 Mov PPlateRGet_3            '経路2
917 Mov PPlateRGet_2            '側板R取り回避点
918     '    ' 部品供給完了待ち(処理変更2/27中村)
919 *RE_FEEDER_READY
920     fnAutoScreenComment(513)    '状態表示[部品供給待ち] 2022/04/27 渡辺
921 '    Wait M_In(11810) = 1
922 MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '供給待ち
923 If MRtn = 1 Then GoTo *CompFeederReady
924 '   ' 部品供給要求終了
925 M_Out(12787) = 0
926 fErrorProcess(11,289,290,0) '284→290に変更(6/7中村)
927 If M_20# = MNext% Then M_20# = MClear%
928 If M_20# = MAbout% Or M_20# = MNgProcess% Then
929     Mov PBracketRGet_2
930     Mov PBracketRGet_3
931     Mov PBracketRSet_3
932     Mov PInitialPosition1
933 EndIf
934 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
935 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
936     ' 部品供給要求
937 M_Out(12787) = 1
938 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
939 *CompFeederReady
940 '    ' 部品供給要求終了
941     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/27 渡辺
942     M_Out(12787) = 0
943 '
944 *RE_PLATE_R_GET_1
945 '
946 If M_20# = MContinue% Then M_20# = MClear%
947 '
948 M_Out(12257) = 0            '本体チャック開OFF(以下3行,側板受け取り時邪魔になるため)
949 M_Out(12256) = 1            '本体チャック閉ON
950 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '本体チャック閉センサーON
951 If MRtn = 1 Then GoTo *CompPlateRGet_1
952 fErrorProcess(11,245,284,0)
953 If M_20# = MNext% Then M_20# = MClear%
954 If M_20# = MAbout% Or M_20# = MNgProcess% Then
955     Mov PPlateRGet_2
956     Mov PPlateRGet_3
957 '    Mov PPlateRGet_4
958     Mov PInitialPosition
959     Break
960 EndIf
961 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
962 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
963 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_1
964 *CompPlateRGet_1
965 '
966 *RE_PLATE_R_GET_2
967 If M_20# = MContinue% Then M_20# = MClear%
968 '
969 Mov PPlateRGet_1            '側板R取り上空
970 Ovrd 25
971 M_Out(12261) = 0            '側板Rシリンダー戻OFF
972 M_Out(12260) = 1            '側板Rシリンダー出ON
973 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)        '側板Rシリンダー出端検出センサーON
974 If MRtn = 1 Then GoTo *CompPlateRGet_2
975 fErrorProcess(11,248,284,0)
976 If M_20# = MNext% Then M_20# = MClear%
977 If M_20# = MAbout% Or M_20# = MNgProcess% Then
978     Mov PPlateRGet_2
979     Mov PPlateRGet_3
980 '    Mov PPlateRGet_4
981     Mov PInitialPosition
982     Break
983 EndIf
984 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
985 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
986 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_2
987 *CompPlateRGet_2
988 '
989 M_Out(12262) = 0            '側板チャック閉OFF
990 M_Out(12263) = 1            '側板チャック開ON
991 Fine 0.05 , P               '公差0.05[mm]以内
992 Mvs PPlateRGet              '側板Rを取る位置
993 Fine 0 , P                  'Fine解除
994 M_Out(12263) = 0            '側板チャック開OFF
995 M_Out(12262) = 1            '側板チャック閉ON
996 MRtn = frInCheck(11276,1,MSETTIMEOUT05&)        '側板Rチャック閉検出センサーON
997 Dly 0.5
998 Ovrd 10         'オーバーライド変更(22/12/09中村)
999 Accel 20,20     '加速度変更(22/12/09中村)
1000 Mvs PPlateRGet_1            '側板L取り上空
1001 If MRtn = 1 Then GoTo *CompPlateRGet_3
1002 fErrorProcess(11,250,292,0) '284→292に変更(6/7中村)
1003 If M_20# = MNext% Then M_20# = MClear%
1004 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1005     Mov PPlateLGet_2
1006     Mov PInitialPosition
1007 EndIf
1008 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1009 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1010 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_2
1011 *CompPlateRGet_3
1012 '
1013 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '側板R検出
1014 If MRtn = 1 Then GoTo *CompPlateRGet_4
1015 fErrorProcess(11,254,292,0) '284→292に変更(6/7中村)
1016 If M_20# = MNext% Then M_20# = MClear%
1017 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1018     Mov PPlateRGet_2
1019     Mov PPlateRGet_3
1020 '    Mov PPlateRGet_4
1021     Mov PInitialPosition
1022     Break
1023 EndIf
1024 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1025 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1026 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_2
1027 *CompPlateRGet_4
1028 '
1029 M_Out(12256) = 0            '本体チャック閉OFF(側板受け取り時邪魔になるため)
1030 M_Out(12257) = 1            '本体チャック開ON(側板受け取り時邪魔になるため)
1031 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)        '本体チャック開センサーON
1032 If MRtn = 1 Then GoTo *CompPlateRGet_5
1033 fErrorProcess(11,244,284,0)
1034 If M_20# = MNext% Then M_20# = MClear%
1035 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1036     Mov PPlateRGet_2
1037     Mov PPlateRGet_3
1038 '    Mov PPlateRGet_4
1039     Mov PInitialPosition
1040     Break
1041 EndIf
1042 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1043 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1044 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_1
1045 *CompPlateRGet_5
1046 '
1047 'Ovrd 100                   'コメントアウト2/15中村
1048 Accel 50 , 50               '加速度変更(22/12/09中村)
1049 Mov PPlateRGet_2            '側板R取り回避点
1050 Mov PPlateRGet_3            '経路2
1051 'Accel 100 , 100
1052 'Ovrd 100
1053 ''    ' 部品供給要求終了(処理位置変更2/11中村)
1054 '    M_Out(12787) = 0
1055 ''    ' 部品取得完了送信(パルス)
1056 '    M_Out(12800) = 1 Dly 0.5
1057 '    '
1058 'Mov PPlateRGet_4            '経路1
1059 '
1060 '側板Rを置く
1061 Mov PPlateRSet_3            '経路
1062 Mov PPlateRSet_2            '側板R置き回避点
1063 Accel 100 , 100             '加速度変更(22/12/09中村)
1064 Ovrd 100                    'オーバーライド変更(22/12/09中村)
1065 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        'もう一度側板R検出
1066 If MRtn = 1 Then GoTo *CompRGet
1067 fErrorProcess(11,254,292,0) '284→292に変更(6/7中村)
1068 If M_20# = MNext% Then M_20# = MClear%
1069 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1070     Mov PPlateRSet_3
1071     Mov PPlateRGet_3
1072 '    Mov PPlateRGet_4
1073     Mov PInitialPosition
1074     Break
1075 EndIf
1076 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1077 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1078 If M_20# = MContinue% Then
1079     Mov PPlateRSet_3
1080     Mov PPlateRGet_3
1081     Mov PPlateRGet_2
1082     Mov PPlateRGet_1
1083 EndIf
1084 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_1
1085 *CompRGet
1086 '    ' 部品供給要求終了
1087     M_Out(12787) = 0
1088 '    ' 部品取得完了送信(パルス)
1089     M_Out(12800) = 1 Dly 0.5
1090     '
1091 'Wait M_In(11891) = 1        'ねじロボ1停止4受信
1092 MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
1093 If MRtn = 0 Then Mov PInitialPosition
1094 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1095 '
1096 Fine 0.05 , P
1097 '
1098 Mov PPlateRSet_1            '側板R置き上空
1099 Ovrd 10
1100 Mvs PPlateRSet              '側板Rを置く位置
1101 '
1102 Fine 0 , P
1103 '
1104 M_Out(12866) = 1 Dly 0.5    'ねじロボ動作再開(停止4～停止5)
1105 'Wait M_In(11892) = 1        'ねじロボ1停止5受信
1106 MRtn = fScrewTighenRoboCheck(11892)    '停止状態を受信する
1107 'If MRtn = 0 Then
1108 '    Mvs PPlateRSet_1
1109 '    Mov PPlateRSet_2
1110 '    Mov PInitialPosition
1111 'EndIf
1112 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1113 M_Out(12262) = 0            '側板チャック閉OFF
1114 M_Out(12263) = 1            '側板チャック開ON
1115 Dly 0.5
1116 Mvs PPlateRSet_1            '側板L置き上空
1117 Ovrd 100
1118 M_Out(12260) = 0            '側板Rシリンダー出OFF
1119 M_Out(12261) = 1            '側板Rシリンダー戻ON
1120 Mov PPlateRSet_2            '側板R置き回避点
1121 '
1122 '
1123 ''側板R置き位置画像検査
1124 Mov PPlateRCheck_2          '側板R検査通過点
1125 Mvs PPlateRCheck            '側板R検査位置
1126 *RE_R_CHECK
1127 If M_20# = MContinue% Then M_20# = MClear%
1128 PInspPosition(1) = PPlateRCheck
1129 MInspGroup%(1) = 3
1130 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,1,-1,1)
1131 If MRtn = 1 Then GoTo *CompCheckR
1132 fErrorProcess(11,43,3,0)
1133 If M_20# = MNext% Then M_20# = MClear%
1134 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1135     Mvs PPlateRCheck_2
1136     Mov PInitialPosition
1137 EndIf
1138 If M_20# = MAbout% Or M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1139 If M_20# = MContinue% Then GoTo *RE_R_CHECK
1140 *CompCheckR
1141 'If MRtn <> 1 Then
1142 '   'エラー処理
1143 'EndIf
1144 M_Out(12866) = 1 Dly 0.5    'ねじロボ動作再開(停止5～停止6)
1145 '
1146 'ねじロボ1の製品を取る
1147 Mov PProductOnRoboGet_2     'ねじロボ1回避点
1148 '
1149 *RE_CYLINDER_R_INI
1150 MRtn = frInCheck(11273,1,MSETTIMEOUT05&)        'R側シリンダー戻検出
1151 If MRtn = 1 Then GoTo *CompCylinderRIni
1152 fErrorProcess(11,249,284,0)
1153 If M_20# = MNext% Then M_20# = MClear%
1154 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1155     Mov PPlateRSet_3
1156     Mov PPlateRGet_3
1157 '    Mov PPlateRGet_4
1158     Mov PInitialPosition
1159     Break
1160 EndIf
1161 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1162 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1163 If M_20# = MContinue% Then
1164     M_Out(12260) = 0            '側板Rシリンダー出OFF
1165     M_Out(12261) = 1            '側板Rシリンダー戻ON
1166 EndIf
1167 If M_20# = MContinue% Then GoTo *RE_CYLINDER_R_INI
1168 *CompCylinderRIni
1169 '
1170 'Wait M_In(11893) = 1        'ねじロボ1停止6受信
1171 'MRtn = frInCheck(11876,1,MSETTIMEOUT05&)   'ねじロボ1完了を受信
1172 MRtn = fScrewTighenRoboCheck(11893)    '停止状態を受信する
1173 If MRtn = 0 Then Mov PInitialPosition
1174 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1175 'Mvs PProductOnRoboGet_1     'ねじロボ1上空(2022/1/11移動タイミング変更(中村))
1176 'Ovrd 25
1177 '
1178 *RE_ROBO_GET
1179 '
1180 If M_20# = MContinue% Then M_20# = MClear%
1181 '
1182 M_Out(12256) = 0            '本体チャック閉OFF
1183 M_Out(12257) = 1            '本体チャック開ON
1184 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)        '本体チャック開センサーON
1185 If MRtn = 1 Then GoTo *CompRoboGet_1
1186 Mov PInitialPosition
1187 fErrorProcess(11,244,284,0)
1188 If M_20# = MNext% Then M_20# = MClear%
1189 'If M_20# = MAbout% Or M_20# = MNgProcess% Then
1190 '    Mvs PProductOnRoboGet_2
1191 '    Mov PInitialPosition
1192 '    Break
1193 'EndIf
1194 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1195 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1196 Mov PProductOnRoboGet_2
1197 If M_20# = MContinue% Then GoTo *RE_ROBO_GET
1198 *CompRoboGet_1
1199 '
1200 Mvs PProductOnRoboGet_1     'ねじロボ1上空(2022/1/11移動タイミング変更(中村))
1201 Ovrd 25
1202 '
1203 Mvs PProductOnRoboGet       '本体を取る位置
1204 'M_Out(12866) = 1 Dly 0.5    'ねじロボ動作再開(停止6～完了)位置変更(12/24中村)
1205 'Wait M_In(11876) = 1        'ねじロボ1完了を受信
1206 'MRtn = fScrewTighenRoboCheck(11876)    '停止状態を受信する
1207 'If MRtn = 0 Then
1208 '    Mvs PProductOnRoboGet_1
1209 '    Mov PProductOnRoboGet_2
1210 '    Mov PInitialPosition
1211 'EndIf
1212 'If MRtn = 0 Then GoTo *ASSY_ERROR_END
1213 '
1214 M_Out(12257) = 0            '本体チャック開OFF
1215 M_Out(12256) = 1            '本体チャック閉ON
1216 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '本体チャック閉センサーON
1217 If MRtn = 1 Then GoTo *CompRoboGet_2
1218 M_Out(12256) = 0            '本体チャック閉OFF
1219 M_Out(12257) = 1            '本体チャック開ON
1220 Dly 2.0
1221 Mvs PProductOnRoboGet_1
1222 Mvs PProductOnRoboGet_2
1223 Mov PInitialPosition
1224 M_Out(12257) = 0            '本体チャック開OFF
1225 M_Out(12256) = 1            '本体チャック閉ON
1226 fErrorProcess(11,245,284,0)
1227 If M_20# = MNext% Then M_20# = MClear%
1228 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1229 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1230 M_Out(12256) = 0            '本体チャック閉OFF
1231 M_Out(12257) = 1            '本体チャック開ON
1232 Dly 2.0
1233 Mov PProductOnRoboGet_2
1234 If M_20# = MContinue% Then GoTo *RE_ROBO_GET
1235 Mvs PProductOnRoboGet_1
1236 Mvs PProductOnRoboGet
1237 M_Out(12257) = 0            '本体チャック開OFF
1238 M_Out(12256) = 1            '本体チャック閉ON
1239 Dly 2.0
1240 *CompRoboGet_2
1241 '
1242 MRtn = frInCheck(11264,1,MSETTIMEOUT05&)        '本体検出センサーON
1243 'Mvs PProductOnRoboGet_1     'ねじロボ1上空
1244 If MRtn = 1 Then GoTo *CompRoboGet_3
1245 M_Out(12256) = 0            '本体チャック閉OFF
1246 M_Out(12257) = 1            '本体チャック開ON
1247 Dly 2.0
1248 Mvs PProductOnRoboGet_1
1249 Mvs PProductOnRoboGet_2
1250 Mov PInitialPosition
1251 fErrorProcess(11,252,284,0)
1252 If M_20# = MNext% Then M_20# = MClear%
1253 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1254 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1255 Mov PProductOnRoboGet_2
1256 If M_20# = MContinue% Then GoTo *RE_ROBO_GET
1257 Mvs PProductOnRoboGet_1
1258 Mvs PProductOnRoboGet
1259 M_Out(12257) = 0            '本体チャック開OFF
1260 M_Out(12256) = 1            '本体チャック閉ON
1261 Dly 2.0
1262 *CompRoboGet_3
1263 '
1264 M_Out(12866) = 1 Dly 0.5    'ねじロボ動作再開(停止6～完了)
1265 MRtn = fScrewTighenRoboCheck(11876)    '停止状態を受信する
1266 'Dly 5'デバッグ
1267 If MRtn = 0 Then
1268     M_Out(12256) = 0
1269     M_Out(12257) = 1
1270     Wait M_In(11265) = 1
1271     Mvs PProductOnRoboGet_1
1272     Mvs PProductOnRoboGet_2
1273     Mov PInitialPosition
1274 EndIf
1275 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1276 '
1277 Mvs PProductOnRoboGet_1     'ねじロボ1上空
1278 MRtn = frInCheck(11264,1,MSETTIMEOUT05&)        '本体検出センサーON
1279 If MRtn = 1 Then GoTo *CompRoboGet_4
1280 MOverride = 2000 / M_OPovrd
1281 If MOverride >100 Then MOverride = 100
1282 Ovrd MOverride
1283 Mov PProductOnRoboGet_2
1284 Mov PInitialPosition
1285 fErrorProcess(11,252,284,0)             'MContinue%もMClear%
1286 If M_20# = MNext% Then M_20# = MClear%
1287 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1288 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1289 If M_20# = MContinue% Then M_20# = MClear%
1290 GoTo *ProductOnPltSet
1291 *CompRoboGet_4
1292 '
1293 'Ovrd 100
1294 Accel 50 , 50
1295 MOverride = 2000 / M_OPovrd
1296 If MOverride >100 Then MOverride = 100
1297 Ovrd MOverride
1298 'Mvs PProductOnRoboGet_1     'ねじロボ1上空
1299 Mov PProductOnRoboGet_2     'ねじロボ1回避点
1300 '
1301 *ProductOnPltSet
1302 'パレットに製品を置く
1303 Mov PProductOnPltSet_2     'パレット回避点
1304 Accel 100 , 100
1305 Mov PProductOnPltSet_1     'パレット上空
1306 Ovrd 10
1307 Mvs PProductOnPltSet       '本体置き位置
1308 Dly 0.2
1309 '
1310 *RE_PLT_SET
1311 '
1312 If MScrewRoboNgFlg% = 1 And M_20# = MContinue% Then M_20# = MRtn
1313 If M_20# = MContinue% Then M_20# = MClear%
1314 '
1315 M_Out(12256) = 0            '本体チャック閉OFF
1316 M_Out(12257) = 1            '本体チャック開ON
1317 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    '本体チャック開センサーON
1318 If MRtn = 1 Then GoTo *CompPltSet_1
1319 If MScrewRoboNgFlg% = 1 Then
1320     MRtn = M_20#
1321     M_20# = MClear%
1322 EndIf
1323 fErrorProcess(11,244,284,0)
1324 If M_20# = MNext% Then M_20# = MClear%
1325 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1326 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1327 If M_20# = MContinue% Then GoTo *RE_PLT_SET
1328 *CompPltSet_1
1329 If MScrewRoboNgFlg% = 1 Then M_20# = MRtn
1330 '
1331 Mvs PProductOnPltSet_1     'パレット上空
1332 Ovrd 100
1333 Mvs PProductOnPltSet_2     'パレット回避点
1334 '
1335 'Mov PInitialPosition        '暫定削除(11/17中村)
1336 '
1337     MRtn = FnCtlValue2(2)          '組立ＯＫ＋１  2022/04/28 渡辺
1338 Mov PTicketRead_1
1339     MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1340 If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
1341 M_Out(12868) = 1 Dly 0.5    'ねじロボ1ねじ締め完了を送信
1342 '
1343 M_20# = MAssyOK%            'Assy正常終了
1344 '
1345 GoTo *AssyEnd
1346 '
1347 *ASSY_ERROR_END
1348     M_Out(12264) = 0            '位置決め出OFF
1349     M_Out(12265) = 1            '位置決め戻ON
1350 *AssyEnd
1351 *fnAssyStart_FEndPosi
1352     Exit Function
1353 FEnd
1354 '
1355 '■fnPiasCheck
1356 ''' <summary>
1357 ''' PIASチケット読込み
1358 ''' </summary>
1359 ''' <returns>   0 : NG
1360 '''             1 : OK(読込み完了)
1361 ''' </returns>
1362 ''' <remarks>
1363 ''' Date   : 2021/07/07 : M.Hayakawa
1364 ''' </remarks>'
1365 Function M% fnPiasCheck
1366     fnPiasCheck = 0
1367     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1368     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1369 '
1370 *RETRY_PIAS
1371     M_20# = MClear%
1372     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1373     '
1374     '【IDチケット読み込み】
1375     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1376     MInspGroup%(1) = 1              '検査G番号
1377     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1378 '
1379     'エラーの場合
1380     If MRtn <> 1 Then
1381         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1382         If MRtn <> 1 Then
1383             'D720 -> D1300 コピー要求
1384             M_Out(12565) = 1
1385             Dly 0.5
1386             M_Out(12565) = 0
1387             'エラー処理記述
1388             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1389             'GOT KEY入力待ち
1390             MKeyNumber = fnKEY_WAIT()
1391             '
1392             Select MKeyNumber
1393                 Case MNext%         '次へを選択した場合
1394                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1395                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1396                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1397                     Break
1398                 Case MAbout%        '停止を選択した場合
1399                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1400                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1401                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1402                     Break
1403                 Case MNgProcess%    'NGを選択した場合
1404                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1405                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1406                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1407                     Break
1408                 Case MContinue%     '継続を選択した場合
1409                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1410                     M_20# = MContinue%
1411                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1412                     Break
1413             End Select
1414         EndIf
1415     EndIf
1416 '----------D720 -> D1300 コピー要求----------
1417     M_Out(12565) = 1
1418     Dly 0.5
1419     M_Out(12565) = 0
1420 '----------通信確認をする----------
1421     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1422     MRtn = 0                ' 初期化
1423     M_20# = MClear%         ' 初期化
1424     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1425     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1426     If MRtn <> 1 Then
1427         If M_20# = MContinue% Then
1428             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1429         Else
1430             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1431         EndIf
1432     EndIf
1433 '----------工程抜け確認----------
1434     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1435     MRtn = 0                ' 初期化
1436     M_20# = MClear%         ' 初期化
1437     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1438     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1439     If MRtn <> 1 Then
1440         If M_20# = MContinue% Then
1441             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1442         Else
1443             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1444         EndIf
1445     EndIf
1446     '
1447     fnPiasCheck = 1
1448     *fnPiasCheck_End
1449     Exit Function
1450 FEnd
1451 '
1452 '■fnPCComuCheck
1453 ''' <summary>
1454 ''' PC-PLC通信チェック
1455 ''' </summary>
1456 ''' <returns>   0 : NG
1457 '''             1 : OK(読込み完了)
1458 ''' </returns>
1459 ''' <remarks>
1460 ''' Date   : 2021/07/07 : M.Hayakawa
1461 ''' </remarks>'
1462 Function M% fnPCComuCheck
1463     fnPCComuCheck = 0
1464     MJudge% = 0                                  '初期化
1465     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1466     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1467     '
1468     For MStaNo = 0 To 5
1469         '
1470         If M_In(MIN_PIAS_ComOK%) = 1 Then
1471             'PC通信OK(M400)
1472             MJudge% = MOK%
1473             MStaNo = 5
1474             Break
1475         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1476             'toRBT_通信確認time out
1477             MJudge% = MNG%
1478             MCommentD1001 = 15
1479             MCommentD1002 = 21
1480             MStaNo = 5
1481             Break
1482         Else
1483             'toRBT_通信確認time out
1484             MJudge% = MNG%
1485             MCommentD1001 = 14
1486             MCommentD1002 = 21
1487             Break
1488         EndIf
1489     Next MStaNo
1490     '
1491     '上記で返信フラグを受信してからPC通信確認OFF
1492     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1493     '
1494     'エラー画面
1495     If MJudge% <> MOK% Then
1496         M_20# = MClear%     '初期化
1497         'エラー処理記述
1498         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1499         'GOT KEY入力待ち
1500         MKeyNumber = fnKEY_WAIT()
1501         '
1502         If MKeyNumber = MAbout% Then            '停止を選択した場合
1503             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1504             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1505             Break
1506         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1507             M_20# = MPass%                      'M_20# プログラム間共通外部変数
1508             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1509             Break
1510         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1511             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1512             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1513             Break
1514         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1515             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1516             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1517             Break
1518         EndIf
1519     Else
1520         'OKの場合
1521         fnPCComuCheck = 1
1522     EndIf
1523     Exit Function
1524 FEnd
1525 '
1526 '■fnProcessCheck
1527 ''' <summary>
1528 ''' 工程抜け確認
1529 ''' </summary>
1530 ''' <returns>    1：工程履歴OK     0：異常終了
1531 '''             -1：前工程履歴NG  -2：自工程履歴あり
1532 '''             -3：モデル仕向NG  -4：タイムアウト
1533 '''             -5：履歴処理エラー
1534 ''' </returns>
1535 ''' <remarks>
1536 ''' Date   : 2021/07/07 : M.Hayakawa
1537 ''' </remarks>'
1538 Function M% fnProcessCheck
1539     fnProcessCheck = 0
1540     MJudge% = MNG%      '一旦NGを初期化とする
1541 '----------工程抜け確認----------
1542     MCommentD1001 = 0   'コメント初期化
1543     For MStaNo = 0 To 5
1544         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1545         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1546         '
1547         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1548             MJudge% = MOK%
1549             fnAutoScreenComment(85)     ' AUTO画面
1550             MStaNo = 5
1551             Break
1552         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1553             MFlgLoop% = 0
1554             MJudge% = MNG%
1555             MCommentD1001 = 27
1556             MCommentD1002 = 22
1557             fnAutoScreenComment(94)     ' AUTO画面
1558             fnProcessCheck = -2         ' NGは-2を返す
1559             MStaNo = 5
1560             Break
1561         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1562            MJudge% = MNG%
1563             MCommentD1001 = 31
1564             MCommentD1002 = 22
1565             fnAutoScreenComment(83)     ' AUTO画面
1566             fnProcessCheck = -3         ' NGは-3を返す
1567             MStaNo = 5
1568             Break
1569         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1570             '履歴NGは直ぐに終了せず繰り返し確認を行う
1571             '前工程の書込みが終了していない可能性があるため
1572             MJudge% = MNG%
1573             MCommentD1001 = 32
1574             MCommentD1002 = 22
1575             fnAutoScreenComment(84)     ' AUTO画面
1576             fnProcessCheck = -1         ' NGは-1を返す
1577             Dly 1.0
1578             '工程抜け確認OFF
1579             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1580             Dly 1.0
1581            'MStaNo = 5
1582             Break
1583         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1584             MFlgLoop% = 0
1585             MJudge% = MNG%
1586             MCommentD1001 = 29
1587             MCommentD1002 = 22
1588             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1589             fnProcessCheck = -5         ' NGは-5を返す
1590             MStaNo = 5
1591             Break
1592         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1593             MJudge% = MNG%
1594             If MCommentD1001 = 32 Then
1595                 '何もしない
1596             Else
1597                 MCommentD1001 = 26
1598             EndIf
1599             MCommentD1002 = 22
1600             fnProcessCheck = -4         ' NGは-4を返す
1601             MStaNo = 5
1602             Break
1603         Else
1604             MJudge% = MNG%
1605             MCommentD1001 = 28
1606             MCommentD1002 = 22
1607         EndIf
1608     Next MStaNo
1609     '工程抜け確認OFF
1610     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1611     '通過履歴NG 工程抜けの場合
1612     If MJudge% = MPass% Then
1613         M_20# = MPass%
1614     EndIf
1615     '
1616     'エラー画面
1617     If MJudge% <> MOK% Then
1618         M_20# = MClear%     '初期化
1619         'エラー処理記述
1620         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1621         'GOT KEY入力待ち
1622         MKeyNumber = fnKEY_WAIT()
1623         '
1624         Select MKeyNumber
1625             Case MAbout%        '停止を選択した場合
1626                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1627                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1628                 Break
1629             Case MNext%         '次へを選択した場合
1630                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1631                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1632                 Break
1633             Case MContinue%     '継続を選択した場合
1634                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1635                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1636                 Break
1637             Case MNgProcess%    'NGを選択した場合
1638                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1639                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1640                 Break
1641         End Select
1642     Else
1643         fnProcessCheck = 1  ' OKは1を返す
1644     EndIf
1645     Exit Function
1646 FEnd
1647 '
1648 '■fnPiasWrite
1649 ''' <summary>
1650 ''' Pias 組立結果書込み要求
1651 ''' </summary>
1652 '''<param name="MFlg%">
1653 '''                 MOK%(1) = 工程履歴にOKを書込む
1654 '''                 MNG%(0) = 工程履歴にNGを書込む
1655 '''</param>
1656 '''<returns></returns>
1657 ''' <remarks>
1658 ''' Date   : 2021/07/07 : M.Hayakawa
1659 ''' </remarks>'
1660 Function M% fnPiasWrite(ByVal MFlg%)
1661       fnPiasWrite = 0
1662 *RETRY_PIASWRITE
1663     '
1664     '組立OK(MOK%)の場合　M306 ON
1665    '組立NG(MNG%)の場合　M307 ON
1666     If MFlg% = MOK% Then
1667         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1668     Else
1669         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1670     EndIf
1671     Dly 0.1                  '念のため
1672     '
1673     'Piasへ書込み開始 M305 -> ON
1674     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1675     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1676     '
1677     MJudge% = MNG%
1678     '
1679     For MStaNo = 0 To 5
1680         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1681             MJudge% = MOK%
1682             'MRet = fnAutoScreenComment(85)  'AUTO画面
1683             MStaNo = 5
1684             Break
1685         '
1686         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1687             MJudge% = MNG%
1688             'MRet = fnAutoScreenComment(85)  'AUTO画面
1689            MCommentD1001 = 34
1690            MCommentD1002 = 25
1691             MStaNo = 5
1692             Break
1693         '
1694         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1695             MJudge% = MNG%
1696             'MRet = fnAutoScreenComment(85)  'AUTO画面
1697            MCommentD1001 = 35
1698            MCommentD1002 = 25
1699             MStaNo = 5
1700             Break
1701         '
1702         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1703             MJudge% = MNG%
1704             'MRet = fnAutoScreenComment(85)  'AUTO画面
1705            MCommentD1001 = 36
1706            MCommentD1002 = 25
1707             MStaNo = 5
1708             Break
1709         '
1710         Else
1711             MJudge% = MNG%
1712            MCommentD1001 = 42
1713            MCommentD1002 = 25
1714         '
1715         EndIf
1716         '
1717     Next MStaNo
1718     '
1719     'Piasへ書込み開始 M305 -> OfF
1720     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1721     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1722     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1723     '
1724     '
1725     '通過履歴NG 工程抜けの場合
1726     If MJudge% = MPass% Then
1727         M_20# = MPass%
1728     EndIf
1729     '
1730    M_20# = MClear%     '初期化
1731     '
1732     'エラー画面
1733     If MJudge% < MOK% Then
1734     '
1735 '残しておくが現状では使用しないラベル
1736 *RETRY_ERR_WRITE
1737         M_20# = MClear%     '初期化
1738         'エラー処理記述
1739         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1740         'GOT KEY入力待ち
1741         MKeyNumber = fnKEY_WAIT()
1742         '
1743         If MKeyNumber = MAbout% Then   '停止を選択した場合
1744             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1745            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1746             Break
1747         '
1748         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1749             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1750             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1751         '
1752         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1753             M_20# = MPass%            'M_20# プログラム間共通外部変数
1754             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1755         '
1756         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1757             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1758            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1759             Break
1760         '
1761         EndIf
1762         '
1763         If M_20# = MClear% Then *RETRY_ERR_WRITE
1764         '
1765     EndIf
1766     '
1767     If M_20# = MContinue% Then *RETRY_PIASWRITE
1768     '
1769     fnPiasWrite = 1
1770     Exit Function
1771 FEnd
1772 '
1773 '■fnPCBNumberCheck
1774 ''' <summary>
1775 ''' Pias 基板番号照合要求
1776 ''' </summary>
1777 '''<param name="%"></param>
1778 '''<param name="%"></param>
1779 '''<returns></returns>
1780 ''' <remarks>
1781 ''' Date   : 2021/07/07 : M.Hayakawa
1782 ''' </remarks>'
1783 Function M% fnPCBNumberCheck
1784       fnPCBNumberCheck = 0
1785     '
1786 *RETRY_PCBCHECK
1787     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1788     'Piasへ基板照合開始 M310 -> ON
1789     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1790     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1791     '
1792     MJudge% = MNG%
1793     '
1794     For MStaNo = 0 To 5
1795         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1796             MJudge% = MOK%
1797             fnAutoScreenComment(96)  'AUTO画面
1798             MStaNo = 5
1799             Break
1800         '
1801         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1802             MJudge% = MNG%
1803             fnAutoScreenComment(97)  'AUTO画面
1804             MCommentD1001 = 37
1805             MCommentD1002 = 25
1806             MStaNo = 5
1807             Break
1808         '
1809         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1810             MJudge% = MNG%
1811             fnAutoScreenComment(98)  'AUTO画面
1812             MCommentD1001 = 38
1813             MCommentD1002 = 25
1814             MStaNo = 5
1815             Break
1816         '
1817         ElseIf M_In(11580) = 1 Then                         'time out
1818             MJudge% = MNG%
1819             fnAutoScreenComment(99)  'AUTO画面
1820             MCommentD1001 = 39
1821             MCommentD1002 = 25
1822             MStaNo = 5
1823             Break
1824         '
1825         Else
1826             MJudge% = MNG%
1827            MCommentD1001 = 41
1828            MCommentD1002 = 25
1829         '
1830         EndIf
1831         '
1832     Next MStaNo
1833     '
1834     'Piasへ基板照合開始 M310 -> OfF
1835     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1836     '
1837     '
1838     '通過履歴NG 工程抜けの場合
1839     If MJudge% = MPass% Then
1840         M_20# = MPass%
1841     EndIf
1842     '
1843    M_20# = MClear%     '初期化
1844     '
1845     'エラー画面
1846     If MJudge% < MOK% Then
1847     '
1848 '残しておくが現状では使用しないラベル
1849 *RETRY_ERR_PCBNUMBER
1850         M_20# = MClear%     '初期化
1851         'エラー処理記述
1852         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1853         'GOT KEY入力待ち
1854         MKeyNumber = fnKEY_WAIT()
1855         '
1856         If MKeyNumber = MAbout% Then   '停止を選択した場合
1857             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1858             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1859             Break
1860         '
1861         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1862             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1863             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1864         '
1865         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1866             M_20# = MNext%            'M_20# プログラム間共通外部変数'MPass%→MNext%へ変更(12/7中村)
1867             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1868         '
1869         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1870             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1871             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1872             Break
1873         '
1874         EndIf
1875         '
1876         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1877         '
1878     EndIf
1879     '
1880     If M_20# = MContinue% Then *RETRY_PCBCHECK
1881     Exit Function
1882 FEnd
1883 '
1884 '■ScrewTight_S2
1885 ''' <summary>
1886 ''' ねじ締めを行う
1887 ''' </summary>
1888 '''<param name="PScrewPos()">
1889 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
1890 '''             PScrewPos(2)    ：ねじ締め回避点
1891 '''             PScrewPos(10)   ：ねじ締め終了高さ
1892 '''</param>
1893 '''<returns>整数
1894 '''         0=異常終了、1=正常終了
1895 '''</returns>
1896 ''' <remarks>
1897 ''' Date   : 2021/07/07 : M.Hayakawa
1898 ''' </remarks>'
1899 Function M% ScrewTight_S2(ByVal PScrewPosition())   'ネジ締め個別設定
1900     ScrewTight_S2 = 0
1901     MOKNGFlg = 0
1902     Ovrd 100
1903     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1904     ' 暫定
1905     Ovrd 5
1906     Mvs PScrewPosition(10),-10    ' パレット上ねじ締めS①の上空へ移動
1907 '    Ovrd MOvrdA
1908     '暫定マスク
1909 '    M_Out(Y62_Driver)=1     ' バンクセッティング　C1
1910 '    Dly 0.1
1911 '    M_Out(Y61_Driver)=1     'ドライバーON　CW
1912 '    'Spd 8.3 '外部ライド100  内部ライド60   'ライド100-40　100%：Spd　15　'ねじ締め速度設定
1913 '    Spd MSpdA               'ネジ締め時Spd個別設定
1914     ' 暫定移動のみ
1915     Mvs PScrewPosition(10)
1916 '    '
1917 '    Dly 0.1
1918 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1919 '    Wait M_In(11584)=1          '完了/エラー検出
1920 '    Dly 0.1
1921 '    Spd M_NSpd
1922 '    '
1923 '    If M_In(X28_Driver)=1 Then  'ねじトータルエラー検出時
1924 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1925 '        Dly 0.1
1926 '        M_Out(Y62_Driver)=0     'バンクセッティング解除　C1
1927 '        Dly 0.1
1928 '        M_Out(Y63_Driver)=0     'バンクセッティング解除　C1
1929 '        Dly 0.1
1930 '        M_Out(Y65_Driver)=0     'プログラム解除　F1
1931 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1932 '        M_Out(Y68_VV1)=0        'ねじ吸着　OFF
1933 '        MOKNGFlg = -1
1934 '        ScrewTight_S2 = 0
1935 '    Else
1936 '        Wait M_In(X29_Driver)=1 ' 正常完了時
1937 '        Dly 0.1
1938 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1939 '        Dly 0.1
1940 '        M_Out(Y62_Driver)=0     'バンクセッティング解除
1941 '        Dly 0.1
1942         M_Out(12249)=1 Dly 0.3         'ねじ吸着　OFF (一時コメントアウト解除,(Y68_VV1)=0を(12249)=1 Dly 0.3に変更(8/5中村))
1943         M_Out(12250)=1 Dly 0.1         '暫定真空破壊
1944 '        Dly 0.1
1945 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
1946 '        ScrewTight_S2 = 1
1947 '    EndIf
1948 ' 暫定
1949     Ovrd 10
1950     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
1951     Ovrd 100
1952     Exit Function
1953 FEnd
1954 '
1955 '■ScrewGet_S3
1956 ''' <summary>
1957 ''' ねじ供給機からねじを得る
1958 ''' </summary>
1959 '''<param name="%"></param>
1960 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1961 '''         PScrewPos(2)    ：ねじ供給器回避点
1962 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1963 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1964 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1965 '''<returns>整数
1966 '''         0=異常終了、1=正常終了、-1=MネジセンサーNG、-2=MネジセンサーON、-3=吸着エラー
1967 '''</returns>
1968 ''' <remarks>
1969 ''' Date   : 2021/07/07 : M.Hayakawa
1970 ''' </remarks>'
1971 Function M% ScrewGet_S3(ByVal PScrewPosition())
1972     ScrewGet_S3 = 0
1973     MMScrewJudge% = 0
1974     'ねじ供給器初期動作エラーチェック
1975 ' ↓暫定削除
1976 '    Wait M_In(X34_ScrewReady1)=1 'ねじ供給器SがReadyになるまで待つ　←　何秒か待ってReadyにならなければ抜けるプログラムが必要？
1977 '    Ovrd 100
1978 '    If M_In(X33_SS2)=0 Then  'Mねじ検出センサがOFF（故障）していた場合
1979 '        Ovrd 30
1980 '        Mvs,-80             'その場所から80mm上空へ移動
1981 '        Mov PInitPos19049   '19049初期位置へ移動
1982 '        M_Out(Y68_VV1)=0    'ねじ吸着 Off
1983 '        'NGとしてここの関数から抜ける
1984 '        ScrewGet_S3 = -1
1985 '        MMScrewJudge% = 1
1986 '        MCommentD1001 = 61
1987 '    EndIf
1988 '    If ScrewGet_S3 = 0 Then
1989 '        'Sタイト用ねじ供給機にMねじが混入していないか監視
1990 '        MMScrewJudge% = 0 'MMScrewJudgeを初期化する
1991 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
1992 '        If MRtn = 0 Then
1993 '            Ovrd 30
1994 '            Mvs,-80            'その場所から50mm上空へ移動
1995 '            Mov PInitPos19049  '19049初期位置へ移動
1996 '            MMScrewJudge% = 2
1997 '            MRtn = All_CLamp_Release()'全てのクランプ解除へ分岐
1998 '            MCnt% = 2   '2を設定
1999 '            MCommentD1001 = 62
2000 '        EndIf
2001 '        If MMScrewJudge% = 2 Then
2002 '            ScrewGet_S3 = -2
2003 '        EndIf
2004 '    EndIf
2005 '    'Mネジ判定がONの場合 NGとして関数を抜ける
2006 '    If MMScrewJudge% = 2 Then
2007 '        ScrewGet_S3 = -2
2008 '    EndIf
2009     'Sネジ用ねじ太郎のMネジ混入確認用ここまで
2010     Ovrd 100
2011     Spd M_NSpd
2012     If MMScrewJudge% = 0 Then
2013         ScrewGet_S3 = 0
2014         M_Out(Y63_Driver)=1         ' バンクセッティング　C2
2015         MScrewCnt% = 0
2016         MFinCnt% = 2
2017 '        For MCnt% = 0 To MFinCnt%
2018             Mov PScrewPosition(2)        ' ねじ供給機回避点
2019             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
2020             Ovrd 80
2021             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2022             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
2023             Mvs PScrewPosition(10), 1.2
2024             M_Out(Y68_VV1)=1 Dly 0.3        ' ねじ吸着　ON
2025             'ビット回転
2026             M_Out(Y60_Driver)=1
2027             Dly 0.2
2028             '
2029             Ovrd 100
2030             JOvrd M_NJovrd
2031             Spd M_NSpd
2032             'ネジ吸着確認位置移動
2033             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
2034             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
2035             'ビット回転停止
2036             'M_Out(Y60_Driver)=0
2037             '
2038             '1秒間ネジ吸着確認
2039 ' 以下暫定削除
2040 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2041 '            'MRtn = 0'強制エラー
2042 '            '吸着エラーの場合
2043 '            'ネジをねじ太郎に戻す
2044 '            If MRtn = 0 Then
2045 '                Ovrd 30
2046 '                'ビット回転停止
2047 '                M_Out(Y60_Driver)=0
2048 '                'ネジ供給機上空
2049 '                Mvs PScrewPos(1)
2050 '                '更に上空
2051 '                Mov PScrewPos(1), -75
2052 '                'ネジ捨て位置
2053 '                Mov PScrewFeedS021
2054 '                '吸着OFF
2055 '                M_Out(Y68_VV1)=0 'ねじ吸着　OFF
2056 '                Dly 0.2
2057 '                '破壊ON
2058 '                M_Out(Y6B_VB1)=1 '真空破壊ON
2059 '                'ビット回転
2060 '                M_Out(Y61_Driver)=1
2061 '                Dly 0.5
2062 '                '
2063 '                Ovrd 100
2064 '                JOvrd M_NJovrd
2065 '                Spd M_NSpd
2066 '                'ドライバーを上下させねじを振り落とす
2067 '                Mov PScrewFeedS021, 10
2068 '                Mov PScrewFeedS021
2069 '                Dly 0.1
2070 '                Mov PScrewFeedS021, 10
2071 '                Mov PScrewFeedS021
2072 '                '
2073 '                'ネジ落ち待ち
2074 '                'ビット回転停止
2075 '                M_Out(Y61_Driver)=0
2076 '                Dly 0.1
2077 '                '破壊OFF
2078 '                M_Out(Y6B_VB1)=0 '真空破壊OFF
2079 '                '
2080 '                '
2081 '                'ねじ落ちたとして、移動更に上空
2082 '                Mov PScrewPos(1), -75
2083 '                Ovrd 100
2084 '                Spd M_NSpd
2085 '                'ネジ供給機上空
2086 '                Mvs PScrewPos(1)
2087 '                '
2088 '                ScrewGet_S3 = -3
2089 '                Break
2090 '                '
2091 '            Else
2092 '                MCnt% = MFinCnt%
2093 '                ScrewGet_S3 = 0
2094 '            EndIf
2095 '        Next  MCnt%
2096         '
2097         Ovrd 100
2098         Spd M_NSpd
2099         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2100         M_Out(Y60_Driver)=0     ' ビット回転停止
2101         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
2102         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2103         'もう一度吸着確認
2104 ' 以下暫定削除
2105 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2106 '        If MRtn = 0 Then      '吸着エラーの場合
2107 '            MCommentD1001 = 94
2108 '            MCommentD1002 = 95
2109 '            ScrewGet_S3 = -3
2110 '        EndIf
2111 '        If MRtn = 1 Then      '吸着OKの場合
2112 '            ScrewGet_S3 = 1
2113 '        EndIf
2114 '        Break
2115     Else
2116         'Mネジ
2117         If MMScrewJudge% = 2 Then
2118             ScrewGet_S3 = -2
2119         EndIf
2120     EndIf
2121     Exit Function
2122 FEnd
2123 '
2124 '■fnKEY_WAIT()
2125 ''' <summary>
2126 ''' GOTからのキー入力待ち
2127 ''' </summary>
2128 '''<returns>1：停止    2：次へ
2129 '''         3：継続    4：トルクチェック開始
2130 '''         5：NG
2131 '''         11：ロボット初期位置1    12：ロボット初期位置2
2132 '''         13：ロボット初期位置3    14：ロボット初期位置4
2133 '''</returns>
2134 ''' <remarks>
2135 ''' Date   : 2021/07/07 : M.Hayakawa
2136 ''' </remarks>'
2137 Function M% fnKEY_WAIT()
2138     fnKEY_WAIT = 0
2139     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
2140     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
2141     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
2142     '下記キー待ちの継続に反応させないため
2143     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
2144     Dly 0.2
2145     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
2146     MLocalLoopFlg=1
2147     While MLocalLoopFlg=1
2148         If M_In(11345) = 1 Then         '停止   M5345
2149             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
2150             fnKEY_WAIT = 1
2151             MLocalLoopFlg=-1
2152             Break
2153         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
2154             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
2155             fnKEY_WAIT = 2
2156             MLocalLoopFlg=-1
2157             Break
2158         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
2159             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
2160             fnKEY_WAIT = 3
2161             MLocalLoopFlg=-1
2162             Break
2163         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
2164             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
2165             fnKEY_WAIT = 4
2166             MLocalLoopFlg=-1
2167             Break
2168         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
2169             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
2170             fnKEY_WAIT = 5
2171             MLocalLoopFlg=-1
2172             Break
2173             '
2174         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
2175             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
2176             fnKEY_WAIT = MRobotInit1%
2177             MLocalLoopFlg=-1
2178             Break
2179             '
2180         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
2181             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
2182             fnKEY_WAIT = MRobotInit2%
2183             MLocalLoopFlg=-1
2184             Break
2185             '
2186         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
2187             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
2188             fnKEY_WAIT = MRobotInit3%
2189             MLocalLoopFlg=-1
2190             Break
2191             '
2192         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2193             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2194             fnKEY_WAIT = MRobotInit4%
2195             MLocalLoopFlg=-1
2196             Break
2197             '
2198         Else
2199         EndIf
2200     WEnd
2201     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2202     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2203     Exit Function
2204 FEnd
2205 '
2206 '■ fnAUTO_CTL
2207 ''' <summary>
2208 ''' AUTOモードOFF、PLCからの開始待ち
2209 ''' </summary>
2210 ''' <remarks>
2211 ''' Date   : 2021/07/07 : M.Hayakawa
2212 ''' </remarks>
2213 Function M% fnAUTO_CTL
2214     fnAUTO_CTL = 0
2215     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2216     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2217     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2218     '
2219     If M_Svo=0 Then             'サーボON確認
2220         Servo On
2221     EndIf
2222     Wait M_Svo=1
2223     Exit Function
2224 FEnd
2225 '
2226 '■ fnWindScreenOpen
2227 ''' <summary>
2228 ''' ウィンド画面の表示、非表示設定
2229 ''' </summary>
2230 '''<param name="%"></param>
2231 '''<param name="%"></param>
2232 '''<param name="%"></param>
2233 '''<param name="%"></param>
2234 ''' <remarks>
2235 ''' コメントD1001, D1002, D1003の設定
2236 ''' MWindReSet = 0     画面非表示
2237 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2238 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2239 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2240 ''' Date   : 2021/07/07 : M.Hayakawa
2241 ''' </remarks>
2242 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2243     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2244         M_Out16(12480) = MCommentD1001            'D1001 コメント
2245     EndIf
2246     '
2247     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2248         M_Out16(12496) = MCommentD1002            'D1002 コメント
2249     EndIf
2250     '
2251     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2252        M_Out16(12512) = MCommentD1003            'D1003 コメント
2253     EndIf
2254     '
2255     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2256     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2257     Dly 0.5
2258     M_Out(12363) = 0                         'ウィンド画面設定
2259     Exit Function
2260 FEnd
2261 '
2262 '■FnCtlValue2
2263 ''' <summary>
2264 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2265 ''' </summary>
2266 ''' <param name="MCtlNo%"></param>
2267 ''' <remarks>
2268 ''' Date : 2022/04/28 渡辺
2269 ''' </remarks>
2270 '''
2271 '''  1：投入数       ＋１
2272 '''  2：組立ＯＫ数   ＋１
2273 '''  3：組立ＮＧ数   ＋１ (未使用)
2274 '''  4：吸着エラー数 ＋１
2275 ''' 99：読書開始信号 OFF
2276 '''
2277 Function M% FnCtlValue2(ByVal MCtlNo%)
2278     FnCtlValue2 = 1
2279     Select MCtlNo%
2280         Case 1        '投入数＋１
2281             M_Out(12569) = 0             '書込み開始信号OFF
2282             M_Out(12568) = 1             '読込み開始信号ON
2283             MInputQty = M_In16(11600)    '投入数受信
2284             MInputQty = MInputQty + 1    '投入数＋１
2285             M_Out16(12592) = MInputQty   '投入数送信
2286             M_Out(12569) = 1             '書込み開始信号ON
2287             Break
2288             '
2289         Case 2        '組立ＯＫ数＋１
2290             M_Out(12569) = 0             '書込み開始信号OFF
2291             M_Out(12568) = 1             '読込み開始信号ON
2292             MAssyOkQty = M_In16(11616)   '組立OK数受信
2293             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2294             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2295             M_Out(12569) = 1             '書込み開始信号ON
2296             Break
2297             '
2298         Case 4        '吸着エラー数＋１
2299             M_Out(12569) = 0                       '書込み開始信号OFF
2300             M_Out(12568) = 1                       '読込み開始信号ON
2301             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2302             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2303             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2304             M_Out(12569) = 1                       '書込み開始信号ON
2305             Break
2306             '
2307         Case 99        '読書開始信号OFF
2308             M_Out(12568) = 0        '読込み開始信号OFF
2309             M_Out(12569) = 0        '書込み開始信号OFF
2310             Break
2311             '
2312     End Select
2313     Exit Function
2314 FEnd
2315 'Insightによる画像処理検査実行（並列処理なし）
2316 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2317 '-------------------------------------------------------------------------------
2318 'Insightによる画像処理検査実行（並列処理なし）
2319 '   引数
2320 '       PInspPos()      ：検査位置
2321 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2322 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2323 '       MInspCnt%       ：検査位置数
2324 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2325 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2326 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2327 '   戻り値：整数
2328 '       0=異常終了、1=正常終了
2329 '
2330 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2331 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2332 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2333 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2334 '   20200410    :   検査グループ設定Retry追加
2335 '-------------------------------------------------------------------------------
2336     '----- 初期設定 -----
2337     Cnt 0                                                           '移動効率化解除(初期値=0)
2338     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2339 '    Cnt 1,0.1,0.1
2340     '変数宣言・初期化
2341     Def Inte MNum                                                   '検査番号(検査順1～)
2342     MNum% = 1                                                       '検査番号初期値設定
2343     Def Inte MEndFlg                                                '検査終了フラグ
2344     MEndFlg% = 0
2345     '
2346     '検査G番号設定要求・検査実行要求off
2347     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2348     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2349     'エラー番号クリア
2350     MInspErrNum = 0                                                 '検査実行エラー番号
2351     M_Out16(MOUT_InspErrNum) = MInspErrNum
2352     MInspNGStepNum = 0                                              '検査実行NGStep番号
2353     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2354     '
2355     'Insight Ready check?
2356     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2357         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2358         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2359         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2360         ISInspectionSingle = 0                                      '異常終了戻り値設定
2361         Exit Function
2362     EndIf
2363     '
2364     '検査位置数確認
2365     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2366         MInspErrNum = 21                                            '検査データなし 21　引数<1
2367         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2368         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2369         ISInspectionSingle = 0                                      '異常終了戻り値設定
2370         Exit Function
2371     EndIf
2372     '
2373     '
2374     '
2375     '----- メイン処理 -----
2376     '設定された検査位置数分の検査実行
2377     While( MEndFlg% = 0 )
2378         '----- 検査グループ番号設定Retry追加 20200410
2379         MSetGrNumRetryExitFlg = 0
2380         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2381         While( MSetGrNumRetryExitFlg = 0 )
2382         '----- 検査グループ番号設定Retry追加ここまで 20200410
2383             '
2384             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2385             '
2386             '----- 検査グループ番号設定 -----
2387             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2388             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2389             '
2390             '検査位置へ移動・移動完了待ち
2391             Mvs PInspPos( MNum% )                                       '移動
2392             Dly 0.05                                                    '移動完了後Delay
2393             '
2394             '検査グループ番号設定終了確認
2395             M_Timer(1) = 0
2396             MExitFlg = 0
2397             While( MExitFlg = 0 )
2398                 '検査G設定正常終了?
2399                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2400                     MExitFlg = 1
2401                 '
2402                 '検査G設定異常終了?
2403                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2404                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2405                     If MInspErrNum = 0 Then                             '1回目のエラー?
2406                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2407                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2408                     EndIf
2409                     MExitFlg = 1
2410                 '
2411                 'timeoutチェック
2412                 ElseIf 1000 < M_Timer(1) Then
2413                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2414                     If MInspErrNum = 0 Then                             '1回目のエラー?
2415                         MInspErrNum = 12                                'timeout エラー番号=12
2416                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2417                     EndIf
2418                     MExitFlg = 1
2419                 EndIf
2420             WEnd
2421             '
2422             '検査G番号設定要求off
2423             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2424             '
2425             '----- 検査グループ設定Retry追加 20200410
2426             'NGなければ抜ける
2427             If MCurrentStepErr = 0 Then
2428                 MSetGrNumRetryExitFlg = 1
2429             Else
2430                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2431                 If MSetGrNumRetryCnt = 0 Then
2432                     MSetGrNumRetryExitFlg = 1
2433                 Else
2434                     'Retryへ　その前にDelay
2435                     Dly 0.5
2436                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2437                 EndIf
2438             EndIf
2439             '----- 検査グループ設定Retry追加ここまで 20200410
2440             '
2441         WEnd
2442         '
2443         '
2444         '
2445         '----- 検査実行 -----
2446         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2447             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2448                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2449                 MInspRetryExitFlg = 0
2450                 MRetryCnt = 2                                        'Retry回数設定
2451                 While( MInspRetryExitFlg = 0 )
2452                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2453                     '
2454                     '検査完了確認
2455                     MRetryCnt = MRetryCnt - 1
2456                     M_Timer(1) = 0
2457                     MExitFlg = 0
2458                     While( MExitFlg = 0 )
2459                     '検査完了待ち
2460                         '検査OK終了?
2461                         If M_In( MIN_IS_InspOK% ) = 1  Then
2462                             MJudgeOKFlg = 1                         '検査OKフラグON
2463                             MExitFlg = 1
2464                         '
2465                         '検査NG終了?
2466                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2467                             If MInspErrNum = 0 Then                 '1回目のエラー?
2468                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2469                                     MInspErrNum = 32                    '検査NG エラー番号=32
2470                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2471                                 EndIf
2472                             EndIf
2473                             MExitFlg = 1
2474                         '
2475                         '検査異常終了(IS timeout)?
2476                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2477                             If MInspErrNum = 0 Then                 '1回目のエラー?
2478                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2479                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2480                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2481                                 EndIf
2482                             EndIf
2483                             MExitFlg = 1
2484                         '
2485                         'timeoutチェック
2486                         ElseIf 3000 < M_Timer(1) Then
2487                             If MInspErrNum = 0 Then                 '1回目のエラー?
2488                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2489                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2490                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2491                                 EndIf
2492                             EndIf
2493                             MExitFlg = 1
2494                         EndIf
2495                     WEnd
2496                     '
2497                     '検査開始要求off
2498                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2499                     '
2500                     'OKなら抜ける
2501                     If MJudgeOKFlg = 1 Then
2502                         MInspRetryExitFlg = 1
2503                     Else
2504                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2505                         If MRetryCnt = 0 Then
2506                             MInspRetryExitFlg = 1
2507                         Else
2508                             'Retryへ　その前にDelay
2509                             Dly 0.3
2510                         EndIf
2511                     EndIf
2512                     '
2513                 WEnd
2514             EndIf
2515         EndIf
2516         '
2517         '
2518         '
2519         MNum% = MNum% + 1                                           '検査Step+1
2520         '検査終了確認　検査終了フラグセット
2521         If (MInspCnt% < MNum% ) Then
2522             MEndFlg% = 1                                            '検査終了フラグセット
2523         EndIf
2524         'NG発生時続行時処理
2525         If MInspErrNum <> 0 Then                                    'NGあり?
2526             If MNgContinue% <> 1 Then                               'NG続行?
2527                 MEndFlg% = 1                                        '検査終了フラグセット
2528             EndIf
2529         EndIf
2530     WEnd
2531     '
2532     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2533     If 0 < MZAxis% Then
2534         PCurrentPos = P_Curr                                        '現在位置取得
2535         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2536         Mvs PCurrentPos                                             '現在位置上空へ移動
2537     EndIf
2538     '
2539     '戻り値設定
2540     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
2541         ISInspectionSingle = 1                                      '正常終了戻り値設定
2542     Else
2543         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2544         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2545         ISInspectionSingle = 0                                      '異常終了戻り値設定
2546     EndIf
2547     '
2548     Fine 0 , P
2549     Exit Function
2550 FEnd
2551 '
2552 '■fnAutoScreenComment
2553 ''' <summary>
2554 ''' メイン画面の動作状況表示
2555 ''' コメントD1005の設定
2556 ''' </summary>
2557 '''<param name="McommentD1005%">コメントID</param>
2558 ''' <remarks>
2559 ''' Date   : 2021/07/07 : M.Hayakawa
2560 ''' </remarks>
2561 Function fnAutoScreenComment(ByVal McommentD1005%)
2562     M_Out16(12576) = McommentD1005%
2563     Exit Function
2564 FEnd
2565 '
2566 '■InitialZoneB
2567 ''' <summary>
2568 ''' 非常停止後の復帰動作
2569 ''' 1)上空退避　Z方向上に移動
2570 ''' 2)J1軸以外を退避ポジションへ移動
2571 ''' 3)J1軸のみを退避ポジションへ移動
2572 ''' 4)イニシャルポジションへ移動
2573 ''' </summary>
2574 ''' <remarks>
2575 ''' Date : 2022/04/08 : N.Watanabe
2576 ''' </remarks>
2577 Function V fnInitialZoneB()
2578     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/04/27 渡辺
2579 '
2580 'パラメータ
2581     Ovrd 5
2582 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2583 '    Cmp Pos, &B100011
2584 '
2585 '復帰動作開始
2586 '
2587 '置き台と両掴みの場所は、チャックを解放する
2588 *RecoveryChuckOpen
2589     PActive = P_Curr          '現在位置を取得
2590     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
2591 'PProductOnRoboSet(ねじロボ1本体置き位置)は、チャック解放
2592     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2593         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2594             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2595                 MRecoveryChuckOpen = 1
2596             EndIf
2597         EndIf
2598     EndIf
2599 'PProductOnRoboGet(ねじロボ1本体取り位置)は、チャック解放
2600     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2601         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2602             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2603                 MRecoveryChuckOpen = 1
2604             EndIf
2605         EndIf
2606     EndIf
2607 '
2608     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2609     M_Out(12256) = 0                           '本体チャック閉OFF
2610     M_Out(12257) = 1                           '本体チャック開ON
2611 '
2612     M_20# = 0                                  'KEY入力初期化
2613     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
2614     If MRtn = 1 Then M_Out(12257) = 0          '本体チャック開OFF
2615     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2616 '
2617     fErrorProcess(11,244,284,0)
2618     If M_20# = MNext% Then M_20# = MClear%
2619     If M_20# = MAbout% Then GoTo *RecoveryEnd
2620     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2621     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2622 '
2623     *RecoveryChuckOpenEnd
2624 '
2625 '上空退避
2626     PActive = P_Curr
2627     Pmove = PActive
2628     Pmove.Z = 640           '上空退避する一律の高さ
2629     If PActive.X > 550 Then
2630         Pmove.Z =480        'パレット上に腕を伸ばしているときは640まで上げられない為、例外処置
2631     EndIf
2632     If PActive.Z < Pmove.Z Then   '現在の高さがPmoveより低いとのみ実行
2633         Mvs Pmove
2634     EndIf
2635 '
2636     Dly 1.0
2637 'J1軸以外を退避ポジションへ移動
2638     JActive = J_Curr
2639     Jmove = JTaihi
2640     Jmove.J1 = JActive.J1        'J1軸のみ現在値を使用し、他の軸はJTaihiのポーズを取る
2641     Mov Jmove
2642     Dly 1.0
2643 'J1軸のみを退避ポジションへ移動
2644     Mov JTaihi
2645     Dly 1.0
2646 'イニシャルポジションへ移動
2647     Mov PInitialPosition
2648     Cmp Off
2649 ' ねじロボを初期位置に戻すために強制的に自動運転開始
2650     If M_In(11856) = 0 Then                 ' 停止中のみ
2651         fnAutoScreenComment(501)            ' 状態表示[ネジ締め機自動運転開始中] 2022/04/27 渡辺
2652         M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
2653         MRet = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
2654         If MRet = 0 Then
2655         Else
2656             M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
2657         EndIf
2658     EndIf
2659     M_Out(12264) = 0            '位置決め出OFF
2660     M_Out(12265) = 1            '位置決め戻ON
2661    fErrorProcess(11,253,281,0)
2662 *RecoveryEnd
2663     Exit Function
2664 FEnd
2665 '
2666 '
2667 '■fnRoboPosChk
2668 ''' <summary>
2669 ''' 最後に終了したロボットポジションの確認
2670 ''' </summary>
2671 '''<param name="MINNumber%">入力番号</param>
2672 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2673 '''<param name="MTimeCnt&">タイムアウト時間</param>
2674 ''' PLCに保続した番号を読込み、確認
2675 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2676 '''<returns>整数 0:タイムアウト 1:OK</returns>
2677 ''' <remarks>
2678 ''' Date   : 2021/07/07 : M.Hayakawa
2679 ''' </remarks>
2680 Function M% fnRoboPosChk
2681     fnRoboPosChk = 0
2682     MRet = fnStepRead()
2683     '初期位置でないと判断した場合
2684     'ウィンド画面切換え
2685     If MRBTOpeGroupNo > 5 Then
2686         '下記キー待ちの継続に反応させないため
2687         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2688         Dly 0.2
2689         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2690         Dly 1.5
2691         '
2692         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2693         '
2694         MLoopFlg% = 1
2695         While MLoopFlg% = 1
2696             '
2697             '
2698             MKeyNumber% = fnKEY_WAIT()
2699             Select MKeyNumber%
2700                 Case Is = MAbout%       '停止
2701                     M_20# = MAbout%
2702                     MLoopFlg% = -1
2703                     Break
2704                 Case Is = MNext%        '次へ
2705                     'MLoopFlg% = -1
2706                     Break
2707                 Case Is = MContinue%    '継続
2708                     M_20# = MContinue%
2709                     MLoopFlg% = -1
2710                     Break
2711                 Default
2712                     Break
2713             End Select
2714         WEnd
2715     EndIf
2716     '
2717     If M_20# = MContinue% Then                              '継続ボタンが押された場合
2718         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
2719         Ovrd 5                                   '低速オーバーライド値設定
2720         Select MRBTOpeGroupNo
2721             Case Is = 5                          '何もしない
2722                 Break
2723             Case Is = 10                         '初期位置へ戻す
2724                 'Mov PTEST001
2725                 Break
2726             Case Is = 15                         '初期位置へ戻す
2727                 'Mov PTEST002
2728                 Dly 0.5
2729                 'Mov PTEST001
2730                 Dly 0.5
2731                 Break
2732             Default
2733                 Break
2734         End Select
2735         '
2736         Ovrd M_NOvrd                            'システムの初期値を設定
2737         M_Out(12364) = 1                        'toPLC_データ保存ON
2738         MRBTOpeGroupNo = 5
2739         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
2740         Dly 1.0
2741         M_Out(12364) = 0                        'toPLC_データ保存OFF
2742         fnRoboPosChk = 1                        '初期位置動作実行
2743         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
2744     EndIf
2745     Exit Function
2746 FEnd
2747 '
2748 '■frInCheck
2749 ''' <summary>
2750 ''' センサーINチェック
2751 ''' </summary>
2752 '''<param name="MINNumber%">入力番号</param>
2753 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2754 '''<param name="MTimeCnt&">タイムアウト時間</param>
2755 '''<returns>整数 0:タイムアウト 1:OK</returns>
2756 ''' <remarks>
2757 ''' Date   : 2021/07/07 : M.Hayakawa
2758 ''' </remarks>
2759 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2760     M_Timer(4) = 0
2761     MloopFlg = 0
2762     While MloopFlg = 0
2763         MCrtTime& = M_Timer(4)
2764         If M_In(MINNumber%) = MCMPFLG% Then
2765             MloopFlg = 1
2766             frInCheck = 1
2767         ElseIf MCrtTime& > MTimeCnt& Then
2768             MloopFlg = 1
2769             frInCheck = 0
2770         EndIf
2771     WEnd
2772     Exit Function
2773 FEnd
2774 '-----------------------------------------------
2775 '
2776 'ねじ締め機通信確認
2777 '
2778 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2779 'fScewTcomChk = 0　：正常終了
2780 '          　　 -1 ：異常終了
2781 '-----------------------------------------------
2782 Function M% fScewTcomChk
2783 *ReCheckScewTcomChk
2784     fScewTcomChk = 0
2785     '通信確認送信
2786     M_Out(MOUT_ScwT_ComChk%) = MOn%
2787     '通信確認受信待機
2788 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
2789     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
2790     '通信確認送信終了
2791     M_Out(MOUT_ScwT_ComChk%) = MOff%
2792     If MRtn = 0 Then
2793         fScewTcomChk = -1
2794     EndIf
2795     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
2796  '
2797 FEnd
2798 '
2799 '
2800 '-----------------------------------------------
2801 '
2802 'ねじ締め開始送信
2803 '
2804 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2805 'fScewTStart = 0　：正常終了
2806 '          　　-1 ：異常終了
2807 '-----------------------------------------------
2808 Function M% fScewTStart
2809     fScewTStart = 0
2810     nRet% = 0
2811     'ねじ締め開始待機を受信
2812 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
2813     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
2814     If MRtn = 0 Then nRet% = -1
2815     If MRtn = 0 Then GoTo *ScrewStartERROR      '開始できなかった場合ジャンプ
2816     Dly 0.1
2817     'ねじ締め開始受信を送信
2818     M_Out(MOUT_ScwT_ST%) = MOn%
2819     Dly 0.5
2820     'Wait M_In(MTEST_KEY%) = MOn%
2821     'ねじ締め開始送信終了
2822     M_Out(MOUT_ScwT_ST%) = MOff%
2823     '
2824 *ScrewStartERROR
2825     fScewTStart = nRet%
2826 FEnd
2827 '
2828 '
2829 '
2830 '-----------------------------------------------
2831 '
2832 'ねじ締め完了受信
2833 '
2834 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2835 'fScewTcomChk = 0　：正常終了
2836 '          　 　-1 ：異常終了
2837 '-----------------------------------------------
2838 Function M% fScewTFinish
2839 *ReCheckScewTFinish
2840     fScewTFinish = 0
2841     'ねじ締め完了待機を受信
2842 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
2843     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
2844     If MRtn = 0 Then
2845         fScewTFinish = -1
2846     EndIf
2847     If MRtn = 2 Then GoTo *ReCheckScewTFinish
2848     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
2849     Dly 0.1
2850     'ねじ締め完了受信を送信
2851     M_Out(MOUT_ScwT_FinOK%) = MOn%
2852     Dly 0.5                          'とりあえず保持時間0.5msec
2853     'ねじ締め開始送信終了
2854     M_Out(MOUT_ScwT_FinOK%) = MOff%
2855     'Wait M_In(MTEST_KEY%) = MOn%
2856     '
2857 *ScewTFinish_ErrEnd
2858 FEnd
2859 '
2860 '
2861 '-----------------------------------------------
2862 '
2863 '条件xx停止受信
2864 '
2865 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2866 'fScewTCaseStop = 0　：正常終了
2867 '          　   　-1 ：異常終了
2868 '-----------------------------------------------
2869 Function M% fScewTCaseStop(ByVal MCase%())
2870 *ReCheckScewTCaseStop
2871     fScewTCaseStop = 0
2872     '条件xx停止を受信
2873     Wait M_In(MCase%(1)) = MOn%
2874     MRtn = fTimeOutJudge(MCase%(1),MOn%)
2875     If MRtn = 0 Then
2876         fScewTCaseStop = -1
2877     EndIf
2878     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
2879     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
2880     Dly 0.1
2881     '条件xx停止受信を送信
2882     M_Out(MCase%(2)) = MOn%
2883     Dly 0.5                          'とりあえず保持時間0.5msec
2884     'ねじ締め開始送信終了
2885     M_Out(MCase%(2)) = MOff%
2886 *ScewTCaseStop_ErrEnd
2887     '
2888 FEnd
2889 '
2890 '-----------------------------------------------
2891 '
2892 '再開始受信
2893 '
2894 '22/09/29 Waitがタイムアウトできるよう修正(中村)
2895 'fScewTReStart = 0　：正常終了
2896 '              　-1 ：異常終了
2897 '-----------------------------------------------
2898 Function M% fScewTReStart()
2899 *ReCheckScewTReStart
2900     fScewTReStart = 0
2901     '再開始を受信
2902     Wait M_In(MIN_ScwT_ReST%) = MOn%
2903     MRtn = fTimeOutJudge(MIN_ScwT_ReST%,MOn%)
2904     If MRtn = 2 Then GoTo *ReCheckScewTReStart
2905     If MRtn = 0 Then GoTo *ScewTReStart_ErrEnd
2906     Dly 0.1
2907     '再開始受信を送信
2908     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msecパルス
2909 *ScewTReStart_ErrEnd
2910     Exit Function
2911 FEnd
2912 '
2913 '■fScrewTighenRoboCheck
2914 '<summary>
2915 'ねじロボ監視
2916 '</summary>
2917 '<param name = "MStopNum%"> 停止番号</param>
2918 '<returns>整数 0:ねじロボ異常終了 1:OK </returns>
2919 '<make>
2920 '2021/12/2 中村天哉
2921 '</make>
2922 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
2923     fnAutoScreenComment(503)    '状態表示[ねじロボ動作終了待ち] 2022/04/27 渡辺
2924     fScrewTighenRoboCheck = 1
2925     MScrewTighenRoboFlg% = 1    'フラグの初期化
2926     MCheck% = 0
2927     While MScrewTighenRoboFlg% = 1
2928         MCheck% = M_In16(11904)
2929         If M_In(MStopNum%) = 1 Then '停止位置まで来たら
2930             MScrewTighenRoboFlg% = 0 '関数を抜ける
2931         EndIf
2932         If MCheck% <> 0 Then
2933             fScrewTighenRoboError(MCheck%)
2934             Select M_20#
2935                 Case MAbout%            '停止が押された場合
2936                     M_Out(12869) = 1 Dly 1.0
2937                     MScrewTighenRoboFlg% = 0
2938                     fScrewTighenRoboCheck = 0   '異常終了
2939                     Break
2940                 Case MNgProcess%        'NGが押された場合
2941                     M_Out(12873) = 1 Dly 1.0
2942                     MScrewTighenRoboFlg% = 0
2943                     fScrewTighenRoboCheck = 0   '異常終了
2944                     Break
2945                 Case MContinue%             'リトライが押された場合
2946                     M_20# = MClear%         'M_20#初期化
2947                     M_Out(12871) = 1 Dly 1.0
2948                     Break
2949                 Case MNext%                 '次へが押された場合
2950                     M_20# = MClear%         'M_20#初期化
2951                     M_Out(12874) = 1 Dly 1.0
2952                     Break
2953             End Select
2954             Dly 0.5
2955         EndIf
2956     WEnd
2957     Exit Function
2958 FEnd
2959 '■fScrewTighenRoboError
2960 '<summary>
2961 'ねじロボエラー処理
2962 '</summary>
2963 '<param name = "ErrorCode%"> エラー番号</param>
2964 '<make>
2965 '2021/12/2 中村天哉
2966 '</make>
2967 Function fScrewTighenRoboError(ErrorCode%)
2968     MCommentD1001 = ErrorCode% + 300
2969     fErrorProcess(11,MCommentD1001,0,0)
2970     Exit Function
2971 FEnd
2972 '■fErrorProcess
2973 '<summary>
2974 'エラー処理
2975 '</summary>
2976 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
2977 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
2978 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
2979 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
2980 '<make>
2981 '2021/11/5 中村天哉
2982 '</make>
2983 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2984     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
2985     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
2986     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
2987     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
2988 *RETRY_ERR_PROCESS
2989      M_20# = MClear%     '初期化
2990 '        'エラー処理記述
2991         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2992 '        'GOT KEY入力待ち
2993         MKeyNumber = fnKEY_WAIT()
2994 '        '
2995         If MKeyNumber = MAbout% Then   '停止を選択した場合
2996             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2997  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2998             Break
2999          '
3000         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
3001             M_20# = MContinue%            'M_20# プログラム間共通外部変数
3002  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3003             Break
3004         '
3005         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
3006             M_20# = MNext%            'M_20# プログラム間共通外部変数
3007  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3008             Break
3009          '
3010         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
3011             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
3012  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3013             Break
3014         '
3015         EndIf
3016         '
3017         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3018         fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3019     Exit Function
3020 FEnd
3021 '
3022 '■fnTorqueCheck
3023 ''' <summary>
3024 ''' トルクチェック動作用のメイン
3025 ''' </summary>
3026 ''' <remarks>
3027 ''' Date   : 2021/12/21 : H.AJI
3028 ''' </remarks>'
3029 Function M% fnTorqueCheck
3030     'トルクチェック中送信  搬送系停止
3031     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
3032     '
3033     fnTorqueCheck = 0
3034     Ovrd 20
3035     'Mov PInitialPosition              '初期位置移動
3036     Ovrd 100
3037     '下記キー待ちの継続に反応させないため
3038     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3039     Dly 0.2
3040     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3041     '
3042     'M6340  トルクチェック受信
3043     'Dly 5.0
3044     M_Out(12340) = 1          'トルクチェック受信 M6340
3045     Dly 1.0
3046     M_Out(12340) = 0
3047     '
3048     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3049     M_Out(12835) = 1                         'ねじロボトルクチェック画面切替
3050    Wait M_In(11843) = 1                         'ねじロボトルクチェック画面切替
3051     M_Out(12835) = 0                         'ねじロボトルクチェック画面切替完了
3052     '
3053     '
3054     MLoopFlg = 1
3055     While MLoopFlg = 1
3056         '
3057         'Mov PInitialPosition              '初期位置移動
3058         '
3059         MKeyNumber = fnKEY_WAIT()
3060         Select MKeyNumber
3061             Case Is = 1           '停止
3062                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3063                 Dly 1.0
3064                 M_Out(12343) = 0
3065                 Ovrd 20
3066                 'Mov PTicketRead_1
3067                 M_Out(12840) = 1          'トルクチェック終了
3068                 Wait M_In(11859) = 1      'ねじロボからの終了
3069                 M_Out(12840) = 0          'トルクチェック終了
3070                 Ovrd 100
3071                 M_20# = 1
3072                 MLoopFlg = -1
3073                 Break
3074             Case Is = 2           '次へ
3075                 Break
3076             Case Is = 3           '継続
3077                 Break
3078             Case Is = 4           'トルクチェック開始
3079                 M_Out(12342) = 1          'トルクチェック開始要求受信 M6342
3080                 Dly 1.0
3081                 M_Out(12342) = 0
3082                 If M_In(11862) = 1 Then             'トルクチェッカー確認
3083                     fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3084                     MRet = fnScrewMTorque()           'ねじロボ用トルクチェック
3085                 EndIf
3086                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3087                 'MRet = fnMoveTorquePosi()
3088                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3089                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3090                 Break
3091             Default
3092                 Break
3093         End Select
3094     WEnd
3095     '
3096     'トルクチェック中停止送信
3097     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3098     '
3099     'ロボットの位置を元に戻す
3100     '
3101     Exit Function
3102  FEnd
3103  '
3104 '
3105 '
3106 '---------------------------
3107 '
3108 '    メイン画面の表示、非表示設定
3109 '         コメントD1001, D1002, D1003の設定
3110 '           MWindReSet = 0     画面非表示
3111 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3112 '           MWindErrScr = 10    エラー画面 D1001, D1002
3113 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3114 '
3115 '---------------------------
3116 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3117     fnMainScreenOpen = 0
3118     '
3119    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3120         M_Out16(12480) = MCommentD1001            'D1001 コメント
3121     EndIf
3122     '
3123     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3124         M_Out16(12496) = MCommentD1002            'D1002 コメント
3125     EndIf
3126     '
3127     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3128         M_Out16(12512) = MCommentD1003            'D1003 コメント
3129     EndIf
3130     '
3131     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3132     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3133     Dly 0.5
3134     M_Out(12362) = 0                         'ウィンド画面設定
3135     Exit Function
3136 FEnd
3137 '
3138 '■Main
3139 ''' <summary>
3140 ''' トルクチェック実動作
3141 ''' </summary>
3142 ''' <remarks>
3143 ''' Date   : 2021/12/21 : H.AJI
3144 ''' </remarks>'
3145 Function M% fnScrewMTorque
3146     fnScrewMTorque = 0
3147     M_Out(12838) = 1                         'トルクチェック開始1
3148     Wait M_In(11857) = 1                     '受信完了
3149     M_Out(12838) = 0                         'トルクチェック開始1
3150     Dly 2.0
3151     Exit Function
3152 FEnd
3153 '
3154 '
3155 '----------------------------------------------------------------
3156 'fTimeOutJudge
3157 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3158 '引数
3159 'Address% = 監視アドレス番号
3160 'JudgeFlg% = 対象アドレスの正常終了時の値
3161 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3162 '戻り値 = 0 エラー
3163 '         1 正常終了
3164 '         2 リトライ
3165 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3166 '作成日
3167 '2022/9/20 中村
3168 '----------------------------------------------------------------
3169 '
3170 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3171     fTimeOutJudge = 0
3172     MJudge% = 1
3173     MRtn = 0
3174     M_20# = MClear%
3175     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3176 *TimeOutLoop
3177     If MRtn = 1 Then GoTo *TimeOut
3178         fErrorProcess(11,202,203,0)
3179         If M_20# = MNext% Then GoTo *TimeOutLoop
3180         If M_20# = MContinue% Then MJudge% = 2
3181         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3182 *TimeOut
3183     fTimeOutJudge = MJudge%
3184 '
3185 *JUDGE_ERROR_END
3186 FEnd
3187 '
3188 '■Main
3189 ''' <summary>
3190 ''' 組立動作用のメイン
3191 ''' </summary>
3192 ''' <remarks>
3193 ''' Date   : 2021/07/07 : M.Hayakawa
3194 ''' </remarks>'
3195 Function Main
3196     MopeNo = M_21#         '外部変数にて動作番号代入
3197     '
3198     If M_Svo=0 Then
3199         Servo On
3200     EndIf
3201     Wait M_Svo=1
3202 '組立スタート日付時刻要求パルスON
3203     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3204 'パトライト操作
3205     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3206     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3207     '
3208     M_20# = 0                                   'KEY入力初期化
3209     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
3210     MRet% = 0
3211 '初期位置の確認と移動
3212 '
3213 '復帰動作　実行・未実行判別      2022/04/08 渡辺 作成
3214     PActive = P_Curr                    '現在位置を取得
3215     MRecoveryPass% = 0
3216     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3217         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3218             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3219                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3220             EndIf
3221         EndIf
3222     EndIf
3223     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3224         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3225             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3226                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3227             EndIf
3228         EndIf
3229     EndIf
3230     If MRecoveryPass% = 0 Then
3231        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3232     EndIf
3233 '
3234 '
3235 '    MRet% = fnRoboPosChk()
3236     If MRet% = 1 Then                           '初期位置の動作を行った場合
3237         fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  'ウィンド画面
3238         MKeyNumber% = fnKEY_WAIT()
3239         Select MKeyNumber%
3240             Case Is = MAbout%       '停止
3241                 M_20# = MAbout%
3242                 MLoopFlg% = -1
3243                 Break
3244             Case Is = MNext%        '次へ
3245                 'MLoopFlg = -1
3246                 Break
3247             Case Is = MContinue%    '継続
3248                 M_20# = MContinue%
3249                 MLoopFlg% = -1
3250                 Break
3251             Default
3252                 Break
3253         End Select
3254     EndIf
3255     '
3256     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3257         M_Out(12364) = 1            'toPLC_データ保存ON
3258 'トルクチェック
3259         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3260             MRet% = fnTorqueCheck()
3261             Break
3262         Else
3263 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
3264 '                MRtn = InspInit()               '画像処理初期化処理
3265 '            EndIf
3266             '
3267            M_20# = MClear%                    '初期化
3268 '組立開始
3269             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3270                 MRet% = fnAssyStart()
3271             Else
3272                 M_20# = MPass%
3273             EndIf
3274 '組立終了日付時刻
3275             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3276             Wait M_In(11572) = 1            '日付取得完了
3277             Dly 0.1
3278             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3279 'リフターユニットへのOUT
3280             '  KEY入力が何もない場合 OKと判断
3281             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3282             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3283 'OK/NGフラグ出力
3284             If M_20# <= 0 Then
3285                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3286             ElseIf M_20# = MPass% Then
3287                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3288             EndIf
3289 'PIASに組立完了書込み
3290             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3291                 If M_20# = MPass% Then
3292                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3293                 Else
3294                     'KEY入力がNGの場合
3295                     If M_20# = MNgProcess% Then
3296                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3297                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3298                         MRet% = fnPiasWrite(MNG%)
3299                        nAssyNgQty = nAssyNgQty + 1
3300                     EndIf
3301                     '
3302                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3303                     If M_20# = MAssyOK% Then
3304                             '-----------------------
3305                             'D732 -> D2600 コピー要求
3306                             M_Out(12566) = 1
3307 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3308                             M_Out(12566) = 0
3309                             '
3310                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3311                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3312                             '基板番号照合(PPは未使用）
3313 '                            MRet% = fnPCBNumberCheck()
3314                         Else
3315                             MRet% = 1
3316                         EndIf
3317                         '
3318                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3319                             If M_20# <> MAbout% Then
3320                                 '工程履歴OK書き込み
3321                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3322                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3323                                 MRet% = fnPiasWrite(MOK%)
3324                                 nAssyOkQty = 0
3325                                 nAssyOkQty = nAssyOkQty + 1
3326                             Else
3327                                 nAssyOkQty = nAssyOkQty + 1
3328                             EndIf
3329                         EndIf
3330                     EndIf
3331 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3332 '                    MRet% = fnPiasWrite(MOK%)
3333                 EndIf
3334             Else
3335                 nAssyOkQty = nAssyOkQty + 1
3336             EndIf
3337             '
3338             '組立終了日付時刻解除
3339             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3340             '投入数、組立OK数、組立NG数書込み
3341 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3342             '
3343 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3344 '                '画像処理終了処理
3345 '                MRtn = InspQuit()
3346 '            EndIf
3347         EndIf
3348         M_Out(12364) = 0                          'toPLC_データ保存OFF
3349     EndIf
3350 'パトライト操作
3351     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3352     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3353 'GOT表示
3354     fnAutoScreenComment(93)  'AUTO画面 工程完了
3355 FEnd
3356 End
3357 '
3358 'おまじないコメント
3359 '絶対削除するな
3360 '
3361 ''
3362 '
3363 '
3364 '
PInspPosition(1)=(-53.94,-368.56,+601.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(-53.94,-598.55,+601.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
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
Pmove=(+313.97,-375.28,+640.00,+179.97,-0.46,-178.25,+0.00,+0.00)(7,0)
PInitialPosition=(+300.00,+0.00,+540.00,+180.00,+0.00,+180.00)(7,0)
PPlateLCheck=(-53.94,-598.55,+601.00,-180.00,+0.00,+90.00)(7,0)
PPlateLCheck_2=(-53.94,-598.55,+631.00,-180.00,+0.00,+90.00)(7,0)
PPlateLGet=(+639.88,-162.76,+441.80,+179.52,-0.10,+91.51)(7,0)
PPlateLGet_1=(+639.88,-162.76,+480.00,+179.52,-0.10,+91.51)(7,0)
PPlateLGet_2=(+396.00,-160.55,+570.00,+179.49,+0.10,+91.01)(7,0)
PPlateLSet=(+49.07,-583.23,+542.63,+179.97,-0.46,-178.26)(7,0)
PPlateLSet_1=(+49.07,-583.23,+600.00,+179.97,-0.46,-178.26)(7,0)
PPlateLSet_2=(+44.56,-280.00,+640.60,+179.98,-0.12,-179.36)(7,0)
PPlateRCheck=(-53.94,-368.56,+601.00,-180.00,+0.00,+90.00)(7,0)
PPlateRCheck_2=(-53.94,-368.56,+631.00,-180.00,+0.00,+90.00)(7,0)
PPlateRGet=(-510.99,+86.93,+362.39,-179.41,+0.80,+2.22)(7,15)
PPlateRGet_1=(-510.99,+86.93,+400.00,-179.41,+0.80,+2.22)(7,15)
PPlateRGet_2=(-323.09,-0.06,+640.58,+179.98,-0.13,+1.25)(7,0)
PPlateRGet_3=(+44.59,-320.00,+640.61,+179.98,-0.13,+99.17)(7,0)
PPlateRSet=(+44.84,-526.60,+546.32,+179.99,+0.61,-179.14)(7,0)
PPlateRSet_1=(+44.84,-526.60,+570.00,+179.99,+0.61,-179.14)(7,0)
PPlateRSet_2=(+45.33,-320.00,+640.00,+180.00,+0.00,-178.68)(7,0)
PPlateRSet_3=(+0.01,-336.64,+640.48,+180.00,-0.01,+90.00)(7,0)
PProductOnPltGet=(+547.57,-99.63,+413.85,-180.00,+0.00,-179.49)(7,0)
PProductOnPltGet_1=(+547.57,-99.63,+460.00,-180.00,+0.00,-179.49)(7,0)
PProductOnPltGet_2=(+547.57,-99.63,+530.00,-180.00,+0.00,-179.49)(7,0)
PProductOnPltSet=(+547.59,-97.90,+414.00,-180.00,+0.00,-179.52)(7,0)
PProductOnPltSet_1=(+547.59,-97.90,+460.00,-180.00,+0.00,-179.52)(7,0)
PProductOnPltSet_2=(+547.59,-97.90,+530.00,-180.00,+0.00,-179.52)(7,0)
PProductOnRoboGet=(+102.69,-556.31,+467.49,+179.84,+0.03,-179.68)(7,0)
PProductOnRoboGet_1=(+102.69,-556.31,+500.00,+179.84,+0.03,-179.68)(7,0)
PProductOnRoboGet_2=(+102.69,-556.31,+640.00,+179.84,+0.03,-179.68)(7,0)
PProductOnRoboSet=(+102.66,-556.32,+468.69,+179.84,+0.03,-179.68)(7,0)
PProductOnRoboSet_1=(+102.66,-556.32,+510.00,+179.84,+0.03,-179.68)(7,0)
PProductOnRoboSet_2=(+102.66,-556.32,+570.00,+179.84,+0.03,-179.68)(7,0)
PTicketRead=(+599.21,-290.97,+473.00,-180.00,+0.00,-90.00)(7,0)
PTicketRead_1=(+599.21,-290.97,+540.00,-180.00,+0.00,-90.00)(7,0)
JActive=(-50.02,+20.03,+76.82,-0.35,+83.46,-51.73,+0.00,+0.00)
Jmove=(-50.02,-9.85,+108.99,+1.35,+80.50,+0.00,+0.00,+0.00)
JTaihi=(+0.00,-9.85,+108.99,+1.35,+80.50,+0.00,+0.00,+0.00)
